/* 
* orbe.js
* by Pedro Maciel Xavier @ github.com/pedromxavier
*
* inspired by ORBE from Tabare Gallardo @ www.astronomia.edu.uy/orbe
*/


// Check for THREE.js & ORBELIB.js
if (THREE === undefined) {
    throw `Error: THREE.js is not available.`;
}

// I/O Functions
function loadJson(path, callback) {
    var rawFile = new XMLHttpRequest();
    rawFile.overrideMimeType("application/json");
    rawFile.open("GET", path, true);
    rawFile.onreadystatechange = function () {
        if (rawFile.readyState === 4 && rawFile.status == "200") {
            callback(JSON.parse(rawFile.responseText));
        }
    }
    rawFile.send(null);
}

// Math Constants
const PI = Math.PI;
const TWO_PI = 2.0 * Math.PI;

// Math Functions
function arg(x) {
    x %= TWO_PI;
    return (x < 0.0) ? x + TWO_PI : x;
}

function radians(x) {
    return x * PI / 180.0;
}

function degrees(x) {
    return x * 180.0 / PI;
}

function SIGN(A, B) {
    // FORTRAN SIGN(A, B)
    return (B >= 0.0) ? Math.abs(A) : -Math.abs(A);
}

// Orbe Constants
const YEAR_DAYS = 365.25; // Days in a year
const GAU = 0.01720209895;
const GM = Math.pow(GAU * YEAR_DAYS, 2);

class Body {
    /*
    ! First, a variable glossary
    * a: semimajor axis of the ellipse in astronomical units
    * e: eccentricity of the ellipse (between 0 and 1, 0 is a circle)
    * i: inclination of the orbit in degrees (from 0 to 180, 0 if is coincident with plane xy or ecliptic, 90 perpendicular, 180 retrograde motion)
    * W: longitude of the ascending node in degrees (from 0 to 360, 0 corresponds to Aries or x-axis)
    * w: argument of the perihelion in degrees (from 0 to 360, 0 corresponds to the direction of the node)
    * M: mean anomaly in degrees (from 0 to 360, 0 corresponds to perihelion, 180 to aphelion)
    * m: mass in solar masses
    * 
    ? Now, let's track down some ALIASES
    ! Input
    * a -> A, A0,
    * e -> E0,
    * i -> I, XI0,
    * W -> NODE, XN0,
    * w -> W, W0, PERI
    * M -> EME0,
    * m -> VAMA,
    ! Other
    * n -> ENE
    * x -> X, POS
    * v -> XP, VEL
    */
    constructor(data) {
        // ! Input parameters
        this.a = data.a; // ? Semimajor axis [0, INF)  (AU)
        this.e = data.e; // ? Eccentricity   [0, 1]    (N/A)
        this.i = data.i; // ? Inclination    [0, PI]   (rad)
        this.W = data.W; // ? Longitude      [0, 2*PI] (rad)
        this.w = data.w; // ? Argument       [0, 2*PI] (rad)
        this.M = data.M; // ? Mean Anomaly   [0, 2*PI] (rad)
        this.m = data.m; // ? mass           [0, INF)  (solar masses)  

        // * i.e. Planet Name
        this.name = data.name;

        // * Assert Data Consistency
        this.verify();

        // ! Extra parameters
        // ? Mean Motion (rad/s)
        this.n = null;
        // ? Orbital Period
        this.T = null;

        // ? Position and Velocity (vectors)
        this.$x = new THREE.Vector3(); // ? [POS]
        this.$v = new THREE.Vector3(); // ? [VEL]
        this.$f = new THREE.Vector3(); // ? [FIND]

        this.alive = true;
    }

    frame() {
        return {
            a: this.a,
            e: this.e,
            i: this.i,
            W: this.W,
            w: this.w,
            M: this.M,
            x: this.$x,
            v: this.$v,
        }
    }

    verify() {
        if (typeof this.a !== 'number' || this.a <= 0.0) {
            throw `Error: missing positive number for 'a' (Semimajor axis) on element data.`
        }

        if (typeof this.e !== 'number' || this.e < 0.0 || this.e > 1.0) {
            throw `Error: missing number in [0, 1] for 'e' (Eccentricity) on element data.`
        }

        if (typeof this.i !== 'number' || this.i < 0.0 || this.i > 180.0) {
            throw `Error: missing number in [0, 180] for 'i' (Inclination) on element data.`
        } else {
            this.i = radians(this.i)
        }

        if (typeof this.W !== 'number') {
            throw `Error: missing number for 'W' (Longitude) on element data.`
        } else {
            this.W = arg(radians(this.W))
        }

        if (typeof this.w !== 'number') {
            throw `Error: missing number for 'w' (Argument) on element data.`
        } else {
            this.w = arg(radians(this.w))
        }

        if (typeof this.M !== 'number') {
            throw `Error: missing number for 'M' (Mean Anomaly) on element data.`
        } else {
            this.M = arg(radians(this.M))
        }

        if (typeof this.m !== 'number' || this.m < 0.0) {
            throw `Error: missing non-negative number for 'm' (mass) on element data.`
        }

        if (typeof this.name !== 'string' && this.name !== null) {
            throw `Error: missing string (or null) for 'name' on element data.`
        } else if (this.name === null) {
            this.name = '';
        }
    }

    kill() {
        this.alive = false;
    }
}

class Orbe {

    constructor(data) {
        // From data object
        this.mass = data.mass;
        this.span = data.span;
        this.lapse = data.lapse;
        this.bodies = data.bodies;

        /*  Below, a few consistency tests.
        **  See './orbeini.json' for an exemple on format patterns.
        */

        if (typeof this.mass !== 'number' || this.mass <= 0.0) {
            throw `Error: missing positive number for 'mass'.`
        }

        if (this.span !== null && (typeof this.span !== 'number' || this.span == 0.0)) {
            throw `Error: missing non-zero number (or 'null') for 'span'.`
        }

        if (typeof this.lapse !== 'number' || this.lapse <= 0.0) {
            throw `Error: missing positive number for 'lapse'.`
        }

        if (!Array.isArray(this.bodies)) {
            throw `Error: missing array for 'bodies'.`
        } else {
            for (let i = 0; i < this.bodies.length; i++) {
                // Cast Object to Body type.
                this.bodies[i] = new Body(this.bodies[i]);
            }
        }

        // Constants
        this.GM0 = GM * this.mass;
        this.STEP = null;
        this.TMIN = null;

        // Time Control Quantities
        this.dt = null;
        this.time = 0.0;
        this.step = 1E-5;     

        // Initialize Simulation
        for (let body of this.bodies) {
            // ! Mean Motion
            body.n = Math.sqrt(GM * (this.mass + body.m) / Math.pow(body.a, 3))

            // ! Minimum Orbital Period
            body.T = TWO_PI / body.n;
            if (this.TMIN === null) {
                this.TMIN = body.T;
            } else {
                this.TMIN = Math.min(this.TMIN, body.T);
            }

            // ! Initial Position and Velocity
            this.PositionAndVelocity(body);
        }

        this.STEP = this.TMIN / 40.0;

        // Initial step
        for (let body of this.bodies) {
            this.ComputeElements(body);
        }

        // This part doesn't make any sense to me
        for (let i = 0; i < 7; i++) {
            if (this.step < this.STEP) {
                this.dt = this.step;
            }
            if (2.0 * this.step < this.STEP) {
                this.dt = 2.0 * this.step;
            }
            if (5.0 * this.step < this.STEP) {
                this.dt = 5.0 * this.step;
            }
            this.step *= 10.0;
        }

        this.lapse = Math.floor(this.lapse / this.dt) * this.dt;

        // ! Integrate Backwards
        if (this.span < 0.0) this.dt = -this.dt;
    }

    N() {
        return this.bodies.length;
    }

    ALIVE() {
        return this.bodies.reduce((s, body) => (body.alive ? s + 1 : s), 0);
    }

    // Main routines
    static load(path, element = null, callback = null) {
        loadJson(path, (data) => {
            let orbe = new this(data);

            if (element !== null) orbe.html(element);

            if (callback !== null) orbe.run(callback);
        });
    }

    run(callback) {
        // Initialize parameters
        this.init();

        if (typeof callback === 'function') {
            const frames = this.render();

            while (true) {
                let frame = frames.next();
                if (frame.done || !callback(frame.value)) break;
            }
        } else { // Run Local Output
            throw 'NotImplementedError';
        }
    }

    * render() {
        /* A frame is intended to be a list of elements and their information.
        **
        */
        var time = 0.0; // ? [TGRID]
        var frame = 0;  // ? [CTIEMPO]
        while (true) {
            let t = frame * this.dt;

            // Advance Integration
            this.KeplerMotion();
            this.Perturbations();
            this.KeplerMotion();

            if (Math.abs(time - SIGN(this.lapse, this.dt)) < 1E-5) {
                for (let body of this.bodies) {
                    if (body.alive) this.ComputeElements(body);
                }
                yield { frame: frame++, bodies: this.bodies.map((body) => (body.frame())) };
                time = this.dt;
                continue;
            }
            if (((this.span - t) * SIGN(1.0, this.dt) > 0.0)) {
                time += this.dt;
                continue;
            } else {
                break;
            }
        }
    }

    PositionAndVelocity(body) {
        /*
        **
        */
        // ! Orbital Elements
        var a, e, i, w, M, n;
        // ! Position & Velocity
        var $x, $v;
        // ! Other parameters
        var E, T, A, B;
        // ! Other Vectors
        var $c, $s, $p, $q;

        [a, e, i, w, M, n] = [body.a, body.e, body.i, body.w, body.M, body.n];
        
        E = this.SolveKepler(e, M);
        T = 2.0 * Math.atan(Math.sqrt((1.0 + e) / (1.0 - e)) * Math.tan(E / 2.0));

        A = body.a * (1.0 - e * Math.cos(E));
        B = body.n * body.a / Math.sqrt(1.0 - e * e);

        $c = new THREE.Vector3(Math.cos(n), Math.cos(i), Math.cos(w));
        $s = new THREE.Vector3(Math.sin(n), Math.sin(i), Math.sin(w));

        $p = new THREE.Vector3(
            $c.x * $c.z - $s.x * $c.y * $s.z,
            $s.x * $c.z + $c.x * $c.y * $s.z,
            $s.y * $s.z,
        )
        $q = new THREE.Vector3(
            -$c.x * $s.z - $s.x * $c.y * $c.z,
            -$s.x * $s.z + $c.x * $c.y * $c.z,
            $s.y * $c.z
        )

        // POSITION AND VELOCITY
        $x = new THREE.Vector3();
        $x.addScaledVector($p, A * Math.cos(T));
        $x.addScaledVector($q, A * Math.sin(T));

        $v = new THREE.Vector3();
        $v.addScaledVector($p, -B * Math.sin(T));
        $v.addScaledVector($q, B * (e + Math.cos(T)));

        body.$x.copy($x);
        body.$v.copy($v);
    }

    ComputeElements(body) {
        /* Calculate Orbital Elements
        ** 
        */
        // ! Orbital Elements
        var a, e, i, w, M;
        // ! Position & Velocity
        var $x, $v;
        // ! Other Parameters
        var c, s, u, v, GMM;
        // ! Other Vectors
        var $z, $w, $n, $u;

        $x = body.$x.clone();
        $v = body.$v.clone();

        $z = new THREE.Vector3();
        $z.crossVectors($x, $v);
        $w = new THREE.Vector3($z.x, $z.y, 0.0);

        c = arg(Math.atan2($z.x, -$z.y));
        s = Math.atan2($w.length(), $z.z);

        i = s;

        GMM = this.GM0 + body.m;

        $n = new THREE.Vector3();
        $n.crossVectors($v, $z);
        $n.divideScalar(GMM);

        $u = $x.clone();
        $u.normalize();

        $n.sub($u);

        if (Math.sin(s) < 1E-12) {
            // ! If Inclination is 0 or 180 deg
            // ! this case w is the longitude of perihelion
            c = 0.0;
            w = arg(Math.atan2($n.y, $n.x));
        } else {
            w = arg(Math.atan2(
                $n.z / Math.sin(s),
                $n.x * Math.cos(c) + $n.y * Math.sin(c)
            ));
        }

        u = 2.0 / $x.length();
        v = $v.lengthSq() / GMM;

        a = 1.0 / (u - v);

        if (a < 0.0 || a > 1E+5) {
            body.alive = false;
            console.log(`Body Eliminated: ${body.name}`);
            return;
        }

        if (a > 0.0) {
            e = arg(Math.atan2(
                $x.dot($v) / Math.sqrt(a * GMM),
                1.0 - $x.length() / a
            ));

            // Kepler's Equation
            M = arg(e - $n.length() * Math.sin(e));
        } else {
            e = $n.length();
        }

        Object.assign(body, {
            a: a,
            e: e,
            i: i,
            W: c,
            w: w,
            M: M
        })
    }

    Perturbations() {
        /* ! Compute Mutual Perturbations in one step
        */
        var body_i, body_j;

        for (let body of this.bodies) {
            // If the i-th element was eliminated
            if (!body.alive) continue;

            body.$f.copy($x);
            body.$f.multiplyScalar(body.m / Math.pow(body.$x.length(), 3.0));
        }

        var $dx, $p, $q;

        $dx = new THREE.Vector3();
        $p = new THREE.Vector3();
        $p = new THREE.Vector3();

        var N = this.N();

        for (let i = 0; i < N; i++) {
            // Retrieve the i-th element
            body_i = this.bodies[i];

            // If the i-th element was eliminated.
            if (!body_i.alive) continue;

            for (let j = i + 1; j < N; j++) {
                // Retrieve the j-th element
                body_j = this.bodies[j];

                // If the j-th element was eliminated.
                if (!body_j.alive) continue;

                // Computations
                $dx.subVectors(body_i.$x, body_j.$x);
                $dx.divideScalar(Math.pow($dx.lengthSq(), 1.5));

                $p.setScalar(0.0);
                $q.setScalar(0.0);

                $p.addScaledVector($dx, +body_i.m);
                $q.addScaledVector($dx, -body_j.m);

                $p.sub(body_i.$f);
                $q.sub(body_j.$f);

                body_j.$v.addScaledVector($p, this.dt);
                body_i.$v.addScaledVector($q, this.dt);
            }
        }
    }

    KeplerMotion() {
        // * Advance Keplerian Motion Half Step
        // ! Orbital Elements
        var a;

        var dt;

        dt = 0.5 * this.dt;

        for (let body of this.bodies) {
            // If the i-th element was eliminated
            if (!body.alive) continue;

            a = 1.0 / ((2.0 / body.$x.length()) - (body.$v.lengthSq() / (this.GM0 + body.m)));

            if (a < 0.0 || a > 1E+5) {
                // Eliminate the body
                body.alive = false;
                console.log(`Body Eliminated: ${body.name}`);
                continue;
            }

            // this.MovRel(body);

            // ! Update parameter 'a'
            body.a = a;
        }
    }

    MovRel(body) {
        let GGM = this.GM0() + body.m;

        let r = Vector.norm(body.$x);

        let u = 2.0 / r;
        let v = Vector.norm2(body.$v) / GGM;

        let a = 1.0 / (u - v);
        let n = Math.sqrt(GGM / Math.pow(a, 3));

        let s = Vector.dot(body.$x, body.$v) / (n * Math.pow(a, 2));
        let c = 1.0 - (r / a);

        body.e = s * s + c * c;

        let dx, fp;
        [dx, s, c, fp] = this.KeplerRel(s, c, n);

        let f = 1.0 + (a / r) * (c - 1.0);
        let g = this.dt + (s - dx) / n;

        let F = -a * n * s / (r * fp);
        let G = 1.0 + (c - 1.0) / fp;

        body.$x = Vector.add(
            Vector.mul(body.$x, f),
            Vector.mul(body.$v, g)
        );
        body.$v = Vector.add(
            Vector.mul(body.$x, F),
            Vector.mul(body.$v, G)
        );
    }

    KeplerRel(s_, c_, n_) {
        // Solve Kepler Equation
        let x0 = n_ * this.dt;
        let dx, s, c, fp = [0.0, 0.0, 0.0];
        let x = x0;
        do {
            s = Math.sin(x);
            c = Math.cos(x);

            let f = x - c_ * s + s_ * (1 - c) - x0;

            fp[0] = 1.0 - c_ * c + s_ * s;
            fp[1] = c_ * s + s_ * c;
            fp[2] = c_ * c - s_ * s;

            dx = -f / fp[0];
            dx = -f / (fp[0] + dx * (fp[1] / 2.0));
            dx = -f / (fp[0] + dx * (fp[1] / 2.0 + dx * fp[2] / 6.0));

            x += dx;
        } while (Math.abs(dx) > this.TOL(13));

        return [dx, s, c, fp[0]];
    }

    SolveKepler(M, e) {
        var E, Ei, s, c, u, v, w;
        
        M = arg(M);    
        E = M;
        do {
            Ei = E;
    
            s = Math.sin(Ei);
            c = Math.cos(Ei);
    
            u = (Ei - e * s - M) / (1.0 - e * c);
            v = (Ei - u);
            w = v / (1 - u * e * s);
    
            E = (v + w) / 2.0;      
        } while (Math.abs(E - Ei) > 1E-14);
    
        return E;
    }

    html(id) {
        let element = document.getElementById(id);
        element.innerHTML = `\
        <p>Total Bodies: <b>${this.N()}</b> (<i>${this.ALIVE()} alive</i>)</p>
        <p>Time Span: <b>${this.span}</b> years</p>
        <p>Time Lapse: <b>${this.dt}</b> years</p>`
    }
}