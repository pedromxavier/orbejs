/* 
* orbe.js
* by Pedro Maciel Xavier @ github.com/pedromxavier
*
* inspired by ORBE from Tabare Gallardo @ www.astronomia.edu.uy/orbe
*/

function loadJSON(path, callback) {
    /**
    * * Asynchronous JSON loading.
    * *
    * * 
    **/
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

class Vector3 {
    /**
    * * Creating a new vector object is very expensive.
    * *
    * *
    **/
    static new(x = 0.0, y = 0.0, z = 0.0) {
        return new Float32Array([x, y, z]);
    }

    static cache(key, x = 0.0, y = 0.0, z = 0.0) {
        if (this.__cache__ === undefined) {
            this.__cache__ = {};
        }
        return this.__cache__[key] = this.new(x, y, z);
    }

    static load(key) {
        return this.__cache__[key];
    }

    static set($w, x = 0.0, y = 0.0, z = 0.0) {
        $w[0] = x;
        $w[1] = y;
        $w[2] = z;
        return $w;
    }

    static copy($w, $u) {
        $w[0] = $u[0];
        $w[1] = $u[1];
        $w[2] = $u[2];
        return $w;
    }

    static add($w, $u, $v) {
        $w[0] = $u[0] + $v[0];
        $w[1] = $u[1] + $v[1];
        $w[2] = $u[2] + $v[2];
        return $w;
    }

    static sub($w, $u, $v) {
        $w[0] = $u[0] - $v[0];
        $w[1] = $u[1] - $v[1];
        $w[2] = $u[2] - $v[2];
        return $w;
    }

    static mul($w, $u, $v) {
        $w[0] = $u[0] * $v[0];
        $w[1] = $u[1] * $v[1];
        $w[2] = $u[2] * $v[2];
        return $w;
    }

    static div($w, $u, $v) {
        $w[0] = $u[0] / $v[0];
        $w[1] = $u[1] / $v[1];
        $w[2] = $u[2] / $v[2];
        return $w;
    }

    static dot($u, $v) {
        return $u[0] * $v[0] + $u[1] * $v[1] + $u[2] * $v[2];
    }

    static cross($w, $u, $v) {
        $w[0] = $u[1] * $v[2] - $u[2] * $v[1];
        $w[1] = $u[2] * $v[0] - $u[0] * $v[2];
        $w[2] = $u[0] * $v[1] - $u[1] * $v[0];
        return $w;
    }

    static span($w, a, $u, b, $v) {
        $w[0] = a * $u[0] + b * $v[0];
        $w[1] = a * $u[1] + b * $v[1];
        $w[2] = a * $u[2] + b * $v[2];
        return $w;
    }

    static scale($w, a, $u) {
        $w[0] = a * $u[0]
        $w[1] = a * $u[1]
        $w[2] = a * $u[2]
        return $w;
    }

    static norm($u) {
        return Math.sqrt($u[0] * $u[0] + $u[1] * $u[1] + $u[2] * $u[2]);
    }

    static norm2($u) {
        return $u[0] * $u[0] + $u[1] * $u[1] + $u[2] * $u[2];
    }

    static unit($w, $u) {
        let u = this.norm($u);
        $w[0] = $u[0] / u
        $w[1] = $u[1] / u
        $w[2] = $u[2] / u
        return $w; 
    }
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

function sign(x, y) {
    return (y >= 0.0) ? Math.abs(x) : -Math.abs(x);
}

/** Orbe Constants
* * GM: (GAU * DAYS)^{2}
**/
const GM = Math.pow(0.01720209895 * 365.25, 2);

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
        this.$x = Vector3.new(); // ? [POS]
        this.$v = Vector3.new(); // ? [VEL]
        this.$f = Vector3.new(); // ? [FIND]

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
        console.time('Orbe-init');

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

        // ! Cache Auxiliary Vectors
        Vector3.cache('c');
        Vector3.cache('s');
        Vector3.cache('f');
        Vector3.cache('n');
        Vector3.cache('p');
        Vector3.cache('q');
        Vector3.cache('u');
        Vector3.cache('v');
        Vector3.cache('x');
        Vector3.cache('y');
        Vector3.cache('z');
        Vector3.cache('w');

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

        // Supposed to take about ~4ms
        console.timeEnd('Orbe-init');
        // console.log(this);
    }

    N() {
        return this.bodies.length;
    }

    ALIVE() {
        return this.bodies.reduce((s, body) => (body.alive ? s + 1 : s), 0);
    }

    // Main routines
    static load(path, element = null, callback = null) {
        loadJSON(path, (data) => {
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

            if (Math.abs(time - sign(this.lapse, this.dt)) < 1E-5) {
                for (let body of this.bodies) {
                    if (body.alive) this.ComputeElements(body);
                }
                yield { frame: frame++, bodies: this.bodies.map((body) => (body.frame())) };
                time = this.dt;
                continue;
            }
            if (((this.span - t) * sign(1.0, this.dt) > 0.0)) {
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
        // ! Other parameters
        var E, T, A, B, C, S;
        // ! Vectors
        var $c, $s, $p, $q;

        [a, e, i, w, M, n] = [body.a, body.e, body.i, body.w, body.M, body.n];
        
        E = this.SolveKeplerII(M, e);
        T = 2.0 * Math.atan(Math.sqrt((1.0 + e) / (1.0 - e)) * Math.tan(E / 2.0));

        A = a * (1.0 - e * Math.cos(E));
        B = n * a / Math.sqrt(1.0 - e * e);

        C = Math.cos(T);
        S = Math.sin(T);

        $c = Vector3.load('c');
        $s = Vector3.load('s');

        Vector3.set($c, Math.cos(n), Math.cos(i), Math.cos(w));
        Vector3.set($s, Math.sin(n), Math.sin(i), Math.sin(w));

        $p = Vector3.load('p');
        $q = Vector3.load('q');

        Vector3.set($p,
            $c[0] * $c[2] - $s[0] * $c[1] * $s[2],
            $s[0] * $c[2] + $c[0] * $c[1] * $s[2],
            $s[1] * $s[2],
        )
        Vector3.set($q,
            -$c[0] * $s[2] - $s[0] * $c[1] * $c[2],
            -$s[0] * $s[2] + $c[0] * $c[1] * $c[2],
            $s[1] * $c[2]
        )

        // POSITION AND VELOCITY
        Vector3.span(body.$x, A * C, $p, A * S, $q);
        Vector3.span(body.$v, -B * S, $p, B * (e + C), $q);
    }

    ComputeElements(body) {
        /* Calculate Orbital Elements
        ** 
        */
        // ! Orbital Elements
        var a, e, w, M;
        // ! Position & Velocity
        var $x, $v;
        // ! Other Parameters
        var c, s, GMM;
        // ! Other Vectors
        var $z, $w, $n, $u;

        $x = Vector3.load('x');
        $v = Vector3.load('v');
        Vector3.copy($x, body.$x);
        Vector3.copy($v, body.$v);
        
        $z = Vector3.load('z');
        Vector3.cross($z, $x, $v);
        $w = Vector3.load('w');
        Vector3.set($w, $z[0], $z[1], 0.0);

        c = arg(Math.atan2($z[0], -$z[1]));
        s = Math.atan2(Vector3.norm($w), $z[2]);

        GMM = this.GM0 + body.m;

        $n = Vector3.load('n');
        Vector3.cross($n, $v, $z);
        Vector3.scale($n, 1.0/GMM, $n);

        $u = Vector3.load('u');
        Vector3.unit($u, $x);
        
        Vector3.sub($n, $n, $u);

        if (Math.sin(s) < 1E-12) {
            // ! If Inclination is 0 or 180 deg
            // ! this case w is the longitude of perihelion
            c = 0.0;
            w = arg(Math.atan2($n[1], $n[0]));
        } else {
            w = arg(Math.atan2(
                $n[2] / Math.sin(s),
                $n[0] * Math.cos(c) + $n[1] * Math.sin(c)
            ));
        }

        a = 1.0 / ((2.0 / Vector3.norm($x)) - (Vector3.norm2($v) / GMM));

        if (a < 0.0 || a > 1E+5) {
            body.alive = false;
            console.log(`Body Eliminated: ${body.name}`);
            return;
        }

        if (a > 0.0) {
            e = arg(Math.atan2(
                Vector3.dot($x, $v) / Math.sqrt(a * GMM),
                1.0 - Vector3.norm($x) / a
            ));

            // Kepler's Equation
            M = arg(e - Vector3.norm($n) * Math.sin(e));
        } else {
            e = Vector3.norm($n);
        }

        Object.assign(body, {
            a: a,
            e: e,
            i: s,
            W: c,
            w: w,
            M: M
        })
    }

    KeplerMotion() {
        // * Advance Keplerian Motion Half Step
        // ! Orbital Elements
        var a, dt;

        dt = 0.5 * this.dt;

        for (let body of this.bodies) {
            // If the i-th element was eliminated
            if (!body.alive) continue;

            a = 1.0 / ((2.0 / Vector3.norm(body.$x)) - (Vector3.norm2(body.$v) / (this.GM0 + body.m)));

            if (a < 0.0 || a > 1E+5) {
                // Eliminate the body
                body.alive = false;
                console.log(`Body Eliminated: ${body.name}`);
                continue;
            }

            this.RelativeMotion(body, dt);

            // ! Update parameter 'a'
            body.a = a;
        }
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

        $dx = Vector3.new();
        $p = Vector3.new();
        $q = Vector3.new();

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

    RelativeMotion(body, dt) {
        var GGM, r, a, n;
        // ! Vectors
        var $x, $v;
        // ! Parameters
        var x, s, c, p;
        // ! Coefficients
        var f, g, F, G;

        $x = Vector3.load('x');
        $v = Vector3.load('v');
        Vector3.copy($x, body.$x);
        Vector3.copy($v, body.$v);

        GGM = this.GM0() + body.m;

        r = Vector.norm($x);
        a = 1.0 / ((2.0 / r) - Vector.norm2($v) / GGM);
        
        n = Math.sqrt(GGM / Math.pow(a, 3));

        s = Vector3.dot($x, $v) / (n * Math.pow(a, 2));
        c = 1.0 - (r / a);

        [x, s, c, p] = this.SolveKelperI(s, c, n, dt);

        f = 1.0 + (a / r) * (c - 1.0);
        g = dt + (s - x) / n;

        F = -a * n * s / (r * p);
        G = 1.0 + (c - 1.0) / p;

        Vector3.span(body.$x, f, $x, g, $v);
        Vector3.span(body.$v, F, $x, G, $v);

        body.e = s * s + c * c;
    }

    SolveKeplerI(S, C, n, dt) {
        // Solve Kepler Equation
        // ! Solution Steps
        var x, x0, dx;
        // ! Other Parameters
        var s, c, f;
        // ! Vector
        var $f;

        $f = Vector3.load('f');

        x0 = n * dt;
        x = x0;

        do {
            s = Math.sin(x);
            c = Math.cos(x);

            $f[0] = 1.0 - C * c + S * s;
            $f[1] = C * s + S * c;
            $f[2] = C * c - S * s;

            f = x - x0 - C * s + S * (1.0 - c);

            dx = -f / ($f[0]);
            dx = -f / ($f[0] + dx * ($f[1] / 2.0));
            dx = -f / ($f[0] + dx * ($f[1] / 2.0 + dx * $f[2] / 6.0));

            x += dx;
        } while (Math.abs(dx) > 1E-13);

        return [dx, s, c, $f[0]];
    }

    SolveKeplerII(M, e) {
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
        element.innerHTML = `
        <table>
            <tr>
                <td>Total Bodies</td>
                <td><b>${this.N()}</b> (<i>${this.ALIVE()} alive</i>)</td>
            </tr>
            <tr>
                <td>Time Span</td>
                <td><b>${this.span}</b> years</td>
            </tr>
            <tr>
                <td>Time Step</td>
                <td><b>${this.dt}</b> years</td>
            </tr>
        </table>`
    }
}