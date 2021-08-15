/* 
* orbe.js
* by Pedro Maciel Xavier @ github.com/pedromxavier
*
* inspired by ORBE from Tabare Gallardo @ www.astronomia.edu.uy/orbe
*/

// Simple Vector Type
class Vector {
    constructor(x = 0.0, y = 0.0, z = 0.0) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    set(x = null, y = null, z = null) {
        this.x = x === null ? this.x : x;
        this.y = y === null ? this.y : y;
        this.z = z === null ? this.z : z;
    }

    static norm2(u) {
        if (u instanceof this) {
            return u.x * u.x + u.y * u.y + u.z * u.z;
        } else if (typeof u === 'number') {
            return u * u;
        } else {
            return undefined;
        }
    }

    static norm(u) {
        if (u instanceof this) {
            return Math.sqrt(this.norm2(u));
        } else if (typeof u === 'number') {
            return Math.abs(u);
        } else {
            return undefined;
        }
    }

    static unit(u) {
        if (u instanceof this) {
            return this.div(u, this.norm(u));
        } else {
            return undefined;
        }
    }

    static dot(u, v) {
        if (u instanceof this && v instanceof this) {
            return u.x * v.x + u.y * v.y + u.z * v.z;
        } else {
            return this.mul(u, v);
        }
    }

    static cross(u, v) {
        if (u instanceof this && v instanceof this) {
            return new this(
                u.y * v.z - u.z * v.y,
                u.x * v.z - u.z * v.x,
                u.x * v.y - u.y * v.x
            )
        } else {
            return this.mul(u, v);
        }
    }

    static mul(u, v) {
        if (u instanceof this && v instanceof this) {
            return new this(
                u.x * v.x,
                u.y * v.y,
                u.z * v.z,
            );
        } else if (u instanceof this && typeof v === 'number') {
            return new this(
                u.x * v,
                u.y * v,
                u.z * v,
            );
        } else if (typeof u === 'number' && v instanceof this) {
            return new this(
                v.x * u,
                v.y * u,
                v.z * u,
            );
        } else if (typeof u === 'number' && typeof v === 'number') {
            return u * v;
        } else {
            return undefined;
        }
    }

    imul(v) {
        if (v instanceof this) {
            this.x *= v.x;
            this.y *= v.y;
            this.z *= v.z;
        } else if (typeof v === 'number') {
            this.x *= v;
            this.y *= v;
            this.z *= v;
        }
    }

    static add(u, v) {
        if (u instanceof this && v instanceof this) {
            return new this(
                u.x + v.x,
                u.y + v.y,
                u.z + v.z,
            );
        } else if (u instanceof this && typeof v === 'number') {
            return new this(
                u.x + v,
                u.y + v,
                u.z + v,
            );
        } else if (typeof u === 'number' && v instanceof this) {
            return new this(
                v.x + u,
                v.y + u,
                v.z + u,
            );
        } else if (typeof u === 'number' && typeof v === 'number') {
            return u + v;
        } else {
            return undefined;
        }
    }

    iadd(v) {
        if (v instanceof Vector) {
            this.x += v.x;
            this.y += v.y;
            this.z += v.z;
            return;
        } else if (typeof v === 'number') {
            this.x += v;
            this.y += v;
            this.z += v;
            return;
        } else {
            return;
        }
    }

    static sub(u, v) {
        if (u instanceof this && v instanceof this) {
            return new this(
                u.x - v.x,
                u.y - v.y,
                u.z - v.z,
            );
        } else if (u instanceof this && typeof v === 'number') {
            return new this(
                u.x - v,
                u.y - v,
                u.z - v,
            );
        } else if (typeof u === 'number' && v instanceof this) {
            return new this(
                v.x - u,
                v.y - u,
                v.z - u,
            );
        } else if (typeof u === 'number' && typeof v === 'number') {
            return u - v;
        } else {
            return undefined;
        }
    }

    isub(v) {
        if (v instanceof this) {
            this.x -= v.x;
            this.y -= v.y;
            this.z -= v.z;
        } else if (typeof v === 'number') {
            this.x -= v;
            this.y -= v;
            this.z -= v;
        }
    }

    static neg(u) {
        if (u instanceof this) {
            return new this(
                -u.x,
                -u.y,
                -u.z
            );
        } else if (typeof u === 'number') {
            return -u;
        } else {
            return undefined;
        }
    }

    ineg() {
        this.x = -this.x;
        this.y = -this.y;
        this.z = -this.z;
        return;
    }

    static div(u, v) {
        if (u instanceof this && v instanceof this) {
            return new this(
                u.x / v.x,
                u.y / v.y,
                u.z / v.z,
            );
        } else if (u instanceof this && typeof v === 'number') {
            return new this(
                u.x / v,
                u.y / v,
                u.z / v,
            );
        } else if (typeof u === 'number' && v instanceof this) {
            return new this(
                v.x / u,
                v.y / u,
                v.z / u,
            );
        } else if (typeof u === 'number' && typeof v === 'number') {
            return u / v;
        } else {
            return undefined;
        }
    }

    idiv(v) {
        if (v instanceof this) {
            this.x /= v.x;
            this.y /= v.y;
            this.z /= v.z;
        } else if (typeof v === 'number') {
            this.x /= v;
            this.y /= v;
            this.z /= v;
        }
    }

    toString() {
        return `[${this.x}, ${this.y}, ${this.z}]`;
    }
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
function SIGN(A, B) {
    // FORTRAN SIGN(A, B)
    return (B >= 0.0) ? Math.abs(A) : -Math.abs(A);
}

function arg(x) {
    if (typeof x === 'number') {
        x %= TWO_PI;
        return (x < 0.0) ? (x + TWO_PI) : x;
    } else {
        return undefined;
    }
}

function degrees(x) {
    if (typeof x === 'number') {
        return x * (PI / 180.0);
    } else {
        return undefined;
    }
}

// Orbe Constants
const YEAR = 365.25;
const GAU = 0.01720209895;
const GM = Math.pow(GAU * YEAR, 2);

const NTT = 100;

class Element {
    constructor(data) {
        this.a = data.a;
        this.e = data.e;
        this.i = data.i;
        this.n = data.n;
        this.w = data.w;
        this.m = data.m;
        this.v = data.v;
        this.name = data.name;

        // Assert Data Consistency
        this.verify();

        // Position and Velocity
        this.$x = new Vector(0.0, 0.0, 0.0);
        this.$v = new Vector(0.0, 0.0, 0.0);

        // Leaning / Inclination
        this.p = 0.0; // ?       [SA]
        this.q = 0.0; // ?       [SE]
        this.l = 0.0; // Leaning [INCLI]
        this.g = 0.0; // ?       [?]

        this.alive = true;
    }

    frame() {
        return {
            q: this.q, // ?       [SA]
            p: this.p, // ?       [SE]
            l: this.l, // Leaning [INCLI]
            c: this.c, // ?       [ON]
            w: this.w, // ?       [W]
            r: this.y, // ?       [AMED]
        }
    }

    verify() {
        if (typeof this.a !== 'number' || this.a <= 0.0) {
            throw `Error: missing positive number for 'a' on element data.`
        }

        if (typeof this.e !== 'number' || this.e <= 0.0) {
            throw `Error: missing positive number for 'e' on element data.`
        }

        if (typeof this.i !== 'number' || this.i < 0.0) {
            throw `Error: missing non-negative number for 'i' on element data.`
        }

        if (typeof this.n !== 'number' || this.n <= 0.0) {
            throw `Error: missing positive number for 'n' on element data.`
        }

        if (typeof this.w !== 'number' || this.w <= 0.0) {
            throw `Error: missing positive number for 'w' on element data.`
        }

        if (typeof this.m !== 'number' || this.m <= 0.0) {
            throw `Error: missing positive number for 'm' on element data.`
        }

        if (typeof this.v !== 'number' || this.v <= 0.0) {
            throw `Error: missing positive number for 'v' on element data.`
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
        this.elements = data.elements;

        // Assert Data Consistency
        this.verify();

        this.t_min = null;

        // Time Control Quantities
        this.dt = null;
        this.time = 0.0;
        this.step = 1E-5;

        // Initialize Simulation
        this.init();
    }

    static load(path) {
        loadJson(path, (data) => {
            new this(data);
        });
    }

    verify() {
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

        if (!Array.isArray(this.elements)) {
            throw `Error: missing array for 'elements'.`
        } else {
            for (let i = 0; i < data.elements.length; i++) {
                // Cast Object to Element type.
                this.elements[i] = new Element(this.elements[i]);
            }
        }
    }

    // Properties
    static TOL(n) {
        // 'n' decimal digits of error tolerance
        return Math.pow(10.0, -n);
    }

    TOL(n) {
        return this.constructor.TOL(n);
    }

    GM0() {
        return GM * this.mass;
    }

    STEP() {
        // ! Time Step tMIN/40 Aproximately
        return this.t_min / 40.0;
    }

    N() {
        return this.elements.length;
    }

    ALIVE() {
        return this.elements.reduce((s, el) => (el.alive ? s + 1 : s), 0);
    }

    // Main routines
    init() {
        for (let el of this.elements) {
            // ! Mean Motion
            el.g = Math.sqrt(GM * (this.mass + el.v) / Math.pow(el.a, 3));

            // ! Minimum Orbital Period
            el.t = TWO_PI / el.g;
            if (this.t_min === null) {
                this.t_min = el.t;
            } else {
                this.t_min = Math.min(this.t_min, el.t);
            }

            // ! Perihelion Passage in Days
            el.m = GM * el.v;

            // ! Initial Position and Velocity
            el.$x = null; // Position
            el.$v = null; // Velocity
            el.$f = null; // ????????
            this.PositionAndVelocity(el);
        }

        // Initial step
        for (let el of this.elements) {
            this.ComputeElements(el);
        }

        // This part doesn't make any sense
        for (let i = 0; i < 7; i++) {
            if (this.step < this.STEP()) {
                this.dt = this.step;
            }
            if (2.0 * this.step < this.STEP()) {
                this.dt = 2.0 * this.step;
            }
            if (5.0 * this.step < this.STEP()) {
                this.dt = 5.0 * this.step;
            }
            this.step *= 10.0;
        }

        this.lapse = Math.floor(this.lapse / this.dt) * this.dt;

        // ! Integrate Backwards
        if (this.span < 0.0) this.dt = -this.dt;
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

            if (Math.abs(time - SIGN(this.lapse, this.dt)) < this.TOL(5)) {
                for (let el of this.elements) {
                    if (el.alive) this.ComputeElements(el);
                }
                yield { frame: frame++, elements: this.elements.map((el) => (el.frame())) };
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

    // Subroutines
    PositionAndVelocity(el) {
        /*
        **
        */
        // SUBROUTINE POVE(a0,E0,XI0,  XN0,  W0, eme0, ene,POS,VEL)
        // SUBROUTINE POVE( A, E,  I,ANODE,PERI,    M,   N,  X, XP)

        let e = this.SolveKepler(el);
        let f = 2.0 * Math.atan(Math.sqrt((1.0 + el.e) / (1.0 - el.e)) * Math.tan(e / 2.0));

        let R = el.a * (1.0 - el.e * Math.cos(e));
        let B = el.v * el.a / Math.sqrt(1.0 - el.e * el.e);

        let $c = new Vector(Math.cos(el.n), Math.cos(el.i), Math.cos(el.w));
        let $s = new Vector(Math.sin(el.n), Math.sin(el.i), Math.sin(el.w));

        // VECTORS P AND Q
        let $p = new Vector(
            $c.x * $c.z - $s.x * $c.y * $s.z,
            $s.x * $c.z + $c.x * $c.y * $s.z,
            $s.y * $s.z,
        )
        let $q = new Vector(
            -$c.x * $s.z - $s.x * $c.y * $c.z,
            -$s.x * $s.z + $c.x * $c.y * $c.z,
            $s.y * $c.z
        )

        // Coefficients
        let [a, b] = [R * Math.cos(f), R * Math.sin(f)];
        let [c, d] = [-B * Math.sin(f), B * (el.e + Math.cos(f))];

        // POSITION AND VELOCITY
        el.$x = Vector.add(Vector.mul($p, a), Vector.mul($q, b));
        el.$v = Vector.add(Vector.mul($p, c), Vector.mul($q, d));
    }

    ComputeElements(el) {
        /* SUBROUTINE PLANO
        ** Calculate Orbital Elements
        */

        let $z = Vector.cross(el.$x, el.$v);
        let $w = new Vector($z.x, $z.y, 0.0);

        let s = Math.atan2(Vector.norm($w), $z.z);
        let c = arg(Math.atan2($z.x, -$z.y));

        let l = s;

        let GMM = this.GM0() + el.m;

        let $e = Vector.sub(
            Vector.div(Vector.cross(el.$v, $z), GMM),
            Vector.unit(el.$x)
        );

        let w;
        if (Math.sin(s) < this.TOL(12)) {
            // ! If Inclination is 0 or 180 deg
            // ! this case w is the longitude of perihelion
            c = 0.0;
            w = arg(Math.atan2($e.y, $e.x));
        } else {
            w = arg(Math.atan2(
                $e.z / Math.sin(s),
                $e.x * Math.cos(c) + $e.y * Math.sin(c)
            ));
        }

        let u = 2.0 / Vector.norm(el.$x);
        let v = Vector.norm2(el.$v) / GMM;

        let a = 1.0 / (u - v);

        if (a < 0.0 || a > 1E+5) {
            el.alive = false;
            console.log(`Body Eliminated: ${el.name}`);
            return;
        }

        el.p = a;
        el.q = Vector.norm($e)

        let y;

        if (a > 0.0) {
            let e = arg(Math.atan2(
                Vector.dot(el.$x, el.$v) / Math.sqrt(a * GMM),
                1.0 - Vector.norm(el.$x) / a
            ));

            // Kepler's Equation
            y = arg(e - Vector.norm($e) * Math.sin(e));
        }


        el.l = l; // ! [INCLI]
        el.c = c; // ? [ON]
        el.w = w; // ? [W]
        el.y = y; // ? [AMED]
    }

    KeplerMotion() {
        // ! Advance Keplerian Motion Half Step
        for (let i = 0; i < this.N(); i++) {
            // Retrieve the i-th element
            let e_i = this.elements[i];

            // If the i-th element was eliminated
            if (!e_i.alive) continue;

            let u = 2.0 / Vector.norm(e_i.$x);
            let v = Vector.norm2(e_i.$v) / (this.GM0() + e_i.m);

            let a = 1.0 / (u - v);

            if (a < 0.0 || a > 1E+5) {
                // Eliminate the body
                e_i.alive = false;
                console.log(`Body Eliminated: ${e_i.name}`);
                continue;
            }

            this.MovRel(e_i);

            // Update parameter 'a' ??
            //el.a = a;
        }
    }

    Perturbations() {
        /* ! Compute Mutual Perturbations in one step
        */
        var e_i, e_j;

        for (let i = 0; i < this.N(); i++) {
            // Retrieve the i-th element
            e_i = this.elements[i];

            // If the i-th element was eliminated
            if (!e_i.alive) continue;

            e_i.$f = Vector.mul(e_i.m / Math.pow(Vector.norm(e_i.$x), 3.0), e_i.$x);
        }

        for (let i = 0; i < this.N(); i++) {
            // Retrieve the i-th element
            e_i = this.elements[i];

            // If the i-th element was eliminated.
            if (!e_i.alive) continue;

            for (let j = i + 1; j < this.N(); j++) {
                // Retrieve the j-th element
                e_j = this.elements[j];

                // If the j-th element was eliminated.
                if (!e_j.alive) continue;

                // Computations
                let $dx = Vector.sub(e_i.$x, e_j.$x);
                $dx = Vector.div($dx, Math.pow(Vector.norm2($dx), 1.5)); // 1.5 = 3/2

                let $p = Vector.sub(Vector.mul(e_i.m, $dx), e_i.$f);
                let $q = Vector.sub(Vector.mul(-e_j.m, $dx), e_j.$f);

                e_j.$v.iadd(Vector.mul(this.dt, $p));
                e_i.$v.iadd(Vector.mul(this.dt, $q));
            }
        }
    }

    SolveKepler(el) {
        // Solve Kepler Equation
        // M = E - e * sin(E)
        var m, e, e_k;
        m = arg(el.m);
        e = m
        do {
            e_k = e;

            let s = Math.sin(e_k);
            let c = Math.cos(e_k);

            let u = (e_k - el.e * s - m) / (1.0 - el.e * c);
            let v = e_k - u;
            let w = v / (1 - u * el.e * s);

            e = (v + w) / 2.0;
        } while (Math.abs(e - e_k) > this.TOL(14));

        return e;
    }

    MovRel(el) {
        let GGM = this.GM0() + el.m;

        let r = Vector.norm(el.$x);

        let u = 2.0 / r;
        let v = Vector.norm2(el.$v) / GGM;

        let a = 1.0 / (u - v);
        let n = Math.sqrt(GGM / Math.pow(a, 3));

        let s = Vector.dot(el.$x, el.$v) / (n * Math.pow(a, 2));
        let c = 1.0 - (r / a);

        el.e = s * s + c * c;

        let dx, fp;
        [dx, s, c, fp] = this.KeplerRel(s, c, n);

        let f = 1.0 + (a / r) * (c - 1.0);
        let g = this.dt + (s - dx) / n;

        let F = -a * n * s / (r * fp);
        let G = 1.0 + (c - 1.0) / fp;

        el.$x = Vector.add(
            Vector.mul(el.$x, f),
            Vector.mul(el.$v, g)
        );
        el.$v = Vector.add(
            Vector.mul(el.$x, F),
            Vector.mul(el.$v, G)
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

    html(id) {
        let element = document.getElementById(id);
        element.innerHTML = `\
        <p>Total Bodies: <b>${this.N()}</b> (<i>${this.ALIVE()} alive</i>)</p>
        <p>Time Span: <b>${this.span}</b> years</p>
        <p>Time Lapse: <b>${this.dt}</b> years</p>`
    }
}