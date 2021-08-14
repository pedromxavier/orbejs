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

    toString() {
        return `[${this.x}, ${this.y}, ${this.z}]`;
    }
}

// Math Constants
const PI = Math.PI;
const TWO_PI = 2.0 * Math.PI;

// Math Functions
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

class Orbe {

    constructor(data) {
        // From data object
        this.mass = data.mass;
        this.span = data.span;
        this.lapse = data.lapse;
        this.elements = data.elements;

        this.peorb_min = null;

        // Time Control Quantities
        this.dt = null;
        this.time = 0.0;
        this.step = 1E-5;
    }

    // Properties
    static TOL(n) {
        // 'n' decimal digits of error tolerance
        return Math.pow(10.0, -n);
    }

    GM0() {
        return GM * this.mass;
    }

    STEP() {
        // ! Time Step PEORBMIN/40 Aproximately
        return this.peorb_min / 40.0;
    }

    N() {
        return this.elements.length;
    }

    // I/O
    static read(data) {
        /*
        *  See './orbeini.json' for format patterns.
        */

        // Check data consistency
        if ((!data instanceof Object)) {
            throw `Error: 'data' must be an Object.`;
        }

        if (!('mass' in data) || typeof data.mass !== 'number' || data.mass <= 0.0) {
            throw `Error: missing positive number for 'mass'.`
        }

        if (!('span' in data) || typeof data.span !== 'number' || data.span <= 0.0) {
            throw `Error: missing positive number for 'span'.`
        }

        if (!('lapse' in data) || typeof data.lapse !== 'number' || data.lapse <= 0.0) {
            throw `Error: missing positive number for 'lapse'.`
        }
            // ?                    ?
            // ?                    ?
            // ?                    ?
            // Elements
            // for (let i=0; i<this.N; i++) {
            //     this.elements[i].i = i;
            //     this.elements[i].w = degrees(this.elements[i].w);
            //     this.elements[i].x_i = degrees(this.elements[i].x_i);
            //     this.elements[i].x_n = degrees(this.elements[i].x_n);
            //     this.elements[i].eme = degrees(this.elements[i].eme);
            // }


            let orbe = new this(data);
        return null;
    }

    // Main routines
    init() {
        for (let i = 0; i < this.N; i++) {
            // Retrieve element
            let element = this.elements[i];

            // Flag for dynamic element removal
            element.alive = true;

            // Conversion from radians to degrees
            element.w = degrees(element.w);
            element.x_i = degrees(element.x_i);
            element.x_n = degrees(element.x_n);
            element.eme = degrees(element.eme);
            // ! Mean Motion
            element.ene = Math.sqrt(GM * (this.mass + element.vama) / Math.pow(element.a, 3));

            // ! Minimum Orbital Period
            element.peorb = TWO_PI / element.ene;
            if (this.peorb_min === null) {
                this.peorb_min = element.peorb;
            } else {
                this.peorb_min = Math.min(this.peorb_min, element.peorb);
            }

            // ! Perihelion Passage in Days
            element.m = GM * element.vama;

            // ! Initial Position and Velocity
            element.$x = null; // Position
            element.$v = null; // Velocity
            element.$f = null; // ????????
            this.PositionAndVelocity(element);

            element.r = Vector.norm(element.$x);
        }

        // This part doesn't make any sense
        for (let i = 0; i<7; i++) {
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
    }

    run() {
        // Initialize parameters
        this.init();
    }

    * render() {
        // Initial step
        for (let i = 0; i < this.N(); i++) {
            this.ComputeElements(this.elements[i]);
        }
    }

    // Subroutines
    PositionAndVelocity(element) {
        /*
        **
        */

        let e = this.SolveKepler(element);
        let f = 2.0 * Math.atan(Math.sqrt((1.0 + element.e) / (1.0 - element.e)) * Math.tan(e / 2.0));

        let R = element.a * (1.0 - element.e * Math.cos(e));
        let B = N_ *element.a / Math.sqrt(1.0 - element.e * element.e);

        let $c = new Vector(Math.cos(ANODE_), Math.cos(I_), Math.cos(PERI_));
        let $s = new Vector(Math.sin(ANODE_), Math.sin(I_), Math.sin(PERI_));

        // VECTORS P AND Q
        let $p = new Vector(
            $c.x * $c.z - $s.x * $c.y * S.z,
            $s.x * $c.z + $c.x * $c.y * S.z,
            $s.y * $s.z,
        )
        let $q = new Vector(
            -$c.x * $s.z - $s.x * $c.y * $c.z,
            -$s.x * $s.z + $c.x * $c.y * $c.z,
            $s.y * $c.z
        )

        // Coefficients
        let [a, b] = [R * Math.cos(f), R * Math.sin(f)];
        let [c, d] = [-B * Math.sin(f), B * (E_ + Math.cos(f))];

        // POSITION AND VELOCITY
        element.$x = Vector.add(Vector.mul($p, a), Vector.mul($q, b));
        element.$v = Vector.add(Vector.mul($p, c), Vector.mul($q, d));
    }

    ComputeElements(element) {
        /* SUBROUTINE PLANO
        ** Calculate Orbital Elements
        */

        let rx = Vector.norm(element.$x);
        let rv = Vector.norm(element.$v);

        let $z = Vector.cross(element.$x, element.$v);
        let $w = new Vector($z.x, $z.y, 0.0);

        let YN = Math.atan2(Vector.norm($w), $z.z);
        let INCLI = YN
        let ON = Math.atan2($z.x, -$z.y);

        if (ON < 0.0) {
            ON += TWO_PI;
        }

        let CMU = GM0_ + $MPLA_[i_];

        let $e = Vector.sub(
            Vector.div(Vector.cross($v, $z), CMU),
            Vector.div($x, rx)
        );

        let re = Vector.norm($e);

        let SE = re
        // IF INCLINATION IS 0 OR 180
        // IN THIS CASE W IS THE LONGITUDE OF PERIHELION
        if (Math.sin(YN) < this.TOL(12)) {
            ON = 0.0;
            W = Math.atan2($e.y, $e.x);
        } else {
            let SEW = $e.z / Math.sin(YN);
            let COW = $e.x * Math.cos(ON); + $e.y * Math.sin(ON);
            W = Math.atan2(SEW, COW);
        }

        if (W < 0.0) {
            W += TWO_PI;
        }

        let A = 1.0 / ((2.0 / rx) - (rv / CMU));

        if (A < 0.0 || A > 1E+5) {
            // WE ELIMINATE THE BODY PUTTING AN ABSURDE MASS
            $MPLA_[i_] = 1.3E-30;
            SA = 90000.0;
            console.log(`BODY ELIMINATED: ${i_}`);
            return;
        }

        let SA = A;
        AE = 0.0;

        let AMED;

        if (A > 0.0) {
            SEE = Vector.div(Vector.dot($x, $v), Math.sqrt(A * CMU));
            COE = 1.0 - rx / A;
            AE = Math.atan2(SEE, COE);
            if (AE < 0.0) {
                AE += TWO_PI;
            }
            // Kepler's Equation
            AMED = (AE - re * Math.sin(AE)) % TWO_PI;

            if (AMED < 0.0) {
                AMED += TWO_PI;
            }
        }


        let INCLI = degrees(INCLI);
        let ON = degrees(ON);
        let W = degrees(W);
        let AMED = degrees(AMED);

        return [INCLI, AMED, ON, SE, SA, W];
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
                console.log(`Body Eliminated: [${i}] ${e_i.name}`);
                continue;
            } 

            this.MovRel(e_i);

            // Update parameter 'a' ??
            element.a = a;
        }
    }

    Perturbations() {
        /* ! Compute Mutual Perturbations in one step
        */
        for (let i = 0; i < this.N(); i++) {
            // Retrieve the i-th element
            let e_i = this.elements[i];

            // If the i-th element was eliminated
            if (!e_i.alive) continue;

            e_i.$f = Vector.mul(e_i.m / Math.pow(e_i.r, 3.0), e_i.$x);
        }

        for (let i = 0; i < this.N(); i++) {
            // Retrieve the i-th element
            let e_i = this.elements[i];

            // If the i-th element was eliminated.
            if (!e_i.alive) continue;

            for (let j = i + 1; j < this.N(); j++) {
                // Retrieve the j-th element
                let e_j = this.elements[j];

                // If the j-th element was eliminated.
                if (!e_j.alive) continue;

                // Computations
                let $dx = Vector.sub(e_i.$x, e_j.$x);
                $dx = Vector.div($dx, Math.pow(Vector.norm2($dx), 1.5)); // 1.5 = 3/2

                let $p = Vector.sub(Vector.mul(e_i.m, $dx), e_i.$f);
                let $q = Vector.sub(Vector.mul(-e_j.m, $dx), e_j.$f);

                e_j.$v = Vector.add(e_j.$v, Vector.mul(this.dt, $p));
                e_i.$v = Vector.add(e_i.$v, Vector.mul(this.dt, $q));
            }
        }
    }

    SolveKepler(element) {
        // Solve Kepler Equation
        // M = E - e * sin(E)
        let m = element.m % TWO_PI;
        let e_k, e = m;
        do {
            e_k = e;

            let s = Math.sin(e_k);
            let c = Math.cos(e_k);

            let u = (e_k - element.e * s - m) / (1.0 - element.e * c);
            let v = e_k - u;
            let w = v / (1 - u * element.e * s);

            let e = (v + w) / 2.0;
        } while (Math.abs(e - e_k) > this.TOL(14));

        return e;
    }

    MovRel(element) {
        let GGM = this.GM0() + element.m;

        let r = Vector.norm(element.$x);

        let u = 2.0 / r;
        let v = Vector.norm2(element.$v) / GGM;

        let a = 1.0 / (u - v);
        let n = Math.sqrt(GGM / Math.pow(a, 3));

        let s = Vector.dot(element.$x, element.$v) / (n * Math.pow(a, 2));
        let c = 1.0 - (r / a);
        
        element.e = s * s + c * c;

        let [dx, s, c, fp] = this.KeplerRel(s, c, n);

        let f = 1.0 + (a / r) * (c - 1.0);
        let g = this.dt + (s - dx) / n;

        let F = -a * n * s / (r * fp);
        let G = 1.0 + (c - 1.0) / fp;

        element.$x = Vector.add(
            Vector.mul(element.$x, f),
            Vector.mul(element.$v, g)
            );
        element.$v = Vector.add(
            Vector.mul(element.$x, F),
            Vector.mul(element.$v, G)
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
}