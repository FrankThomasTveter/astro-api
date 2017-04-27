console.log("Loading vector3.js");

/**
 * @author mrdoob / http://mrdoob.com/
 * @author *kile / http://kile.stravaganza.org/
 * @author philogb / http://blog.thejit.org/
 * @author mikael emtinger / http://gomo.se/
 * @author egraether / http://egraether.com/
 * @author WestLangley / http://github.com/WestLangley
 */

Vector3 = function ( x, y, z ) {
    this.x = x || 0;
    this.y = y || 0;
    this.z = z || 0;
};

Vector3.prototype = {

    constructor: Vector3,

    set: function ( x, y, z ) {

	this.x = x;
	this.y = y;
	this.z = z;

	return this;

    },

    setX: function ( x ) {

	this.x = x;

	return this;

    },

    setY: function ( y ) {
	
	this.y = y;

	return this;

    },

    setZ: function ( z ) {

	this.z = z;

	return this;

    },

    interpolate: function ( u , v , f ) {
	this.uf= 1-f;
	this.x = ( u.x * this.uf + v.x * f ); 
	this.y = ( u.y * this.uf + v.y * f ); 
	this.z = ( u.z * this.uf + v.z * f ); 
	u.len = u.length();
	v.len = v.length();
	this.len=this.length();
	if (this.len < 1e-10) {
	    if ( u.len > 1e-10) {
		u.fact=(u.len*this.uf - v.len*f)/u.len;
		this.copy(u).multiplyScalar(u.fact);
	    } else if (v.len > 1e-10) {
		v.fact=-(u.len*this.uf - v.len*f)/v.len;
		this.copy(v).multiplyScalar(v.fact);
	    } else {
		this.x=0.0;
		this.y=0.0;
		this.z=0.0;
	    }
	} else {
	    this.fact=(u.len*this.uf + v.len*f)/this.len;
	    this.multiplyScalar(this.fact);
	}
    },

    setSpherical: function ( r,lat,lon ) {
	this.r = r;
	this.lat = lat;
	this.lon = lon;
    },

    spherical2cartesian: function ( axis ) {
	if (axis !== undefined) {
  	    var xm=this.r*Math.cos(this.lat)*Math.cos(this.lon);
	    var ym=this.r*Math.cos(this.lat)*Math.sin(this.lon);
	    var zm=this.r*Math.sin(this.lat);
	    this.x = xm*axis.i.x + ym*axis.j.x + zm*axis.k.x;
	    this.y = xm*axis.i.y + ym*axis.j.y + zm*axis.k.y;
	    this.z = xm*axis.i.z + ym*axis.j.z + zm*axis.k.z;
	} else {
  	    this.x=this.r*Math.cos(this.lat)*Math.cos(this.lon);
	    this.y=this.r*Math.cos(this.lat)*Math.sin(this.lon);
	    this.z=this.r*Math.sin(this.lat);
	}
	return this;
    },
    cartesian2rotated: function (axis) {
	if (axis !== undefined) {
	    var xm = this.x*axis.i.x + this.y*axis.i.y + this.z*axis.i.z;
	    var ym = this.x*axis.j.x + this.y*axis.j.y + this.z*axis.j.z;
	    var zm = this.x*axis.k.x + this.y*axis.k.y + this.z*axis.k.z;
	    this.set(xm,ym,zm);
	}
	return this;
    },
    rotated2cartesian: function (axis) {
	if (axis !== undefined) {
	    var xm = this.x*axis.i.x + this.y*axis.j.x + this.z*axis.k.x;
	    var ym = this.x*axis.i.y + this.y*axis.j.y + this.z*axis.k.y;
	    var zm = this.x*axis.i.z + this.y*axis.j.z + this.z*axis.k.z;
	    this.set(xm,ym,zm);
	}
	return this;
    },
    cartesian2Spherical: function (axis) {
	if (axis !== undefined) {
	    var xm = this.x*axis.i.x + this.y*axis.i.y + this.z*axis.i.z;
	    var ym = this.x*axis.j.x + this.y*axis.j.y + this.z*axis.j.z;
	    var zm = this.x*axis.k.x + this.y*axis.k.y + this.z*axis.k.z;
   	    this.r = Math.max(Math.sqrt(xm*xm+ym*ym+zm*zm),1e-10);
	    this.lat = Math.asin(zm/this.r);
	    if (Math.abs(xm) < 1e-7 && Math.abs(ym) < 1e-7) { 
		this.lon = 0.0; 
	    } else { 
	        this.lon = Math.atan2(ym,xm);
	    }
	} else {
   	    this.r = Math.max(Math.sqrt(this.x*this.x+this.y*this.y+this.z*this.z),1e-10);
	    this.lat = Math.asin(this.z/this.r);
	    if (Math.abs(this.x) < 1e-7 && Math.abs(this.y) < 1e-7) { 
		this.lon = 0.0;
	    } else {
	        this.lon = Math.atan2(this.y,this.x);
	    }
	    return this;
	};
    },

    setComponent: function ( index, value ) {

	switch ( index ) {

	case 0: this.x = value; break;
	case 1: this.y = value; break;
	case 2: this.z = value; break;
	default: throw new Error( 'index is out of range: ' + index );

	}

    },

    getComponent: function ( index ) {

	switch ( index ) {

	case 0: return this.x;
	case 1: return this.y;
	case 2: return this.z;
	default: throw new Error( 'index is out of range: ' + index );

	}

    },

    clone: function () {

	return new this.constructor( this.x, this.y, this.z );

    },

    copy: function ( v ) {

	this.x = v.x;
	this.y = v.y;
	this.z = v.z;

	return this;

    },

    add: function ( v ) {

	this.x += v.x;
	this.y += v.y;
	this.z += v.z;

	return this;

    },

    addScalar: function ( s ) {

	this.x += s;
	this.y += s;
	this.z += s;

	return this;

    },

    addVectors: function ( a, b ) {

	this.x = a.x + b.x;
	this.y = a.y + b.y;
	this.z = a.z + b.z;

	return this;

    },

    addScaledVector: function ( v, s ) {

	this.x += v.x * s;
	this.y += v.y * s;
	this.z += v.z * s;

	return this;

    },

    sub: function ( v ) {

	this.x -= v.x;
	this.y -= v.y;
	this.z -= v.z;

	return this;

    },

    subScalar: function ( s ) {

	this.x -= s;
	this.y -= s;
	this.z -= s;

	return this;

    },

    subVectors: function ( a, b ) {

	this.x = a.x - b.x;
	this.y = a.y - b.y;
	this.z = a.z - b.z;

	//console.log("subVectors Returning vector:",this.x,this.y,this.z);

	return this;

    },

    multiply: function ( v ) {

	this.x *= v.x;
	this.y *= v.y;
	this.z *= v.z;

	return this;

    },

    multiplyScalar: function ( scalar ) {

	if ( isFinite( scalar ) ) {
	    this.x *= scalar;
	    this.y *= scalar;
	    this.z *= scalar;
	} else {
	    this.x = 0;
	    this.y = 0;
	    this.z = 0;
	}

	return this;

    },

    multiplyVectors: function ( a, b ) {

	this.x = a.x * b.x;
	this.y = a.y * b.y;
	this.z = a.z * b.z;

	return this;

    },

    applyMatrix3: function ( m ) {

	var x = this.x;
	var y = this.y;
	var z = this.z;

	var e = m.elements;

	this.x = e[ 0 ] * x + e[ 3 ] * y + e[ 6 ] * z;
	this.y = e[ 1 ] * x + e[ 4 ] * y + e[ 7 ] * z;
	this.z = e[ 2 ] * x + e[ 5 ] * y + e[ 8 ] * z;

	return this;

    },

    transformDirection: function ( m ) {

	// input: Matrix4 affine matrix
	// vector interpreted as a direction

	var x = this.x, y = this.y, z = this.z;

	var e = m.elements;

	this.x = e[ 0 ] * x + e[ 4 ] * y + e[ 8 ]  * z;
	this.y = e[ 1 ] * x + e[ 5 ] * y + e[ 9 ]  * z;
	this.z = e[ 2 ] * x + e[ 6 ] * y + e[ 10 ] * z;

	this.normalize();

	return this;

    },

    divide: function ( v ) {

	this.x /= v.x;
	this.y /= v.y;
	this.z /= v.z;

	return this;

    },

    divideScalar: function ( scalar ) {

	return this.multiplyScalar( 1 / scalar );

    },

    min: function ( v ) {

	this.x = Math.min( this.x, v.x );
	this.y = Math.min( this.y, v.y );
	this.z = Math.min( this.z, v.z );

	return this;

    },

    max: function ( v ) {

	this.x = Math.max( this.x, v.x );
	this.y = Math.max( this.y, v.y );
	this.z = Math.max( this.z, v.z );

	return this;

    },

    clamp: function ( min, max ) {

	// This function assumes min < max, if this assumption isn't true it will not operate correctly

	this.x = Math.max( min.x, Math.min( max.x, this.x ) );
	this.y = Math.max( min.y, Math.min( max.y, this.y ) );
	this.z = Math.max( min.z, Math.min( max.z, this.z ) );

	return this;

    },

    clampScalar: function () {

	var min, max;

	return function clampScalar( minVal, maxVal ) {

	    if ( min === undefined ) {

		min = new Vector3();
		max = new Vector3();

	    }

	    min.set( minVal, minVal, minVal );
	    max.set( maxVal, maxVal, maxVal );

	    return this.clamp( min, max );

	};

    }(),

    clampLength: function ( min, max ) {

	var length = this.length();

	this.multiplyScalar( Math.max( min, Math.min( max, length ) ) / length );

	return this;

    },

    floor: function () {

	this.x = Math.floor( this.x );
	this.y = Math.floor( this.y );
	this.z = Math.floor( this.z );

	return this;

    },

    ceil: function () {

	this.x = Math.ceil( this.x );
	this.y = Math.ceil( this.y );
	this.z = Math.ceil( this.z );

	return this;

    },

    round: function () {

	this.x = Math.round( this.x );
	this.y = Math.round( this.y );
	this.z = Math.round( this.z );

	return this;

    },

    roundToZero: function () {

	this.x = ( this.x < 0 ) ? Math.ceil( this.x ) : Math.floor( this.x );
	this.y = ( this.y < 0 ) ? Math.ceil( this.y ) : Math.floor( this.y );
	this.z = ( this.z < 0 ) ? Math.ceil( this.z ) : Math.floor( this.z );

	return this;

    },

    negate: function () {

	this.x = - this.x;
	this.y = - this.y;
	this.z = - this.z;

	return this;

    },

    dot: function ( v ) {

	return this.x * v.x + this.y * v.y + this.z * v.z;

    },

    lengthSq: function () {

	return this.x * this.x + this.y * this.y + this.z * this.z;

    },

    length: function () {

	return Math.sqrt( this.x * this.x + this.y * this.y + this.z * this.z );

    },

    lengthManhattan: function () {

	return Math.abs( this.x ) + Math.abs( this.y ) + Math.abs( this.z );

    },

    normalize: function () {

	return this.divideScalar( this.length() );

    },

    setLength: function ( length ) {

	return this.multiplyScalar( length / this.length() );

    },

    lerp: function ( v, alpha ) {

	this.x += ( v.x - this.x ) * alpha;
	this.y += ( v.y - this.y ) * alpha;
	this.z += ( v.z - this.z ) * alpha;

	return this;

    },

    lerpVectors: function ( v1, v2, alpha ) {

	this.subVectors( v2, v1 ).multiplyScalar( alpha ).add( v1 );

	return this;

    },

    cross: function ( v ) {

	var x = this.x, y = this.y, z = this.z;

	this.x = y * v.z - z * v.y;
	this.y = z * v.x - x * v.z;
	this.z = x * v.y - y * v.x;

	return this;

    },
    isZero : function () {
	return (Math.abs(this.x) < 1e-10 && Math.abs(this.y) < 1e-10 && Math.abs(this.z) < 1e-10 );
    },
    makeNormal : function (pos) {
	if (Math.abs(pos.x) > 0.0) {
	    this.x=pos.y;
	    this.y=-pos.x;
	    this.z=0.0;
	} else if (Math.abs(pos.y) > 0.0) {
	    this.x=0.0;
	    this.y=pos.z;
	    this.z=-pos.y;
	} else if (Math.abs(pos.z) > 0.0) {
	    this.x=-pos.z;
	    this.y=0.0;
	    this.z=pos.x;
	};
	this.normalize();
    },
    crossVectors: function ( a, b ) {

	var ax = a.x, ay = a.y, az = a.z;
	var bx = b.x, by = b.y, bz = b.z;

	this.x = ay * bz - az * by;
	this.y = az * bx - ax * bz;
	this.z = ax * by - ay * bx;

	return this;

    },

    projectOnVector: function () {

	var v1, dot;

	return function projectOnVector( vector ) {

	    if ( v1 === undefined ) v1 = new Vector3();

	    v1.copy( vector ).normalize();

	    dot = this.dot( v1 );

	    return this.copy( v1 ).multiplyScalar( dot );

	};

    }(),

    projectOnPlane: function () {

	var v1;

	return function projectOnPlane( planeNormal ) {

	    if ( v1 === undefined ) v1 = new Vector3();

	    v1.copy( this ).projectOnVector( planeNormal );

	    return this.sub( v1 );

	}

    }(),

    reflect: function () {

	// reflect incident vector off plane orthogonal to normal
	// normal is assumed to have unit length

	var v1;

	return function reflect( normal ) {

	    if ( v1 === undefined ) v1 = new Vector3();

	    return this.sub( v1.copy( normal ).multiplyScalar( 2 * this.dot( normal ) ) );

	}

    }(),

    angleTo: function ( v ) {

	var theta = this.dot( v ) / ( this.length() * v.length() );

	// clamp, to handle numerical problems

	return Math.acos( Math.clamp( theta, - 1, 1 ) );

    },

    distanceTo: function ( v ) {

	return Math.sqrt( this.distanceToSquared( v ) );

    },

    distanceToSquared: function ( v ) {

	var dx = this.x - v.x;
	var dy = this.y - v.y;
	var dz = this.z - v.z;

	return dx * dx + dy * dy + dz * dz;

    },

    setFromMatrixPosition: function ( m ) {

	this.x = m.elements[ 12 ];
	this.y = m.elements[ 13 ];
	this.z = m.elements[ 14 ];

	return this;

    },

    setFromMatrixScale: function ( m ) {

	var sx = this.set( m.elements[ 0 ], m.elements[ 1 ], m.elements[ 2 ] ).length();
	var sy = this.set( m.elements[ 4 ], m.elements[ 5 ], m.elements[ 6 ] ).length();
	var sz = this.set( m.elements[ 8 ], m.elements[ 9 ], m.elements[ 10 ] ).length();

	this.x = sx;
	this.y = sy;
	this.z = sz;

	return this;

    },

    setFromMatrixColumn: function ( index, matrix ) {

	var offset = index * 4;

	var me = matrix.elements;

	this.x = me[ offset ];
	this.y = me[ offset + 1 ];
	this.z = me[ offset + 2 ];

	return this;

    },

    equals: function ( v ) {

	return ( ( v.x === this.x ) && ( v.y === this.y ) && ( v.z === this.z ) );

    },

    fromArray: function ( array, offset ) {

	if ( offset === undefined ) offset = 0;

	this.x = array[ offset ];
	this.y = array[ offset + 1 ];
	this.z = array[ offset + 2 ];

	return this;

    },

    toArray: function ( array, offset ) {

	if ( array === undefined ) array = [];
	if ( offset === undefined ) offset = 0;

	array[ offset ] = this.x;
	array[ offset + 1 ] = this.y;
	array[ offset + 2 ] = this.z;

	return array;

    },

    fromAttribute: function ( attribute, index, offset ) {

	if ( offset === undefined ) offset = 0;

	index = index * attribute.itemSize + offset;

	this.x = attribute.array[ index ];
	this.y = attribute.array[ index + 1 ];
	this.z = attribute.array[ index + 2 ];

	return this;

    }

};
