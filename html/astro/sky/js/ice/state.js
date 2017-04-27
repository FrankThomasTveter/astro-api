// calculates to and from osculating elements
State ={};

// check if we have a new request to process.
State.update = function () {
    var reqId=requests.last.request;
    var req = requests[reqId]
    if (state.reqId != reqid & req.orbit) { //
	
	
	
	
    }
    if ( req.processed != undefined) { // we have a new processed request
	// check if state needs update
	if (state.update == undefined || state.update || state.reqId != reqId) {
	    
	    
	    
	};    
	    // calculate osculating orbital elements
	    for ( var tt = 0; tt < req["events"].length; tt++) {
		if (req["events"][tt]["state"] != undefined) {
		    var state=req["events"][tt]["state"];
		    req["events"][tt]["elements"]=[];
		    var elements=req["events"][tt]["elements"];
		    for (var id =0; id < state.length; id++) {
			var s = state[id];
			elements[id]={};
			var e=elements[id];
			var main =  req["initial"][id]["main"];
			var r, xmu;
			if (main > 0) { // body orbits another body...
			    r=state[main-1];
			    xmu=req["initial"][main-1]["xmu"]
			    State.state2elements(s,e,r,xmu);
			    State.elements2anomalies(e);
			}
		    };
		};
	    };
	};
	req.state = true;
	    
    }
}

// xmu = G * Mass
State.xmu = { sun     : 132712440018.0,
	      Mercury :        22032.0,
	      Venus   :       324859.0,
	      Earth   :       398601.3,
	      Mars    :        42828.0,
	      Ceres   :           63.0,
	      Jupiter :    126686534.0,
	      Saturn  :     37931187.0,
	      Uranus  :      5793947.0,
	      Neptune :      6836529.0,
	      Pluto   :         1001.0 };


State.state2elements = function( s, e, r, xmu) {
    //
    //   Calculates the classical orbital elements from a state.
    //
    // i  s  = state wrt central body, position & velocity of s/c
    // o  e.a = semi-major axis
    // o  e.e = eccentricity
    // o  e.i = inclination
    // o  e.o = ascending node  ( =0 if i=0)   between 0 and twopi.
    // o  e.w = argument of perigee ( =0 if e=0) between 0 and twopi.
    // o  e.v = true anomaly,     between 0 and twopi.
    // i  r = main body
    // i  xmu =  gravity potential of main body in km**3/sec**2
    //
    if (xmu == undefined) {
	console.log("Not calculating elements.");
    } else {
	var sx,sy,sz,svx,svy,svz;
	if (r == undefined) {
	    sx= s.x;
	    sy = s.y;
	    sz = s.z;
	    svx = s.vx;
	    svy = s.vy;
	    svz = s.vz;
	} else {
	    sx = s.x-r.x;
	    sy = s.y-r.y;
	    sz = s.z-r.z;
	    svx = s.vx-r.vx;
	    svy = s.vy-r.vy;
	    svz = s.vz-r.vz;
	}
	var c1 = sy * svz - sz * svy;
	var c2 = sz * svx - sx * svz;
	var c3 = sx * svy - sy * svx;
	var cc12  =  c1 * c1 + c2 * c2;
	var cc  =  cc12  +  c3 * c3;
	var c = Math.sqrt(cc);
	var v02 = svx * svx + svy * svy + svz * svz;
	var r0v0 = sx * svx + sy * svy + sz * svz;
	var r02 = sx * sx + sy * sy + sz * sz;
	var r0 = Math.sqrt(r02);
	var x = r0*v02/xmu;
	var cx = cc/xmu;
	var ste = r0v0 * c/(r0 * xmu);
	var cte = cx/r0 - 1.0;
	e.a = Math.abs(r0/(2.0 - x));
	e.e = Math.sqrt(ste * ste + cte * cte);
	e.i = Math.atan2(Math.sqrt(cc12),c3);
	if(cc12 > cc * 1.0e-20) {
	    u  =  Math.atan2(c * sz,sy * c1 - sx * c2);
	    e.o  =  Math.atan2(c1, - c2);
	} else {
	    u  =  Math.atan2(sy, sx)*Math.sign(c3);
	    e.o = 0.0;
	}
	if( e.e  > 1.0e-20) {
	    e.v  =  Math.atan2(ste,cte);
	    e.w = u - e.v;
	} else {
	    e.v  =  u;
	    e.w = 0.0;
	}
	if(e.o < 0.0) e.o  =  e.o + (Math.PI * 2.0);
	if(e.w < 0.0) e.w  =  e.w + (Math.PI * 2.0);
	if(e.v < 0.0) e.v  =  e.v + (Math.PI * 2.0);
	e.n=(Math.sqrt(xmu/e.a))/e.a; // mean angular motion
	console.log("Elements a=",e.a," e=",e.e," i=",e.i," ascn=",e.o," argp=",e.w," v=",e.v," N=",e.n," mu=",xmu);
    }
}

State.elements2anomalies = function ( e ) {
    //
    // get mean anomaly from true anomaly
    //
    // i  e.a = semi-major axis
    // i  e.e = eccentricity
    // i  e.i = inclination
    // i  e.o = ascending node  ( =0 if i=0)   between 0 and twopi.
    // i  e.w = argument of perigee ( =0 if e=0) between 0 and twopi.
    // i  e.v = true anomaly,     between 0 and twopi.
    // o  e.c = eccentric anomaly
    // o  e.m = mean anomaly
    if (((1.0-e.e)  < 1.0E-10) || (e.e < 0.0)) {
	e.c=e.v;
	e.m=e.v;
    } else if ((Math.abs((e.v/2.0)-(Math.PI/2.0)) < 1.0E-10) || 
	       (Math.abs((e.v/2.0)-(3.0*Math.PI/2.0)) < 1.0E-10)) {
	e.c=e.v;
	e.m=e.v;
        console.log('Tangens isn`t defined for the input ',e.v);
    } else {
        e.c = 2.0 * Math.atan(Math.sqrt((1.0 - e.e)/(1.0 + e.e)) * Math.tan(e.v/2.0));
        if (e.c < 0.0) {e.c = e.c + 2.0*Math.PI;}
    }
    // get mean anomaly from eccentric anomaly
    e.m = e.c - e.e*Math.sin(e.c);
    console.log("Anomalies ecc=",e.c," mean=",e.m);
}

State.anomalies2elements = function ( e ) {
    //
    // get true anomaly from mean anomaly
    //
    // i  e.a = semi-major axis
    // i  e.e = eccentricity
    // i  e.i = inclination
    // i  e.o = ascending node  ( =0 if i=0)   between 0 and twopi.
    // i  e.w = argument of perigee ( =0 if e=0) between 0 and twopi.
    // o  e.v = true anomaly,     between 0 and twopi.
    // i  e.c = eccentric anomaly
    // i  e.m = mean anomaly
    if (((1.0-e.e)  < 1.0E-10) || (e.e < 0.0)) {
	e.v=e.m;
	e.c=e.m;
    } else if ((Math.abs((e.m/2.0)-(Math.PI/2.0)) < 1.0E-10) || 
	       (Math.abs((e.m/2.0)-(3.0*Math.PI/2.0)) < 1.0E-10)) {
	e.v=e.m;
	e.c=e.m;
    } else {
	// get eccentric anomaly from mean anomaly
	e.c = State.solveEccentricAnomaly(e.e, e.m);
	// get true anomaly from eccentric anomaly
        e.v = 2.0 * Math.atan(Math.sqrt((1.0 + e.e)/(1.0 - e.e)) * Math.tan(e.c/2.0));
        if (e.v < 0.0) {e.v = e.v + 2.0*Math.PI;}
    }
}

State.elements2state = function ( s, e, r, xmu ) {
    //
    //  Calculates state from classical orbital elements.
    //
    //i  e.a =  semi-major axis
    //i  e.e =  eccentricity
    //i  e.i =  inclinationy, in interval 0 to pi
    //i  e.o =  ascending node, in interval 0 to twopi
    //i  e.w =  arg. of perigee, in interval 0 to twopi
    //i  e.v =  true anomaly, in interval 0 to twopi
    //o  s  = state, (position and velocity)
    //
    var p = e.a*(1.0 - e.e*e.e);
    //   safety measure for the square root
    p = Math.max(p,1.0e-30);
    var f = wmu/Math.sqrt(p);
    var cv = Math.cos(e.v);
    var ecv = 1.0 + e.e*cv;
    var r = p/ecv;
    var u = e.w + e.v;
    var cu = Math.cos(u);
    var su = Math.sin(u);
    var co = Math.cos(e.o);
    var so = Math.sin(e.o);
    var ci = Math.cos(e.i);
    var si = Math.sin(e.i);
    var cocu = co*cu;
    var sosu = so*su;
    var socu = so*cu;
    var cosu = co*su;
    var fx = cocu - sosu*ci;
    var fy = socu + cosu*ci;
    var fz = su*si;
    var vr = f*e.e*Math.sin(e.v);
    var vu = f*ecv;
    s.x = r*fx + r.x;
    s.y = r*fy + r.y;
    s.z = r*fz + r.z;
    s.vx = vr*fx - vu*(cosu + socu*ci) + r.vx;
    s.vy = vr*fy - vu*(sosu - cocu*ci) + r.vy;
    s.vz = vr*fz + vu*cu*si + r.vz;
    if (r == undefined) {
	s.x = s.x;
	s.y = r*fy;
	s.z = r*fz;
	s.vx = vr*fx - vu*(cosu + socu*ci);
	s.vy = vr*fy - vu*(sosu - cocu*ci);
	s.vz = vr*fz + vu*cu*si;
    } else {
	s.x = s.x + r.x;
	s.y = r*fy + r.y;
	s.z = r*fz + r.z;
	s.vx = vr*fx - vu*(cosu + socu*ci) + r.vx;
	s.vy = vr*fy - vu*(sosu - cocu*ci) + r.vy;
	s.vz = vr*fz + vu*cu*si + r.vz;
    }
}

var maxIterationsForEccentricAnomaly = 10;
var maxDE = 1e-15;

State.solveEccentricAnomaly = function(e, M){
    // calculate mean anomaly from eccentric anomaly
    if (e == 0.0) {
	return M;
    }  else if (e < 0.9) {
	var sol = solveEccentricAnomaly_(State.solveKepler(e, M), M, 6);
	return sol;
    } else if (e < 1.0) {
	var E = M + 0.85 * e * ((Math.sin(M) >= 0.0) ? 1 : -1);
	var sol = solveEccentricAnomaly_(State.solveKeplerLaguerreConway(e, M), E, 8);
	return sol;
    } else if (e == 1.0) {
	return M;
    } else {
	var E = Math.log(2 * M / e + 1.85);
	var sol = solveEccentricAnomaly_(State.solveKeplerLaguerreConwayHyp(e, M), E, 30);
	return sol;
    }
};

State.solveEccentricAnomaly_ = function (f, x0, maxIter) {
    var x = 0;
    var x2 = x0;
    for (var i = 0; i < maxIter; i++) {
	x = x2;
	x2 = f(x);
    }
    
    return x2;
}

State.solveKepler = function(e, M) {
    return function(x) {
	return x + (M + e * Math.sin(x) - x) / (1 - e * Math.cos(x));
    };
};

State.solveKeplerLaguerreConway = function(e, M) {
    return function(x) {
	var s = e * Math.sin(x);
	var c = e * Math.cos(x);
	var f = x - s - M;
	var f1 = 1 - c;
	var f2 = s;
	x += -5 * f / (f1 + CMath.sign(f1) * Math.sqrt(Math.abs(16 * f1 * f1 - 20 * f * f2)));
	return x;
    };
};

State.solveKeplerLaguerreConwayHyp = function(e, M) {
    return function(x) {
	var s = e * CMath.sinh(x);
	var c = e * CMath.cosh(x);
	var f = x - s - M;
	var f1 = c - 1;
	var f2 = s;

	x += -5 * f / (f1 + CMath.sign(f1) * Math.sqrt(Math.abs(16 * f1 * f1 - 20 * f * f2)));
	return x;
    };
};
