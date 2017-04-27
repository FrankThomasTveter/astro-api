console.log("Loading stack.js");


// maintains the stack state
Stack ={redraw:false};
// switch between two states...

// useful constants...
Stack.DEG_TO_RAD = Math.PI/180;
Stack.RAD_TO_DEG = 180/Math.PI;
Stack.AU = 149597870;
Stack.CIRCLE = 2 * Math.PI;
Stack.KM = 1000;
Stack.DEG_TO_RAD = Math.PI/180;
Stack.RAD_TO_DEG = 180/Math.PI;
Stack.NM_TO_KM = 1.852;
Stack.LB_TO_KG = 0.453592;
Stack.LBF_TO_NEWTON = 4.44822162;
Stack.FT_TO_M = 0.3048;
Stack.DAY = 60 * 60 * 24;
Stack.YEAR = 365.2525;
Stack.CENTURY = 100 * Stack.YEAR;
Stack.SIDERAL_DAY = 3600 * 23.9344696;

Stack.pushUrl = function () {
    var reqId=requests.current;
    var req = requests[reqId]
    if ( req.elements) { // we have a target
	var reqLocation=requests[reqId]["location"];
	if (reqLocation !== undefined && stack[stack.current] !== undefined) {
	    var lat=reqLocation.latitude;
	    var lon=reqLocation.longitude;
	    var dtg=stack[stack.current].dtg;
	    var dir=camera.getDir();
	    var fov=camera.getFovX();
	    var con=Chain.consReq;
	    var play=(stack.play.speed||0);
	    var hrs=(stack.play.hrs||1);
	    var lab="";
	    var url="sky.html";
	    var first=true;
	    if (dir !== undefined) {
		if (first) {url=url+"?";first=false;} else {url=url+"&";};
		url=url + "dir=" + parseFloat(dir.x).toFixed(5)
		    +","+parseFloat(dir.y).toFixed(5)
		    +","+parseFloat(dir.z).toFixed(5);
	    }
	    if (fov !== undefined) {
		if (first) {url=url+"?";first=false;} else {url=url+"&";};
		url=url + "fov=" + parseFloat(fov).toFixed(4);
	    }
	    if (lat !== undefined) {
		if (first) {url=url+"?";first=false;} else {url=url+"&";};
		url=url + "lat=" + parseFloat(+lat).toFixed(3);
	    }
	    if (lon !== undefined) {
		if (first) {url=url+"?";first=false;} else {url=url+"&";};
		url=url + "lon=" + parseFloat(+lon).toFixed(3);
	    }
	    if (dtg !== undefined) {
		if (first) {url=url+"?";first=false;} else {url=url+"&";};
		url=url + "dtg=" + dtg;
	    }
	    if (hrs !== undefined) {
		if (first) {url=url+"?";first=false;} else {url=url+"&";};
		url=url + "hrs=" + hrs;
	    }
	    if (play !== undefined) {
		if (first) {url=url+"?";first=false;} else {url=url+"&";};
		url=url + "play=" + play;
	    }
	    if (con !== undefined) {
		if (first) {url=url+"?";first=false;} else {url=url+"&";};
		url=url + "con=" + con;
	    }
	    console.log("Setting URL to:",url);
	    window.history.replaceState("", "js", url);
	}
    }
}
Stack.play = function () {
    if (stack.play !== undefined) {
	if (stack.play.speed === undefined) { // start simulation first time
	    var tnow=(new Date()).getTime();
	    stack.play.speed = 1.0;
	    stack.play.e0 = tnow;
	    stack.play.m0 = (new Date(stack[stack.current].epoch)).getTime();
	} else if (stack.play.halt === undefined){ // stop running simulation, store time
	    var tnow=(new Date()).getTime();
	    stack.play.m0 = stack.play.m0 + (tnow-stack.play.e0)*stack.play.speed;
	    stack.play.e0 = tnow;
	    stack.play.halt = stack.play.speed;
	    stack.play.speed = 0.0;
	} else  { // start stopped simulation
	    var tnow=(new Date()).getTime();
	    stack.play.e0 = tnow;
	    stack.play.speed = stack.play.halt;
	    stack.play.halt=undefined;
	}
    }
}

// check if we have a new request to process.
Stack.processNewRequests = function () {
    var ret=false;
    var reqId=requests.current;
    var req = requests[reqId]
    if ( req.received & ! req.elements) { // we have a new target
	//console.log("Calculating orbital elements:",req);
	// calculate osculating orbital elements
	for ( var tt = 0; tt < req["events"].length; tt++) {
	    req["events"][tt]["epoch"]=(new Date(req["events"][tt]["dtg"])).getTime();// get epoch
	    if (req["events"][tt]["state"] !== undefined) {
		var reqState=req["events"][tt]["state"];
		req["events"][tt]["elements"]={};
		var elements=req["events"][tt]["elements"];
		for (var name in reqState) {
		    var s = reqState[name].position;
		    elements[name]={};
		    var e=elements[name];
		    var r, xmu;
		    var main =  req["initial"][name]["main"];
		    if (main !== "") { // body orbits another body...
			r=reqState[main].position;
			xmu=req["initial"][main]["xmu"];
			//Stack.state2elements(s,e,r,xmu);
			//Stack.elements2anomalies(e);
		    }
		};
	    };
	    //document.getElementById("log").innerHTML="orbit loop end";
	};
	req.elements = true;
	//console.log("Calculated orbital elements:",req);
	ret=true;
    }
    return ret;
};

// update 3D stack of the solar system...
Stack.update = function () {
    var ret = false;
    var reqId=requests.current;
    var req = requests[reqId]
    if (req.elements & stack.reqId != reqId ) { // process new request
	//console.log("Constructing stack from orbital elements.");
	// get times
	stack.epochs = [];
	for ( var tt = 0; tt < req["events"].length; tt++) {
	    stack.epochs[tt]=req["events"][tt]["epoch"];
	};
	stack.play={};
	if (req.play.event !== undefined) {
	    stack.play.event=req.play.event;
	};
	if (req.play.speed !== undefined) {
	    var tnow=(new Date()).getTime();
	    stack.play.speed=req.play.speed;
	    if (req.play.m0 !== undefined) {
		stack.play.m0=req.play.m0;
	    } else {
		stack.play.m0=tnow;
	    };
	    stack.play.e0=tnow;
	    if (stack.play.speed > 0 ) {
		stack.play.halt=undefined;
	    } else {
		stack.play.halt=1.0;
	    };
	}
	stack.young=(stack.current+1)%2;
	if (Stack.getState(req,stack[stack.young])) {
	    stack.old=stack.current;
	    stack.current=stack.young;
	    if (stack[stack.old]["bodies"] != undefined) {
		// "tween" stack from oldState to newState
	    };
	    //  point camera
	    if (req.play.event !== undefined) {
		stack[stack.current].dtg = new Date(requests[reqId]["events"][req.play.event]["dtg"]).toISOString();
		camera.position.copy(stack[stack.current].observer.position);
		camera.setUp(stack[stack.current].observer.zenith); // up is always towards observer zenith...
		var target = requests[reqId]["events"][req.play.event]["target"]; // target could be mis-spelled...
		var dir = requests[reqId]["events"][req.play.event]["dir"];
		var fov = requests[reqId]["events"][req.play.event]["fov"];
		var con = requests[reqId]["events"][req.play.event]["con"];
		var stackBodies=stack[stack.current].bodies;
		if (target !== undefined && stackBodies[target] !== undefined) {
		    camera.pointAt(stackBodies[target].position);
		} else if (dir !== undefined) {
		    camera.pointDir(dir);
		};
		if (fov !== undefined) {
		    camera.setFovX(fov);
		};
		if (con !== undefined) {
		    if (con == 0) { // no constellations
			Chain.consTime=new Date().getTime()-1000.0; // no fade
		    } else {
			Chain.consTime=new Date().getTime()+2000.0; // fade inn
		    };
		    Chain.consReq = con;
		};
	    }
	    // delete old requests
	    for (var key in requests) {
		if (requests.hasOwnProperty(key)) {
		    if (key != stack.old & requests[key]["events"] != undefined) {
		    }
		}
	    }
	    stack.reqId=reqId;
	};
	req.orbit = false;
        Request.setInfo(stack[stack.current].dtg,stack[stack.current].lat,stack[stack.current].lon);
	Stack.redraw=true;
    } else if (req.elements & stack.play.speed!==undefined) { // time lapse...
	//console.log("Time lapse.",stack.play.speed);
	stack.young=(stack.current+1)%2;
	if (Stack.getState(req, stack[stack.young])) {
            Request.setInfo(stack[stack.current].dtg,stack[stack.current].lat,stack[stack.current].lon);
	    stack.old=stack.current;
	    stack.current=stack.young;
	    Stack.redraw=true;
	}
//    } else {
//	console.log("Nothing to do:",req.elements,stack.play.speed);
    }
    return ret;
};

Stack.getState = function (req, newStack){
    var ret=true;
    var tnow=(new Date()).getTime();
    if (stack.play.speed !== undefined) { // propagate state to right epoch using elliptical orbits
	var ttrg = (tnow-stack.play.e0) * stack.play.speed + stack.play.m0;
	newStack.lat=req.location.latitude;
	newStack.lon=req.location.longitude;
	stack.play.prev=-1;
	stack.play.next=-1;
	stack.play.prevEpoch=0;
	stack.play.nextEpoch=0;
	// find correct epoch
	for ( var tt = 0; tt < req["events"].length; tt++) {
	    if (req["events"][tt]["epoch"] <= ttrg &  
		(stack.play.prev < 0 || req["events"][tt]["epoch"] >=  stack.play.prevEpoch)) {
		stack.play.prev=tt;
		stack.play.prevEpoch=req["events"][stack.play.prev]["epoch"];
	    };
	    if (req["events"][tt]["epoch"] >= ttrg &  
		(stack.play.next < 0 || req["events"][tt]["epoch"] <=  stack.play.nextEpoch)) {
		stack.play.next=tt;
		stack.play.nextEpoch=req["events"][stack.play.next]["epoch"];
	    };
	};
	if (stack.play.prev != -1 & stack.play.next != -1) { // success!
	    var dt = ttrg-stack.play.prevEpoch;
	    var dt0 = stack.play.nextEpoch - stack.play.prevEpoch;
	    var f = dt/Math.max(1e-10,dt0);
	    Stack.interpolateBodies(req["events"][stack.play.prev],req["events"][stack.play.next],dt,f,newStack.bodies);
	    Stack.interpolateObserver(req["events"][stack.play.prev],req["events"][stack.play.next],dt,f,newStack.observer,newStack.bodies);
	    newStack.epoch = ttrg;
	    newStack.dtg=new Date(newStack.epoch).toISOString();
	} else if (stack.play.prev != -1 ) {  // found epoch before, but not after
	    Stack.getBodies(req["events"][stack.play.prev],newStack.bodies);
	    Stack.getObserver(req["events"][stack.play.prev],newStack.observer,newStack.bodies);
	    newStack.epoch = req["events"][stack.play.prev].epoch;
	    newStack.dtg=new Date(newStack.epoch).toISOString();
	} else if (stack.play.next != -1) {  // found epoch before, but not after
	    Stack.getBodies(req["events"][stack.play.next],newStack.bodies);
	    Stack.getObserver(req["events"][stack.play.next],newStack.observer,newStack.bodies);
	    newStack.epoch = req["events"][stack.play.next].epoch;
	    newStack.dtg=new Date(newStack.epoch).toISOString();
	} else {
	    //console.log("Times:",+ttrg,"|",+stack.play.e0 ,"|",+stack.play.speed, "|",+stack.play.m0);
	    console.log("Unable to find interval:",+ttrg,req["events"]);
	    ret=false;
	}
	//console.log("Pos:",+ttrg,req["events"][0]," REQ:",stack.play.prev,stack.play.next," DTG:",newStack.dtg,stack.play.prev,stack.play.next);
    } else {
	//console.log("******************getState fixed: ",newStack);
	Stack.getBodies(req["events"][stack.play.event],newStack.bodies);
	Stack.getObserver(req["events"][stack.play.event],newStack.observer,newStack.bodies);
	newStack.epoch = req["events"][stack.play.event].epoch;
	newStack.dtg=new Date(newStack.epoch).toISOString();
	newStack.lat=req.location.latitude;
	newStack.lon=req.location.longitude;
    }
    //console.log("******************getState: ",newStack);
    return ret;
};

Stack.getBodies = function (reqState,stackBodies) {
    for (var name in reqState["state"]) {
	var reqBody = reqState["state"][name];
	if (stackBodies[name] === undefined) {
	    stackBodies[name]={position:new Vector3(),rotation:new Vector3()};
	};
	//console.log("getStackBodies State:",state,s,name);
	stackBodies[name].position.copy(reqBody.position);
	stackBodies[name].position.vx = +reqBody.position.vx;
	stackBodies[name].position.vy = +reqBody.position.vy;
	stackBodies[name].position.vz = +reqBody.position.vz;
	stackBodies[name].rotation.ra = +reqBody.rotation.ra;
	stackBodies[name].rotation.dec = +reqBody.rotation.dec;
	stackBodies[name].rotation.w = +reqBody.rotation.w;
	stackBodies[name].rotation.lon = stackBodies[name].rotation.ra;
	stackBodies[name].rotation.lat = stackBodies[name].rotation.dec;
	stackBodies[name].rotation.r = 1.0;
	stackBodies[name].rotation.spherical2cartesian();
	stackBodies[name].name=name;
    } 
}

Stack.interpolateBodies = function (reqStatePrev,reqStateNext,dt,f,stackBodies) {
    for (var name in reqStatePrev["state"]) {
	var reqPrev = reqStatePrev["state"][name];
	var reqNext = reqStateNext["state"][name];
	if (stackBodies[name] === undefined) {
	    stackBodies[name]={position:new Vector3(),rotation:new Vector3()};
	};
	stackBodies[name].position.interpolate(reqPrev.position,reqNext.position,f);
	stackBodies[name].position.vx =  Stack.intlin(reqPrev.position.vx,reqNext.position.vx,f);
	stackBodies[name].position.vy =  Stack.intlin(reqPrev.position.vy,reqNext.position.vy,f);
	stackBodies[name].position.vz =  Stack.intlin(reqPrev.position.vz,reqNext.position.vz,f);
	stackBodies[name].rotation.ra =  Stack.intlin(reqPrev.rotation.ra,reqNext.rotation.ra,f);
	stackBodies[name].rotation.dec = Stack.intlin(reqPrev.rotation.dec,reqNext.rotation.dec,f);
	stackBodies[name].rotation.w =   Stack.intlin(reqPrev.rotation.w,reqNext.rotation.w,f);
	stackBodies[name].rotation.lon = stackBodies[name].rotation.ra;
	stackBodies[name].rotation.lat = stackBodies[name].rotation.dec;
	stackBodies[name].rotation.r = 1.0;
	stackBodies[name].rotation.spherical2cartesian();
	stackBodies[name].name=name;
    } 
}

// get observer position and EF-coordinate system...
Stack.getObserver = function (reqState,stackObserver,stackBodies) { 
    if (stackObserver.position === undefined) stackObserver.position=new Vector3(); 
    if (stackObserver.i === undefined) stackObserver.i=new Vector3(); 
    if (stackObserver.j === undefined) stackObserver.j=new Vector3(); 
    if (stackObserver.k === undefined) stackObserver.k=new Vector3(); 
    if (stackObserver.zenith === undefined) stackObserver.zenith=new Vector3(); 
    var reqObserver=reqState["observer"];
    var reqOrigo=reqObserver.position.origo;
    if (stackBodies[reqOrigo] === undefined ) {
	stackObserver.position.copy(reqObserver.position);
    } else {
	stackObserver.position.origo=reqOrigo;
	stackObserver.position.copy(reqObserver.position).add(stackBodies[reqOrigo].position);
    }
    stackObserver.zenith.copy(reqObserver.zenith);
    stackObserver.zenith.normalize();
    stackObserver.i.copy(reqObserver.i);
    stackObserver.j.copy(reqObserver.j);
    stackObserver.k.copy(reqObserver.k);

    //console.log("setting axis:", stackObserver.i.x,stackObserver.i.y,stackObserver.i.z);
}

// get observer position and EF-coordinate system...
Stack.interpolateObserver = function (reqStatePrev,reqStateNext,dt,f,stackObserver,stackBodies) { 
    if (stackObserver.position === undefined) stackObserver.position=new Vector3(); 
    if (stackObserver.i === undefined) stackObserver.i=new Vector3(); 
    if (stackObserver.j === undefined) stackObserver.j=new Vector3(); 
    if (stackObserver.k === undefined) stackObserver.k=new Vector3(); 
    if (stackObserver.zenith === undefined) stackObserver.zenith=new Vector3();
    var reqPrev = reqStatePrev["observer"];
    var reqNext = reqStateNext["observer"];
    var reqOrigo=reqPrev.position.origo;
    if (stackBodies[reqOrigo] === undefined ) {
	stackObserver.position.interpolate(reqPrev.position, reqNext.position, f);
    } else {
	stackObserver.position.origo=reqOrigo;
	stackObserver.position.interpolate(reqPrev.position, reqNext.position, f);
	stackObserver.position.add(stackBodies[reqOrigo].position);
    }
    stackObserver.zenith.interpolate(reqPrev.zenith,reqNext.zenith,f);
    stackObserver.zenith.normalize();
    stackObserver.i.interpolate(reqPrev.i,reqNext.i,f);
    stackObserver.j.interpolate(reqPrev.j,reqNext.j,f);
    stackObserver.k.interpolate(reqPrev.k,reqNext.k,f);

    //console.log("setting axis:", stackObserver.i.x,stackObserver.i.y,stackObserver.i.z);
}

Stack.interpolateBodies2 = function(req,dt,f,stackBodies) {
    // interpolate body positions between times
    //console.log(">>> interpolating body positions.")
    var reqStatePrev = req["events"][req.play.prev]["state"];
    var reqStateNext = req["events"][req.play.next]["state"];
    if (reqStatePrev === undefined || reqStateNext === undefined) {
	console.log("THIS SHOULD NEVER HAPPEN!.");
	return;
    };
    for (var name in req["events"][req.play.next]["state"]) {
	var name_ = reqStatePrev[name].name;
	stackBodies[name]={position:new Vector3(),
			   rotation:new Vector3()};
	var main =  req["initial"][name]["main"];
	if (main !== "") { // use orbit to interpolate position
	    var ep=req["events"][req.play.prev]["elements"][name];
	    var en=req["events"][req.play.next]["elements"][name];
	    // interpolate elements
	    var el = {};
	    el.a = Stack.intlin(ep.a, en.a, f);
	    el.e = Stack.intlin(ep.e, en.e, f);
	    el.i = Stack.int2pi(ep.i, en.i, f);
	    el.w = Stack.int2pi(ep.w, en.w, f);
	    el.n = Stack.intlin(ep.n, en.n, f);
	    var trg = dt * el.n;
 	    el.m = Stack.int2pi(ep.m, en.m, f, trg);
	    // get state
	    Stack.anomalies2elements(el);
	    
	    r=stackBodies[main];
	    var xmu=req["initial"][main]["xmu"];
	    Stack.elements2state(stackBodies[name], el, r, xmu);
	    //console.log("Prev elements:",ep);
	    //console.log("Next elements:",en);
	    //console.log("Intp elements:",el);
	    //console.log("State main:",r,xmu);
	    //console.log("State elements:",stackBodies[name]);
	} else { // no orbit available, interpolate state directly
	    stackBodies[name].position.set( Stack.intlin(reqStatePrev[name].position.x,reqStateNext[name].position.x,f),
					    Stack.intlin(reqStatePrev[name].position.y,reqStateNext[name].position.y,f),
					    Stack.intlin(reqStatePrev[name].position.z,reqStateNext[name].position.z,f));
	    stackBodies[name].position.vx = Stack.intlin(reqStatePrev[name].position.vx,reqStateNext[name].position.vx,f);
	    stackBodies[name].position.vy = Stack.intlin(reqStatePrev[name].position.vy,reqStateNext[name].position.vy,f);
	    stackBodies[name].position.vz = Stack.intlin(reqStatePrev[name].position.vz,reqStateNext[name].position.vz,f);
	    //console.log("StackBodies fixed:",stackBodies[name].position);
	};
	stackBodies[name].rotation.ra =    Stack.intlin(reqStatePrev[name].ra,reqStateNext[name].ra,f);
	stackBodies[name].rotation.dec =   Stack.intlin(reqStatePrev[name].dec,reqStateNext[name].dec,f);
	stackBodies[name].rotation.w =     Stack.intlin(reqStatePrev[name].w,reqStateNext[name].w,f);
	stackBodies[name].rotation.lon = stackBodies[name].rotation.ra;
	stackBodies[name].rotation.lat = stackBodies[name].rotation.dec;
	stackBodies[name].rotation.r = 1.0;
	stackBodies[name].rotation.spherical2cartesian();
	stackBodies[name].name=name;
    }
}

// interpolate observer position and EF-coordinate system...
Stack.interpolateObserver2 = function(req,dt,f,stackObserver) {
    if (stackObserver.position === undefined) stackObserver.position=new Vector3(); 
    if (stackObserver.i === undefined) stackObserver.i=new Vector3(); 
    if (stackObserver.j === undefined) stackObserver.j=new Vector3(); 
    if (stackObserver.k === undefined) stackObserver.k=new Vector3(); 
    if (stackObserver.zenith === undefined) stackObserver.zenith=new Vector3(); 
    var reqObsPrev=req["events"][req.play.prev]["observer"];
    var reqObsNext=req["events"][req.play.next]["observer"];
    var origo=reqObsPrev.position.origo;
    if (origo == undefined) {origo="earth";};
    //console.log("Origo is now: ",origo);
    var rot = req["initial"][origo]["rotation"];
    //
    // make rotation from start to the final coordinate system
    var m=new THREE.Matrix4();
    if (m.elements.te === undefined) {m.elements.te=[];};
    m.elements.te[ 0]= reqObsPrev.i.x*reqObsNext.i.x + reqObsPrev.j.x*reqObsNext.j.x + reqObsPrev.k.x*reqObsNext.k.x;
    m.elements.te[ 4]= reqObsPrev.i.y*reqObsNext.i.x + reqObsPrev.j.y*reqObsNext.j.x + reqObsPrev.k.y*reqObsNext.k.x;
    m.elements.te[ 8]= reqObsPrev.i.z*reqObsNext.i.x + reqObsPrev.j.z*reqObsNext.j.x + reqObsPrev.k.z*reqObsNext.k.x;
    m.elements.te[ 1]= reqObsPrev.i.x*reqObsNext.i.y + reqObsPrev.j.x*reqObsNext.j.y + reqObsPrev.k.x*reqObsNext.k.y;
    m.elements.te[ 5]= reqObsPrev.i.y*reqObsNext.i.y + reqObsPrev.j.y*reqObsNext.j.y + reqObsPrev.k.y*reqObsNext.k.y;
    m.elements.te[ 9]= reqObsPrev.i.z*reqObsNext.i.y + reqObsPrev.j.z*reqObsNext.j.y + reqObsPrev.k.z*reqObsNext.k.y;
    m.elements.te[ 2]= reqObsPrev.i.x*reqObsNext.i.z + reqObsPrev.j.x*reqObsNext.j.z + reqObsPrev.k.x*reqObsNext.k.z;
    m.elements.te[ 6]= reqObsPrev.i.y*reqObsNext.i.z + reqObsPrev.j.y*reqObsNext.j.z + reqObsPrev.k.y*reqObsNext.k.z;
    m.elements.te[10]= reqObsPrev.i.z*reqObsNext.i.z + reqObsPrev.j.z*reqObsNext.j.z + reqObsPrev.k.z*reqObsNext.k.z;
    //
    // make quaternion from the rotation matrix
    var q=new THREE.Quaternion();
    q.setFromRotationMatrix(m);
    //
    // re-scale rotation angle depending on time elapsed
    //q.scaleAngle(f,rot);
    //
    // get average axis and add quaternion for closest complete number of revolutions
    var angle= f * Math.floor((dt/1000) * rot.rw/360.0 + 0.5) * 2.0 * Math.PI; // absolute rotation since prev
    var qn=new THREE.Quaternion();
    qn.setFromAxisAngle(rot,angle);
    //
    // calculate resulting quaternion
    //q.multiply(qn);
    //
    // apply resulting rotation to observer position and axis.
    var pos=new THREE.Vector3();
    pos.set(reqObsPrev.position.x,reqObsPrev.position.y,reqObsPrev.position.z);

    //console.log("**** Body before:",reqObsPrev.position.x,reqObsPrevos.x);

    pos.applyQuaternion(q);// gives undefined position ***************************************

    //console.log("**** Body after:",reqObsPrev.position.x,reqObsPrevos.x,origo);

    //console.log("*** stackObserver:",stackObserver);

    if (stackObserver[origo] === undefined ) {
	stackObserver.position.copy(pos);
    } else {
	stackObserver.position.copy(pos).add(stackObserver[origo].position);
	//console.log("**** Body origo:",reqObsPrev.position.x,reqObsPrevos.x,stackObserver[origo].x,origo);
    }
    //console.log("**** Interpolating observer:",stackObserver.x,stackObserver.y,stackObserver.z);
    if (stackObserver.i === undefined) stackObserver.i=new Vector3(); 
    pos.set(reqObsPrev.i.x,reqObsPrev.i.y,reqObsPrev.i.z);
    pos.applyQuaternion(q);
    stackObserver.i.copy(pos);
    if (stackObserver.j === undefined) stackObserver.j=new Vector3(); 
    pos.set(reqObsPrev.j.x,reqObsPrev.j.y,reqObsPrev.j.z);
    pos.applyQuaternion(q);
    stackObserver.j.copy(pos);
    if (stackObserver.k === undefined) stackObserver.k=new Vector3(); 
    pos.set(reqObsPrev.k.x,reqObsPrev.k.y,reqObsPrev.k.z);
    pos.applyQuaternion(q);
    stackObserver.k.copy(pos);
}



Stack.intlin = function (p,n,f) { // linear interpolation
    return ( p + (n-p)*f );
}
Stack.int2pi = function (p,n,f,t) { // targetted cyclic interpolation
    if (t === undefined) { t=0.0; };
    var diff = (n-p)%(2.0*Math.PI);
    var norb = Math.floor(0.5+(t -diff)/(2.0*Math.PI));
    return ( p + (diff + norb*2.0*Math.PI)*f ); 
}



// xmu = G * Mass
Stack.xmu = { sun     : 132712440018.0,
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


Stack.state2elements = function( s, e, r, xmu) {
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
    if (xmu === undefined) {
	console.log("Not calculating elements.");
    } else {
	var sx,sy,sz,svx,svy,svz;
	if (r === undefined) {
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
	//console.log("Elements a=",e.a," e=",e.e," i=",e.i," ascn=",e.o," argp=",e.w," v=",e.v," N=",e.n," mu=",xmu);
    }
}

Stack.elements2anomalies = function ( e ) {
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
    //console.log("Anomalies ecc=",e.c," mean=",e.m);
}

Stack.anomalies2elements = function ( e ) {
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
	e.c = Stack.solveEccentricAnomaly(e.e, e.m);
	// get true anomaly from eccentric anomaly
        e.v = 2.0 * Math.atan(Math.sqrt((1.0 + e.e)/(1.0 - e.e)) * Math.tan(e.c/2.0));
        if (e.v < 0.0) {e.v = e.v + 2.0*Math.PI;}
    }
}

Stack.elements2state = function ( s, e, r, xmu ) {
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
    var f = xmu/Math.sqrt(p);
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
    if (r === undefined) {
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

Stack.solveEccentricAnomaly = function(e, M){
    // calculate mean anomaly from eccentric anomaly
    if (e == 0.0) {
	return M;
    }  else if (e < 0.9) {
	var sol = Stack.solveEccentricAnomaly_(Stack.solveKepler(e, M), M, 6);
	return sol;
    } else if (e < 1.0) {
	var E = M + 0.85 * e * ((Math.sin(M) >= 0.0) ? 1 : -1);
	var sol = Stack.solveEccentricAnomaly_(Stack.solveKeplerLaguerreConway(e, M), E, 8);
	return sol;
    } else if (e == 1.0) {
	return M;
    } else {
	var E = Math.log(2 * M / e + 1.85);
	var sol = Stack.solveEccentricAnomaly_(Stack.solveKeplerLaguerreConwayHyp(e, M), E, 30);
	return sol;
    }
};

Stack.solveEccentricAnomaly_ = function (f, x0, maxIter) {
    var x = 0;
    var x2 = x0;
    for (var i = 0; i < maxIter; i++) {
	x = x2;
	x2 = f(x);
    }
    
    return x2;
}

Stack.solveKepler = function(e, M) {
    return function(x) {
	return x + (M + e * Math.sin(x) - x) / (1 - e * Math.cos(x));
    };
};

Stack.solveKeplerLaguerreConway = function(e, M) {
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

Stack.solveKeplerLaguerreConwayHyp = function(e, M) {
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

