//
// The chain runs continuously.
// The "run" subroutine calls sub-processes for
//  * input, 
//  * server-requests, 
//  * orbit-update, 
//  * 3D-modelling and 
//  * display. 
// The "run" subroutine finally re-calls itself.
// Some chain - elements are called every time the
// "run" subroutine is called, while some are only
// called at certain "step"s (like the orbit update).
//

console.log("Loading chain.js");

Chain = { step : 0,
	  redraw : true};

Chain.initRequest = function () {
    var url=getUrlVars();
    Request.launch(url["lat"],url["lon"],url["dtg"],url["label"],url["target"]);
}
Chain.initRenderer = function (context) {
    renderer = new Renderer(context);
    //renderer.setSize( window.innerWidth, window.innerHeight );
    //document.body.appendChild( renderer.domElement );
}

Chain.initScene = function () {
    scene = new Scene();
    scene.add (Planets.sun);
    scene.add (Planets.mercury);
    scene.add (Planets.venus);
    scene.add (Planets.earth);
    scene.add (Planets.moon);
    scene.add (Planets.mars);
    scene.add (Planets.jupiter);
    scene.add (Planets.saturn);
    scene.add (Planets.uranus);
    scene.add (Planets.neptune);
    scene.add (Planets.pluto);
    scene.display=0;
    scene.axis=0;
    scene.position=0;
    milkyway=new Milkyway();
    milkyway.init('sky/data/stars.json','sky/data/const.json','sky/data/descr.json');
}

Chain.initCamera = function () {
    camera = new Camera();
}

Chain.initControls = function () { 
    controls = new Controls( camera );
    controls.addEventListener( 'change', function () { renderer.render(scene,camera);} );
}
    

Chain.updateScene = function () {
    var bodies=model[model.current]["bodies"];
    if (bodies !== undefined) {
	// update position of all bodies
        //console.log("Sun:",bodies.sun.x,bodies.sun.y,bodies.sun.z," target:", bodies.mercury.x, bodies.mercury.y, bodies.mercury.z);
	Planets.sun.oldPosition.copy(Planets.sun.position);
	Planets.sun.position.set     (bodies.sun.x,     bodies.sun.y,     bodies.sun.z);
	Planets.mercury.oldPosition.copy(Planets.mercury.position);
	Planets.mercury.position.set (bodies.mercury.x, bodies.mercury.y, bodies.mercury.z);
	Planets.venus.oldPosition.copy(Planets.venus.position);
	Planets.venus.position.set   (bodies.venus.x,   bodies.venus.y,   bodies.venus.z);
	Planets.earth.oldPosition.copy(Planets.earth.position);
	Planets.earth.position.set   (bodies.earth.x,   bodies.earth.y,   bodies.earth.z);
	Planets.moon.oldPosition.copy(Planets.moon.position);
	Planets.moon.position.set    (bodies.moon.x,    bodies.moon.y,    bodies.moon.z);
	Planets.mars.oldPosition.copy(Planets.mars.position);
	Planets.mars.position.set    (bodies.mars.x,    bodies.mars.y,    bodies.mars.z);
	Planets.jupiter.oldPosition.copy(Planets.jupiter.position);
	Planets.jupiter.position.set (bodies.jupiter.x, bodies.jupiter.y, bodies.jupiter.z);
	Planets.saturn.oldPosition.copy(Planets.saturn.position);
	Planets.saturn.position.set  (bodies.saturn.x,  bodies.saturn.y,  bodies.saturn.z);
	Planets.uranus.oldPosition.copy(Planets.uranus.position);
	Planets.uranus.position.set  (bodies.uranus.x,  bodies.uranus.y,  bodies.uranus.z);
	Planets.neptune.oldPosition.copy(Planets.neptune.position);
	Planets.neptune.position.set (bodies.neptune.x, bodies.neptune.y, bodies.neptune.z);
	Planets.pluto.oldPosition.copy(Planets.pluto.position);
	Planets.pluto.position.set   (bodies.pluto.x,   bodies.pluto.y,   bodies.pluto.z);

    }
};

Chain.updateCamera = function () {
    var bodies=model[model.current]["bodies"];
    if (bodies !== undefined) {
	// update camera position
	//console.log ("Setting camera pos: ", bodies.observer.x,bodies.observer.y,bodies.observer.z);
	camera.position.set(bodies.observer.x,bodies.observer.y,bodies.observer.z);

	// point camera
	// camera.oldReference(); // clear any old reference
	// camera.newReference(Planets[scene.target].position,bodies.observer.zenith); // define new reference
	// camera.axis.k.cartesian2Spherical(camera.newRef); // store camera position in new reference
	// var oLat=camera.axis.k.lat; // current camera lat wrt new reference
	// var oLon=camera.axis.k.lon;  // current camera lon wrt new reference
	// Planets[scene.target].position.cartesian2Spherical(camera.newRef); // store target position in new reference
	// var nLat=Planets[scene.target].position.lat; // target camera lat wrt new reference
	// var nLon=Planets[scene.target].position.lon; // target camera lon wrt new reference
	// camera.shiftReference(); // activate the new reference (only copies new to old reference)
	// if (nLon-oLon > Math.Pi) {nLon=nLon-2*Math.PI}; // handle 0-360 discontinuity in Lon
	// if (nLon-oLon < -Math.Pi) {nLon=nLon+2*Math.PI}; // handle 0-360 discontinuity in Lon
	// // the camera target may change position during the tween, this is why the tween target is relative to a reference...
	// var tweenTarget={lat: nLat, lon: nLon}; // this is the tween target for lat and lon
	// var tween = new TWEEN.Tween(camera.axis.k).to(tweenTarget,2000);; // create tween that will last for 2 seconds.
	// tween.onUpdate(function() {camera.pointAtSpherical(bodies.observer.zenith)}); // update camera based on lat and lon
	// Chain.tweentime=new Date().getTime() + 2000; // set control timeout
	// scene.follow=scene.target; 
	// scene.target="follow";

	camera.setUp(bodies.observer.zenith); // up is always towards observer zenith...
    }
}

Chain.toggleFullScreen  = function () {
    var pos=0;
    if (!document.fullscreenElement &&    // alternative standard method
	!document.mozFullScreenElement && !document.webkitFullscreenElement && !document.msFullscreenElement ) {  // current working methods
	if (document.documentElement.requestFullscreen) {
	    document.documentElement.requestFullscreen();
	    pos=1;
	} else if (document.documentElement.msRequestFullscreen) {
	    document.documentElement.msRequestFullscreen();
	    pos=2;
	} else if (document.documentElement.mozRequestFullScreen) {
	    document.documentElement.mozRequestFullScreen();
	    pos=3;
	} else if (document.webkitRequestFullscreen) {
	    document.webkitRequestFullscreen();
	    pos=4;
	} else {
	    pos=5;
	}
    } else {
	if (document.exitFullscreen) {
	    document.exitFullscreen();
	    pos=6;
	} else if (document.msExitFullscreen) {
	    document.msExitFullscreen();
	    pos=7;
	} else if (document.mozCancelFullScreen) {
	    document.mozCancelFullScreen();
	    pos=8;
	} else if (document.webkitExitFullscreen) {
	    document.webkitExitFullscreen();
	    pos=9;
	} else {
	    pos=10;
	}
    }
//    var documentLog = document.getElementById("log");
//    documentLog.innerHTML="Pos: "+pos;
}

Chain.setDate= function() {
}

Chain.shiftDisplay= function() {
    scene.display=(scene.display+1)%2;
}

Chain.shiftAxis= function() {
    scene.axis=(scene.axis+1)%2;
}

Chain.shiftPosition= function() {
    scene.position=(scene.position+1)%2;
}

getUrlVars= function() {
    var vars = {};
    var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi,    
    function(m,key,value) {
      vars[key] = value;
    });
    return vars;
  }
 //var fType = getUrlVars()["type"];


Chain.init = function (canvas) {
    Chain.initRequest();
    Chain.initRenderer(canvas);
    Chain.initScene();
    Chain.initCamera();
    Chain.initControls();
}

Chain.run = function () {
    requestAnimFrame(Chain.run);
    Chain.nowMsec=new Date().getTime();
    if (Chain.lastMsec === undefined) {Chain.lastTimeMsec=Chain.nowMsec;};
    var deltaMsec   = Math.min(200, Chain.nowMsec - Chain.lastMsec)
    if (Chain.tweentime > Chain.nowMsec) {
	TWEEN.update();
	Chain.redraw=true;
    } else {
	if (controls.update()) { Chain.step=0;};
	if (controls.redraw) {
	    controls.redraw=false;
	    Chain.redraw=true;
	};
    }
    if (Request.update()) { Chain.step=0;};
    if (Orbit.update()) { Chain.step=0;};
    if (Chain.step == 0) {
	Model.update();
	if (Model.redraw) {
	    Chain.redraw=true;
	    Model.redraw=false;
	};
	Chain.updateScene();
    };
    if (Chain.redraw) {
	Chain.updateCamera();
	Chain.redraw=false;
    };
    Chain.lastTimeMsec= Chain.nowMsec;
    Chain.step=(Chain.step+1)%10;    
    renderer.render( scene, camera );
    //setTimeout(function(){ Chain.run();},10);
}

// requestAnim shim layer by Paul Irish
window.requestAnimFrame = (function(){
    return  window.requestAnimationFrame       || 
        window.webkitRequestAnimationFrame || 
        window.mozRequestAnimationFrame    || 
        window.oRequestAnimationFrame      || 
        window.msRequestAnimationFrame     || 
        function(/* function */ callback, /* DOMElement */ element){
            window.setTimeout(callback, 1000 / 60);
        };
})();
