//
// The chain runs continuously.
// The "run" subroutine calls sub-processes for
//  * input,
//  * server-requests, 
//  * stack-update, 
//  * 3D-stackling and 
//  * display. 
// The "run" subroutine finally re-calls itself.
// Some chain - elements are called every time the
// "run" subroutine is called, while some are only
// called at certain "step"s (like the orbit update).
//

console.log("Loading chain.js");

Chain = { step : 0,
	  redraw : true,
	  consTime : (new Date().getTime())-10000.0,
	  consReq : 0,
	  lastCon : 0,
	  lastTime : (new Date().getTime())-10000.0
	};

Chain.initRequest = function () {
    var url=getUrlVars();
    Request.launch(url["lat"],url["lon"],url["dtg"],url["hrs"],url["label"],url["target"],url["fov"],url["dir"],url["con"],url["play"]);
}
Chain.initRenderer = function (context) {
    renderer = new Renderer(context);
    //renderer.setSize( window.innerWidth, window.innerHeight );
    //document.body.appendChild( renderer.domElement );
}

Chain.initScene = function () {
    scene = new Scene();
    scene.observer=Planets.observer;
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
    controls.offzoom(1.0);
}
    

Chain.updateScene = function () {
    // update position of all bodies
    if (stack.reqId !== -1) {
	var st=stack[stack.current];
	Planets.copyObserver( st.observer,        Planets.observer);
	Planets.copyBody(     st.bodies.sun,      Planets.sun);
	Planets.copyBody(     st.bodies.mercury,  Planets.mercury);
	Planets.copyBody(     st.bodies.sun,      Planets.sun);
	Planets.copyBody(     st.bodies.mercury,  Planets.mercury);
	Planets.copyBody(     st.bodies.venus,    Planets.venus);
	Planets.copyBody(     st.bodies.earth,    Planets.earth);
	Planets.copyBody(     st.bodies.moon,     Planets.moon);
	Planets.copyBody(     st.bodies.mars,     Planets.mars);
	Planets.copyBody(     st.bodies.jupiter,  Planets.jupiter);
	Planets.copyBody(     st.bodies.saturn,   Planets.saturn);
	Planets.copyBody(     st.bodies.uranus,   Planets.uranus);
	Planets.copyBody(     st.bodies.neptune,  Planets.neptune);
	Planets.copyBody(     st.bodies.pluto,    Planets.pluto);
	scene.defined=true;
    } else {
	scene.defined=false;
    }
};

Chain.updateCamera = function () {
    // update camera position
    if (scene.defined) {
	camera.position.copy(scene.observer.position);
	camera.setUp(scene.observer.zenith); // up is always towards observer zenith...
    }
}

Chain.play  = function () {
    Stack.play();
};

Chain.pushUrl  = function () {
    Stack.pushUrl();
};

Chain.toggleConstellations  = function () {
    Chain.consTime=new Date().getTime();
    Chain.consReq = (+Chain.consReq+1)%2;
};
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
//    document.getElementById("log").innerHTML="Chain done...";
//    documentLog.innerHTML="Pos: "+pos;
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
	//console.log("URL item:",key," ",value)
      vars[key] = value;
    });
    return vars;
  }
 //var fType = getUrlVars()["type"];


Chain.init = function (canvas) {
    Chain.initCamera();
    Chain.initRequest();
    Chain.initRenderer(canvas);
    Chain.initScene();
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
    if (Stack.processNewRequests()) { 
	controls.offzoom(0.5);
	Chain.step=0;
    };
    if (Chain.step == 0) {
	Stack.update();
	if (Stack.redraw) {
	    Chain.redraw=true;
	    Stack.redraw=false;
	};
	Chain.updateScene();
    };
    if (Chain.redraw) {
	Chain.updateCamera();
	Chain.redraw=false;
    };
    Chain.lastTimeMsec= Chain.nowMsec;
    if (stack.reqId !== -1 && stack.play.speed > 0) {
	Chain.step=(Chain.step+1)%2;    
    } else {
	Chain.step=(Chain.step+1)%100;    
    };
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
            window.setTimeout(callback, 1000 / 30);
        };
})();
