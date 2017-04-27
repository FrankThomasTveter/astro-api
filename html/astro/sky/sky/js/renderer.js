console.log("Loading renderer.js");

Renderer = function (canvas) {
    this.canvas=canvas;
    this.context=canvas.getContext("2d");
    this.ow=0;
    this.oh=0;
    this.oow=false;
    this.pow=false;
    this.firstDraw=true;
    this.firstPoint=true;
    this.clockwise=true;
    this.pos = new Vector3();
    this.pos2 = new Vector3();
    this.pos3 = new Vector3();
    this.earthPos=new Vector3();
    this.objectAxis={i:new Vector3(),j:new Vector3(),k:new Vector3()};
    this.itemAxis={i:new Vector3(),j:new Vector3(),k:new Vector3()};
    this.viewAxis={i:new Vector3(),j:new Vector3(),k:new Vector3()};
    this.sunAxis={i:new Vector3(),j:new Vector3(),k:new Vector3()};
    this.zenithAxis={i:new Vector3(),j:new Vector3(),k:new Vector3()};
    this.penline=new Penline();
    this.spaceStyle;
    this.au = 149597870700e-3;
};

Renderer.prototype = {
    constructor : Renderer,
    render: function (scene, camera) {
	if (this.context.rect !== undefined) {
	    //console.log("Drawing context:", this.context);
	    // draw coordinate system
	    this.drawSpace(this.canvas, this.context, scene, camera);
	    //this.drawCoordinates(this.canvas, this.context, scene, camera);
	    this.drawConstellations(this.canvas, this.context, scene, camera);
	    this.drawStars(this.canvas, this.context, scene, camera);
	    this.drawBodies(this.canvas, this.context, scene, camera);
	    //this.drawLabels(this.canvas, this.context, scene, camera);
	} else {
	    console.log("Undefined context:");
	}
    },
    numberWithCommas : function (x) {
	return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, " ");
    },
    drawSpace : function (canvas, context,scene,camera) {
	context.rect(0,0,canvas.width,canvas.height);
	var r=0,g=0,b=0;//black
	this.spaceStyle="#"+(r).toString(16)+(g).toString(16)+(b).toString(16);
	context.fillStyle=this.spaceStyle;
	context.globalAlpha=1.0;
	context.fill();
    },
    drawBodies : function (canvas, context,scene,camera) {
	// sort bodies according to distance to observer
	var bodies=model[model.current]["bodies"];
	if (bodies !== undefined) {
	    this.zenithAxis=this.makeZenithAxis();
	    scene.sort(bodies.observer);
	    //
	    var itemLen=scene.items.length;
	    for(var ii = 0; ii < itemLen; ii++){
		var item=scene.items[ii];
		if (item.type === "sphere") {
		    this.drawSphere(canvas,context,scene,camera,item);
		}
	    };
	};
    },
    drawSphere : function (canvas,context,scene,camera,item) {
	camera.vector2Angle(item.position);
	camera.angle2Screen(item.position);
	//console.log("Drawing:",item.title,item.position);
	var alpha=Math.asin(item.radius/item.distance);
	// draw body
	if (alpha/camera.getFovX() < 0.005) { // a large body (earth) is not a circle on screen, but a polygon
	    this.drawSmallSphere(canvas,context,scene,camera,item,alpha);
	} else {
	    this.drawLargeSphere(canvas,context,scene,camera,item,alpha);
	}
    },
    drawSmallSphere :  function (canvas,context,scene,camera,item,alpha) { // no effects
	if (item.position.k !== undefined) {
	    var rr=0.5*alpha*camera.screen.width/camera.getFovX();
	    var angle= Math.acos(item.position.zp/item.position.r);
	    var rr= rr/Math.sin(angle);
	    context.fillStyle=item.color;
	    context.beginPath();
	    //console.log("Drawing:",item.position.k,item.position.w,item.position.h,camera);
	    context.arc(item.position.w,item.position.h,rr, 0, 2*Math.PI);
	    context.closePath();
	    context.fill();
	    context.fillStyle="white";
	    context.strokeStyle="white";
	    rr=rr+7;
	    context.beginPath();
	    //context.setLineDash([1, 3]);
	    context.arc(item.position.w,item.position.h,rr, 0, 2*Math.PI);
	    context.closePath();
	    context.stroke();
	    context.font="30px Arial";
	    var posx=item.position.w+rr+3;
	    var posy=item.position.h+10;
	    context.fillText(item.title,posx,posy);
	    posy+=20;
	    if (0.005 > camera.fovx) {
		context.font="16px Arial";
		this.pos.subVectors(item.position,camera.position);
		this.pos.cartesian2Spherical(this.zenithAxis);
		// 3.08567758e13 km = parsecs
		// 9.4605284e9 km = lightyear
		var dist=this.pos.length();
		var dkm=this.numberWithCommas(Math.round(dist));
		var dau=this.numberWithCommas(parseFloat(dist/this.au).toFixed(3));
		context.fillText(" dis="+dkm+" km ("+dau+" AU)",posx,posy);
		posy+=20;
		context.fillText(" azi="+parseFloat((360.0-(this.pos.lon*180.0/Math.PI))%360.0).toFixed(2)+" deg",posx,posy);
		posy+=20;
		context.fillText(" ele="+parseFloat(this.pos.lat*180.0/Math.PI).toFixed(2)+" deg",posx,posy);
		posy+=20;
	    }
	}
    },
    drawLargeSphere : function (canvas,context,scene,camera,item,alpha) {
	//console.log("Drawing: ",item.title);
	context.fillStyle=item.color;
	if (item.title === "The Sun") {
	    this.makeItemAxis(camera,item.position,this.itemAxis);
	    this.drawLargeSpherePlain(canvas,context,scene,camera,item.position,alpha,item.radius,item.distance,this.itemAxis,item.title);
	} else if (item.title === "The Earth") {
	    // the earth is an ellipsoide in the calculatinos, but we DRAW a round earth.
	    // so we need to find the center of the round earth that has the same approximate horison to the observer.
	    var observer=model[model.current]["bodies"]["observer"];
	    // find center of round earth
	    this.earthPos.copy(observer.zenith);
	    var distance=item.radius+ 2.0e-3;//*(1.0e0 + 4.8e-5);
	    this.earthPos.multiplyScalar(-distance);
	    this.earthPos.add(observer);
	    camera.vector2Angle(this.earthPos);
	    camera.angle2Screen(this.earthPos);
	    alpha=Math.max(1e-5,Math.asin(item.radius/distance));
	    context.globalAlpha=0.2;
	    // draw round earth
	    this.makeItemAxis(camera,item.position,this.itemAxis);
	    this.drawLargeSpherePlain(canvas,context,scene,camera,this.earthPos,alpha,item.radius,distance,this.itemAxis,item.title);
	    context.globalAlpha=1.0;
	    context.strokeStyle="#0000ff";
	    // make equator cirle
	    this.drawCirle(canvas,context,scene,camera,observer,10000,0.0,this.penline.clockwise);
	    // make compass
	    this.objectAxis.k.copy(observer.zenith);
	    this.objectAxis.j.crossVectors(observer.k,this.objectAxis.k);
	    this.objectAxis.j.normalize();
	    this.objectAxis.i.crossVectors(this.objectAxis.j, this.objectAxis.k);
	    // zenith
	    this.drawCompass(canvas,context,scene,camera,this.objectAxis);
	    context.fillStyle="#770000";
	    this.pos.copy(observer.zenith);
	    this.earthPos.multiplyScalar(item.radius);
	    context.font="30px Arial";
	    camera.vector2AngleLocal(this.pos);
	    camera.angle2Screen(this.pos);
	    if (this.pos.k !== undefined) {
		context.fillText("Z",this.pos.w,this.pos.h);
	    }
	    // north pole
	    this.pos.lon=0.0;
	    this.pos.lat=Math.PI*0.5;
	    this.pos.spherical2cartesian(observer);
	    camera.vector2AngleLocal(this.pos);
	    camera.angle2Screen(this.pos);
	    if (this.pos.k !== undefined) {
		context.fillText("N",this.pos.w,this.pos.h);
	    }
	    // south pole
	    this.pos.lon=0.0;
	    this.pos.lat=-Math.PI*0.5;
	    this.pos.spherical2cartesian(observer);
	    camera.vector2AngleLocal(this.pos);
	    camera.angle2Screen(this.pos);
	    if (this.pos.k !== undefined) {
		context.fillText("S",this.pos.w,this.pos.h);
	    }
	} else {
	    //console.log("Drawing: ",item.title);
	    this.makeItemAxis(camera,item.position,this.itemAxis);
	    if (this.drawLargeSpherePlain(canvas,context,scene,camera,item.position,alpha,item.radius,item.distance,this.itemAxis,item.title)) {
		this.drawLargeSphereShadows(canvas,context,scene,camera,item);
	    }
	};
    },
    drawLargeSpherePlain : function (canvas,context,scene,camera,origo,alpha,radius,distance,axis,title){
	// set context styles
	var ret=false;
	var n=64;
	this.penline.open(context,camera);
	for (var ii = 0; ii <= n; ii++){ // loop over points
	    this.pos.r=radius;
	    this.pos.lon=ii/n*Math.PI*2;
	    this.pos.lat=-alpha;
	    this.pos.spherical2cartesian(axis);
	    this.pos.add(origo);
	    camera.vector2Angle(this.pos);
	    camera.angle2Screen(this.pos);
	    this.penline.addPoint(this.pos, this.penline.clockwise);
	};
	if (this.penline.close()) {
	    context.fill();
	    ret= true;
	    if (origo.k !== undefined) {
		var rr=0.5*alpha*camera.screen.width/camera.getFovX();
		var angle= Math.acos(origo.zp/origo.r);
		var rr= rr/Math.sin(angle);
		if (rr < 5000) {
		    rr=rr+7;// rr is size on screen
		    var posx=origo.w+rr+3;
		    var posy=origo.h+10;
		    context.fillStyle="white";
		    context.strokeStyle="white";
		    context.font="30px Arial";
		    context.fillText(title,posx,posy);
		    posy+=20;
		    if (rr > 50 || 0.005 > camera.fovx) {
			context.font="16px Arial";
			this.pos.subVectors(origo,camera.position);
			this.pos.cartesian2Spherical(this.zenithAxis);
			// 3.08567758e13 km = parsecs
			// 9.4605284e9 km = lightyear
			var dist=this.pos.length();
			var dkm=this.numberWithCommas(Math.round(dist));
			var dau=this.numberWithCommas(parseFloat(dist/this.au).toFixed(3));
			context.fillText(" dis="+dkm+" km ("+dau+" AU)",posx,posy);
			posy+=20;
			context.fillText(" azi="+parseFloat((360.0-(this.pos.lon*180.0/Math.PI))%360.0).toFixed(2)+" deg",posx,posy);
			posy+=20;
			context.fillText(" ele="+parseFloat(this.pos.lat*180.0/Math.PI).toFixed(2)+" deg",posx,posy);
			posy+=20;
		    }
		};
	    }
	};
	return ret;
    },
    drawLargeSphereShadows : function (canvas,context,scene,camera,item) {
	context.fillStyle=this.spaceStyle;
	var bodies=model[model.current]["bodies"];
	this.makeSunAxis(camera,item.position,bodies.sun,this.itemAxis,this.sunAxis);
	this.drawSunShadow(canvas,context,scene,camera,item);
	if (item.title == "The Moon") {
	    this.drawObjectShadow(canvas,context,scene,camera,item,Planets.earth,Planets.sun,this.sunAxis);
	}
    },
    drawObjectShadow : function(canvas,context,scene,camera,item,object,sun,sunAxis) {
	// make viewAxis, k->item, i-> away from object
	this.viewAxis.k.subVectors(item.position,camera.position);
	this.viewAxis.k.normalize();
	this.pos2.subVectors(object.position,item.position);
	var ll=sunAxis.k.dot(this.pos2);
	//console.log("Shadow distance:",ll);
	if (ll < 0) { return;};
	this.viewAxis.j.crossVectors(this.pos2,this.viewAxis.k);
	this.viewAxis.j.normalize();
	this.viewAxis.i.crossVectors(this.viewAxis.j, this.viewAxis.k);
	// make objectAxis, k->sun, i-> away from observer
	this.objectAxis.k.copy(sunAxis.k);
	this.objectAxis.j.crossVectors(this.objectAxis.k,this.viewAxis.k);
	this.objectAxis.j.normalize();
	this.objectAxis.i.crossVectors(this.objectAxis.j, this.objectAxis.k);
	// get radii
	this.pos3.subVectors(sun.position,item.position);
	var distsi=this.pos3.length();
	this.pos3.subVectors(sun.position,object.position);
	var distso=this.pos3.length();
	var distio=this.objectAxis.k.dot(this.pos2);
	this.pos3.copy(this.objectAxis.k);
	this.pos3.multiplyScalar(distio);
	this.pos3.sub(this.pos2);
	var dd=this.pos3.length();
	var distfo=(object.radius/(sun.radius-object.radius))*distso;
	var distco=(object.radius/(sun.radius+object.radius))*distso;
	var umbra=(distfo-distio)*object.radius/distfo;
	var penumbra=(distco+distio)*object.radius/distco;
	// get distance between object and item-sun line
	//console.log("Shadow width:",dd/(item.radius+object.radius));
	if (dd > item.radius + penumbra) { return;}
	context.globalAlpha=0.7;
	this.drawSphereShadow(canvas,context,scene,camera,item,object,penumbra,sun,sunAxis);
	context.globalAlpha=1.0;
	if (dd > item.radius + umbra) { return;}
	this.drawSphereShadow(canvas,context,scene,camera,item,object,umbra,sun,sunAxis);
    },
    drawSphereShadow : function(canvas,context,scene,camera,item,object,radius,sun,sunAxis) {
	// move around sun-shadow border of object
	var first=true;
	var startLon,stopLon;
	var last=false;
	var lastLon;
	var lastOov=false;
	var sunItem=this.getSunItem(this.itemAxis,sunAxis);
	var cnt=0;
	var n=64*4;
	this.penline.open(context,camera);
	for (var ii = 0; ii <= n; ii++){ // loop over points, starting out of view
	    this.pos.r=radius;
	    this.pos.lon=(ii/n)*Math.PI*2;
	    this.pos.lat=0.0;
	    this.pos.spherical2cartesian(this.objectAxis);
	    this.pos.add(this.pos2); // relative to item
	    var hh=this.pos.dot(this.objectAxis.k);
	    this.pos3.copy(this.objectAxis.k);
	    this.pos3.multiplyScalar(hh);
	    this.pos.sub(this.pos3);
	    var dd=this.pos.length();
	    if (dd <= item.radius) { // shadow on sphere
		hh=Math.sqrt(item.radius*item.radius-dd*dd);
		this.pos3.copy(this.objectAxis.k);
		this.pos3.multiplyScalar(hh);
		this.pos.add(this.pos3);
	    } else {
		this.pos.normalize().multiplyScalar(item.radius);
	    };
	    // check if visible
	    this.pos.cartesian2Spherical(this.viewAxis);
	    if (this.pos.lat > 0) { // backside of sphere...
		var xlat=this.pos.lat;
		var xlon=this.pos.lon;

		this.pos.cartesian2Spherical(sunAxis);
		this.pos.lat=this.getSunLat(this.pos,sunAxis,sunItem);

		var slat=this.pos.lat;

		this.pos.spherical2cartesian(sunAxis);
		this.pos.cartesian2Spherical(this.viewAxis);

		//console.log("Sunlat:",slat,xlat,xlon,this.pos.lat,this.pos.lon);
	    };
	    this.pos.add(item.position);
	    camera.vector2Angle(this.pos);
	    camera.angle2Screen(this.pos);
	    this.penline.addPoint(this.pos, this.penline.clockwise);
	};
	if (this.penline.close()) {
	    context.fill();
	    ret= true;
	};
    },
    drawSunShadow : function(canvas,context,scene,camera,item) {
	var ret=false;
	var first=true;
	var startLon,stopLon;
	var last=false;
	var lastLon;
	var lastOov=false;
	var cnt=0;
	var n=64;
	this.penline.open(context,camera);
	for (var ii = 0; ii <= n; ii++){ // loop over points, starting out of view
	    this.pos.r=item.radius;
	    this.pos.lon=ii/n*Math.PI*2;
	    this.pos.lat=0.0;
	    this.pos.spherical2cartesian(this.sunAxis);
	    this.pos.cartesian2Spherical(this.itemAxis);
	    if (this.pos.lon < 0.0) {this.pos.lon=this.pos.lon+2*Math.PI;}
	    if (this.pos.lat > 0) { // backside of sphere...
		cnt=cnt+1
		lastLon=this.pos.lon;
		if (ii != 0 && ! lastOov  && startLon === undefined) {
		    startLon=lastLon;
		}
		lastOov=true;
	    } else {
		if (ii != 0 && lastOov && stopLon === undefined) {
		    stopLon=lastLon;
		}
		lastOov=false;
		this.pos.add(item.position);
		camera.vector2Angle(this.pos);
		camera.angle2Screen(this.pos);
		this.penline.addPoint(this.pos, this.penline.clockwise);
	    }
	};
	var dlon=(stopLon-startLon)/Math.max(1,cnt-1);
	for (var ii = 0; ii < cnt; ii++){ // loop over points
	    this.pos.r=item.radius;
	    this.pos.lon=startLon+ii*dlon;;
	    this.pos.lat=0.0;
	    this.pos.spherical2cartesian(this.itemAxis);
	    this.pos.add(item.position);
	    camera.vector2Angle(this.pos);
	    camera.angle2Screen(this.pos);
	    this.penline.addPoint(this.pos, this.penline.clockwise);
	}
	if (this.penline.close()) {
	    context.fill();
	    ret= true;
	};
	return ret;
    },
    makeZenithAxis : function () {
	var observer=model[model.current]["bodies"]["observer"];
	var axis={i:new Vector3(),j:new Vector3(),k:new Vector3()};
	axis.k.copy(observer.zenith);
	axis.j.crossVectors(axis.k,observer.k);
	axis.j.normalize();
	axis.i.crossVectors(axis.j, axis.k);
	return axis;
    },
    makeItemAxis : function (camera,position,axis) {
	axis.k.subVectors(position,camera.position);
	axis.k.normalize();
	axis.j.crossVectors(camera.axis.k,axis.k);
	if (axis.j.isZero()) {
	    axis.j.makeNormal(axis.k);
	} else {
	    axis.j.normalize();
	}
	axis.i.crossVectors(axis.j, axis.k);
    },
    makeSunAxis : function (camera,itemPosition,sunPosition,itemAxis,sunAxis) {
	itemAxis.k.subVectors(itemPosition,camera.position);
	sunAxis.k.subVectors(sunPosition,itemPosition);
	itemAxis.k.normalize();
	sunAxis.k.normalize();
	itemAxis.j.crossVectors(itemAxis.k,sunAxis.k);
	if (itemAxis.j.isZero()) {
	    itemAxis.j.makeNormal(itemAxis.k);
	} else {
	    itemAxis.j.normalize();
	}
	itemAxis.i.crossVectors(itemAxis.j, itemAxis.k); // point towards sun
	sunAxis.j.crossVectors(sunAxis.k,itemAxis.k);
	if (sunAxis.j.isZero()) {
		sunAxis.j.makeNormal(sunAxis.k);
	} else {
	    sunAxis.j.normalize();
	}
	sunAxis.i.crossVectors(sunAxis.j, sunAxis.k); // away from camera...
    },
    drawCirle: function (canvas,context,scene,camera,axis,radius,lat,dir) {
	var n=64;
	this.penline.open(context,camera);
	for (var ii = 0; ii <= n; ii++){ // loop over points
	    this.pos.r=radius;
	    this.pos.lon=(ii/n)*Math.PI*2;
	    this.pos.lat=lat;
	    this.pos.spherical2cartesian(axis);
	    camera.vector2AngleLocal(this.pos);
	    camera.angle2Screen(this.pos);
	    this.penline.addPoint(this.pos, dir);
	};
	if (this.penline.close()) {
	    context.stroke();
	};
    },
    drawCompass: function (canvas,context,scene,camera,axis) {
	context.font="30px Arial";
	var lons=[0.0,    22.5,    45.5,    67.5,    
		  90.0,  112.5,   135.0,   157.5, 
		  180.0, 202.5,   225.0,   247.5, 
		  270.0, 292.5,   315.0,   337.5];
	var lab=["S","SSW","SW","WSW",
		 "W","WNW","NW","NNW",
		 "N","NNE","NE","ENE",
		 "E","ESE","SE","SSE"];
	var style=["#ff9999","#ff0000","#ff0000","#ff0000",
		   "#ff9999","#ff0000","#ff0000","#ff0000",
		   "#ff9999","#ff0000","#ff0000","#ff0000",
		   "#ff9999","#ff0000","#ff0000","#ff0000"];
	this.pos.r=1.0;
	// set context styles
	var ow,oh,oov;
	var n=lons.length;
	context.textAlign="center";
	for (var ii = 0; ii < n; ii++){ // loop over points
	    var label=lab[ii];
	    // draw triangle
	    var first=true;
	    this.pos.lon=(lons[ii])*Math.PI/180.0;
	    this.pos.lat=0.0;
	    this.pos.spherical2cartesian(axis);
	    camera.vector2AngleLocal(this.pos);
	    camera.angle2Screen(this.pos);
	    if (this.pos.k !== undefined) {
		context.fillStyle=style[ii];
		context.fillText(label,this.pos.w,this.pos.h);
	    }
	}
	context.textAlign="left";
    },
    drawStars : function (canvas,context,scene,camera) {
	milkyway.drawStars(canvas,context,scene,camera);
    },
    drawConstellations : function (canvas,context,scene,camera) {
	milkyway.drawConstellations(canvas,context,scene,camera);
    },
    drawLabels : function (canvas, context,scene,camera) {
	// var pos=new Vector3();
	// pos.r=1;
	// var lat = 0.0;
	// var lon = 0.0;
	// pos.lat=lat;
	// pos.lon=lon
	// pos.spherical2cartesian(camera.axis);
	// camera.vector2AngleLocal(pos);
	// camera.angle2Screen(pos);
	// context.font="15px Arial";
	// var s = "xp="+(pos.xp).toString()+" yp="+(pos.yp).toString()+" zp="+(pos.zp).toString()
	//     +" i="+(pos.i).toString()+" j="+(pos.i).toString()
	//     +" w="+(pos.w).toString()+" h="+(pos.h).toString()
	//     +" fy="+(camera.fovy).toString()+" sy="+(camera.screen.height).toString();
	// context.fillText(s,30,60);
    },
    drawCoordinates : function (canvas, context, scene, camera ) {	
	context.strokeStyle="#222222";
	//context.setLineDash([]);
	context.globalAlpha=1.0;
	var bodies=model[model.current]["bodies"];
	if (bodies !== undefined) {
	    var limits=camera.getSphericalLimits(bodies.observer);
	    this.findTags(limits);
	    this.drawLatitudes(canvas, context, scene, camera, limits, bodies.observer );
	    this.drawLongitudes(canvas, context, scene, camera, limits, bodies.observer );
	}
    },
    drawLatitudes : function (canvas, context, scene, camera, limits, axis ) {	
	var bodies=model[model.current]["bodies"];
	if (bodies !== undefined && limits.taglon !== undefined) {
	    var pos=new Vector3();
	    pos.r=1.0;
	    var nlon=Math.max(16,Math.ceil(limits.dlon*36/Math.PI));
	    for (var ilat = limits.latstart-1; ilat <= limits.latstop+1; ilat++){
		var lat=ilat*limits.taglat;
		pos.lat=lat;
		// draw main latitude
		context.strokeStyle="#770000";
		var firstVisible=true;
		if (lat <= Math.PI*0.5 & lat >= -Math.PI*0.5) {
		    this.drawLatitude(canvas, context, scene, camera, limits, axis, pos, nlon, firstVisible );
		}
	    }
	}
    },
    drawLatitude : function (canvas, context, scene, camera, limits, axis, pos, nlon, firstVisible ) {	
	var firstPlot=true;	
	var digits=Math.pow(10,-Math.min(Math.floor(Math.log10(limits.dlon)),0));
	var latdeg=pos.lat*180.0/Math.PI;
	for (var ilon = 0; ilon <= nlon; ilon++){
	    var lon=limits.lon - limits.dlon + 2*limits.dlon*ilon/nlon;
	    pos.lon=lon;
	    pos.spherical2cartesian(axis);
	    //console.log("Position:",pos);
	    //camera.check(axis);
	    camera.vector2AngleLocal(pos);
	    if (pos.k !== undefined) {
		camera.angle2Screen(pos);
		var plot=(pos.w-100 < camera.screen.left + camera.screen.width && 
			 pos.w+100 > camera.screen.left && 
			 pos.h+100 > camera.screen.top  && 
			 pos.h-100 < camera.screen.top + camera.screen.height);
		//console.log("Pos:",pos.w,pos.h,camera.screen.left,camera.screen.width,camera.screen.top,camera.screen.height,plot);
		if (plot) {
		    if (firstPlot) {
			context.beginPath();
			context.moveTo(pos.w,pos.h);
			firstPlot=false;
		    } else {
			context.lineTo(pos.w,pos.h);
		    };
		    var visible=(pos.w+50 < camera.screen.left + camera.screen.width && 
			 pos.w-50 > camera.screen.left && 
			 pos.h-50 > camera.screen.top  && 
			 pos.h+50 < camera.screen.top + camera.screen.height);
		    if (visible) {
			if (firstVisible) {
			    firstVisible=false;
			    var label=Math.round(latdeg*digits)/digits;
			    context.fillStyle="#773333";
			    context.fillText(label,pos.w,pos.h);
			}
		    }
		    //console.log("Plotting coordinate:",lat,lon,pos.w,pos.h);
		    
		}
	    } else {
		//console.log("Hidden:",lat,lon,limits.lat,limits.lon,limits.dlat,limits.dlon);
	    }
	}
	if (! firstPlot) {
	    context.stroke();
	}
    },
    drawLongitudes : function (canvas, context, scene, camera, limits, axis ) {	
	var bodies=model[model.current]["bodies"];
	if (bodies !== undefined && limits.taglon !== undefined) {
	    var pos=new Vector3();
	    pos.r=1.0;
	    var nlat=Math.max(16,Math.ceil(limits.dlat*36/Math.PI));
	    for (var ilon = limits.lonstart-1; ilon <= limits.lonstop+1; ilon++){
		var lon=ilon*limits.taglon;
		pos.lon=lon;
		// draw main lonitude
		context.strokeStyle="#770000";
		var firstVisible=true;
		if (lon <= Math.PI*2 & lon >= -Math.PI*2) {
		    this.drawLongitude(canvas, context, scene, camera, limits, axis, pos, nlat, firstVisible );
		}
	    }
	}
    },
    drawLongitude : function (canvas, context, scene, camera, limits, axis, pos, nlat, firstVisible ) {	
	var firstPlot=true;	
	var digits=Math.pow(10,-Math.min(Math.floor(Math.log10(limits.dlat)),0));
	var londeg=pos.lon*180.0/Math.PI;
	for (var ilat = 0; ilat <= nlat; ilat++){
	    var lat=limits.lat - limits.dlat + 2*limits.dlat*ilat/nlat;
	    pos.lat=lat;
	    pos.spherical2cartesian(axis);
	    //console.log("Position:",pos);
	    //camera.check(axis);
	    camera.vector2AngleLocal(pos);
	    if (pos.k !== undefined) {
		camera.angle2Screen(pos);
		var plot=(pos.w-100 < camera.screen.left + camera.screen.width && 
			 pos.w+100 > camera.screen.left && 
			 pos.h+100 > camera.screen.top  && 
			 pos.h-100 < camera.screen.top + camera.screen.height);
		//console.log("Pos:",pos.w,pos.h,camera.screen.left,camera.screen.width,camera.screen.top,camera.screen.height,plot);
		if (plot) {
		    if (firstPlot) {
			context.beginPath();
			context.moveTo(pos.w,pos.h);
			firstPlot=false;
		    } else {
			context.lineTo(pos.w,pos.h);
		    };
		    var visible=(pos.w+50 < camera.screen.left + camera.screen.width && 
			 pos.w-50 > camera.screen.left && 
			 pos.h-50 > camera.screen.top  && 
			 pos.h+50 < camera.screen.top + camera.screen.height);
		    if (visible) {
			if (firstVisible) {
			    firstVisible=false;
			    var label=Math.round(londeg*digits)/digits;
			    context.fillStyle="#773333";
			    context.fillText(label,pos.w,pos.h);
			}
		    }
		    //console.log("Plotting coordinate:",lat,lat,pos.w,pos.h);
		    
		}
	    } else {
		//console.log("Hidden:",lat,lat,limits.lat,limits.lat,limits.dlat,limits.dlat);
	    }
	}
	if (! firstPlot) {
	    context.stroke();
	}
    },
    // returns area of "p" not covered by "m"
    getVisibleArea : function (p, // radius of primary body
				   m, // radius of blocking body
				   d) { // distance between center of primary and blocking
	var d2=d*d;
	var p2=p*p;
	var m2=m*m;
	var a = Math.PI * p2;
	if ( d > p+m) { // no overlap
	    a = Math.PI * p2;
	} else if ( d+m<p ) {  // 2 inside 1
	    a = Math.PI * (p2 - m2);
	} else if ( d+p<m ) {  // 1 inside 2
	    a = 0;
	} else {
	    // http://mathworld.wolfram.com/Circle-CircleIntersection.html
	    a= Math.PI*p2-(p2*Math.acos((d2+p2-m2)/(2*d*p)) +
			   m2*Math.acos((d2+m2-p2)/(2*d*m)) -
			   0.5*Math.sqrt((-d+p+m)*(d+p-m)*(d-p+m)*(d+p+m)));
	}
	return a;
    },
    findTags : function (ret) {
	var d2p=Math.PI/180.0;
	//var tags=[d2p*10.0,  d2p*1.0,  d2p*1.0/6.0, d2p*1.0/60.0, d2p*1.0/3.60e2, d2p*1.0/3.6e3, d2p*1.0/3.6e4,  d2p*1.0/3.6e5];
	var tags=[d2p*10.0,  d2p*1.0,  d2p*1.0e-1, d2p*1.0e-2, d2p*1.0e-3, d2p*1.0e-4, d2p*1.0e-5, d2p*1.0e-6]; 
	//
	for (var ii=0; ii<tags.length-1;ii++){
	    if (ret.taglon === undefined) {
		if (ret.dlon/tags[ii] > 1 || ii == tags.length-1) {
		    ret.taglon=tags[ii];
		    if (ret.dlon/tags[ii+1] < 30) {
			ret.sublon=tags[ii+1];
			ret.loncnt=Math.floor(0.5+tags[ii]/tags[ii+1]);
			ret.lonint=tags[ii]/ret.dlon;
		    }
		} 
	    } 
	    if (ret.taglat === undefined  || ii == tags.length-1) {
		if (ret.dlat/tags[ii] > 1) {
		    ret.taglat=tags[ii];
		    if (ret.dlat/tags[ii+1] < 30) {
			ret.sublat=tags[ii+1];
			ret.latcnt=Math.floor(0.5+tags[ii]/tags[ii+1]);
			ret.latint=tags[ii]/ret.dlat;
		    }
		} 
	    } 
	}
	if (ret.taglon !== undefined && ret.taglat !== undefined ) {
	    ret.lonstart=Math.ceil((ret.lonmin)/ret.taglon);
	    ret.lonstop=Math.floor((ret.lonmax)/ret.taglon);
	    ret.latstart=Math.ceil((ret.latmin)/ret.taglat);
	    ret.latstop=Math.floor((ret.latmax)/ret.taglat);
	}
    },
    getSunItem : function (itemAxis,sunAxis) {
	// cosine of angle between item-sun and camera-item
	var c=itemAxis.k.dot(sunAxis.k);
	//console.log("Dot:",c,itemAxis.k,sunAxis.k);
	return c;
    },
    getSunLat : function (pos,sunAxis,sunItem) {
	// assumes sunAxis.i points away from camera...
	var res= Math.PI;
	var clon=pos.dot(sunAxis.i)/pos.length();
	if (Math.abs(clon) > 1e-10) {
	    if (Math.abs(sunItem) > 1e-10) {
		var sunItemS=Math.sqrt(1.0-sunItem*sunItem);
		if (Math.abs(sunItemS) > 1e-10) {
		    res= Math.atan((-sunItem)/(sunItemS*clon));
		    if (res < 0) {res=res+Math.PI;};
		} else {
		    res = Math.PI*0.5;
		}
	    } else {
		res = Math.PI;
	    }
	} else {
	    ret = Math.PI*0.5;
	}
	//console.log("Clon:",clon,sunItem,res);
	return (0.5*Math.PI - res);
    }
 };
