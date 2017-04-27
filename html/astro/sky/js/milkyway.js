Milkyway = function () { 
    this.X =          0;
    this.Y =          1;
    this.Z =          2;
    this.MAG =        3;
    this.SPECT =      4;
    this.RA =         5;
    this.DEC =        6;
    this.LABEL =      7;
    this.CLASS =      8;
    this.CONST =      9;
    this.CONSTID =    10;
    this.NAME =       11;
    this.BRIGHTNESS = 12;
    this.BRIGHTNLEV = 13;
    this.SMAG = 5;
    this.NMAG = 10;
    this.DIST = 5.0*Math.PI/180.0;
    this.spectralColors = [
	0xfbf8ff,
	0xc8d5ff,
	0xd8e2ff,
	0xe6ecfc,
	0xfbf8ff,
	0xfff4e8,
	0xffeeda,
	0xfeead3,
	0xfccecb,
	0xebd3da,
	0xe7dbf3
    ];
    this.constellations=[];
    this.descriptions;
    this.starsJson = 'js/jsorrery/data/stars.json',
    this.constJson = 'js/jsorrery/data/const.json',
    this.descrJson = 'js/jsorrery/data/descr.json',
    this.pxRatio = (window.devicePixelRatio || 1);
    //keys of the loaded array
    this.namedStars = {};
    this.starList=[];
    this.initialised=false;
    // workspace
    this.canvasWidth;
    this.canvasHeight;
    this.canvasData;
    this.ll = this.starList.length;
    this.fact;
    this.cnt;
    this.position;
    this.iw;
    this.ih;
    this.index;
    this.mf;
    this.r;
    this.g;
    this.b;
    this.a;
    this.ln10=Math.log(10);
    this.parsec= 3.08567758e13;// km 
    this.lightyear = 9.4605284e12; // km
    this.cons;
    this.cpos;
    this.dist;
    this.ow;
    this.oh;
    this.period=5000.0;
    this.first
};

Milkyway.prototype = {
    constructor : Milkyway,
    lightenDarkenColor : function (hex, amount) {
	var col = [hex >> 16, (hex >> 8) & 0x00FF,  hex & 0x0000FF];
	var mc=Math.max(1,Math.max(col[0],col[1],col[2]));
	var ma=Math.min(amount,255/mc);
	col[0]=Math.max(0,Math.min(255,col[0]*ma));
	col[1]=Math.max(0,Math.min(255,col[1]*ma));
	col[2]=Math.max(0,Math.min(255,col[2]*ma));
	return col;
    },
    numberWithCommas : function (x) {
	return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, " ");
    },
    init : function(starsJson, constJson, descrJson) {
	var this_ = this;
	this.NLAT=Math.ceil(Math.PI/this.DIST);
	this.NLON=2*this.NLAT;
	for (var imag=this.SMAG;imag < this.NMAG; imag++) {
	    this.starList[imag]=[];
	    for (var ilat=0;ilat < this.NLAT; ilat++) {	    
		this.starList[imag][ilat]=[];
		for (var ilon=0;ilon < this.NLON; ilon++) {	    
		    this.starList[imag][ilat][ilon]=[];
		}
	    }
	};
	//console.log("Initialised:", this.NMAG,this.NLAT,this.NLON);
	starsJson=starsJson || this.starsJson;
	constJson=constJson || this.constJson;
	descrJson=descrJson || this.descrJson;
	var onStarsLoaded = new ResourceLoader().loadJSON(starsJson);
	var onConstLoaded = new ResourceLoader().loadJSON(constJson);
	var onDescrLoaded = new ResourceLoader().loadJSON(descrJson);
	var onReady = $.Deferred();
	$.when(onStarsLoaded,onConstLoaded,onDescrLoaded).then(
	    function(stars,consts,descr) {
		this_.generateDescr(descr[0],this_);
		this_.generateConst(consts[0],this_);
		this_.generateStars(stars[0],this_);
		onReady.resolve();
	    },
            function(starsResponse, constResonse, descrResponse){
                console.log("Failed:", starsResponse);
            }

	);
	return onReady.promise();
    },
    generateStars : function(stars,this_) {
	var star;
	var starVect;
	var mag;
	var name;
	var spectralType;
	var starColor;
	var cnt=0;
	var count = stars.length;
	console.log("Adding stars:",count);
	for( var i=0; i<count; i++ ){
	    //if (i > 1000) {continue;};
	    star = stars[i];
	    //console.log("Looping:",i,star);
	    position = new Vector3(star[this_.X], star[this_.Y], star[this_.Z]);
	    position.cartesian2Spherical();
	    //console.log("Looping:",i,star,position);
	    if(position.x === 0 && position.y === 0 && position.z === 0) continue;//dont add the sun
	    //position.multiplyScalar(9.4605284e9);//normalize().
	    position.multiplyScalar(this.parsec);// parsecs...
	    mag = position.mag = star[this_.MAG];
	    name = star[this_.NAME] || "";
	    spectralType = Math.round(star[this_.SPECT]);
	    starColor  = this_.spectralColors[spectralType] || this_.spectralColors.F;
	    /**/
	    //position.size = Math.floor(10 * (2 + (1 / mag))) / 10;
	    var starRGB = this_.lightenDarkenColor(starColor, Math.pow(10,-mag/2.5));
	    position.color = starRGB;
	    position.maxcolor=Math.max(starRGB[0],starRGB[1],starRGB[2]);
	    //console.log("Adding star:",starRGB,spectralType,mag);
	    if(name) {
		this_.namedStars[name] = position;
		cnt++;
	    };
	    position.ra=star[this_.RA];
	    position.dec=star[this_.DEC];
	    position.lab=star[this_.LABEL] || "";
	    position.cls=star[this_.CLASS] || "";
	    position.con=star[this_.CONST] || "";
	    position.cid=star[this_.CONSTID];
	    position.name=name;
	    position.brt=star[this_.BRIGHTNESS] || "";
	    position.lev=star[this_.BRIGHTNLEV] || "";
	    position.imag = Math.min(this.NMAG-1,Math.max(this.SMAG,Math.floor(this.getIMag(position.mag))));
	    position.ilat = this.modulus(this.getILat(position.lat),this.NLAT);
	    position.ilon = this.modulus(this.getILon(position.lon),this.NLON);
	    position.phase=Math.random()*Math.PI*2.0;
	    this.list = this.starList[position.imag][position.ilat][position.ilon];
	    this.list.push( position );
	};
	console.log("Stars with name:",cnt);
	this_.initialised=true;
    },
    generateConst : function(consts,this_) {
	for (var con in consts) {
	    var spos=new Vector3();
	    var xpos=new Vector3();
	    var scnt=0;
	    var smg=20;
	    var strokes=[];
	    var stks=consts[con];
	    var lenii = stks.length;
	    for( var ii=0; ii<lenii; ii++ ){
		var stk=stks[ii];
		var stroke=[];
		var lenjj=stk.length;
		for( var jj=0; jj<lenjj; jj++ ){
		    var pos=new Vector3(stk[jj][0],stk[jj][1],stk[jj][2]);
		    xpos.copy(pos).normalize();
		    var mg=stk[jj][3];
		    pos.brt=stk[jj][4];
		    pos.lev=stk[jj][5];
		    smg=Math.min(mg,smg);
		    pos.cartesian2Spherical();
		    pos.multiplyScalar(this.parsec);// parsecs...
		    spos.add(xpos);
		    scnt++;
		    stroke.push(pos);
		};
		strokes.push(stroke);
	    };
	    spos.divideScalar(Math.max(1,scnt));
	    spos.multiplyScalar(this.parsec);
	    this_.constellations.push([spos,strokes,smg,con,
				       this_.getConstellation(con,this_),
				       this_.getConstellationD(con,this_)]);
	}
	console.log("Adding constellations.");
    },
    generateDescr : function(descr,this_) {
	this_.descriptions=descr;
	console.log("Adding descriptions.");
    },
    componentToHex: function (c) {var hex = c.toString(16);return hex.length == 1 ? "0" + hex : hex;},
    rgbToHex : function (r, g, b) {
	return "#" + this.componentToHex(r) + this.componentToHex(g) + this.componentToHex(b);},
    drawStars : function(canvas,context,scene,camera){
	var tnow=new Date().getTime();
	var phase=(tnow/this.period)*Math.PI*2;
	if ( this.initialised) {
	    //context.setLineDash([1,0]);
	    context.textAlign="left";
	    context.fillStyle="#ff0000";
	    this.canvasWidth = canvas.width;
	    this.canvasHeight = canvas.height;
	    var limits=camera.getSphericalLimits();
	    //console.log("Width=",canvasWidth," height=",canvasHeight," stars=",ll);
	    this.fact=9/(camera.getFovX()*camera.getFovY());// background intensity measure, 3x3 pixels => full color
	    this.cnt=0;
	    limits.nlatmin=this.getILat(limits.latmin);
	    limits.nlatmax=this.getILat(limits.latmax);
	    limits.nlonmin=this.getILon(limits.lonmin);
	    limits.nlonmax=this.getILon(limits.lonmax);
	    limits.nmag = Math.min(this.NMAG,Math.max(this.SMAG+1,this.getIMag(-2.5*this.log10(50/(255*this.fact)))));
	    //console.log("Nmag:",this.NMAG,-2.5*this.log10(10/(255*this.fact)),10/(255*this.fact),limits.nmag);
	    this.tot=0;
	    this.cnt=0;
	    this.mag=0;
	    this.col=255;
	    var pos = new Vector3();
	    var observer=scene.observer;
	    var axis={i:new Vector3(),j:new Vector3(),k:new Vector3()};
	    axis.k.copy(observer.zenith);
	    axis.j.crossVectors(axis.k,observer.k);
	    axis.j.normalize();
	    axis.i.crossVectors(axis.j, axis.k);
	    for (var ilat = limits.nlatmin; ilat <= limits.nlatmax; ilat++) {
		for (var ilon = limits.nlonmin; ilon <= limits.nlonmax; ilon++) {
		    for (var imag = this.SMAG; imag < limits.nmag; imag++) {
			this.list=this.starList[imag][this.modulus(ilat,this.NLAT)][this.modulus(ilon,this.NLON)];
			if (this.list === undefined) {
			    console.log("Missing starlist:",
					imag,
					this.modulus(ilat,this.NLAT),
					this.modulus(ilon,this.NLON));
			}
			this.ll = this.list.length;
			for (var ii = 0; ii < this.ll; ii++) { // LOOP OVER SECTOR STARS
			    this.position=this.list[ii];
			    this.tot=this.tot+1;
			    this.mf=Math.min(this.fact,255/this.position.maxcolor);
//			    var cc=this.position.maxcolor*(0.9 + 0.1*Math.sin(phase+this.position.phase));
//			    this.mf0=Math.min(this.fact,255/cc);
			    camera.vector2Angle(this.position);
			    camera.angle2Screen(this.position);
			    this.iw=Math.floor(this.position.w+0.5);
			    this.ih=Math.floor(this.position.h+0.5);
			    //console.log("Star:",ii,x,y,camera.screen.left,camera.screen.left+camera.screen.width,
			    //	    camera.screen.top,camera.screen.top + camera.screen.height);
			    if (this.iw >= camera.screen.left && this.iw <= camera.screen.left+camera.screen.width  &&
				this.ih >= camera.screen.top  && this.ih <= camera.screen.top + camera.screen.height) {
				this.cnt=this.cnt+1;
				this.mag=Math.max(this.mag,this.position.mag);
				this.col=Math.min(this.col,+this.mf*this.position.maxcolor);
				//this.canvasData = context.getImageData(this.iw, this.ih,1,1);
				// //this.index = (this.iw + this.ih * this.canvasWidth) * 4;
				// this.index=0;
				// this.mc=Math.max(this.position.color[0],this.position.color[1],this.position.color[2]);
				this.r = Math.ceil(Math.min(255,Math.max(0,this.mf*this.position.color[0])));
				this.g = Math.ceil(Math.min(255,Math.max(0,this.mf*this.position.color[1])));
				this.b = Math.ceil(Math.min(255,Math.max(0,this.mf*this.position.color[2])));
				this.a = 255;
				
				var dd=camera.getDiameter(this.position.mag);
				//var bb=Math.sqrt(Math.max(1,this.fact/this.mf));
				//console.log("Magnitude:",dd,bb);
				var col=this.rgbToHex(this.r,this.g,this.b);
				//console.log("Adding star:.",col,this.r,this.g,this.b,this.position.maxcolor*this.mf, this.position.color[0],this.position.color[1],this.position.color[2],"mf:",this.mf,this.fact);
				
				camera.drawAura(context,this.iw,this.ih,dd,col);
				this.drawText(context,this.iw,this.ih,dd,pos,axis);
			    }
			}
		    }
		}
	    }
	    //if (this.col < 2) {
		//console.log("Drew stars:",this.cnt," of ",this.tot," max(mag):",this.mag,limits.nmag," col:",this.col);
	    //}
	    // for (var ii = 0; ii < -ll; ii++) {
	    // 	this.position=this.starList[ii];
	    // 	camera.vector2Angle(position);
	    // 	camera.angle2Screen(position);
	    // 	var x=Math.floor(position.w+0.5);
	    // 	var y=Math.floor(position.h+0.5);
	    // 	if (x >= camera.screen.left && x <= camera.screen.left+ camera.screen.width  &&
	    // 	    y >= camera.screen.top  && y <= camera.screen.top + camera.screen.height) {
	    // 	    context.beginPath();
	    // 	    context.setLineDash([]);
	    // 	    context.arc(x,y,10, 0, 2*Math.PI);
	    // 	    context.stroke();
	    // 	};
	    // }
	};
    },
    drawText : function(context,ww,hh,dd,pos,axis){
	if (dd>2) {
	    context.fillStyle="#ff0000";
	    var posy=hh;
	    if (this.position.name) {
		context.font="30px Arial";
		context.fillText(" "+this.position.name,ww,posy);
	    };
	    posy=posy+20;
	    context.font="16px Arial";
	    if (dd > 10 ){
		if (this.position.lab) {
		    context.fillText("   "+this.position.lab,ww,posy);
		    posy+=20;
		}
		if (this.position.con) {
		    if (this.position.brt) {
			context.fillText("   "+this.getConstellation(this.position.con)+" ("
					 +this.getBrightness(this.position.brt) + this.position.lev+")",ww,posy);
			posy+=20;
		    } else {
			context.fillText("   "+this.getConstellation(this.position.con),ww,posy);
			posy+=20;
		    }
		}
		if (this.position.cls) {
		    context.fillText("   "+this.getClass(this.position.cls),ww,posy);
		    posy+=20;
		}
		posy+=5;
	    };
	    if (dd > 50  && scene.defined){
		pos.subVectors(this.position,camera.position);
		pos.cartesian2Spherical(axis);
		var dist=pos.length();
		var dparsec=this.numberWithCommas(Math.round(dist/this.parsec));
		var dlightyear=this.numberWithCommas(Math.round(dist/this.lightyear));
		context.fillText("   dis="+dparsec+" parsec ("+dlightyear+" ly)",ww,posy);
		posy+=20;
		context.fillText("   azi="+parseFloat((360.0-(pos.lon*180.0/Math.PI))%360.0).toFixed(3)+" deg",ww,posy);
		posy+=20;
		context.fillText("   ele="+parseFloat(pos.lat*180.0/Math.PI).toFixed(3)+" deg",ww,posy);
		posy+=20;
	    };
	    if (dd > 50){
		context.font="16px Arial";
		context.fillText("   ra ="+parseFloat(this.position.ra).toFixed(2)+" hrs",ww,posy);
		posy+=20;
		context.fillText("   dec="+parseFloat(this.position.dec).toFixed(2)+" deg",ww,posy);
		posy+=20;
	    };
	}
    },
    drawConstellations : function(canvas,context,scene,camera){
	var alpha=0;
	var tnow=new Date().getTime();
	var ftime=Math.max(0,Math.min(1.0,(tnow-Chain.consTime)/1000));
	if (Chain.consReq == 0) {
	    if (ftime == 1.0) { return;}
	    alpha=1-ftime;
	} else {
	    alpha=ftime;
	}
	var fact=10/(camera.getFovX()*camera.getFovY());
	var dd=Math.sqrt(Math.max(1,fact));
	if ( this.initialised) {
	    context.strokeStyle="#ff0000";
	    context.fillStyle="#ff0000";
	    context.font="60px Arial";
	    context.textAlign="center";
	    context.globalAlpha=alpha;
	    // determine "closest" constellation
	    var iimin=this.getCurrentConstellation(camera);
	    // plot "closest" constellation
	    var fnew=Math.max(0,Math.min(1.0,(tnow-Chain.lastTime)/1000));
	    if (iimin == Chain.lastCon) {
		Chain.lastTime=tnow;
		fnew=1.0;
	    } else if (fnew >= 1.0) {
		Chain.lastCon=iimin;
		Chain.lastTime=tnow;
		fnew=1.0;
	    };
	    if (fnew < 1.0) {
		context.globalAlpha=alpha*fnew;
		if (iimin >= 0) {
		    this.drawConstellation(canvas,context,scene,camera,iimin,dd);
		};
		context.globalAlpha=alpha*(1.0-fnew);
		if (Chain.lastCon >= 0) {
		    this.drawConstellation(canvas,context,scene,camera,Chain.lastCon,dd);
		};	
		context.globalAlpha=alpha;
	    } else {
		if (iimin >= 0) {
		    this.drawConstellation(canvas,context,scene,camera,iimin,dd);
		};
	    }
	    // draw all constellation names
	    this.drawConstellationNames(canvas,context,scene,camera,alpha,iimin,dd);
	    context.globalAlpha=1.0;
	    context.strokeStyle="#ffffff";
	}
    },
    getCurrentConstellation : function (camera) {
	var iimin=-1;
	var distmin=0;
	var first=true;
	var len = this.constellations.length;
	for (var ii = 0; ii < len; ii++) {
	    var cons=this.constellations[ii];
	    var cpos=cons[0];
	    camera.vector2Angle(cpos);
	    camera.angle2Screen(cpos);
	    if (cpos.k !== undefined) {
		var dist=(cpos.i*cpos.i+cpos.j*cpos.j);
		if (first || dist < distmin) {
		    first=false;
		    iimin=ii;
		    distmin=dist;
		};
	    }
	};
	return iimin;
    },    
    drawConstellation : function (canvas,context,scene,camera,iimin,dd) {
	var cons=this.constellations[iimin];
	var cpos=cons[0];
	var mg=Math.max(0,cons[2]);
	var con=cons[3];
	var strokes=cons[1];
	var lens=strokes.length;
	var font="30px Arial";
	for (var jj = 0; jj < lens; jj++) {
	    this.stroke=strokes[jj];
	    this.lenx=this.stroke.length;
	    context.lineWidth=5;
	    this.first=true;
	    for (var pp = 0; pp < this.lenx; pp++) {
		this.pos=this.stroke[pp];
		camera.vector2Angle(this.pos);
		camera.angle2Screen(this.pos);
		this.v= camera.visible(this.pos);
		if (this.first) {
		    this.first=false;
		} else  {
		    context.beginPath();
		    context.moveTo(this.ow,this.oh);
		    context.lineTo(this.pos.w,this.pos.h);
		    context.stroke();
		};
		if (this.v) {
		    if (dd > 9 && dd < 50 && this.pos.brt && this.pos.brt != "ID") {
			context.font=font;
			context.textAlign="right";
			if (this.pos.lev) {
			    context.fillText(this.getBrightness(this.pos.brt) + this.pos.lev,this.pos.w-10,this.pos.h);
			} else {
			    context.fillText(this.getBrightness(this.pos.brt),this.pos.w-10,this.pos.h);
			}
		    }
		};
		this.ow=this.pos.w;
		this.oh=this.pos.h;
	    };
	    context.lineWidth=1;
	}
    },
    drawConstellationNames : function (canvas,context,scene,camera,alpha,iimin,dd) {
	var len = this.constellations.length;
	//var px=Math.max(20,Math.min(60,Math.floor((camera.screen.width/5)*(0.1/camera.getFovX()))));
	var nfont="48px Arial";
	var dfont="30px Arial";
	//console.log("font n:",nfont," d:",dfont);
	for (var ii = 0; ii < len; ii++) {
	    var cons=this.constellations[ii];
	    var cpos=cons[0];
	    var mg=Math.max(0,cons[2]);
	    var con=cons[3]
	    var name=cons[4];
	    var descr=cons[5];
	    this.iw=Math.floor(cpos.w+0.5);
	    this.ih=Math.floor(cpos.h+0.5);
	    if (cpos.k !== undefined) {
		if (this.iw >= camera.screen.left && this.iw <= camera.screen.left+camera.screen.width  &&
		    this.ih >= camera.screen.top  && this.ih <= camera.screen.top + camera.screen.height) {
		    context.font=nfont;
		    context.textAlign="center";
		    // print name
		    //context.strokeStyle="#000033";
		    if (ii == iimin) {
			context.globalAlpha=Math.min(1,alpha+0.1);
			context.fillText(name,this.iw,this.ih);
			context.globalAlpha=alpha;
		    } else {
			context.globalAlpha=Math.max(0.1,alpha-0.3);
			context.fillText(name,this.iw,this.ih);
			context.globalAlpha=alpha;
		    }
		    if (dd>7) {
			context.font=dfont;
			context.fillText("("+descr+")",this.iw,this.ih+30);
			context.font="60px Arial";
		    }
		}
	    }
	}
    },
    setSectorLimits : function ( limits) {
	if (limits.latmin < 0 ) {
	    limits.latstart=Math.round( + 0.5 + (limits.latmin+Math.PI/2)/this.DIST);
	} else {
	    limits.latstart=Math.round( - 0.5 + (limits.latmin+Math.PI/2)/this.DIST);
	};
	if (limits.latmax < 0 ) {
	    limits.latstop=Math.round( + 0.5 + (limits.latmax+Math.PI/2)/this.DIST);
	} else {
	    limits.latstop=Math.round( - 0.5 + (limits.latmax+Math.PI/2)/this.DIST);
	};
	limits.lonstart=Math.floor(limits.lonmin/this.DIST);
	limits.lonstop=Math.floor(limits.lonmax/this.DIST);
    },
    getIMag : function (mag) {
	return Math.min(this.NMAG,Math.max(Math.round(mag),this.SMAG));
    },
    getILat : function (lat) {
	if (lat < 0 ) {
	    return Math.round( + 0.5 + (lat+Math.PI/2)/this.DIST);
	} else {
	    return Math.round( - 0.5 + (lat+Math.PI/2)/this.DIST);
	}
    },
    getILon : function (lon) {
	return Math.min(Math.floor(lon/this.DIST),this.NLON);
    },
    modulus : function (a,n) {
	return a - (n * Math.floor(a/n));
    },
    log10 : function (a) {
	return Math.log(a)/this.ln10;
    },
    getConstellation : function (con,this_) {
	this_=this_||this;
	return this_.descriptions["con"][con]["name"]||con;
    },
    getConstellationD : function (con, this_) {
	this_=this_||this;
	return this_.descriptions["con"][con]["descr"]||con;
    },
    getBrightness : function (brt, this_) {
	this_=this_||this;
	if (this_.descriptions["brt"][brt]) {
	    return String.fromCharCode(this_.descriptions["brt"][brt]["code"]);
	} else {
	    return brt;
	}
    },
    getClass : function (cls, this_) {
	this_=this_||this;
	return this_.descriptions["class"][cls]||cls;
    }
    
};
