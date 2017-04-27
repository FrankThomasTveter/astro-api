console.log("Loading penline.js");

// move pen in

Penline = function (canvas) {
    // stepping parameters
    this.cnt=0;
    this.xcnt=0; // number of points outside canvas
    this.ucnt=0; // number of points undefined (behind camera)
    this.osector; // previous sector
    this.ow; // previous w
    this.oh; // previous h
    // corner stack
    this.lastSector=-1;
    this.lastDir=0;
    this.firstSector=-1;
    this.firstDir=0;
    this.corners=[];
    // auxiliaries
    this.firstValid;
    this.firstPlot;
    this.firstUndef;
    this.firstCall;
    this.pos=new Vector3();
    this.clockwise=1;
    this.anticlockwise=-1;
    this.bdeb=false;
};

Penline.prototype = {
    constructor : Penline,
    debugOn : function() { this.bdeb=true;},
    debugOff : function() { this.bdeb=false;},
    open : function (context, camera) {
	this.context=context;
	this.camera=camera;
	this.cnt=0;
	this.xcnt=0;
	this.ucnt=0;
	this.clearCorners();
	this.firstValid=true;
	this.firstPlot=true;
	this.firstUndef=false;
	this.firstCall=true;
	if(this.bdeb){console.log("Init");};
    },
    addPoint : function ( pos, dir) {
	var ret=false;
	if (pos.k !== undefined ) { // position on plate
	    var w=pos.w;
	    var h=pos.h;
	    if(this.bdeb){console.log("   >> Raw in:  ",this.camera.getSectorPos(w,h),w,h);};
	    // determine if point is inside canvas
	    var sector=this.camera.getSector(pos);
	    //console.log("   == sector:",sector,pos.w,pos.h);
	    if (this.ucnt > 0) { // exited undefined area
		this.pushSector(sector);
		this.plotCorners();
		this.clearCorners();
		this.ucnt=0;
		if (sector!=0) {
		    this.osector=sector;
		    this.xcnt=1;
		} else {
		    this.xcnt=0;
		}
	    };
	    if (sector == 0) { // inside canvas
		ret=true;
		// check outside canvas count
		if (this.xcnt > 1) { // plot previous point
		    //this.plotCorners();
		    this.plot(this.ow,this.oh);
		};
		if (this.xcnt > 0) {
		    //this.clearCorners();
		    this.xcnt=0;
		};
		this.plot(pos.w,pos.h);
	    } else {
		this.xcnt=this.xcnt+1;
		if ( this.xcnt == 1 ) {
		    this.plot(pos.w,pos.h);
		} else if (this.osector != sector) {
		    //console.log("        Sector change:",this.osector,sector);
		    this.plot(this.ow,this.oh);
		    this.plot(pos.w,pos.h);
		};
		//console.log("Pushing A:",sector,dir);
		//this.pushSector(sector);
	    }
	    this.osector=sector;
	    this.ow=pos.w;
	    this.oh=pos.h;
	    if (this.firstValid) {
		this.firstSector=sector;
		this.firstDir=dir;
		this.firstValid=false;
	    };
	} else {
	    this.ucnt=this.ucnt+1;
	    if (this.firstCall) {
		this.firstUndef=true;
	    } else if (this.ucnt == 1 && this.osector > 0) {
		this.pushSector(this.osector);
		this.pushDir(dir);
	    }
	    //console.log("   == ******:",this.ucnt);
	};
	if (this.firstCall) {
	    this.firstCall=false;
	};
	return ret;
    },
    fill : function () {
	if (!this.firstValid) {
	    if (this.ucnt == 0) {
		this.firstUndef=true;
		this.ucnt=this.ucnt+1;
		this.pushSector(this.osector);
	    };
	};
    },
    close : function () {
	//console.log("Closing:",this.xcnt);
	if (this.firstUndef && ! this.firstValid) {// undefined ending point
	    this.pushDir(this.firstDir);
	    this.pushSector(this.firstSector);
	};
	//console.log("Close:",this.firstUndef,this.firstValid,this.firstPlot,this.corners);
	if (this.xcnt > 1 ) { // last point is outside canvas, empty corner stack
	    this.plotCorners();
	};
	if (this.xcnt > 0 ) {
	    this.clearCorners();
	    this.xcnt=0;
	};
	if (this.ucnt > 0 ) {
	    this.ucnt=0;
	};
	if (!this.firstPlot) {
	    this.context.closePath();
	};
	return ! this.firstPlot;
    },
    clearCorners : function () {
	this.corners=[];
	this.lastSector=-1;
	this.lastDir=0;
    },
    pushSector : function (sector) {
	if (this.lastSector != sector) {
	    this.corners.push({sector:sector});
	};
	this.lastSector=sector;
	this.lastDir=0;
    },
    pushDir          : function (dir) {
	if (this.lastDir != dir) {
	    this.corners.push({dir:dir});
	};
	this.lastSector=0;
	this.lastDir=dir;
    },
    plotCorners : function () {
	//console.log(">>>plot:",this.corners);
	var first= true;
	for (var ii=0;ii<this.corners.length;ii++) {
	    var corner=this.corners[ii];
	    if (corner.sector !== undefined) {
		var ps=corner.sector;
		if (this.camera.setCorner(ps,this.pos)) {
		    this.plot(this.pos.w,this.pos.h);
		    //console.log("   -- Corner:",ps);
		};
	    } else if (ii >0 && ii+1 < this.corners.length) {
		var dir=corner.dir;
		var ps=this.corners[ii-1].sector;
		var ns=this.corners[ii+1].sector;
		if (dir == this.clockwise) { // clockwise
		    if (ns < ps ) {ns=ns+8;}
		    for (var jj=ps+1;jj<ns;jj++) {
			var sector=((jj-1)%8)+1;
			if (this.camera.setCorner(sector,this.pos)) {
			    this.plot(this.pos.w,this.pos.h);
			    //console.log("   -- corner:",sector);
			}
		    };
		} else if (dir ==this.anticlockwise) {// anti-clockwise
		    if (ns > ps ) {ps=ps+8;}
		    for (var jj=ps-1;jj>ns;jj--) {
			var sector=((jj-1)%8)+1;
			if (this.camera.setCorner(sector,this.pos)) {
			    this.plot(this.pos.w,this.pos.h);
			    //console.log("   -- corner:",sector);
			}
		    };
		}
	    }
	}
    },
    plotCornerLoop : function (os,ns,dir) {
    },
    plot : function (w,h) {
	if (this.firstPlot) {
	    this.context.beginPath();
	    this.context.moveTo(w,h);
	    this.firstPlot=false;
	    if(this.bdeb){console.log("   ++ Plotting:",this.camera.getSectorPos(w,h),w,h);};
	} else {
	    this.context.lineTo(w,h);
	    if(this.bdeb){console.log("   -- plotting:",this.camera.getSectorPos(w,h),w,h);};
	};
	this.cnt=this.cnt+1;
    }
}