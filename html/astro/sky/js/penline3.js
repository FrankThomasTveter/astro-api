console.log("Loading penline.js");

// move pen in

Penline = function (canvas) {
    // stepping parameters
    this.cnt=0;
    this.xcnt=0; // number of points outside canvas
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
};

Penline.prototype = {
    constructor : Penline,
    open : function (context, camera) {
	this.context=context;
	this.camera=camera;
	this.cnt=0;
	this.xcnt=0;
	this.clearCorners();
	this.firstValid=true;
	this.firstPlot=true;
	this.firstUndef=false;
	this.firstCall=true;
    },
    addPoint : function ( pos, dir) {
	var ret=false;
	if (pos.k !== undefined ) { // position on plate
	    // determine if point is inside canvas
	    var sector=this.camera.getSector(pos);
	    ret=(sector==0);
	    // check outside canvas count
	    if (this.xcnt > 0) { // plot previous point
		this.pushSector(sector);
		this.plotCorners();
	    };
	    if (this.xcnt > 0) {
		this.clearCorners();
		this.xcnt=0;
	    };
	    this.plot(pos.w,pos.h);
	    this.osector=sector;
	    this.ow=pos.w;
	    this.oh=pos.h;
	    if (this.firstValid) {
		this.firstSector=sector;
		this.firstValid=false;
	    };
	} else {
	    if (this.firstCall) {
		this.firstUndef=true;
	    }
	    this.xcnt=this.xcnt+1;
	    if (!this.firstValid && this.xcnt == 1) {
		//console.log("Pushing A:",sector,dir);
		this.pushSector(this.osector);
		this.pushDir(dir);
	    }
	};
	if (this.firstCall) {
	    this.firstCall=false;
	};
	return ret;
    },
    close : function () {

	if (this.firstUndef && ! this.firstValid) {// undefined ending point
	    this.pushSector(this.firstSector);
	};

	console.log("Closing:",this.xcnt,this.corners);

	if (this.xcnt > 0 ) { // last point is outside canvas, empty corner stack
	    this.plotCorners();
	    this.clearCorners();
	    this.xcnt=0;
	};
	if (!this.firstPlot) {
	    this.context.closePath();
	};
	return ! this.firstPlot;
    },
    clearCorners : function () {
	this.corners=[];
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

	console.log(">>> plotC:",this.xcnt, this.corners);

	var first= true;
	for (var ii=0;ii<this.corners.length;ii++) {
	    var corner=this.corners[ii];
	    if (corner.sector !== undefined) {
		var ps=corner.sector;
		if (this.camera.setCorner(ps,this.pos)) {
		    this.plot(this.pos.w,this.pos.h);
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
			}
		    };
		} else if (dir ==this.anticlockwise) {// anti-clockwise
		    if (ns > ps ) {ps=ps+8;}
		    for (var jj=ps-1;jj>ns;jj--) {
			var sector=((jj-1)%8)+1;
			if (this.camera.setCorner(sector,this.pos)) {
			    this.plot(this.pos.w,this.pos.h);
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
	} else {
	    this.context.lineTo(w,h);
	};
	this.cnt=this.cnt+1;
    }
}
