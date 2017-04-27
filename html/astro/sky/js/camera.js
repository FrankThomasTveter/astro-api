console.log("Loading camera.js");

Camera = function (pos,fov,axis) {
    // screen size
    this.screen = { left: 0, top: 0, width: 1, height: 1 };
    // camera position
    this.position = pos || new Vector3(0,0,0);
    // field of view in radians
    this.fovmin=0.0001;
    this.fovx = Math.max(this.fovmin,fov) ||  Math.PI*0.5; // 45 degree FOV
    this.fovy = Math.max(this.fovmin,fov) ||  Math.PI*0.5; // 45 degree FOV
    // camera directional axis, i=horisontal(right), j=vertial(down), k=into camera... 
    this.axis=axis || {i: new Vector3(1,0,0),j: new Vector3(0,1,0),k: new Vector3(0,0,1)};
    this.newRef;
    this.oldRef;
    this.vector1 = new Vector3();
    this.axis1 = {i: new Vector3(1,0,0),j: new Vector3(0,1,0),k: new Vector3(0,0,1)};
    this.axis2 = {i: new Vector3(1,0,0),j: new Vector3(0,1,0),k: new Vector3(0,0,1)};
    this.dd=20;
};

Camera.prototype={
    constructor : Camera,
    setPosition : function (position) {
	this.position.x=position.x;
	this.position.y=position.y;
	this.position.z=position.z;
    },
    getAxis : function () {
	return this.axis;
    },
    setAxis : function (axis) {
	this.axis.i.x=+axis.i.x;
	this.axis.i.y=+axis.i.y;
	this.axis.i.z=+axis.i.z;
	this.axis.j.x=+axis.j.x;
	this.axis.j.y=+axis.j.y;
	this.axis.j.z=+axis.j.z;
	this.axis.k.x=+axis.k.x;
	this.axis.k.y=+axis.k.y;
	this.axis.k.z=+axis.k.z;
    }, 
    setI : function (i) {
	this.axis.i.x=i.x;
	this.axis.i.y=i.y;
	this.axis.i.z=i.z;
    },
    setJ : function (j) {
	this.axis.j.x=j.x;
	this.axis.j.y=j.y;
	this.axis.j.z=j.z;
    },
   setK : function (k) {
	this.axis.k.x=k.x;
	this.axis.k.y=k.y;
	this.axis.k.z=k.z;
    },
    setScreen : function (left,top,width,height) {
	this.screen.left = left;
	this.screen.top = top;
	this.screen.width = Math.max(1,width);
	this.screen.height = Math.max(1,height);
	this.fovy=this.fovx*this.screen.height/this.screen.width;
    },
    getDir : function () {
	return this.axis.k;
    },
    getFovX : function () {
	return this.fovx;
    },
    getFovY : function () {
	return this.fovy;
    },
    setFovX : function (fov) {
	this.fovx=Math.min(Math.PI*0.5,Math.max(this.fovmin,fov));
	this.fovy=this.fovx*this.screen.height/this.screen.width;
    },
    normalise : function (axis) {
	this.check(axis);
	axis.k.normalize();
	axis.i.crossVectors(axis.j,axis.k).normalize();
	axis.j.crossVectors(axis.k, axis.i);
	this.check(axis);
    },
    check : function (axis) {
	console.log("Dot products:",
		    Math.round(axis.i.dot(axis.j)*1000)/1000,
		    Math.round(axis.i.dot(axis.k)*1000)/1000,
		    Math.round(axis.j.dot(axis.k)*1000)/1000,
		    Math.round(axis.i.length()*1000)/1000,
		    Math.round( axis.j.length()*1000)/1000,
		    Math.round(axis.k.length()*1000)/1000);
    },
    visible : function (pos) {
	return (pos.w > this.screen.left && pos.w < this.screen.left+this.screen.width &&  
		pos.h > this.screen.top  && pos.h < this.screen.top+this.screen.height);
    },
    getSector : function (pos) {
	if (pos.w < this.screen.left) {
	    if (pos.h < this.screen.top) {
		return 1;
	    } else if (pos.h > this.screen.top+this.screen.height) {
		return 7;
	    } else {
		return 8;
	    }
	} else if (pos.w > this.screen.left+this.screen.width) {
	    if (pos.h < this.screen.top) {
		return 3;
	    } else if (pos.h > this.screen.top+this.screen.height) {
		return 5;
	    } else {
		return 4;
	    }
	} else {
	    if (pos.h < this.screen.top) {
		return 2;
	    } else if (pos.h > this.screen.top+this.screen.height) {
		return 6;
	    } else {
		return 0;
	    }
	};
    },
    getSectorPos : function (w,h) {
	if (w < this.screen.left) {
	    if (h < this.screen.top) {
		return 1;
	    } else if (h > this.screen.top+this.screen.height) {
		return 7;
	    } else {
		return 8;
	    }
	} else if (w > this.screen.left+this.screen.width) {
	    if (h < this.screen.top) {
		return 3;
	    } else if (h > this.screen.top+this.screen.height) {
		return 5;
	    } else {
		return 4;
	    }
	} else {
	    if (h < this.screen.top) {
		return 2;
	    } else if (h > this.screen.top+this.screen.height) {
		return 6;
	    } else {
		return 0;
	    }
	};
    },
    isCorner : function (sector) {
	if (sector == 1) {
	    return true;
	} else if (sector == 3) {
	    return true;
	} else if (sector == 5) {
	    return true;
	} else if (sector == 7) {
	    return true;
	} else {
	    return true;
	};
    },
    setCorner : function (sector,pos) {
	if (sector == 1) {
	    pos.w=this.screen.left;
	    pos.h=this.screen.top;
	    return true;
	} else if (sector == 3) {
	    pos.w=this.screen.left+this.screen.width;
	    pos.h=this.screen.top;
	    return true;
	} else if (sector == 5) {
	    pos.w=this.screen.left+this.screen.width;
	    pos.h=this.screen.top+this.screen.height;
	    return true;
	} else if (sector == 7) {
	    pos.w=this.screen.left;
	    pos.h=this.screen.top+this.screen.height;
	    return true;
	} else {
	    return false;
	}
    },
    rotateAxis : function(angle,axis) {
	axis=axis || this.axis;
	//this.check(axis);
	//this.normalise(axis);
	this.rotateAxisK(this.axis1,axis,angle.k);
	this.rotateAxisJ(this.axis2,this.axis1,angle.j);
	this.rotateAxisI(axis,this.axis2,angle.i);
	//console.log("Axis:",axis.i,axis.j,axis.k);
	//this.normalise(axis);
	//console.log("Norm:",axis.i,axis.j,axis.k);
    },
    //around the i-axis would be
    //    |1     0           0| |i|   |        i        |   |im|
    //    |0   cos θ    -sin θ| |j| = |j cos θ - k sin θ| = |jm|
    //    |0   sin θ     cos θ| |k|   |j sin θ + k cos θ|   |km|
    rotateAxisI : function (ret,axis,angle) {
	var c=Math.cos(+angle);
	var s=Math.sin(+angle);
	ret.i.x=(+axis.i.x);
	ret.i.y=(+axis.i.y);
	ret.i.z=(+axis.i.z);
	//
	ret.j.x=(+axis.j.x)*c + (+axis.k.x)*s;
	ret.j.y=(+axis.j.y)*c + (+axis.k.y)*s;
	ret.j.z=(+axis.j.z)*c + (+axis.k.z)*s;
	//
	ret.k.x=-(+axis.j.x)*s + (+axis.k.x)*c ;
	ret.k.y=-(+axis.j.y)*s + (+axis.k.y)*c;
	ret.k.z=-(+axis.j.z)*s + (+axis.k.z)*c;
	return ret;
    },
    //around the j-axis would be
    //    | cos θ    0   sin θ| |i|   | i cos θ + k sin θ|   |im|
    //    |   0      1       0| |j| = |         j        | = |jm|
    //    |-sin θ    0   cos θ| |k|   |-i sin θ + k cos θ|   |km|
    rotateAxisJ : function (ret,axis,angle) {
	var c=Math.cos(+angle);
	var s=Math.sin(+angle);
	ret.i.x=(+axis.i.x)*c + (+axis.k.x)*s;
	ret.i.y=(+axis.i.y)*c + (+axis.k.y)*s;
	ret.i.z=(+axis.i.z)*c + (+axis.k.z)*s;
	//
	ret.j.x=(+axis.j.x);
	ret.j.y=(+axis.j.y);
	ret.j.z=(+axis.j.z);
	//
	ret.k.x=-(+axis.i.x)*s + (+axis.k.x)*c ;
	ret.k.y=-(+axis.i.y)*s + (+axis.k.y)*c;
	ret.k.z=-(+axis.i.z)*s + (+axis.k.z)*c;
	return ret;
    },
    //around the k-axis would be
    //    |cos θ   -sin θ    0| |i|   |i cos θ - j sin θ|   |im|
    //    |sin θ    cos θ    0| |j| = |i sin θ + j cos θ| = |jm|
    //    |  0       0       1| |k|   |        k        |   |km|
    rotateAxisK : function (ret,axis,angle) {
	var c=Math.cos(+angle);
	var s=Math.sin(+angle);
	ret.i.x=(+axis.i.x)*c + (+axis.j.x)*s;
	ret.i.y=(+axis.i.y)*c + (+axis.j.y)*s;
	ret.i.z=(+axis.i.z)*c + (+axis.j.z)*s;
	//
	ret.j.x=-(+axis.i.x)*s + (+axis.j.x)*c;
	ret.j.y=-(+axis.i.y)*s + (+axis.j.y)*c;
	ret.j.z=-(+axis.i.z)*s + (+axis.j.z)*c;
	//
	ret.k.x=(+axis.k.x);
	ret.k.y=(+axis.k.y);
	ret.k.z=(+axis.k.z);
	return ret;
    },
    // The "camera reference coordinate system" is used to make the camera follow an object or a (rotating) coordinate system...
    newReference : function(point, up) { // defined new reference of the camera, point = target/axis, up = up direction
	if (point === undefined) {
	    this.newRef=undefined;
	} else {
	    if (this.newRef===undefined) {this.newRef={i: new Vector3(),j: new Vector3(),k: new Vector3()};};
	    if (up===undefined) {
		this.newRef.i.copy(point.i);
		this.newRef.j.copy(point.j);
		this.newRef.k.copy(point.k);
		//console.log ("Camera Setting axis:",this.newRef.i.x,this.newRef.i.y,this.newRef.i.z);
	    } else {
		if (point.x === undefined) {
		    this.newRef.k.copy(point.k).normalize();
		} else {
		    this.newRef.k.subVectors(point,this.position).normalize();
		};
		this.newRef.i.crossVectors(this.newRef.k,up).normalize();
		this.newRef.j.crossVectors(this.newRef.k, this.newRef.i);
		//console.log ("Camera Calculating k-axis:",this.newRef.k.x,this.newRef.k.y,this.newRef.k.z,point);
		//console.log ("Camera Calculating i-axis:",this.newRef.i.x,this.newRef.i.y,this.newRef.i.z,up);
	    }
	}
    },

    shiftReference : function() { // shift camera from the old to the new reference and replace old reference
	if (this.oldRef !== undefined & this.newRef !== undefined) {
	    var ii= this.axis.i.x * this.oldRef.i.x + this.axis.i.y * this.oldRef.i.y + this.axis.i.z * this.oldRef.i.z;
            var ij= this.axis.i.x * this.oldRef.j.x + this.axis.i.y * this.oldRef.j.y + this.axis.i.z * this.oldRef.j.z;
            var ik= this.axis.i.x * this.oldRef.k.x + this.axis.i.y * this.oldRef.k.y + this.axis.i.z * this.oldRef.k.z;
	    var ji= this.axis.j.x * this.oldRef.i.x + this.axis.j.y * this.oldRef.i.y + this.axis.j.z * this.oldRef.i.z;
            var jj= this.axis.j.x * this.oldRef.j.x + this.axis.j.y * this.oldRef.j.y + this.axis.j.z * this.oldRef.j.z;
            var jk= this.axis.j.x * this.oldRef.k.x + this.axis.j.y * this.oldRef.k.y + this.axis.j.z * this.oldRef.k.z;
	    var ki= this.axis.k.x * this.oldRef.i.x + this.axis.k.y * this.oldRef.i.y + this.axis.k.z * this.oldRef.i.z;
            var kj= this.axis.k.x * this.oldRef.j.x + this.axis.k.y * this.oldRef.j.y + this.axis.k.z * this.oldRef.j.z;
            var kk= this.axis.k.x * this.oldRef.k.x + this.axis.k.y * this.oldRef.k.y + this.axis.k.z * this.oldRef.k.z;
            this.axis.i.x = ii * this.newRef.i.x + ij * this.newRef.j.x + ik * this.newRef.k.x;
            this.axis.i.y = ii * this.newRef.i.y + ij * this.newRef.j.y + ik * this.newRef.k.y;
            this.axis.i.z = ii * this.newRef.i.z + ij * this.newRef.j.z + ik * this.newRef.k.z;
            this.axis.j.x = ji * this.newRef.i.x + jj * this.newRef.j.x + jk * this.newRef.k.x;
            this.axis.j.y = ji * this.newRef.i.y + jj * this.newRef.j.y + jk * this.newRef.k.y;
            this.axis.j.z = ji * this.newRef.i.z + jj * this.newRef.j.z + jk * this.newRef.k.z;
            this.axis.k.x = ki * this.newRef.i.x + kj * this.newRef.j.x + kk * this.newRef.k.x;
            this.axis.k.y = ki * this.newRef.i.y + kj * this.newRef.j.y + kk * this.newRef.k.y;
            this.axis.k.z = ki * this.newRef.i.z + kj * this.newRef.j.z + kk * this.newRef.k.z;
	}
	this.oldReference(this.newRef);
    },
    oldReference : function(reference) { // set the old camera reference
	if (reference === undefined) {
	    this.oldRef=undefined;
	} else {
	    if (this.oldRef === undefined) { 
		this.oldRef = {i: new Vector3(1,0,0),
			       j: new Vector3(0,1,0),
			       k: new Vector3(0,0,1)};
	    };
	    this.oldRef.i.copy(reference.i);
	    this.oldRef.j.copy(reference.j);
	    this.oldRef.k.copy(reference.k);
	}
    },
    angle2Vector : function(angle,axis,position) {
	axis=axis || this.axis;
	position=position || this.position;
	if (angle.k !== undefined) {
	    angle.x=(+axis.i.x)*angle.i +(+axis.j.x)*angle.j+(+axis.k.x);
	    angle.y=(+axis.i.y)*angle.i +(+axis.j.y)*angle.j+(+axis.k.y);
	    angle.z=(+axis.i.z)*angle.i +(+axis.j.z)*angle.j+(+axis.k.z);
	    angle.normalize().multiplyScalar(angle.r);
	} else {
	    angle.x=undefined;
	    angle.y=undefined;
	    angle.z=undefined;
	}
    },
    vector2Angle : function(angle) {
	this.vector1.subVectors(angle,this.position);
	angle.zr=this.vector1.length();
	this.vector1.divideScalar(+angle.zr);
	angle.xp=this.vector1.dot(this.axis.i);
	angle.yp=this.vector1.dot(this.axis.j);
	angle.zp=this.vector1.dot(this.axis.k);
	if (angle.zp > 0) {
	    angle.i=angle.xp/angle.zp;
	    angle.j=angle.yp/angle.zp;
	    angle.k=0.0;
	} else {
	    angle.i=undefined;
	    angle.j=undefined;
	    angle.k=undefined;
	}
    },
    vector2AngleLocal : function(angle) {
	this.vector1.set(angle.x,angle.y,angle.z);
	angle.zr=this.vector1.length();
	this.vector1.divideScalar(+angle.zr);
	angle.xp=this.vector1.dot(this.axis.i);
	angle.yp=this.vector1.dot(this.axis.j);
	angle.zp=this.vector1.dot(this.axis.k);
	if (angle.zp > 0.0) {
	    angle.i=angle.xp/angle.zp;
	    angle.j=angle.yp/angle.zp;
	    angle.k=0.0;
	} else {
	    angle.i=undefined;
	    angle.j=undefined;
	    angle.k=undefined;
	}
    },
    vector2AngleRelativeOld : function(angle) {
	this.vector1.set(angle.x,angle.y,angle.z);
	angle.zr=this.vector1.length();
	this.vector1.divideScalar(+angle.zr);
	angle.xp=this.vector1.dot(this.axis.i);
	angle.yp=this.vector1.dot(this.axis.j);
	angle.zp=this.vector1.dot(this.axis.k);
	if (Math.abs(angle.xp) < 1e-7 && Math.abs(angle.zp)<1e-7) {
	    angle.i=0.0;
	} else {
	    angle.i=Math.atan2(angle.xp,angle.zp);
	}
	if (Math.abs(angle.yp) < 1e-7 && Math.abs(angle.zp)<1e-7) {
	    angle.j=0.0;
	} else {
	    angle.j=Math.atan2(angle.yp,angle.zp);
	}
	angle.k=0;
    },
    angle2Screen : function(angle) {
	if (angle.k !== undefined) {
	    var fov=1e4*(this.fovx*this.fovx+this.fovy*this.fovy);
	    var factor=Math.sqrt(fov/(fov + angle.i*angle.i + angle.j*angle.j));
	    angle.w=this.screen.left + 0.5*(1 + (+angle.i*factor)/(this.fovx))*(this.screen.width);
	    angle.h=this.screen.top + 0.5*(1 + (+angle.j*factor)/(this.fovy))*(this.screen.height);
	} else {
	    angle.w=this.screen.left - this.screen.width;
	    angle.h=this.screen.top - this.screen.height;
	}
    },
    screen2Angle : function(angle) {
	angle.i=((2*(angle.w-this.screen.left)/(this.screen.width))-1)*this.fovx;
	angle.j=((2*(angle.h-this.screen.top)/(this.screen.height))-1)*this.fovy;
    },
    setUp : function (u) { // u = up-direction
	// rotate around k-axis until j-axis points away from u
	if (u !== undefined) {
	    this.axis.i.crossVectors(this.axis.k,u);
	} else {
	    this.axis.i.crossVectors(this.axis.j,this.axis.k);
	}
	this.axis.i.normalize();
	this.axis.j.crossVectors(this.axis.k, this.axis.i);
    },
    pointAtSpherical : function (up) {
	this.axis.k.sperical2cartesian(this.newRef);
	this.setUp(up);
    },
    pointAt : function (point,up) {
	this.axis.k.subVectors(point,this.position);
	this.axis.k.normalize();
	if (up !== undefined) {
	    this.axis.i.crossVectors(this.axis.k,up);
	    if (axis.i.isZero()) {
		axis.i.makeNormal(axis.k);
	    } else {
		axis.i.normalize();
	    }
	    this.axis.j.crossVectors(this.axis.k, this.axis.i);
	} else {
	    if (this.axis.k.dot(this.axis.j) == 0.0) {
		this.axis.j.crossVectors(this.axis.k, this.axis.i);
	    } else {
		this.axis.i.crossVectors(this.axis.j,this.axis.k);
		this.axis.i.normalize();
		this.axis.j.crossVectors(this.axis.k, this.axis.i);
	    }
	}
    },
    pointDir : function (point,up) {
	this.axis.k.copy(point);
	this.axis.k.normalize();
	if (up !== undefined) {
	    this.axis.i.crossVectors(this.axis.k,up);
	    if (axis.i.isZero()) {
		axis.i.makeNormal(axis.k);
	    } else {
		axis.i.normalize();
	    }
	    this.axis.j.crossVectors(this.axis.k, this.axis.i);
	} else {
	    if (this.axis.k.dot(this.axis.j) == 0.0) {
		this.axis.j.crossVectors(this.axis.k, this.axis.i);
	    } else {
		this.axis.i.crossVectors(this.axis.j,this.axis.k);
		this.axis.i.normalize();
		this.axis.j.crossVectors(this.axis.k, this.axis.i);
	    }
	}
    },
    getSphericalLimits : function (axis) { // get the spherical max,min values for the camera view in given axis
	var ret={};
	// camera-k projection into axis
	if (axis === undefined) {
	    var px = this.axis.k.x;
	    var py = this.axis.k.y;
	    var pz = this.axis.k.z;
	} else {
	    var px = this.axis.k.dot(axis.i);
	    var py = this.axis.k.dot(axis.j);
	    var pz = this.axis.k.dot(axis.k);
	}
	// get spherical coordinates if camera-k in axis...
	var lat=Math.asin(Math.min(1.0,Math.max(-1.0,pz)));
	var lon;
	if (Math.abs(py)<1e-7 && Math.abs(px) < 1e-7) {
	    lon=0.0;
	} else {
	    lon=Math.atan2(py,px);
	}
	// get maximum fov angle
	var cfov = 1.0/Math.sqrt(1.0 + this.fovx*this.fovx + this.fovy*this.fovy);
	var fov = Math.acos(cfov);
	// 
	var dlon=0;
	var dlat=0;
	var sfov=Math.sin(fov);
	var saz=Math.cos(lat);
	ret.lon=lon;
	ret.lat=lat;
	ret.fov=fov;
	if (sfov>=saz) { // pole inside fov, all longitudes must be considered...
	    dlon=Math.PI;
	} else { // get maximum longitude following the great circle..
	    dlon=Math.min(Math.PI,Math.asin(sfov/saz));
	}
	ret.lonmin = lon - dlon;
	ret.lonmax = lon + dlon;
	ret.dlon=dlon;
	dlat=fov;
	ret.latmin = Math.max(lat - dlat,-Math.PI*0.5);
	ret.latmax = Math.min(lat + dlat,+Math.PI*0.5);
	ret.dlat=dlat;
	//
	return ret;
    },
    getDiameter : function(mag,fact=1){ // calculate aura diameter from magnitude and FOV
	return Math.sqrt(Math.max(1,fact*Math.pow(10,1-(mag/2.5))/(this.getFovX()*this.getFovY())));
    },
    drawAura : function(context,ww,hh,dd,col){
	context.fillStyle=col;
	context.strokeStyle=col;
	if (dd > this.dd) {
	    dd=Math.max(1,(Math.log(dd/this.dd)+1)*this.dd);
	};
	if (dd < 2) {
	    context.beginPath();
	    context.moveTo(ww,hh);
	    context.lineTo(ww-1,hh);
	    context.lineTo(ww,hh);
	    context.stroke();
	} else {
	    var dw=0
	    var dh=dd
	    this.drawCross(context,ww,hh,dw,dh,col);
	    // if (dd > 3) {
	    // 	dw=dd/3;
	    // 	dh=dw;
	    // 	this.drawCross(context,ww,hh,dw,dh,col);
	    // }
	}
    },
    drawCross : function(context,ww,hh,dw,dh,col){
	context.beginPath();
	context.moveTo(ww,hh);
	context.lineTo(ww-dw,hh-dh);
	context.lineTo(ww+dw,hh+dh);
	context.lineTo(ww,hh);
	context.lineTo(ww-dh,hh+dw);
	context.lineTo(ww+dh,hh-dw);
	context.lineTo(ww,hh);
	context.stroke();
    }
};
