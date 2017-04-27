console.log("Loading scene.js");

Scene = function () {
    this.items=[];
    this.defined=false;
};

Scene.prototype = {
    constructor : Scene,
    add : function (object) {
	this.items.push(object);
    },
    addDistance : function (pos) {
 	var itemLen=this.items.length;
	for(var ii = 0; ii < itemLen; ii++){
	    var item=this.items[ii];
	    if (item.position !== undefined) {
		item.distance = item.position.distanceTo(pos);
	    }
	}
    },
    sort : function (pos) {
	this.addDistance(pos);
	this.items.sort(function(a,b){return b.distance-a.distance;});
    }
}
