console.log("Loading model.js");

// maintains the model state
Model ={redraw:false};

// update 3D model of the solar system...
Model.update = function () {
    var ret = false;
    var reqId=requests.current;
    var req = requests[reqId]
    if (req.elements & model.reqId != reqId ) { // process new request
	console.log("Constructing model from orbital elements.");
	model.reqId=reqId;
	// get times
	model.epochs = [];
	for ( var tt = 0; tt < req["events"].length; tt++) {
	    model.epochs[tt]=req["events"][tt]["epoch"];
	};
	model.play={};
	if (req.play.event != undefined) {model.play.event=req.play.event;}
	if (req.play.speed != undefined) {model.play.speed=req.play.speed;}
	if (req.play.e0 != undefined) {model.play.e0=req.play.e0;}
	if (req.play.m0 != undefined) {model.play.m0=req.play.m0;}
	model.old=model.current;
	model.current=(model.current+1)%2;
	Orbit.getState(req,model[model.current]);
	if (model[model.old]["bodies"] != undefined) {
	    // "tween" model from oldState to newState
	}
	// delete old requests
	for (var key in requests) {
	    if (requests.hasOwnProperty(key)) {
		if (key != model.old & requests[key]["events"] != undefined) {
		}
	    }
	}
	req.orbit = false;
	Model.redraw=true;
    } else if (req.elements&model.play.event==undefined&model.play.speed > 0.0) { // time lapse...
	console.log("Re-calculating orbit state :",model.play.speed);
	model.old=model.current;
	model.current=(model.current+1)%2;
	Orbit.getState(req, model[model.current]);
	Model.redraw=true;
    }
    return ret;
}


Model.draw = function () {



}
