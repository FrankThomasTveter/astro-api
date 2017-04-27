console.log("Loading request.js");

Request={ 
    launch: function (lat, lon, dtg, label, target) {
	var newRequest=requests.current + 1;
	requests[newRequest]={ location : {latitude : lat,
					   longitude : lon,
					   height : 0.0},
			       play : { event : 0 } ,
			       events : [{reqId : 1,
					  label: label,
					  pointAt : target,
					  viewAngle : 25.0,
					  dtg : dtg }],
			       current : 0
			     };
	requests.current=newRequest;
    },
    update : function () {
	var ret=false;
	function urlrequest() {
	    this.clean_ = function () {var obj=Object.keys(this);for (var ii=0; ii<obj.length;ii++) 
				       {if (obj[ii].match(/_$/g)) {delete this[obj[ii]];}}}
	    this.addLat_ = function(val) {this["lat"]=val;};
	    this.addLon_ = function(val) {this["lon"]=val;};
	    this.addHgt_ = function(val) {this["hgt"]=val;};
	    this.addDtg_ = function(val) {this["dtg"]=val;};
	}
	var reqId=requests.current;
	var req = requests[reqId]
	if ( ! req.sent) { // we have a new target
	    console.log("Sending new request.");
	    // send server request for data
	    var urlreq=new urlrequest();
	    urlreq.addLat_(req.location.latitude);
	    urlreq.addLon_(req.location.longitude);
	    urlreq.addHgt_(req.location.height);
	    var dtgs = [];
	    for ( var tt = 0; tt < req["events"].length; tt++) {
		dtgs.push(req["events"][tt]["dtg"]);
	    };
	    urlreq.addDtg_(dtgs);
	    urlreq.clean_();
	    req.sent= true;
	    //console.log("Sending request :",reqId,urlreq);
	    $.get("/cgi-bin/dump.pl",urlreq,function(data, status){Request.received(data,status,reqId);});
	}
	return ret;
    },
    received : function (data,status,reqId) {
	//console.log("Received response '"+status+"' for request '",reqId,"'");
	if (status == "success") {
	    var req=requests[reqId];
	    // update info
	    var info=document.getElementById("info");
	    //console.log("Request:",req);
	    var lat=parseFloat(req.location.latitude).toFixed(2);
	    var lon=parseFloat(req.location.longitude).toFixed(2);
	    info.innerHTML = "Date:"+req.events[req.current].dtg+" Latitude:"+lat+" Longitude:"+lon;
	    // update data
	    var d=new Date();
	    var tnow=d.getTime();
	    var ss=data.getElementsByTagName("solarsystem")[0];
	    var loc=ss.getElementsByTagName("location")[0];
	    // store initial data
	    var ini=ss.getElementsByTagName("initial")[0];
	    var inibod=ini.getElementsByTagName("body");
	    var inino=ini.getAttribute("no");
	    req["initial"]={};
	    for (var ii=0; ii<inino; ii++) {
		var bod=inibod[ii];
		var name=bod.getAttribute("name");
		req["initial"][name]={};
		req["initial"][name]["rotation"]={};
		req["initial"][name]["rotation"]["ra"]=+bod.getAttribute("ra");
		req["initial"][name]["rotation"]["dec"]=+bod.getAttribute("dec");
		req["initial"][name]["rotation"]["w"]=+bod.getAttribute("w")*Math.PI/180.0;
		req["initial"][name]["rotation"]["dwdt"]=+bod.getAttribute("dwdt")*Math.PI/180.0;
		req["initial"][name]["main"]=bod.getAttribute("main");
		req["initial"][name]["xmu"]=+bod.getAttribute("xmu");
	    }
	    // store time data
	    var tis=ss.getElementsByTagName("times")[0];
	    var tistim=tis.getElementsByTagName("time");
	    var tisno=tis.getAttribute("no");
	    //console.log("Times ",tisno);
	    for (var tt=0; tt<tisno; tt++) {
		var tim = tistim[tt];
		// store observer data
		var dtg  = tim.getAttribute("dtg");
		var dtg_ = req["events"][tt]["dtg"];
		if (dtg != dtg_) { // check if dtg in reply matches dtg in request
		    console.log("Date mismatch: ",dtg," != ",dtg_);
		} else {
		    var jd2000=tim.getAttribute("jd2000");
		    var obs=tim.getElementsByTagName("observer")[0];
		    var obsi=obs.getElementsByTagName("i")[0];
		    var obsj=obs.getElementsByTagName("j")[0];
		    var obsk=obs.getElementsByTagName("k")[0];
		    var obsloc=obs.getElementsByTagName("location")[0];
		    req["events"][tt]["observer"]={};
		    req["events"][tt]["observer"]["i"]={};
		    req["events"][tt]["observer"]["i"]["x"]=+obsi.getAttribute("x");
		    req["events"][tt]["observer"]["i"]["y"]=+obsi.getAttribute("y");
		    req["events"][tt]["observer"]["i"]["z"]=+obsi.getAttribute("z");
		    req["events"][tt]["observer"]["j"]={};
		    req["events"][tt]["observer"]["j"]["x"]=+obsj.getAttribute("x");
		    req["events"][tt]["observer"]["j"]["y"]=+obsj.getAttribute("y");
		    req["events"][tt]["observer"]["j"]["z"]=+obsj.getAttribute("z");
		    req["events"][tt]["observer"]["k"]={};
		    req["events"][tt]["observer"]["k"]["x"]=+obsk.getAttribute("x");
		    req["events"][tt]["observer"]["k"]["y"]=+obsk.getAttribute("y");
		    req["events"][tt]["observer"]["k"]["z"]=+obsk.getAttribute("z");
		    req["events"][tt]["observer"]["loc"]={};
		    req["events"][tt]["observer"]["loc"]["x"]=+obsloc.getAttribute("x");
		    req["events"][tt]["observer"]["loc"]["y"]=+obsloc.getAttribute("y");
		    req["events"][tt]["observer"]["loc"]["z"]=+obsloc.getAttribute("z");
		    req["events"][tt]["observer"]["loc"]["origo"]=obsloc.getAttribute("origo");
		    var pointAt = req["events"][tt]["pointAt"];
		    req["events"][tt]["pointId"] = -1;
		    var sta=tim.getElementsByTagName("state")[0];
		    var stabod=sta.getElementsByTagName("body");
		    var stano=sta.getAttribute("no");
		    req["events"][tt]["state"]={};
		    for (var ii=0; ii<stano; ii++) {
			var bod=stabod[ii];
			var name=bod.getAttribute("name");
			if (name == pointAt) {req["events"][tt]["pointId"] = ii;}
			//console.log("Added state for ",name," at ",dtg);
			req["events"][tt]["state"][name]={};
			req["events"][tt]["state"][name]["x"]=+bod.getAttribute("x");
			req["events"][tt]["state"][name]["y"]=+bod.getAttribute("y");
			req["events"][tt]["state"][name]["z"]=+bod.getAttribute("z");
			req["events"][tt]["state"][name]["vx"]=+bod.getAttribute("vx");
			req["events"][tt]["state"][name]["vy"]=+bod.getAttribute("vy");
			req["events"][tt]["state"][name]["vz"]=+bod.getAttribute("vz");
			req["events"][tt]["state"][name]["ra"]=+req["initial"][name]["rotation"]["ra"]; 
			req["events"][tt]["state"][name]["dec"]=+req["initial"][name]["rotation"]["dec"]; 
			req["events"][tt]["state"][name]["w"]=+req["initial"][name]["rotation"]["w"]
			    + req["initial"][name]["rotation"]["dwdt"] * jd2000;
			req["events"][tt]["state"][name]["name"]=name;
		    }
		}
	    }
	    //console.log("Requests:",JSON.stringify(requests));
	    req.received= true;
	} else {
	    //console.log("Request failed.");
	}
    }
};

