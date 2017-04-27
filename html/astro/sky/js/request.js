console.log("Loading request.js");

Request={ 
    launch: function (lat, lon, dtg, hrs, label, target, fov, dir, con, play) {
	lat=(lat || 51.5);
	lon=(lon || 0.0);
	con=(con || 0);
	hrs=Math.max(0,Math.min(24,(hrs || 1)));
	var dtgdate;
	var m0;
	if (dtg) {
	    dtgdate=new Date(dtg);
	    m0=dtgdate.getTime();
	} else {
	    dtgdate=new Date();
	};
	var dtgs=[];
	dtgs.push(dtgdate.toISOString());
	for ( var tt = 0; tt < hrs; tt++) {
	    dtgs.push(dtgdate.addHours(1.0).toISOString());
	};
	if (dir !== undefined) { // direction in J2000 (the star coordinate system)
	    var items=dir.split(',');
	    if (items.length == 3) {
		dir = new Vector3(items[0],items[1],items[2]);
	    } else {
		console.log("Unable to interpred dir:",dir);
		dir=undefined;
	    }
	}
	var newRequest=requests.current + 1;
	requests[newRequest]={ location : {latitude : lat,
					   longitude : lon,
					   height : 0.0},
			       play : {  } ,
			       events : [],
			       current : 0
			     };
	for ( var tt = 0; tt < dtgs.length; tt++) {
	    requests[newRequest]["events"].push({reqId : tt+1,
						 label: label,
						 target : target,
						 dir : dir,
						 fov : fov,
						 con : con,
						 dtg : dtgs[tt] 
						});
	};
	if (play !== undefined) {
	    requests[newRequest]["play"]["event"] = 0;
	    requests[newRequest]["play"]["speed"] = play;
	    requests[newRequest]["play"]["hrs"] = hrs;
	    requests[newRequest]["play"]["m0"] = m0;
	    
	    console.log("New request:",requests[newRequest]);

	} else {
	    requests[newRequest]["play"]["event"] = 0;
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
	    console.log("Sending state-request :",reqId,urlreq);
	    $.get("cgi-bin/state.pl",urlreq,function(data, status){Request.received(data,status,reqId);});
	}
	return ret;
    },
    setInfo : function (dtg,lat,lon) {
	var info=document.getElementById("info");
	info.innerHTML = dtg+" lat:"+parseFloat(lat).toFixed(2)+" lon:"+parseFloat(lon).toFixed(2);
    },
    received : function (data,status,reqId) {
	//console.log("Received response '"+status+"' for request '",reqId,"'");
	if (status == "success") {
	    var req=requests[reqId];
	    // update info
	    this.setInfo(req.events[req.current].dtg,
			 req.location.latitude,
			 req.location.longitude);
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
		    var obszen=obs.getElementsByTagName("zenith")[0];
		    req["events"][tt]["observer"]={};
		    req["events"][tt]["observer"]["i"]=new Vector3();
		    req["events"][tt]["observer"]["i"]["x"]=+obsi.getAttribute("x");
		    req["events"][tt]["observer"]["i"]["y"]=+obsi.getAttribute("y");
		    req["events"][tt]["observer"]["i"]["z"]=+obsi.getAttribute("z");
		    req["events"][tt]["observer"]["j"]=new Vector3();
		    req["events"][tt]["observer"]["j"]["x"]=+obsj.getAttribute("x");
		    req["events"][tt]["observer"]["j"]["y"]=+obsj.getAttribute("y");
		    req["events"][tt]["observer"]["j"]["z"]=+obsj.getAttribute("z");
		    req["events"][tt]["observer"]["k"]=new Vector3();
		    req["events"][tt]["observer"]["k"]["x"]=+obsk.getAttribute("x");
		    req["events"][tt]["observer"]["k"]["y"]=+obsk.getAttribute("y");
		    req["events"][tt]["observer"]["k"]["z"]=+obsk.getAttribute("z");
		    req["events"][tt]["observer"]["position"]=new Vector3();
		    req["events"][tt]["observer"]["position"]["x"]=+obsloc.getAttribute("x");
		    req["events"][tt]["observer"]["position"]["y"]=+obsloc.getAttribute("y");
		    req["events"][tt]["observer"]["position"]["z"]=+obsloc.getAttribute("z");
		    req["events"][tt]["observer"]["position"]["origo"]=obsloc.getAttribute("origo");
		    req["events"][tt]["observer"]["zenith"]=new Vector3();
		    req["events"][tt]["observer"]["zenith"]["x"]=+obszen.getAttribute("x");
		    req["events"][tt]["observer"]["zenith"]["y"]=+obszen.getAttribute("y");
		    req["events"][tt]["observer"]["zenith"]["z"]=+obszen.getAttribute("z");
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
			req["events"][tt]["state"][name]={"position":new Vector3(),"rotation":new Vector3()};
			req["events"][tt]["state"][name]["position"]["x"]=+bod.getAttribute("x");
			req["events"][tt]["state"][name]["position"]["y"]=+bod.getAttribute("y");
			req["events"][tt]["state"][name]["position"]["z"]=+bod.getAttribute("z");
			req["events"][tt]["state"][name]["position"]["vx"]=+bod.getAttribute("vx");
			req["events"][tt]["state"][name]["position"]["vy"]=+bod.getAttribute("vy");
			req["events"][tt]["state"][name]["position"]["vz"]=+bod.getAttribute("vz");
			req["events"][tt]["state"][name]["rotation"]["ra"]=+req["initial"][name]["rotation"]["ra"]; 
			req["events"][tt]["state"][name]["rotation"]["dec"]=+req["initial"][name]["rotation"]["dec"]; 
			req["events"][tt]["state"][name]["rotation"]["w"]=+req["initial"][name]["rotation"]["w"]
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

Date.prototype.addHours = function(h) {    
   this.setTime(this.getTime() + (h*60*60*1000)); 
   return this;   
}
