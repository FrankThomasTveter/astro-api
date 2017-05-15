var rawData = [];
var targetSet=false;
var targetTime=0;
var targetId=undefined;
var lastLat=999;
var lastLon=999;
var targetUpdate=new Date().getTime()-10000000;
var drawAll=true;
var mapReady=false;
function drawData(documentTable, id) {
    if (rawData[id]==undefined) {
	documentTable.innerHTML="<em>No data available.</em>";
    } else {
	var d=new Date();
	var tnow=d.getTime();
	var cellCnt=4;
	var reportCnt=0;
	var documentReportCnt=documentTable.rows.length;
	rawData[id].clean(undefined);
	var dataReportsCnt=rawData[id].length;
	if (dataReportsCnt != documentReportCnt) {
	    //console.log("Must redraw."+dataReportsCnt+" "+documentReportCnt);
	    drawAll=true;
	};
	if (drawAll && documentReportCnt==0) {
	    documentTable.innerHTML="";
	};
	for (var jj=0; jj< dataReportsCnt;jj++) {
	    if (drawAll) {
		if (reportCnt >= documentReportCnt) { // create row
		    //console.log("Report:"+reportCnt+"  "+jj+" "+dataReportsCnt+rawData[id][jj][0]);
		    var row=documentTable.insertRow(reportCnt);
		    for (var kk=0; kk < cellCnt;kk++) {
			var cell=row.insertCell(kk);
			cell.innerHTML="init"+reportCnt;
		    }
		};
	    };
	    var dataReport=rawData[id][jj];
	    var documentReport=documentTable.rows[reportCnt];
	    documentCells=documentReport.cells;
	    var t=new Date(dataReport[0]);
	    var repid=dataReport[3];
	    if (drawAll) {
                //console.log("Drawing buttons.");
 		documentCells[0].innerHTML="<button class=\"delete\" onclick=\"deleteRow("+
		    id+","+reportCnt+")\">X</button>";
		documentCells[1].innerHTML="<button class=\"hot\" onclick=\"setTarget("+
		    (t*1)+","+(repid)+")\">"+t.nice()+"</button>";
		//console.log("drawing button:"+t.nice());
	    }
	    var dt=t-tnow;
	    if (targetSet) {dt=t-targetTime;}
	    documentCells[2].innerHTML=getTimeDiff(dt);
	    //documentCells[4].innerHTML=dataReport[0];
	    if (drawAll) {
		documentCells[3].innerHTML="<button class=\"launch\" onclick=\"setTargetId("+
		    (t*1)+","+(repid)+");launch3D();\">"+dataReport[5].substring(20)+"</button>";
		//documentCells[3].innerHTML=dataReport[5].substring(20);
	    }
	    if (Math.abs(dt) < 1100) {
		documentReport.style.backgroundColor="#FFFFFF";
	    } else if (dt < 0) {
		documentReport.style.backgroundColor="#AAAAAA";
	    } else {
		documentReport.style.backgroundColor="#00FF00";
	    }
	    reportCnt=reportCnt+1;
	}
	if (drawAll) {
	    if (documentReportCnt > reportCnt) {
		for (var ii=documentReportCnt-1; ii>= reportCnt;ii--) {
		    documentTable.deleteRow(ii);
		}
	    }
	}
	if (drawAll && reportCnt==0) {
	    documentTable.innerHTML="<em>No data available.</em>";
	}
	if (drawAll) {
	    drawAll=false;
            //console.log("Stopped redrawing."+documentTable.rows.length);
	}
	//console.log(rawData[id][0][0]);
	// documentPos.innerHTML="Data:" + rawData[id];
    }
}
function deleteRow(id,row) {
    //console.log("Deleting row:"+row);
    rawData[id][row]=undefined;
    var drawAll=true;
}
function setTarget(tt,id) {
    //console.log("Setting target to:"+tt+" "+targetSet);
    if (targetSet && tt==targetTime) {
	targetSet=false;
	targetId=undefined;
    } else {
	targetSet=true;
	targetTime=tt;
	setStartDate(targetTime);
	setEndDate(targetTime);
	targetId=id;
    }
}
function setTargetId(tt,id) {
    //console.log("Setting target to:"+tt+" "+targetSet);
    targetSet=true;
    targetTime=tt;
    setStartDate(targetTime);
    setEndDate(targetTime);
    targetId=id;
}

function getTimeDiff(dt) {
    var s="";
    var msec=Math.abs(dt);
    var dd = Math.floor(msec / 1000 / 60 / 60 / 24);
    msec -= dd * 1000 * 60 * 60 * 24;
    var hh = Math.floor(msec / 1000 / 60 / 60);
    msec -= hh * 1000 * 60 * 60;
    var mm = Math.floor(msec / 1000 / 60);
    msec -= mm * 1000 * 60;
    var ss = Math.floor(msec / 1000);
    msec -= ss * 1000;
    if (dt<0) {
	s="-";
    } else if (dt > 0) {
	s="+";
    } else {
	s="0";
    }
    if (dd != 0) s=s+" "+numberWithCommas(dd)+"d";
    if (hh != 0) s=s+" "+hh+"h";
    if (mm != 0) s=s+" "+mm+"m";
    if (ss != 0) s=s+" "+ss+"s";
    return s;
}

function numberWithCommas(x) {
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

function isNewPos(lat,lon) {
    var ret= (Math.abs(lastLat-lat)+Math.abs(lastLon-lon) > 1./40000000.0);
    //console.log("Lat:"+lat+" Lon:"+lon+"  "+ret);
    if (ret) {
	lastLat=lat;
	lastLon=lon;
    }
    return ret;
}
function addDataToArray(data,status,id,documentLog) {
    //console.log("adding data to array.");
    if (rawData[id] != undefined) {
	dataToArray(data,status,id+1,documentLog);
	dataMerge(id,id+1);
    } else {
	dataToArray(data,status,id,documentLog);
    }
}
function dataMerge(id1,id2) {
    rawData[id1]=rawData[id1].concat(rawData[id2]).unique().sort(function(a, b){return a[0]-b[0]});
}
Array.prototype.unique = function() {
    var a = this.concat();
    for(var i=0; i<a.length; ++i) {
        for(var j=i+1; j<a.length; ++j) {
	    //[localDtg,eventId,reportId,reportVal,reportHint]
            if(Math.abs(a[i][0]-a[j][0]) < 2000 && a[i][1] == a[j][1] &&a[i][2] == a[j][2])
                a.splice(j--, 1);
        }
    }

    return a;
};
function clearArray() {
    rawData = [];
    drawAll=true;
}
function dataToArray(data,status,id,documentLog) {
    lastCnt=1;
    if (status == "success") {
	var d=new Date();
	var tnow=d.getTime();
	//console.log("Adding data to array-id="+id);
	if (rawData[id]==undefined) {
	    rawData[id]=[];
	}
	var cnt=0;
	targetUpdate=new Date("3000-01-01T00:00:00.000Z").getTime();
	var events=data.getElementsByTagName("Event");
	var len=rawData[id].length;
	for (var ii = 0; ii < events.length; ii++) {
	    var event=events[ii];
	    var eventSeq = event.getAttribute("Seq");
	    var eventId = event.getAttribute("Id");
	    var eventStart = event.getAttribute("Start");
	    var eventSearch = event.getAttribute("Search");
	    var eventStop = event.getAttribute("Stop");
	    var eventVal1 = event.getAttribute("Val1");
	    var eventVal2 = event.getAttribute("Val2");
	    var eventVal3 = event.getAttribute("Val3");
	    var eventReports = event.getAttribute("reports");
	    var costms=event.getAttribute("cost");
	    var reports=event.getElementsByTagName("Report");
	    var cost = 0;
	    if (costms.length > 0) cost=costms.match(/^[\d\.]+/g);
	    updateCost(cost);
	    for (var jj = 0; jj < reports.length; jj++) {
		var report=reports[jj];
		var reportDtg=report.getAttribute("time");
		var t=new Date(reportDtg);
		var localDtg=t.getTime();
		//console.log("Got date: "+reportDtg+" => "+localDtg);
		if (localDtg > tnow) {
		    if (targetUpdate < tnow || localDtg < targetUpdate ) {
			targetUpdate=localDtg;
		    };
		}
		var reportId=report.getAttribute("repId");
		var reportVal=report.getAttribute("repVal");
		var reportHint=report.getAttribute("hint");
		var localSort=getSortTime(localDtg,eventId,reportId);
		rawData[id][cnt++]=[localDtg,localSort,eventId,reportId,reportVal,reportHint];
	    };
	};
	if (cnt < len) {rawData[id].splice(cnt, len-cnt);}
	rawData[id].sort(function(a, b){return a[1]-b[1]});
	drawAll=true;
	//console.log("Added data to array-id="+id+" "+rawData[id].length);
	documentLog.innerHTML = "";
    } else {
	documentLog.innerHTML = "<em>Server-request:"+status+"</em>";
    }
}

Date.prototype.nice = function() {
   var yyyy = this.getFullYear().toString();
   var mm = (this.getMonth()+1).toString(); // getMonth() is zero-based
   var dd  = this.getDate().toString();
   var hh  = this.getHours().toString();
   var mi  = this.getMinutes().toString();
   var ss  = this.getSeconds().toString();
   return yyyy+"-"+(mm[1]?mm:"0"+mm[0])+"-"+(dd[1]?dd:"0"+dd[0])+" "
   +(hh[1]?hh:"0"+hh[0])+":"+(mi[1]?mi:"0"+mi[0])+":"+(ss[1]?ss:"0"+ss[0]); // padding
};

Array.prototype.clean = function(deleteValue) {
  for (var i = 0; i < this.length; i++) {
    if (this[i] == deleteValue) {         
      this.splice(i, 1);
      i--;
    }
  }
  return this;
};

function request() {
    this.clean             = function () {var obj=Object.keys(this);for (var ii=0; ii<obj.length;ii++) 
				    {if (! obj[ii].match(/^event/g) && ! obj[ii].match(/^debug/g)) {delete this[obj[ii]];}}}
    this.addDebug          = function(id,val) {this["debug"]=1;};
    this.addSearch         = function(id,val) {this["event"+id+"Search"]=val;};
    this.addStartTime      = function(id,val) {this["event"+id+"Start"]=val;};
    this.addStopTime       = function(id,val) {this["event"+id+"Stop"]=val;};
    this.addPosition       = function(id,lat,lon,hgt) {this["event"+id+"Val1"]=lat;
						       this["event"+id+"Val2"]=lon;
						       this["event"+id+"Val3"]=hgt;}
    this.addSunRise        = function(id) {this["event"+id+"Id"]=600;};
    this.addSunSet         = function(id) {this["event"+id+"Id"]=610;};
    this.addMoonState      = function(id) {this["event"+id+"Id"]=100;};
    this.addMoonTcEf       = function(id,dt) {this["event"+id+"Id"]=110;
					  this["event"+id+"Val4"]=dt;};
    this.addSunState       = function(id) {this["event"+id+"Id"]=120;};
    this.addSunVisible     = function(id) {this["event"+id+"Id"]=125;};
    this.addSunTcEf        = function(id,dt) {this["event"+id+"Id"]=130;
					 this["event"+id+"Val4"]=dt;};
    this.addSouthSolstice = function(id) {this["event"+id+"Id"]=150;};
    this.addAscSolarEquinox  = function(id) {this["event"+id+"Id"]=160;};
    this.addNorthSolstice = function(id) {this["event"+id+"Id"]=170;};
    this.addDescSolarEquinox= function(id) {this["event"+id+"Id"]=180;};
    this.addEarthPerihelion= function(id) {this["event"+id+"Id"]=190;};
    this.addEarthAphelion  = function(id) {this["event"+id+"Id"]=195;};

    this.addSouthLunastice = function(id) {this["event"+id+"Id"]=290;};
    this.addAscLunarEquinox  = function(id) {this["event"+id+"Id"]=292;};
    this.addNorthLunastice = function(id) {this["event"+id+"Id"]=294;};
    this.addDescLunarEquinox= function(id) {this["event"+id+"Id"]=296;};
    this.addMoonPerigee    = function(id) {this["event"+id+"Id"]=200;};
    this.addMoonApogee     = function(id) {this["event"+id+"Id"]=205;};
    this.addMoonNew        = function(id) {this["event"+id+"Id"]=210;};
    this.addMoonFirstQuart = function(id) {this["event"+id+"Id"]=220;};
    this.addMoonFull       = function(id) {this["event"+id+"Id"]=230;};
    this.addMoonLastQuart  = function(id) {this["event"+id+"Id"]=240;};
    this.addMoonPhase      = function(id) {this["event"+id+"Id"]=250;};
    this.addMoonIllMin     = function(id) {this["event"+id+"Id"]=260;};
    this.addMoonIllMax     = function(id) {this["event"+id+"Id"]=270;};
    this.addMoonIll        = function(id,val) {this["event"+id+"Id"]=280;};
    this.addMercInfConj    = function(id) {this["event"+id+"Id"]=300;};
    this.addMercSupConj    = function(id) {this["event"+id+"Id"]=310;};
    this.addMercWestElong  = function(id) {this["event"+id+"Id"]=320;};
    this.addMercEastElong  = function(id) {this["event"+id+"Id"]=330;};
    this.addVenusInfConj   = function(id) {this["event"+id+"Id"]=340;};
    this.addVenusWestElong = function(id) {this["event"+id+"Id"]=350;};
    this.addVenusSupConj   = function(id) {this["event"+id+"Id"]=360;};
    this.addVenusEastElong = function(id) {this["event"+id+"Id"]=370;};
    this.addMarsConj       = function(id) {this["event"+id+"Id"]=380;};
    this.addMarsWestQuad   = function(id) {this["event"+id+"Id"]=390;};
    this.addMarsOpp        = function(id) {this["event"+id+"Id"]=400;};
    this.addMarsEastQuad   = function(id) {this["event"+id+"Id"]=410;};
    this.addJupiterConj    = function(id) {this["event"+id+"Id"]=420;};
    this.addJupiterWestQuad= function(id) {this["event"+id+"Id"]=430;};
    this.addJupiterOpp     = function(id) {this["event"+id+"Id"]=440;};
    this.addJupiterEastQuad= function(id) {this["event"+id+"Id"]=450;};
    this.addSaturnConj     = function(id) {this["event"+id+"Id"]=460;};
    this.addSaturnWestQuad = function(id) {this["event"+id+"Id"]=470;};
    this.addSaturnOpp      = function(id) {this["event"+id+"Id"]=480;};
    this.addSaturnEastQuad = function(id) {this["event"+id+"Id"]=490;};
    this.addMercTransit    = function(id) {this["event"+id+"Id"]=500;};
    this.addVenusTransit   = function(id) {this["event"+id+"Id"]=520;};
    this.addLunarEclipseMinMax = function(id, min,max) {this["event"+id+"Id"]=550;
						    this["event"+id+"Val1"]=min;
						    this["event"+id+"Val2"]=max;};
    this.addLunarEclipse       = function(id, min,max) {this["event"+id+"Id"]=560;};
    this.addSunEleMax      = function(id) {this["event"+id+"Id"]=620;};
    this.addSunEleMin      = function(id) {this["event"+id+"Id"]=630;};
    this.addTwilightCivilStart         = function(id) {this["event"+id+"Id"]=640;};
    this.addTwilightCivilStop          = function(id) {this["event"+id+"Id"]=650;};
    this.addTwilightNauticalStart      = function(id) {this["event"+id+"Id"]=660;};
    this.addTwilightNauticalStop       = function(id) {this["event"+id+"Id"]=670;};
    this.addTwilightAstronomicalStart  = function(id) {this["event"+id+"Id"]=680;};
    this.addTwilightAstronomicalStop   = function(id) {this["event"+id+"Id"]=690;};
    this.addNightStart     = function(id) {this["event"+id+"Id"]=700;};
    this.addNightStop      = function(id) {this["event"+id+"Id"]=710;};
    this.addSunAzi         = function(id,target) {this["event"+id+"Id"]=750;
					    this["event"+id+"Val4"]=target;};
    this.addSunTime        = function(id,target) {this["event"+id+"Id"]=760;
					     this["event"+id+"Val4"]=target;};
    this.addMoonTime       = function(id,target) {this["event"+id+"Id"]=770;
					      this["event"+id+"Val4"]=target;};
    this.addMoonRise       = function(id) {this["event"+id+"Id"]=800;};
    this.addMoonSet        = function(id) {this["event"+id+"Id"]=810;};
    this.addMoonEleMax     = function(id) {this["event"+id+"Id"]=820;};
    this.addMoonEleMin     = function(id) {this["event"+id+"Id"]=830;};
    this.addMoonAzi        = function(id,target) {this["event"+id+"Id"]=840;
						  this["event"+id+"Val4"]=target;};
    this.addPolarSunDayStart   = function(id) {this["event"+id+"Id"]=900;};
    this.addPolarSunDayStop    = function(id) {this["event"+id+"Id"]=910;};
    this.addPolarSunNightStart = function(id) {this["event"+id+"Id"]=920;};
    this.addPolarSunNightStop  = function(id) {this["event"+id+"Id"]=930;};
    this.addPolarMoonDayStart  = function(id) {this["event"+id+"Id"]=940;};
    this.addPolarMoonDayStop   = function(id) {this["event"+id+"Id"]=950;};
    this.addPolarMoonNightStart= function(id) {this["event"+id+"Id"]=960;};
    this.addPolarMoonNightStop = function(id) {this["event"+id+"Id"]=970;};
    this.addSunEclipseMinMax   = function(id,min,max) {this["event"+id+"Id"]=980;
						       this["event"+id+"Val4"]=min;
						       this["event"+id+"Val5"]=max;};
    this.addSunEclipse     = function(id) {this["event"+id+"Id"]=990;};
    this.addSolarSystemTcEf= function(id,dt) {this["event"+id+"Id"]=1000;
					      this["event"+id+"Val4"]=dt;};
}

function getSortTime(localDtg,eventId,reportId) {
    if (eventId == 640 ) { // civil twilight start
	return localDtg+1;
    } else if (eventId == 650) {
	return localDtg-1;
    } else {
	return localDtg;
    }
}

//Function called to initialize / create the map.
//This is called when the page has loaded.
function initMap() {
    //console.log("Initialising map.");
    //The center location of our map.
    var lat= +(document.getElementById("lat").value);
    var lng= +(document.getElementById("lng").value);
    var centerOfMap = new google.maps.LatLng(lat,lng);
 
    //Map options.
    var options = {
	mapTypeId: google.maps.MapTypeId.ROADMAP,
	mapTypeControl: false,
	disableDoubleClickZoom: true,
	zoomControlOptions: true,
	streetViewControl: false,
	center: centerOfMap, //Set center.
	zoom: 3 //The zoom value.
    };

    //Create the map object.
    map = new google.maps.Map(document.getElementById('map'), options);
    map.setCenter(centerOfMap);

    var styleOptions = {name: "Dummy Style"};
    var MAP_STYLE = [{
        featureType: "road",
        elementType: "all",
        stylers: [{ visibility: "on" }]
    }];
    var mapType = new google.maps.StyledMapType(MAP_STYLE, styleOptions);
    map.mapTypes.set("Dummy Style", mapType);
    map.setMapTypeId("Dummy Style");

    var latlng = new google.maps.LatLng(lat,lng);
    marker = new google.maps.Marker({
	position: latlng,
	draggable: true //make it draggable
    });
    marker.setMap(map);
    google.maps.event.addListener(marker, 'dragend', function(event){markerLocation();});

    //Listen for any clicks on the map.
    google.maps.event.addListener(map, 'click', function(event) {                
        //Get the location that the user clicked.
        var clickedLocation = event.latLng;
        //If the marker hasn't been added.
        if(marker === undefined){
            //Create the marker.
	    marker = new google.maps.Marker({
                position: clickedLocation,
                map: map,
                draggable: true //make it draggable
            });
            //Listen for drag events!
            google.maps.event.addListener(marker, 'dragend', function(event){
                markerLocation();
            });
        } else{
            //Marker has already been added, so just change its location.
            marker.setPosition(clickedLocation);
        }
        //Get the marker's location.
        markerLocation();
    });
    //console.log("Initialised map.");
    mapReady=true;
}
        
//This function will get the marker's current location and then add the lat/long
//values to our textfields so that we can save the location.
function markerLocation(){
    //Get location.
    var currentLocation = marker.getPosition();
    //Add lat and lng values to a field that we can save.
    document.getElementById('lat').value = currentLocation.lat(); //latitude
    document.getElementById('lng').value = currentLocation.lng(); //longitude
}
function geoloc(latitude, longitude)
{
    this.coords={latitude:latitude,longitude:longitude};
}    
function setMapPosition() {
    if (mapReady) {
	var lat= +(document.getElementById("lat").value);
	var lng= +(document.getElementById("lng").value);
	if (lat !== undefined && lng !== undefined) {
	    var latlng = new google.maps.LatLng(lat,lng);
	    marker.setPosition(latlng);
	    map.setCenter(latlng);   
	}
    }
}

//Load the map when the page has finished loading.
google.maps.event.addDomListener(window, 'load', initMap);
function setCookie(cname, cvalue, exdays) {
    var d = new Date();
    d.setTime(d.getTime() + (exdays*24*60*60*1000));
    var expires = "expires="+d.toUTCString();
    document.cookie = cname + "=" + cvalue + "; " + expires;
}
function getCookie(cname) {
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i=0; i<ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') c = c.substring(1);
        if (c.indexOf(name) == 0) return c.substring(name.length,c.length);
    }
    return "";
} 
