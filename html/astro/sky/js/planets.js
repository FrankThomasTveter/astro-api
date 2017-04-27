console.log("Loading planets.js");

var Planets = {	 AU : 149597870,
		 sun  :  {
		     title : 'The Sun',
		     type:   'sphere',
		     radius : 6.96342e5,
		     color : '#ffff00',
		     //color : '#ffffff',
		     mg : -26.74, // at 1 au
		     md : 1,
		     rotation : new Vector3(),
		     position : new Vector3()
		 },
		 mercury : {
		     title : 'Mercury',
		     type:   'sphere',
		     radius:2439.0,
		     color : '#588a7b',
		     shade : 'sun',
		     mg : -2.45, // superior conjunction
		     md : 1.307,
		     rotation : new Vector3(),
		     position : new Vector3()
		 },
		 venus : {
		     title : 'Venus',
		     type:   'sphere',
		     radius : 6051.0,
		     color : '#fda700',
		     shade : 'sun',
		     mg : -3.82, // far side of sun
		     md : 1.718,
		     rotation : new Vector3(),
		     position : new Vector3()
		 },
		 earth : {
		     title : 'The Earth',
		     type:   'sphere',
		     radius : 6371.0088,
		     color : '#1F7CDA',
		     shade : 'sun',
		     shadows : ['moon'],
		     rotation : new Vector3(),
		     position : new Vector3()
		 },
		 mars : {
		     title : 'Mars',
		     type:   'sphere',
		     radius : 3376.0,
		     color : '#ff3300',
		     shade : 'sun',
		     mg : -2.91, //closest to earth
		     md : 0.38,
		     rotation : new Vector3(),
		     position : new Vector3()
		 },
		 jupiter : {
		     title : 'Jupiter',
		     type:   'sphere',
		     radius : 71492.0,
		     color : '#ff9932',
		     shade : 'sun',
		     shadows : ['io', 'europa', 'ganymedes','callisto'],
		     mg : -2.94 , //closest to earth
		     md : 3.95,
		     rotation : new Vector3(),
		     position : new Vector3()
		 },
		 saturn : {
		     title : 'Saturn',
		     type:   'sphere',
		     radius : 58232.0,
		     color : '#ffcc99',
		     shade : 'sun',
		     mg : -0.49, // at opposition
		     md : 8.05,
		     rotation : new Vector3(),
		     position : new Vector3(),
		     ring : {
			 innerRadius : 74500.0,
			 outerRadius : 117580.0,
			 density : 0.9,
			 color : '#aa8866'
		     },
		 },
		 uranus : {
		     title : 'Uranus',
		     type:   'sphere',
		     radius : 25559.0,
		     color : '#99ccff',
		     shade : 'sun',
		     mg : 5.32, // closest to earth
		     md : 17.4,
		     rotation : new Vector3(),
		     position : new Vector3(),
		     ring : {
			 innerRadius : 54500.0,
			 outerRadius : 57580.0,
			 density : 0.5,
			 color : '#6688aa'
		     },
		 },
		 neptune : {
		     title : 'Neptune',
		     type:   'sphere',
		     radius : 24764.0,
		     color : '#3299ff',
		     shade : 'sun',
		     mg : 7.78, // closest to earth
		     md : 28.8,
		     rotation : new Vector3(),
		     position : new Vector3()
		 },
		 pluto : {
		     title : 'Pluto',
		     type:   'sphere',
		     radius : 1153.0,
		     color : '#aaaaaa',
		     shade : 'sun',
		     shadows : ['charon'],
		     mg : 13.65, // closest to earth
		     md : 28.7,
		     rotation : new Vector3(),
		     position : new Vector3()
		 },
		 moon : {
		     title : 'The Moon',
                     orbits: 'earth',
		     type:   'sphere',
		     radius : 1738.1,
		     color : "#ffffff",
		     shade : 'sun',
		     shadows : ['earth'],
		     mg : -12.90, // closest to earth
		     md : 0.00257,
		     rotation : new Vector3(),
		     position : new Vector3()
		 },
		 deathStar : {
		     title : 'Death Star',
		     orbits: 'moon',
		     type:   'sphere',
		     radius : 900.0,
		     color : '#aaaaaa',
		     shade : 'sun',
		     rotation : new Vector3(),
		     position : new Vector3()
		 },
		 observer : {
		     position : new Vector3(),
		     i : new Vector3(),
		     j : new Vector3(),
		     k : new Vector3(),
		     zenith : new Vector3()
		 }
	      };

Planets.createStarfield	= function(dir){
    var ret={};
    return ret;	
};
Planets.copyObserver = function (src, trg) {
    trg.position.copy(src.position);
    trg.i.copy(src.i);
    trg.j.copy(src.j);
    trg.k.copy(src.k);
    trg.zenith.copy(src.zenith);
};
Planets.copyBody = function (src, trg) {
    trg.position.copy(src.position);
    trg.position.vx=src.position.vx;
    trg.position.vy=src.position.vy;
    trg.position.vz=src.position.vz;
    trg.rotation.copy(src.rotation);
    trg.rotation.lon=src.rotation.lon;
    trg.rotation.lat=src.rotation.lat;
    trg.rotation.ra=src.rotation.ra;
    trg.rotation.dec=src.rotation.dec;
    trg.rotation.w=src.rotation.w;
};
Planets.linkShadows = function () {
    for (var body in Planets) {
	// link source of shade
	if (body.shade !== undefined) {
	    var shades= body.shade;
	    for (var shade in shades) {
		if (Planets[shade] !== undefined) {
		    if (body.shadeLink === undefined) {body.shadeLink={};};
		    body.shadeLink[shade]=Planets[shade];
		}
	    }
	}
	// link bodies shaded by this body
	if (body.shadows !== undefined) {
	    var shadows= body.shadows;
	    for (var shadow in shadows) {
		if (Planets[shadow] !== undefined) {
		    if (body.shadowLink === undefined) {body.shadowLink={};};
		    body.shadowLink[shadow]=Planets[shadow];
		}
	    }
	}
    }
}
