console.log("Loading controls.js");

Controls = function ( object, domElement ) {
    var _this = this;
    this.object = object;
    this.domElement = ( domElement !== undefined ) ? domElement : document;
    this.noRotate = false;
    this.noZoom   = false;
    this.noPan    = true;
    this.enabled  = true;
    this.set1     = false;
    this.set2     = false;
    this.redraw   = false;
    _movePrev = new Vector2();
    _moveCurr = new Vector2();    
    _zoomStart = new Vector2();
    _zoomEnd = new Vector2();
    _panStart = new Vector2();
    _panEnd = new Vector2();
    _touchZoomDistanceStart = 0;
    _touchZoomDistanceEnd = 0;
    var STATE = { NONE: - 1, ROTATE: 0, ZOOM: 1, PAN: 2, TOUCH_ROTATE: 3, TOUCH_ZOOM_PAN: 4 };
    this.zoomSpeed = 0.2;
    this.staticMoving = false;
    this.dynamicDampingFactor = 0.2;
    this.keys = [ 65 /*A*/, 83 /*S*/, 68 /*D*/ ];
    var _state = STATE.NONE;
    var _prevState = STATE.NONE;
    // events
    var changeEvent = { type: 'change' };
    var startEvent = { type: 'start' };
    var endEvent = { type: 'end' };
    this.handleResize = function () {
	if ( this.domElement === document ) {
	    this.object.setScreen(0,0,window.innerWidth,window.innerHeight);
	    this.redraw=true;
	} else {
	    var box = this.domElement.getBoundingClientRect();
	    // adjustments come from similar code in the jquery offset() function
	    var d = this.domElement.ownerDocument.documentElement;
	    this.object.setScreen(box.left + window.pageXOffset - d.clientLeft,
				  box.top + window.pageYOffset - d.clientTop,
				  box.width, box.height);
	    this.redraw=true;
	}
    };
    this.update = function () {
	if ( ! _this.noRotate ) {
	    this.rotate();
	}
	if ( ! _this.noZoom ) {
	    this.zoom();
	}
	if ( ! _this.noPan ) {
	    _this.pan();
	}
    };
    this.rotate = function() {
	moveDirection = {};
	moveDirection.x = -_moveCurr.x + _movePrev.x;
	moveDirection.y = -_moveCurr.y + _movePrev.y;
	moveDirection.z = 0;
	if ( Math.abs(moveDirection.x) > 0 || Math.abs(moveDirection.y) > 0 ) {
	    var angle={};
	    angle.j =  -this.object.getFovX() * moveDirection.x ;
	    angle.i =   this.object.getFovY() * moveDirection.y ;
	    angle.k=0;
	    //angle.i=0.0;
	    //angle.j=0.2
//	    var documentLog = document.getElementById("log");
//	    documentLog.innerHTML="Pos:"+_moveCurr.x +" "+_moveCurr.y +" "+_this.pageX+" "+_this.object.screen.width;
	    this.object.rotateAxis(angle);
	    this.redraw=true;
	    _movePrev.x= _moveCurr.x;
	    _movePrev.y= _moveCurr.y;
	    _movePrev.z= _moveCurr.z;
	};
    };
    this.zoom = function () {
	var factor;
	if ( _state === STATE.TOUCH_ZOOM_PAN ) {
	    factor = _touchZoomDistanceStart / _touchZoomDistanceEnd;
	    _touchZoomDistanceStart = _touchZoomDistanceEnd;
	    this.object.setFovX(this.object.getFovX()*factor);
	    this.redraw=true;
	} else {
	    factor = 1.0 + ( _zoomEnd.y - _zoomStart.y ) * _this.zoomSpeed;
	    if ( factor !== 1.0 && factor > 0.0 ) {
		this.object.setFovX(this.object.getFovX()*factor);
		this.redraw=true;
		_zoomStart.y += ( _zoomEnd.y - _zoomStart.y ) * this.dynamicDampingFactor;
	    }
	}
    };
    this.pan = function() {
	console.warn("pan is not implemented.");
    };
    this.target = function (target) {
	
    };
    this.followVector = function (target) {
    };
    this.followAxis = function (target) {
    };
    // methods
    this.handleEvent = function ( event ) {
	if ( typeof this[ event.type ] == 'function' ) {
	    this[ event.type ]( event );
	}
    };
    var getMouseOnScreen = ( function () { // 0 .. 1
	var vector = new Vector2();
	return function getMouseOnScreen( pageX, pageY ) {
	    vector.set(
		( pageX - _this.object.screen.left ) / _this.object.screen.width,
		( pageY - _this.object.screen.top ) / _this.object.screen.height
	    );
	    return vector;
	};
    }() );
    var getMouseOnCircle = ( function () { // -1 .. +1
	var vector = new Vector2();
	return function getMouseOnCircle( pageX, pageY ) {
	    vector.set(
		( ( + pageX - _this.object.screen.width * 0.5 - _this.object.screen.left ) / ( _this.object.screen.width * 0.5 ) ),
		( ( - pageY + _this.object.screen.height* 0.5 + _this.object.screen.top ) / ( _this.object.screen.height * 0.5 ) )// screen.width intentional
	    );
	    _this.pageX=pageX;
	    _this.pageY=pageY;
	    return vector;
	};
    }() );
    // listeners
    function keydown( event ) {
	if ( _this.enabled === false ) return;
	window.removeEventListener( 'keydown', keydown );
	_prevState = _state;
	if ( _state !== STATE.NONE ) {
	    return;
	} else if ( event.keyCode === _this.keys[ STATE.ROTATE ] && ! _this.noRotate ) {
	    _state = STATE.ROTATE;
	} else if ( event.keyCode === _this.keys[ STATE.ZOOM ] && ! _this.noZoom ) {
	    _state = STATE.ZOOM;
	} else if ( event.keyCode === _this.keys[ STATE.PAN ] && ! _this.noPan ) {
	    _state = STATE.PAN;
	}
    }
    function keyup( event ) {
	if ( _this.enabled === false ) return;
	_state = _prevState;
	window.addEventListener( 'keydown', keydown, false );
    }
    function mousedown( event ) {
	if ( _this.enabled === false ) return;
	event.preventDefault();
	event.stopPropagation();
	if ( _state === STATE.NONE ) {
	    _state = event.button;
	}
	if ( _state === STATE.ROTATE && ! _this.noRotate ) {
	    _moveCurr.copy( getMouseOnCircle( event.pageX, event.pageY ) );
	    _movePrev.copy( _moveCurr );
	} else if ( _state === STATE.ZOOM && ! _this.noZoom ) {
	    _zoomStart.copy( getMouseOnScreen( event.pageX, event.pageY ) );
	    _zoomEnd.copy( _zoomStart );
	} else if ( _state === STATE.PAN && ! _this.noPan ) {
	    _panStart.copy( getMouseOnScreen( event.pageX, event.pageY ) );
	    _panEnd.copy( _panStart );
	}
	document.addEventListener( 'mousemove', mousemove, false );
	document.addEventListener( 'mouseup', mouseup, false );
	_this.dispatchEvent( startEvent );
    }
    function mousemove( event ) {
	if ( _this.enabled === false ) return;
	event.preventDefault();
	event.stopPropagation();
	if ( _state === STATE.ROTATE && ! _this.noRotate ) {
	    //_movePrev.copy( _moveCurr );
	    _moveCurr.copy( getMouseOnCircle( event.pageX, event.pageY ) );
	} else if ( _state === STATE.ZOOM && ! _this.noZoom ) {
	    _zoomEnd.copy( getMouseOnScreen( event.pageX, event.pageY ) );
	} else if ( _state === STATE.PAN && ! _this.noPan ) {
	    _panEnd.copy( getMouseOnScreen( event.pageX, event.pageY ) );
	}
    }
    function mouseup( event ) {
	if ( _this.enabled === false ) return;
	event.preventDefault();
	event.stopPropagation();
	_state = STATE.NONE;
	document.removeEventListener( 'mousemove', mousemove );
	document.removeEventListener( 'mouseup', mouseup );
	_this.dispatchEvent( endEvent );
    }
    function mousewheel( event ) {
	if ( _this.enabled === false ) return;
	event.preventDefault();
	event.stopPropagation();
	var delta = 0;
	if ( event.wheelDelta ) {
	    // WebKit / Opera / Explorer 9
	    delta = event.wheelDelta / 40;
	} else if ( event.detail ) {
	    // Firefox
	    delta = - event.detail / 3;
	}
	_zoomStart.y += delta * 0.01;
	_this.dispatchEvent( startEvent );
	_this.dispatchEvent( endEvent );
    }
    function touchstart( event ) {
	if ( _this.enabled === false ) return;
	switch ( event.touches.length ) {
	case 1:
	    _state = STATE.TOUCH_ROTATE;
	    _moveCurr.copy( getMouseOnCircle( event.touches[ 0 ].pageX, event.touches[ 0 ].pageY ) );
	    _movePrev.copy( _moveCurr );
	    this.set1=true;
	    this.set2=false;
	    break;
	case 2:
	    _state = STATE.TOUCH_ZOOM_PAN;
	    var dx = event.touches[ 0 ].pageX - event.touches[ 1 ].pageX;
	    var dy = event.touches[ 0 ].pageY - event.touches[ 1 ].pageY;
	    _touchZoomDistanceEnd = _touchZoomDistanceStart = Math.sqrt( dx * dx + dy * dy );
	    var x = ( event.touches[ 0 ].pageX + event.touches[ 1 ].pageX ) / 2;
	    var y = ( event.touches[ 0 ].pageY + event.touches[ 1 ].pageY ) / 2;
	    _moveCurr.copy( getMouseOnScreen( x, y ) );
	    _movePrev.copy( _moveCurr );
	    this.set1=false;
	    this.set2=true;
	    break;
	default:
	    this.set1=false;
	    this.set2=false;
	    _state = STATE.NONE;
	}
	_this.dispatchEvent( startEvent );
    }
    function touchmove( event ) {
	if ( _this.enabled === false ) return;
	event.preventDefault();
	event.stopPropagation();
	switch ( event.touches.length ) {
	case 1:
	    if (this.set1) {
		//_movePrev.copy( _moveCurr );
		_moveCurr.copy( getMouseOnCircle(  event.touches[ 0 ].pageX, event.touches[ 0 ].pageY ) );
	    } else {
		_moveCurr.copy( getMouseOnCircle(  event.touches[ 0 ].pageX, event.touches[ 0 ].pageY ) );
		_movePrev.copy( _moveCurr );
	    }
	    this.set1=true;
	    this.set2=false;
	    break;
	case 2:
	    var dx = event.touches[ 0 ].pageX - event.touches[ 1 ].pageX;
	    var dy = event.touches[ 0 ].pageY - event.touches[ 1 ].pageY;
	    _touchZoomDistanceEnd = Math.sqrt( dx * dx + dy * dy );
	    var x = ( event.touches[ 0 ].pageX + event.touches[ 1 ].pageX ) / 2;
	    var y = ( event.touches[ 0 ].pageY + event.touches[ 1 ].pageY ) / 2;
	    if (this.set2) {
		_movePrev.copy( _moveCurr );
		_moveCurr.copy( getMouseOnScreen( x, y ) );
	    } else {
		_moveCurr.copy( getMouseOnScreen( x, y ) );
		_movePrev.copy( _moveCurr );
		_touchZoomDistanceStart = _touchZoomDistanceEnd;
	    };
	    this.set1=false;
	    this.set2=true;
	    break;
	default:
	    _state = STATE.NONE;
	}
    }
    function touchend( event ) {
	if ( _this.enabled === false ) return;
	switch ( event.touches.length ) {
	case 1:
	    if (this.set1) {
		_movePrev.copy( _moveCurr );
		_moveCurr.copy( getMouseOnCircle(  event.touches[ 0 ].pageX, event.touches[ 0 ].pageY ) );
	    } else {
		_moveCurr.copy( getMouseOnCircle(  event.touches[ 0 ].pageX, event.touches[ 0 ].pageY ) );
		_movePrev.copy( _moveCurr );
	    }
	    this.set1=false;
	    break;
	case 2:
	    _touchZoomDistanceStart = _touchZoomDistanceEnd = 1;
	    var x = ( event.touches[ 0 ].pageX + event.touches[ 1 ].pageX ) / 2;
	    var y = ( event.touches[ 0 ].pageY + event.touches[ 1 ].pageY ) / 2;
	    _moveCurr.copy( getMouseOnCircle(  x,y ) );
	    _movePrev.copy( _moveCurr );
	    //_panEnd.copy( getMouseOnScreen( x, y ) );
	    //_panStart.copy( _panEnd );
	    this.set2=false;
	    this.set1=false;
	    break;
	}
	_state = STATE.NONE;
	_this.dispatchEvent( endEvent );
    }
    function contextmenu( event ) {
	event.preventDefault();
    }
    this.dispose = function() {
	this.domElement.removeEventListener( 'contextmenu', contextmenu, false );
	this.domElement.removeEventListener( 'mousedown', mousedown, false );
	this.domElement.removeEventListener( 'mousewheel', mousewheel, false );
	this.domElement.removeEventListener( 'MozMousePixelScroll', mousewheel, false ); // firefox
	this.domElement.removeEventListener( 'touchstart', touchstart, false );
	this.domElement.removeEventListener( 'touchend', touchend, false );
	this.domElement.removeEventListener( 'touchmove', touchmove, false );
	document.removeEventListener( 'mousemove', mousemove, false );
	document.removeEventListener( 'mouseup', mouseup, false );
	window.removeEventListener( 'keydown', keydown, false );
	window.removeEventListener( 'keyup', keyup, false );
    }
    this.domElement.addEventListener( 'contextmenu', contextmenu, false );
    this.domElement.addEventListener( 'mousedown', mousedown, false );
    this.domElement.addEventListener( 'mousewheel', mousewheel, false );
    this.domElement.addEventListener( 'MozMousePixelScroll', mousewheel, false ); // firefox
    this.domElement.addEventListener( 'touchstart', touchstart, false );
    this.domElement.addEventListener( 'touchend', touchend, false );
    this.domElement.addEventListener( 'touchmove', touchmove, false );
    window.addEventListener( 'keydown', keydown, false );
    window.addEventListener( 'keyup', keyup, false );
    this.handleResize();
    // force an update at start
    this.update();
};
Controls.prototype = Object.create( EventDispatcher.prototype );
Controls.prototype.constructor = Controls;
