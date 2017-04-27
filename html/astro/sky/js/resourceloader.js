ResourceLoader=	function() {
    this.allLoaders = {};
    this.currentScenarioLoaders=[];
}

ResourceLoader.prototype = {
    constructor : ResourceLoader,
    getCached : function(id){
	if(this.allLoaders[id]){
	    this.currentScenarioLoaders.push(this.allLoaders[id]);
	    return this.allLoaders[id];
	}
    },
    reset : function(){
	this.currentScenarioLoaders = [];
    },
    loadJSON : function(dataSrc){
	var onDataLoaded = this.getCached(dataSrc);
	if(onDataLoaded) return onDataLoaded;
	onDataLoaded = $.ajax({
	    url : dataSrc,
	    dataType : 'json'
	});
	this.allLoaders[dataSrc] = onDataLoaded.promise();
	this.currentScenarioLoaders.push(onDataLoaded.promise());
	return onDataLoaded;
    }
};
