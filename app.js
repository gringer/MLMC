var location = require('iprequest.js');
location(function(location){
    if(location){
	// process based on location
    } else {
	console.log('unable to get location');
    }
}
