var ws = new WebSocket('ws://localhost:12345/timer');
var time = document.getElementById('time');

ws.onmessage = function(e) {
    var d = e.data;
    if ( d == "start" ) {
	document.location = "http://localhost/ranking";
    } else {
	time.textContent = d;
    }
};
