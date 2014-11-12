//!javascript

// Opens YouTube videos with mplayer.

var regex = new RegExp("http(.*)://www.youtube.com/watch\\?(.*&)*v=.*");

Signal.connect("navigation", function (wv, frame, request) {
	if (wv.mainFrame == frame && regex.test(request.uri))
		system.spawn("sh -c 'mpv \"$(youtube-dl -g " + request.uri + ")\"'");
	return false;
});
