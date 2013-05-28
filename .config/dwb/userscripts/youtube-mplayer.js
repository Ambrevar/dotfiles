//!javascript 

// Opens YouTube videos with mplayer.

var regex = new RegExp("http(.*)://www.youtube.com/watch\\?(.*&)*v=.*");

signals.connect("navigation", function (wv, frame, request) {
    if (wv.mainFrame == frame && regex.test(request.uri)) 
        system.spawn("sh -c 'mplayer \"$(youtube-dl -g " + request.uri + ")\"'");
    return false;
});
