//!javascript

// Prevents previously-opened tabs from reloading all at once after a restart.

execute("set load-on-focus true");

var sigId = signals.connect("navigation", function(wv) {
    if (wv == tabs.current)
    {
        execute("set load-on-focus false");
        signals.disconnect(sigId);
    }
});