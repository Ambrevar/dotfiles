//!javascript

Signal.connect("changeMode", function(wv, mode) {
    if (mode == Modes.NormalMode)
    {
        wv.focusedFrame.inject("document.activeElement.blur();");
    }
});
