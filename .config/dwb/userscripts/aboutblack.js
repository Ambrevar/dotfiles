#!javascript

Signal.connect("documentLoaded", function (wv) {
  if (wv.uri == "about:blank") {
    wv.inject("document.body.style.background = '#000';");
  }
});
