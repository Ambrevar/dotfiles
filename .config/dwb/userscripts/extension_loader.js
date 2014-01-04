//!javascript
//<adblock_subscriptions___SCRIPT
extensions.load("adblock_subscriptions", {
//<adblock_subscriptions___CONFIG

// Shortcut to subscribe to a filterlist
scSubscribe : null,
// Command to subscribe to a filterlist
cmdSubscribe : "adblock_subscribe",

// Shortcut to unsubscribe from a filterlist
scUnsubscribe : null,

// Command to unsubscribe from a filterlist
cmdUnsubscribe : "adblock_unsubscribe",

// Shortcut to update subscriptions and reload filter rules
// Note that dwb will also update all subscriptions on startup
scUpdate : null,

// Command to update subscriptions and reload filter rules
// Note that dwb will also update all subscriptions on startup
cmdUpdate : "adblock_update",

// Path to the filterlist directory, will be created if it doesn't exist.
filterListDir : "~/.config/dwb/adblock_lists"
//>adblock_subscriptions___CONFIG
});
//>adblock_subscriptions___SCRIPT
//<navtools___SCRIPT
extensions.load("navtools", {
//<navtools___CONFIG
// Shortcut for navigating to the next page
forwardBind : "]]",

// Shortcut for navigating to the previous page
backBind : "[[",

// Go up one directory, e.g. from http://www.example.com/foo/bar to
// http://www.example.com/foo
updirBind : "gu",

// Go to top directory, e.g. from http://www.example.com/foo/bar to
// http://www.example.com
topdirBind : "gU",

// Patterns to match against when searching for "next"-links
nextPatterns : "next,more,>,\u2192,\xbb,\u226b,>>",

// Patterns to match against when searching for "previous"-links
previousPatterns : "prev,previous,back,<,\u2190,\xab,\u226a,<<"

//>navtools___CONFIG
});
//>navtools___SCRIPT
//<autoquvi___SCRIPT
/*<autoquvi___DISABLED
extensions.load("autoquvi", {
//<autoquvi___CONFIG
  // The quvi command
  quvi      : "quvi",

  // External player command
  player    : "mplayer %u",

  // Whether to automatically play videos when quvi find a playable
  // video
  autoPlay  : true,

  // Whether to choose the quality before quvi starts
  chooseQuality : true,

  // A shortcut that spawns quvi for the current website
  shortcut  : "",

  // A command that spawns quvi for the current website
  command  : "autoquvi"

//>autoquvi___CONFIG
});
autoquvi___DISABLED>*/
//>autoquvi___SCRIPT
//<downloadhandler___SCRIPT
extensions.load("downloadhandler", {
//<downloadhandler___CONFIG
   handler : [
     // Each handler must have 2 or 3 properties:
     //
     // command : command to execute, must contain %f which will be replaced with
     //           the filepath, this property is mandatory
     //
     // extension : a filename extension, optional
     //
     // mimeType  : a mimetype, optional
     //

     // { command : "xpdf %f", mimeType : "application/pdf" }
     // { command : "xdvi %f", extension : "dvi" }

   ]
//>downloadhandler___CONFIG
});
//>downloadhandler___SCRIPT
//<contenthandler___SCRIPT
extensions.load("contenthandler", {
//<contenthandler___CONFIG
  // The handler can either be a string or a function, if it is a string
  // %u will be replaced with the uri of the request, if the handler is a
  // function the first parameter of the function will be the uri and the
  // function must return the command to execute.

  // Handle requests based on filename extension
  extension : {
    // "torrent" : "xterm -e 'aria2 %u'",
    // "pdf" : "xterm -e 'wget %u --directory-prefix=~/mypdfs'"
  },

  // Handle requests based on URI scheme
  uriScheme : {
      ftp : function(uri) {
         if (uri[uri.length-1] == "/")
             return "xterm -e 'ncftp " + uri + "'";
         else
             return "xterm -e 'ncftpget " + uri + "'";
      },

      // We need to use 'sh -c' since we need to use a pipe so that xclip can
      // read from stdin
      mailto : function(uri) {
          uri=uri.replace(/mailto:/, "")
          uri=uri.replace(/\?/, " ")
          return "sh -c ' echo " + uri + " | xclip -i'";
      },

      // 'trc' is a wrapper script that will launch transmission-daemon if not
      // already started.
      //   magnet: "transmission-remote -a '%u'"
      magnet: "trc -a '%u'"
  },

  // Handle requests based on MIME type
  mimeType : {
    // "application/pdf" : "xterm -e 'wget %u --directory-prefix=~/mypdfs'"
  }
//>contenthandler___CONFIG

});
//>contenthandler___SCRIPT
