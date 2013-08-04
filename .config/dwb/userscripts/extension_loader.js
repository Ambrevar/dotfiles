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
