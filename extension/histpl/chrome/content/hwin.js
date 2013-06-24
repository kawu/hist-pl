var hwin = {
  onLoad: function(x) {
    this.initialized = true;

    // Setting proper address. 
    var prefs = Components.classes["@mozilla.org/preferences-service;1"]
                        .getService(Components.interfaces.nsIPrefService)
                        .getBranch("extensions.histpl.");
    var uriBase = prefs.getCharPref("servicepref");
    var newUri = "".concat(uriBase, "/ext?query=", encodeURIComponent(x));
    var browser = document.getElementById('browser');
    browser.loadURI(newUri);

    // Setting proper size attributes. 
    if (!prefs.getBoolPref("winon")) {
        var width  = prefs.getIntPref("widthpref");
        var height = prefs.getIntPref("heightpref");
        window.resizeTo(width, height);
    }
    prefs.setBoolPref("winon", true);

    // document.getElementById('prev-button').disabled = !browser.canGoBack;
    // document.getElementById('next-button').disabled = !browser.canGoForward;
  }
};

// Move the browser backward.
hwinGoBack = function () {
    var browser = document.getElementById('browser');
    browser.goBack();
}

// Move the browser forward.
hwinGoForward = function () {
    var browser = document.getElementById('browser');
    browser.goForward();
}

var key = window.arguments[0];
window.addEventListener("load", function () { hwin.onLoad(key); }, false);
