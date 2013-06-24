var hwin = {
  onLoad: function(x) {
    this.initialized = true;

    var prefService = Components.classes["@mozilla.org/preferences-service;1"]
                                .getService(Components.interfaces.nsIPrefBranch);
    var uriBase = prefService.getPref("extensions.histpl.servicepref");

    // var newUri = "".concat("http://glass.ipipan.waw.pl:10019/ext?query=", encodeURIComponent(x));
    var newUri = "".concat(uriBase, "/ext?query=", encodeURIComponent(x));
    var browser = document.getElementById('browser');
    browser.loadURI(newUri);

    // document.getElementById('prev-button').disabled = !browser.canGoBack;
    // document.getElementById('next-button').disabled = !browser.canGoForward;
  }
};

var key = window.arguments[0];
window.addEventListener("load", function () { hwin.onLoad(key); }, false);

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
