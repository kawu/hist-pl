var hwin = {
  onLoad: function(x) {
    this.initialized = true;
    var newUri = "".concat("http://glass.ipipan.waw.pl:10019/ext?query=", encodeURIComponent(x));
    var browser = document.getElementById('browser');
    browser.loadURI(newUri);
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
