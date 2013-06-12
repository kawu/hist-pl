var hwin = {
  onLoad: function(x) {
    this.initialized = true;
    var newUri = "".concat("http://localhost:8000/ext?query=", x);
    var browser = document.getElementById('browser');
    browser.loadURI(newUri);
  }
};

var key = window.arguments[0];
window.addEventListener("load", function () { hwin.onLoad(key); }, false);
