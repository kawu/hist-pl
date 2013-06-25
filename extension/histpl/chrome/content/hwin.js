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


// Tooltip.
function FillInHTMLTooltip(tipElement)
{
    const XLinkNS = "http://www.w3.org/1999/xlink";
    var retVal = false;
    var titleText = null;
    var XLinkTitleText = null;

    if (tipElement.namespaceURI == "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul")
        return retVal;

    while (!titleText && !XLinkTitleText && tipElement) {
        if (tipElement.nodeType == Node.ELEMENT_NODE) {
            titleText = tipElement.getAttribute("title");
            XLinkTitleText = tipElement.getAttributeNS(XLinkNS, "title");
        }
        tipElement = tipElement.parentNode;
    }
    
    var texts = [titleText, XLinkTitleText];
    var tipNode = document.getElementById("aHTMLTooltip");
    
    for (var i = 0; i < texts.length; ++i) {
        var t = texts[i];
        if (t && t.search(/\S/) >= 0) {
            tipNode.setAttribute("label", t);
            retVal = true;
        }
    }
    
    return retVal;
}
