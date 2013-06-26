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
    browser.addEventListener("pageshow", function () { ableButtons(); }, false);
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

// Determine disability state of the next/prev buttons.
ableButtons = function () {
    var browser = document.getElementById('browser');

    var prevButton = document.getElementById('prev-button');
    prevButton.disabled = !browser.canGoBack;
    if (browser.canGoBack) {
        prevButton.image = "chrome://histpl/skin/icons/left-arrow/left.png";
    } else {
        prevButton.image = "chrome://histpl/skin/icons/left-arrow/left_grey.png";
    }

    var nextButton = document.getElementById('next-button');
    nextButton.disabled = !browser.canGoForward;
    if (browser.canGoForward) {
        nextButton.image = "chrome://histpl/skin/icons/right-arrow/right.png";
    } else {
        nextButton.image = "chrome://histpl/skin/icons/right-arrow/right_grey.png";
    }
}

// Tooltip.
function FillInHTMLTooltip(tipElement) {
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

// Open preferences tab.
function openPrefs() {
    var win = Components.classes['@mozilla.org/appshell/window-mediator;1']
                    .getService(Components.interfaces.nsIWindowMediator)
                    .getMostRecentWindow('navigator:browser');
    win.openUILinkIn('chrome://histpl/content/options.xul', 'tab');
}

// Code to run when closing the window.
function onClose() {
    var prefs = Components.classes["@mozilla.org/preferences-service;1"]
                    .getService(Components.interfaces.nsIPrefService)
                    .getBranch("extensions.histpl.");
    prefs.setBoolPref("winon", false);
    window.close();
}

// Initialization.
var key = window.arguments[0];
window.addEventListener("load", function () { hwin.onLoad(key); }, false);
window.addEventListener("close", function () { hwin.onClose(); }, false);
