var hwin = {
  onLoad: function(x) {
    this.initialized = true;
    var prefs = Components.classes["@mozilla.org/preferences-service;1"]
                        .getService(Components.interfaces.nsIPrefService)
                        .getBranch("extensions.histpl.");
    // Setting proper size attributes. 
    if (!prefs.getBoolPref("winon")) {
        var width  = prefs.getIntPref("widthpref");
        var height = prefs.getIntPref("heightpref");
        window.resizeTo(width, height);
    }
//     // Setting transliter checkbox.
//     var trbox = document.getElementById('transliter');
//     trbox.checked = prefs.getBoolPref('transliterpref');
    // Set info, that the extension window is on.
    prefs.setBoolPref("winon", true);
    // Add browser pageshow event.
    var browser = document.getElementById('browser');
    browser.addEventListener("pageshow", function () { ableButtons(); }, false);
    search(x);
  }
};

// Set the browser element to the particular query text.
function search(x) {
    var prefs = Components.classes["@mozilla.org/preferences-service;1"]
                        .getService(Components.interfaces.nsIPrefService)
                        .getBranch("extensions.histpl.");
    // Do we perform transliteration?
    var trFlag = "0";
    if (prefs.getBoolPref("transliterpref")) {
        trFlag = "1";
    }
    // URI to the old Polish service.
    var uri = "".concat(
            prefs.getCharPref("servicepref"),
            "/ext?transliter=", trFlag,
            "&query=", encodeURIComponent(x));
    var browser = document.getElementById('browser');
    browser.loadURI(uri);
}


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

// // When state of the transliteration checkbox changes.
// function transliterChange() {
//     var prefs = Components.classes["@mozilla.org/preferences-service;1"]
//                     .getService(Components.interfaces.nsIPrefService)
//                     .getBranch("extensions.histpl.");
//     var trbox = document.getElementById('transliter');
//     prefs.setBoolPref('transliterpref', trbox.checked);
// }

// Custom search.
function findText() {
    var text = document.getElementById('find-text').value;
    search(text);
}

// Initialization.
var key = window.arguments[0];
window.addEventListener("load", function () { hwin.onLoad(key); }, false);
