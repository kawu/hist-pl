function openRequestedPopup(x) {
    // var winFeats = "menubar=no,location=no,resizable=yes,scrollbars=yes,status=no,toolbar=yes";
    // var winFeats = "resizable=yes,scrollbars=yes,centerscreen=yes,chrome=yes,height=480,width=320";
    var winFeats = "resizable=yes,scrollbars=yes,centerscreen=yes,chrome=yes,dependent=yes,titlebar=no";
    var address = "chrome://histpl/content/hwin.xul";
    window.openDialog(address, "HistPL", winFeats, x);
}

var histpl = {
  onLoad: function() {
    // initialization code
    this.initialized = true;
    this.strings = document.getElementById("histpl-strings");

    var prefs = Components.classes["@mozilla.org/preferences-service;1"]
                        .getService(Components.interfaces.nsIPrefService)
                        .getBranch("extensions.histpl.");
    prefs.setBoolPref("winon", false);
  },

  onMenuItemCommand: function(e) {
    var xpc = new XPCNativeWrapper(document.commandDispatcher.focusedWindow,"document","getSelection()");
    var sel = xpc.getSelection.call(xpc);
    // sel=dtip_jsObject.trimWhitespace(sel);

//     var promptService = Components.classes["@mozilla.org/embedcomp/prompt-service;1"]
//                                   .getService(Components.interfaces.nsIPromptService);
//     promptService.alert(window, this.strings.getString("helloMessageTitle"),
//                                 sel.toString());
//                                 // this.strings.getString("helloMessage"));
    openRequestedPopup(sel.toString());
  },

  onToolbarButtonCommand: function(e) {
    // just reuse the function above.  you can change this, obviously!
    histpl.onMenuItemCommand(e);
  }
};

histpl.onFirefoxLoad = function(event) {
  document.getElementById("contentAreaContextMenu")
          .addEventListener("popupshowing", function (e) {
    histpl.showFirefoxContextMenu(e);
  }, false);
};

histpl.showFirefoxContextMenu = function(event) {
  // show or hide the menuitem based on what the context menu is on
  document.getElementById("context-histpl").hidden = gContextMenu.onImage;
};

window.addEventListener("load", function () { histpl.onLoad(); }, false);
window.addEventListener("load", function () { histpl.onFirefoxLoad(); }, false);
