function openRequestedPopup(x) {
    // var winFeats = "menubar=no,location=no,resizable=yes,scrollbars=yes,status=no,toolbar=yes";
    // var winFeats = "resizable=yes,scrollbars=yes,centerscreen=yes,chrome=yes,height=480,width=320";
    // var address = "".concat("http://localhost:8000/ext?query=", x);

    var winFeats = "resizable=yes,scrollbars=yes,centerscreen=yes,chrome=yes,dependent=yes";
    var address = "chrome://histpl/content/hwin.xul";

    window.openDialog(address, "HistPL", winFeats, x);
}

var histpl = {
  onLoad: function() {
    // initialization code
    this.initialized = true;
    this.strings = document.getElementById("histpl-strings");
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

window.addEventListener("load", function () { histpl.onLoad(); }, false);


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

window.addEventListener("load", function () { histpl.onFirefoxLoad(); }, false);
