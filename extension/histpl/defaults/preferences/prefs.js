// Default preference values. These are accessible via the preferences system
// or via the optional chrome/content/options.xul preferences dialog.

// User preferences.
pref("extensions.histpl.widthpref", 480);
pref("extensions.histpl.heightpref", 480);
pref("extensions.histpl.servicepref", "localhost:8000");
pref("extensions.histpl.transliterpref", false);

// Internal persistent variable -- is the popup window opened?
pref("extensions.histpl.winon", false);

// https://developer.mozilla.org/en/Localizing_extension_descriptions
pref("extensions.histpl@waszczuk.kuba.pl.description", "chrome://histpl/locale/overlay.properties");
