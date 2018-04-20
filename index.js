var binwrap = require("binwrap");
var path = require("path");

// TODO do this the way elm-lang/elm-platform does this, once we're ready
// to switch to GitHub Releases.
var root = "https://dl.bintray.com/elmlang/elm-test/0.18.3/";

module.exports = binwrap({
  binaries: ["elm-interface-to-json"],
  urls: {
    "darwin-x64": root + "darwin-x64.tar.gz",
    "linux-x64": root + "linux-x64.tar.gz",
  }
});

