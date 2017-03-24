var binstall = require("binstall");
var path = require("path");
var fs = require("fs");
var packageInfo = require(path.join(__dirname, "package.json"));

// 'arm', 'ia32', or 'x64'.
var arch = process.arch;
// 'darwin', 'freebsd', 'linux', 'sunos' or 'win32'
var operatingSystem = process.platform;

var filename = operatingSystem + "-" + arch + ".tar.gz";
var version = "0.18.3";
var url = "https://dl.bintray.com/elmlang/elm-test/" + version + "/" + filename;

var binariesDir = path.join(__dirname, "binaries");
var packageInfo = require(path.join(__dirname, "package.json"));
var binaryExtension = process.platform === "win32" ? ".exe" : "";
var executablePaths = Object.keys(packageInfo.bin).map(function(executable) {
  return path.join(binariesDir, executable + binaryExtension);
});

binstall(url, {path: binariesDir, strip: 1}, {verbose: true, verify: executablePaths})
  .then(function(successMessage) {
    console.log(successMessage);
  }, function(errorMessage) {
    console.error(errorMessage);
    process.exit(1);
  });
