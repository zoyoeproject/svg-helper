console.log("test");
var demo = require ("../lib/js/src/demo.bs.js");
var parent = document.getElementById("test-svg-helper");
if (parent) {
  demo.demo_cfg(parent)
}

module.exports = {
  demo: demo,
}
