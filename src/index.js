console.log("test");
var demo = require ("../lib/js/src/demo.bs.js");
var parent = document.getElementById("test-svg-helper");
var tools = document.getElementById("test-svg-tools");
if (parent) {
  demo.demo_cfg(parent);
  demo.demo_component(tools);
  /*demo.demo_tools(tools) */
}

module.exports = {
  demo: demo,
}
