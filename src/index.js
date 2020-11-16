console.log("test");
var demo = require ("../lib/js/src/demo.bs.js");
var parent = document.getElementById("test-svg-helper");
var tools = document.getElementById("test-svg-tools");
if (parent) {
  var context = demo.init_context(parent);
  demo.demo_cfg(context, parent);
  demo.demo_component(context, tools);
  /*demo.demo_tools(tools) */
}

module.exports = {
  demo: demo,
}
