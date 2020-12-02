console.log("test");
var demo = require ("../lib/js/src/demo.bs.js");
var parent = document.getElementById("test-svg-helper");
var tools = document.getElementById("test-svg-tools");
if (parent) {
  var context = demo.init_context_with_constr(parent);
  demo.demo_component(context, tools);
}

module.exports = {
  demo: demo,
}
