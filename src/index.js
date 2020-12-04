console.log("test");
var demo = require ("../lib/js/src/demo.bs.js");
var parent = document.getElementById("test-svg-helper");
var tools = document.getElementById("test-svg-tools");
if (parent) {
  var env = demo.get_demo_env ();
  demo.display_env_as_cfg(tools, parent, env);
}

module.exports = {
  demo: demo,
}
