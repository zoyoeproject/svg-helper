var demo = require ("../lib/js/src/demo.bs.js");
var parent = document.getElementById("test-svg-helper");
var tools = document.getElementById("test-svg-tools");
var prompt = function (v, f) {
  var args = [];
  for (var i=0; i<v.length; i++) {
    var x = window.prompt(v[i].label + "[" + v[i].info + "]", "x");
    if (x != null) {
      args[i] = x;
    } else {
      return;
    }
  }
  f(args);
}
if (parent) {
  var env = demo.get_demo_env ();
  demo.display_env_as_cfg(prompt, tools, parent, env);
}

module.exports = {
  demo: demo,
}
