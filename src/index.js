var demo = require("../lib/js/src/demo.bs.js");
var lustreCodegen = require("../lib/js/src/lustreCodegen.bs.js");

var parent = document.getElementById("test-svg-helper");
var tools = document.getElementById("test-svg-tools");

/* v is label * info list. info = test | Bool | Int | XXX
 *
 *  label["text"] : [ ????????? ]
 *  label["BOOL|Int|XXX"] : [ \/
 *                          "BOOL"
 *                          "INT" <--- selected
 *                          "XXX"
 *                          ]
 *  args[0] = "?????"
 *  args[1] = "1"
 */

var prompt = function (v, f) {
  var args = [];
  for (var i = 0; i < v.length; i++) {
    var x = window.prompt(v[i].label + "[" + v[i].info + "]", "x");
    if (x != null) {
      args[i] = x;
    } else {
      return;
    }
  }
  f(args);
}

window.codegen = function() {
  let env = demo.get_demo_env();
  let ast = lustreCodegen.codegen_from_ctx(env);
  console.log(ast)
}

if (parent) {
  var env = demo.get_demo_env();
  demo.display_env_as_cfg(prompt, demo.default_parse, tools, parent, env);
}

module.exports = {
  demo: demo,
}
