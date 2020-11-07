console.log("test");
var demo = require ("../lib/js/src/demo.bs.js");
var cfg = demo.demo_cfg();
document.getElementById("test").innerHTML = cfg;
console.log(cfg);
