'use strict';

let main = require('../src/Main.purs').main;

if (module.hot) {
  module.hot.accept('../src/Main.purs', _ => Location.reload());
}
main();

console.log('app starting');
