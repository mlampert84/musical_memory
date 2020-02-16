'use strict';

require('./index.html');
require('./main.scss');
var Elm = require('./Main.elm').Elm;

var app = Elm.Main.init({
    node: document.getElementById('main')
});