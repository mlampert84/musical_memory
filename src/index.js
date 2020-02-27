'use strict';

require('./index.html');
require('./main.scss');
const allSoundFiles = require.context(".", true, /\.mp3$/);
let audioFiles = []

allSoundFiles.keys().forEach(key =>
    audioFiles.push(key)
)
console.log("Audio files:", audioFiles)

var Elm = require('./Main.elm').Elm;

var app = Elm.Main.init({
    node: document.getElementById('main'),
    flags: audioFiles
});