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

app.ports.playFile.subscribe(function(file) {
    console.log("Now it's time to play:" + file)
    const audio = new Audio(file);
    audio.play();
    audio.onended = () => {
        app.ports.playEnded.send(null);
        console.log("audio ended on card " + file);
    }
});