'use strict';

require('./index.html');
require('./main.scss');
const allSoundFiles = require.context(".", true, /\.mp3$/);
let audioFiles = []
let filePlaying = null;

allSoundFiles.keys().forEach(key => {

    const audioElement = new Audio(key);

    audioElement.onended = () => {
        filePlaying = null;
        app.ports.playEnded.send(null);
        console.log("audio ended on card " + key);
    }

    audioFiles.push({
        file: key,
        audio: audioElement
    })
})

console.log("Audio files:", audioFiles)

var Elm = require('./Main.elm').Elm;

var app = Elm.Main.init({
    node: document.getElementById('main'),
    flags: audioFiles
});



//stop any audio playing
function stopAudio(file) {

    if (filePlaying !== null) {
        console.log("Reseting audio currently playing");
        filePlaying.audio.pause();
        filePlaying.audio.load();
        return true;
    } else {
        return false;
    }
}

app.ports.playFile.subscribe(function(file) {

    //stop anything else if it is playing
    stopAudio();

    let selection = audioFiles.find(element => element.file === file)

    if (typeof selection === "undefined") {
        console.log("Error: file not found");
        return;
    }

    console.log("Now it's time to play:" + selection.file)
    filePlaying = selection;
    selection.audio.play();
});


app.ports.stopFile.subscribe(function() {

    console.log("Got the message to any playing file: ");

    if (stopAudio()) {
        console.log("audio ended on card " + filePlaying.file);
        filePlaying = null;
        app.ports.playEnded.send(null);

    }

});