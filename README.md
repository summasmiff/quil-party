# quil-party
Doodling with the [Quil library](http://quil.info/) in Clojure

## Usage
- Jack-in using Leiningen
- sketches live in `./src/quil_party`
- Evaluate (q/defsketch) to open visualizer
- Mess around

## troubleshooting
### axidraw isn't drawing anything
- make sure you add a `stroke="black"` attribute on the top level `<g>` in your svg
- or figure out how to get q/create-graphics to do this

<img src="./img/waveforms.jpg">

**Waveforms** 2025
