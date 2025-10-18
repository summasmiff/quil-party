# quil-party
Doodling with Clojure

## Usage
- Jack-in using Leiningen
- Evaluate whole file / (q/defsketch) to open visualizer
- Mess around

## troubleshooting
### axidraw isn't drawing anything
- make sure you add a `stroke="black"` attribute on the top level `<g>` in your svg
- or figure out how to get q/create-graphics to do this
