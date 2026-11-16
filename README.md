# quil-party
A creative coding sketchbook using Clojure + Quil (Processing) for generative art and visual experimentation.

## the sketches

<style>
img { display:block; width:1000px; height:250px; object-fit:cover; margin:0 auto 0.5rem; }
figure { margin:0 0 1.5rem; }
figcaption { text-align:center; }
</style>

<figure>
  <img src="./img/readme_imgs/crop-circles-cropped.jpg" alt="Crop Circles" width="1000" height="250">
  <figcaption><strong>Crop Circles</strong> 2025</figcaption>
</figure>

<figure>
  <img src="./img/readme_imgs/fern.jpg" alt="Fern" width="1000" height="250">
  <figcaption><strong>Fern</strong> 2025</figcaption>
</figure>

<figure>
  <img src="./img/readme_imgs/ginkgo.png" alt="Ginkgo" width="1000" height="250">
  <figcaption><strong>Ginkgo</strong> 2025</figcaption>
</figure>

<figure>
  <img src="./img/readme_imgs/waveforms.jpg" alt="Waveforms" width="1000" height="250">
  <figcaption><strong>Waveforms</strong> 2025</figcaption>
</figure>

## Usage
### New sketch generation
Use the generation script: `bin/new.sh --name "3d fern"`

### Playing with existing sketches
- Jack-in using Leiningen
- sketches live in `./src/quil_party`
- Evaluate (q/defsketch) to open visualizer (shift ctrl opt)
- Mess around: update & eval code called by the draw loop, save svgs when it looks cool, etc

In order to call the functions directly in the REPL, they need to be wrapped with the current sketch:
```
(quil.applet/with-applet quil-party.basic-svg
    (quil.core/no-loop)) ;; pause the draw loop

(quil.applet/with-applet quil-party.basic-svg
    (quil.core/start-loop)) ;; restart the loop
```

## troubleshooting
### axidraw isn't drawing anything
- make sure you add a `stroke="black"` attribute on the top level `<g>` in your svg
- or figure out how to get q/create-graphics to do this


