# svgexample

Shows the two routes an SVG document can take onto a `TpvCanvas`, driven entirely from a POCA
script. The picture is the tiger that vector graphics renderers have been shown off with since
the PostScript days: 240 paths, all of them filled with a plain color and stroked, which means
the whole document goes through the vector path shader.

## Running it

Build `src/svgexample.lpi` with Lazarus or `src/svgexample.dpr` with FPC directly. The script in
`assets/poca/application.poca` is reloaded with F5, so the drawing code can be changed while the
program keeps running.

## Keys

| Key | What it does |
|---|---|
| `G` | switch between the vector path shader and the software texture |
| `C` | switch how the shader works out how much of a pixel an outline covers |
| `V` | switch how many samples the supersampling coverage mode takes |
| `M` | switch what is done about the seam between two shapes that share an edge |
| `R` | turn the rotation on and off |
| `W` / `S` | zoom in and out, as does the mouse wheel |
| `N` | record the shapes again at the size they are currently drawn at |
| `B` | outline the area the recorded shapes cover |
| `Space` | back to the starting view |
| `F5` | reload the script |

## Command line

| Option | What it does |
|---|---|
| `--screenshot <file>` | write the frame buffer to that PNG file and end the program |
| `--screenshot-delay <n>` | how many frames to let pass first, 30 by default |
| `--screenshot-keep-running` | take the picture but carry on instead of ending |

`--screenshot` is what makes an unattended run useful: it ends by itself, so

```
./svgexample --screenshot /tmp/tiger.png
```

leaves a picture behind and needs nobody to close a window. It goes through
`TpvFramebufferCapture` and `SavePNGImageAsFile`, so it captures what was really presented rather
than what the window manager has on screen.

## What to look at

Zoom well past 100 percent and switch between the two routes with `G`. The texture route stretches
the pixels it rasterized once, while the shader route resolves every outline per pixel and stays
sharp however far it is scaled, without anything being rasterized again.

The whiskers are the other thing to look at, and `C` is what changes them. They are stroked at
0.172 units, so at a zoom of 100 percent they are a sixth of a pixel wide, and the `Estimate` mode
the shader starts with draws them solid white and ragged, because the distance to the nearest edge
alone cannot tell that there is a second edge a sixth of a pixel behind the first.
`AnalyticalAccumulation` adds up the area each edge cuts off the pixel instead and gets them right,
and `Supersampling` asks the estimate question at four places inside the pixel and gets close, with
`V` raising that to sixteen.

What does get coarse on the shader route at a big zoom are the **stroked** outlines, because the
stroker turned them into polygons at the scale they were recorded at. `N` records them again at
the current size and they snap back to being smooth, which is the one thing the resolution
independent route still trades away. The status line shows the zoom the shapes were last recorded
at next to the current one.

## The API

The `VectorGraphics` section of `docs/pocascriptapi.adoc` documents the namespace, its three object
types and the two canvas functions this example uses.
