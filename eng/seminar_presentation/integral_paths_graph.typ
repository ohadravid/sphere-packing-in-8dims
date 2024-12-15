
#import "@preview/cetz:0.3.1"

#show math.equation: block.with(fill: white, inset: 1pt)

#let integral-path-graph = [
  #cetz.canvas(length: 2.5cm, debug: false, {
    import cetz.draw: *
  
    set-style(
      mark: (fill: black, scale: 2),
      stroke: (thickness: 0.4pt, cap: "round"),
      angle: (
        radius: 0.3,
        label-radius: .22,
        fill: green.lighten(80%),
        stroke: (paint: green.darken(50%))
      ),
      content: (padding: 1pt)
    )
  
    grid((-1.5, -0.5), (1.4, 1.8), step: 0.5, stroke: gray + 0.2pt)
  
  
    line((-1.5, 0), (1.5, 0), mark: (end: "stealth"))
    content((), $ "" $, anchor: "west")
    line((0, -0.5), (0, 2), mark: (end: "stealth"))
    content((), $ "" $, anchor: "south")
  
    for (x, ct) in ((-1, $ -1 $), (1, $ 1 $)) {
      line((x, 3pt), (x, -3pt))
      content((), anchor: "north", ct)
    }
  
    for (y, ct) in ((1, $ i $),) {
      line((3pt, y), (-3pt, y))
      content((), anchor: "east", ct)
    }
  
    
    set-style(stroke: (thickness: 1.2pt))
    
    line((-1,0), (-0.5, 1.8), name: "m1toinf", mark: (end: "straight"))
    content(("m1toinf.start", 50%, "m1toinf.end"), text(red)[$ (-1, i infinity) $])
  
    line((-1,0), (0, 1), name: "m1toi", stroke: blue, mark: (end: "straight"))
    content(("m1toi.start", 50%, "m1toi.end"), text(green)[$ (-1, i) $])
    
    line((1,0), (0.5, 1.8), name: "p1toinf", mark: (end: "straight"))
    content(("p1toinf.start", 50%, "p1toinf.end"), text(red)[$ (1, i infinity) $])
  
    line((1,0), (0, 1), name: "p1toi", stroke: blue, mark: (end: "straight"))
    content(("p1toi.start", 50%, "p1toi.end"), text(green)[$ (1, i) $])
    
    line((0,1), (0, 1.8), name: "itoinf", stroke: blue, mark: (end: "straight"))
    content(("itoinf.start", 50%, "itoinf.end"), text(green)[$ (i, i infinity) $])
    
    line((0,0), (0, 1), name: "p0toi", stroke: blue, mark: (end: "straight"))
    content(("p0toi.start", 50%, "p0toi.end"), text(green)[$ (0, i) $])
  })
]
