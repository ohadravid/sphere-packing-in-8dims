#let Pkg = math.cal([P])

#let p1 = [
  #import "@preview/cetz:0.3.1"
  
  #show math.equation: block.with(fill: white, inset: 1pt)
  
  #cetz.canvas(length: 0.4cm, debug: false, {
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
  
    grid((-1, -1), (5.9, 5.9), step: 0.5, stroke: gray + 0.2pt, name: "grid")
    content("grid.south", anchor: "north", padding: 9pt, $Pkg_1$)
  
    for x in range(0, 8, step: 4) {
      for y in range(0, 8, step: 4) {
        circle((x,y), radius: 0.5, stroke: black)
      }
    }
    
    line((-1, 0), (7, 0), mark: (end: "stealth"))
    content((), $ "x" $, anchor: "west")
    line((0, -1), (0, 7), mark: (end: "stealth"))
    content((), $ "y" $, anchor: "south")
  
    for (x, ct) in ((1, $ 1 $),(2, $ 2 $),(3, $ 3 $),(4, $ 4 $),(5, $ 5 $),) {
      line((x, 3pt), (x, -3pt))
      content((), anchor: "north", ct)
    }
  
    for (y, ct) in ((1, $ 1 $),(2, $ 2 $),(3, $ 3 $),(4, $ 4 $),(5, $ 5 $),) {
      line((3pt, y), (-3pt, y))
      content((), anchor: "east", ct)
    }
  })
]

#let p2 = [
  #import "@preview/cetz:0.3.1"
  
  #show math.equation: block.with(fill: white, inset: 1pt)
  
  #cetz.canvas(length: 0.4cm, debug: false, {
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
  
    grid((-1, -1), (5.9, 5.9), step: 0.5, stroke: gray + 0.2pt, name: "grid")
    content("grid.south", anchor: "north", padding: 9pt, $Pkg_2$)

    for x in range(0, 8, step: 4) {
      for y in range(0, 8, step: 4) {
        circle((x,y), radius: 0.5, stroke: blue)
        circle((x+1,y+1), radius: 0.5, stroke: red)
      }
    }
    
    line((-1, 0), (7, 0), mark: (end: "stealth"))
    content((), $ "x" $, anchor: "west")
    line((0, -1), (0, 7), mark: (end: "stealth"))
    content((), $ "y" $, anchor: "south")
  
    for (x, ct) in ((1, $ 1 $),(2, $ 2 $),(3, $ 3 $),(4, $ 4 $),(5, $ 5 $),) {
      line((x, 3pt), (x, -3pt))
      content((), anchor: "north", ct)
    }
  
    for (y, ct) in ((1, $ 1 $),(2, $ 2 $),(3, $ 3 $),(4, $ 4 $),(5, $ 5 $),) {
      line((3pt, y), (-3pt, y))
      content((), anchor: "east", ct)
    }

  })
]


#let p1withsquares = [
  #import "@preview/cetz:0.3.1"
  
  #show math.equation: block.with(fill: white, inset: 1pt)
  
  #cetz.canvas(length: 0.4cm, debug: false, {
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
  
    grid((-1, -1), (5.9, 5.9), step: 0.5, stroke: gray + 0.2pt, name: "grid")
    
    // content("grid.south", anchor: "north", padding: 9pt, $Pkg_1$)
  
    for x in range(0, 8, step: 4) {
      for y in range(0, 8, step: 4) {
        if x != 0 or y != 0 {
          circle((x,y), radius: 0.5, stroke: black)
        }
      }
    }
    circle((0,0), radius: 0.5, stroke: green)
    rect((0,0),(4,4), stroke: green)
    
    line((4,0),(6,0), stroke: black)
    line((4,4),(6,4), stroke: black)
    line((0,4),(0, 6), stroke: black)
    line((4,4),(4, 6), stroke: black)
    
    line((-1, 0), (7, 0), mark: (end: "stealth"))
    content((), $ "x" $, anchor: "west")
    line((0, -1), (0, 7), mark: (end: "stealth"))
    content((), $ "y" $, anchor: "south")
  
    for (x, ct) in ((1, $ 1 $),(2, $ 2 $),(3, $ 3 $),(4, $ 4 $),(5, $ 5 $),) {
      line((x, 3pt), (x, -3pt))
      content((), anchor: "north", ct)
    }
  
    for (y, ct) in ((1, $ 1 $),(2, $ 2 $),(3, $ 3 $),(4, $ 4 $),(5, $ 5 $),) {
      line((3pt, y), (-3pt, y))
      content((), anchor: "east", ct)
    }
  })
]