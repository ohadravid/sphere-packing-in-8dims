

#align(center)[  
  #import "@preview/cetz:0.2.2": canvas, plot, draw
  
  #let style = (stroke: black, fill: rgb(0, 0, 200, 75))
  
  #canvas(length: 1cm, {
    plot.plot(
      name: "A-inf-6-plot",
      size: (8, 8),
      x-domain: (0.8, 1.5),
      {
        plot.add(
          style: style,
          domain: (0.8, 1.5), 
          t => (
            return -72 / calc.pow(calc.pi, 2) * calc.pow(calc.e, 2 * calc.pi * t) - 23328 / calc.pow(calc.pi, 2) + 
184320 / calc.pow(calc.pi, 2) * calc.pow(calc.e, -calc.pi * t) - 5194368 / calc.pow(calc.pi, 2) * calc.pow(calc.e, -2 * calc.pi * t) + 
22560768 / calc.pow(calc.pi, 2) * calc.pow(calc.e, -3 * calc.pi * t) - 250583040 / calc.pow(calc.pi, 2) * calc.pow(calc.e, -4 * calc.pi * t) + 
869916672 / calc.pow(calc.pi, 2) * calc.pow(calc.e, -5 * calc.pi * t) + 
t * (8640 / calc.pi + 2436480 / calc.pi * calc.pow(calc.e, -2 * calc.pi * t) + 113011200 / calc.pi * calc.pow(calc.e, -4 * calc.pi * t)) - 
t * t * (518400 * calc.pow(calc.e, -2 * calc.pi * t) + 31104000 * calc.pow(calc.e, -4 * calc.pi * t))
          ),
        )
        
      }
    )
    draw.content(((0,3), "-|", "A-inf-6-plot"), $f_0$)
  })

  #canvas(length: 1cm, {
    plot.plot(
      name: "f-hat-plot",
      size: (8, 2),
      axis-style: "left",
      x-tick-step: none,
      x-ticks: ((0, 0), (calc.sqrt(2), $sqrt(2)$), (calc.sqrt(4), $sqrt(4)$), (calc.sqrt(6), $sqrt(6)$)),
      y-tick-step: none,
      {
        plot.add(
          style: style,
          domain: (1, calc.sqrt(7)), 
          x => (1.0 / (x + 0.05)) * calc.pow(calc.sin(0.5 * calc.pow(x, 2) * calc.pi), 2)
        )
      }
    )
    draw.content(((0,2.5), "-|", "f-hat-plot"), $hat(f)_0$)
  })  
  
]


#pagebreak()
