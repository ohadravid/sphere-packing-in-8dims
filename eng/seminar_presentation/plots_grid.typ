#import "@preview/cetz:0.3.1": canvas, draw
#import "@preview/cetz-plot:0.1.0": plot, chart

#let style = (stroke: black, fill: rgb(0, 0, 200, 75))

#let plots_grid = [
#grid(
  columns: (1fr, 1fr),
  [
    #align(center)[
      #figure([
      #align(center)[  
        #canvas(length: 1cm, {
          plot.plot(
            name: "f-hat-plot",
            size: (6, 2),
            axis-style: "school-book",
            x-tick-step: none,
            x-ticks: ((0, 0), (calc.sqrt(2), $sqrt(2)$), (calc.sqrt(4), $sqrt(4)$), (calc.sqrt(6), $sqrt(6)$)),
            y-tick-step: none,
            y-ticks: ((-10, -10), (10, 10)),
            y-domain: (-10, 10),
            y-min: -0.5,
            y-max: 1,
            {
              plot.add(
                style: style,
                domain: (1, calc.sqrt(7)), 
                x => (1.0 / (x + 0.05)) * calc.pow(calc.sin(0.5 * calc.pow(x, 2) * calc.pi), 2)
              )
            }
          )
          draw.content(((1,2.5), "-|", "f-hat-plot"), $hat(f)_0$)
        })  
      ]
      ])
    ]
  ],
  [
    #align(center)[
      #figure([
      #align(center)[  
        #canvas(length: 1cm, {
          plot.plot(
            name: "f-plot",
            size: (6, 2),
            axis-style: "school-book",
            x-tick-step: none,
            x-ticks: ((0, 0), (calc.sqrt(2), $sqrt(2)$), (calc.sqrt(4), $sqrt(4)$), (calc.sqrt(6), $sqrt(6)$)),
            y-tick-step: none,
            y-ticks: ((-10, -10), (10, 10)),
            y-domain: (-10, 10),
            y-min: -0.5,
            y-max: 1,
            {
              plot.add(
                style: style,
                domain: (1, calc.sqrt(7)), 
                x => (
                  if x <= calc.sqrt(2) {
                    return calc.pow(calc.sin(0.5 * calc.pow(x, 2) * calc.pi), 1)
                  } else if x <= calc.sqrt(4) {
                    return (1/calc.pow(x, 2)) * calc.pow(calc.sin(0.5 * calc.pow(x, 2) * calc.pi), 1)
                  } else {
                    return -(1/calc.pow(x, 2)) * calc.pow(calc.sin(0.5 * calc.pow(x, 2) * calc.pi), 2)
                  }
                ),
              )
              
            }
          )
          draw.content(((1,2.5), "-|", "f-plot"), $f_0$)
        })
      ]
      ])
    ]
  ]
)
]