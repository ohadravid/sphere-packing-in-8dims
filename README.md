
The sphere packing problem is a mathematical problem of arranging identical spheres within a containing space such that they do not overlap.

In 8 dimensions, this problem was solved by Maryna Viazovska in 2016 ([paper](https://arxiv.org/abs/1603.04246)).

As part of my Bachelor's seminar work, I wrote a detailed explanation of the proof which assumes only undergraduate-level knowledge of mathematics (mostly linear algebra and complex analysis).

The original work is in Hebrew, but I also translated it to English.


## Building

To build the PDFs, you need to have [`typst`](https://github.com/typst/typst) installed.

The documents are written for version `0.12.0`.

To build the documents, run the following commands:

```bash
typst compile heb/seminar/main.typ heb/seminar.pdf
typst compile heb/seminar_presentation/main.typ heb/seminar_presentation.pdf
```