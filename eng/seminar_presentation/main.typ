// Take a look at the file `template.typ` in the file panel
// to customize this template and discover how it works.

#import "template.typ": *

#show: project.with()

#show: slides.with(
  title: "Sphere Packing in Dimension 8",
  subtitle: "Undergraduate Seminar Work",
  date: "05.11.2024",
  authors: ("Ohad Ravid", "Advisor: Dr. Shoni Gilboa"),

  // Optional Styling
  ratio: 16/9,
  layout: "medium",
  title-color: green.darken(26%),
  footer: true,
  counter: false,
  
)

#set text(
  size: 13.5pt
)


// Remove the prefix before the equation number when referencing it.
#set ref(supplement: "")

#let CnE = [Cohn and Elkies]

#let Viaz = [Viazovska]

#let show-notes-slides = false;

#let speaker-notes-slide(mode: "typ", setting: it => it, note) = {
  if show-notes-slides {
    [
      == notes
      #note
    ]
  } else {
    none
  }
}

#speaker-notes-slide[
Empty
]


== The Sphere Packing Problem

Sphere packing is a geometric problem dealing with a seemingly simple question:


#align(center)[
What is the most efficient way to arrange spheres in space?
]

// wine: https://www.flickr.com/photos/webmoof/5082984598
// golf: https://www.pexels.com/photo/close-up-of-pyramid-of-golf-balls-15877131/

#grid(
  columns: (1fr, 1fr),
  [
    #align(center)[
      #image("golf.jpg", width: 80%)
    ]
  ],
  [
    #align(center)[
      #image("wine.jpg", width: 80%)
    ]
  ]
)


#speaker-notes-slide[

Hi,

I am Ohad, I have been studying at the Open University since 2018, and today I would like to present my mathematics seminar on the topic of sphere packing in dimension 8.

The sphere packing problem is a geometric problem that sounds relatively simple: how can we best arrange spheres in space?

In 2D and 3D, the intuitive solution is also the correct one, yet proving that it is indeed optimal is difficult.

Today, I will present the proof of the solution in dimension 8, which is one of only two known higher dimensions for which the answer is known.

]

= Lattices and Packings


#speaker-notes-slide[

Empty
]

== Preliminary Definitions


#let vol = math.op("vol", limits: true)
#let Pkg = math.cal([P])
#let Ball = $ B_r^n $
#let HalfBall = $ B_(r \/ 2)^n $

We denote by
$Ball(x)$
a closed ball of radius
$r$
centered at
$x$.

/ *_Sphere packing_*: #[
  A non-empty subset of
$RR^n$
of equal-radius spheres whose interiors do not overlap.
]
_Upper density_
of any packing
$Pkg$
is defined as:

$ limsup_(r->infinity) vol(Ball(0) sect Pkg) / vol(Ball(0)) $

Here we use the standard Euclidean space and
$vol$
is the Lebesgue measure.\
#let spherepackingdensity = $Delta_(RR^n)$
_The sphere packing density in $RR^n$_, denoted by
$spherepackingdensity$,
is the supremum of all upper densities.#footnote[
This definition does not guarantee that this density is in fact achieved by some packing, but it can be proven (Groemer, 1963).
]

/// #footnote[Groemer, Helmut. "Existenzsätze für Lagerungen im Euklidischen Raum" \ Mathematische Zeitschrift 81 (1963): 260-278. http://eudml.org/doc/170152].

#speaker-notes-slide[

The first concept we need to define is a sphere packing:
a non-empty set of equal-radius spheres whose interiors are disjoint.

To measure the upper density of a packing, we take a ball of increasing radius and look at the limit superior of the ratio of the packing volume inside it to the volume of the ball itself.

We want to know the highest possible packing density in a given dimension, denoted by
DELTA-R-N.
]

== Lattices and Periodic Packings

#import "./packings_figures.typ": p1, p2, p1withsquares

/ *
_Lattice_*: #[
 A rank-$n$ subgroup consisting of a basis of
$RR^n$, and all integer linear combinations of members of the base
]

#speaker-note[
(An integer combination is also called an integral combination, and the basis is called an integral basis.)
]

If 
$B$
is a basis of 
$RR^n$,
then the lattice
$Lambda$
can be represented as:

$ Lambda := {a_1 v_1 + ... + a_n v_n | a_i in ZZ , v_i in B} $


/ *
_Periodic packing_*: #[
  A sphere packing for which there exists a lattice
$Lambda$
so that it is invariant under translation by any lattice element

$ Pkg = Pkg + v,  forall v in Lambda $
]


#speaker-notes-slide[

General packings can be very complicated, so we'll focus on simpler types.

A lattice is a rank-N subgroup composed of a basis of the space and all integer linear combinations of it.

Using lattices, we can define two types of packings: periodic packings, which are invariant under translations by lattice vectors, and lattice packings.
]

==

/ *
_Lattice packing_*: #[
A periodic packing in which all sphere centers lie on a lattice, up to translation

$ Pkg = Lambda + x_0, x_0 in RR^n $
]

#let packs-examples = [

We can use the basis $B = {(0,4), (4,0)}$ \ to construct 
$Lambda = {(4n,4m) | n,m in ZZ}$, and define:

$
Pkg_1 = {B^2_(1\/2)(x) | x in Lambda} \
Pkg_2 = {B^2_(1\/2)(x) | x in Lambda union (Lambda + (1,1))}$
Then $Pkg_1$ is a lattice packing, while $Pkg_2$
is a periodic packing that is not a lattice packing. 
]

#grid(
  columns: (2fr, 1fr,  1fr),
  [
    #packs-examples
  ],
  [
    #align(right + horizon)[#p1]
  ],
  [
    #align(right + horizon)[#p2]
  ]
)


#speaker-notes-slide[

[Lattice packings (click)]
These are packings where every sphere center is exactly on a lattice (up to a translation).

For example, if we take a basis with vectors (0,4) and (4,0), we get a lattice of all points where both coordinates are multiples of 4. If we place a sphere of radius 1/2 at each such point, we get a lattice packing.

A periodic packing is more general. For example, P3 here is a periodic packing but not a lattice packing.
]

== Lattice Packing Density

#grid(
  columns: (3fr, 1fr),
  [
    

We can think of the lattice as a tiling of space by parallelepipeds.


_The fundamental cell_
of a sphere packing based on a lattice
$Lambda$
with basis
$B$
is given by:

$ C := {x_1 v_1 + ... + x_n v_n | 0 <= x_i <= 1 , v_i in B} $

Translations of the fundamental cell by elements of
$Lambda$
tile the entire space, and a lattice packing places a sphere at each edge.
  ],
  [

    #align(right + horizon)[#p1withsquares]
  ],
)

Because there is one sphere per copy of the cell, the density is given by
$vol(Ball) \/ vol(C)$.
We can use the fact that
$vol(C) = vol(RR^n \/ Lambda)$,
so the lattice packing density is:

$ vol(Ball) / vol(RR^n \/ Lambda) $ <packing-vol>

#speaker-notes-slide[

One advantage of lattice packings is that their density is relatively easy to calculate.

We can think of the lattice as a tiling of space by fundamental cells (the parallelepipeds defined by the lattice basis).

Since there's one sphere per cell copy, the density is the volume of the sphere divided by the volume of the cell.

We have $vol(C) = vol(RR^n / Lambda)$, so the packing density is $vol(Ball) / vol(RR^n / Lambda)$.
]

== Definition and Basic Properties of $E_8$

$E_8$ is a sphere packing in
$RR^8$.
We present the lattice on which it is based,
$Lambda_8$:

$ Lambda_8 = { (x_1, ... , x_8) in ZZ^8 union (ZZ + 1/2)^8 mid(|) sum^8_(i=1)x_i equiv 0 (mod 2) } $


$Lambda_8$
is an _integral lattice_,
i.e., all inner products between its basis vectors are integers.
\
$Lambda_8$
is an _even lattice_,
i.e., the squared length of every vector in it is an even integer.
\
In particular, the distance between any two points in
$Lambda_8$
is of the form
$sqrt(2k)$, 
and between two closest points
it is
$sqrt(2)$.
\
We choose a lattice packing with
$r=sqrt(2) / 2$,
denoted by
$E_8$.
Its resulting density is:

$ vol(RR^8 \/ Lambda_8) = 1, #h(3em) vol(B^8_(sqrt(2) \/ 2)) / vol(RR^8 \/ Lambda_8) =  pi^4 / 384 = 0.2538 ...  $


#speaker-notes-slide[

We focus on the $E_8$ packing in $RR^8$.

It is based on the lattice
$Lambda_8$,
defined by vectors either all integers or all half-integers, with even sum.

$Lambda_8$ is integral and even. The minimal distance between lattice points is $sqrt(2)$.

From this, we build a lattice packing of spheres of radius
$sqrt(2)/2$.
The density is $pi^4/384 = 0.2538...$.

]
== The Duality of $Lambda_8$

We need one more property of
$Lambda_8$:
it is it's own _dual lattice_.


For a lattice
$Lambda$
with a given basis
${ v_1, ..., v_n }$
define:

/ *
_Dual lattice_ of
$Lambda$
*: #[
The lattice with basis
${ v^*_1, ..., v^*_n }$
such that 
$
angle.l v_i, v^*_j angle.r = cases(
  1 "if" i = j "," "and",
  0 "otherwise"
)
$
]

We denote the dual lattice by
$Lambda^*$.
If
$Lambda = Lambda^*$
then $Lambda$ is its own dual lattice.


#speaker-notes-slide[

Another crucial property: $Lambda_8$ is it's own dual.

The dual lattice is defined so that its basis is dual to the original basis.

If $Lambda = Lambda^*$, then we call $Lambda$ is own dual lattice.
]

= Using Harmonic Analysis to Find Density Bounds <harmonic-analysis-and-enc>



#speaker-notes-slide[

Empty
]

== 

If we can prove that the highest possible sphere packing density in
$RR^8$
is equal to the density of
$E_8$,
we will in fact prove that it is an optimal packing in this dimension.

This problem was addressed by 
$CnE$,
who showed that one can use auxiliary functions with certain properties (to be detailed shortly) to bound the maximal possible density in a given dimension.

The main insight is in the relationship between a function and its Fourier transform, through the #[Poisson summation formula].


#speaker-notes-slide[

Cohn and Elkies

We want to show that the maximal sphere packing density in $R^8$ equals that of $E_8$,
thus proving $E_8$ is optimal.

Cohn and Elkies showed that if an auxiliary function with certain properties exists, it can be used to bound the maximum density.

Their result relies on the Poisson summation formula, which relates a function and its Fourier transform.
]

== Schwartz Functions

We consider functions from
$RR^n$
to
$RR$
(and sometimes complex-valued functions of a real variable).

#let Sw = $cal(S)(RR^n)$

/ * _Schwartz functions_ - $Sw$ *: #[
All infinitely differentiable functions for which

$ sup_(x in RR^n) | x^beta (diff / (diff x) )^alpha f(x) | < infinity $

for all
$alpha,beta in NN$,
where
$(diff / (diff x) )^alpha$ is a series of partial derivatives in some order.
]
In other words, these are functions whose values and all derivatives decay "very rapidly", or faster than any polynomial.

#speaker-note[
This ensures we can avoid technical complications. Also, Viazovska’s proof builds Schwartz functions, making these conditions sufficient for our needs.
]


#speaker-notes-slide[

We'll focus on Schwartz functions: infinitely differentiable functions whose values and derivatives decay faster than any polynomial.

This assumption simplifies technicalities, and since Viazovska's proof constructs Schwartz functions, these conditions suffice.
]

== The Fourier Transform in $RR^n$ for Schwartz Functions

Define the Fourier transform of $f$:

$ hat(f)(xi) = integral_(RR^n) f(x) e^(-2pi i x dot xi) dif x , "for" xi in RR^n $

#speaker-note[
We integrate over the entire space; $x dot xi$ is the inner product.
]

We'll use the notation
$f(x) -> g(xi)$
to mean that
$g$
is the Fourier transform of
$f$.

For Schwartz functions, the inverse Fourier transform also holds:

$ f(x) = integral_(RR^n) hat(f)(xi) e^(2pi i x dot xi) dif xi $ <fourier-inversion-formula>


#speaker-notes-slide[

The Fourier transform for Schwartz functions is defined as above.

Because $f$ is a Schwartz function, the inverse transform also holds, letting us recover $f$ from $hat(f)$.
]

==

/ *A _radial function_*: #[
A function that depends only on
the value of $|x|$
]

A key property of radial functions is $hat(hat(f)) = f$:

#speaker-note[
This follows from the inverse Fourier transform.
]

$ 
hat(hat(f))(y) &= integral_(RR^n) hat(f)(xi) e^(-2pi i xi dot y) dif xi \
&= integral_(RR^n) hat(f)(xi) e^(2pi i xi dot (-y)) dif xi \
&= f(-y) \
&= f_0(|-y|) \
&= f(y)
$


#speaker-notes-slide[

For radial functions (which depend only on the radius), applying the Fourier transform twice returns the original function. This is easy to see from the inversion formula and the radial symmetry.
]

== Poisson Summation Formula

To apply tools from analysis to the density problem, we use the Poisson summation formula.

#speaker-note[
This formula expresses a duality between summing a function over a lattice and summing its Fourier transform over the dual lattice.
]

We start by proving for the one-dimensional case:

For $f in cal(S)(RR)$,

$ sum_(n=-infinity)^(infinity) f(n) = sum_(n=-infinity)^(infinity) hat(f)(n) $ <poisson-sum-for-r>

#speaker-note[
Summing $f$ over all integers equals summing $hat(f)$ over all integers.
]

We know two functions are identical if their Fourier series expansions match.

We define two 1-periodic functions and compare their Fourier coefficients to show equality.


#speaker-notes-slide[

The Poisson summation formula says summing $f$ over all integers equals summing $hat(f)$ over all integers.

We'll show this by constructing two 1-periodic functions and comparing their Fourier expansions.
]

==

Let:
$ F_1(x) := sum_(n=-infinity)^(infinity) f(x+n) $

$F_1$
is 1-periodic, and since $f$ decays very rapidly, the sum converges absolutely and
$F_1$
is continuous.


We can define another 1-periodic function:

$ F_2(x) := sum_(n=-infinity)^(infinity) hat(f)(n) e^(2 pi i n x) $

We can think of
$F_2$
as a discrete version of the inverse Fourier transform of
$f$, by replacing the integration with a sum in $f(x) = integral_(RR^n) hat(f)(xi) e^(2pi i x dot xi) dif xi$.


#speaker-notes-slide[

Define
$F_1(x)$ as the sum of $f(x+n)$ over all integers $n$, giving a 1-periodic function.

Because $f$ is Schwartz, it converges absolutely and is continuous.

Similarly, define
$F_2(x)$ using $hat(f)(n)$.

$F_2$ is like the discrete version of the inverse Fourier transform, restricted to integers.
]

==

For
$F_2(x)$,
the $m$-th Fourier coefficient is $hat(f)(m)$.
For $F_1(x)$:

$ 
a_m &= integral_0^1 (sum_(n=-infinity)^(infinity) f(x+n)) e^(-2 pi i m x) dif x \
    &= sum_(n=-infinity)^(infinity) integral_0^1 f(x+n) e^(-2 pi i m x) dif x \
    &= sum_(n=-infinity)^(infinity) integral_n^(n+1) f(y) e^(-2 pi i m y) dif y \
    &= integral_(-infinity)^infinity f(y) e^(-2pi i m y) dif y \
    &= hat(f) (m)
$

#speaker-notes-slide[

Comparing Fourier coefficients:

For $F_2$, the $m$th coefficient is $hat(f)(m)$.

For $F_1$, after changing variables and summation order, we also find its $m$th coefficient is $hat(f)(m)$.

Thus $F_1=F_2$.
]

==

Note that swapping integration and summation is allowed ($f$ is $cal(S)$).


So have $F_1=F_2$, and setting $x=0$ gives:

$ sum_(n=-infinity)^(infinity) f(n) = sum_(n=-infinity)^(infinity) hat(f)(n) $

This generalizes to $RR^n$:

$ sum_(x in ZZ^n) f(x) = sum_(x in ZZ^n) hat(f)(x) $ <poisson-sum-for-r-n>


#speaker-notes-slide[

By setting $x=0$, we recover the Poisson summation formula.

This extends to $ZZ^n$ as well.
]

==

To extend the result to general lattices, let
$M: Z^n -> RR^n$
be the linear transformation with respect to the basis
$B = {v_1, ... , v_n}$
of
$Lambda$.
Then
$M$
is a matrix whose columns are the basis vectors,
and
$Lambda = M ZZ^n$.

Thus,
$ sum_(x in Lambda) f(x) = sum_(x in ZZ^n) f(M x) $

Let
$g(x)=f(M x)$
and from the formula we obtain that
$hat(g)(xi) = integral_(RR^n) f(M x) e^(-2pi i xi dot x) dif x$.

Through a bit of algebraic manipulation, we obtain that
$
hat(g)(xi) &= 1/vol(RR^n \/ Lambda) integral_(RR^n) f(u) e^(-2pi i (M^(-1))^T xi dot u) dif u = 1/vol(RR^n \/ Lambda) hat(f)((M^(-1))^T xi)
$

#speaker-notes-slide[
  
For a general lattice, we apply a linear transform. Using $M$, we rewrite the sum over $Lambda$ as a sum over $ZZ^n$.

Then we get a relation involving $f$ and $hat(f)$ evaluated at the dual lattice $Lambda^*$.
]

==

We substitute back and obtain that

$ sum_(x in Lambda) f(x)  = sum_(x in ZZ^n) f(M x) 
= 
sum_(xi in ZZ^n) hat(g)(xi) = 1 / vol(RR^n \/ Lambda) sum_(xi in ZZ^n) hat(f)((M^(-1))^T xi) $

Let
$D$
be the basis of the dual lattice
$Lambda^*$.
Directly from the definition, we obtain that
$M^T D = I$,
so
$D = (M^T)^(-1)$ 
and therefore
$Lambda^* = D ZZ^n = (M^T)^(-1) ZZ^n$.
That is:

#let filled-block(body) = {
  [#align(center)[    #block(      fill: luma(230),      inset: 8pt,      radius: 4pt,      [        #body      ]
    )
  ]]
}

#filled-block[  $ sum_(x in Lambda) f(x) = 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^*) hat(f)(y) $]

And this is Poisson summation formula for lattices in
$RR^n$.

#speaker-notes-slide[
  
We derive the Poisson summation formula for a general lattice:

$ sum_(x in Lambda) f(x) = 1/vol(RR^n / Lambda) sum_(y in Lambda^*) hat(f)(y)$
]


== Linear Programming Bounds <lin-bounds>

The main theorem of $CnE$:

*Let*
$f in Sw$
and $0 < r in RR$,
such that:

1. $f(0) = hat(f)(0) > 0$
2. $hat(f)(y) >= 0$ for all $y in RR^n$
3. $f(x) <= 0$ for all $|x| >= r$

*Then*
The density in a lattice packing in
$RR^n$
is bounded above by
$vol(HalfBall)$.

We start by proving for lattice packings, then extend to general packings.


#speaker-notes-slide[

Cohn and Elkies' main theorem:

If there exists a Schwartz function $f$ and $r>0$ so that:
1. $f(0)=hat(f)(0)>0$
2. $hat(f)(y)≥0$ for all $y$
3. $f(x)≤0$ for all $|x|≥r$

Then the maximal density is at most $vol(B_{r/2}^n)$.

We'll first prove it for lattice packings and then extend to general packings.
]

==

#counter(footnote).update(0)


Let $Lambda$ be a lattice in $RR^n$.

Without loss of generality, assume $Lambda$ has minimal vector length $r$#footnote()[
Scaling the lattice (and thus the sphere radius) does not change the packing density.
].

Then there's a lattice packing with spheres of radius
$r/2$
and density

$ vol(HalfBall) / vol(RR^n \/ Lambda) $

We need to show
$vol(RR^n \/ Lambda) >= 1$.


#speaker-notes-slide[

Assume the lattice $Lambda$ has minimal vector length $r$.

Then there's a corresponding lattice packing of spheres with radius $r/2$ and density $vol(HalfBall) / vol(RR^n / Lambda)$.

We must show $vol(RR^n / Lambda) ≥ 1$.
]

==

By Poisson summation,
$sum_(x in Lambda) f(x) = 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^*) hat(f)(y)$.


#let left-sum = [
Expressing the left sum:

$ sum_(x in Lambda) f(x) = f(0) + sum_(x in Lambda , x != 0) f(x) $

We have $f(0)>0$ and $f(x) <= 0$ for $|x|>=r$ (and for $x in Lambda, x != 0$).
Thus:

$ sum_(x in Lambda) f(x) <= f(0) $
]

#let right-sum = [
For the right sum:

$ hat(f)(0) <= sum_(y in Lambda^*) hat(f)(y) $

So

$ (hat(f)(0)) / vol(RR^n \/ Lambda) <=  1 / vol(RR^n \/ Lambda) sum_(y in Lambda^*) hat(f)(y) $
]

#grid(
  gutter: 1em,
  columns: (1fr, 1fr),
  [
    #block(inset: 1em)[
        #left-sum
    ]
  ],
  grid.vline(stroke: black),
  [
    #block(inset: 1em)[
        #right-sum
    ]
  ]
)


 #speaker-notes-slide[

Starting from Poisson summation:

- Left side: $f(0)$ plus terms $f(x)$ for $x \neq 0$. Since $f(x)≤0$ for $|x|≥r$, the sum is ≤ $f(0)$.

- Right side: $hat(f)(0)$ is less or equal to the sum over $Lambda^*$, so $(hat(f)(0))/vol(RR^n/Lambda) ≤ ...$
]

==

Combining:

$ f(0) >= sum_(x in Lambda) f(x) = 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^*) hat(f)(y) >= (hat(f)(0)) / vol(RR^n \/ Lambda) $

So

$ (hat(f)(0)) / vol(RR^n \/ Lambda) <=  f(0) $

But we required
$f(0) = hat(f)(0)$,
thus

$ vol(RR^n \/ Lambda) >= 1  $


 #speaker-notes-slide[

Combining these inequalities yields $vol(RR^n / Lambda) ≥ 1$.

Hence the density is at most $vol(B_{r/2})$.
]

== Extension to periodic and general packings

Our proof only cover lattice packings. What about general packings?

Every general packing can be approximated arbitrarily closely by a periodic packing.
If we repeat a large enough finite arrangement periodically, the density loss is negligible.

For a periodic packing constructed from $Lambda$ and $N$ translations
$t_1, ..., t_N$,
we again assume minimal vector length $r$, so radius $r/2$ and density
$N dot vol(HalfBall) / vol(RR^n \/ Lambda)$.

We need $ vol(RR^n \/ Lambda) >= N$,
proven similarly using:
$ sum_(j,k=1)^N sum_(x in Lambda) f(t_j - t_k + x) $

#speaker-note[
Unsure if the phrase fully clear. But it mirrors the lattice argument.
]


#speaker-notes-slide[

For general packings, approximate them by periodic ones, losing negligible density.

For a periodic packing with $N$ translates of a lattice, a similar argument with sums over $t_j - t_k + x$ shows the same bound.

Thus the result extends beyond lattices.
]

== The zeros of the function and further conclusions 
The density of a lattice packing of spheres of radius
$r/2$ is:

$ vol(HalfBall) / vol(RR^n \/ Lambda) $

So if we have such a function $f$ and $r$,
the density equals the bound when
$vol(RR^n \/ Lambda) = 1$.

Plugging into Poisson summation:

$ f(0) + sum_(x in Lambda , x != 0) f(x) = hat(f)(0) + sum_(y in Lambda^* , y != 0) hat(f)(y) $

From the conditions, both infinite sums vanish.
So $f$ must vanish on the lattice (except at 0), and $hat(f)$ must vanish on the dual lattice (except at 0).

#speaker-note[
We required $f(0)=hat(f)(0)$, and considering signs, we see all other terms must be zero.
]


#speaker-notes-slide[

If the maximum density is achieved, we have equality in the inequalities.

Then $f$ vanishes on the lattice (except at 0) and $hat(f)$ on the dual lattice (except at 0).

This means a function achieving equality must have these zeros, guiding the search for a suitable $f$.
]

==

All the conditions on the desired function are linear and remain invariant under rotation, and therefore we can think of
$f$
as a _radial_ function
$f_0(x), x >= 0$.

With respect to the lattice
$Lambda_8$,
we see it must vanish at every point $sqrt(2k)$ and can also deduce the parity of the order of each zero, and schematically depict the expected shape of the function and its Fourier transform:

#import "./plots_grid.typ": plots_grid

#plots_grid

#speaker-notes-slide[

Focusing on radial functions, we find that $f_0$ must have zeros at all distances $sqrt{2k}$.

This sets a very specific pattern for $f_0$ and its transform $hat(f_0)$, both having prescribed zeros and multiplicities.
]

= Modular Forms <modular-forms>

#speaker-notes-slide[

Empty
]

==

It seems we made a lot of progress: we have a "recipe" for the required function:
Find a function with zeros at the $E_8$ lattice points and whose Fourier transform also has zeros there.

But constructing such a function is very hard.

While we can build a function with given zeros, and one whose Fourier transform has given zeros, controlling both simultaneously is not straightforward.


#speaker-notes-slide[

We have a path: we need a function and its Fourier transform to vanish at the right points.

But constructing such a function is very challenging.
]

==

We have seen that for radial functions, 
$hat(hat(f)) = f$.
So such $f$, we can have:

$ g_(+1) = (f+hat(f)) / 2  , #h(2em) g_(-1) = (f-hat(f)) / 2 $

Under Fourier transform:
$hat(g_(+1)) =& (hat(f)+f) \/ 2 =&  g_(+1) , 
  hat(g_(-1)) =& (hat(f)-f) \/ 2 =& -g_(-1)$.

/ *_Eigenfunction_ of the Fourier transform
*: #[A function satisfying for some scalar
$alpha in RR$
somewhere
$hat(f) = alpha f$.
]
So
$g_(+1)$
is an eigenfunction with
$alpha = 1$,
and
$g_(-1)$
is an eigenfunction with
$alpha = -1$. \
They receive the required roots as
$f$,
and we can obtain via them back the
$f$ since
$f = g_(+1) + g_(-1)$. 
The two new functions are independent of each other, and therefore we can construct each of them separately.

#speaker-notes-slide[

We split $f$ into two parts: $g_(+1)$ and $g_(-1)$, which are eigenfunctions of the Fourier transform.

If we can build these two eigenfunctions separately, we can combine them to get $f$.
]

== The Laplace Transform

To find such eigenfunctions, we turn to the #[_Laplace Transform_] of _Gaussian functions_.

#speaker-note[
\
The #[_Gaussian_]
(_Gaussian_),
$f(x) = e^(-t pi abs(x)^2)$,
is a function that satisfies
$e^(-t pi abs(x)^2) -> t^(-n/2) e^(-pi abs(y)^2 \/ t)$.
]

/ *_Laplace Transform_*: #[
For a function
$g$,
we define its Laplace transform using:
$ f(x) = integral_0^infinity e^(-t pi abs(x)^2) g(t) dif t $
]

As long as
$g$
is simple, we can obtain the Fourier transform of
$f$
by a simple substitution:

$ hat(f)(y) =& integral_0^infinity t^(-n/2) e^(-pi abs(y)^2 \/ t) g(t) dif t 
// Replace t -> 1/t.
            =& integral_0^infinity e^(-pi abs(y)^2 t) t^(n/2 - 2)  g(1 \/ t) dif t
$ <fourier-transform-of-laplace-transform>

If it holds that
$g(1 \/ t) = epsilon t^(2 - n/2) g(t)$
then
$hat(f) = epsilon f$,  and so we are looking for such functions.

#speaker-notes-slide[

By using the Laplace transform of Gaussians, we can derive conditions for $f$ to be an eigenfunction of the Fourier transform.

If $g(1/t)$ is proportional to $g(t)$ times a power of $t$, we get $hat(f)=alpha f$.
]

== Initial Definitions

_Modular forms_ are functions that satisfy certain functional equations.


#let hh = $frak(h)$
#let Im = $op("Im")$
#let Re = $op("Re")$

Let $hh = { z in CC | Im z > 0 }$ (upper half-plane).

/ *_Weakly modular form of weight k_*: #[
If $F: hh -> CC$ and $k in ZZ$,
$F$ is a weakly modular form of weight $k$ if:

$ F((a z + b)/(c z + d)) = (c z + d)^k F(z) $

for all
$mat(a, b; c, d) in "SL"(2, RR)$.
]

If $F$ is holomorphic in $hh$ and bounded as
$Im z -> infinity$,
then $F$ is a modular form.


#speaker-notes-slide[

Modular forms are a family of functions satisfying certain functional equations under transformations of the upper half-plane.

If they are also holomorphic and bounded at infinity, they are called modular forms.
]

== Eisenstein Series - $E_k$

#counter(footnote).update(0)
#let overlap-footnote = [
Although there's a notation overlap between the packing
$E_8$
and
$E_k$
when $k=8$, we'll only use $k in {2,4,6}$.
]

The Eisenstein series is defined#footnote(numbering: "1")[#overlap-footnote]

$ E_k (z) = 1 / (2 zeta (k)) sum_((m,n) in ZZ^2 \ (m,n)!=(0,0)) 1 / (m z + n)^k $

$zeta$ is the Riemann zeta function ($zeta(k) = sum_(n=1)^infinity 1/n^k$
).
For even $k>2$:

$ 
E_k (z+1) = E_k (z) , space
E_k (-1 / z) = z^k E_k(z) 
$

So $E_k$ are modular forms of weight $k$.


#speaker-notes-slide[

Eisenstein series $E_k$ are defined by a certain double sum involving the lattice $ZZ^2$ and the Riemann zeta function.

For even $k>2$, $E_k$ is 1-periodic and satisfies a functional equation making it a modular form of weight $k$.
]

==

For $k=2$, the series converges conditionally, not absolutely.
Define the summation order:

$ E_k (z) &= 1/(2zeta(k)) sum_(n!=0) 1/n^k + 1/(2zeta(k)) sum_(m!=0) sum_(n in ZZ) 1/(m z + n)^k $

For $k=2$, $E_2(z+1) = E_2 (z)$,
but
$ E_2 (-1 \/ z) = z^2 E_2 (z) - 6i z \/ pi $

$E_2$ is a _quasimodular form_ of weight 2.


#speaker-notes-slide[

For $k=2$, we get a quasimodular form $E_2$ which is almost modular but with an extra term appearing in the transformation.
]

==

Sketch of the proof: For every
$epsilon>0$,
we can denote

$ G_(2,epsilon) (z) = 1 / 2 sum_((m,n) in ZZ^2 \ (m,n) != (0,0)) 1 / ((m z + n)^2 |m z + n|^(2epsilon)) $

and obtain a series that converges absolutely.
The new function satisfies the equation

// This the same as @eqt:eisen-k (but with the general az+b/cz+d input).

$ G_(2,epsilon)((a z + b) / (c z + d)) = (c z + d)^2 |c z + d|^(2epsilon) G_(2,epsilon)(z) $

The limit
$G^*_2 (z) := lim_(epsilon -> 0) G_(2,epsilon)(z)$
exists and is equal to 
$zeta(2) E_2 (z) - pi \/ (2Im(z))$.

$G^*_2$
behaves similarly to a modular form of weight 2,
and substituting it into the functional equation gives the desired result.

#speaker-notes-slide[

I want to present a short sketch of the proof, to explain where this factor came from.

Although the series does not converge absolutely, it is so close that for every epsilon greater than zero, we can denote the following function, which is similar to the original Eisenstein series but without the constant factor,
and obtain a series that converges absolutely.

The new function satisfies this functional equation, which means it behaves like a modular form.

Now, we can find the limit as epsilon approaches zero, and show that it is equal to
zeta(2) multiplied by E_2(z),
minus pi over two times the imaginary part of Z.

We denote the function
$G^*_2$ as this limit, it also satisfies the equation, and we can substitute the obtained limit into the equation to get the desired result.
]

== Fourier expansion of $E_k$

A 1-periodic function has the following Fourier expansion:
$g(z) space ~ sum_(n = -infinity)^infinity a_n e^(2 pi i n z)$.

From here on, we denote 
$q = e^(2 pi i z)$.
To find the Fourier expansion for
$E_k$
we use:

$ sum_(n in ZZ) 1 / (z + n) = pi / tan(pi z) = -2pi i (1/2 + sum_(r=1)^infinity q^r), space z in CC \/ ZZ $ <sum-one-over-z-plus-n>

and by using
$k$
term-by-term derivatives, we can get an expansion for
$sum_(n in ZZ) 1 / (z + n)^k$.  In total, we get that:

$ E_k (z) = 1 + 2 / (zeta (1 - k)) sum_(m=1)^infinity sigma_(k-1)(m) q^m $ <e-k-fourier-exp>

where
$sigma_(k-1)(m)$
returns the sum of the divisors of
$m$, 
each is raised to the power
$k-1$.

#speaker-notes-slide[

From now on, we denote
Q
$e^(2 pi i z)$.

Since
$E_k$
are 1-periodic functions, it is natural to compute their Fourier expansions.

What is important for me to note is that, for example, by taking term-by-term derivatives of the series here, we can obtain an explicit expression for 
the coefficient of each term in the Fourier expansion, for each of the functions
$E_k$.

-

// We want to work with the Fourier expansion of
// $E_k$.

// From here on, we denote
// $E^{2\pi i z}$
// by $Q$,
// and use the following identity to find the Fourier expansion of
// $E_k$.

// If we differentiate $k$ times term by term, we can obtain an expansion for a sum that is almost the desired sum. 

// Then, we can replace the double sum 

// with a sum from 1 to infinity of $\sigma_{k-1}(M)$,

// which is a function that returns the sum of the divisors of $M$ raised to the power $k-1$.

$ E_k (z) &= 1 + 1/(zeta(k)) sum_(m=1)^infinity [(-2pi i)^k / (k-1)! sum_(r=1)^infinity r^(k-1) q^(m r)] 
 &= 1 + 1/(zeta(k)) (2pi i)^k / (k-1)! sum_(m=1)^infinity sigma_(k-1)(m) q^m\ 
 $ <e-k-fourier-exp>

]

#let thetaseries = $Theta_Lambda$
#let thetaseriesdual = $Theta_(Lambda^*)$
#let thetaseries = $Theta_Lambda$
#let thetaseriesdual = $Theta_(Lambda^*)$

== Theta Function of a Lattice - $thetaseries (z)$ <theta-series>

For every lattice
$Lambda$
we can define a function, called the theta function, as follows

$ thetaseries(z) = sum_(x in Lambda) e^(pi i abs(x)^2 z) $

This function converges when
$Im z > 0$
and defines an analytic function on
$hh$.

This function has an important property:
For every lattice
$Lambda$
in
$RR^n$,
it holds that

$ thetaseries(z) = 1 / vol(RR^n \/ Lambda) (i/z)^(n \/ 2) thetaseriesdual (-1 \/ z) $

for every
$z in hh$.

The proof is by using Poisson's summation formula on the inner function,
$e^(-t pi abs(x)^2)$.

 #speaker-notes-slide[

Without dwelling too much on the details, for every lattice, we can define a function called the lattice theta function by summing all the vectors in the lattice while applying the Gaussian function to them.

This is a function that converges in the upper half-plane and satisfies the following functional equation.

(Click)
]

==

We will use the following theta functions,
defined on the lattice
$ZZ$:

$ 
theta_00 (z) = sum_(n in ZZ) e^(pi i n^2 z)\
theta_01 (z) = sum_(n in ZZ) (-1)^n e^(pi i n^2 z)\
theta_10 (z) = sum_(n in ZZ) e^(pi i (n + 1/2)^2 z)
$ <theta-z-funcs>

#grid(
  columns: (1fr, 1fr),
  [
    #align(center)[
      $
      z^(-2) theta_00^4(-1 \/ z) &= -theta_00^4(z) \
      z^(-2) theta_01^4(-1 \/ z) &= -theta_10^4(z) \
      z^(-2) theta_10^4(-1 \/ z) &= -theta_01^4(z) \
      $
    ]
  ],
  [
    #align(center)[
      $
      theta_00^4(z + 1) &= &theta_01^4(z) \
      theta_01^4(z + 1) &= &theta_00^4(z) \
      theta_10^4(z + 1) &= -&theta_10^4(z)
      $
    ]
  ]
)

 #speaker-notes-slide[

We will use the following three functions, which I just want to present here for completeness.
]

= Viazovska's function <viaz-func>

#speaker-notes-slide[

Empty
]
== Constructing a Function with the Appropriate Zeros

Modular forms, combined with the Laplace transform, are well-suited for our search for Fourier-transform eigenfunctions.

But how can we build a function with the required zeros?
One very direct way is by multiplying by a simple function that already has similar zeros. This is exactly the approach #Viaz used to prove the existence of the necessary functions.

The function
$sin^2(pi|x|^2\/2)$
has second-order zeros at every
$x = sqrt(2k), k=1,2,...$.

Combined with the Laplace transform, we get a template suitable for both eigenfunctions we are looking for:

#filled-block[
$ sin^2(pi abs(x)^2 \/ 2) integral_0^infinity e^(-t pi abs(x)^2) f(t) dif t $ <eign-fn-pattern>
]

#speaker-note[
These are almost the zeros we need, except for an undesirable fourth-order zero at
$x=0$,
and a second-order zero at
$x=sqrt(2)$
instead of a first (or some other odd order). However, as it turns out, these issues can be resolved.

Note that in order to use this template, it’s not enough just to find eigenfunctions: we must also compensate for the unwanted zeros and ensure that the final function meets the required bounds.
]

== Conditions for Suitable Eigenfunctions

// Intro

We begin by searching for the eigenfunction with eigenvalue 1. Suppose we have a function:

$ a(x) = sin^2(pi abs(x)^2 \/ 2) integral_0^infinity e^(-t pi abs(x)^2) g_0(t) dif t $

We want to know when
$hat(a) = a$.

We seek non-necessary conditions. We do not need tools to characterize all functions that could serve as a proof of the bound, just an example of a single suitable function.

We will try making educated guesses to narrow down the "search area."

As a start, let us assume that
$abs(x)>sqrt(2)$,
because that is where the function’s behavior should be simpler.


#speaker-notes-slide[

We begin the search for the eigenfunction with eigenvalue 1.
If we take the given template, we want to know when
A-hat equals A.

We are not looking for necessary conditions. We do not need to characterize all such functions, just find one "magic" function that works.

What we will do is make some informed guesses to narrow our search area.

As a start, assume that the length of x is greater than sqrt(2), since we know that the function’s behavior should be simpler there.
]
==

#counter(footnote).update(0)

We can also think of
$a$
as a radial function defined#footnote[
When we discuss the Fourier transform, we continue denoting
$a(x): RR^n -> RR$
and its transform by
$hat(a)(y)$,
replacing
$r^2$
with the square of the norm of the vector
$x$.
]
for
$r in RR_(>=0)$.

We saw that the Fourier transform of the Laplace transform takes the following form:
$  integral_0^infinity e^(-t pi abs(x)^2) g(t) dif t -> integral_0^infinity e^(-pi abs(y)^2 t) t^2 g(1 / t) dif t $

So if we assume
$a$
is an eigenfunction (i.e.
$hat(a) = a$),
we can move toward the desired form if we choose
$g_0(t) = h_0(1/t) t^2$,
meaning
$ a(r) = sin^2(pi r^2 \/ 2) integral_0^infinity e^(-pi r^2 t) t^2 h_0(1 / t) dif t $


#speaker-notes-slide[

Recall that we can think of
A
as a radial function, for
R ≥ 0.

We also saw that the Fourier transform of the Laplace transform replaces the inner function by
t^2 times the inner function evaluated at 1/t.

So if we assume
A
is an eigenfunction, it makes sense to approach that form.

Therefore, we replace
G_0(t)
with
H_0(1/t) t^2.
]

#let phi0 = $phi.alt_0$

== 

We want to work in 
#hh,
the upper half-plane in order to use modular forms,
and we can simplify even further if we look for a function 
$phi0 : i RR -> i RR$.
We can denote:

$ i phi0 (1/(i t)) = h_0(1/t) $

#speaker-note[
The choice of where to "place" the $i$'s is somewhat arbitrary, but it will fit our next step very well, as we will see shortly.
]

We can now change the integration domain to
$(0, i infinity)$
by changing variables
$z= i t => t = -i z$:

$ a(r) &= sin^2(pi r^2 \/ 2) integral_0^infinity e^(-pi r^2 t) t^2  i phi0(1 / (i t)) dif t \

 &= - sin^2(pi r^2 \/ 2) integral_0^(i infinity) phi0(-1/z) z^2  e^(z pi i r^2) dif z 
$

#speaker-notes-slide[

To use modular forms, we need to work in the upper half-plane.

We know our function takes a real variable and returns real values, so we can look for a complex function 
from IR to IR,
and denote it as phi0
(rhymes with “ksai” and “fai”).

We substitute and perform the change to go to the integration segment from 0 to i-infinity, and in total we get this form:

A equals

minus sine squared of pi-R-squared over 2,
times the integral from zero to i-infinity of phi0 of minus one over
Z,
times
Z-squared, e^(Z pi i R^2),
dZ.

Although the expression looks rather complicated, the motivation for each part is quite clear:

- The sine gives us zeros on the lattice,
- The integral resembles a Laplace transform but on the imaginary axis for modular forms,
- phi0(-1/Z) times Z^2 is expected since we are aiming at an eigenfunc.
]

==

What if we try to get even closer to the form of a Laplace transform?
That is, we want 
$e^(z pi i r^2)$
to be one of the factors inside the integral, avoiding factors outside it.

We can do this using the identity

$ sin^2(pi r^2 \/ 2) = -1/4 (e^(i pi r^2) - 2 + e^(-i pi r^2)) $

So we define:

$ a(r) := -4 sin^2(pi r^2 \/ 2) integral_0^(i infinity) phi0(-1/z) z^2  e^(pi i r^2 z) dif z  $ <a-for-r-gt-sqrt-2>

From now on, we will no longer change 
$a$,
and we will focus on the inner function,
$phi0$.

#speaker-notes-slide[

We are still not very close to a Laplace transform form. We need the factor
$e^(z pi i r^2)$
to be one of the factors inside the product in the integral, and we don’t want factors outside the integral.

We can replace the sine with exponentials using the following identity:

Then we will update our definition of
A
to include the multiplication by 4, keeping the decomposed form simpler.
]

==

We will use the identity and obtain

$ a(r) = integral_0^(i infinity) [ phi0(-1/z) z^2  e^(pi i r^2 (z+1)) - 2 phi0(-1/z) z^2  e^(pi i r^2 z) + phi0(-1/z) z^2  e^(pi i r^2 (z-1)) ] dif z $

We will deal with the exponents
$z+1, z-1$
in the factors
$e^(pi i r^2 (z ± 1))$,
which prevent us from obtaining a Laplace transform. \
We can perform a change of variables in each of the terms, and get:

$ 
// Fix the e^(z+1) by ~z= z+1 => z= ~z-1  
integral_0^(i infinity) phi0(-1/z) z^2  e^(pi i r^2 (z+1)) dif z = integral_(1)^(i infinity+1) phi0(-1/(z-1)) (z-1)^2  e^(pi i r^2 z) dif z \

// Fix the e^(z-1) by ~z= z-1 => z= ~z+1  
integral_0^(i infinity) phi0(-1/z) z^2  e^(pi i r^2 (z-1)) dif z = integral_(-1)^(i infinity-1) phi0(-1/(z+1)) (z+1)^2  e^(pi i r^2 z) dif z
$

#speaker-notes-slide[

This identity allows us to reach the following form of the function, which is much more verbose yet completely equivalent, and suitable for analysis using a Fourier transform.

The exponents involving $(z ± 1)$ are relatively easy to handle: we can perform a change of variables in each integral to obtain the integrals shown.
]

==

Let us assume it is possible to go from
$phi0(-1/z) z^2$
to
$phi0(z)$.
Changing the integration domain is problematic for us:

#let integral-path-text = [

$ integral_(i a)^(i b) phi0(-1/z) z^2 e^(pi i abs(x)^2 z) dif z -> \ integral_(i 1/a)^(i 1/b) phi0(w) e^(pi i abs(y)^2 w) dif w , space a,b in [-infinity, infinity]  $

Divide the segment
$(0, i infinity)$
into
$(0, i)$
and
$(i, i infinity)$,
and find they mirror each other. 
We will change the integration along the segments
$(±1, i infinity)$
to the segments
$(±1, i) -> (i, i infinity)$.

]

#import "./integral_paths_graph.typ": integral-path-graph

#grid(
  columns: (1.2fr, 1fr),
  [
    #integral-path-text
  ],
  [
    #align(right + horizon)[#integral-path-graph]
  ]
)


#speaker-notes-slide[

The problem is that even if we have a way to go from $phi0(-1/z)$ to $phi0(z)$,
the change in the integration domain prevents us from returning to the exact same function.

However, we can partition $(0, i infinity)$ into $(0, i)$ and $(i, i infinity)$,
and find that each one is the mirror image of the other under the Fourier transform and the substitution we applied.

We want to avoid integration along the real axis, so we choose to split the segments
$(±1, i infinity)$
into
$(±1, i) -> (i, i infinity)$.
]

==

$

a(r) &=  integral_(-1)^(i) phi0(-1/(z+1)) (z+1)^2  e^(pi i r^2 z) dif z \
     & -2 integral_0^(i) phi0(-1/z) z^2  e^(pi i r^2 z) dif z \
     & + integral_(1)^(i) phi0(-1/(z-1)) (z-1)^2  e^(pi i r^2 z) dif z \
     & + integral_(i)^(i infinity) [
       phi0(-1/(z+1)) (z+1)^2  e^(pi i r^2 z) \ &#h(4em) -2 phi0(-1/z) z^2  e^(pi i r^2 z)  + phi0(-1/(z-1)) (z-1)^2  e^(pi i r^2 z)
     ] dif z

$ <a-as-sum-of-3-finite-integrals-and-one-i-to-inf-integral>


#speaker-notes-slide[

In total, we get this “monster”, which is essentially the relatively simple function we started with, but after we have brought it as close as possible to the form of a Laplace transform.
]

==

We will look at the Fourier transform of the integral over the segment
$(-1,i)$
under the substitution
$w=-1/z$, and we get:

$
integral_(-1)^(i) phi0(-1/(z+1)) (z+1)^2  e^(pi i abs(x)^2 z) dif z -> integral_(1)^(i) phi0(-1 - 1/(w-1)) (w-1)^2 e^(pi i abs(y)^2 w) dif w
$

And we find that
it is almost the integral over the segment
$(1,i)$!

If
$phi0$
is 1-periodic (a property we have already proven for modular forms), then
$ phi0(-1 - 1/(w-1)) = phi0(1 + -1 - 1/(w-1)) = phi0(- 1/(w-1)) $

In other words,
the part composed of the integrals over the segments
$(±1, i)$
is essentially a eigenfunction (under the assumption that
$phi0(z+1) = phi0(z)$
).


#speaker-notes-slide[

If we now look at the Fourier transform of the integral over the segment from -1 to i,
under a substitution we get the integral from 1 to i of the following function, which is almost the integral over the segment from 1 to i in the original function.

If $phi0$ is 1-periodic, then we can get exactly the integral from 1 to i.

(click)

In other words, the integrals over the segments ±1 to i are already eigenfunctions (assuming 1-periodicity).
]

==

$

a(r) &= #text(green.darken(26%))[ $ integral_(-1)^(i) phi0(-1/(z+1)) (z+1)^2  e^(pi i r^2 z) dif z $ ] \
     & -2 integral_0^(i) phi0(-1/z) z^2  e^(pi i r^2 z) dif z \
     & #text(green.darken(26%))[ $ + integral_(1)^(i) phi0(-1/(z-1)) (z-1)^2  e^(pi i r^2 z) dif z $] \
     & + integral_(i)^(i infinity) [
       phi0(-1/(z+1)) (z+1)^2  e^(pi i r^2 z) \ &#h(4em) -2 phi0(-1/z) z^2  e^(pi i r^2 z)  + phi0(-1/(z-1)) (z-1)^2  e^(pi i r^2 z)
     ] dif z

$ <a-as-sum-of-3-finite-integrals-and-one-i-to-inf-integral>


 #speaker-notes-slide[

(click)

That is, the integrals over the segments from minus one to i are already eigenfunctions (assuming 1-periodicity).

If we continue this line of thought, we must have that the second part of the function, that is, the integrals over the segments from zero to i and from i to i infinity, are also eigenfunctions.

]

==

To ensure that 
$a(r)$
is entirely a eigenfunction, it must hold that the second part of the function, 
that is, the segments 
$(0, i)$ 
and 
$(i, i infinity)$, 
are also eigenfunctions.

We can apply the Fourier transform to it and compare, obtaining that if:

$
phi0(-1/(z+1)) (z+1)^2 -2 phi0(-1/z) z^2 + phi0(-1/(z-1)) (z-1)^2 = 2phi0(z)
$ <phi0-func-eq>

then the second part of 
$a(r)$
will also be a eigenfunction, meaning that 
$a(r)$
itself will be exactly the eigenfunction we are seeking!

#speaker-note[
To summarize:
We need to find a function, denoted by 
$phi0$,
such that:

1.#[
$phi0$
is 1-periodic, meaning
$phi0(z)=phi0(z+1)$.
]

2.#[
$phi0$
satisfies the
equation
]

3.#[
$phi0(z)=O(e^(-2pi i z))$
 as
$Im z -> infinity$,
for the convergence of the integrals both absolutely and conditionally.
]

4.#[
$phi0(-1/z) z^2  e^(pi i r^2 z) \to 0$
 as
$Im z -> infinity$,
so that we can change the integration contour.
]

If we also require that
$a(x)$
itself is a Schwartz function,
these conditions will ensure that we obtain a eigenfunction.

We know that it will have the required zeros when 
$r > sqrt(2)$
but we will need to check the additional zero when
$r = sqrt(2)$.
]


 #speaker-notes-slide[

[
If we continue this line of thought, we must have that the second part of the function, that is, the integrals over the segments from zero to i and from i to i infinity, are also eigenfunctions.
]

We can apply the Fourier transform and compare the result to the original expression, obtaining that if the following functional equation holds, which closely resembles the properties we obtained for modular forms,

then every A is a eigenfunction as we desired!
]

== A Function that Satisfies the Conditions


We now introduce 
$j$,
also known as the _elliptic j-invariant_.

It is defined as follows:

$ j  = (1728 E_4^3) / (E_4^3 - E_6^2) $ <elliptic-j-invariant>

#speaker-note[
At first glance, the expression looks very strange. Why choose a fraction composed of "polynomials" of modular forms?
]

We can verify and find that indeed
$ 
j(-1 / z) = (1728 E_4^3(-1/z)) / (E_4^3(-1/z) - E_6^2(-1/z)) = 
(1728 z^12 E_4^3(z)) / (z^12 E_4^3(z) - z^12 E_6^2(z)) = j(z)
$

That is, 
$j$
is indeed invariant in this sense. 

Immediately, we can also obtain that
$j(z) = j(z+1)$.

#speaker-note[
Thanks to the development, the roles of the various 
$E_k$'s
become clear, as well as the specific exponents chosen for them. We also immediately see where the constant $1728$ in the numerator comes from.
]

 #speaker-notes-slide[

We will begin constructing a function that satisfies the required conditions.

We start by introducing a function called the
_elliptic j-invariant_.

At first glance, it is a rather strange expression: why choose a fraction composed of "polynomials" of modular forms?

But if we substitute minus one over z,
the different roles of the 
$E_k$'s
become clear:
for example,
$E_4$
replaces minus one over z
with z raised to the 4th power,
and raising the entire form to the 3rd power gives z raised to the 12th power, which exactly cancels with the z's in the denominator.

The constant $1728$ is required for normalizing the Fourier expansion of J.
]

==

Similar to the function
$j$,
we define two helper functions:

$

phi_(-2) := (-1728 E_4 E_6) / (E_4^3 - E_6^2) , #h(4em)
phi_(-4) := (1728 E_4^2) / (E_4^3 - E_6^2) \

$

These are functions that have the properties

$

phi_(-2)(-1 \/ z) = z^(-2) phi_(-2)(z) \

phi_(-4)(-1 \/ z) = z^(-4) phi_(-4)(z)

$

(and of course both are 1-periodic)

#speaker-note[
These are the "building blocks" that we can use whenever we want to eliminate unwanted exponents of
$z$.
]

#speaker-notes-slide[

Similarly, we define two more functions:
phi minus two and phi minus four.

phi minus two of minus one over z
becomes
Z^-2 times phi minus two of Z,
and phi minus four...
]

==

Using these functions, we define

$

phi0 := phi_(-4) E_2^2 + 2phi_(-2) E_2 + j - 1728

$

and
$phi0$
fulfills all the properties we need!
The functional equation follows from:
$
phi0(-1 / z) = phi0(z) - (12i) / pi dot 1 / z (phi_(-4) E_2 + phi_(-2)) - 36 / (pi^2) 1 / (z^2) phi_(-4) (z)
$ <phi0-one-over-z-tranform>
and most of the other properties can be proven using the Fourier expansion of
$phi0$,
which can be computed using an automatic tool:

$

phi0(z) &= 518400 q + 31104000 q^2 + 870912000 q^3 + 15697152000 q^4 + O(q^5) 

$

in combination with asymptotic bounds on the coefficients.

#speaker-notes-slide[

And now we can build phi0, the function we need!

phi0 is defined as:
[phi minus four times E2 squared...]

Part of the reasoning behind the polynomial is that
E2 adds
Z^2,
so phi minus two cancels it.

Similarly
E2 squared and phi minus four.

J
and the constant allow us to obtain a Fourier expansion that contains only terms from
Q to the first power
and above.

The functional equation we need is easy to derive from the following equation, which is easy to check by substitution.
]

== The Function's Values on the Lattice and at the Origin 

To find the values of
$a(r)$
for
$r=0$
and
$r=sqrt(2)$,
we use the following estimate of
$phi0$:

#speaker-note[
Derived from the functional equation we saw earlier
]

// This is Prop 3 (38) in the paper.

$

phi0(i/t) t^2 = 36 / (pi^2) e^(2 pi t) - 8640 / pi t + 18144 / (pi^2) + O(t^2 e^(-2 pi t))

$ <phi0-estimate-for-approx>

For
$r > sqrt(2)$
we can compute
the appropriate integral over the first three terms and obtain:

$

tilde(a)(r) = 4i sin^2(pi r^2 \/ 2)  (&
    36 / (pi^3 (r^2 - 2)) - ... 
    &+ integral_0^infinity (t^2 phi0(i / t) - 36 / (pi^2 e^(2 pi t)) + ...) e^(-pi r^2 t) dif t
)

$

$tilde(a)(r)$
is analytic in some neighborhood of
$[0, infinity)$
as is
$a(r)$ and are equal. By taking a limit:
$
a(sqrt(2)) &= lim_(r->sqrt(2)) 4i sin^2(pi r^2 \/ 2)  (36 / (pi^3 (r^2 - 2)) - ...) 
& = 0, a(0) = (-i 8640) / pi
$

#speaker-notes-slide[

As a reminder, when we defined
$A$,
we focused on the domain where
$R$
is greater than $sqrt(2)$.
We know that the required zeros are obtained there, but we need to check its values at the origin and when
$R = sqrt(2)$.

If we use the functional equation we saw earlier, we can use the Fourier expansions of the various EKs to obtain the following estimate of $phi0$:

In the domain
$r > sqrt(2)$,
we can compute the integral over the first terms and obtain a form like
$A$:
We have the original sine, and we extracted the first three terms from the integral,
and subtract them back inside.

We obtained a function analytic in some neighborhood from zero to infinity. But also
the original
$A$
is analytic in that neighborhood, and therefore they are equal over the entire domain.

Now, we can use the new form we found to find the values via a simple limit.
For example, $A$ at
$sqrt(2)$
is obtained via the following limit, which can be solved using L'Hôpital's rule and yields 0 (as required).
]

== The Other Eigenfunction

We still need a function that satisfies
$b(x) = -hat(b)(x)$.
We use the functions from before and denote

#let psiI = $psi_I$
#let psiT = $psi_T$
#let psiS = $psi_S$

$

// This is the form used in the Cohn paper (before 7.2),
// except they have -32 (but we place the -4 in the b-for-r func).

psiI := 128 (theta_00^4 + theta_01^4) / theta_10^8 + 128 (theta_01^4 - theta_10^4) / theta_00^8 \

// Original psi_T and psi_S from the paper.
// psiT = 128 (theta_00^4 + theta_01^4) / theta_10^8 + 128 (theta_00^4 + theta_10^4) / theta_01^8 \
// psiS = -128 (theta_00^4 + theta_10^4) / theta_01^8 - 128 (theta_10^4 - theta_01^4) / theta_00^8 \

$

Using it, we define the following function 


$ b(r) := -4 sin^2(pi r^2 \/ 2) integral_0^(i infinity) psiI(z) e^(pi i r^2 z) dif z  $ <b-for-r-gt-sqrt-2>

and
$b(x)$
is the required eigenfunction,
with
$b(0) = 0, b(sqrt(2)) = 0$.

#speaker-notes-slide[

So we found the first eigenfunction, but we still need a eigenfunction of minus one.

We will use the theta functions we defined earlier to define $psi_I$.

We substitute it into a similar template to the one we used before, and similarly, we can decompose the sine and check that it indeed satisfies this.

We can also perform an estimate and compute an integral and limit as we did before, and check that it attains zeros at the two additional points.

]

== Checking the Appropriate Bounds <proof-g-satisfies-lin-bounds>

Define

#filled-block[
$ g(x) := (pi i) / 8640 a(x) + i / (240 pi) b(x) $
]

and this is the magic function we need!
\
We can substitute and obtain that
$abs(x) = sqrt(2) => g(x) = 0$.

Since
$a(0) = (-i 8640) / pi$
and
$b(0) = 0$,
we also get that
$g(0) = hat(g)(0) = 1 > 0$.

We only need to prove the other two conditions, namely:
#align(center)[
$hat(g)(y) >= 0$ for all $y in RR^n$

$g(x) <= 0$ for all $|x| >= r$
]

#speaker-notes-slide[

Almost done! We define
G
as the following sum of the two functions we found, and this will be the magic function we need!

We can verify by substitution that the first condition, namely that
G
and
hat(g)
are equal and positive at 0, is satisfied.

However, we still need to prove the other two conditions: namely that
hat(g)
is non-negative everywhere
and G is negative or zero for all
$x >= R$.
]

==
Sketch of the proof that
$g(x) <= 0$
for all
$abs(x) > sqrt(2)$.
We present the function as follows:

$
g(r) = pi / 2160 sin^2(pi r^2 \/ 2) integral_0^(infinity) A(t) e^(- pi r^2 t) dif t, space

A(t) = -t^2 phi0(i \/ t) - 36/pi^2 psiI(i t) \
$

We prove that
$A(t) < 0$
in the segment, so $g$
does not take positive values in 
$r>sqrt(2)$.

#let A0 = $A_0^((n))$
#let A0m = $A_0^((m))$
#let A06 = $A_0^((6))$

In $(0,1]$ we denote
$A0$:
// A(t) &= -t^2 phi0(i \/ t) + 36/pi^2 t^2 psiS(i \/ t), 
$A(t) = A0(t) + O(t^2 e^(- pi n \/ t))$,
with error
$abs( A(t) - A0m(t) )$.

#speaker-note[
That is,
$A0(t)$
is a function that approximates 
$A(t)$
around 0, up to a factor
$t^2 e^(- pi n \/ t)$.
]

#let R0(m) = $(t^2 + 36/pi^2) sum_(n=#m)^infinity 2 e^(2 sqrt(2) pi sqrt(n)) e^ (-pi n \/ t)$
#let Rinf(m) = $(t^2 + 12/pi t + 36/pi^2) sum_(n=#m)^infinity 2 e^(2 sqrt(2) pi sqrt(n)) e^ (-pi n t)$

Using bounds on the coefficients of the Fourier expansion, we can bound the error.

$A06(t) < 0$,
and
$abs( A(t) - A06(t) )  &<= abs(A06(t))$,
therefore 
$A(t) < 0$
when
$t in (0,1]$.

Similar estimates can be obtained in the segment
$(1, infinity)$,
and a similar process allows us to prove that
$hat(g)(y) >= 0$ for all $y in RR^n$.

#speaker-notes-slide[

I want to present only a sketch of the proof that
G is not positive for all
$x$ greater than sqrt(2).

If we denote by
A-large
the inner function in the integral,
it is enough to show that
A
is negative in the entire integration segment, and from this we conclude that
G
does not take positive values for all
$r>sqrt(2)$.

We divide the segment from 0 to infinity into two parts, and in each of them we find an estimate of
A.

// In the segment zero to one, we can denote 
// A-zero-N,
// which is a function that approximates 
// $A$
around 0, up to a factor
// $t^2 e^(- pi n \/ t)$.

Using bounds on the coefficients of the Fourier expansion of the two functions we constructed, we can bound the error of this estimate, and when
N=6
we can see that
A-zero-6 is less than 0 in the segment, and also that the error is smaller than it, 
therefore also
A
itself is negative in the segment.

A similar estimate can be obtained in the segment one to infinity, and a similar process allows us to prove that
hat(g)
is non-negative.
]

==

So,
$g(x)$
is the required magic function,
and therefore the maximal density in
$RR^8$
is
$ vol(B_(sqrt(2) \/ 2)^8) = pi^4 / 384 = 0.2538 ... $

Since this is the density we obtained for
$E_8$,
we have proven that it is indeed the packing with the highest possible density in this dimension.
$square$

#speaker-notes-slide[

So!
G
is the required magic function for us, and thus the maximal density in
R8
is
pi^4 / 384.

Since this is the density we obtained for the packing
E8,
we have proven that it is indeed the packing with the highest possible density in this dimension.

Thank you!
]

==
