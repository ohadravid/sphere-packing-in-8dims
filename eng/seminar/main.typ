#import "template.typ": *
// Take a look at the file `template.typ` in the file panel
// to customize this template and discover how it works.
#show: project.with(
  title: "Seminar on Sphere Packing in Dimension 8",
  authors: (
    "Ohad Ravid",
  ),
)

#let vol = math.op("vol", limits: true)


// Set numbering of equations to only include labeled ones.

#set heading(numbering: "1.")

#import "i-figured.typ"

// this resets all figure counters at every level 1 heading.
// custom figure kinds must be added here.
#show heading: i-figured.reset-counters.with(extra-kinds: ("atom",),)
// this show rule is the main logic, custom prefixes for custom figure kinds
// can optionally be added here.
#show figure: i-figured.show-figure.with(extra-prefixes: (atom: "atom:",))

// a similar function exists for math equations
#show math.equation: i-figured.show-equation.with(only-labeled: true)

// Remove the prefix before the equation number when referencing it.

#set ref(supplement: "")

#outline(title: "Table of Contents", depth: 2, indent: auto)

#pagebreak()

= Introduction

Sphere packing is a geometric problem dealing with the seemingly simple question:

#align(center)[
What is the most efficient way to arrange spheres in space?
]

And not only is the question seemingly simple, it is even easy to guess the correct answer:

For example, in three-dimensional space, the common stack of oranges in a supermarket is indeed the most efficient!

We find the true depth and difficulty when we try to prove that this is indeed the correct answer: that no more efficient arrangement exists.

This seminar will address the answer (and the question!) in $RR^8$, one of the only two high dimensions for which the answer is known.

== Plan of Action

We will begin, as expected, with the definitions that allow us to discuss the problem with the required precision (what does “most efficient density” mean? and how do we define “packing”?),
and we will present the main packing we will focus on, called $E_8$.

#let CnE = [Cohn and Elkies]
#let Viaz = [Viazovska]

Then we will prepare the ground and present our first main theoretical claim, the result of $CnE$,
with which we can transform the geometric problem into an analytical one: \
We will see that finding a function that satisfies several unique properties
enables us to prove that for a given dimension there is a bound on the maximum density, and therefore if a certain packing has that density, it is also the most efficient in that dimension.

Finally, we will prove that the function found by $Viaz$ indeed meets all these properties, and therefore shows that $E_8$ is the most efficient packing in $RR^8$.

#pagebreak()

= Initial Definitions

The sphere packing problem deals with the question: what is the largest portion of
$RR^n$
that can be covered by equal-radius spheres that do not intersect?

#let Pkg = math.cal([P])

_A sphere packing_
(_sphere packing_)
is a non-empty subset of
$RR^n$
of spheres with equal radius whose interiors are disjoint (though they may be tangent).

#let Ball = $ B_r^n $
#let HalfBall = $ B_(r \/ 2)^n $

We denote by
$Ball(x)$
the closed ball with radius
$r$
whose center is at
$x$.

The _upper density_
of a given packing
$Pkg$
is

$ limsup_(r->infinity) vol(Ball(0) sect Pkg) / vol(Ball(0)) $

where we use the standard Euclidean space and
$vol$
is the Lebesgue measure.

#let spherepackingdensity = $Delta_(RR^n)$

The _
sphere packing density in $RR^n$
_, denoted by
$spherepackingdensity$,
is the supremum of all upper densities.

At first glance, this definition does not guarantee that this density is actually attained by any packing, but we rely on a result proven by
Groemer @Groemer1963,
according to which there exists a packing
$Pkg$
for which

$ lim_(r->infinity) vol(Ball(x) sect Pkg) / vol(Ball(x)) = spherepackingdensity $

equally for all
$x in RR^n$.
In other words, there is a packing that achieves the maximal density.

== Lattices and Periodic Packings

A _lattice_ in
$RR^n$
is a subgroup of rank
$n$
composed of a basis of
$RR^n$
together with all linear combinations with integer coefficients of that basis (such a combination is also called an _integral combination_, and the basis is called an _integral basis_).

Formally, if
$B$
is some basis of
$RR^n$,
then the lattice
$Lambda$
can be expressed as:

$ Lambda := {a_1 v_1 + ... + a_n v_n | a_i in ZZ , v_i in B} $

Using the concept of a lattice, we can also define a _periodic packing_.

A sphere packing
$Pkg$
is _periodic_
if there exists a lattice
$Lambda$
such that
$Pkg$
is invariant under translation by any element of
$Lambda$. In symbols:

$ Pkg = Pkg + v,  forall v in Lambda $

A _lattice packing_
is a periodic packing in which all the centers of the spheres lie on a lattice, up to translation (since a lattice always includes the origin, which is not necessarily desired for packings).

For example, if we take the basis

$ B = {(0,4), (4,0)} subset RR^2 $

we can build from it the lattice

$ Lambda = {(4n,4m) | n,m in ZZ} $

Since we started from a lattice, the packing
$Pkg_1 = {B^2_1(x) | x in Lambda}$
is a lattice packing, and also
$Pkg_2 = {B^2_1(x + (1, 1)) | x in Lambda}$
is a lattice packing.

However, there are, of course, periodic packings that are not lattice packings.

#pagebreak()

For instance,

$ Pkg_3 = {B^2_(1\/2)(x) | x in Lambda union (Lambda + (1,0))} $

is a periodic packing (it is certainly invariant under translation by any element of
$Lambda$),
but it is not a lattice packing.
We can show this as follows:

Assume that the set of centers of the spheres in
$Pkg_3$
is a lattice, denote it by
$Lambda_B$,
and denote its corresponding basis by
$B = {(a,b), (c,d)}$.

Note that a vector
$(x,y)$
is in the lattice if and only if
$
x mod 4 in {0,1}\
y mod 4 = 0
$

A vector not in the lattice is certainly not an element of the basis, and it is also easy to see that
$(1,t) in.not B$,
otherwise
$(2,2t) in Lambda_B$.

In fact, any vector
$(x,y)$
with
$x mod 4 = 1$
cannot be in the basis for the same reason, \
so we conclude that
$a,b,c,d$
are all divisible by
$4$.
But then we cannot obtain, for example,
$(5,0) in Lambda_B$.

This means there is no such basis, and therefore the packing is not a lattice packing.

== Density of a Lattice Packing

Since we are dealing with the density of sphere packings, it is natural that we would want to compute the density of lattice packings.

We can do this more easily if we think of the lattice as tiling the space with parallelepipeds.

The _fundamental cell_
of a sphere packing based on a lattice
$Lambda$
with basis
$B$
can be represented as:

$ C := {x_1 v_1 + ... + x_n v_n | 0 <= x_i <= 1 , v_i in B} $

Translations of the fundamental cell by elements of
$Lambda$
tile the space, and the lattice packing places a sphere in each lattice point of this tiling.

Since there is one sphere per copy of the cell, for any radius
$r$
we can compute the density using the ratio
$ vol(Ball) / vol(C) $

We get the volume of the sphere from the formula

$ vol(Ball) = pi^(n \/ 2) / (n \/ 2)! r^n $

(from Calculus 3),
and we compute the volume of the parallelepiped using the determinant.

In total, we find that the density of a lattice packing is

$ vol(Ball) / vol(RR^n \/ Lambda) $ <packing-vol>

The radius is usually chosen based on the _minimal vector length_ of the lattice
$Lambda$,
i.e., the shortest length of any non-zero lattice vector.

If this length is
$d$,
then the largest radius we can use is
$r = d \/ 2$
to ensure that the spheres are tangent.

#pagebreak()

== Definition and Basic Properties of $E_8$

We will deal with the packing
$E_8$,
which is a highly symmetric sphere packing in
$RR^8$.

We present the lattice on which it is based,
$Lambda_8$,
and prove several of its important and unique properties.

$ Lambda_8 = { (x_1, ... , x_8) in ZZ^8 union (ZZ + 1/2)^8 mid(|) sum^8_(i=1)x_i equiv 0 (mod 2) } $

In other words, the lattice that contains all vectors whose coordinates are all either integers or all differ from an integer by half, and whose sum is even.

We can also see that
$Lambda_8$
is closed under addition, noting that if

$ 
x in Lambda_8 , y in Lambda_8 
$

then
$ x + y in ZZ^8 union (ZZ + 1/2)^8$
, because exactly one of the following occurs:

$ 
x, y in ZZ^8  &=> x + y in ZZ^8 \
x in ZZ^8 and y in (ZZ + 1/2)^8  &=> x + y in (ZZ + 1/2)^8 \
y in ZZ^8 and x in (ZZ + 1/2)^8  &=> x + y in (ZZ + 1/2)^8 \
x, y in (ZZ + 1/2)^8  &=> x + y in ZZ^8 \
$

And since the sum of even numbers is even,

$
sum^8_(i=1)x_i equiv 0 (mod 2) and sum^8_(i=1)y_i equiv 0 (mod 2) => \
sum^8_(i=1) x_i+y_i = sum^8_(i=1)x_i + sum^8_(i=1)y_i equiv 0 (mod 2)
$

The packing
$E_8$
is the lattice packing based on
$Lambda_8$.

#pagebreak()

=== Choosing a Basis for $Lambda_8$

Since
$E_8$
is a lattice packing, we want to refer to the lattice definition to prove the properties we will need later.
For this, we need a basis.

// From p. 34, after Proposition 12., in D. Zagier:
We will use the following basis, defined relative to the standard basis
$e_1, ... , e_8$ of
$RR^8$: \
$B = {v_1, ... , v_8}$
where
$v_i = e_i - e_(i+1)$
for
$1<=i<=6$,
$v_7 = e_6 + e_7$
and
$v_8 = -1/2 (e_1 + ... + e_8)$.

We will see that this is indeed an integral basis for the lattice.

First, we will show that any integral combination of the basis elements is in the lattice. Let

$ u = z_1 v_1 + ... + z_8 v_8 = (t_1, ..., t_8) | z_i in ZZ $

First, note that the evenness property of
$Lambda_8$
holds:

$ z_1(-1+1) + .. +z_6(-1+1) + z_7 (1+1) + z_8 (-1/2 *8) = 2z_7 -4z_8 equiv 0 (mod 2) $

Since:

$
  t_1 &= z_1 - 1/2 z_8 \
  t_2 &= z_2 - z_1 - 1/2 z_8 \
  ... \
  t_5 &= z_5 - z_4  - 1/2 z_8 \
  t_6 &= z_6 - z_5 + z_7 - 1/2 z_8 \
  t_7 &= z_7 - z_6 - 1/2 z_8 \
  t_8 &= -1/2 z_8 \
$

When
$z_8 equiv 0 (mod 2)$,
we have
$t_i in ZZ$
and thus
$u in ZZ^8$,
and if
$z_8 equiv 1 (mod 2)$,
then
$u in (ZZ+1/2)^8$,
so
$u in Lambda_8$,
as desired.

In the other direction, let
$w= (t_1, ... , t_8) in Lambda_8$.
We will show that there exist
$z_i in ZZ$
such that
$w = z_1 v_1 + ... + z_8 v_8$.

We rely on the same equations we found earlier, and choose
$z_8 = t_8 * -2$.
Immediately we get 
$z_1 = t_1 - t_8$, $z_2 = t_2 + t_1 - 2 t_8$
and by the lattice’s property, we ensure the values are integers at every step.

We can continue to solve the equations one by one until
$z_5 = t_5 + t_4 + t_3 + t_2 + t_1 - 5 t_8$.

Then we have

$
t_6 &= z_6 - (t_5 + ... + t_1 - 5 t_8) + z_7 + t_8 \
t_7 &= z_7 - z_6 + t_8
$

Summing up, we get

$
2 z_7 = t_1 + ... + t_5 + t_6 + t_7 + t_8 - 8 t_8
$

We know that
$sum^8_(i=1)t_i equiv 0 (mod 2)$,
and of course
$8 t_8$
is even, so we can find such a
$z_7$,
and from it also find
$z_6$.

Thus, we have expressed
$w$
as an integral combination of the basis elements,
as desired.

#pagebreak()

=== The Gram Matrix of $Lambda_8$

Instead of proving properties directly via the basis elements, we can use the
Gram
matrix.

It is easy to see that
$angle.l v_i, v_i angle.r = 2$,
and
$angle.l v_i, v_(i+1) angle.r = -1$,
except in the case
$angle.l v_6, v_7 angle.r = 0$.

In addition, the product
$angle.l v_i, v_j angle.r$
vanishes in all other cases, except
$angle.l v_5, v_7 angle.r = -1$.

In other words, the corresponding matrix is

$ (angle.l v_i, v_j angle.r)_(1<=i,j<=8) = mat(delim: "[",
   2, -1,  0,  0,  0,  0,  0,  0; 
  -1,  2, -1,  0,  0,  0,  0,  0; 
   0, -1,  2, -1,  0,  0,  0,  0; 
   0,  0, -1,  2, -1,  0,  0,  0; 
   0,  0,  0, -1,  2, -1, -1,  0; 
   0,  0,  0,  0, -1,  2,  0,  0; 
   0,  0,  0,  0, -1,  0,  2, -1; 
   0,  0,  0,  0,  0,  0, -1,  2; 
) $

This matrix is, of course, symmetric, and it is easy to see that it is also totally positive.

Its characteristic polynomial is

$ t^8 - 16t^7 + 105t^6 - 364t^5 + 714t^4 - 784t^3 + 440t^2 - 96t + 1 $

which is always positive for $t<0$, since it has no negative terms, so we can conclude that all its zeros, i.e., the eigenvalues of the matrix, are strictly positive, and therefore it is indeed totally positive.

=== Additional Properties of $Lambda_8$

$Lambda_8$
is an _integral lattice_,
meaning all inner products between its basis elements are integers (as we saw above).

In addition,
$Lambda_8$
is an _even lattice_,
meaning the squared length of every vector in it is an even integer. We can see this by noting that for all
$m_i in ZZ$,

$ lr(| m_1 v_1 + ... + m_8 v_8 |)^2 &= 2m^2_1 + ... + 2m^2_8 + sum_(1<=i,j<=8) 2m_i m_j angle.l v_i, v_j angle.r $

From the Gram matrix we saw, it follows that the squared length is even, and in particular the distance between any two points in
$Lambda_8$
is of the form
$sqrt(2k)$,
and between two closest points
it is
$sqrt(2)$.

From this it follows that we can choose a lattice packing with
$r = sqrt(2) / 2$
, which we denote by
$E_8$.

Its density, according to
@eqt:packing-vol,
is given by

$ vol(B^8_(sqrt(2) \/ 2)) / vol(RR^8 \/ Lambda_8) = pi^4 / (384 vol(RR^8 \/ Lambda_8)) $

All that remains is to compute
$vol(RR^8 \/ Lambda_8)$
in order to find the packing density.

From the properties of the determinant, we get

$ vol(RR^8 \/ Lambda_8) = lr(| 
det mat(delim: "[",
  <- v_1 ->;
  <- v_2 ->;
  ... ;
  <- v_8 ->;
) 
|) $

#pagebreak()

But we can also write the Gram matrix we found earlier as

$
mat(delim: "[",
  <- v_1 ->;
  <- v_2 ->;
  ... ;
  <- v_8 ->;
)

mat(delim: "[",
  arrow.t , arrow.t ,     , arrow.t ,  ;
  v_1     , v_2     , ... , v_8     ,  ;
  arrow.b , arrow.b ,     , arrow.b ,  ;
)
$

Therefore

$
vol(RR^8 \/ Lambda_8) = sqrt(lr( ( det (angle.l v_i, v_j angle.r)_(1<=i,j<=8) ) ))
$

From the characteristic polynomial we know that
$ det (angle.l v_i, v_j angle.r)_(1<=i,j<=8) = 1 $

so

$
vol(RR^8 \/ Lambda_8) = 1
$

We have thus shown that the density of
$E_8$
is

$ pi^4 / 384 = 0.2538 ... $ <e8-packing-density>

#pagebreak()

=== Duality of $Lambda_8$ <dual-of-lambda-8>

Finally, we need one more property of
$Lambda_8$:
that it is its own _dual lattice_.

For a lattice
$Lambda$
with a certain basis
$v_1, ..., v_n$,
there is a lattice
$Lambda^*$,
called the dual lattice,
with a basis
$v^*_1, ..., v^*_n$
such that

$
angle.l v_i, v^*_j angle.r = cases(
  1 "if" i = j "," "and",
  0 "otherwise"
)
$

This is the dual lattice,
and if
$Lambda = Lambda^*$
we say that
$Lambda$
is it's own dual.

We will prove that
$Lambda_8$
has this property via a more general claim: any lattice whose Gram matrix is
_unimodular_
(i.e., with integer entries and determinant $1$ or $-1$)
is it's own dual.

#let adj = math.op("adj")

Let
$Lambda$
be such a lattice with matrix
$A$.

The adjoint matrix,
$adj(A)$,
satisfies
$A^(-1)=1/det(A) adj(A)$
(Conclusion 4.7.3 in Linear Algebra 1).

Because it is based on minors of the original matrix,
$adj(A)$
is integral,
and since the determinant of
$A$
is
$1$ or
$-1$,
we have that
$A^(-1)$
is also integral.

Denote by
$M: Z^n -> RR^n$
the linear map with respect to the basis
$B = {v_1, ... , v_n}$
of
$Lambda$,
i.e.,

$ M vec(a_1, ... , a_n) := a_1 v_1 + ... + a_n v_n $

Then
$M$
is a matrix whose columns are the basis vectors, and the Gram matrix of
$Lambda$
is
$A = M^T M$.

By the definition of a lattice,
$ Lambda = M ZZ^n $

Denote by
$D$
the basis of the dual lattice
$Lambda^*$.

Directly from the definition we get
$M^T D = I$,
so
$D = (M^T)^(-1)$.

Again, from the definition of the lattice

$ Lambda^* = D ZZ^n = (M^T)^(-1) ZZ^n $

We can replace
$(M^T)^(-1) = M (M^T M)^(-1)$
to get

$ Lambda^* = M (M^T M)^(-1) ZZ^n = M A^(-1) ZZ^n $

Since
$A^(-1)$
is integral, we have
$A^(-1) ZZ^n subset.eq ZZ^n$.

Also,
$A$
is integral,
so
$A ZZ^n subset.eq ZZ^n$.
Thus
$I ZZ^n = A^(-1) A ZZ^n subset.eq A^(-1) ZZ^n$.

Hence
$ A^(-1) ZZ^n = ZZ^n $

and therefore
$ Lambda^* = M ZZ^n = Lambda $

We have shown that the lattice is it's own dual.

We already saw that the Gram matrix of
$Lambda_8$
has integral entries and determinant $1$,
so we conclude that it is it's own dual.

#pagebreak()

= Using Harmonic Analysis to Obtain Density Bounds <harmonic-analysis-and-enc>

After defining what density is and after defining the lattice packing
$E_8$
and its main properties, we now address the central problem at hand: how can we prove what the highest possible density is in a given dimension?

If we can prove that the highest possible density in
$RR^8$
equals the density of
$E_8$,
we will have shown that $E_8$ is indeed the optimal packing in that dimension.

This problem is answered in the work of
$CnE$,
who proved that auxiliary functions with certain properties, which we will detail shortly, can be used to bound the maximum possible density in a given dimension.

This seems surprising, since lattices are inherently discrete, and the density problem is purely geometric. How can analysis of functions help us?

The key insight lies in the relationship between a function and its Fourier transform, via the #[_Poisson Summation Formula_].
We will present the main elements of the theory in the next section, and then move on to proving the main theorem using it.

// Next 3 parts:
// Based on "Lectures in Analysis Vol1 - Fourier Analysis"

== Schwartz Functions

We will deal with functions from
$RR^n$
to
$RR$
(and sometimes with complex-valued functions of a real variable).

#let Sw = $cal(S)(RR^n)$

_Schwartz functions_
are a family of functions denoted by
$Sw$
or shortly,
$cal(S)$.

These are all infinitely differentiable functions that satisfy

$ sup_(x in RR^n) lr(| x^beta (diff / (diff x) )^alpha f(x) |) < infinity $

for all
$alpha,beta in NN$,
where
$(diff / (diff x) )^alpha$ is a sequence of $alpha$ partial derivatives (in some sequence of variables).

In other words, this set of functions is a space (in the usual sense of a function space) of functions whose value and the value of all their derivatives decrease “very rapidly”, i.e., faster than any polynomial.

The results we will prove also hold under less strict conditions, but by restricting ourselves to Schwartz functions we can avoid technical difficulties.
Since the construction in the $Viaz$'s proof (which we will discuss in the following chapters) produces Schwartz functions, these results will suffice.


== Fourier Transforms in $RR^n$ of Schwartz Functions

We define the Fourier transform of a Schwartz function
$f$
as follows:

$ hat(f)(xi) = integral_(RR^n) f(x) e^(-2pi i x dot xi) dif x , "for" xi in RR^n $

We integrate over the entire space, where
$x dot xi$
denotes the inner product between the two vectors.
We also use the common notation
$f(x) -> g(xi)$
to indicate that
$g$
is the Fourier transform of
$f$. \
The Fourier transform of Schwartz functions has several interesting properties:

/// Prop 2.1 in LiA @stein2011fourier page 181.

Let
$f in Sw$, then

1. $f(x + h) -> hat(f)(xi)e^(2pi i xi dot h)$ for $h in RR^n$.
2. $f(x) e^(-2pi i x dot h) -> hat(f)(xi + h)$ for $h in RR^n$.
3. $f(delta x) -> delta^(-n) hat(f)(delta^(-1) xi)$ for $delta > 0$.
4. $(diff / (diff x) )^alpha f(x) -> (2pi i xi)^alpha hat(f)(xi)$.
5. $(-2pi i x)^alpha f(x) -> (diff / (diff xi) )^alpha hat(f)(xi)$.

/// Proofs are on page 136.

#pagebreak()

We provide the proofs for properties 4 and 5 in the one-dimensional case.

Let $f in cal(S)(RR)$.
For property 4, we can find
$hat(f')$
using integration by parts. For
$N in NN$,
we have

$ integral_(-N)^N f'(x) e^(-2 pi i x xi) dif x = [f(x) e^(-2 pi i x xi)]_(-N)^N + 2 pi i xi integral_(-N)^N f(x) e^(-2 pi i x xi) dif x $

Taking the limit as
$N -> infinity$,
we find that indeed
$f'(x) -> 2pi i xi hat(f)(xi)$.

For property 5, we show that
$hat(f)$
is differentiable, and that its derivative equals the Fourier transform of
$-2 pi i x f$.

We show that the difference between them can be made arbitrarily small, hence the derivative exists and they are equal. We get:

$ (hat(f)(xi + h) - hat(f)(xi)) / h - hat((-2 pi i x f))(xi) = integral_(-infinity)^infinity f(x) e^(-2 pi i x xi) [(e^(-2 pi i x h) - 1) / h + 2 pi i x] dif x $

We know that both
$f(x)$
and
$x f(x)$
decrease rapidly (since they are Schwartz functions).

Let
$epsilon > 0$.
There exists
$N in NN$
such that
$integral_(|x|>=N) |f(x)| dif x <= epsilon$,
and also
$integral_(|x|>=N) |x| |f(x)| dif x <= epsilon$.

Additionally, for
$|x|<=N$,
since
$e^(-2 pi i x h)$
is differentiable (as a function of $h$)
around $0$
with derivative
$-2pi i x$, \
we deduce that there is an
$h_0$
such that for all
$|h| < h_0$,

$ abs((e^(-2 pi i x h) - 1) / h + 2 pi i x) <= epsilon / N $

Hence, for all such
$h$,

$ abs( (hat(f)(xi + h) - hat(f)(xi)) / h - hat((-2 pi i x f))(xi) ) <= 
integral_(-N)^N abs(f(x) e^(-2 pi i x xi) [(e^(-2 pi i x h) - 1) / h + 2 pi i x]) dif x  + C epsilon <= C' epsilon $

Thus, the derivative exists and is given by the Fourier transform of
$-2pi i x f(x)$.

From these last two properties, we can conclude that the Fourier transform converts differentiation to multiplication (up to a simple polynomial factor), \
meaning that *$Sw$ is closed under the Fourier transform*.

For Schwartz functions, we also have the important identity:

$ f(x) = integral_(RR^n) hat(f)(xi) e^(2pi i x dot xi) dif xi $ <fourier-inversion-formula>

which is the _inverse Fourier transform_.

/// Optional: Prove (based on Theorem 1.9, p141)

== Radial Functions <radial-functions>

We call a function
$f$
_radial_
if it depends only on
$|x|$.
That is, if there is a function
$f_0(u), u >= 0$
such that
$f(x) = f_0(|x|)$.

/// Optional: Corollary 2.3 - Fourier of a radial function is radial
An important property of radial functions is that they satisfy
$hat(hat(f)) = f$,
as follows from the inverse Fourier transform
@eqt:fourier-inversion-formula:

$ 
hat(hat(f))(y) &= integral_(RR^n) hat(f)(xi) e^(-2pi i xi dot y) dif xi 

&= integral_(RR^n) hat(f)(xi) e^(2pi i xi dot (-y)) dif xi 

&= f(-y) 

&= f_0(|-y|) = f_0(|y|) 

&= f(y)
$

#pagebreak()

== The Poisson Summation Formula

To apply analytical tools to the density problem, we use the Poisson Summation Formula.

This formula expresses the duality between summing a function over a lattice and summing its Fourier transform over the dual lattice.

// Prop 4.2 in Cohn 2016

_The Poisson Summation Formula_ states that for
$f in Sw$,
the identity

$ sum_(x in Lambda) f(x) = 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^*) hat(f)(y) $ <poisson-sum>

holds, i.e., they are equal up to a constant factor.


// Based on Theorem 3.1 (p153) in Lectures in Analysis Volume 1

We start by proving the result in one dimension.

_The one-dimensional Poisson Summation Formula_ states that for all
$f in cal(S)(RR)$:

$ sum_(n=-infinity)^(infinity) f(n) = sum_(n=-infinity)^(infinity) hat(f)(n) $ <poisson-sum-for-r>

Define

$ F_1(x) := sum_(n=-infinity)^(infinity) f(x+n) $

Since
$f$
decreases very rapidly,
the series converges absolutely on the real line, and $F_1$ is continuous.
It is easy to see that this new function is 1-periodic.

Using Fourier series expansion, we reach another 1-periodic function:

$ F_2(x) := sum_(n=-infinity)^(infinity) hat(f)(n) e^(2 pi i n x) $

We can view this in analogy to Fourier series, treating it as a discrete version of

$ f(x) = integral_(-infinity)^infinity hat(f)(xi) e^(2pi i x xi) dif xi $

We know that two functions are identical if their Fourier expansions match.

For
$F_2$,
the $m$-th coefficient is exactly
$hat(f)(m)$.
For $F_1$, we have

$ 
a_m &= integral_0^1 (sum_(n=-infinity)^(infinity) f(x+n)) e^(-2 pi i m x) dif x \
    &= sum_(n=-infinity)^(infinity) integral_0^1  f(x+n) e^(-2 pi i m x) dif x \
                                                                // (y-n) => e^-2pi*i*mn = 1
    &= sum_(n=-infinity)^(infinity) integral_n^(n+1) f(y) e^(-2 pi i m y) dif y \
    &= integral_(-infinity)^infinity f(y) e^(-2pi i m y) dif y \
    &= hat(f)(m)
$

where interchanging summation and integration is allowed because
$f in cal(S)$.

So indeed
$F_1 = F_2$,
and setting
$x=0$
gives the final result.

#pagebreak()

// Prop 4.4 in Cohn 2016
// Based on https://see.stanford.edu/materials/lsoftaee261/chap8.pdf p360

Generalizing to $n$ dimensions is immediate, and we get:

$ sum_(x in ZZ^n) f(x) = sum_(x in ZZ^n) hat(f)(x) $ <poisson-sum-for-r-n>

But if we consider
$ZZ^n$
as a lattice, how can we extend the result to general lattices?

As before, let
$M: Z^n -> RR^n$
be the linear map with respect to the basis
$B = {v_1, ... , v_n}$
of
$Lambda$.
Then
$M$
is a matrix whose columns are the basis vectors,
and
$Lambda = M ZZ^n$.

First,
$ sum_(x in Lambda) f(x) = sum_(x in ZZ^n) f(M x) $


To use
@eqt:poisson-sum-for-r-n,
we need the Fourier transform of
$f(M x)$.

Set
$g(x)=f(M x)$
and from the formula we get

$ hat(g)(xi) = integral_(RR^n) f(M x) e^(-2pi i xi dot x) dif x $

Continuing:

$ 
hat(g)(xi) &= integral_(RR^n) f(M x) e^(-2pi i xi dot x) dif x \
          &= integral_(RR^n) f(M x) e^(-2pi i xi dot (M^(-1) M x)) 1/abs(det(M)) abs(det(M)) dif x \
          &= 1/abs(det(M)) integral_(RR^n) f(M x) e^(-2pi i xi dot (M^(-1) u)) dif u
$

where we used $u=M x$, and $abs(det(M))=vol(RR^n \/ Lambda)$:

$
hat(g)(xi) &= 1/vol(RR^n \/ Lambda) integral_(RR^n) f(u) e^(-2pi i (M^(-1))^T xi dot u) dif u = 1/vol(RR^n \/ Lambda) hat(f)((M^(-1))^T xi)
$

Substitute back:

$ sum_(x in Lambda) f(x)  = sum_(x in ZZ^n) f(M x) 
= sum_(xi in ZZ^n) hat(g)(xi) = 1 / vol(RR^n \/ Lambda) sum_(xi in ZZ^n) hat(f)((M^(-1))^T xi) $

If we let
$D$
be the basis of the dual lattice
$Lambda^*$,
then
$D = (M^T)^(-1)$ \
and thus
$Lambda^* = D ZZ^n = (M^T)^(-1) ZZ^n$.

Hence,

$ sum_(x in Lambda) f(x) = 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^*) hat(f)(y) $

as desired.

#pagebreak()

== Linear Programming Bounds <lin-bounds>

We now have all the tools needed to prove the main theorem in the work of
$CnE$.

*Let*
$f in Sw$
and let
$0 < r in RR$,
such that:

1. $f(0) = hat(f)(0) > 0$
2. $hat(f)(y) >= 0$ for all $y in RR^n$
3. $f(x) <= 0$ for all $|x| >= r$

*Then*
the maximal packing density in
$RR^n$
is at most
$vol(HalfBall)$.

We will first prove the claim for lattice packings and then extend the result to arbitrary packings.

Let
$Lambda$
be a lattice in
$RR^n$.
We can assume without loss of generality that
$Lambda$
has a minimal vector length
$r$,
since the density of the lattice packing remains the same if we scale the lattice (and with it the minimal vector length).
Then there is a lattice packing based on it consisting of spheres of radius
$r\/2$.
As we saw in
@eqt:packing-vol,
it follows that its density is

$ vol(HalfBall) / vol(RR^n \/ Lambda) $

Thus, we want to show that
$vol(RR^n \/ Lambda) >= 1$.

By the Poisson summation formula
@eqt:poisson-sum,

$ sum_(x in Lambda) f(x) = 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^*) hat(f)(y) $

Write the left sum as

$ sum_(x in Lambda) f(x) = f(0) + sum_(x in Lambda , x != 0) f(x) $

Recall that for $x \neq 0$,
$|x| >= r$,
and from conditions (1) and (3) we get

$ sum_(x in Lambda) f(x) <= f(0) $

Similarly, we can bound the right-hand side:

$ hat(f)(0) <= sum_(y in Lambda^*) hat(f)(y) $

Thus,

$ (hat(f)(0)) / vol(RR^n \/ Lambda) <=  f(0) $

Since we require that
$f(0) = hat(f)(0)$,
we have

$ vol(RR^n \/ Lambda) >= 1 $

as desired.

#pagebreak()

=== Extension to Periodic and General Packings

We proved that if such a function exists, no *lattice* packing can have higher density. But what about general packings? \
// From the paper: "given an optimal packing, 
// one can approximate it by taking the spheres contained in a large box 
// and repeating them periodically throughout space, 
// and the density loss is negligible if the box is large enough."
Every general packing can be approximated by a *periodic* packing:
we take all spheres in a sufficiently large box and tile them throughout space. The loss in density from this process is negligible.

Thus, it suffices to prove the claim for periodic packings. If we show that the bound also holds for them,
then there cannot be a general packing with a higher density (since we could approximate it by a periodic packing beyond the bound we found). 
Let there be a periodic packing of spheres whose centers lie on translations of the lattice
$Lambda$
by
$N$
vectors
$t_1, ..., t_N$.

As before, we can assume without loss of generality that the radius of the spheres is
$r\/2$, with density:

$
(N vol(HalfBall)) / vol(RR^n \/ Lambda)
$

So we need to prove that
$ vol(RR^n \/ Lambda) >= N$.
Now, the sum takes the form:

$ sum_(j,k=1)^N sum_(x in Lambda) f(t_j - t_k + x) $

Recall that the Fourier transform turns a translation into a multiplication, so if we use the Poisson summation formula
@eqt:poisson-sum
again, we get

$ sum_(j,k=1)^N sum_(x in Lambda) f(t_j - t_k + x) = sum_(j,k=1)^N  [ 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^*) hat(f)(y) e^(2 pi i y dot (t_j - t_k)) ] $

We can rewrite the final series as:
$sum_(j,k=1)^N e^(2 pi i y dot t_j) e^(-2 pi i y dot t_k) = 
(sum_(j=1)^N e^(2 pi i y dot t_j)) (sum_(k=1)^N e^(-2 pi i y dot t_k))$.

Using the properties of complex conjugation, we have

$
sum_(j,k=1)^N sum_(x in Lambda) f(t_j - t_k + x) = 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^*) hat(f)(y) abs(sum_(j=1)^N e^(2 pi i y dot t_j))^2
$

As before, from conditions (1) and (3) we deduce that

$ sum_(j,k=1)^N sum_(x in Lambda) f(t_j - t_k + x) <= N f(0)   $

We can bound the right-hand side similarly:

// We get N^2 for y=0 since Σexp^0 = N.

$ N^2 hat(f)(0) <= sum_(y in Lambda^*) hat(f)(y) abs(sum_(j=1)^N e^(2 pi i y dot t_j))^2 $

Hence,

$ (N^2 hat(f)(0)) / vol(RR^n \/ Lambda) <= N f(0) $

Since
$f(0) = hat(f)(0)$,
we obtain

$ vol(RR^n \/ Lambda) >= N $

as desired.

#pagebreak()

== Zeros of the Function and Further Conclusions <zeros-of-the-magic-function>

Before we search for functions that meet the conditions detailed in
@lin-bounds,
we can note a few general properties that will help us in the search.

First, all conditions on the desired function are linear and invariant under rotation. \
Therefore, we can consider
$f$
as a _radial_ function, i.e., a function of a single variable
($abs(x)$).

Another property we can deduce from the way we proved the claim is that such a function must vanish on the lattice (except at the origin)—and similarly, its Fourier transform must also vanish on the dual lattice.

Formally: Let 
$f$
be such a function, and let
$Lambda$
be a lattice in
$RR^n$
with minimal vector length
$r$.

Then the density of the lattice packing
$Lambda$
equals the bound 
$vol(HalfBall)$
if and only if
1. $x in Lambda \/ {0} => f(x) = 0$
2. $y in Lambda^* \/ {0} => hat(f)(y) = 0$

We start by noting from
@eqt:packing-vol
that the density equals the bound if and only if

$ vol(HalfBall) / vol(RR^n \/ Lambda) = vol(HalfBall) $

i.e.,
$vol(RR^n \/ Lambda) = 1$.

Using the Poisson summation formula
@eqt:poisson-sum,
we have

$ f(0) + sum_(x in Lambda , x != 0) f(x) = sum_(x in Lambda) f(x) = 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^*) hat(f)(y) = 1 / vol(RR^n \/ Lambda) hat(f)(0) + 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^* , y != 0) hat(f)(y) $

In one direction, if
$vol(RR^n \/ Lambda) = 1$
then

$ f(0) + sum_(x in Lambda , x != 0) f(x) = hat(f)(0) + sum_(y in Lambda^* , y != 0) hat(f)(y) $

From condition (1) in
@lin-bounds,
$f(0) = hat(f)(0)$,
so

$ sum_(x in Lambda , x != 0) f(x) = sum_(y in Lambda^* , y != 0) hat(f)(y) $

Using conditions (2) and (3) in
@lin-bounds,
all elements in both sums are $0$.

The other direction is immediate:
if we set these values to zero, we get

$ f(0) + sum_(x in Lambda , x != 0) 0  = 1 / vol(RR^n \/ Lambda) hat(f)(0) + 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^* , y != 0) 0 $

and using condition (1) in
@lin-bounds
we have

$ 1 = 1 / vol(RR^n \/ Lambda) => vol(RR^n \/ Lambda) = 1 $

#pagebreak()

Since we know that
$f$
remains non-positive for
$abs(x) >= r$,
if $f$
is the desired “magic” function, then it must vanish on the
$E_8$
lattice (except at the origin).

Since we can think of it as a radial function
$f_0(x)$,
we deduce that it must have zeros at all
$x = sqrt(2k), k = 1, 2, ...$,
and all these zeros have even order except in the case
$k = 1$,
where we expect an odd-order zero.

Similarly, since
$E_8$
is self-dual, we deduce that
$hat(f_0)$
has even-order zeros at all
$y = sqrt(2k), k = 1, 2, ...$.

We can even schematically plot the expected shape of the function and its Fourier transform:

#figure([
#align(center)[  
  #import "@preview/cetz:0.2.2": canvas, plot, draw
  
  #let style = (stroke: black, fill: rgb(0, 0, 200, 75))

  #canvas(length: 1cm, {
    plot.plot(
      name: "f-plot",
      size: (8, 2),
      axis-style: "school-book",
      x-tick-step: none,
      x-ticks: ((0, 0), (calc.sqrt(2), $sqrt(2)$), (calc.sqrt(4), $sqrt(4)$), (calc.sqrt(6), $sqrt(6)$)),
      y-tick-step: none,
      y-ticks: ((-10, -10), (10, 10)),
      y-domain: (-10, 10),
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
    draw.content(((0,2.5), "-|", "f-plot"), $f_0$)
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
], caption: [A schematic plot of functions], supplement: "") <f0-and-f0-hat-plots>
#pagebreak()
= Modular Forms <modular-forms>

At first glance, we have advanced significantly: not only do we know the required definitions and proofs, we even have a "recipe" for finding the desired function:
we need to find a function of a single variable that vanishes at every point of the
$E_8$
lattice, and whose Fourier transform also vanishes at these points.

However, this problem turns out to be very difficult.

While it is possible to construct a function that has zeros at the required points, and it is possible to construct a function whose Fourier transform has such zeros, there is no general or simple way to construct a single function that simultaneously meets both requirements.

// Splitting into f_- and f_+ as eigenfunctions

Nevertheless, we can simplify further even now.
As we saw in@radial-functions,
for radial functions we have
$hat(hat(f)) = f$.

Therefore, for every
$f$
that meets the required conditions, we can define

$ g_(+1) = (f+hat(f)) / 2  , #h(2em) g_(-1) = (f-hat(f)) / 2 $

Applying the Fourier transform to them yields

$ hat(g_(+1)) =& (hat(f)+f) / 2 =&  g_(+1) , #h(2em)
  hat(g_(-1)) =& (hat(f)-f) / 2 =& -g_(-1) $

A _Fourier eigenfunction_ is a function for which there exists some scalar
$alpha in RR$
such that
$hat(f) = alpha f$.

Thus,
$g_(+1)$
is an eigenfunction with
$alpha = 1$,
and
$g_(-1)$
is an eigenfunction with
$alpha = -1$.

They have the required zeros just like
$f$,
and we can recover
$f$
from them by:

$ f = g_(+1) + g_(-1) => hat(f) = g_(+1) - g_(-1) $

The problem remains very challenging, but now the two new functions are independent of each other, so we can attempt to construct each separately.

== Laplace Transform

Our connection to modular forms, which have broad theoretical significance, arises from seeking building blocks for our special function using the _Laplace transform_ of _Gaussians_.
\
A _Gaussian_,
$f(x) = e^(-t pi abs(x)^2)$,
satisfies

$ e^(-t pi abs(x)^2) -> t^(-n/2) e^(-pi abs(y)^2 \/ t) $

Since we can easily compute the Fourier transform of a Gaussian, we can pick a simple function
$g(t)$,
and use it to choose a combination of Gaussians:

$ f(x) = integral_0^infinity e^(-t pi abs(x)^2) g(t) dif t $

This formula is called the _Laplace transform_ of the function
$g$.
\
As long as
$g$
is reasonably simple, we can use the Gaussian’s properties to get the Fourier transform of
$f$
through a simple substitution:

$ hat(f)(y) =& integral_0^infinity t^(-n/2) e^(-pi abs(y)^2 \/ t) g(t) dif t 
// Replace t -> 1/t.
            =& integral_0^infinity e^(-pi abs(y)^2 t) t^(n/2 - 2)  g(1 \/ t) dif t
$ <fourier-transform-of-laplace-transform>

Thus, for
$epsilon = \pm 1$,
if
$g(1 \/ t) = epsilon t^(2-n/2) g(t)$
then
$hat(f) = epsilon f$,
and we seek functions that satisfy this equation.

#pagebreak()

== Introduction to Modular Forms

_Modular forms_
are functions satisfying certain functional equations.

We will focus on a small set of particularly useful forms for our case.

#let hh = $frak(h)$
#let Im = $op("Im")$
#let Re = $op("Re")$

Let
$hh$
be the upper half-plane:

$ hh = { z in CC | Im z > 0 } $

// Based on "Definition 3" in "An Introduction to Modular Forms".

For a function
$F$
and an integer
$k$, 
we say that
$F$
is a _weakly modular form of weight k_ if it satisfies:

$ F((a z + b) / (c z + d)) = (c z + d)^k F(z) $

for all
$mat(a, b; c, d) in "SL"(2, RR)$.

We call
$F$
a _modular form_ if it is also holomorphic on
$hh$
and bounded as
$Im z -> infinity$.

#let thetaseries = $Theta_Lambda$
#let thetaseriesdual = $Theta_(Lambda^*)$

== The Theta Function of a Lattice - $thetaseries (z)$ <theta-series>

For every lattice
$Lambda$,
we define its theta function:

$ thetaseries(z) = sum_(x in Lambda) e^(pi i abs(x)^2 z) $

This function converges whenever
$Im z > 0$
and defines an analytic function on
$hh$.

It has an important property:

For any lattice
$Lambda$
in
$RR^n$,

$ thetaseries(z) = 1 / vol(RR^n \/ Lambda) (i/z)^(n \/ 2) thetaseriesdual (-1 \/ z) $

for all
$z in hh$.

We prove this using the Poisson summation formula on the inner function
$e^(-t pi abs(x)^2)$.

As seen before, this Gaussian satisfies

$ e^(-t pi abs(x)^2) -> t^(-n/2) e^(-pi abs(y)^2 \/ t) $.

We can analytically continue this result, and from Poisson’s formula
@eqt:poisson-sum
we get:

$ sum_(x in Lambda) e^(-t pi abs(x)^2) = 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^*) t^(-n/2) e^(-pi abs(y)^2 \/ t) $

Set
$z = i t$,
so
$t = - i z$
and

$ sum_(x in Lambda) e^(z i pi abs(x)^2) = 1 / vol(RR^n \/ Lambda) sum_(y in Lambda^*) (- i z)^(-n/2) e^(pi abs(y)^2 \/ z i) $

Now simplify the first factor on the right side:

$ (- i z)^(-n/2) = (z / i)^(-n/2) = (i/z)^(n/2) $

If we substitute 
$-1/z$
into
$thetaseriesdual$,

$ thetaseriesdual(-1/z) = sum_(y in Lambda^*) e^(- pi i abs(y)^2 / z) = sum_(y in Lambda^*) e^(pi abs(y)^2 \/ z i) $

Substituting back, we get:

$ thetaseries(z) = 1 / vol(RR^n \/ Lambda) (i/z)^(n \/ 2) thetaseriesdual (-1 \/ z) $

as desired.

We will use the following theta functions, defined on the lattice
$ZZ$:

$ 
theta_00 (z) = sum_(n in ZZ) e^(pi i n^2 z) \
theta_01 (z) = sum_(n in ZZ) ((-1)^n e^(pi i n^2 z)) \
theta_10 (z) = sum_(n in ZZ) e^(pi i (n + 1/2)^2 z)
$ <theta-z-funcs>

They satisfy:

$
z^(-2) theta_00^4(-1 / z) = -theta_00^4(z) \
z^(-2) theta_01^4(-1 / z) = -theta_10^4(z) \
z^(-2) theta_10^4(-1 / z) = -theta_01^4(z) \
$

and

$
theta_00^4(z + 1) = theta_01^4(z) \
theta_01^4(z + 1) = theta_00^4(z) \
theta_10^4(z + 1) = -theta_10^4(z)
$

#pagebreak()

#let rtl-mark = [\u{200F}]
== Eisenstein Series - $E_k$

The Eisenstein series is a function defined as follows

$ E_k (z) = 1 / (2 zeta (k)) sum_((m,n) in ZZ^2 \ (m,n) != (0,0)) 1 / (m z + n)^k $

where $zeta$ is the Riemann zeta function (
  $zeta(k) = sum_(n=1)^infinity 1/n^k$
).

Although there is an overlap in notation between the lattice $E_8$ and $E_k$ when $k=8$, fortunately we will only use $k \in \{2,4,6\}$, and therefore we will restrict ourselves to the standard notation without defining a different notation.

// $E_k(-1/z) = z^4 E_k(z)$ and $E_k(z+1) = E_k(z)$ [for $k > 2$, $k$ even]

For every even $k > 2$, we can obtain the identities

$ 
E_k (z+1) = E_k (z) , space space space
E_k (-1 \/ z) = z^k E_k  (z) 
$ <eisen-k>

by simple substitution.

and in detail:

$
sum_((m,n) in ZZ^2 \ (m,n) != (0,0)) 1 / (m (z + 1) + n)^k 
&= sum_((m,n) in ZZ^2 \ (m,n) != (0,0)) 1 / (m z + m + n)^k 
= sum_((m,n) in ZZ^2 \ (m,n) != (0,0)) 1 / (m z + n)^k

\

sum_((m,n) in ZZ^2 \ (m,n) != (0,0)) 1 / (m (-1/z) + n)^k 
&= sum_((m,n) in ZZ^2 \ (m,n) != (0,0)) 1 / ((n z - m) / z)^k 
= sum_((m,n) in ZZ^2 \ (m,n) != (0,0)) z^k / (n z - m)^k 
= z^k sum_((m,n) in ZZ^2 \ (m,n) != (0,0)) 1 / (m z + n)^k 
$

According to our definitions, $E_k$ are modular forms of weight $k$, where $k > 2$.

// Edge case for $k=2$:
    // $E_2(-1/z) = z^4 E_2(z) - 6i z pi$ and $E_2(z+1) = E_2(z)$

For $k=2$, there's a problem: the series converges _conditionally_ and not _absolutely_. We relied on absolute convergence in order to change the order of summation of the series which we cannot do now.

We can choose to define the order of summation as follows:

$ E_k (z) &= 1/(2zeta(k)) sum_(n!=0) 1/n^k + 1/(2zeta(k)) sum_(m!=0) sum_(n in ZZ) 1/(m z + n)^k \ &= 1/(zeta(k)) sum_(n=1)^infinity 1/n^k + 1/(zeta(k)) sum_(m=1)^infinity sum_(n=-infinity)^infinity 1/(m z + n)^k $ <e-k-as-inf-sum>

And of course, the previous results will continue to hold when $k > 2$.

For $k=2$, we can again obtain the identity by substitution

$ E_2 (z+1) = E_2 (z)  $

but the counterpart of the identity 

$ E_k (-1 \/ z) = z^k E_k  (z)  $

will get a slightly different form:

$ E_2 (-1 \/ z) = z^2 E_2 (z) - 6i z \/ pi $   <eisen-2>

We say that $E_2$ is a _quasimodular form_ (_quasimodular form_) of weight 2.

We will prove this equation


=== Deriving the Functional Equation for $E_2$ #rtl-mark

For $k=2$, the series is

$ 1 / (2 zeta(2)) sum_((m,n) in ZZ^2 \ (m,n) != (0,0)) 1 / (m z + n)^2 $

// p19 in "1-2-3 of Modular Forms".
To simplify some computations, define:

$ G_2 (z) = zeta(2) E_2 (z) $

Since
$zeta(2) = pi^2/6$,
we have
$E_2(z) = 6/pi^2 G_2(z)$.

Although the series is not absolutely convergent, it is very close. For any
$epsilon>0$,
define

$ G_(2,epsilon) (z) = 1 / 2 sum_((m,n) in ZZ^2 \ (m,n) != (0,0)) 1 / ((m z + n)^2 |m z + n|^(2epsilon)) $

which does converge absolutely.
This new function satisfies:

$ G_(2,epsilon)((a z + b) / (c z + d)) = (c z + d)^2 |c z + d|^(2epsilon) G_(2,epsilon)(z) $

We will show that

$ lim_(epsilon -> 0) G_(2,epsilon)(z) = G_2(z) - pi / (2Im(z)) $

Since
$G^*_2(z) := G_2(z) - pi / (2Im(z))$
behaves like a weight-2 modular form, we get:

$ 
G^*_2((a z + b) / (c z + d)) &= (c z + d)^2 G^*_2(z) \
$

In particular,

$
G_2(-1\/z) - pi / (2Im(-1/z)) &= z^2 [G_2(z) - pi / (2Im(z))]
$
But if we denote
$z=x + i y$,
we can compute that
$Im(-1\/z) = Im(-1 / (x+i y)) = y / (x^2 + y^2)$.

That is
$
G_2(-1\/z) - pi(x^2+y^2) / (2y) &= z^2 [G_2(z) - pi / (2y) ] \

G_2(-1\/z) &= z^2 G_2(z) + pi/(2y) (x^2 + y^2 - z^2) \

G_2(-1\/z) &= z^2 G_2(z) + pi/(2y) (-2 x y i + 2 y^2) \

// Since -iz = -ix + y.

G_2(-1\/z) &= z^2 G_2(z) - pi i z
$

and therefore also

$ E_2 (-1 \/ z) = z^2 E_2 (z) - 6i z \/ pi $

as we wanted.

#pagebreak()

To complete the proof, we must show the existence of the limit and its value.

Define $G_2$ directly using the series from
@eqt:e-k-as-inf-sum:

$ G_2(z) = 1/2 sum_(n!=0) 1/n^2 + 1/2 sum_(m!=0) sum_(n in ZZ) 1/(m z + n)^2 $.

#let Ieps=$I_epsilon$

Define an auxiliary function
$Ieps(z)$
for
$z in hh$
and
$epsilon > -1/2$:

$ Ieps(z) = integral_(-infinity)^infinity 1/((z+t)^2 abs(z+t)^(2epsilon)) dif t $

For each
$epsilon > 0$,

$ 
G_(2,epsilon) - sum_(m=1)^(infinity) Ieps(m z) = &sum_(n=1)^infinity 1/n^(2+2epsilon) \
+ &sum_(m=1)^(infinity) sum_(n=-infinity)^(infinity) [  1 / ((m z + n)^2 |m z + n|^(2epsilon)) - integral_(n)^(n+1) 1/((m z+t)^2 abs(m z+t)^(2epsilon)) dif t ] 
$

By using the mean value theorem and bounds on derivatives, we find the bracketed expression is
$O(abs(m z + n)^(-3-2epsilon))$,
and thus the sums converge absolutely and uniformly locally.

So the sums on the right side of the equation absolutely converge, and locally uniformly as well.
Therefore, the limit exists when
$epsilon -> 0$, 
and if we replace
$epsilon$
with
$0$
we will obtain exactly 
$G_2$:

$ 
sum_(n=1)^infinity 1/n^2 + sum_(m=1)^(infinity) sum_(n=-infinity)^(infinity) [  1 / (m z + n)^2 - integral_(n)^(n+1) 1/((m z+t)^2) dif t] &= 
 sum_(n=1)^infinity 1/n^2 + sum_(m=1)^(infinity) sum_(n=-infinity)^(infinity) [  1 / (m z + n)^2 - 0] \
// Note the change from n=1..inf to n∈Z,n!=0 (also for m).
&= 1/2 ( sum_(n!=0) 1/n^2 + sum_(m!=0) sum_(n in ZZ) 1/(m z + n)^2 ) \
&= G_2(z)
$

On the other hand, for
$epsilon > -1/2$
it holds that

$ 
Ieps(x + i y) &= integral_(-infinity)^infinity 1/((x + i y + t)^2 abs(x + i y +t)^(2epsilon)) dif t \
&= integral_(-infinity)^infinity 1/((x + i y + t)^2 ((x + t)^2 + y^2)^(epsilon)) dif t \
$

We can replace
$t$
with
$u=t+x$,
and get, after renaming the variable back, that

$
&= integral_(-infinity)^infinity 1/((t + i y)^2 (t^2 + y^2)^(epsilon)) dif t \
$

Then we can factor out
$y$, substituent
$u=t/y => u'=1/y$
and get (after renaming the variable) that

$
Ieps(x + i y) &= integral_(-infinity)^infinity 1/(y^2(t/y + i)^2 y^(2epsilon)(t^2/y^2 + 1)^(epsilon)) dif t \
// (t/y)' = 1/y
&= integral_(-infinity)^infinity 1/y^(2+2epsilon)  1/((t + i)^2 (t^2 + 1)^(epsilon)) dot 1/y dif t 
&= 1/y^(1+2epsilon) integral_(-infinity)^infinity 1/((t + i)^2 (t^2 + 1)^(epsilon)) dif t 
$

#pagebreak()

If we denote

// Same as I(eps) but with z=i.
$ 
I(epsilon) = Ieps(i) = integral_(-infinity)^infinity 1/((t + i)^2 (t^2 + 1)^(epsilon)) dif t $
    
we obtain that
$ Ieps(x + i y) = I(epsilon)/(y^(1+2epsilon)) $
    
Note that for
$I(0)$,
we obtain that
    
$ I(0) = integral_(-infinity)^infinity 1/((t + i)^2) dif t = lr(-1/(t+i)|)_(-infinity)^infinity=0 $
    
and that for
$I'(0)$,
we can find 
$ I'(epsilon) $
by
    
$ 
I'(epsilon) &= partial / (partial epsilon) integral_(-infinity)^infinity 1/((t + i)^2 (t^2 + 1)^(epsilon)) dif t \
&=  integral_(-infinity)^infinity partial / (partial epsilon) [1/((t + i)^2 (t^2 + 1)^(epsilon))] dif t 
&= - integral_(-infinity)^infinity (log(1 + t^2))/((i + t)^2 (1 + t^2)^(epsilon)) dif t
$
    
so
$
I'(0) &= - integral_(-infinity)^infinity log(t^2 + 1)/((t + i)^2) dif t = lr(
  ((1+log(t^2 + 1)) / (t+i) - 1 / tan(t) ) |
) _(-infinity)^infinity = -pi $
    
We add to this the fact that
$zeta(1+2epsilon) = 1/(2epsilon) + O(1)$,
and infer that
    
$ 
// remove the O(1) from zeta.
lim_(epsilon->0) (I(epsilon) zeta(1+2epsilon))/y^(1+2epsilon) &= lim_(epsilon->0) I(epsilon) / (y^(1+2epsilon) 2epsilon) \
// l'hopital's rule for 0/0.
&= lim_(epsilon->0) (I'(0)) / (2 epsilon dot 2y^(1+2 epsilon)log(y) + 2y^(1+2epsilon)) 
&= -pi/(2y) 
$
    
That is
    
$ lim_(epsilon -> 0) [G_(2,epsilon)(z) - sum_(m=1)^(infinity) Ieps(m z)] &= G_2 (z) \ 
G^*_2(z) - lim_(epsilon -> 0) sum_(m=1)^(infinity) I(epsilon)/((m y)^(1+2epsilon)) &= G_2(z) \
G^*_2(z) - lim_(epsilon -> 0) I(epsilon) /y^(1+2epsilon) sum_(m=1)^(infinity) 1/(m^(1+2epsilon)) &= G_2(z)\
G^*_2(z) - lim_(epsilon->0) (I(epsilon) zeta(1+2epsilon))/y^(1+2epsilon) &= G_2(z) \
G^*_2(z) + pi/(2y) &= G_2(z)
$
    
and therefore
$G^*_2(z) &= G_2(z) - pi/(2 Im(z))$,
as we wanted to prove.

#pagebreak()

=== Fourier Expansion of $E_k$

A 1-periodic function has a Fourier expansion of the form:

$ g(z) ~ sum_(n = -infinity)^infinity a_n e^(2 pi i n z) $

From now on let
$q = e^(2 pi i z)$.

To find the Fourier expansion for
@eqt:e-k-as-inf-sum,
we use the identity:

$ sum_(n in ZZ) 1/(z + n) = pi / tan(pi z), space z in CC \/ ZZ $ <sum-one-over-z-plus-n>

The left sum converges conditionally, and we interpret it as the limit
$lim_(N->infinity) sum_(-N)^N 1/(z+n)$.

The right-hand side is 1-periodic, and its Fourier expansion results from:

$ pi / tan(pi z) = pi cos(pi z) / sin(pi z) = pi i (e^(pi i z) + e^(-pi i z)) / (e^(pi i z) - e^(-pi i z)) = - pi i (1+q)/(1-q) = -2pi i (1/2 + sum_(r=1)^infinity q^r) $

If we differentiate
@eqt:sum-one-over-z-plus-n
term by term
$k-1$
times, we obtain (after dividing by
$(-1)^(k-1)(k-1)!$
)
since

$ sum_(n in ZZ) 1 / (z + n)^k = (-1)^(k-1) / (k-1)! d^(k-1)/(d z^(k-1)) (pi / tan(pi z)) $

Then we can substitute and obtain (again after differentiating term by term) because

$ sum_(n in ZZ) 1 / (z + n)^k = (-2pi i)^k / (k-1)! sum_(r=1)^infinity r^(k-1) q^r $

Note that this identity holds for all
$z in hh, k >= 2$.

We substitute back into
@eqt:e-k-as-inf-sum
and obtain the following Fourier expansion:


// Fourier expansion in the paper

$ E_k (z) &= 1 + 1/(zeta(k)) sum_(m=1)^infinity [(-2pi i)^k / (k-1)! sum_(r=1)^infinity r^(k-1) q^(m r)] \
&= 1 + 1/(zeta(k)) (2pi i)^k / (k-1)! sum_(m=1)^infinity sigma_(k-1)(m) q^m\ 
&= 1 + 2 / (zeta (1 - k)) sum_(m=1)^infinity sigma_(k-1)(m) q^m $ <e-k-fourier-exp>

where
$sigma_(k-1)(m)$
is a function that returns the sum of the divisors of
$m$,
each raised to the power
$k-1$.

#pagebreak()


=== Uses of $E_k$

Now we introduce $j$, the _elliptic j-invariant_.
This is a modular function with the fundamental property:

$ j(-1 \/ z) = j(z) $

defined by:

$ j = (1728 E_4^3) / (E_4^3 - E_6^2) $ <elliptic-j-invariant>

At first glance, this looks strange: why choose such a fraction of “polynomials” of modular forms?

If we use the properties of
$E_k$
from
@eqt:eisen-k,
particularly
$E_k (-1 / z) = z^k E_k (z)$,
we find that

$ 
j(-1 \/ z) = (1728 E_4^3(-1/z)) / (E_4^3(-1/z) - E_6^2(-1/z)) = 
(1728 z^12 E_4^3(z)) / (z^12 E_4^3(z) - z^12 E_6^2(z)) = j(z)
$

so $j$ is indeed invariant. Clearly,
$j(z)=j(z+1)$
as well.

From this construction, we see the roles of the different
$E_k$,
and the specific exponents chosen for them. We also see where the constant $1728$ comes from.

=== Using the Fourier Expansion of $E_k$

// Based on "Proposition 5" in Zagier - 1-2-3 Of Modular Forms

From
@eqt:e-k-fourier-exp,
we get:

$ 
E_4 (z) &= 1 + 2 / (zeta (-3)) ( sigma_(3)(1) q^1 + sigma_(3)(2) q^2 + ... ) \
        &= 1 + 2 / (1/120) ( 1dot q^1 + 9 dot q^2 ) \
        &= 1 + 240q + 2160q^2 + ...
$ <e4-fourier>

And also: 

$ 
E_6 (z) &= 1 - 504 q - 16632 q^2 - ... \
E_2 (z) &= 1 - 24 q - 72 q^2 - ...
$ <e2-and-e6-fourier>

Similarly:

$ 
E_4^2 &= 1 + 480 q + ... \
E_4^3 &= 1 + 720 q + ... \
E_6^2 &= 1 - 1008 q + ...
$ <ek-powers-fourier>

The combination
$E_4^3 - E_6^2$
is itself useful, with expansion

$ E_4^3 - E_6^2 = 1728 q + ... $

Normalizing it:

$ Delta (z) = 1/1728 (E_4^3 - E_6^2) $

Thus

$ j(z) = E_4^3(z) / Delta(z) $

We can then use automated tools to obtain expansions for
$j$:

$ j(z) = q^(−1) + 744 + 196884 q + 21493760 q^2 + 864299970 q^3 + 20245856256 q^4 + O(q^5) $ <j-foureir>

We also want asymptotic formulas for the $n$th coefficient of a modular form.

While the formula is complex, for our purposes the ability to bound these coefficients will be sufficient for approximations.

Consider a simple case:

From
@eqt:e-k-fourier-exp,
for $n>=1$,
the $n$th coefficient of
$E_k$
satisfies
$a_n = O(n^(k-1))$,
since
$n^(k-1) <= sigma_(k-1)(n) < \zeta(k-1) n^(k-1)$.

For example, the $n$th coefficient in the Fourier expansion of
$E_2$
satisfies, for
$n>=1$,

$ |c_(E_2(z))(n)| <= C_1 n $

We know
$c_(E_2(z))(0) = 1$,
so

$
|E_2(z)| = |1 + sum_(n=1)^infinity c_(E_2(z))(n) e^(2 pi i n z)| 
<= 1 + sum_(n=1)^infinity C_1 n |e^(2 pi i n z)|
= 1 + C_1 sum_(n=1)^infinity n e^(-2 pi n Im(z))
$

For any 
$Im(z) > 0$,
this series converges, and

$
sum_(n=1)^infinity n e^(-2 pi n Im(z)) = (e^(2 pi Im(z))) / (1 - e^(2 pi Im(z)))^2
$

And
// https://www.wolframalpha.com/input?i=sum+(ne%5E(-2pi*n*t)),+n%3D2+to+infinity
$

sum_(n=1)^infinity n e^(-2pi Im(z)) &= e^(-2pi Im z) + sum_(n=2)^infinity n e^(-2pi n Im(z)) \
&= e^(-2pi Im z) + (e^(-2 pi Im(z)) (2 e^(2 pi Im(z)) - 1))/(e^(2 pi Im(z)) - 1)^2 \
&= e^(-2pi Im z) (1 + (2 e^(2 pi Im(z)) - 1)/(e^(2 pi Im(z)) - 1)^2)
$


Hence, for
$Im(z) > 1/2$,
we can bound the expression independently of $z$, and thus

$ |E_2(z)| <= 1 + C_2 e^(-2 pi Im(z)) $ <e-k-estimation>

#pagebreak()

= #Viaz's Function <viaz-func>

== Constructing a function with appropriate roots

We saw in the previous chapter
that modular forms, combined with the Laplace transform, are suitable for our search for eigenfunctions of the Fourier transform.

But how can we construct a function that has the appropriate roots?
One straightforward way is by multiplication with a simple function that attains similar roots, and this is the method by which
#Viaz
proved the existence of the required functions.

The function
$sin^2(pi abs(x)^2 \/ 2)$
has second-order zeros for all
$x = sqrt(2k), k=1,2,...$.
As we saw in the figure
@fig:f0-and-f0-hat-plots,
these are almost the zeros we desire, apart from an unwanted fourth-order zero at
$x=0$,
and the zero at
$x=sqrt(2)$
which is of second order rather than first (or another odd order). But these are problems that (it turns out) can be addressed.
If we take the Laplace transform from the previous section, we obtain a template that can fit the two eigenfunctions we are searching for:

$ sin^2(pi abs(x)^2 \/ 2) integral_0^infinity e^(-t pi abs(x)^2) f(t) dif t $ <eign-fn-pattern>

Note that it is not sufficient just to find eigenfunctions:
we must also compensate for the unsuitable roots and ensure that the final function meets the appropriate bounds from section
@lin-bounds.

== Conditions for suitable eigenfunctions

// Intro

We begin by searching for the eigenfunction with eigenvalue 1. Assume that we have a function:

$ a(x) = sin^2(pi abs(x)^2 \/ 2) integral_0^infinity e^(-t pi abs(x)^2) g_0(t) dif t $

// From g_0 to h_0(1/t) * t^2

We want to know when
$hat(a) = a$.
As a start, let us assume that
$abs(x)>sqrt(2)$,
since there the behavior of the function should be simpler.
/*
// Note about the b(x) eigen func.

The search for the function
$b$
for which
$hat(b) = -b$
is similar, and we will touch on it briefly at the end of this section, so that we can prove the required bounds.
*/
It is important to remember that we are looking for _non-necessary_ conditions. We do not need tools to characterize _all_ functions that can serve in proving the bound, but rather just one example of such a function.
We will try to make educated guesses to narrow down the "search area," and remember what assumptions we used to get there.

Let us begin by trying to approach the form of the Laplace transform, since we saw that for a sufficiently simple combination of Gaussians, we can obtain the Fourier transform of the function more easily.

// About a(r) vs a(x)

Note a technical point related also to notation:
When we formulated the statements about the Fourier transform
in previous sections, we dealt with functions
$f(x) : RR^n -> RR$
and denoted their Fourier transform by
$hat(f)(y)$,
but in section
@zeros-of-the-magic-function
we saw that we only need a radial function defined for
$r in RR_(>=0)$.

So we define
$a(r)$
as a function of the variable
$r >= 0$.
To maintain consistent notation,
whenever we refer to its Fourier transform, we will refer to it as
$a(x)$,
replacing
$r^2$
with the squared norm of the vector
$x$.
Of course, we will need to justify this extension
later, but it will not be a great difficulty.

As we saw in
@eqt:fourier-transform-of-laplace-transform,
we are want to obtain the following form for the Fourier transform of our function:

$ a(x) -> integral_0^infinity e^(-pi abs(y)^2 t) t^2  f(1 \/ t) dif t $

If we assume that
$a$
is an eigenfunction (that is,
$hat(a) = a$),
we can refine its definition a bit if we choose
$g_0(t) = h_0(1\/t) t^2 $:
$ a(r) = sin^2(pi r^2 \/ 2) integral_0^infinity e^(-pi r^2 t) t^2  h_0(1 \/ t) dif t $



#pagebreak()

// From integral 0->∞ to integral 0->i∞

Another preliminary move we can make is to change the domain of integration.
We want to work in
#hh,
the upper half-plane, so that we can use the useful results we proved for modular forms.

#let phi0 = $phi.alt_0$

Moreover, we can simplify even further if we look for a function
$phi0 : i RR -> i RR$.

We can denote:

$ i phi0 (1/(i t)) = h_0(1/t) $

The choice of where to "place" the
$i$ factors
is somewhat arbitrary, but it will fit very well with our next step, as we will see shortly.

Substitute:

$ a(r) = sin^2(pi r^2 \/ 2) integral_0^infinity e^(-pi r^2 t) t^2  i phi0(1 / (i t)) dif t $


To transition to the integration segment
$(0, i infinity)$,
we perform the substitution
$z= i t => t = -i z$
to obtain:

// Almost (37), but without the 4

$ a(r) &= sin^2(pi r^2 \/ 2) integral_0^(i infinity) (-i z)' e^(- pi r^2 (-i z)) i phi0(-1/z) (-i z)^2 dif z \
 &= sin^2(pi r^2 \/ 2) integral_0^(i infinity) -i e^(pi r^2 i z) i phi0(-1/z) (-1) z^2 dif z \
 &= - sin^2(pi r^2 \/ 2) integral_0^(i infinity) phi0(-1/z) z^2  e^(z pi i r^2) dif z 
$


// From sin² ∫ to ∫(e^2pi - e^-2pi) using sin ident

So far, we have not imposed additional requirements on the function we seek (assuming the written integral is well-defined).

What happens if we try to get even closer to the form of the Laplace transform?
In other words, we want
$e^(z pi i r^2)$
to be one of the factors inside the integral, and to avoid external factors.

If we rely on the identity

$ sin^2((pi r^2) / 2) = -1/4 (e^(i pi r^2 / 2) - e^(-i pi r^2 / 2))^2
= -1/4 (e^(i pi r^2) - 2 + e^(-i pi r^2)) $

we get:

$ a(r) = 1/4 integral_0^(i infinity) [ phi0(-1/z) z^2  e^((z+1) pi i r^2) - 2 phi0(-1/z) z^2  e^(z pi i r^2) + phi0(-1/z) z^2  e^((z-1) pi i r^2) ] dif z $

Since we defined
$a(r)$
somewhat arbitrarily, we can multiply it by a suitable scalar to keep the form of the function simpler. That is, define:

$ a(r) := -4 sin^2(pi r^2 \/ 2) integral_0^(i infinity) phi0(-1/z) z^2  e^(pi i r^2 z) dif z  $ <a-for-r-gt-sqrt-2>

From now on, we will not change
$a$
further, and will focus on the inner function,
$phi0$.


#pagebreak()

We get, as we saw before:

$ a(r) = integral_0^(i infinity) [ phi0(-1/z) z^2  e^(pi i r^2 (z+1)) - 2 phi0(-1/z) z^2  e^(pi i r^2 z) + phi0(-1/z) z^2  e^(pi i r^2 (z-1)) ] dif z $

The next problem we want to solve is the powers
$z+1, z-1$
in the factors
$e^(pi i r^2 (z plus.minus 1))$,
since they "interfere" with us reaching the Laplace transform (and thus the simple transition to the Fourier transform).

We can perform a change of variables in each of the summands, and obtain:

$ 
// Fix the e^(z+1) by ~z= z+1 => z= ~z-1  
integral_0^(i infinity) phi0(-1/z) z^2  e^(pi i r^2 (z+1)) dif z = integral_(1)^(i infinity+1) phi0(-1/(z-1)) (z-1)^2  e^(pi i r^2 z) dif z \

// Fix the e^(z-1) by ~z= z-1 => z= ~z+1  
integral_0^(i infinity) phi0(-1/z) z^2  e^(pi i r^2 (z-1)) dif z = integral_(-1)^(i infinity-1) phi0(-1/(z+1)) (z+1)^2  e^(pi i r^2 z) dif z
$

Thus:

$

a(r) =  integral_(-1)^(i infinity-1) phi0(-1/(z+1)) (z+1)^2  e^(pi i r^2 z) dif z
      - 2 integral_0^(i infinity) phi0(-1/z) z^2  e^(pi i r^2 z) dif z \
      + integral_(1)^(i infinity+1) phi0(-1/(z-1)) (z-1)^2  e^(pi i r^2 z) dif z 

$ <a-as-sum-of-3-integrals>

Although we have not obtained a simple expression, we have come very close to the form of the Laplace transform, and now we will try to exploit it.

Look at the expression:

$ integral_(i a)^(i b) phi0(-1/z) z^2 e^(pi i abs(x)^2 z) dif z , space a,b in [-infinity, infinity] $

We seek an eigenfunction, and we know the corresponding Fourier transform (assuming all necessary conditions for performing it hold) will look like:

$ integral_(i a)^(i b) phi0(-1/z) z^2 z^(-4) e^(pi i abs(y)^2 (-1/z)) dif z $

One way to bring the second expression closer to the first is by "adjusting" the exponent’s coefficient.
We can do this by the substitution
$w=-1/z$:

// w=-1/z => z=-1/w

$ 
integral_(i 1/a)^(i 1/b) phi0(w) (-1/w)^(-2) e^(pi i abs(y)^2 w) (-1/w)' dif w =
integral_(i 1/a)^(i 1/b) phi0(w) e^(pi i abs(y)^2 w) dif w
$

Here, modular forms can assist us, but only to a certain extent.

Even if we assume that we have a way to move between
$phi0(-1/z) z^2 <-> phi0(z)$,
the change in the integration domain prevents us from returning to exactly the same function.

However, regarding the segments appearing in
@eqt:a-as-sum-of-3-integrals,
we can, for example, divide the segment
$(0, i infinity)$
into
$(0, i)$
and
$(i, i infinity)$,
and we find that each of them is a mirror image of the other under the Fourier transform and the substitution we made.


// First requirement: phi0 z^2 e^z -> 0 ("From (29) we deduce that ..", page 12).

Thus, we require that for
$r>sqrt(2)$,
the integrand satisfies
$phi0(-1/z) z^2  e^(pi i r^2 z) -> 0$
as
$Im (z) -> infinity$,
so that we can change the integration path.

#pagebreak()

We will try to gather all the integrals in suitable segments, avoiding integration on the real axis (where we cannot use modular forms):

The integration along the segments
$(plus.minus 1, i infinity)$
will be changed to
$(plus.minus 1, i) -> (i, i infinity)$.

The integration along the segment
$(0, i infinity)$
will be changed to
$(0, i) -> (i, i infinity)$.

Altogether, we have:

$

a(r) &=  integral_(-1)^(i) phi0(-1/(z+1)) (z+1)^2  e^(pi i r^2 z) dif z 
            + integral_(i)^(i infinity) phi0(-1/(z+1)) (z+1)^2  e^(pi i r^2 z) dif z \
     & -2 integral_0^(i) phi0(-1/z) z^2  e^(pi i r^2 z) dif z 
            -2 integral_i^(i infinity) phi0(-1/z) z^2  e^(pi i r^2 z) dif z \
     & + integral_(1)^(i) phi0(-1/(z-1)) (z-1)^2  e^(pi i r^2 z) dif z 
            + integral_(i)^(i infinity) phi0(-1/(z-1)) (z-1)^2  e^(pi i r^2 z) dif z \

$

Collecting together the integrals on the segment
$(i, i infinity)$
we get:


$

a(r) &=  integral_(-1)^(i) phi0(-1/(z+1)) (z+1)^2  e^(pi i r^2 z) dif z \
     & -2 integral_0^(i) phi0(-1/z) z^2  e^(pi i r^2 z) dif z \
     & + integral_(1)^(i) phi0(-1/(z-1)) (z-1)^2  e^(pi i r^2 z) dif z \
     & + integral_(i)^(i infinity) [
       phi0(-1/(z+1)) (z+1)^2  e^(pi i r^2 z) -2 phi0(-1/z) z^2  e^(pi i r^2 z)  + phi0(-1/(z-1)) (z-1)^2  e^(pi i r^2 z)
     ] dif z

$ <a-as-sum-of-3-finite-integrals-and-one-i-to-inf-integral>

We saw that there is a certain symmetry between the segments
$(i, i infinity)$
and
$(i, 0)$.
A similar symmetry exists between the segments
$(-1, i)$ and $(1,i)$.
It turns out to be a full symmetry between the integrals, under the Fourier transform.

/// In the paper, "we exchange the contour integration w.r.t z var and Fourier trans. w.r.t to x in (35). This can be done since the corresponding double integral converges absolutely."

Look at the Fourier transform of the integral on the segment
$(-1,i)$:

$
integral_(-1)^(i) phi0(-1/(z+1)) (z+1)^2  e^(pi i abs(x)^2 z) dif z -> integral_(-1)^(i) phi0(-1/(z+1)) (z+1)^2  z^(-4) e^(pi i abs(y)^2 (-1/z)) dif z
$

Performing the substitution as before,
$w=-1/z$,
we get:

// Taking the ∫-1..i and using w=-1/z to get ∫1..i
// w=-1/z => z=-1/w

$
integral_(-1)^(i) phi0(-1/(z+1)) (z+1)^2  z^(-4) e^(pi i abs(y)^2 (-1/z)) dif z = \

integral_(1)^(i) phi0(-1/((-1/w)+1)) (-1/w+1)^2 (-1/w)^(-4)  e^(pi i abs(y)^2 w) (-1/w)' dif w = \

// (-1/w)' = 1/w^2, (-1/w+1)^2 = (w-1/w)^2 = (w-1)^2 / w^2

integral_(1)^(i) phi0(-1 - 1/(w-1)) (w-1)^2 e^(pi i abs(y)^2 w) dif w
$

This is almost the integral on the segment
$(1,i)$
in
@eqt:a-as-sum-of-3-finite-integrals-and-one-i-to-inf-integral!

It suffices to require that
$phi0$
be 1-periodic (a property we have already proven for modular forms), for us to have
$phi0(-1 - 1/(w-1)) = phi0(1 + -1 - 1/(w-1)) = phi0(- 1/(w-1))$.

#pagebreak()

We summarize as follows:
In the equation we obtained in
@eqt:a-as-sum-of-3-finite-integrals-and-one-i-to-inf-integral,
the part composed of the integrals on the segments
$(plus.minus 1, i)$
is actually an eigenfunction (under the assumption that
$phi0(z+1) = phi0(z)$).

In other words, for
$a(r)$
to be entirely an eigenfunction,
the second part of the function,
i.e. the part composed of the segments
$(0, i)$ and $(i, i infinity)$,
must also be an eigenfunction.

Let us start from the simpler expression, the integral on the segment
$(0,i)$.
Its Fourier transform is:

$ -2 integral_0^(i) phi0(-1/z) z^2  e^(pi i abs(x)^2 z) dif z -> -2  integral_0^(i) phi0(-1/z) z^2 z^(-4) e^(pi i abs(y)^2 (-1/z)) dif z $

Again, we perform the substitution
$w=-1/z$,

$
-2 integral_0^(i) phi0(-1/z) z^2 z^(-4) e^(pi i abs(y)^2 (-1/z)) dif z = \

-2 integral_(i infinity)^(i) phi0(w) (-1/w)^(-2) e^(pi i abs(y)^2 w) (-1/w)' dif w = \

2 integral_(i)^(i infinity) phi0(w) e^(pi i abs(y)^2 w) dif w 
$

We have obtained an expression that is close, but not identical, to the original function. So how do we complete the expression so that it does form an eigenfunction?

In fact, we at least know one component that must be present for the Fourier transform result: the original integral on the segment
$(0,i)$!

That is, we look for a combination such that:

$
-2 integral_0^(i) phi0(-1/z) z^2  e^(pi i abs(x)^2 z) dif z + ??? -> 2 integral_(i)^(i infinity) phi0(z) e^(pi i abs(y)^2 z) dif z  -2 integral_0^(i) phi0(-1/z) z^2  e^(pi i abs(y)^2 z) dif z
$

But what happens if we apply the Fourier transform to the result? We immediately get:

$
2 integral_(i)^(i infinity) phi0(z) e^(pi i abs(y)^2 z) dif z  -2 integral_0^(i) phi0(-1/z) z^2  e^(pi i abs(y)^2 z) dif z -> -2 integral_0^(i) phi0(-1/z) z^2  e^(pi i abs(x)^2 z) dif z + 2 integral_(i)^(i infinity) phi0(z) e^(pi i abs(x)^2 z) dif z
$
However, in the development from
@eqt:a-as-sum-of-3-finite-integrals-and-one-i-to-inf-integral
we have a suitable integral on the segment
$(i, i infinity)$,
and therefore if

$
phi0(-1/(z+1)) (z+1)^2 -2 phi0(-1/z) z^2 + phi0(-1/(z-1)) (z-1)^2 = 2phi0(z)
$ <phi0-func-eq>

then the second part of
$a(r)$
is also an eigenfunction, meaning that
$a(r)$
as a whole is exactly the eigenfunction we are seeking!

#pagebreak()

To sum up: if we find a 1-periodic function that satisfies the equation
@eqt:phi0-func-eq,
then the definition
@eqt:a-for-r-gt-sqrt-2
does indeed lead us to the desired eigenfunction.

We must fulfill a few more technical conditions, such as the absolute convergence of the required integrals, and we will do so shortly.

Before that, we address another limitation.
When we defined
@eqt:a-for-r-gt-sqrt-2,
we only considered the case of
$r>sqrt(2)$.

But during the process, we actually obtained a more useful expression: if we substitute
@eqt:phi0-func-eq
into
@eqt:a-as-sum-of-3-finite-integrals-and-one-i-to-inf-integral,
we get, for every
$x in RR^8$:

// This is (35) in the paper.

#let a_for_x_in_r8 = $
integral_(-1)^(i) phi0(-1/(z+1)) (z+1)^2  e^(pi i abs(x)^2 z) dif z + integral_(1)^(i) phi0(-1/(z-1)) (z-1)^2  e^(pi i abs(x)^2 z) dif z \
     & -2 integral_0^(i) phi0(-1/z) z^2  e^(pi i abs(x)^2 z) dif z 
      + 2 integral_(i)^(i infinity) phi0(z)  e^(pi i abs(x)^2 z) dif z
$

$
a(x) = & #a_for_x_in_r8
$ <a-for-x-in-r8>

The advantage of this form of the eigenfunction is that under the assumption
$phi0(z)=O(e^(-2pi i z))$
as
$Im z -> infinity$,
the integrals converge absolutely and uniformly, and thus the expression is indeed defined for every vector.

In summary:
We must find a function, denoted by
$phi0$,
such that:

1.#[
  $phi0$
  is 1-periodic, i.e.
  $phi0(z)=phi0(z+1)$.
]

2.#[
  $phi0$
  satisfies
  @eqt:phi0-func-eq.
]

3.#[
  $phi0(z)=O(e^(-2pi i z))$
  as
  $Im z -> infinity$,
  for uniform and absolute convergence of the integrals.
]

4.#[
  $phi0(-1/z) z^2  e^(pi i r^2 z) -> 0$
  as
  $Im (z) -> infinity$,
  so we can change the integration path.
]

If we also require that
$a(x)$
itself be a Schwartz function,
these conditions will ensure that by the definition
@eqt:a-for-x-in-r8
we obtain an eigenfunction.

We know that it will attain the required zeros when
$r>sqrt(2)$
due to
@eqt:a-for-r-gt-sqrt-2,
but we will need to check the additional zero at
$r=sqrt(2)$.


In addition,
we can only prove the other conditions from
@lin-bounds
once we also find the second eigenfunction,
in particular we will want to know the value of
$a(r)$
at
$r=0$.

#pagebreak()

== A function that meets the conditions

// The building blocks for phi0, 23-24 in the paper.

Similar to the function
$j$
that we defined in
@eqt:elliptic-j-invariant,
we define two auxiliary functions:

$

phi_(-2) := (-1728 E_4 E_6) / (E_4^3 - E_6^2) \
phi_(-4) := (1728 E_4^2) / (E_4^3 - E_6^2) \

$

These functions have the properties

$

phi_(-2)(-1 \/ z) = 
(-1728 E_4(-1 \/ z) E_6(-1 \/ z)) / (E_4^3(-1 \/ z) - E_6^2(-1 \/ z)) = 
(-1728 z^4 E_4(z) z^6 E_6(z)) / (z^12 E_4^3(z) - z^12 E_6^2(z)) = z^(-2) phi_(-2)(z) \

phi_(-4)(-1 \/ z) = z^(-4) phi_(-4)(z)

$

(and of course both are 1-periodic)

These are "building blocks" we can use whenever we want to get rid of undesirable powers of
$z$.

// The explicit def of phi0, 23-38 in the paper.

Using these functions, we define

$

phi.alt_(-4) := phi_(-4) \
phi.alt_(-2) := phi_(-4) E_2 + phi_(-2) \

phi0 := phi_(-4) E_2^2 + 2phi_(-2) E_2 + j - 1728

$

We will prove that
$phi0$
meets all the conditions we need.

To prove that the identity
@eqt:phi0-func-eq
holds, it is enough to note that

// This is (29) in the paper.

$

phi0(-1 / z) = phi0(z) - (12i) / pi dot 1 / z phi.alt_(-2)(z) - 36 / (pi^2) 1 / (z^2) phi.alt_(-4) (z)

$ <phi0-one-over-z-tranform>

To show this,
we use the property of
$E_2$
we obtained in
@eqt:eisen-2,
and substitute:

$ 
phi0(-1 / z) &= phi_(-4) (-1 / z) (E_2(-1 / z))^2 + 2phi_(-2) (-1 / z) E_2(-1 / z) + j(-1 / z) - 1728 \
             &= z^(-4) phi_(-4) (z^2 E_2 (z) - 6i z \/ pi)^2 + 2z^(-2) phi_(-2) (z^2 E_2 (z) - 6i z \/ pi) + j - 1728 \
             &= z^(-4) phi_(-4) (z^4 E_2^2 - 12 i z^3E_2 \/ pi - 36 z^2 \/ pi^2) + 2phi_(-2)E_2-((12i)/(pi) dot 1/z) phi_(-2) +j -1728 \
             &= phi_(-4) E_2^2 -(12 i) / (pi z) phi_(-4)E_2 - 36 / (pi^2 z^2) phi_(-4) +  2phi_(-2)E_2-((12i)/(pi) dot 1/z) phi_(-2) +j -1728 \
             &= phi0 (z) -(12 i) / (pi z) phi_(-4)E_2 - 36 / (pi^2 z^2) phi_(-4) -((12i)/(pi) dot 1/z) phi_(-2) \
             &= phi0 (z) - 36 / (pi^2 z^2) phi.alt_(-4)(z) - (12 i) / (pi z) (phi_(-4) E_2 + phi_(-2) ) \
             &= phi0 (z) - 36 / (pi^2 z^2) phi.alt_(-4)(z)  - (12 i) / (pi z) phi.alt_(-2) (z)
               
$

#pagebreak()

Now, we can substitute
@eqt:phi0-one-over-z-tranform
into
@eqt:phi0-func-eq
and obtain

$

phi0(-1/(z+1)) (z+1)^2 -2 phi0(-1/z) z^2 + phi0(-1/(z-1)) (z-1)^2 &= \

phi0(z + 1)(z + 1)^2 - 2 phi0(z) z^2 + phi0(z - 1)(z - 1)^2 \
- (12i) / pi [phi.alt_(-2)(z + 1)(z + 1) - 2 phi.alt_(-2)(z) z + phi.alt_(-2)(z - 1)(z - 1)] \
- 36 / pi^2 [phi.alt_(-4)(z + 1) - 2 phi.alt_(-4)(z) + phi.alt_(-4)(z - 1)] 
&= 2 phi0(z)

$

as required.

We have shown that the equation
@eqt:phi0-func-eq
holds, and all that remains is to show that the other required conditions also hold. The simplest way to do this is via the Fourier expansion.

=== Fourier expansion of $phi0$ #rtl-mark

Since we have
@eqt:e-k-fourier-exp,
we can compute any Fourier coefficient we want for each of the functions we defined.

We will examine the first terms of each expansion, since they will be the most important for the initial properties we want to prove.

Let’s start with
$phi.alt_(-4)$.
We have already seen the Fourier expansions of the denominator and numerator
in
@eqt:ek-powers-fourier,
therefore:

// Maybe note that this is because 1/(1-x)=Σx^n 
// https://math.stackexchange.com/questions/33140/j-invariant-fourier-expansion
$ 

phi.alt_(-4) &= (1 + 480q + ...) / (q - 24q^2 + 252q^3 + ...) 
             &= (1 + 480q + ...) (q^(-1) + 24 - 324q + ...) #h(3em) \ 
             &= q^(-1) + 504 + ...
$


For
$phi.alt_(-2)$,
since
$
phi_(-2) &= -q^(-1) + 240 + 141444q + ... #h(18em)
$

we can use
@eqt:e2-and-e6-fourier
and compute:

$

phi.alt_(-2) &= (q^(-1) + 504 + ...) (1 - 24q - 72q^2 - ...) + -q^(-1) + 240 + 141444q + ... \
             &= 720 + 203040q + ....
$

Finally, we can also use the Fourier expansion of
$j$
we saw in
@eqt:j-foureir
and get:

$
// E_2^2 = 1 - 48q + 432q^2 + ...
phi0 = &(q^(-1) + 504 + ...)(1 - 24q - 72q^2 - ...)^2 \
       &+ 2(-q^(-1) + 240 + ...)(1 - 24q - 72q^2 - ...) \
       &+ (q^(-1) + 774 + ...) \
       &-1728 \
      
     = &q^(-1) -2(q^-1) + q^(-1) + 504 - 48 + 2(240 + 24) + 744 - 1728 + ... \
$

Notice that the
$q^(-1)$
terms cancel out, as does the constant term!

We can again leave the calculation to an automatic tool and obtain the following Fourier expansions:

$

phi.alt_(-4)(z) &= q^(-1) + 504 + 73764 q + 2695040 q^2 + 54755730 q^3 + O(q^4) \
phi.alt_(-2)(z) &= 720 + 203040 q + 9417600 q^2 + 223473600 q^3 + 3566782080 q^4 + O(q^5) \
phi0(z) &= 518400 q + 31104000 q^2 + 870912000 q^3 + 15697152000 q^4 + O(q^5) \

$ <phi0-fourier>

#pagebreak()


=== Checking that $a(x)$ is a Schwartz function

Recall that throughout the development, we arrived at the definition
@eqt:a-for-x-in-r8:

$ a(x) :=& #a_for_x_in_r8 $

From the Fourier expansion we have seen, we deduce that
$phi0(z) = O(e^(-2 pi i z))$
as
$Im(z) -> infinity$,
and therefore the integrals converge absolutely and uniformly, and the function 
$a(x)$
is defined for every
$x in RR^8$.
It is also easy to see that
$phi0(-1/z) z^2  e^(pi i r^2 z) -> 0$
as
$Im (z) -> infinity$,
so we can change the integration path as we did during the development.

Hence,
$phi0$
meets all the requirements for obtaining an eigenfunction,
and all that is left is to ensure that
$a(x)$
is a Schwartz function.

As mentioned in chapter@modular-forms,
not only can we compute the coefficients of the Fourier expansion of modular forms, we can also find asymptotic bounds for them.
We will use this to estimate the first summand.
First, the Fourier coefficients of
$phi0$
satisfy

$ c_(phi0)(0) = 0, |c_(phi0)(n)| <= 2 e^(4 pi sqrt(n)) $

So, similarly to the estimate we obtained in
@eqt:e-k-estimation,
with a slight adjustment since
$c_(phi0)(0) = 0$,
we find that there is a constant
$C$
such that for all
$Im(z) > 1/2$,

$ |phi0(z)| <= C  e^(-2 pi Im(z)) $

We will use this to obtain the following estimate when
$r>=0$:

$ abs(integral_(-1)^i phi0(-1 / (z + 1))(z + 1)^2 e^(pi i r^2 z) dif z) = abs(integral_(i infinity)^(-1/(i+1)) phi0(z) z^(-4) e^(pi i r^2 (-1 / z - 1)) dif z) <= \
C_1 integral_(1/2)^infinity e^(-2 pi t) e^(-pi r^2 / t) dif t <= C_1 integral_(0)^infinity e^(-2 pi t) e^(-pi r^2 / t) dif t
$

The last integral can be estimated using the _modified Bessel function of the second kind_,
$K_alpha (x)$. \
// Proposition 5.15 in "An Introduction to Modular Forms", p40.
$K_alpha (x)$
is a solution to a certain type of differential equation, and decays exponentially 
(as
$x -> infinity$,
$K_alpha (x) ∼ (pi / (2x))^(1/2)e^(−x)$).
From the obtained estimate we have:

$ C_1 integral_(0)^infinity e^(-2 pi t) e^(-pi r^2 / t) dif t <= C_2 r K_1(2sqrt(2) pi r) $

A similar estimate can be obtained for the 2nd and 3rd summands, while the last is estimated as follows:

$ abs(integral_i^infinity phi0(z) e^(pi i r^2 z) dif z) <= C integral_1^infinity e^(-2 pi t) e^(-pi r^2 t) dif t = 
C_3 e^(-pi (r^2 + 2)) / (r^2 + 2)
 $

Altogether, we get that

$ abs(a(x)) <= 4 C_2 r K_1(2sqrt(2) pi r) + 2 C_3 e^(-pi (r^2 + 2)) / (r^2 + 2) $

We conclude that
$a(x)$
decays faster than any polynomial. Similar estimates can also be obtained for all its derivatives,
and therefore it is a Schwartz function.

#pagebreak()

== Finding the values of the function on the lattice and at the origin

// Finding a(0) and a(sqrt(2)) using approx.

Since we know that
$a(x)$
takes the form
@eqt:a-for-r-gt-sqrt-2
when
$x > sqrt(2)$,
we deduce that there it indeed vanishes on the lattice, as we wanted.
All that remains is to find the values of
$a(r)$
for
$r=0$
and
$r=sqrt(2)$.

To do this, we will present
$a$
in a third form, which will be convenient for this purpose.

// This is Prop 3 (38) in the paper.

$

a(r) = 4i sin^2(pi r^2 \/ 2)  (&
    36 / (pi^3 (r^2 - 2)) - 8640 / (pi^3 r^4) + 18144 / (pi^3 r^2) \
    &+ integral_0^infinity (t^2 phi0(i / t) - 36 / (pi^2 e^(2 pi t)) + 8640 / (pi t) - 18144 / (pi^2)) e^(-pi r^2 t) dif t
)

$ <a-for-r-gt-0-using-approx>

This representation is justified, because if we start from
@eqt:a-for-r-gt-sqrt-2
and make the substitution
$t=z / i$,
we get

$ 
a(r) = 4 i sin^2(pi r^2 \/ 2) integral_0^infinity phi0(i/t) t^2  e^(-pi r^2 t) dif t
$ <a-for-r-gt-0-integral-over-reals>

We can use
@eqt:phi0-one-over-z-tranform
to obtain

$ phi0(i/t) t^2 &= phi0(i t) t^2  - (12t) / pi phi.alt_(-2)(i t) + 36 / (pi^2) phi.alt_(-4) (i t) $

From the Fourier expansions we saw in
@eqt:phi0-fourier
we deduce that as
$t -> infinity$,
$phi0(i t) t^2 = O(t^2 e^(-2 pi t))$.

For the other two functions, as
$t -> infinity$
they satisfy

$

phi.alt_(-4)(i t) &= e^(2 pi t) + 504 + O(e^(-2 pi t)) \
phi.alt_(-2)(i t) &= 720 + O(e^(-2 pi t))

$

We conclude that

$

phi0(i/t) t^2 &=  - (12t) / pi dot 720 + 36 / (pi^2) (e^(2 pi t) + 504) + O(t^2 e^(-2 pi t)) \
              &= 36 / (pi^2) e^(2 pi t) - 8640 / pi t + 18144 / (pi^2) + O(t^2 e^(-2 pi t))
$ <phi0-estimate-for-approx>

But, for
$r > sqrt(2)$
we can compute

$

integral_0^infinity (36 / (pi^2) e^(2 pi t) - 8640 / pi t + 18144 / (pi^2)) dif t = 36 / (pi^3 (r^2 - 2)) - 8640 / (pi^3 r^4) + 18144 / (pi^3 r^2)

$

Thus
@eqt:a-for-r-gt-0-using-approx
holds for
$r > sqrt(2)$.

Note that the estimate we found for
$phi0(i\/t) t^2$
shows that the right-hand side of equation
@eqt:a-for-r-gt-0-using-approx
is an analytic function in some neighborhood of
$[0, infinity)$.
But from
@eqt:a-for-x-in-r8
we can deduce that
$a(r)$
defined in
@eqt:a-for-r-gt-0-integral-over-reals
is also analytic in some neighborhood of
$[0, infinity)$,
so
@eqt:a-for-r-gt-0-using-approx
holds for all
$[0, infinity)$.

We will use it to find the requested values.

#pagebreak()

Note that using the estimate we found in
@eqt:phi0-estimate-for-approx,
we get that the defined integral satisfies

$

abs(
  integral_0^infinity (t^2 phi0(i / t) - 36 / (pi^2 e^(2 pi t)) + 8640 / (pi t) - 18144 / (pi^2)) e^(-pi r^2 t) dif t 
) <= integral_0^infinity abs(
  C t^2 e^(-2 pi t) e^(-pi r^2 t)
) dif t <= C_1 integral_0^infinity t^2 e^(-(2+r^2) pi t)  dif t

$

Hence it obtains a finite value. We deduce from this that when the sine is zero, their product is also zero.

So for
$r=0$,
it is enough to compute the limit

$
a(0) &= lim_(r->0) 4i sin^2(pi r^2 / 2)  (36 / (pi^3 (r^2 - 2)) - 8640 / (pi^3 r^4) + 18144 / (pi^3 r^2)) \
& = lim_(r->0) -4i pi^2 r^4 / 4 dot 8640 / (pi^3 r^4) \
& = (-i 8640) / pi
$

Similarly, for
$r=sqrt(2)$
we get

$
a(sqrt(2)) &= lim_(r->sqrt(2)) 4i sin^2(pi r^2 / 2)  (36 / (pi^3 (r^2 - 2)) - 8640 / (pi^3 r^4) + 18144 / (pi^3 r^2)) \
& = lim_(r->sqrt(2)) -4i pi^2 (r^2 - 2)^2 / 4 dot 36 / (pi^3 (r^2 - 2)) \
& = 0
$

#pagebreak()

== The other eigenfunction

We still need a function that satisfies
$b(x) = -hat(b)(x)$.

Similarly to the development of
$a(x)$,
we will use the functions defined in
@eqt:theta-z-funcs,
and denote

#let psiI = $psi_I$
#let psiT = $psi_T$
#let psiS = $psi_S$

$

// This is the form used in the Cohn paper (before 7.2),
// except they have -32 (but we place the -4 in the b-for-r func).

psiI := 128 (theta_00^4 + theta_01^4) / theta_10^8 + 128 (theta_01^4 - theta_10^4) / theta_00^8 \

$

Using the properties of the theta functions we proved, substitute and denote:

$

psiT(z) &:= psiI(z+1) = 128 (theta_01^4 + theta_00^4) / theta_10^8 + 128 (theta_00^4 + theta_10^4) / theta_01^8 = psiI(z-1)\

psiS(z) &:= z^2 psiI(-1 \/ z) = 128 (- theta_00^4 - theta_10^4) / theta_01^8 + 128 (- theta_10^4 + theta_01^4) / theta_00^8 
$ <psi-func-eqs-1>

In particular, it holds that:

// In the Cohn paper, they say that $psi(z) = psi(z+1) + z^2 psi(-1/z)$, which is the "key" to the -1 eigen function, but I'm not sure we used everything here.
$
psiI(z) =  psiI(z+1) + z^2 psiI(-1 \/ z) \
psiT(z) + psiS(z) = psiI(z)
$ <psi-func-eqs-2>

Using these, define

$ b(r) := -4 sin^2(pi r^2 \/ 2) integral_0^(i infinity) psiI(z) e^(pi i r^2 z) dif z  $ <b-for-r-gt-sqrt-2>

for all $r>sqrt(2)$.

Here too, the required extensions for
$b(x)$
for all
$x in RR^8$
exist, and a presentation that allows convenient calculation of the function’s values.

We will suffice by showing that
$b(x)$
is indeed the required eigenfunction.
Similar to the method we used at the beginning of the section, we use the definition of sine and a change of variables to get

$

b(r) = integral_(-1)^(i infinity - 1) psiI(z+1) e^(pi i r^2 z) dif z - 2 integral_0^(i infinity) psiI(z) e^(pi i r^2 z) dif z + integral_(1)^(i infinity + 1) psiI(z-1) e^(pi i r^2 z) dif z

$

The Fourier expansions of
$psiI$, $psiT$, and $psiS$
are

$ 
psiI(z) &= q^(-1) + 144 - 5120 q^(1\/2) + 70524 q - 626688 q^(3\/2) + 4265600 q^2 + O(q^(5\/2))
\
psiT(z) &= q^(-1) + 144 + 5120 q^(1\/2) + 70524 q + 626688 q^(3\/2) + 4265600 q^2 + O(q^(5\/2))
\
psiS(z) &= -10240 q^(1\/2) - 1253376 q^(3\/2) - 48328704 q^(5\/2) - 1059078144 q^(7\/2) + O(q^(9\/2))
$ <psi-i-fourier>

In particular
$psiI(z) = e^(-2 pi i z) + O(1)$
as
$Im(z) -> infinity$.
So we can change the integration path, and using
@eqt:psi-func-eqs-1
we get

#let gsdz=$e^(pi i r^2 z) dif z$

$
integral_(-1)^(i infinity - 1) psiI(z+1) gsdz &= integral_(-1)^i psiT(z) gsdz + integral_(i)^(i infinity) psiT(z) gsdz \
integral_(1)^(i infinity + 1) psiI(z-1) gsdz &= integral_(1)^(i) psiT(z) gsdz + integral_(i)^(i infinity) psiT(z) gsdz \
integral_0^(i infinity) psiI(z) gsdz &= integral_0^(i) psiI(z) gsdz + integral_i^(i infinity) psiI(z) gsdz
$


#pagebreak()

Collecting them we get

// (56) in the paper.

$

b(r) =& integral_(-1)^i psiT(z) gsdz + integral_(1)^(i) psiT(z) gsdz + \
 &2 integral_(i)^(i infinity) psiT(z) gsdz - 2 integral_0^(i) psiI(z) gsdz -2 integral_i^(i infinity) psiI(z) gsdz \
$


That is

$
b(r) =& integral_(-1)^i psiT(z) gsdz + integral_(1)^(i) psiT(z) gsdz -2 integral_0^(i) psiI(z) gsdz + 2 integral_i^(i infinity) (psiT(z) - psiI(z)) gsdz
$

From
@eqt:psi-func-eqs-2
it follows that

$
b(r) &= integral_(-1)^i psiT(z) gsdz + integral_(1)^(i) psiT(z) gsdz -2 integral_0^(i) psiI(z) gsdz - 2 integral_i^(i infinity) psiS(z) gsdz
$

We can apply the Fourier transform and get

$
#let difz=$z^(-4) e^(pi i abs(x)^2 (-1 / z)) dif z$
hat(b)(x) = &integral_(-1)^i psiT(z) difz + integral_(1)^(i) psiT(z) difz \
            &-2 integral_0^(i) psiI(z) difz - 2 integral_i^(i infinity) psiS(z) difz
$

We perform the substitution
$w=-1/z$:
$
#let difw=$w^2 e^(pi i abs(x)^2 w) dif w$
hat(b)(x) = &integral_(i)^1 psiT(-1/w) difw + integral_(-1)^(i) psiT(-1/w) difw \ 
            &-2 integral_(i infinity)^(i) psiI(-1/w) difw - 2 integral_i^(0) psiS(-1/w) difw
$

And use @eqt:psi-func-eqs-1 to get:

#let gsxdz=$e^(pi i abs(x)^2 z) dif z$

$

#let difw=$w^2 e^(pi i abs(x)^2 w) dif w$
hat(b)(x) = &integral_(i)^1 -psiT(z) gsxdz  + integral_(-1)^(i) -psiT(z) gsxdz \
            &2 integral_(i)^(i infinity) psiS(z) gsxdz + 2 integral_0^(i) psiI(z) gsxdz
$

Thus

$ b(x) = -hat(b)(x) $

as we wanted.

For
$b$
we can use the following representation

$ 
b(r) = 4i sin^2(pi r^2 / 2) ( 144 / (pi r^2) + 1 / (pi (r^2 - 2)) + integral_(0)^infinity (psiI(i t) - 144 - e^(2 pi t)) e^(-pi r^2 t) dif t )
$

to obtain the following values at the important points:

// Prop 8 (61) in the paper.

$ b(0) = 0, b(sqrt(2)) = 0 $

#pagebreak()

== Checking the appropriate bounds <proof-g-satisfies-lin-bounds>

Define

$ g(x) := (pi i) / 8640 a(x) + i / (240 pi) b(x) $

and we will prove that it is the required magic function that meets all the requirements of
@lin-bounds.

/*
1. f(0) = hat(f)(0) > 0
2. hat(f)(y) >= 0 for all y in RR^n
3. f(x) <= 0 for all |x| >= r
*/

We begin with proving the third requirement, i.e.
$g(x) <= 0$
for all
$abs(x) > sqrt(2)$
(we know that
$abs(x) = sqrt(2) => g(x) = 0$).

We know that in this case, we can write the function as

$
g(r) = pi / 2160 sin^2(pi r^2 / 2) integral_0^(infinity) A(t) e^(- pi r^2 t) dif t \

A(t) = -t^2 phi0(i / t) - 36/pi^2 psiI(i t) 
$

If we show that
$A(t) < 0$
over the integration interval, we can conclude that
$g$
does not take positive values for any
$r>sqrt(2)$.

We will use two different representations of
$A(t)$,
which will allow us to evaluate it accurately enough on the segments
$(0,1]$ and
$[1, infinity)$
respectively:

$

A(t) &= -t^2 phi0(i / t) + 36/pi^2 t^2 psiS(i / t) \

A(t) &= -t^2  phi0(i t) - t (12i) / pi phi.alt_(-2)(i t) - 36 / (pi^2) phi.alt_(-4) (i t) - 36/pi^2 psiI(i t)
$

#let A0 = $A_0^((n))$
#let A0m = $A_0^((m))$
#let A06 = $A_0^((6))$
#let Ainf = $A_infinity^((n))$
#let Ainfm = $A_infinity^((m))$
#let Ainf6 = $A_infinity^((6))$

When
$t -> 0$,
we denote:
$ A(t) = A0(t) + O(t^2 e^(- pi n / t)) $
That is,
$A0(t)$
approximates 
$A(t)$
near 0, up to a factor of
$t^2 e^(- pi n / t)$.

Similarly, when
$t -> infinity$
we denote:
$ A(t) = Ainf(t) + O(t^2 e^(- pi n t)) $

As we mentioned in chapter
@modular-forms,
we can find asymptotic bounds on the Fourier coefficients of weakly holomorphic modular forms.
In particular, the
$n$-th coefficient
of the Fourier expansion of
$psiI$,
denoted
$c_(psiI)(n)$,
satisfies:

$ |c_(psiI)(n)| <= e^(4 pi sqrt(n)) &space.quad n in 1/2 ZZ_(>0) $

Similarly, there are bounds for the coefficients of each of the functions
$psiS, phi0, phi.alt_(-2), phi.alt_(-4)$:

$ 
|c_(psiS)(n)| <= 2e^(4 pi sqrt(n)) &space.quad n in 1/2 ZZ_(>0) \
|c_(phi0)(n)| <= 2e^(4 pi sqrt(n)) &space.quad n in ZZ_(>0) \
|c_(phi.alt_(-2))(n)| <= e^(4 pi sqrt(n)) &space.quad n in ZZ_(>0) \
|c_(phi.alt_(-4))(n)| <= e^(4 pi sqrt(n)) &space.quad n in ZZ_(>0) \
$

#pagebreak()

We can use these bounds to bound the error we get if we use the approximations
$A0$ and $Ainf$:

#let R0(m) = $(t^2 + 36/pi^2) sum_(n=#m)^infinity 2 e^(2 sqrt(2) pi sqrt(n)) e^ (-pi n / t)$
#let Rinf(m) = $(t^2 + 12/pi t + 36/pi^2) sum_(n=#m)^infinity 2 e^(2 sqrt(2) pi sqrt(n)) e^ (-pi n t)$
$

abs( A(t) - A0m(t) ) &<= R0(m) \
abs( A(t) - Ainfm(t) ) &<= Rinf(m)
$

From the Fourier expansions obtained in
@eqt:phi0-fourier
and
@eqt:psi-i-fourier,
we can compute
$A06$
and
$Ainf6$.
For 
$A06$
we get:
#let A06Expansion = $&t^2 (- (368640) / (pi^2) e^(-pi / t) - 518400 e^(-2 pi / t) - (45121536) / (pi^2) e^(-3 pi / t) - 31104000 e^(-4 pi / t) - (1739833344) / (pi^2) e^(-5 pi / t))$

#let Ainf6Expanson = $
&- (72) / (pi^2) e^(2 pi t) - (23328) / (pi^2) + \
&(184320) / (pi^2) e^(-pi t) - (5194368) / (pi^2) e^(-2 pi t) + (22560768) / (pi^2) e^(-3 pi t) - (250583040) / (pi^2) e^(-4 pi t) + (869916672) / (pi^2) e^(-5 pi t) \ &+ 
t ((8640) / (pi) + (2436480) / (pi) e^(-2 pi t) + (113011200) / (pi) e^(-4 pi t)) \
&-t^2 (518400 e^(-2 pi t) + 31104000 e^(-4 pi t))
$

$
A06(t) = #A06Expansion
$

The expansion of
$Ainf6$
is obtained similarly but is even longer, so we will not present it here.

It is easy to see that for every
$t in (0,1]$,
$A06(t) < 0$.

In addition, in this interval one can compute that

$ abs( A(t) - A06(t) ) &<= abs(R0(6)) &<= abs(A06(t)) $

Similarly, also for
$Ainf6$
it holds for every
$t in [1,infinity)$
that
$Ainf6(t) < 0$ and
$ 
abs( A(t) - Ainf6(t) ) <= abs(Rinf(6)) <= abs(Ainf6(t)) 
$

Therefore,
$A(t) < 0$
for
$t in (0,infinity)$.
Hence, the third requirement is met, as we wanted to prove.

For the Fourier transform of
$g$,
which takes the form

$
hat(g)(r) = pi / 2160 sin^2(pi r^2 / 2) integral_0^(infinity) B(t) e^(- pi r^2 t) dif t \\

B(t) = -t^2 phi0(i / t) + 36/pi^2 psiI(i t) 
$
similar evaluations show that 
$B(t) > 0$
for
$t in (0,infinity)$,
so the second requirement,
$hat(f)(y) >= 0$ for all $y in RR^n$,
is also met.

Finally, since
$a(0) = (-i 8640) / pi$
and
$b(0) = 0$,
we get
$g(0) = hat(g)(0) = 1 > 0$.

Thus, we have shown that
$g(x)$
is the required magic function,
and therefore the maximal packing density in
$RR^8$
is
$ vol(B_(sqrt(2) / 2)^8) = pi^4 / 384 = 0.2538 ... $

Since this is the density we obtained for
$E_8$
in
@eqt:e8-packing-density,
we have proven that it is indeed the densest possible packing in this dimension.

#pagebreak()

= Bibliography

The definitions and results on Schwartz functions and the proof of Poisson summation are based on
@stein2011fourier.

The proof of the magic function is of course based on 
@Viazovska_2017,
and also on
@Cohn_2017,
on which the definitions and proofs of lattices and the main bounding proof for packing densities rely.

The basic definitions of modular forms and the estimate of $K_alpha$ are taken from
@cohen2018introduction,
and the development of
$E_k$
and other modular forms is based on
@zagier2008,
as well as the construction of the basis for
$Lambda_8$.

The proof of
$E_8$
self-duality is based on
@ben2022unimodularselfdual.

#[
  #set text(font: "Linux Libertine", lang: "en")
  #bibliography("works.bib", title: none, style: "springer-basic")
]
