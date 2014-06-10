# Euclid

## Deciding Statments in Euclidean Geometry

This is a proof of concept of an application of commutative algebra to
deciding statements about Euclidean geometry. We translate possible hypotheses
and conclusions to multivariate polynomials. From these we form an
<a href="http://en.wikipedia.org/wiki/Algebraic_variety">algebraic variety</a>
_V_. The conclusion then follows from the hypotheses if and only if it is
contained in the ideal of the polynomial ring __R__[_a_,_b_,...] which
vanishes on _V_. We test this by computing the reduced Groebner basis of a
modified ideal. Consult [Cox et al, 2010] for further reference.

## Examples

Tautologies:

    $ cat test
    parallel (AB,CD); decide parallel(AB,CD)
    $ ./main test
    Judgment: yes

    $ cat test
    parallel (AB,CD); decide parallel(AD,BC)
    $ ./main test
    Judgment: no

The parallelogram theorem:

    $ cat test
    parallel (AB,CD); parallel(AC, BD); colin (A,N,D); colin (B,N,C);
    decide disteq(AN,ND)
    $ ./main test
    Judgment: yes

Removing the requirement that _AND_ are collinear breaks the theorem:

    $ cat test
    parallel (AB,CD); parallel(AC, BD); colin (B,N,C);
    decide disteq(AN,ND)
    $ ./main test
    Judgment: no

## Supported Syntax

We support the following constructs:

* `parallel (AB,CD)` - _AB_ is parallel to _CD_;
* `perp (AB,CD)` - _AB_ is perpendicular to _CD_;
* `colin (A,B,C)` - _A_, _B_, _C_ are collinear;
* `disteq (AB,CD)` - |_AB_| = |_CD_|;
* `circle (C,A,AB)` - _C_ lies on the circle with center _A_ and radius _AB_;
* `midpoint (A,B,C)` - _A_ is the midpoint of _BC_;
* `angleeq (ABC,DEF)` - angle _ABC_ equals angle _DEF_ (both acute);
* `bisects(BD,ABC)` - _BD_ bisects angle _ABC_.

## Limitations

* Statements are decided over the complex plane __C__ rather than the real
plane. [Sturmfels 1989] gives examples of complicated statements which will
hence fail here.
* We are unable to output a _proof_ of any conclusions, only give a
judgment as to whether they follow. One possible future direction is to
create a reverse translation between polynomials and geometric statements
and then convert each intermediate result of the basis algorithm back to
the language of geometry.

## Building

A Makefile has been provided, allowing you to build the project with only
the default OCaml installation. OCaml 4.01 is required; this requirement may
be relaxed by replacing the application operator `(|>)` with `fun x g -> g x`.

    $ make

This will generate a native binary. You can generate OCaml bytecode instead
by replacing `ocamlopt` with `ocamlc` in the Makefile.

## License

This project is licensed under the MIT open source license.
