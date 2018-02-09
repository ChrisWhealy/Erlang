-module(first).
-compile(export_all).

mult(X,Y) -> X*Y.

double(X) -> mult(2,X).
treble(X) -> mult(3,X).

sqr(X) -> mult(X,X).

area(A,B,C) ->
  S = (A+B+C)/2,
  math:sqrt(S*(S-A)*(S-B)*(S-C)).
