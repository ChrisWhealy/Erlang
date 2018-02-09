-module(hof).
-compile(export_all).

add(X)   -> fun(Y) -> X+Y end.
times(X) -> fun(Y) -> X*Y end.

compose(F,G) -> fun(X) -> G(F(X)) end.

idiot(X) -> X.

iterate(0) -> fun idiot/1;
iterate(N) -> fun(F) -> compose(F,(iterate(N-1))(F)) end.
      
