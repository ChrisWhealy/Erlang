-module(quiz).
-compile(export_all).

foo(_,[])              -> [];
foo(Y,[X|_]) when X==Y -> [X];
foo(Y,[X|Xs])          -> [X | foo(Y,Xs) ].

bar (N, [N])                 -> [];
bar (N, [Y])                 -> [Y];
bar (N, [Y|Ys]) when N =/= Y -> [Y|bar (N, Ys)];
bar (N, [Y|Ys])              -> bar(N,Ys).

baz([])     -> [];
baz([X|Xs]) -> [X | baz(zab(X,Xs))].

zab(N,[])     -> [];
zab(N,[N|Xs]) -> zab(N,Xs);
zab(N,[X|Xs]) -> [X | zab(N,Xs)].

