-module(week3).
-compile(export_all).

doubleAll(L) -> lists:map(fun(X) -> X*2 end,L).
evens(L)     -> lists:filter(fun(X) -> X rem 2 == 0 end, L).
product(L)   -> lists:foldl(fun(X,Acc) -> X * Acc end, 1, L).

%% Solution to part a)
zip([],[])           -> [];
zip([],[_|_])        -> [];
zip([_|_],[])        -> [];
zip([H1|T1],[H2|T2]) -> [{H1,H2} | zip(T1,T2)].


% Solution to part b)
zip([],[])           -> [];
zip([],[_|_])        -> [];
zip([_|_],[])        -> [];
zip([H1|T1],[H2|T2]) -> [{H1,H2} | zip(T1,T2)].

zip_with(F,L1,L2) -> do_zip_with(F,zip(L1,L2),[]).

do_zip_with(_,[],Acc)           -> lists:reverse(Acc);
do_zip_with(F,[{A,B}|Rest],Acc) -> do_zip_with(F,Rest,[F(A,B)|Acc]).


% Solution to part c)
zip([],[])           -> [];
zip([],[_|_])        -> [];
zip([_|_],[])        -> [];
zip([H1|T1],[H2|T2]) -> [{H1,H2} | zip(T1,T2)].

zip_with(F,L1,L2) -> lists:map(F,zip(L1,L2)).


% Solution to part d)
zip(L1,L2) -> zip_with(fun(A,B) -> {A,B} end, L1, L2).

zip_with(_,[],[])           -> [];
zip_with(_,[],[_|_])        -> [];
zip_with(_,[_|_],[])        -> [];
zip_with(F,[H1|T1],[H2|T2]) -> [F(H1,H2) | zip_with(F,T1,T2)].

