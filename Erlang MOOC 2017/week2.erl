-module(week2).
-compile(export_all).

product_tail([])     -> 1;
product_tail([X|Xs]) -> product_tail(X,Xs).

product_tail(Acc,[])     -> Acc;
product_tail(Acc,[X|Xs]) -> product_tail(Acc*X,Xs).

product_dir([])     -> 1;
product_dir([X|Xs]) -> X * product_dir(Xs).


maximum_tail([])     -> error('empty_list_has_no_maximum');
maximum_tail([X|Xs]) -> maximum_tail(X,Xs).

maximum_tail(CurrentMax,[])     -> CurrentMax;
maximum_tail(CurrentMax,[X|Xs]) -> maximum_tail(max(CurrentMax,X),Xs).

maximum_dir([])     -> error('empty_list_has_no_maximum');
maximum_dir([X])    -> X;
maximum_dir([X|Xs]) -> max(X,maximum_dir(Xs)).

double_dir([])     -> [];
double_dir([X|Xs]) -> [X*2 | double_dir(Xs)].

double_tail([])     -> [];
double_tail([X|Xs]) -> double_tail([X*2],Xs).

double_tail(Acc,[])     -> lists:reverse(Acc);
double_tail(Acc,[X|Xs]) -> double_tail([X*2|Acc],Xs).

evens([]) -> [];
evens([X|Xs]) ->
  case X rem 2 of
    0 -> [X | evens(Xs)];
    1 -> evens(Xs)
  end.

median([])      -> error(undefined);
median([X])     -> X;
median([_|_]=L) ->
  Len = length(L),
  Mid = Len div 2,

  case Len rem 2 of
    0 -> (lists:nth(Mid,L) + lists:nth(Mid+1,L)) / 2;
    1 -> lists:nth(Mid+1,L)
  end.

modes([_|_]=L) -> modes([],lists:sort(L)).

modes(Acc, [])      -> lists:reverse(Acc);
modes(Acc, [X])     -> lists:reverse([{1,X}|Acc]);
modes(Acc, [X|_]=L) ->
  {F,R} = lists:splitwith(fun(Y) -> Y == X end, L),
  modes([{length(F),hd(F)}|Acc],R).

-spec take(integer(),[T]) -> [T].

take(0,_)                     -> [];
take(N,L) when N >= length(L) -> L;
take(N,L)                     -> take([],N,L).

take(Acc,0,_) -> lists:reverse(Acc);
take(Acc,N,L) -> take([hd(L)|Acc],N-1,tl(L)).


nub([]) -> [];
nub(L)  -> nub([],L,true).

nub(Acc,[],true)  -> lists:reverse(Acc);
nub(Acc,[],false) -> Acc;

nub(Acc,L,Reverse)  ->
  F = lists:filter(fun(X) -> X =/= hd(L) end, tl(L)),
  nub([hd(L) | Acc],F,Reverse).

bun([]) -> [];
bun(L) -> nub([],lists:reverse(L),false).

good_chars(Chr) -> not lists:member(Chr, " -,.;:\'\t\n\"").

palindrome(L) ->
  F = string:to_lower(lists:filter(fun good_chars/1, L)),
F == lists:reverse(F).

join([],[]) -> [];
join(X,[])  -> X;
join([],X)  -> X;

join([_|_]=X,[_|_]=Y) -> join(lists:droplast(X),[lists:last(X)|Y]).


member(_,[])     -> false;
member(X,[X|_])  -> true;
member(X,[_|Ys]) -> member(X,Ys).

mergesort([X])     -> [X];
mergesort([_|_]=L) ->
  {A,B} = lists:split(length(L) div 2, L),
  lists:merge(mergesort(A),mergesort(B)).

quicksort([]) -> [];
quicksort([X|Xs]) ->
  quicksort(lists:filter(fun(N) -> N < X end, Xs)) ++
  [X] ++
  quicksort(lists:filter(fun(N) -> N >= X end, Xs)).

insertionsort([])       -> [];
insertionsort([_|_] =L) -> lists:foldl(fun insert/2, [], L).

insert(X,[])                  -> [X];
insert(X,[H|_]=L) when X =< H -> [X|L];
insert(X,[H|T])               -> [H|insert(X,T)].

permutations([])      -> [[]];
permutations([_|_]=L) -> [[X|Y] || X <- L, Y <- permutations(L -- [X])].
