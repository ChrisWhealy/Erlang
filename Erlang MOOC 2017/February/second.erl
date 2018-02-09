-module(second).
-compile(export_all).

hypot(Opp,Adj) -> math:sqrt(first:sqr(Opp) + first:sqr(Adj)).

perim(Opp,Adj) -> Opp + Adj + hypot(Opp,Adj).

area(Opp,Adj) -> first:area(Opp,Adj,hypot(Opp,Adj)).

maxThree(A,B,C) when A =< B andalso B =< C -> C;
maxThree(A,B,_) when A =< B                -> B;
maxThree(A,B,C) when A >= B andalso B >= C -> A;
maxThree(A,_,C)                            -> max(A,C).

howManyEqual(A,A,A) -> 3;
howManyEqual(A,A,_) -> 2;
howManyEqual(A,_,A) -> 2;
howManyEqual(_,A,A) -> 2;
howManyEqual(_,_,_) -> 0.

xOr(true,X)  when is_boolean(X) -> not(X);
xOr(false,X) when is_boolean(X) -> X.

fact_reduce(N) -> lists:foldl(fun (X,Y) -> X*Y end, 1, lists:seq(2,N)).

fact_tail(0) -> 1;
fact_tail(N) when N > 0 -> do_fact(1,N).

do_fact(Acc,0) -> Acc;
do_fact(Acc,N) -> do_fact(Acc * N, N-1).

fib(0) -> 0;
fib(1) -> 1;
fib(N) when N >= 2 -> fib(0,1,N).

fib(N2,_,0)  -> N2;
fib(N2,N1,N) -> fib(N2+N1,N2,N-1).


fibP(0) -> {0,1};
fibP(N) ->
  {Prev,Curr} = fibP(N-1),
  {Curr,Prev+Curr}.

fib_dir(N) ->
  {P,_} = fibP(N),
  P.

fib_binet(N) ->
  Root5 = math:sqrt(5),
  trunc((math:pow(1 + Root5,N) - math:pow(1 - Root5,N)) / (math:pow(2,N) * Root5)).


pieces(0) -> 1;
pieces(N) when N > 0 -> N + pieces(N-1).

pieces_tail(0) -> 1;
pieces_tail(N) when N > 0 -> do_pieces_tail(N, N-1).

do_pieces_tail(Acc,0) -> Acc + 1;
do_pieces_tail(Acc,N) -> do_pieces_tail(Acc+N, N-1).

pieces_list(N) when N >= 0 -> lists:foldl(fun(X,Acc) -> Acc + X end, 1, lists:seq(1,N)).


perfect_lc(N) when N > 0 -> lists:sum([X || X <- lists:seq(1,N div 2), N rem X == 0]) == N.

perfect(N) when N > 5 -> perfect(0,2,N,N div 2);
perfect(_)            -> false.

perfect(Acc,Divisor,N,Nover2) when Divisor > Nover2   -> Acc + 1 == N;
perfect(Acc,Divisor,N,Nover2) when N rem Divisor == 0 -> perfect(Acc + Divisor, Divisor + 1, N, Nover2);
perfect(Acc,Divisor,N,Nover2)                         -> perfect(Acc,           Divisor + 1, N, Nover2).

performance(N) ->
  statistics(runtime),
  statistics(wall_clock),

  io:format("Is ~w a perfect number? ~w~n",[N,perfect(N)]),

  {_, CPUTime1}    = statistics(runtime),
  {_, ElapseTime1} = statistics(wall_clock),

  io:format("~nTail recursion needed:~n   CPU Time: ~pms~nElapse time: ~pms~n",[CPUTime1,ElapseTime1]),

  statistics(runtime),
  statistics(wall_clock),

  _Whatever = perfect_lc(N),

  {_, CPUTime2}    = statistics(runtime),
  {_, ElapseTime2} = statistics(wall_clock),

  io:format("~nList comprehension needed:~n   CPU Time: ~pms~nElapse time: ~pms~n",[CPUTime2,ElapseTime2]).


