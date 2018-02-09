-module(rps).
-export([tournament/2
        , play/1
        , rock/1
        , echo/1
        , beats_last/1
        , random/1
        , cycle/1
        , most_used/1]).

-define(OPTION_COUNT, 3).

% -----------------------------------------------------------------------------
% Who looses to who
% -----------------------------------------------------------------------------
options(1) -> rock;
options(2) -> paper;
options(3) -> scissors.

lose(rock)     -> scissors;
lose(paper)    -> rock;
lose(scissors) -> paper.

beats(rock)     -> paper;
beats(paper)    -> scissors;
beats(scissors) -> rock.

round(L,R) when L == R -> 0;
round(L,R) ->
  case lose(L) == R of
    true  -> +1;
    false -> -1
  end.

tournament(Left,Right) ->
  lists:foldl(fun({L,R},Acc) -> round(L,R) + Acc end, 0, lists:zip(Left,Right)).

% -----------------------------------------------------------------------------
% Interactive play
% -----------------------------------------------------------------------------
play(Strategy) ->
  io:format("Rock - paper - scissors~n"),
  io:format("Play one of rock, paper, scissors, ...~n"),
  io:format("... r, p, s, stop, followed by '.'~n"),
  play(Strategy,[],{0,0}).

play(Strategy,Moves,Score) ->
  {ok,P} = io:read("You play: "),
  UPlay = expand(P),

  case UPlay of
  	stop ->
	    io:format("Stopped~n");
	  _    ->
      IPlay = Strategy(Moves),
      {Winner,{YourScore,MyScore}} = who_wins(round(UPlay,IPlay),Score),

	    io:format("I play: ~p~nResult: ~p~nScore ~p:~p~n~n",[IPlay,Winner,YourScore,MyScore]),
	    play(Strategy,[UPlay|Moves],{YourScore,MyScore})
  end.

expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

who_wins(-1,{You,Me}) -> {"I win", {You,Me+1}};
who_wins(0,S)         -> {"Draw",S};
who_wins(1,{You,Me})  -> {"You win",{You+1,Me}}.

% -----------------------------------------------------------------------------
% Strategies - ranging from really dumb to slightly intelligent
% -----------------------------------------------------------------------------
rock(_) -> rock.

echo([])    -> paper;
echo([H|_]) -> H.

beats_last([])     -> beats(paper);
beats_last([H|_])  -> beats(H).

random(_)    -> options(rand:uniform(?OPTION_COUNT)).

cycle(Moves) -> options(length(Moves) rem ?OPTION_COUNT + 1). 

most_used(Ms) -> mode(Ms).





mode([]) -> paper;
mode(L)  -> mode([],lists:sort(L)).

mode(Acc, [])      -> {_Count,Val} = lists:max(Acc), Val;
mode(Acc, [X])     -> {_Count,Val} = lists:max([{1,X}|Acc]), Val;
mode(Acc, [X|_]=L) ->
  {F,R} = lists:splitwith(fun(Y) -> Y == X end, L),
  mode([{length(F),hd(F)}|Acc],R).

