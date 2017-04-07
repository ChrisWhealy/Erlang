-module(rpsls).
-compile(export_all).

-define(OPTION_COUNT, 5).

% -----------------------------------------------------------------------------
% Who looses to who
% -----------------------------------------------------------------------------
options(1) -> rock;
options(2) -> paper;
options(3) -> scissors;
options(4) -> lizard;
options(5) -> spock.

beats(rock)     -> [paper,spock];
beats(paper)    -> [scissors,lizard];
beats(scissors) -> [spock,rock];
beats(lizard)   -> [scissors,rock];
beats(spock)    -> [paper,lizard].

losesTo(rock)     -> [scissors,lizard];
losesTo(paper)    -> [spock,rock];
losesTo(scissors) -> [paper,lizard];
losesTo(lizard)   -> [paper,spock];
losesTo(spock)    -> [scissors,rock].

round(L,R) when L == R -> 0;
round(L,R) ->
  case lists:member(R,losesTo(L)) of
    true  -> +1;
    false -> -1
  end.

tournament(Left,Right) ->
  lists:foldl(fun({L,R},Acc) -> round(L,R) + Acc end, 0, lists:zip(Left,Right)).

% -----------------------------------------------------------------------------
% Interactive play
% -----------------------------------------------------------------------------
play(Strategy) ->
  io:format("Rock - Paper - Scissors - Lizard - Spock~n"),
  io:format("Play one of rock, paper, scissors, ...~n"),
  io:format("... r, p, s, l, v, stop, followed by '.'~n"),
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
expand(l) -> lizard;
expand(v) -> spock;
expand(X) -> X.

who_wins(-1,{You,Me}) -> {"I win", {You,Me+1}};
who_wins(0,S)         -> {"Draw",S};
who_wins(1,{You,Me})  -> {"You win",{You+1,Me}}.

% -----------------------------------------------------------------------------
% Strategies - ranging from really dumb to ever-so-slightly intelligent
% -----------------------------------------------------------------------------
rock(_) -> rock.

echo_last([])    -> paper;
echo_last([H|_]) -> H.

% Randomly choose a winner
beats_last([])     -> lists:nth(rand:uniform(2), beats(paper));
beats_last([H|_])  -> lists:nth(rand:uniform(2), beats(H)).

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

