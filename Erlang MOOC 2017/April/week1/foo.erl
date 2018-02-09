-module(foo).
-compile(export_all).

bar() ->
  timer:sleep(500),
  io:format("bar started~n"),
  io:format("bar working~n"),
  io:format("bar finished~n").

bar(Pid) ->
  Pid ! "bar started~n",
  Pid ! "bar working~n",
  Pid ! "bar finished~n".

baz() ->
  receive
    Msg -> io:format("got: ~s~n", [Msg])
  end,

  baz().

buz() ->
  receive
    stop -> io:format("stopped~n");
    Msg  -> io:format("got: ~s~n", [Msg]),
            buz()
  end.
