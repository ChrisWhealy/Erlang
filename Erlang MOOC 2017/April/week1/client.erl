-module(client).
-compile(export_all).

check(Msg,ServerPid) ->
  ServerPid ! {check, Msg, self()},

  receive
    {result, true}  -> io:format("\"~s\" is a palindrome~n",[Msg]);
    {result, false} -> io:format("\"~s\" is not a palindrome~n",[Msg])
    
    after 2000 -> server_timeout
  end.

stop(ServerPid) -> ServerPid ! stop.
