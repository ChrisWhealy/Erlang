-module(server).
-compile(export_all).

start() -> spawn(?MODULE,handle_msg, []).

handle_msg() ->
  receive
    {check, Msg, From} ->
      From ! {result, is_palindrome(Msg)},
      handle_msg();

    stop ->
      io:format("server: Normal shutdown~n")
  end.

alpha_only(Chr) ->
  C = string:to_lower(Chr),
  $a =< C andalso C =< $z.

is_palindrome(L) ->
  F = string:to_lower(lists:filter(fun alpha_only/1, L)),
  F == lists:reverse(F).
