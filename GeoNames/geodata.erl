-module(geodata).
-compile(export_all).

service(SessionID, _Env, Input) ->
  print_query_str_tokens(parse_query_str(Input)),

  mod_esi:deliver(SessionID, [
    "Content-Type: text/html\r\n\r\n",
    "<html><body>Look in the console to see the parameter values</body></html>"
  ]).


print_query_str_tokens(Tokens) -> lists:map(fun({Name, Value}) -> io:format("{~w,~s}~n", [Name, Value]) end, Tokens).
  

print_query_str(Qstr) ->
  ParmList = string:tokens(Qstr,"&"),
  lists:map(fun(NV_Pair) -> io:format("~s = ~s~n", string:tokens(NV_Pair,"=")) end, ParmList).

parse_query_str(Qstr) ->
  ParmList = string:tokens(Qstr,"&"),
  lists:foldl(
    fun(NV_Pair, Acc) ->
      [Name, Value] = string:tokens(NV_Pair,"="),
      [{list_to_atom(Name), Value}] ++ Acc
    end,
    [],
    ParmList).
    
