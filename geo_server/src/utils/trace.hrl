% *****************************************************************************
% Trace macro switches on when the compile flag 'debug' is defined
% *****************************************************************************
-define(FUNCTION_SIG, io_lib:format("~s:~s/~w ",[?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY])).

-ifdef(debug).
  -define(TRACE(Str),         io:fwrite("~s ~s~n",             [?FUNCTION_SIG, Str])).
  -define(TRACE(FStr,Params), io:fwrite("~s" ++ FStr ++ "~n", [?FUNCTION_SIG] ++ Params)).
-else.
  -define(TRACE(_),   void).
  -define(TRACE(_,_), void).
-endif.
