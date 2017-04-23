% *****************************************************************************
% Trace macro switches on when the compile flag 'debug' is defined
% *****************************************************************************
-define(FUNCTION_SIG, io_lib:format("~s:~s/~w ",[?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY])).

-ifdef(debug).
  -define(TRACE(X), io:fwrite("~s ~s~n",[?FUNCTION_SIG,X])).
-else.
  -define(TRACE(X), void).
-endif.

