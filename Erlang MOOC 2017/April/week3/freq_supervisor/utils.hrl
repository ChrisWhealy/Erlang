% -----------------------------------------------------------------------------
% Create strings from various Erlang data types
% -----------------------------------------------------------------------------
make_str(X) when is_tuple(X)   orelse
                 is_pid(X)     orelse
                 is_integer(X) orelse
                 is_atom(X)           -> io_lib:format("~w",[X]);
make_str(L) when is_list(L)           -> io_lib:format("~s",[L]).

