% *****************************************************************************
% In order to see trace output, compile using c(freq_client,{d,debug}).
% *****************************************************************************
-module(freq_client).
-export([start_server/0, stop_server/0]).
-export([request_frequency/0, drop_frequency/1]).

% Length of time the client will wait for a server response (in ms)
-define(TIMEOUT, 500).

% *****************************************************************************
% Trace macro based on the presence of the compile flag 'debug'
% *****************************************************************************
-define(FUNCTION_SIG, io_lib:format("~s:~s/~w ",[?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY])).

-ifdef(debug).
  -define(TRACE(X), io:fwrite("~s ~s~n",[?FUNCTION_SIG,X])).
-else.
  -define(TRACE(X), void).
-endif.

% *****************************************************************************
% Public API
% *****************************************************************************

% -----------------------------------------------------------------------------
% Start server.
% Check to see if the server is already running before attempting to register
% the process name
start_server() ->
  % Does the server's registered name already exist?
  case whereis(freq_server) of
    undefined ->
      % Nope, so spawn the server and register its name
      ?TRACE("Starting server"),
      register(freq_server, spawn(freq_server, init, []));

    _Pid ->
      % Yup, so the server is already running. Nothing more to do.
      ?TRACE("Server already running"),
      true
  end.

% -----------------------------------------------------------------------------
% Stop the server
stop_server() ->
  ?TRACE("Stopping server"),
  send_to_server(stop).

% -----------------------------------------------------------------------------
% The client needs a freqeuncy
request_frequency() ->
  ?TRACE("Requesting a frequency"),
  send_to_server(allocate).

% -----------------------------------------------------------------------------
% The client no longer needs its allocated frequency
drop_frequency(F) ->
  ?TRACE("Deallocating frequency " ++ make_str(F)),
  send_to_server({deallocate, F}).


% *****************************************************************************
% Private API
% *****************************************************************************

% -----------------------------------------------------------------------------
% How old is the message just retrieved from the mailbox?
% In other words, did we receive this message after we have already timed out
% waiting for the server to respond?
is_msg_stale(T) -> (?TIMEOUT * 1000) - T =< 0.

make_str(X) when is_tuple(X)   orelse
                 is_pid(X)     orelse
                 is_integer(X) orelse
                 is_atom(X)           -> io_lib:format("~w",[X]);
make_str(L) when is_list(L)           -> io_lib:format("~s",[L]).

% -----------------------------------------------------------------------------
% Handles all server communication
%
% All requests sent to the server are a four tuple containing:
% - The atom 'request'
% - The client's process id
% - The timestamp of when the client sent the request (needed for deciding
%   whether or not the server responded in time)
% - The request payload
send_to_server(Payload) ->
  ?TRACE("Sending " ++ make_str(Payload) ++ " to server"),
  freq_server ! {request, self(), os:timestamp(), Payload},
  listen_to_server().

% -----------------------------------------------------------------------------
% Waits for a response from the server
listen_to_server() ->
  receive
    % Server responded to stop command
    {reply, ExecTime, stopped} ->
      io:fwrite("Frequency server stopped in " ++ make_str(ExecTime) ++ "ms~n");

    % Normal server reply
    {reply, ExecTime, {Cmd, F} = Msg} ->
      % Is the current server response message a stale one?
      % I.E. did the server respond to a previous command after we have already
      % timed out?  If so, then this message is considered stale.
      case is_msg_stale(ExecTime) of
        true ->
          % Yes this is a stale message, so ignore it and process the next one
          ?TRACE("Ignoring stale message " ++ make_str(Msg)),
          listen_to_server();

        _ ->
          % Nope.  This message is good and will be the response to either an
          % alloc or dealloc command
          case Cmd of
            error   -> io:fwrite("Error: " ++ F ++ "~n");
            alloc   -> io:fwrite("Frequency " ++ make_str(F) ++ " allocated~n");
            dealloc -> io:fwrite("Frequency " ++ make_str(F) ++ " deallocated~n")
          end
      end

  % After a preset number of milliseconds, we'll get bored and stop waiting
  after ?TIMEOUT ->
    frequency_server_timeout
  end.
