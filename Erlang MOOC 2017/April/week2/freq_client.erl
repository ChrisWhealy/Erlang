% *****************************************************************************
% In order to see trace output, compile using c(freq_client,{d,debug}).
% *****************************************************************************
-module(freq_client).
-export([request_frequency/0, drop_frequency/1, free_frequencies/0, inject/1]).

% Length of time the client will wait for a server response (in ms)
-define(TIMEOUT, 500).

% *****************************************************************************
% Trace macro switches on when the compile flag 'debug' is defined
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
% The client requests a frequency
request_frequency() ->
  ?TRACE("Requesting a frequency"),
  send_to_server(allocate).

% -----------------------------------------------------------------------------
% The client no longer needs its allocated frequency
drop_frequency(F) ->
  ?TRACE("Deallocating frequency " ++ make_str(F)),
  send_to_server({deallocate, F}).

% -----------------------------------------------------------------------------
% Ask the server which frequencies it currently has free
free_frequencies() ->
  ?TRACE("Asking server what frequencies it currently has free"),
  send_to_server(free_freqs).

% -----------------------------------------------------------------------------
% Inject a new set of frequencies into the server
inject(Freqs) ->
  ?TRACE("Injecting new frequencies into the server"),
  send_to_server({inject,Freqs}).


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
    % Normal server reply
    {reply, ExecTime, {Cmd, F} = Msg} ->
      % Is the current server response message a stale one?
      % I.E. did the server respond to a previous command after we have already
      % timed out?  If so, then this message is considered stale.
      case is_msg_stale(ExecTime) of
        true ->
          % Yes this is a stale message and can be ignored unless it is a
          % notification of lease expiry.  Either way, process the next message
          case Cmd of
            lease_expired -> io:fwrite("Lease expired: Frequency " ++ make_str(F) ++ " no longer allocated~n");
            _             -> io:fwrite("Ignoring stale message " ++ make_str(Msg) ++ "~n")
          end,

          listen_to_server();

        _ ->
          % Nope.  This message is good and will be the response to either an
          % alloc or dealloc command
          case Cmd of
            error      -> io:fwrite("Error: " ++ make_str(F) ++ "~n");
            alloc      -> io:fwrite("Client " ++ make_str(self()) ++
                                    " is allocated frequency " ++ make_str(F) ++ "~n"),
                          F;
            dealloc    -> io:fwrite("Frequency " ++ make_str(F) ++
                                    " deallocated from client " ++ make_str(self()) ++ "~n");
            free_freqs -> io:fwrite("Free frequencies are ~w~n",[F]);
            inject     -> io:fwrite("Frequencies injected~n")
          end
      end

  % After a preset number of milliseconds, we'll get bored and stop waiting
  after ?TIMEOUT ->
    frequency_server_timeout
  end.
