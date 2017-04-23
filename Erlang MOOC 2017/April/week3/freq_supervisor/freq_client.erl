% *****************************************************************************
% In order to see trace output, compile using c(freq_client,{d,debug}).
% *****************************************************************************
-module(freq_client).

-export([request_frequency/0, drop_frequency/1, free_frequencies/0]).

-include("utils.hrl").
-include("trace.hrl").

% Length of time the client will wait for a server response (in ms)
-define(TIMEOUT, 500).

% *****************************************************************************
% Public API
% *****************************************************************************

% -----------------------------------------------------------------------------
% The client requests a frequency
request_frequency() ->
  ?TRACE("Requesting a frequency"),
  put(alloc_info, send_to_server(allocate)).

% -----------------------------------------------------------------------------
% The client no longer needs its allocated frequency
drop_frequency(F) ->
  ?TRACE("Deallocating frequency " ++ make_str(F)),
  {Freq,ServerId} = get(alloc_info),
  send_to_server({deallocate, Freq, ServerId}).

% -----------------------------------------------------------------------------
% Ask the server which frequencies it currently has free
free_frequencies() ->
  ?TRACE("Asking what frequencies are currently free"),
  send_to_server(free_freqs).



% *****************************************************************************
% Private API
% *****************************************************************************

% -----------------------------------------------------------------------------
% How old is the message just retrieved from the mailbox?
% In other words, did we receive this message after we have already timed out
% waiting for the server to respond?
is_msg_stale(T) -> (?TIMEOUT * 1000) - T =< 0.

% -----------------------------------------------------------------------------
% Handles all communication with the supervisor
%
% All requests sent to the supervisor are a four tuple containing:
% - The atom 'request'
% - The client's process id
% - The timestamp of when the client sent the request (needed for deciding
%   whether or not the server responded in time)
% - The request payload
send_to_server(Payload) ->
  ?TRACE("Sending " ++ make_str(Payload) ++ " to supervisor"),
  freq_supervisor ! {request, self(), os:timestamp(), Payload},
  listen_to_server().

% -----------------------------------------------------------------------------
% Waits for a response from the server
listen_to_server() ->
  receive
    % Normal server reply
    {reply, ExecTime, {Cmd, Freq, ServerId} = Msg} ->
      % Is the current server response message a stale one?
      % I.E. did the server respond to a previous command after we have already
      % timed out?  If so, then this message is considered stale.
      case is_msg_stale(ExecTime) of
        true ->
          % Yes this is a stale message and can be ignored unless it is a
          % notification of lease expiry.  Either way, process the next message
          case Cmd of
            lease_expired -> io:fwrite("Lease expired: Frequency " ++ make_str(Freq) ++
                                       " deallocated~n");
            _             -> io:fwrite("Ignoring stale message " ++ make_str(Msg) ++ "~n")
          end,

          listen_to_server();

        _ ->
          % Nope.  This message is good and will be the response to either an
          % alloc or dealloc command
          case Cmd of
            error      -> io:fwrite("Error: " ++ make_str(Freq) ++ "~n");
            alloc      -> io:fwrite("Frequency " ++ make_str(Freq) ++
                                    " allocated by server " ++ make_str(ServerId) ++ "~n");
            dealloc    -> io:fwrite("Frequency " ++ make_str(Freq) ++ " deallocated~n");
            free_freqs -> io:fwrite("Free frequencies are ~w~n",[Freq])
          end
      end

  % After a preset number of milliseconds, we'll get bored and stop waiting
  after ?TIMEOUT ->
    frequency_server_timeout
  end.
