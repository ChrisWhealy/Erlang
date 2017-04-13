-module(freq_supervisor).
-export([start/0, init/0, stop/0]).

-define(RESTART_LIMIT, 2).

% *****************************************************************************
% Start a supervisor that administers the frequency server
% *****************************************************************************
start() ->
  case whereis(?MODULE) of 
    undefined -> register(?MODULE,spawn(?MODULE, init, []));
    _         -> true
  end.


% *****************************************************************************
% Stop the supervisor
% *****************************************************************************
stop() ->
  ?MODULE ! {stop, self()},

  receive
    supervisor_stopped -> done
  after 1000 -> supervisor_lost
  end.

% *****************************************************************************
% Initialise the supervisor
% *****************************************************************************
init() ->
  process_flag(trap_exit,true),
  child_minder(start_server(0)).

% *****************************************************************************
%
% *****************************************************************************
start_server(Restart) ->
  freq_server:start(),
  ServerPid = whereis(freq_server),
  ServerRef = monitor(process, ServerPid),
  {ServerPid, ServerRef, Restart}.

% *****************************************************************************
% The child minder process receives a three tuple of the server pid, its
% monitor reference and its restart count
% Look after the server and restart it if it crashes.
% Do not restart the server if it has crashed more than ?RESTART_LIMIT times
% *****************************************************************************
child_minder({ServerPid,ServerRef,Restart}) ->
  io:format("Watching frequency server ~s~n",[pid_to_list(ServerPid)]),

  receive
    {'DOWN',ServerRef, process, ServerPid, Reason} when Restart =< ?RESTART_LIMIT ->
      io:format("Frequency server ~s went down with reason ~w.  Restart count = ~w~n",[pid_to_list(ServerPid), Reason, Restart+1]),
      child_minder(start_server(Restart + 1));

    {'DOWN',ServerRef, process, ServerPid, _Reason} ->
      io:format("Frequency server not restarted. Restart limit exceeded~n"),
      io:format("Supervisor terminating."),
      exit(normal);

    {stop, From} ->
      io:format("Frequency supervisor shutting down~n"),
      exit(ServerPid,stopped_by_supervisor),
      From ! supervisor_stopped

  end.

