-module(freq_supervisor).

-export([start/0, init/1, stop/0]).
-export([add_server/1, drop_server/1, list_servers/0]).

-include("utils.hrl").
-include("trace.hrl").

% *****************************************************************************
% Configuration values for default behaviour
% *****************************************************************************
-define(RESTART_LIMIT, 2).
-define(SERVER_COUNT, 2).
-define(FREQUENCY_LIST(N), lists:nth(N, [[10,11,12,13,14,15],
                                         [20,21,22,23,24,25]])).
-define(SEPARATOR,"+----------------------------------+").


% *****************************************************************************
% Public API
% *****************************************************************************

% -----------------------------------------------------------------------------
% Start a supervisor that administers the frequency servers
% -----------------------------------------------------------------------------
start() ->
  case whereis(?MODULE) of 
    undefined ->
      ?TRACE("Starting frequency server supervisor"),
      register(?MODULE,spawn(?MODULE, init, [?SERVER_COUNT]));

    _ ->
      supervisor_already_running
  end.


% -----------------------------------------------------------------------------
% Stop the supervisor
% -----------------------------------------------------------------------------
stop() ->
  ?TRACE("Stopping frequency server supervisor"),
  ?MODULE ! {stop, self()},

  receive supervisor_stopped -> done
    after 1000               -> supervisor_lost
  end.

% -----------------------------------------------------------------------------
% Initialise the supervisor and start the required number of frequency servers
% -----------------------------------------------------------------------------
init(ServerCount) ->
  ?TRACE(lists:concat(["Initializing ", ServerCount, " frequency servers"])),
  process_flag(trap_exit,true),

  % Start each server and assign it a set of frequencies from the default list
  ServerList =
    [start_server(N,?FREQUENCY_LIST(N), 0) || N <- lists:seq(1,ServerCount)],

  % Watch the kids - all of them...
  child_minder(ServerList,1).

% -----------------------------------------------------------------------------
% Add a new server that administers the frequencies listed in its argument
% -----------------------------------------------------------------------------
add_server(FreqList) -> ?MODULE ! {add, FreqList}.

% -----------------------------------------------------------------------------
% Drop an existing server identified by its id
% -----------------------------------------------------------------------------
drop_server(N) -> ?MODULE ! {drop, N}.

% -----------------------------------------------------------------------------
% List the currently active servers
% -----------------------------------------------------------------------------
list_servers() -> ?MODULE ! list.




% *****************************************************************************
% Private API
% *****************************************************************************

% -----------------------------------------------------------------------------
% Start a single frequency server
% Do not restart the server if it has crashed more than ?RESTART_LIMIT times
% -----------------------------------------------------------------------------
start_server(_ServerId, _FreqList, RestartCount) when RestartCount > ?RESTART_LIMIT ->
  ?TRACE(lists:concat(["Frequency server ", _ServerId, " not restarted. Restart limit exceeded"])),
  false;

start_server(ServerId, FreqList, RestartCount) ->
  ?TRACE(lists:concat(["Frequency server ", ServerId, " starting (Restarts = ", RestartCount, ")"])),
  {ServerId, ServerPid} = freq_server:start(ServerId, FreqList),
  link(ServerPid),

  % Return the server's tag
  {ServerId, ServerPid, FreqList, RestartCount}.

% -----------------------------------------------------------------------------
% Write the details of a server to the console
% -----------------------------------------------------------------------------
write_server_details({ServerId, ServerPid, FreqList, RestartCount}) ->
  io:fwrite("~s~nServer Id      : ~w~nServer Pid     : ~s~nFrequency List : ~w~nRestart count  : ~w~n",
            [?SEPARATOR, ServerId, pid_to_list(ServerPid), FreqList, RestartCount]).

% -----------------------------------------------------------------------------
% Handle server communication
send_to_server({_, _, _, allocate} = ClientMsg, ServerList, RoundRobin) ->
  {ServerId, ServerPid, _, _} = lists:nth(RoundRobin, ServerList),
  ?TRACE("Sending " ++ make_str(ClientMsg) ++ " to server " ++ make_str(ServerId)),
  ServerPid ! ClientMsg;

send_to_server({_, ClientPid, TS, {deallocate, Freq, ServerId}}, ServerList, _) ->
  ?TRACE("Sending {deallocate," ++ make_str(Freq) ++ "} to server " ++ make_str(ServerId)),
  {ServerId, ServerPid, _, _} = lists:nth(ServerId, ServerList),
  ServerPid ! {request, ClientPid, TS, {deallocate, Freq}}.

% -----------------------------------------------------------------------------
% The child minder process receives a list servers to look after
% -----------------------------------------------------------------------------
child_minder([],_) -> error(all_servers_exceeded_restart_limit);

child_minder(ServerList, RoundRobin) ->
  ?TRACE(lists:concat(["Monitoring ", length(ServerList), " frequency servers"])),

  receive
    % -------------------------------------------------------------------------
    % One of the servers died
    {'EXIT', ServerPid, _Reason} ->
      ?TRACE(lists:concat(["Frequency server ", pid_to_list(ServerPid),
                           " went down with reason ", make_str(_Reason)])),

      % Is this one of my servers?
      NewServerList1 = 
        case lists:keyfind(ServerPid, 2, ServerList) of
          % Nope, so I don't care...
          false -> ServerList;

          % Yup, so we'd better try to restart it
          {ServerId, _, FreqList, RestartCount} ->
            % Remove old ServerPid from ServerList
            NewServerList2 = lists:keydelete(ServerPid, 2, ServerList),

            % Attempt to restart the server
            case start_server(ServerId, FreqList, RestartCount + 1) of
              false     -> NewServerList2;
              ServerTag -> [ServerTag | NewServerList2]
            end
        end,

      ?TRACE("NewServerList1 = " ++ io_lib:format("~w",[NewServerList1])),
      child_minder(NewServerList1, RoundRobin);

    % -------------------------------------------------------------------------
    % Stop the supervisor
    {stop, From} ->
      ?TRACE("Frequency supervisor shutting down"),
      From ! supervisor_stopped,
      exit(supervisor_shutdown);

    % -------------------------------------------------------------------------
    % Add a new frequency server
    {add, FreqList} ->
      ?TRACE("Adding new frequency server"),
      % Find next unused server id
      ServerId = element(1,lists:max(ServerList)) + 1,
      NewServerList = [start_server(ServerId,FreqList,0) | ServerList],
      child_minder(NewServerList, RoundRobin);

    % -------------------------------------------------------------------------
    % Drop an existing frequency server
    {drop, N} ->
      ?TRACE(lists:concat(["Dropping frequency server ",N])),

      % Check if server id exists
      NewServerList =
        case lists:keyfind(N, 1, ServerList) of
          false ->
            ?TRACE(lists:concat(["No server with id ", N, " found"])),
            ServerList;

          {_ServerId, ServerPid, _FreqList, _RestartCount} ->
            exit(ServerPid,dropped_by_supervisor),
            lists:keydelete(N, 1, ServerList)
        end,

      NewRoundRobin = case length(NewServerList) of
        N when RoundRobin > N -> 1;
        _                     -> RoundRobin
      end,

      child_minder(NewServerList, NewRoundRobin);
    
    % -------------------------------------------------------------------------
    % List the currently running frequency servers
    list ->
      [write_server_details(ServerTag) || ServerTag <- ServerList],
      child_minder(ServerList, RoundRobin);

    % -------------------------------------------------------------------------
    % Pass a client request on to a server
    {request, ClientPid, Ts, Payload} ->
      send_to_server({request, ClientPid, Ts, Payload}, ServerList, RoundRobin),

      NewRoundRobin = case length(ServerList) of
        RoundRobin -> 1;
        _          -> RoundRobin + 1
      end,

      child_minder(ServerList, NewRoundRobin)

  end.

