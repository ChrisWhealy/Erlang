-module(client_test).
-export([start/1, wrap_client/2, client_simulator/5]).

-define(SIM_DELAY, 10000).
-define(TEST_PLAN_COUNT, 4).
-define(TEST_PLANS, [[request_frequency, stop]
                    ,[request_frequency, crash]
                    ,[request_frequency, drop_frequency, stop]
                    ,[request_frequency, drop_frequency, crash]
                    ]).
-define(GET_TEST_PLAN(N), lists:nth(N,?TEST_PLANS)).

% *****************************************************************************
% Start N client simulator processes (function wrap_client/2)
% These will be sent subsequent actions as defined in the above test plans
% that simulate client behaviour
% *****************************************************************************
start(N) ->
  % Start the Erlang process observer and fire up the frequency server
  observer:start(),
  freq_server:start(),

  % Spawn N client wrappers and keep their Pids in a list
  io:fwrite("Starting ~w clients~n",[N]),
  ClientPids = [{Id, spawn(?MODULE, wrap_client, [Id, none])} || Id <- lists:seq(1,N)],

  % Run a simulator process to send commands to each client wrapper
  run_simulators(ClientPids,?TEST_PLAN_COUNT),

  % Shut down all client wrapper processes and the frequency server
  [ P ! stop || {_, P} <- ClientPids],
  freq_server:stop().

% *****************************************************************************
% This function acts as a wrapper around the client behaviour and then waits
% for commands to arrive.  These command are then invoked as client requests to
% the server
% *****************************************************************************
wrap_client(Id,Freq) ->
  receive
    % Client requests a new frequency if it doesn't already have one
    request_frequency ->
      case Freq of
        none ->
          io:fwrite("Client ~w requesting frequency~n",[Id]),
          wrap_client(Id,freq_client:request_frequency());
        _ ->
          wrap_client(Id,Freq)
      end;

    % Client drops its allocated frequency if it has one
    drop_frequency ->
      case Freq of
        none ->
          wrap_client(Id,none);
        _ ->
          io:fwrite("Client ~w dropping frequency ~w~n",[Id,Freq]),
          wrap_client(Id,freq_client:drop_frequency(Freq))
      end;

    % Client goes bang!
    crash ->
      io:fwrite("Client ~w crashing~n",[Id]),
      exit(crash);

    % Client stops normally
    stop ->
      io:fwrite("Client ~w stopped normally~n",[Id]),
      exit(normal)

  end.

% *****************************************************************************
% Runs as many client simulators as needed in parallel, then wait for each to
% finish.
% Each spawned client_simulator/5 function then sends commands to wrap_client/2
% based on the contents of the test plan
% *****************************************************************************
run_simulators(ClientPids,N) ->
  Sim_pids = [spawn(?MODULE, client_simulator, [Id, Pid, N, 1, self()]) || {Id, Pid} <- ClientPids],
  [receive {Pid, done} -> done end || Pid <- Sim_pids].

% *****************************************************************************
% Handles sending the next command in the test plan to a single client
% *****************************************************************************
client_simulator(_Id, _ClientPid, N, N, From) ->
  % This client simulator has finished so report back to the parent
  From ! {self(), done};

client_simulator(Id, ClientPid, N, T, From) ->
  io:fwrite("Running test plan ~w for client ~w~n",[T, Id]),

  % Run instructions belonging to the current test plan
  % Since the current test plan might cause the client process to crash,
  % run_test_plan/3 always returns the Pid of the Client because this is
  % potentially a new process created after a crash or normal termination
  NewClientPid = run_test_plan(Id, ClientPid, ?GET_TEST_PLAN(T)),

  % Rerun the client simulator with the next set of actions in the test plan
  client_simulator(Id, NewClientPid, N, T+1, From).


% *****************************************************************************
% Run the instructions in a single test plan
% *****************************************************************************
run_test_plan(_Id, ClientPid, []) -> ClientPid;

run_test_plan(Id, ClientPid, [Action|Rest]) ->
  % Sleep for a random number of milleseconds before invoking action
  Delay = rand:uniform(?SIM_DELAY),
  io:fwrite("Client ~w simulator waiting for ~wms before invoking ~w~n",[Id, Delay, Action]),
  timer:sleep(Delay),

  % Tell the client to perform the next action in the test plan
  ClientPid ! Action,

  % If the previous action was 'stop' or 'crash', then the current ClientPid
  % no longer exists, so the client process must be recreated
  NewClientPid =
    case Action of
      stop  ->
        io:fwrite("Restarting client ~w after normal stop~n",[Id]),
        spawn(?MODULE, wrap_client, [Id, none]);

      crash ->
        io:fwrite("Restarting client ~w after crash~n",[Id]),
        spawn(?MODULE, wrap_client, [Id, none]);

      _ ->
        ClientPid
    end,

  % Run remaining actions in the current test plan
  run_test_plan(Id, NewClientPid, Rest).


