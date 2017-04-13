% *****************************************************************************
% In order to see trace output, compile using c(freq_server,{d,debug}).
% *****************************************************************************
-module(freq_server).
-export([start/0, stop/0, init/0, loop/1]).

% Delay (in ms) to simulate server load
-define(SERVER_DELAY, 200).

% Frequency lease validity period (in ms)
-define(LEASE_EXPIRY, 30000).

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
% Start the frequency server using a predefined list of frequencies
% The server must also listen for client termination
init() ->
  process_flag(trap_exit, true),
  freq_server:loop({get_frequencies(), []}).

% -----------------------------------------------------------------------------
% Start server.
% Check to see if the server is already running before attempting to register
% the process name
start() ->
  % Is the server already running?
  case whereis(?MODULE) of
    undefined ->
      % Nope, so spawn the server and register its name
      ?TRACE("Starting frequency server"),
      register(?MODULE, spawn(?MODULE, init, []));

    _Pid ->
      % Yup, so the server is already running. Nothing more to do.
      ?TRACE("Frequency server already running"),
      true
  end.

% -----------------------------------------------------------------------------
% Stop the server
stop() ->
  ?TRACE("Stopping frequency server"),
  ?MODULE ! {request, self(), os:timestamp(), stop},
  flush_mailbox().

% *****************************************************************************
% Private API
% *****************************************************************************

% -----------------------------------------------------------------------------
% Provide a hard-coded list of frequencies
%get_frequencies() -> [10,11,12,13,14,15].
get_frequencies() -> [10].

% -----------------------------------------------------------------------------
% Empty the mailbox ignoring all messages except for a stop command
flush_mailbox() ->
  receive
    {reply, ExecTime, stopped} ->
      io:fwrite("Frequency server stopped in " ++ make_str(ExecTime) ++ "ms~n"),
      flush_mailbox();

    Msg ->
      io:fwrite("Flushing stale message " ++ make_str(Msg) ++ "~n"),
      flush_mailbox()

  after 0 ->
    done
  end.

% -----------------------------------------------------------------------------
% Create strings from various Erlang data types
make_str(X) when is_tuple(X)   orelse
                 is_pid(X)     orelse
                 is_integer(X) orelse
                 is_atom(X)           -> io_lib:format("~w",[X]);
make_str(L) when is_list(L)           -> io_lib:format("~s",[L]).

% -----------------------------------------------------------------------------
% Handle frequency allocation
% Each allocated frequency now contains a timestamp to allow for lease
% expiration
allocate({[], Allocated}, _TS, _ClientPid) ->
  {{[], Allocated}, {error, no_free_frequencies}};

allocate({[Freq|Rest] = Free, Allocated}, TS, ClientPid) ->
  ?TRACE("Allocating next frequency to client " ++ make_str(ClientPid)),

  % Check first if a frequency has already been allocated to the client
  case lists:keyfind(ClientPid, 2, Allocated) of
    false ->
      % Nope, so allocate a frequency and watch the client process in case it
      % dies before deallocating the frequency
      link(ClientPid),
      {{Rest, [{Freq, ClientPid, TS} | Allocated]}, {alloc, Freq}};

    {_, ClientPid, _TS} ->
      % Yup. Client already has an allocated frequency
      {{Free, Allocated}, {error, frequency_already_allocated}}
  end.

% -----------------------------------------------------------------------------
% Handle frequency deallocation
deallocate({Free, Allocated}, Freq) ->
  % Check first that the frequency being deallocated belongs to the client 
  case lists:keyfind(Freq, 1, Allocated) of
    {Freq, ClientPid, _TS} ->
      % Unlink from the client Pid, remove the frequency from allocated list,
      % then inform the client
      ?TRACE("Deallocating frequency " ++ make_str(Freq) ++ " from " ++ make_str(ClientPid)),
      unlink(ClientPid),
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {{[Freq|Free], NewAllocated}, {dealloc, Freq}};

    false ->
      % Can't deallocate a frequency that doesn't belong to you!
      {{Free, Allocated}, {error, not_your_frequency}}

  end.

% -----------------------------------------------------------------------------
% Inject new frequencies
inject({Free, Allocated}, Freqs) ->
  % Merge the new frequencies into the list of free frequencies removing and
  % duplicates
  NewFree = lists:sort(sets:to_list(sets:from_list(lists:merge(Free, Freqs)))),
  {{NewFree, Allocated}, {inject, ok}}.

% -----------------------------------------------------------------------------
% Handle frequency deallocation due to client process termination
exited({_Free,Allocated} = Frequencies, ClientPid) ->
  ?TRACE("Deallocating frequency due to termination of client " ++ make_str(ClientPid)),

  % Check that we have a record of allocating a frequency to this client
  case lists:keyfind(ClientPid, 2, Allocated) of
    % If its there, dellocate it in the normal way
    {Freq, ClientPid, _TS} ->
      % Since this function is only called when the client process has died,
      % we don't care about the reply from dellocate because there's no client
      % process to which that reply could be sent!
      {NewFrequencies, _Reply} = deallocate(Frequencies, Freq),
      NewFrequencies;
  
    % If its not there, then there's nothing to do. Just return the unmodified
    % frequency list
    false -> Frequencies
  end.

% -----------------------------------------------------------------------------
% Check timestamp for lease expiration
lease_too_old(TS) -> timer:now_diff(os:timestamp(), TS) >= ?LEASE_EXPIRY * 1000.

% -----------------------------------------------------------------------------
% Check allocated frequencies for lease expiration
expire_leases({Free, Allocated}) -> do_expire_leases(Free,[], Allocated).

% -----------------------------------------------------------------------------
% Transfer frequencies from the Allocated to the Free list based on whether
% the lease has expired
do_expire_leases(NewFree,NewAlloc,[]) -> {NewFree, NewAlloc};

do_expire_leases(NewFree,NewAlloc,[{Freq,ClientPid,TS}|Rest]) ->
  ?TRACE("Checking expiration of frequency " ++ make_str(Freq)),

  % Has the lease on the allocated frequency expired?
  case lease_too_old(TS) of
    % Yup
    true ->
      % Inform client of lease expiration and unlink from client process
      ?TRACE("Lease on frequency " ++ make_str(Freq) ++
             " by client " ++ make_str(ClientPid) ++ " has expired"),
      ClientPid ! {reply, timer:now_diff(os:timestamp(), TS), {lease_expired, Freq}},
      unlink(ClientPid),

      % Move expired frequency to NewFree list and check next lease
      do_expire_leases([Freq|NewFree],NewAlloc,Rest);

    % Nope
    false ->
      % Move valid frequency to NewAlloc list and check next lease
      ?TRACE("Lease on frequency " ++ make_str(Freq) ++
             " by client " ++ make_str(ClientPid) ++ " is still valid"),
      do_expire_leases(NewFree,[{Freq,ClientPid,TS}|NewAlloc],Rest)

  end.

% -----------------------------------------------------------------------------
% Receive loop
loop(Frequencies) ->
  ?TRACE("Entering receive loop"),

  receive
    % Trap client exit signal
    {'EXIT', ClientPid, _Reason} ->
      ?TRACE("Client process " ++ pid_to_list(ClientPid) ++ " died with reason " ++ make_str(_Reason)),
      freq_server:loop(exited(Frequencies, ClientPid));

    % Stop the server
    {request, ClientPid, TS, stop} ->
      ?TRACE("Processing stop command"),
      ClientPid ! {reply, timer:now_diff(os:timestamp(), TS), stopped};

    % Process command from the client
    {request, ClientPid, TS, Cmd} ->
      % Simulate server load
      ?TRACE("Sleeping for " ++ make_str(?SERVER_DELAY) ++ "ms before answering request..."),
      timer:sleep(?SERVER_DELAY),

      % Process client command
      % This will be either an allocate or deallocate command
      {NewFrequencies, Reply} =
        case Cmd of
          free_freqs         -> {Frequencies, {free_freqs, element(1, Frequencies)}};
          allocate           -> allocate(Frequencies, TS, ClientPid);
          {deallocate, Freq} -> deallocate(Frequencies, Freq);
          {inject, Freqs}    -> inject(Frequencies, Freqs)
        end,

      % All replies sent to the client are a three tuple containing:
      % - The atom 'reply'
      % - The time taken for the server to respond (in 𝛍s)
      % - The reply message, which itself could be either an atom or a two tuple
      ClientPid ! {reply, timer:now_diff(os:timestamp(), TS), Reply},
      freq_server:loop(NewFrequencies)
  
  % Periodically check for frequencies whose leases have expired then restart
  % the receive loop
  after ?LEASE_EXPIRY div 2 ->
    ?TRACE("Checking for expired leases"),
    freq_server:loop(expire_leases(Frequencies))

  end.
