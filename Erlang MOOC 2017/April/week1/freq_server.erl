% *****************************************************************************
% In order to see trace output, compile using c(freq_server,{d,debug}).
% *****************************************************************************
-module(freq_server).
-export([init/0]).

-define(FREQUENCY_LIST, [10,11,12,13,14,15]).
-define(SERVER_DELAY,   1000).

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
% Start the frequency server using predifined list of frequencies
init() -> loop({?FREQUENCY_LIST, []}).



% *****************************************************************************
% Private API
% *****************************************************************************

make_str(X) when is_tuple(X)   orelse
                 is_pid(X)     orelse
                 is_integer(X) orelse
                 is_atom(X)           -> io_lib:format("~w",[X]);
make_str(L) when is_list(L)           -> io_lib:format("~s",[L]).

% -----------------------------------------------------------------------------
% Handle frequency allocation
allocate({[], Allocated}, _ClientPid) ->
  {{[], Allocated}, {error, no_free_frequencies}};

allocate({[Freq|Rest] = Free, Allocated}, ClientPid) ->
  ?TRACE("Allocating next frequency"),

  % Check first if a frequency has already been allocated to the client
  case lists:keyfind(ClientPid, 2, Allocated) of
    false ->
      % Nope, so allocate frequency
      {{Rest, [{Freq, ClientPid}| Allocated]}, {alloc, Freq}};

    {_,ClientPid} ->
      % Yup. Client already has an allocated frequency
      {{Free, Allocated}, {error, frequency_already_allocated}}
  end.

% -----------------------------------------------------------------------------
% Handle frequency deallocation
deallocate({Free, Allocated}, Freq) ->
  ?TRACE("Deallocating frequency " ++ make_str(Freq)),

  % Check first that the frequency being deallocated belongs to the client 
  case lists:keyfind(Freq, 1, Allocated) of
    false ->
      % Can't deallocate a frequency that doesn't belong to you!
      {{Free, Allocated}, {error, not_your_frequency}};

    _ ->
      % Remove frequency from allocated list and inform client
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {{[Freq|Free], NewAllocated}, {dealloc, Freq}}
  end.

% -----------------------------------------------------------------------------
% Receive loop
loop(Frequencies) ->
  ?TRACE("Entering receive loop"),

  receive
    % Stop the server
    {request, ClientPid, TS, stop} ->
      ClientPid ! {reply, timer:now_diff(os:timestamp(), TS), stopped};

    {request, ClientPid, TS, Cmd} ->
      % Simulate server load
      ?TRACE("Sleeping for " ++ make_str(?SERVER_DELAY) ++ "ms before answering request..."),
      timer:sleep(?SERVER_DELAY),

      % Allocate or deallocate frequency to/from client
      case Cmd of
        allocate           -> {NewFrequencies, Reply} = allocate(Frequencies, ClientPid);
        {deallocate, Freq} -> {NewFrequencies, Reply} = deallocate(Frequencies, Freq)
      end,

      % All replies sent to the client are a three tuple containing:
      % - The atom 'reply'
      % - An integer representing the time taken for the server to respond (in 𝛍s)
      % - The reply message, which itself could be eithe an atom or a two tuple
      ClientPid ! {reply, timer:now_diff(os:timestamp(), TS), Reply},
      loop(NewFrequencies)
  end.
