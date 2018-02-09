-module(frequency_detlef). 
-export([start/0, init/0, allocate/0, deallocate/1, stop/0]).

%% Client-side code

start() -> register(?MODULE, spawn(?MODULE, init, [])).

allocate() -> 
  clear(), 
  ?MODULE ! {request, self(), allocate}, 
  reply().

deallocate(Freq) -> 
  clear(), 
  ?MODULE ! {request, self(), {deallocate, Freq}}, 
  reply().

stop() -> 
  clear(), 
  ?MODULE ! {request, self(), stop}, 
  reply().

reply() -> 
  receive 
    {reply, Msg} -> Msg; 
    Any          ->  {any, Any} 
 
  after 10000 -> none 
  end.

clear() -> 
  receive 
    _Msg -> clear() 

  after 0 -> ok 
  end.

%% These are the start functions used to create and 
%% initialize the server.
init() -> 
  Frequencies = {get_frequencies(), []}, 
  loop(Frequencies).

% Hard Coded 
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) -> 
receive 
  {request, Pid, allocate} -> 
    {NewFrequencies, Reply} = allocate(Frequencies, Pid), 
    Pid ! {reply, Reply}, 
    loop(NewFrequencies); 

  {request, Pid , {deallocate, Freq}} -> 
    {NewFrequencies, Result} = deallocate(Frequencies, Freq), 
    Pid ! {reply, Result}, 
    loop(NewFrequencies); 

  {request, Pid, stop} -> 
    Pid ! {reply, stopped} 
  end.

%% The Internal Help Functions used to allocate and 
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) -> 
  {{[], Allocated}, {error, no_frequency}}; 

allocate({[Freq|Free], Allocated}, Pid) -> 
  case lists:keymember(Pid, 2, Allocated) of 
    true  -> {{[Freq|Free], Allocated}, {error, dup_frequency}}; 
    false -> {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}} 
  end.

deallocate({Free, Allocated}, Freq) -> 
  case lists:keymember(Freq, 1, Allocated) of 
    true  -> NewAllocated=lists:keydelete(Freq, 1, Allocated), 
             {{[Freq|Free], NewAllocated}, ok}; 
    false -> {{Free, Allocated}, nok} 
  end.

