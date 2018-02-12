-module(populate).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 0.1").
-created("Date: 2018/02/08 11:15:07").
-created_by("chris.whealy@sap.com").

-export([start/0, start/1]).
-export([http_get_request/3]).
-export([handle_country_batch/2, handle_zip_file/3]).

-include("utils/trace.hrl").

-define(GEONAMES_URL, "http://download.geonames.org/export/dump/").
-define(TARGET_DIR,   "geonames_files/").

-define(FEATURECODE_FILE(Lang), {"featureCodes_" ++ Lang, ".txt"}).
-define(TIMEZONE_FILE,          {"timeZones",             ".txt"}).
-define(HIERARCHY_FILE,         {"hierarchy",             ".zip"}).
-define(COUNTRIES_INFO_FILE,    {"countryInfo",           ".txt"}).

-define(PAR_REQ_LIM, 15).


%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------
start() -> start("en").

start(Lang) ->
  inets:start(),

  TxtFileList = [?FEATURECODE_FILE(Lang), ?TIMEZONE_FILE, ?COUNTRIES_INFO_FILE],
  Parent   = self(),

  %% Download each plain file in the file list, then write it to disc
  lists:foreach(fun({F,Ext}) -> spawn(?MODULE, http_get_request, [Parent, F, Ext]) end, TxtFileList),
  wait_for_text_resources(length(TxtFileList)),

  %% Get the Hierarchy ZIP file
  spawn(?MODULE, http_get_request, [Parent, "hierarchy", ".zip"]),
  wait_for_zip_resources(1),

  %% For each country code listed in countryInfo.txt, do the following:
  %%  * Download the corresponding ZIP file
  %%  * Unzip only the data file (ignore the readme file)
  %%  * Write the data file to disc
  %%  * Delete the ZIP file
  %%
  %% The HTTP requests must be batched into sequential groups to avoid looking
  %% like an DoS attack.  The PAR_REQ_LIM macro defines the limit for the number
  %% of parallel HTTP requests
  Countries      = parse_countries_file(),
  BatchSize      = bump(length(Countries) / ?PAR_REQ_LIM),
  CountryBatches = chop(Countries, BatchSize),

  fetch_countries(Parent, CountryBatches),
  wait_for_zip_resources(length(Countries)).



%% -----------------------------------------------------------------------------
%%                            P R I V A T E   A P I
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Wait for a text file to be returned by HTTP request
wait_for_text_resources(0) -> done;

wait_for_text_resources(Count) ->
%  ?TRACE("Waiting for ~w resources",[Count]),

  receive
    {ok, Filename, Ext, Body} ->
      io:format("Received ~s~n",[Filename ++ Ext]),
      file:write_file(?TARGET_DIR ++ Filename ++ Ext, Body),
      wait_for_text_resources(Count-1);

    {error, StatusCode, Reason, Filename} -> io:format("HTTP ~w \"~s\": ~s~n", [StatusCode, Reason, ?GEONAMES_URL ++ Filename]);
    {error, Reason, To, Addr, Port}       -> io:format("Error: ~w ~w ~s on port ~w~n", [Reason, To, Addr, Port]);
    {error, Reason}                       -> io:format("Error: ~w~n", [Reason])
  end.

%% -----------------------------------------------------------------------------
%% Wait for a ZIP file to be returned by HTTP request
wait_for_zip_resources(0) -> done;

wait_for_zip_resources(Count) ->
  ?TRACE("Waiting for ~w resources",[Count]),

  receive
    {ok, Filename, Ext, Body} ->
      io:format("Received ~s~n",[Filename ++ Ext]),
      spawn(?MODULE, handle_zip_file, [?TARGET_DIR, Filename, Body]),
      wait_for_zip_resources(Count-1);

    {error, StatusCode, Reason, Filename} -> io:format("HTTP ~w \"~s\": ~s~n", [StatusCode, Reason, ?GEONAMES_URL ++ Filename]);
    {error, Reason, To, Addr, Port}       -> io:format("Error: ~w ~w ~s on port ~w~n", [Reason, To, Addr, Port]);
    {error, Reason}                       -> io:format("Error: ~w~n", [Reason])
  end.


%% -----------------------------------------------------------------------------
%% Start a process to fetch each batch of country codes
fetch_countries(_, []) -> done;
fetch_countries(Parent, [CountryBatch | Rest]) ->
  spawn(?MODULE, handle_country_batch, [Parent, CountryBatch]),
  fetch_countries(Parent, Rest).



%% -----------------------------------------------------------------------------
%% Fetch a list of country code files sequentially
handle_country_batch(_, []) -> finished_country_batch;

handle_country_batch(Parent, [CountryCode | Rest]) ->
  http_get_request(Parent, CountryCode, ".zip"),
  handle_country_batch(Parent, Rest).


%% -----------------------------------------------------------------------------  
%% Unzip only the data file from a zipped country file, then throw away the ZIP
%% file
handle_zip_file(Dir, File, Body) ->
  ZipFile = Dir ++ File ++ ".zip",
  TxtFile = Dir ++ File ++ ".txt",

  file:write_file(ZipFile, Body),

  ?TRACE("Unzipping ~s",[ZipFile]),
  {ok, [TxtFile]} = zip:unzip(ZipFile, [{file_list, [File ++ ".txt"]}, {cwd, Dir}]),

  ok = file:delete(ZipFile).



%% -----------------------------------------------------------------------------  
http_get_request(CallerPid, Filename, Extension) ->
  Url = ?GEONAMES_URL ++ Filename ++ Extension,
%  ?TRACE("~s", [Url]),

  Response = httpc:request(get, {Url, []}, [], []),
    
  CallerPid ! case Response of
    {ok, {{_, 200, "OK"}, _Hdrs, Body}}             -> {ok, Filename, Extension, Body};
    {ok, {{_, StatusCode, Reason}, _Hdrs, _Body}}   -> {error, StatusCode, Reason, ?GEONAMES_URL ++ Filename};
    {error, {Reason, [{To, {Addr, Port}} | _Rest]}} -> {error, Reason, To, Addr, Port};
    {error, Reason}                                 -> {error, Reason}
  end.
    

%% -----------------------------------------------------------------------------
%% Read the countryInfo.txt file and fetch each individual country file
parse_countries_file() ->
  ?TRACE(""),

  % Generate a list of country codes
  {Filename, Ext} = ?COUNTRIES_INFO_FILE,

  case file:open(?TARGET_DIR ++ Filename ++ Ext, [read]) of
    {ok, IoDevice}  -> read_countries_file(IoDevice, []);
    {error, Reason} ->
      io:format("Unable to read ~s.~s. ~s~n",[Filename, Ext, Reason]),
      []
  end.

%% -----------------------------------------------------------------------------
%% Read a text file skipping any lines that start with a hash character
read_countries_file(IoDevice, L) ->
  case io:get_line(IoDevice,"") of
    eof ->
      file:close(IoDevice),
      L;

    DataLine ->
      LineTokens = string:tokens(DataLine,"\t"),
      [[Char1 | _] | _] = LineTokens,
      
      read_countries_file(IoDevice, get_country_code(Char1, LineTokens, L))
  end.

get_country_code($#, _,                     L) -> L;
get_country_code(_,  [CountryCode | _Rest], L) -> L ++ [CountryCode].


%% -----------------------------------------------------------------------------
%% Bump a float up to the next integer unless it is n.0
bump(F) ->
  I = trunc(F),

  case F == I of
    true  -> I;
    false -> I + 1
  end.

%% -----------------------------------------------------------------------------
%% Chop up a list into sublists of a given size
chop(L, BatchSize) ->
  % Avoid crashing on something as trivial as trying to take N items from a list
  % of length M where N > M
  case BatchSize >= length(L) of
    true  -> [L];
    false ->
      {H,T} = lists:split(BatchSize, L),
      chop(T, BatchSize, length(T), [H])
  end.

chop([], _, _, Acc) -> Acc;

chop(L, BatchSize, N, Acc) when BatchSize =< N ->
  {H,T} = lists:split(BatchSize, L),
  chop(T, BatchSize, length(T), Acc ++ [H]);

chop(L, _, _, Acc) -> Acc ++ [L].


