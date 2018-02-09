-module(populate).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 0.1").
-created("Date: 2018/02/08 11:15:07").
-created_by("chris.whealy@sap.com").

-export([start/0, start/1]).
-export([http_get_request/2]).

-include("utils/trace.hrl").

-define(GEONAMES_URL, "http://download.geonames.org/export/dump/").
-define(TARGET_DIR,   "geonames_files/").

-define(FEATURECODE_FILE(Lang), "featureCodes_" ++ Lang ++ ".txt").
-define(TIMEZONE_FILE,          "timeZones.txt").
-define(HIERARCHY_FILE,         "hierarchy.zip").
-define(COUNTRIES_INFO_FILE,    "countryInfo.txt").


%% -----------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% -----------------------------------------------------------------------------
start() -> start("en").

start(Lang) ->
  inets:start(),

  FileList = [?FEATURECODE_FILE(Lang), ?TIMEZONE_FILE, ?HIERARCHY_FILE, ?COUNTRIES_INFO_FILE],
  Parent   = self(),

  %% Download each file in the file list, then write it to disc
  lists:foreach(fun(F) -> spawn_link(fun() -> http_get_request(Parent, F) end) end, FileList),
  wait_for_resources(length(FileList)),

  %% Generate a list of country codes from countryInfo.txt and then download the
  %% corresponding ZIP file and write each to disc
  Countries = parse_countries_file(),
  lists:foreach(fun(C) -> spawn_link(fun() -> http_get_request(Parent, C ++ ".zip") end) end, Countries),
  wait_for_resources(length(Countries)).



%% -----------------------------------------------------------------------------
%%                            P R I V A T E   A P I
%% -----------------------------------------------------------------------------

wait_for_resources(0) -> done;

wait_for_resources(Count) ->
  ?TRACE("Waiting for ~w resources",[Count]),

  receive
    {ok, Filename, Body} ->
      file:write_file(?TARGET_DIR ++ Filename, Body),
      wait_for_resources(Count-1);

    {error, StatusCode, Reason, ?GEONAMES_URL ++ Filename} ->
      io:format("HTTP ~w \"~s\": ~s~n", [StatusCode, Reason, Filename]);
    
    {error, Reason, To, Addr, Port} ->
      io:format("Error: ~w ~w ~s on port ~w~n", [Reason, To, Addr, Port])
  end.

%% -----------------------------------------------------------------------------  
http_get_request(CallerPid, Filename) ->
  Url = ?GEONAMES_URL ++ Filename,
  ?TRACE("~s", [Url]),

  Response = httpc:request(get, {Url, []}, [], []),
    
  CallerPid ! case Response of
    {ok, {{_, 200, "OK"}, _Hdrs, Body}}             -> {ok, Filename, Body};
    {ok, {{_, StatusCode, Reason}, _Hdrs, _Body}}   -> {error, StatusCode, Reason, ?GEONAMES_URL ++ Filename};
    {error, {Reason, [{To, {Addr, Port}} | _Rest]}} -> {error, Reason, To, Addr, Port}
  end.
    

%% -----------------------------------------------------------------------------
%% Read the countryInfo.txt file and fetch each individual country file
parse_countries_file() ->
  ?TRACE(""),

  case file:open(?TARGET_DIR ++ ?COUNTRIES_INFO_FILE, [read]) of
    {ok, IoDevice}  -> L = read_countries_file(IoDevice, []);
    {error, Reason} ->
      io:format("Unable to read ~s. ~s~n",[?COUNTRIES_INFO_FILE, Reason]),
      L = []
  end,

  L.

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

