-module(week2_pedro). 
-export([index_file/2]). 
%% 
%% Notes: 
%% 1 - Uses external functions where2:nub, index:get_file_contents 
%% 2 - nocaps was copied from where3.erl 
%% 3 - In a real program, words to be ignored or normalised should be loaded from a file or database 
%% 4 - the resulting data is saved in "normal.txt" 
%% 5 - the list with all {word_not_ignored_and_normalised, list_of_pages} is saved in "result.txt"

%% Ignore and Normalise should be load from elsewhere 
%% in a more elaborate program 
-define(Ignore, ["a", "an", "as", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "the", "and", "that", "then"]). 
-define(Normalise, [ 
{"bottles", "bottle"}, 
{"boxes", "box"}, 
{"boys", "boy"}, 
{"bounds", "bound"} 
]).

get_file_contents(Name) ->
  {ok,File} = file:open(Name,[read]),
  Rev = get_all_lines(File,[]),
  lists:reverse(Rev).

% -----------------------------------------------------------------------------
% Auxiliary function for get_file_contents.
get_all_lines(File,Partial) ->
  case io:get_line(File,"") of
    eof  -> file:close(File),
            Partial;
    Line -> {Strip,_} = lists:split(length(Line)-1,Line),
            get_all_lines(File,[Strip|Partial])
  end.


nocaps([]) -> []; 
nocaps([X|Xs]) -> [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
  case $A =< X andalso X =< $Z of 
    true -> X+32;
    false -> X 
  end.

% entry function, Name = filename, MinWordLen = integer 
index_file(Name, MinWordLen) -> 
  Lines = get_file_contents(Name), 
  % index words starting with lineno 1 
  L = index_words(Lines, 1), 
  % here L is a List of list of {word, line}, ex. 
  % [[{"a",1}], [{"a",2}, ["b",2]] 
  Indexes = consolidate_indexes(lists:flatten(L)), 
  Sindexes = lists:sort(Indexes), 
  {ok, File} = file:open("result.txt", [write]), 
 
  lists:foreach(fun(X) -> io:format(File, "~p.~n", [X]) end, Sindexes), 
  file:close(File), 
 
  Sfiltered = lists:filter(fun({Word, _}) -> length(Word) >= MinWordLen end, Sindexes), 
  Snormal = normalise_pages(Sfiltered), 
  {ok, File2} = file:open("normal.txt", [write]), 
  lists:foreach(fun(X) -> io:format(File2, "~p.~n", [X]) end, Snormal), 
  file:close(File2).

% index all tokens in Lines starting with Lineno 
index_words(Lines, Start) -> index_words_acc(Lines, Start, []). 

% uses accumulator Acc 
index_words_acc([], _Lineno, Acc) -> lists:reverse(Acc); 

index_words_acc([Line | Lines], Lineno, Acc) -> 
  Words = split(Line), 
  % remove duplicates 
  NoDup = week2:nub(Words), 
  % indexes (creates list of {word, lineno}) 
  LineIndex = index_line_words(NoDup, Lineno), 
  % increment line, accumulate, recurse 
  index_words_acc(Lines, Lineno + 1, [LineIndex | Acc]).

% index a list of words 
% index_line_words(["a", "b"],1). should return 
% [{"a",1}, {"b", 1}] 
index_line_words([], _) -> []; 
index_line_words([W | Ws], Lineno) -> [{W, Lineno} | index_line_words(Ws, Lineno)].

% L list of {word, lineno} 
% [{"aaa", 1}, {"aaa", 2}] will be consolidated as 
% [{"aaa", [1,2]}] 
consolidate_indexes(L) -> consolidate_indexes_acc(L, []).

% uses accumulator 
consolidate_indexes_acc([], Acc) -> Acc; 
consolidate_indexes_acc([H | T], Acc) -> 
  {Word, Lineno} = H, 
  
  case lists:keyfind(Word, 1, Acc) of 
    {Word, List} -> 
      % append to the existing list 
      NewAcc = lists:keyreplace(Word, 1, Acc, {Word, List ++ [Lineno]}); 
    false -> 
      NewAcc = lists:keystore(Word, 1, Acc, {Word, [Lineno]}) 
    end, 

  % accumulate, recurse 
  consolidate_indexes_acc(T, NewAcc).

% change [{"aaa", [1, 2, 3, 5]}] to 
% [ { "aaa", [{1,3}, {5,5}] } ] 
normalise_pages(List) -> normalise_pages_acc(List, []).

normalise_pages_acc([], Acc) -> lists:reverse(Acc); 
normalise_pages_acc([H|T], Acc) -> 
  {Word, ListOfPages} = H, 
  ListOfTuples = normalise_page_list(ListOfPages), 
  normalise_pages_acc(T, [{Word, ListOfTuples} | Acc]).

% from list of integers 
% to list of tuples with intervals 
% L=[3, 4 , 5, 7, 11, 12, 13] will return 
% [{3,5},{7,7},{11,13}] 
normalise_page_list([H|T]) -> normalise_page_list(T, H, H). 
normalise_page_list([], Mark, Previous) -> [{Mark, Previous}]; 
normalise_page_list([H|T], Mark, Previous) -> 
  case H == Previous + 1 of % check if consecutive integer 
    true ->  normalise_page_list(T, Mark, H); % update previous 
    false -> [{Mark, Previous} | normalise_page_list(T, H, H)] 
  end.

% L : string 
% example: 
% split("Abc,. de f g,"). 
% ["abc","de","f", "g"] 
% after checking the words to be ignored 
% and normalised (replacing "boys" with "boy", for exemple) 
split(L) -> 
  Lall = split(nocaps(L), []), 
  Lvalid = valid_words(Lall), 
  Lkeep = remove_ignore(Lvalid), 
  Lnormalised = normalise_words(Lkeep), 
  Lnormalised.

%% split 
%% example: 
%% split("abc,. de f g,"). 
%% ["abc",[],[],"de","f","g",[]]

% Word holds chars 
split([], Word) -> [lists:reverse(Word)]; 
split([X | Xs], Word) -> 
  case lists:member(X,"-()!`[]<?/.,\ ;:\t\n\'\"") of 
    % add word to list 
    true  -> [lists:reverse(Word) | split(Xs, [])]; 

    % add char to word 
    false -> split(Xs, [X | Word]) 
  end.

%% throw away empty strings 
%% example: 
%% valid_words(["abc",[],[],"de","f","g",[]]). 
%% ["abc","de","f", "g"] 
valid_words([]) -> []; 
valid_words([X | Xs]) -> 
  case length(X) > 0 of 
    true ->  [X | valid_words(Xs)]; 
    false -> valid_words(Xs) 
  end.

% In a more elaborate program, Ignore should be loaded, for example, from a file 
% Here, we are using Ignore list define in line 13. 
remove_ignore(L) -> remove_ignore(L, ?Ignore).
remove_ignore([], _) -> []; 
remove_ignore([H|T], Lignore) -> 
  case lists:member(H, Lignore) of 
    true ->  remove_ignore(T, Lignore); 
    false -> [H | remove_ignore(T, Lignore)] 
  end.

% 
% In a more elaborate program, Normalise should be loaded, for example, from a file. 
% Here, we are using Normalise defined in line 14. 
normalise_words(L) -> normalise_words(L, ?Normalise).

normalise_words([], _) -> []; 
normalise_words([H|T], Lnormal) -> 
  case lists:keyfind(H, 1, Lnormal) of 
    {H, Normal} -> [Normal | normalise_words(T, Lnormal)]; 
    false       -> [H | normalise_words(T, Lnormal)] 
  end.

  