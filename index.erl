-module(index).
-export([start/0, start/1, show_keystore/1]).
  
% -----------------------------------------------------------------------------
% Create word index by line
% -----------------------------------------------------------------------------

% Error messages must not only be correct, but they must also be helpful and
% move the user towards the solution.
% Therefore, start/0 was created for no other reason than to print a usage
% message.  This is better than the correct, but unhelpful message:
% "undefined function index:start/0"
start() -> io:format("Usage: index:start(<file_name>)~n").
start(F) ->
  % Strip out extraneous words and characters from the text to obtain a clean
  % list of words
  BookWords = lists:map(fun words_per_line/1, get_file_contents(F)),

  % Create a keystore using each word as the key followed by an array of the
  % line numbers on which that word occurs
  KS = words_on_this_line([],1,BookWords),

  % For each keystore entry, first optimise the line number list, then sort the
  % resulting list into lexicographical order.
  sort_keystore(lists:keymap(fun opt_lineno_list/1, 2, KS)).

% -----------------------------------------------------------------------------
% File handling
% -----------------------------------------------------------------------------
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

% -----------------------------------------------------------------------------
% File handling
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
% Record which words occur on each line of the file
words_on_this_line(Acc, _, []) -> Acc;
words_on_this_line(Acc, LineNo, [Line|Lines]) ->
  NewAcc = keystore_from_words_on_line(Acc,LineNo,Line),
  words_on_this_line(NewAcc, LineNo+1, Lines).

% -----------------------------------------------------------------------------
% Add each word to the keystore
keystore_from_words_on_line(Acc, _, []) -> Acc;
keystore_from_words_on_line(Acc, LineNo, [Word|Words]) ->
  Freqs = case lists:keysearch(Word,1,Acc) of
    {value,{_,F}} -> [LineNo | F];
    false         -> [LineNo]
  end,

  NewAcc = lists:keystore(Word,1,Acc,{Word,Freqs}),
  keystore_from_words_on_line(NewAcc, LineNo, Words).
  
% -----------------------------------------------------------------------------
% Divide a single line into words.  This function also:
% 1) Filters out punctuation and other extraneous characters
% 2) Converts all capital letters to lower case
% 3) Filters out the normal English stop words
words_per_line([])  -> [];
words_per_line(Line) ->
  Naughty_Chars = " .,;:?!-()`'\"\\",
  % Filter out the naughty characters and then convert each word to lowercase
  Wurdz = lists:map(fun string:to_lower/1, string:tokens(Line,Naughty_Chars)),
  lists:filter(fun drop_stop_words/1, Wurdz).

% -----------------------------------------------------------------------------
% Using the quicksort algorithm, sort the keystore into lexicographical order
sort_keystore([]) -> [];
sort_keystore([{W,_}=Item | Words]) ->
  sort_keystore(lists:filter(fun({K,_}) -> K < W end, Words)) ++
  [Item] ++
  sort_keystore(lists:filter(fun({K,_}) -> K >= W end, Words)).


% -----------------------------------------------------------------------------
% Optimise the line number list
opt_lineno_list([])          -> [];
opt_lineno_list([_|_]=Lines) ->
  Rev = lists:reverse(Lines),
  lists:reverse(opt_lineno_list([],Rev,hd(Rev),hd(Rev))).

% Line number list is either empty or contains only a single entry
opt_lineno_list(Acc,L,First,Last) when length(L) < 2 ->
  [{First,Last} | Acc];

% Adjacent lines numbers are equal
opt_lineno_list(Acc,[Freq,Freq|Lines],_First,Last) ->
  opt_lineno_list(Acc, Lines, Freq, Last);

% Adjacent lines numbers are sequential
opt_lineno_list(Acc,[Freq,Next|_]=Lines,First,_Last) when Next == Freq+1 ->
  opt_lineno_list(Acc, tl(Lines), First, Next);

% Adjacent lines numbers are not sequential
opt_lineno_list(Acc,[_Freq,Next|_]=Lines,First,Last) ->
  opt_lineno_list([{First,Last} | Acc], tl(Lines), Next, Next).


% -----------------------------------------------------------------------------
% Remove common words
drop_stop_words(W) ->
  not lists:member(W, ["a","about","above","after","again","against","all","am",
    "an","and","any","are","aren\'t","as","at","be","because","been","before",
    "being","below","between","both","but","by","can\'t","cannot","could",
    "couldn\'t","did","didn\'t","do","does","doesn\'t","doing","don\'t","down",
    "during","each","few","for","from","further","had","hadn\'t","has","hasn\'t",
    "have","haven\'t","having","he","he\'d","he\'ll","he\'s","her","here","here\'s",
    "hers","herself","him","himself","his","how","how\'s","i","i\'d","i\'ll","i\'m",
    "i\'ve","if","in","into","is","isn\'t","it","it\'s","its","itself","let\'s","me",
    "more","most","mustn\'t","my","myself","no","nor","not","of","off","on","once",
    "only","or","other","ought","our","ours","ourselves","out","over","own","same",
    "shan't","she","she\'d","she\'ll","she\'s","should","shouldn\'t","so","some",
    "such","than","that","that\'s","the","their","theirs","them","themselves",
    "then","there","there\'s","these","they","they\'d","they\'ll","they\'re",
    "they\'ve","this","those","through","to","too","under","until","up","very",
    "was","wasn\'t","we","we\'d","we\'ll","we\'re","we\'ve","were","weren\'t",
    "what","what\'s","when","when\'s","where","where\'s","which","while","who",
    "who\'s","whom","why","why\'s","with","won\'t","would","wouldn\'t","you",
    "you\'d","you\'ll","you\'re","you\'ve","your","yours","yourself","yourselves"]).


% -----------------------------------------------------------------------------
% Utility function to dump the keystore to the console
% -----------------------------------------------------------------------------
show_keystore([]) -> ok;
show_keystore([{Key,Freqs}|Items]) ->
  io:format("Key = ~s, Frequencies = ~w~n",[Key,Freqs]),
  show_keystore(Items).
