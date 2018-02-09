-module(lang_proc).
-compile(export_all).

%% ----------------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------------
-type expr() :: {'num', integer()}
              | {'var', atom()}
              | {'add', expr(), expr()}
              | {'sub', expr(), expr()}
              | {'dvd', expr(), expr()}
              | {'mul', expr(), expr()}.

-type env() :: [{atom(),integer()}].

-type instr() :: {'push', integer()}
               | {'fetch', atom()}
               | {'add2', expr(), expr()}
               | {'mul2', expr(), expr()}.

-type program() :: [instr()].
-type stack()   :: [integer()].

%% ----------------------------------------------------------------------------
%% Function specs
%% ----------------------------------------------------------------------------
-spec lookup(atom(),env()) -> integer(). 

-spec compile(expr())              -> program().
-spec run(program(),env(),stack()) -> integer().

-spec print(expr())        -> string().
-spec eval(env(), expr())  -> integer().

-spec parse(string()) -> {expr(), string()}.

-spec get_while(fun((T) -> boolean()),[T]) -> {[T],[T]}.

%% ----------------------------------------------------------------------------
%% Look up a value in the environment
%% ----------------------------------------------------------------------------
lookup(A,[{A,V}|_]) -> V;
lookup(A,[_|Rest])  -> lookup(A,Rest).


%% ----------------------------------------------------------------------------
%% Print an expression
%% ----------------------------------------------------------------------------
print({num,N}) -> integer_to_list(N);
print({var,A}) -> atom_to_list(A);

print({add,E1,E2}) -> print_int(E1,E2,"+");
print({sub,E1,E2}) -> print_int(E1,E2,"-");
print({dvd,E1,E2}) -> print_int(E1,E2,"/");
print({mul,E1,E2}) -> print_int(E1,E2,"*").

print_int(E1,E2,Op) -> "(" ++ print(E1) ++ Op ++ print(E2) ++ ")".

%% ----------------------------------------------------------------------------
%% Evaluate an expression
%% ----------------------------------------------------------------------------
eval(_Env, {num,N}) -> N;
eval(Env, {var,A})  -> lookup(A,Env);

eval(Env, {add,E1,E2}) -> eval(Env, E1) + eval(Env, E2);
eval(Env, {sub,E1,E2}) -> eval(Env, E1) - eval(Env, E2);
eval(Env, {dvd,E1,E2}) -> eval(Env, E1) / eval(Env, E2);
eval(Env, {mul,E1,E2}) -> eval(Env, E1) * eval(Env, E2).

%% ----------------------------------------------------------------------------
%% Run a compiled program
%% ----------------------------------------------------------------------------
run([{push, N}  | Continue], Env, Stack) -> run(Continue,Env, [N | Stack]);
run([{fetch, A} | Continue], Env, Stack) -> run(Continue, Env, [lookup(A,Env) | Stack]);

run([{add2} | Continue], Env, [N1,N2 | Stack]) -> run(Continue, Env, [(N1+N2) | Stack]);
run([{mul2} | Continue], Env, [N1,N2 | Stack]) -> run(Continue, Env, [(N1*N2) | Stack]);

run([], _Env, [N]) -> N.

%% ----------------------------------------------------------------------------
%% Compile an expression
%% ----------------------------------------------------------------------------
compile({num,N}) -> [{push,N}];
compile({var,A}) -> [{fetch,A}];

compile({add,E1,E2}) -> compile(E1) ++ compile(E2) ++ [{add2}];
compile({mul,E1,E2}) -> compile(E1) ++ compile(E2) ++ [{mul2}].

%% ----------------------------------------------------------------------------
%% Parse an expression string
%% ----------------------------------------------------------------------------
get_while(Has_property,[Chr|Rest]) ->
  case Has_property(Chr) of
    true ->
      {Succeeds,Remainder} = get_while(Has_property,Rest),
      {[Chr|Succeeds],Remainder};
    false ->
      {[],[Chr|Rest]}
  end;

get_while(_,[]) -> {[],[]}.

parse(ExprStr) ->
  {Result,_} = parse_int(ExprStr),
  Result.

parse_int([$( | Rest]) ->
  {E1,Rest1}   = parse_int(Rest),
  [Op | Rest2] = Rest1,
  {E2,Rest3}   = parse_int(Rest2),
  [$) | Rest4] = Rest3,

  {case Op of
     $+ -> {add,E1,E2};
     $* -> {mul,E1,E2}
   end,
   Rest4};

parse_int([Chr|Rest]) when $a =< Chr andalso Chr =< $z ->
  {Succeeds,Remainder} = get_while(fun is_alpha/1, Rest),
  {{var, list_to_atom([Chr|Succeeds])}, Remainder};

parse_int([Chr|Rest]) when $0 =< Chr andalso Chr =< $9 ->
  {Succeeds,Remainder} = get_while(fun is_numeric/1, Rest),
  {{num, list_to_integer([Chr|Succeeds])}, Remainder}.

is_alpha(Chr)   -> $a =< Chr andalso Chr =< $z.
is_numeric(Chr) -> $0 =< hd(Chr) andalso hd(Chr) =< $9.

