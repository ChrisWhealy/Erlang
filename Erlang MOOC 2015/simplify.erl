-module(simplify).
-compile(export_all).

expr2() -> {add,{mul,{num,1},{var,b}},{mul,{add,{mul,{num,2},{var,b}},{mul,{num,1},{var,b}}},{num,0}}}.

zeroAdd({add,E,{num,0}}) -> E;
zeroAdd({add,{num,0},E}) -> E;
zeroAdd(E)               -> E.

zeroMul({mul,_,{num,0}}) -> {num,0};
zeroMul({mul,{num,0},_}) -> {num,0};
zeroMul(E)               -> E.

mulOne({mul,E,{num,1}}) -> E;
mulOne({mul,{num,1},E}) -> E;
mulOne(E)               -> E.



