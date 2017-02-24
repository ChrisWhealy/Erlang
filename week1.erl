-module(week1).
-export([perimeter/1, area/1, enclose/1, bits_tail/1, bits_dir/1]).

%% ----------------------------------------------------------------------------
%% Type definitions
%% ----------------------------------------------------------------------------
-type radius() :: float().
-type side()   :: float().

-type shape()  :: {'circle',    radius()}
                | {'square',    side()}
                | {'rectangle', side(), side()}
                | {'triangle',  side(), side(), side()}.

%% ----------------------------------------------------------------------------
%% Function specs
%% ----------------------------------------------------------------------------
-spec perimeter(shape()) -> float().
-spec area(shape())      -> float().
-spec enclose(shape())   -> shape().

-spec is_triangular(side(),side(),side()) -> boolean();
                   (side(),side(),side()) -> no_return().

%% ----------------------------------------------------------------------------
%% Geometric functions
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% Test whether three sides of the given lengths can form a triangle
%
% If we don't check whether the three sides can form a triangle, then functions
% such as area/1 will blow up with the somewhat obscure message that it can't
% evaluate an arithmetic expression (due to sqrt of a negative number).
% Therefore, to make the root cause of the problem easier to debug, we will
% raise a more meaningful error message.
is_triangular(A,B,C) ->
  [Short,Med,Long] = lists:sort([A,B,C]),

  case Long =< Short + Med of
    true  -> true;
    false -> error('sides_dont_form_a_triangle')
  end.

%% ----------------------------------------------------------------------------
% Total edge length of a shape
perimeter({circle,R})       -> 2*math:pi()*R;
perimeter({rectangle,H,W})  -> 2*H + 2*W;
perimeter({square,S})       -> perimeter({rectangle,S,S});
perimeter({triangle,A,B,C}) -> 
  case is_triangular(A,B,C) of
    _ -> A+B+C
  end.

%% ----------------------------------------------------------------------------
% Area of a shape
area({circle,R})           -> math:pi()*R*R;
area({rectangle,H,W})      -> H*W;
area({square,S})           -> area({rectangle,S,S});
area({triangle,A,B,C} = T) ->
  S = perimeter(T)/2,
  math:sqrt(S*(S-A)*(S-B)*(S-C)).

%% ----------------------------------------------------------------------------
% Enclose a shape in the smallest rectangle
enclose({circle,R})           -> {square,2*R};
enclose({rectangle,H,W})      -> {rectangle,H,W};
enclose({square,S})           -> {square,S};
enclose({triangle,A,B,C} = T) ->
  % Use the longest side as the base
  [_,_,Long] = lists:sort([A,B,C]),
  Height = 2 * area(T) / Long,
  {rectangle,Height,Long}.



%% ----------------------------------------------------------------------------
%% Count the bits
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
% Tail recursive solution
bits_tail(0)            -> 0;
bits_tail(N) when N > 0 -> bits_tail(0,N).

bits_tail(Acc,0) -> Acc;
bits_tail(Acc,N) -> bits_tail(Acc + N rem 2, N div 2).

%% ----------------------------------------------------------------------------
% Direct recursion solution
bits_dir(0)            -> 0;
bits_dir(N) when N > 0 -> N rem 2 + bits_dir(N div 2).


