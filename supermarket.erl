-module(supermarket).
-export([start/0]).

% -----------------------------------------------------------------------------
% Types
% -----------------------------------------------------------------------------
-type sku()          :: pos_integer().
-type qty()          :: pos_integer().
-type description()  :: string().
-type unit_price()   :: pos_integer().
-type discount_amt() :: pos_integer().

-type stock_item()   :: {sku(), description(), unit_price()}.
-type stocklist()    :: [stock_item()].

-type shoppinglist() :: [sku()].

-type bill_item()    :: {sku(), qty(), description(), unit_price(), qty(), discount()}.
-type bill()         :: [bill_item()].

-type discount()     :: {sku(), qty(), discount_amt()}.
-type discounts()    :: [discount()].

% -----------------------------------------------------------------------------
% Function specs
% -----------------------------------------------------------------------------
-spec start() -> any().

-spec check_please(bill(),stocklist(),discounts(),shoppinglist()) -> any().

-spec print_bill(bill())         -> 'ok'.
-spec print_bill(bill(),float()) -> 'ok'.

-spec read_sku_item(stocklist(),sku())      -> stock_item().
-spec check_for_discount(discounts(),sku()) -> discount().
-spec update_qty(bill(),sku())              -> number().

% -----------------------------------------------------------------------------
% Macros
% -----------------------------------------------------------------------------
-define(UNKNOWN_SKU, {0,"Unknown SKU",0}).
-define(NO_DISCOUNT, {0,1,0}).
-define(PRINT_WIDTH, 30).

% -----------------------------------------------------------------------------
% Public functions
% -----------------------------------------------------------------------------
start() ->
  Barcodes  = [1234,4719,3814,1234,1112,1113,1234],
  Discounts = [{1234,2,100}],

  Stock_list = [
    {4719, "Fish Fingers", 121},
    {5643, "Nappies" , 1010},
    {3814, "Orange Jelly", 56},
    {1111, "Hula Hoops", 21},
    {1112, "Hula Hoops (Giant)", 133},
    {1234, "Dry Sherry, 1lt", 540}
  ],

  Bill = check_please([],Stock_list,Discounts,Barcodes),
  print_bill(Bill).

% -----------------------------------------------------------------------------
% Internal functions
% -----------------------------------------------------------------------------
check_please(Acc,_,_,[]) -> Acc;
check_please(Acc,KS,D,[BC|BCs]) ->
  {_, Desc, Unit_price}           = read_sku_item(KS,BC),
  {_, Discount_qty, Discount_amt} = check_for_discount(D,BC),
  New_qty                         = update_qty(Acc,BC),

  NewAcc = lists:keystore(BC,1,Acc,{BC, New_qty, Desc, Unit_price, Discount_qty, Discount_amt}),

  % Process next barcode
  check_please(NewAcc,KS,D,BCs).



% -----------------------------------------------------------------------------
% Print the bill
print_bill(Bill) ->
  io:format("~n~s~n~n",[string:centre("Erlang Stores",?PRINT_WIDTH)]),
  print_bill(Bill,0.0).

print_bill([],Total) ->
  Padding = ?PRINT_WIDTH - (if Total > 9.99 -> 1; true -> 0 end),
  io:format("~n~s£~.2f~n",[string:left("Total",Padding,$.),Total]);

print_bill([{_, _, "Unknown SKU", _, _, _}|Rest],Total) ->
  print_bill(Rest,Total);

print_bill([{_, Qty, Desc, Unit_price, Discount_qty, Discount_amt}|Rest],Total) ->
  Discount = (Qty div Discount_qty * (Discount_amt / 100)),
  Amt      = Qty * (Unit_price / 100) - Discount,
  Padding  = ?PRINT_WIDTH - (if Amt > 9.99 -> 5; true -> 4 end),

  io:format("~w x ~s£~.2f",[Qty,string:left(Desc,Padding,$.),Amt]),

  case Discount of
    0.0 -> io:format("~n");
    _   -> io:format(" (£~.2f off for buying ~w)~n",[Discount,Qty])
  end,

  print_bill(Rest,Total+Amt).



% -----------------------------------------------------------------------------
% Read SKU item from key store
read_sku_item(KS,BC) ->
  case lists:keysearch(BC,1,KS) of
    {value, V} -> V;
    false      -> ?UNKNOWN_SKU
  end.

% -----------------------------------------------------------------------------
% Check SKU item has a discount
check_for_discount(D,BC) ->
  case lists:keysearch(BC,1,D) of
    {value, V} -> V;
    false      -> ?NO_DISCOUNT
  end.

% -----------------------------------------------------------------------------
% Increment the quantity for an existing item on the bill
update_qty(Bill,BC) ->
  case lists:keysearch(BC,1,Bill) of
    {value,{_, Qty, _, _, _, _}} -> Qty+1;
    false                        -> 1
  end.


