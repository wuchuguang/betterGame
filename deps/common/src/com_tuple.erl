%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 六月 2015 下午4:06
%%%-------------------------------------------------------------------
-module(com_tuple).
-author("root").

%% API
-export([append/2, foreach/2]).

append(Val, Tuple) ->
    setelement(tuple_size(Tuple) + 1, Tuple, Val).

foreach(Fun, Tuple) ->
    lists:foreach(Fun, tuple_to_list(Tuple)).

