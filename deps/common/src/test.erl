%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十一月 2015 上午11:24
%%%-------------------------------------------------------------------
-module(test).
-author("root").
-include_lib("merl/include/merl.hrl").

-export([test/0]).

test() ->
    Tuple = ?Q("{foo, 42}"),
    "{foo, _@Number}" = Tuple,
    Call = "foo:bar(_@Number)".
