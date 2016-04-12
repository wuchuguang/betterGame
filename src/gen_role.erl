%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 四月 2016 上午11:42
%%%-------------------------------------------------------------------
-module(gen_role).
-author("root").

%% API
-export([callbacks/0]).

callbacks() ->
    [{id,0}].

id() ->
    0.

access() ->
    0.

trigger_() ->
    [].

extend_data() ->
    [].
