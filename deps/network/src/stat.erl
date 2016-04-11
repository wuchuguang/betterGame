%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 五月 2015 下午2:32
%%%-------------------------------------------------------------------
-module(stat).
-author("root").

%% API
-export([env/0,debug/1]).

env() ->
    erlang:system_info().


debug(Fun) ->
    eprof:start(),
    eprof:profile(Fun),
    eprof:stop(),
    ok.