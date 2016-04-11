%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十一月 2015 下午1:47
%%%-------------------------------------------------------------------
-module(udp_listener_server).


%% API
-export([startup/2]).

startup(FatherPID, Boot) ->
    udp_listener_sup:startup(FatherPID, Boot),
    udp_listener:startup(),
    ok.
