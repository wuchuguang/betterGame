%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 五月 2015 下午2:50
%%%-------------------------------------------------------------------
-module(tcp_process_sup).
-author("root").
-include_lib("common/include/common.hrl").
-include_lib("network/include/network.hrl").

-export([start_child/2,get_name/1]).
-export([init/1]).

start_child(BootArgs,Socket)->
    boot:start_child(get_name(BootArgs),#boot{module = server_reader,
                                              type = simple_worker,params = {Socket,BootArgs}}).

get_name(#network_boot{port = Port})->
    boot:listname([?MODULE,Port]).

init(_BootArgs) ->
    [{server_reader,simple_worker}].