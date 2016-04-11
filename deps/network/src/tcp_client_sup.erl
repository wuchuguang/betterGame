%%%-----------------------------------
%%% @Module  : tcp_client_sup
%%% @Created : 2015.05.05
%%% @Description: 客户端服务监控树
%%%-----------------------------------
-module(tcp_client_sup).
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
