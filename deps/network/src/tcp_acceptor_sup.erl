%%%-----------------------------------
%%% @Module  : tcp_acceptor_sup
%%% @Created : 2015.05.05
%%% @Description: tcp acceptor 监控树
%%%-----------------------------------
-module(tcp_acceptor_sup).
-include_lib("network/include/network.hrl").
-include_lib("common/include/common.hrl").
-export([init/1,get_name/1]).


-export([start_child/2]).

get_name(#network_boot{port = Port})->
    boot:listname([?MODULE,Port]).

start_child(LSock,BootArgs)->
    boot:start_child(get_name(BootArgs),#boot{hasname = ?false,module = tcp_acceptor,type = ?boot_type_simple_worker,params = {LSock,BootArgs}}).

init(_BootArgs) ->
    [{tcp_acceptor,simple_worker}].
