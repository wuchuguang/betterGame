%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     监听服务模块
%%% @end
%%% Created : 08. 九月 2015 上午9:52
%%%-------------------------------------------------------------------
-module(tcp_listener_server).
-include_lib("common/include/common.hrl").

%% API
-export([startup/2, bind_count/0, binds/0]).

%% @doc 启动监听树
startup(FatherPID, NetWork) ->
    boot:start_child(FatherPID, #boot{module = tcp_listener_sup:module(),type = ?boot_type_supervisor,params = NetWork}).

%% @doc 统计监听数据
bind_count() ->
    erlang:length(binds()).

%% @todo 获取当前Node绑定多少Socket
binds() ->
    [].