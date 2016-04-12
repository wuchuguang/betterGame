%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 四月 2016 上午11:48
%%%-------------------------------------------------------------------
-module(gen_plugin).
-author("root").

%% API
-export([type/0, mgr/0]).


%% @doc 插件类型
type() ->
    0.

%% @doc 插件管理器，
mgr() ->
    plugin_mgr:sss().

%% @doc
start() ->
    ok.

%% @doc
restart() ->
    ok.

call() ->
    ok.

cast() ->
    ok.

info() ->
    ok.

%% @doc
stop() ->
    ok.



