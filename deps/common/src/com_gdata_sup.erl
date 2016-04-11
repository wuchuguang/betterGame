%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十一月 2015 下午4:32
%%%-------------------------------------------------------------------
-module(com_gdata_sup).
-include("common.hrl").
-export([startup/1, start_child/1, init/1, get_name/1]).

startup(FatherPID) ->
    boot:start_child(FatherPID, #boot{module = ?MODULE, type = ?boot_type_supervisor}).

start_child(Boot) ->
    boot:start_child(?MODULE, Boot).

init(_) ->
    [].

get_name(_) ->
    ?MODULE.