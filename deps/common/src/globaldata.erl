%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 六月 2015 上午11:34
%%%-------------------------------------------------------------------
-module(globaldata).
-include_lib("common/include/common.hrl").

-record(globaldata, {key, value}).
%% API
-export([start/1,init/1,get_name/1,handle_cast/2, handle_call/3]).

-export([get_value/2, set_value/2, set_value_syn/2]).

start(Fun) ->
    boot:start(#boot{module = ?MODULE,params = Fun,type = ?boot_type_worker}).

init(Fun) ->
    ?LAGER_DEBUG("init"),
    ets:new(?MODULE, [set, public, named_table, {keypos, #globaldata.key}]),
    Fun(),
    {ok,state}.

get_name(_) ->
    ?MODULE.

call(MSG) ->
    gen_server:call(?MODULE, MSG).

cast(MSG) ->
    gen_server:cast(?MODULE, MSG).


get_value(Key, Default) ->
    case ets:lookup(?MODULE, Key) of
        [] ->
            Default;
        [#globaldata{value=Value}] -> Value
    end.

set_value(Key, Fun) ->
    cast({set_value, Key, Fun}).

set_value_syn(Key, Fun) ->
    call({set_value, Key, Fun}).


handle_cast({set_value, Key, Fun}, State) ->
    OldValue = get_value(Key, undefined),
    NewValue = ?catch_exp(Fun(OldValue)),
    set_value_(Key, NewValue),
    {noreply, State}.

handle_call({set_value, Key, Fun}, _, State) ->
    OldValue = get_value(Key, undefined),
    NewValue = ?catch_exp(Fun(OldValue)),
    set_value_(Key, NewValue),
    {reply, NewValue, State}.


set_value_(Key, NewValue) ->
    ets:insert(?MODULE, #globaldata{key = Key, value = NewValue}).