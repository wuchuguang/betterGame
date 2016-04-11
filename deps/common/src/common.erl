%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 五月 2015 下午12:28
%%%-------------------------------------------------------------------
-module(common).
-include_lib("common/include/common.hrl").
-export([get_value/3, tuple2kvs/1, lists2kvs/1,lists2kvs/2, while/3]).
-compile(export_all).
%% API


get_value(Fun, [{Key,Value}|List], Default) when is_function(Fun,1)->
    case Fun(Key) of
        true -> Value;
        {true,NewVal} -> NewVal;
        false->
            get_value(Fun,List,Default)
    end;
get_value(Fun, [{Key,Value}|List], Default) when is_function(Fun,2)->
    case Fun(Key, Value) of
        true -> Value;
        {true,NewVal} -> NewVal;
        false->
            get_value(Fun,List,Default)
    end;
get_value(Key, [{Key,Value}|_List], _Default)->
    Value;
get_value(Key, [_KV|List], Default) ->
    get_value(Key, List, Default);
get_value(Key, [], Default) ->
    default_value(Key, Default);
get_value(Key,NotList, Default) ->
    ?LOG_ERROR("get_value(~p, ~p, ~p) stacktrace ~p",[Key, NotList, Default,erlang:get_stacktrace()]),
    exit({get_value, Key, NotList, Default}).

default_value(_Key, Default) when is_function(Default,0)->
    Default();
default_value(Key, Default) when is_function(Default, 1) ->
    Default(Key);
default_value(_Key, Default) ->
    Default.

tuple2kvs(Tuple) when is_tuple(Tuple) ->
    lists2kvs(tuple_to_list(Tuple)).
lists2kvs(Lists) when is_list(Lists) ->
    lists2kvs__(Lists,[]).
lists2kvs(Fun, Lists) ->
    lists:map(Fun, lists2kvs(Lists)).

lists2kvs__([K,V|Lists], Ret) ->
    lists2kvs__(Lists, [{K,V}|Ret]);
lists2kvs__([], Ret) ->
    lists:reverse(Ret).



while(Fun, [Elem|List], Default) when is_function(Fun,1)->
    case Fun(Elem) of
        ?continue ->
            while(Fun, List, Default);
        ?continue(Value) ->
            while(Fun,List,Value);
        ?break ->
            Elem;
        ?break(Value) ->
            Value
    end;
while(_Fun, [], Default) ->
    Default.




test_time() ->
    ?amc(com_time:unixtime()).

test_amc() ->
    ?amc(ddd:ff(),[{undef,"undef fasdfasf"}]).

test_port(Port) ->
    TCP_OPTIONS = [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true},{delay_send,true}, {send_timeout, 5000}, {keepalive, false}, {backlog, 64}, {exit_on_close, true}],
    gen_tcp:listen(Port, TCP_OPTIONS).