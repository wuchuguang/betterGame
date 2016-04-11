%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 五月 2015 下午8:01
%%%-------------------------------------------------------------------
-module(meta_session).
-include("meta.hrl").
-author("root").

%% parse transform 'meta_session'

-export([parse_transform/2,funstring_get/1]).

parse_transform(Forms, _Options) ->
    case meta:new_from_forms(Forms) of
        {ok, Meta} ->
            Module = meta:get_modulename(Meta),
            {ok, MetaGet} = meta:add_func(Meta, funstring_get(Module)),
            {ok, MetaSet} = meta:add_func(MetaGet, funstring_set(Module)),
            {ok, MetaDel} = meta:add_func(MetaSet, funstring_del(Module)),
            MetaFinal = MetaDel,
            meta:to_forms(MetaFinal);
        Error ->
            io:format("parse_transform error-----------> ~p ~n",[Error]),
            Forms
    end.

funstring_get(Module) ->
    "get(Key, Default) -> com_util:dic_get({"++Module++",Key},Default).".

funstring_set(Module) ->
    "set(Key, Value) -> com_util:dic_set({"++Module++",Key},Value).".

funstring_del(Module) ->
    "del(Key) -> com_util:dic_erase({"++Module++",Key}).".