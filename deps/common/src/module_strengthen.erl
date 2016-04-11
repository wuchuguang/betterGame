%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 十一月 2015 下午2:00
%%%-------------------------------------------------------------------
-module(module_strengthen).
-include("meta.hrl").

%% API
-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
    NewForms = include_module(Forms),
    {ok, Meta} = meta:new_from_forms(NewForms),
    ModuleName = meta:get_modulename(Meta),
    {ok, Meta1} = meta:add_func(Meta, "module() ->'"++ModuleName++"'.", true),
    meta:to_forms(Meta1).

include_module(Forms) ->
%%     {ok, Meta} = meta:new_from_forms(Forms),
    Forms.