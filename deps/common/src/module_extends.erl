%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十二月 2015 上午11:33
%%%-------------------------------------------------------------------
-module(module_extends).
-author("root").

%% API
-export([parse_transform/2]).

parse_transform(Forms, Opts) ->
    {ok, Meta} = meta:new_from_forms(Forms),
    Extends = meta:get_attribute(Meta, extends),
    io:format("forms ~p~n", [Forms]),
    Forms.
