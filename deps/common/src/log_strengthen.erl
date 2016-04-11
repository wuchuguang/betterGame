%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 十一月 2015 上午11:22
%%%-------------------------------------------------------------------
-module(log_strengthen).
-author("root").

%% API
-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
%%     io:format("forms ~p ~n", [Forms]),
    Forms.