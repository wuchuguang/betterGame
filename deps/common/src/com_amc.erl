%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 六月 2015 上午11:48
%%%-------------------------------------------------------------------
-module(com_amc).
-include("common.hrl").
%% API
-export([exe/2]).

exe(Do, Args) ->
    exe__(Do, Args).

exe__(Fun, _Args) when is_function(Fun,0) ->Fun();
exe__(Fun, Args) when is_function(Fun,1) ->Fun(Args);
exe__({M,F,A},Args) when is_atom(M),is_atom(F) ->
    apply(M,F,A++Args).
