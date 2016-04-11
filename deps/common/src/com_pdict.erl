%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     进程字典操作
%%% @end
%%% Created : 11. 六月 2015 上午10:18
%%%-------------------------------------------------------------------
-module(com_pdict).
-author("root").

%% API
-export([get/2, set/2, del/1]).

get(Key, Default) ->
    case erlang:get(Key) of
        undefined->Default;
        Val -> Val
    end.

set(Key, Val) ->
    erlang:put(Key, Val).

del(Key) ->
    erlang:erase(Key).
