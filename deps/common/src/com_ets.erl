%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 五月 2015 下午3:49
%%%-------------------------------------------------------------------
-module(com_ets).
-author("root").
-include_lib("common/include/common.hrl").

%% API
-export([info/1, while/3, size/1, prints/1]).

info(Tab) ->
    ets:info(Tab).

size(Tab) ->
    ets:info(Tab, size).

%% @doc Fun == function(_) function(_, _)
%% @doc Default
%% @doc Ets
%% @doc return----> ?break | ?break(Ret) | ?continue | ?continue(New)
while(Fun, Default, Ets) ->
    next(Fun, Default, Ets, ets:first(Ets)).

next(_Fun, Default, _Ets, '$end_of_table') ->
    Default;
next(Fun, Default, Ets, Key) when is_function(Fun,1)->
    [Data|_] = ets:lookup(Ets, Key),
    case Fun(Data) of
        ?break->Default;
        ?break(Ret) -> Ret;
        ?continue->next(Fun, Default, Ets, ets:next(Ets,Key));
        ?continue(New) -> next(Fun, New, Ets, ets:next(Ets,Key))
    end;
next(Fun, Default, Ets, Key) when is_function(Fun,2) ->
    [Data|_] = ets:lookup(Ets, Key),
    case Fun(Data, Default) of
        ?break->Default;
        ?break(Ret) -> Ret;
        ?continue->next(Fun, Default, Ets, ets:next(Ets,Key));
        ?continue(New) -> next(Fun, New, Ets, ets:next(Ets,Key))
    end.


prints(Tab) ->
    [io:format("~p~n",[Ele])||Ele <- ets:tab2list(Tab)].