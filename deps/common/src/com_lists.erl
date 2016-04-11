%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 六月 2015 下午2:11
%%%-------------------------------------------------------------------
-module(com_lists).
-include_lib("common/include/common.hrl").
%% API
-export([while/3, keyreplace/4, position/2]).

while(Fun, [H|List], Default) ->
    case Fun(H) of
        ?break-> Default;
        ?break(Break) -> Break;
        ?continue->
            while(Fun,List,Default);
        ?continue(Continue) ->
            while(Fun,List,Continue)
    end;
while(_Fun, [], Default) ->
    Default.


keyreplace(Key, Nth, List, NewElem) ->
    case lists:keytake(Key, Nth, List) of
        false-> [NewElem | List];
        {value, _, List0} -> [NewElem | List0]
    end.

position(Member, List) ->
    position__(Member, List, 0).

position__(Member, [Member|_L], P) -> P+1;
position__(Member, [_|L], P) -> position__(Member, L, P+1);
position__(_, _, _P) -> {error, not_find}.