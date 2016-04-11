%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 六月 2015 下午3:48
%%%-------------------------------------------------------------------
-module(com_params).
-author("root").

%% API
-export([input/1, input/2]).

input(Strings) ->
    input(Strings, fun(_) -> true end).

input(Strings, CheckFun) ->
    Fun =
    fun(Str, Ret) ->
        case re:run(Str, "^--(?<Key>(.*))=(?<Val>(.*))", [{capture, all_names, list}]) of
            {match,[Key,Val]} ->
                [{Key,Val}|Ret];
            _ ->
                case re:run(Str,"^-(?<Key>(.*))", [{capture, all_names, list}]) of
                    {match,[Key]} ->
                        [{Key}|Ret];
                    _ ->
                        [Tuple|Ret2] = Ret,
                        [com_tuple:append(Str, Tuple)|Ret2]
                end
        end
    end,
    Kvs = lists:foldl(Fun, [], Strings),
    lists:filtermap(CheckFun, Kvs).
