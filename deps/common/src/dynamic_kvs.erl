%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十二月 2015 下午2:43
%%%-------------------------------------------------------------------
-module(dynamic_kvs).

-include("common.hrl").

-export([start/1, start/0, get_name/1, init/1, handle_call/3, new/1, new/2]).
%% API
-export([set/3]).

-record(dynamic_kvs, {
    modules =[] :: [{Mod::module(),[{Key::atom(),Val::any()}]}]
}).

start(FatherPID) ->
    boot:start_child(FatherPID, #boot{module = ?MODULE,params = {},type = ?boot_type_worker}).

start() ->
    boot:start(#boot{module = ?MODULE,params = {},type = ?boot_type_worker}).

get_name(_) ->
    ?MODULE.

init(_) ->
    {ok, #dynamic_kvs{modules = []}}.

new(ModuleName) ->
    new(ModuleName, []).

new(ModuleName, Kvs) ->
    boot:call(?MODULE, {new, ModuleName, Kvs}).

set(ModuleName, Key, Val) ->
    boot:call(?MODULE, {set, ModuleName, Key, Val}).

handle_call({new, ModuleName, Kvs}, _, DynamicKvs) ->
    gen(ModuleName, Kvs),
    NewDynamicKvs =
    case lists:keyfind(ModuleName, 1, DynamicKvs#dynamic_kvs.modules) of
        false->
            DynamicKvs#dynamic_kvs{modules = [{ModuleName,Kvs}|DynamicKvs#dynamic_kvs.modules]};
        _ ->
            DynamicKvs
    end,
    {reply, ok, NewDynamicKvs};

handle_call({set, ModuleName, Key, Val}, _, DynamicKvs) ->
    case lists:keytake(ModuleName, 1, DynamicKvs#dynamic_kvs.modules) of
        false->
            {reply, {error, not_find_module}, DynamicKvs};
        {value,{_,Kvs}, RemainMods} ->
            NewKvs = com_lists:keyreplace(Key, 1, Kvs, {Key, Val}),
            gen(ModuleName, NewKvs),
            {reply, ok, DynamicKvs#dynamic_kvs{modules = [{ModuleName,NewKvs}|RemainMods]}}
    end.


concat(List) ->
    lists:concat(List).

gen(ModuleName, Kvs) ->
    Meta = meta:new(ModuleName),
    Fun =
        fun({K,V}, MetaTemp) ->
            FunStr = concat([K,"() ->\n\t",com_util:term_to_string(V),".\n"]),
            case meta:add_func(MetaTemp, FunStr) of
                {ok, MetaTemp2} -> MetaTemp2;
                Error ->
                    io:format("module(~p) define func(~s) error ~p~n", [ModuleName, FunStr,Error]),
                    exit(Error)
            end
        end,
    FinalMeta = lists:foldl(Fun, Meta, Kvs),
    FinalMeta2 = meta:add_func(FinalMeta, concat(["set(Key, Val) ->\n\tdynamic_kvs:set(",ModuleName,", Key, Val).\n"])),
    case meta:compile(FinalMeta2) of
        {error, Error} ->
            io:format("error ~p",[Error]),
            ok;
        ok ->
            ok
    end.