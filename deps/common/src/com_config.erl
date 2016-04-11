%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 十一月 2015 下午4:19
%%%-------------------------------------------------------------------
-module(com_config).
-include("common.hrl").

%% API
-export([parse/1, file2appname/1, file2envs/1]).
-compile(export_all).

parse(Dir) ->
    case filelib:is_dir(Dir) of
        ?true ->
            Fun =
            fun(File, AppNames) ->
                AppName = file2appname(File),
%%                 application:load(AppName),
                set_envs(AppName, file2envs(File)),
                AppNames ++ [AppName]
            end,
            filelib:fold_files(Fun, "\.txt$", true, Dir, [])
    end.

set_envs(App, Envs) ->
    Fun =
    fun({EnvKey, EnvVal}) ->
        function("value", EnvKey, EnvVal)
    end,
    Functions = string:join(lists:map(Fun, Envs), ";\n") ++ ";\n value(_) -> ok.",
    io:format("functions ~p ~n",[Functions]),
    M = meta:new(App),
    {ok, M1} = meta:add_func(M, Functions, true),
    meta:to_src(M1, "test.erl").

function(Func, FunArg, FunCluase) ->
    Func ++ "(\"" ++ FunArg ++ "\") ->\n\t" ++ com_util:term_to_string(FunCluase).


file2appname(File) ->
    Base = filename:basename(File),
    Root = filename:rootname(Base),
    erlang:list_to_atom(Root).

%% file2envs(File) ->
%%     case zucchini:parse_file(File) of
%%         {ok, [{_,Envs}]} ->
%%             envs(Envs);
%%         Error ->
%%             io:format("file2envs ~p error ~p~n",[File, Error]),exit({file2envs, File})
%%     end.
file2envs(File) ->
    case zucchini:parse_file(File) of
        {ok, [{_,Envs}]} ->
            Envs;
        Error ->
            io:format("file2envs ~p error ~p~n",[File, Error]),exit({file2envs, File})
    end.

envs(Envs) ->
    Fun =
    fun({Key,Val}, Meta) ->
        bind_env(key(Key),Val, Meta)
    end,
    lists:foldl(Fun, meta:new(template), Envs).

-define(TYPE_VAR, "@").
-define(TYPE_POINT, "->").
-define(TYPE_LIST, "{").
bind_env(KeyP, Val, Meta) ->
    Fun =
    fun({?TYPE_VAR, Var}, Str) ->
        ok;
%%         {H ++ [Var], Envs0};
       ({?TYPE_POINT, Point}, Envs0) ->
           ok;
       ({?TYPE_LIST, List}, Envs0) ->
           ok
    end,
    lists:foldl(Fun, "", KeyP).


key(Key) ->
    case key_analysis(Key) of
        error ->
            error;
        Matchs ->
            io:format("key_matchs ~p~n",[Matchs]),
            key_dispose(Key, Matchs)
    end.

key_analysis(Key) ->
    case re:run(Key, "@|\\{|(-\>)", [global]) of
        {match,Match} ->
            Fun =
            fun([{P,L}|_]) ->
                {P+1,L}
            end,
            lists:map(Fun, Match);
        _ -> error
    end.

key_dispose(Key, Matchs) ->
    io:format("key_dispose ~p~n",[Matchs]),
    Count  = length(Matchs),
    Counts = lists:seq(1, Count),
    Fun =
    fun(Nth) ->
        {P,L} = lists:nth(Nth,Matchs),
        Type = string:substr(Key, P, L),
        L2 =
        case Nth >= Count of
            true ->
                string:len(Key);
            false->
                {P1,_} = lists:nth(Nth+1,Matchs),
                P1
        end,
        Data = string:substr(Key, P+L, L2-(P+L)),
        {Type, Data}
    end,
    lists:map(Fun, Counts).