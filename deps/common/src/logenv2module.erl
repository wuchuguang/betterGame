%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 十二月 2015 上午10:51
%%%-------------------------------------------------------------------
-module(logenv2module).

%% API
-export([start/1, compile/1]).

-define(MODULE_LOG_ENV, module_log_env).

start(FileName) ->
    case file:consult(FileName) of
        {ok, Consult} ->
            compile(Consult),
            {ok, ?MODULE_LOG_ENV};
        Error ->
            Error
    end.

compile(Consult) ->
    Fun =
    fun({Module, LogTags}, Meta) ->
        FunString = module_log_tags(Module, LogTags),
        io:format(" funstring ~s ~n",[FunString]),
        {ok, Meta1} = meta:add_func(Meta, FunString, true),
        Meta1
    end,
    ModuleMeta = lists:foldl(Fun, meta:new(?MODULE_LOG_ENV),Consult),
    meta:compile(ModuleMeta).


module_log_tags(Module, LogTags) ->
    Fun =
    fun(Tag) ->
        concat([Module, "(", Tag,") ->\n\ttrue; \n"])
    end,
    Trues = string:join(lists:map(Fun, LogTags), ";\n"),
    Trues ++ concat([Module, "(_) ->\n\tfalse."]).

concat(List) ->
    lists:concat(List).

