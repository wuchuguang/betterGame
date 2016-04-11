%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 五月 2015 下午4:36
%%%-------------------------------------------------------------------
-module(com_module).
-include_lib("common/include/common.hrl").
%% API
-export([attr_value/2, attr_value/3, function_exported/3]).

attr_value(Module, Attr) ->
    Attrs = attributes(Module),
    ?get_value(Attr, Attrs, undefined).

attr_value(Module, Attr, Default) ->
    Attrs = attributes(Module),
    ?get_value(Attr, Attrs, Default).

function_exported({Module, FuncName, Args},TrueFunc, FalseFunc) when is_atom(Module), is_atom(FuncName)->
    case com_type:type(Args) of
        list->
            case erlang:function_exported(Module, FuncName, length(Args)) of
                true ->
                    exec(TrueFunc, Args);
                false->
                    exec(FalseFunc)
            end;
        integer->
            case erlang:function_exported(Module, FuncName, Args) of
                true ->
                    exec(TrueFunc);
                false->
                    exec(FalseFunc)
            end
    end;
function_exported(_MFA,_,FalseFunc) ->
%%     exit({_MFA, erlang:get_stacktrace()}),
    FalseFunc.

attributes(Module) ->
    code:ensure_loaded(Module),
    Module:module_info(attributes).

exec(Fun) when is_function(Fun) ->
    Fun();
exec({Module,Fun}) when is_atom(Module) andalso is_atom(Fun)->
    Module:Fun();
exec({Module,Fun,Arg}) when is_atom(Module), is_atom(Fun),is_list(Arg) ->
    apply(Module,Fun,Arg);
exec(Val) ->
    Val.

exec({Module,Fun},Args) when is_atom(Module) andalso is_atom(Fun)->
    apply(Module, Fun, Args);
exec({Module,Fun,Arg},_Args) when is_atom(Module),is_atom(Fun),is_list(Arg)->
    apply(Module,Fun,Arg);
exec(Fun, Args) when is_function(Fun) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    if
        Arity == 0 ->
            Fun();
        true ->
            apply(Fun, Args)
    end;
exec(Val,_Args) ->
    Val.
