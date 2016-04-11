%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 五月 2015 上午11:32
%%%-------------------------------------------------------------------
-module(com_proc).
-author("root").

%% API
-export([info/1,info/2, exist/1, registered/0]).

-export([new/0, new/1, new/2, new/3, close/1, cast/2]).

-export([register_init/0, register_new/2, register_free/1, register_exist/1, register_get/1, register_list/0]).

info(PID) when is_pid(PID) ->
    erlang:process_info(PID);
info(Name) when is_atom(Name) ->
    info(whereis(Name));
info(List) when is_list(List)->
    info(list_to_pid(List)).

info(PID, Info) ->
    proplists:get_value(Info,info(PID)).

exist(undefined) ->false;
exist(PID) when is_pid(PID) ->
    erlang:is_process_alive(PID);
exist(Name) when is_atom(Name) ->
    exist(erlang:whereis(Name)).

registered() ->
    Fun =
    fun(Name) ->
        Pid = erlang:whereis(Name),
        io:format("Name ~p Pid ~p~n",[Name, Pid])
    end,
    lists:foreach(Fun, erlang:registered()).

new(Name,InitFun) ->
    new(Name, InitFun,undefined).
new(Name) ->
    new(Name,undefined).
new() ->
    new(undefined).

new(Name,InitFun, InitState) ->
    Name =/= undefined andalso begin
                                   case whereis(Name) of
                                       undefined->ok;
                                       NamePID -> throw({proc_name_defined, Name, NamePID})
                                   end
                               end,
    PID  = spawn(fun() -> loop_new__(InitState) end),
    Name =/= undefined andalso register(Name, PID),
    InitFun =/= undefined andalso cast(PID ,InitFun),
    PID.

cast(Proc,Exe) ->
    Proc ! {exefun,Exe}.

close(Proc) ->
    Proc ! close.

loop_new__(State) ->
    receive
        {exefun, Fun} ->
            case exe(Fun, State) of
                {ok, NewState} ->
                    loop_new__(NewState);
                _ ->
                    loop_new__(State)
            end;
        close->
            close
    end.
exe(Do, Args) ->
    exe__(Do, Args).

exe__(Fun, _Args) when is_function(Fun,0) ->Fun();
exe__(Fun, Args) when is_function(Fun,1) ->Fun(Args);
exe__({M,F,A},_Args) when is_atom(M),is_atom(F) ->
    apply(M,F,A).


register_init() ->
    ets:new(register_tab(), [public, set, named_table]).

register_new(Name,PID) ->
    case ets:lookup(register_tab(), Name) of
        []->
            ets:insert(register_tab(),{Name,PID}),
            true;
        _ ->
            false
    end.

register_free(PID) when is_pid(PID)->
    case ets:match_object(register_tab(), {'_',PID},1) of
        '$end_of_table' -> false;
        {[{Name,PID}|_],_} ->
            register_free(Name)
    end;
register_free(Name) ->
    ets:delete(register_tab(), Name).

register_get(Name) ->
    case ets:lookup(register_tab(), Name) of
        [{_,PID}] -> PID;
        _ -> false
    end.

register_exist(PID) when is_pid(PID) ->
    case ets:match_object(register_tab(), {'_',PID},1) of
        '$end_of_table' -> false;
        _ -> true
    end;
register_exist(Name) ->
    case ets:lookup(register_tab(), Name) of
        []-> false;
        _ -> true
    end.

register_list() ->
    ets:tab2list(register_tab()).

register_tab() ->
    register_tab.
