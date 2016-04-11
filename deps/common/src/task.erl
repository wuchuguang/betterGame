%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 五月 2015 上午10:33
%%%-------------------------------------------------------------------
-module(task).
-include_lib("common/include/common.hrl").
-include_lib("common/include/task.hrl").
-author("root").

%% API
-export([new/1, del/1, exec_time/1, exec_event/2]).

new(NewTask)->
    case NewTask#task.ref of
        ?undefined->
            ignore;
        Ref ->
            del(Ref)
    end,
    case NewTask#task.type of
        ?task_type_event->
            new_event(NewTask);
        ?task_type_time ->
            new_time(NewTask)
    end.

del(Ref) ->
    case ref(Ref) of
        ?undefined->
            ?undefined;
        RefVal when erlang:is_reference(RefVal) ->
            erlang:cancel_timer(RefVal),
            ref(Ref, ?undefined),
            {?true, time_ref};
        EventType->
            NewEvents = lists:keydelete(Ref, #task.ref, events(EventType)),
            events(EventType, NewEvents),
            ref(Ref, ?undefined),
            {?true, task_ref}
    end.


new_time(NewTask) ->
    NextTime = caluc_next_time(NewTask),
    Ref = erlang:send_after(NextTime,self(),?task_hook(NewTask)),
    case NewTask#task.ref of
        ?undefined->
            ignore;
        NeedRef->
            ref(NeedRef, Ref)
    end,
    ok.

new_event(NewTask) ->
    NewTask#task.ref /= ?undefined andalso ref(NewTask#task.ref, NewTask#task.type_key),
    Events = events(NewTask#task.type_key),
    events(NewTask#task.type_key,[NewTask|Events]).

exec_time(Task) ->
    do_exec(Task),
    Task#task.ref /= ?undefined andalso ref(Task#task.ref, ?undefined),
    case Task#task.type_key of
        ?task_time_type_once->
            finish;
        ?task_time_type_repeat->
            new(Task);
        _ ->
            ignore
    end.

exec_event(EventType, EventVal) ->
    Events = events(EventType),
    ?LAGER_DEBUG("exec_event EventType(~p), EventVal(~p), Events(~p)", [EventType, EventVal, Events]),
    [do_exec(Task)||#task{type_val = Typeval}=Task<-Events, Typeval=:=?undefined orelse Typeval=:=EventVal],
    ok.


do_exec(Task) when erlang:is_function(Task#task.exec,1)->
    (Task#task.exec)(Task#task.exec_data);
do_exec(Task) when erlang:is_function(Task#task.exec,0)->
    (Task#task.exec)();
do_exec(#task{exec = {Module,Function},exec_data = Data}) ->
    apply(Module,Function,Data);
do_exec(Task) ->
    ?LOG_ERROR("do_exec task not execute ~p",[Task]).


caluc_next_time(#task{type_key = ?task_time_type_once,type_val = Time}) ->
    Time;
caluc_next_time(#task{type_key = ?task_time_type_repeat, type_val = Time}) ->
    Time;
caluc_next_time(Task) ->
    exit({task_next_time,Task}).

-define(EVENT(__TYPE), {?MODULE, event, __TYPE}).
events(Type) ->
    com_util:dic_get(?EVENT(Type), []).

events(Type, Events) ->
    com_util:dic_set(?EVENT(Type), Events).

-define(REF(__REF), {?MODULE, ref, __REF}).
ref(Ref) ->
    com_util:dic_get(?REF(Ref), ?undefined).

ref(Ref, ?undefined) ->
    com_util:dic_erase(?REF(Ref));
ref(Ref, Val) ->
    com_util:dic_set(?REF(Ref), Val).
