%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 五月 2015 上午10:24
%%%-------------------------------------------------------------------
-author("root").
-compile({parse_transform, module_task}).
-define(task_hook(Task), {task_hook, Task}).
-define(task_time_type_once, 0).
-define(task_time_type_repeat, 1).
-define(task_type_time, 1).
-define(task_type_event, 2).

%% wcg:
%% ref  =undefined:表示不能task:del(ref) 其它都可以，如果ref重复，那task:new(Task)不成功
-record(task, {ref=undefined::any(), type::integer(), type_key=0::any(),type_val=undefined::any(),exec=0::any(),exec_data=0::any()}).

-define(task_event_post(Type, TypeVal), task:exec_event(Type, TypeVal)).
-define(task_event_post(Type), ?task_event_post(Type, undefined)).