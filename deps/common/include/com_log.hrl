%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 五月 2015 下午5:35
%%%-------------------------------------------------------------------
-author("root").


%% log_module callback
%% new_process(FuncName,PID)

-define(log_opt_module(__MODULE), {module, __MODULE}).
-define(log_opt_level(__Level), {level, __MODULE}).
-define(log_opt_process(__Process), {process, __Process}).
-define(log_opt_output(__Output), {process, __Output}).

-define(log_init(__OPTIONS), fun() ->
                                     lists:map(fun({Function,Argvs}) ->
                                                    com_log:Function(Argvs)
                                               end,__OPTIONS)
                             end()).
-define(level_debug, 1).
-define(level_info, 2).
-define(level_error, 3).

-define(LOG_DEBUG(__DATA), ?LAGER_DEBUG("~p",[__DATA])).
-define(LOG_DEBUG(__FMT, __DATA), ?LAGER_DEBUG("common",__FMT, __DATA)).
-define(LOG_DEBUG(__FunModule, __FMT, __DATA), com_log:log_module(__FunModule,?level_debug, io_lib:format("[DEBUG] S(~p)  M(~p)  L(~p)~n"++__FMT++"~n",[self(),?MODULE,?LINE]++__DATA))).

-define(LOG_INFO(__DATA), ?LOG_INFO("~p",[__DATA])).
-define(LOG_INFO(__FMT, __DATA), ?LOG_INFO("common",__FMT, __DATA)).
-define(LOG_INFO(__FunModule, __FMT, __DATA), com_log:log_module(__FunModule,?level_info, io_lib:format("[INFO] S(~p)  M(~p)  L(~p)~n"++__FMT++"~n",[self(),?MODULE,?LINE]++__DATA))).

-define(LOG_ERROR(__DATA), ?LOG_ERROR("~p",[__DATA])).
-define(LOG_ERROR(__FMT, __DATA), ?LOG_ERROR("common",__FMT, __DATA)).
-define(LOG_ERROR(__FunModule, __FMT, __DATA), com_log:log_module(__FunModule,?level_error, io_lib:format("[ERROR] S(~p)  M(~p)  L(~p)~n"++__FMT++"~n",[self(),?MODULE,?LINE]++__DATA))).
