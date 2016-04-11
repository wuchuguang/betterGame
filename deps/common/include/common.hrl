%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 五月 2015 上午11:16
%%%-------------------------------------------------------------------

%% 常用atom定为macro
-define(true, true).
-define(false, false).
-define(break, break).
-define(continue, continue).
-define(undefined, undefined).

-compile({parse_transform, module_strengthen}).

-define(break(__VALUE), {?break, __VALUE}).
-define(continue(__VALUE), {?continue, __VALUE}).


-define(ifdo(__CONDS, __DO), ?func(case __CONDS of ?true -> __DO;_ -> false end)()).
-define(ifdo_else(__CONDS, __DO, _ELSE), ?func(case __CONDS of ?true -> __DO;_ -> _ELSE end)()).
-define(inline(__FUN, __ARGCOUNT), -compile({inline, [__FUN / __ARGCOUNT]})).

-define(format(__FORMAT, __DATA), io_lib:format(__FORMAT, __DATA)).

%% format_record(record名, record数据) -> [{#record.field, record_field_value}]
-define(format_record(__RECORD_NAME, __RECORD_DATA),
    fun() ->
        __Fields = record_info(fields, __RECORD_NAME),
        [_ | __DATA] = tuple_to_list(__RECORD_DATA),
        {__RECORD_NAME, lists:zip(__Fields, __DATA)}
    end()).

-define(block(__BLOCK), begin __BLOCK end).

-define(CONFIG(__KEY), ?CONFIG(__KEY, undefined)).

-define(CONFIG(__KEY, __DEF), (fun() ->
    case application:get_env(__KEY) of
        {ok, Val} -> Val;
        _ -> __DEF
    end
                               end)()).

-define(CONFIG(__KEY, __SUBKEY, __DEF), (fun() ->
    case application:get_env(__KEY) of
        {ok, Val} when is_list(Val) ->
            proplists:get_value(__SUBKEY, Val, __DEF);
        _ -> __DEF
    end
                                         end)()).

-define(LOG_DEBUG(__DATA), ?LOG_DEBUG("~p", [__DATA])).
-define(LOG_DEBUG(__FMT, __DATA), io:format("[DEBUG] S(~p)  M(~p)  L(~p)~n" ++ __FMT ++ "~n", [self(), ?MODULE, ?LINE] ++ __DATA)).

-define(LOG_ERROR(__DATA), ?LOG_ERROR("~p", [__DATA])).
-define(LOG_ERROR(__FMT, __DATA), io:format("[ERROR] S(~p)  M(~p)  L(~p)~n" ++ __FMT ++ "~n", [self(), ?MODULE, ?LINE] ++ __DATA)).

-define(LOG_INFO(__DATA), ?LOG_INFO("~p", [__DATA])).
-define(LOG_INFO(__FMT, __DATA), io:format("[INFO] S(~p)  M(~p)  L(~p)~n" ++ __FMT ++ "~n", [com_type:to_list(self()),
    com_type:to_list(?MODULE),
    com_type:to_list(?LINE)] ++ __DATA)).

-define(LAGER_DEBUG(__DATA), ?LAGER_DEBUG("~p", [__DATA])).

-define(LAGER_DEBUG(__FORMAT, __DATA), ?LARGER_LOG(debug, __FORMAT, __DATA)).

-define(LAGER_INFO(__DATA), ?LARGER_LOG(info, "~p", [__DATA])).

-define(LAGER_INFO(__FORMAT, __DATA), ?LARGER_LOG(info, __FORMAT, __DATA)).


-define(LAGER_WARNING(__DATA), ?LARGER_LOG(warning, "~p", [__DATA])).

-define(LAGER_WARNING(__FORMAT, __DATA), ?LARGER_LOG(warning, __FORMAT, __DATA)).

-define(LAGER_ERROR(__DATA), ?LARGER_LOG(error, "~p", [__DATA])).

-define(LAGER_ERROR(__FORMAT, __DATA), ?LARGER_LOG(error, __FORMAT, __DATA)).




-define(LARGER_LOG(__LEVEL, __FORMAT, __DATA), lager:log(__LEVEL, self(), "M(~p)L(~p)S(~p) ~n" ++ __FORMAT, [?MODULE, ?LINE, self()] ++ __DATA)).

-define(debug_kv(__K_V_LIST), lists:foldl(fun({K, V}, Str) -> ?format("~s ~p(~p)", [Str, K, V]) end, "", __K_V_LIST)).

-define(return(__RETURN), throw(__RETURN)).

-define(exit(__EXIT), {'EXIT', __EXIT}).
-define(exit(__BASE, __REASON), ?exit({__BASE, __REASON})).

-define(tmlm(__TAG, __MODULE, __LINE, __MSG), {__TAG, {__MODULE, __LINE}, __MSG}).

-define(err(__ERR), ?err_match(?MODULE, ?LINE, __ERR)).
-define(err_match(__ERR), ?err_match(_, _, __ERR)).
-define(err_match(__MODULE, __LINE, __ERR), ?tmlm(err, __MODULE, __LINE, __ERR)).

-define(ok(__OK), ?ok_match(?MODULE, ?LINE, __OK)).
-define(ok_match(__OK), ?ok_match(_, _, __OK)).
-define(ok_match(__MODULE, __LINE, __OK), ?tmlm(ok, __MODULE, __LINE, __OK)).

-define(notknow(__NOTKNOW), ?notknow_match(?MODULE, ?LINE, __NOTKNOW)).
-define(notknow_match(__NOTKNOW), ?notknow_match(_, _, __NOTKNOW)).
-define(notknow_match(__MODULE, __LINE, __NOTKNOW), ?tmlm(notknow, __MODULE, __LINE, __NOTKNOW)).

-define(pack_sendbinary(__Binary), {?MODULE, ?LINE, __Binary}).

-define(noreply(__State), {noreply, __State}).
-define(stop(__Reason, __NewState), {stop, __Reason, __NewState}).

-define(self, self()).

-define(msg(__MSG), {msg, ?MODULE, ?LINE, __MSG}).

-define(send_msg(__MSG), ?send_msg(?self, __MSG)).
-define(send_msg(__SENDER, __MSG), (__SENDER ! __MSG)).

-define(func(__Fun), fun() -> __Fun end).

-define(get_value(__KEY, __LIST), ?get_value(__KEY, __LIST, ?undefined)).
-define(get_value(__KEY, __LIST, __DEFAULT), common:get_value(__KEY, __LIST, __DEFAULT)).

%% base
-define(amc(__MFA, __Options, Module, Line), fun() ->
    case catch __MFA of
        ?exit(Way, _) ->
            ErrMsg = ?get_value(Way, __Options, {Way, undefined}),
            Debug = ?get_value(debug, __Options, not_defined),
            ?LAGER_ERROR("M(~p)L(~p) D(~p) ErrMsg(~p) stacktrace(~p)", [Module, Line, Debug, ErrMsg, erlang:get_stacktrace()]),
            error({undef, __MFA});
        Ret -> Ret
    end
                                             end()).

%% api
-define(amc(__MFA), ?amc(__MFA, [], ?MODULE, ?LINE)).
%% api
-define(amc(__MFA, __Options), ?amc(__MFA, __Options, ?MODULE, ?LINE)).

-define(catch_exp(__EXP), fun() -> catch __EXP end()).

-define(SYSTE_PROCESS(__TRUE_OR_FLASE), erlang:process_flag(trap_exit, __TRUE_OR_FLASE)).

-record(boot, {module :: atom(), type :: boot_type(), hasname = true :: boolean(), params :: any()}).

-define(boot_type_supervisor, supervisor).
-define(boot_type_worker, worker).
-define(boot_type_simple_worker, simple_worker).
-type boot_type() :: ?boot_type_simple_worker | ?boot_type_supervisor | ?boot_type_worker.
-define(MATH_INT32_MAX, 4294967296).


-define(cast_int(__Val), com_type:to_integer(__Val)).
-define(cast_str(__Val), com_type:to_list(__Val)).
-define(cast_erl(__Val), com_type:to_list(__Val)).

-define(record_key_val_get(__RECORDNAME, __RECORDDATA, __KEY), (fun() ->
    case erlang:is_integer(__KEY) of
        true ->
            erlang:element(__KEY, __RECORDDATA);
        false ->
            Fields = record_info(fields, __RECORDNAME),
            case com_lists:position(__KEY, Fields) of
                {error, Error} ->
                    {error, Error};
                Position ->
                    erlang:element(Position + 1, __RECORDDATA)
            end
    end
                                                                end)()).

-define(record_key_val_set(__RECORDNAME, __RECORDDATA, __KEY, __VAL), (fun() ->
    case erlang:is_integer(__KEY) of
        true ->
            erlang:setelement(__KEY, __RECORDDATA, __VAL);
        false ->
            Fields = record_info(fields, __RECORDNAME),
            case com_lists:position(__KEY, Fields) of
                {error, Error} ->
                    {error, Error};
                Position ->
                    erlang:setelement(Position + 1, __RECORDDATA, __VAL)
            end
    end
                                                                       end)()).