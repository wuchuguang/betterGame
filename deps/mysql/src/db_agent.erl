%%%--------------------------------------
%%% @doc 统一的数据库处理模块
%%%--------------------------------------
-module(db_agent).
-include_lib("common/include/common.hrl").
-compile(export_all).

%%############数据库初始化##############
%% @doc 数据库连接初始化
init(filestore) ->
    ok = init_db(db_filestore, db_filestore, true),
    ok = init_db(db_log, db_log, true).


db_log(Module, Line, Level, FormatFun) ->
    {Format, Arguments} = FormatFun(),
    case Level of
        error ->
            ?LOG_ERROR("~w:~b: "++ Format ++ "~n", [Module, Line] ++ Arguments);
        _ ->
            ?LAGER_DEBUG("~w:~b: "++ Format ++ "~n", [Module, Line] ++ Arguments)
    end.

%% @doc 初始化数据库连接
init_db(Ref, [Type, Host, Port, User, Password, DB, Poolsize, Encode], IsReconnect) ->
    case Type of
        mysql ->
            LTemp =
            case Poolsize > 1 of
                true ->
                    lists:duplicate(Poolsize, dummy);
                false ->
                    [dummy]
            end,
            R = mysql:start_link(Ref, mysql_conn, Host, Port, User, Password, DB,  fun db_log/4, Encode),
            ?LAGER_DEBUG("ret ~p r ~p",[Ref, R]),
            [begin
                 mysql:connect(Ref, mysql_conn, Host, Port, User, Password, DB, Encode, IsReconnect)
             end || _ <- LTemp];
        mongo ->
            PoolId = com_type:to_list(Ref),
            emongo_sup:start_link(),
            emongo_app:initialize_pools([PoolId, Host, Port, DB, Poolsize]),
            ok;
        _ -> no_action
    end,
    ok;
init_db(Ref, DbConfig, IsReconnect) ->
    case ?CONFIG(db, DbConfig, ?undefined) of
        ?undefined ->
            ?LAGER_DEBUG("init_db ref ~p dbconfig ~p not config", [Ref, DbConfig]);
        DbConfigs ->
            ?LAGER_DEBUG("init_db ref ~p dbconfig ~p ", [Ref, DbConfigs]),
            init_db(Ref, analysis_db_config(DbConfigs), IsReconnect)
    end.

%% @doc 转换config
analysis_db_config(DBCfg)->
    {_, Type} = lists:keyfind(type, 1, DBCfg),
    {_, Host} = lists:keyfind(host, 1, DBCfg),
    {_, Port} = lists:keyfind(port, 1, DBCfg),
    {_, User} = lists:keyfind(user, 1, DBCfg),
    {_, Password} = lists:keyfind(pass, 1, DBCfg),
    {_, DBName} = lists:keyfind(name, 1, DBCfg),
    {_, Poolsize} = lists:keyfind(pool, 1, DBCfg),
    {_, Encode} = lists:keyfind(encode, 1, DBCfg),
    [Type, Host, Port, User, Password, DBName, Poolsize, Encode].