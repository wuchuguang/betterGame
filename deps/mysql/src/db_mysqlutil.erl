%%%--------------------------------------
%%% @Module  : db_mysqlutil
%%% @Created : 2011.07.27
%%% @Description: MYSQL数据库操作 
%%%--------------------------------------
-module(db_mysqlutil).
-include("mysql.hrl").

-export([execute/2,
    execute2/2,
    function/2,
    transaction/2,
    select_limit/3,
    select_limit/4,
    select_limit/5,
    get_all/2,
    get_one/2,
    get_row/2,
    make_select_sql/3,
    make_select_sql/5,
    make_inserts_sql/3,
    make_insert_sql/2,
    make_insert_sql/3,
    make_replace_sql/2,
    make_update_sql/3,
    make_update_sql/5,
    make_delete_sql/2,
    make_createtable_sql/1]).

-export([get_sql_val/1]).
%% 执行一个SQL查询,返回影响的行数
execute(Server, Sql) ->
    case mysql:fetch(Server, mysql_conn, Sql, undefined) of
        {updated, {_, _, _, R, _}} -> R;
        {data,{mysql_result,_,Data,_,_}} -> Data;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Server, Sql, Reason])
    end.

execute2(Server, Sql) ->
    case mysql:fetch(Server, mysql_conn, Sql, undefined) of
        {updated, {_, _, _, R, _}} -> R;
        {data,{mysql_result,_,Data,_,_}} -> Data;
        {error, {_,_,_,_,Reason}} ->
            {false, analyze_reason(Reason)}
    end.


%% @doc 调用函数
function(FunctionName, FunctionArgs) ->
    concat(["select ",FunctionName, "(", string:join([dbvalue(com_type:to_list(Arg))||Arg<-FunctionArgs],","),") "]).
%% @doc 事务处理
transaction(Server, Fun) ->
    case mysql:transaction(Server, mysql_conn, Fun) of
        {atomic, R} -> R;
        {updated, {_, _, _, R, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Server, transaction, Reason]);
        {aborted, {Reason, _}} -> mysql_halt([Server, transaction, Reason]);
        Error -> mysql_halt([Server, transaction, Error])
    end.

%% @doc 执行分页查询返回结果中的所有行
select_limit(Sql, Offset, Num) ->
    S = erlang:list_to_binary([Sql, <<" limit ">>, erlang:integer_to_list(Offset), <<", ">>, erlang:integer_to_list(Num)]),
    case mysql:fetch(mysql_conn, S) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([server, Sql, Reason])
    end.

select_limit(Sql, Args, Offset, Num) ->
    select_limit(mysql_dispatcher, Sql, Args, Offset, Num).

select_limit(Server, Sql, Args, Offset, Num) ->
    S = erlang:list_to_binary([Sql, <<" limit ">>, erlang:list_to_binary(erlang:integer_to_list(Offset)), <<", ">>, erlang:list_to_binary(erlang:integer_to_list(Num))]),
    mysql:prepare(Server, s, S),
    case mysql:execute(mysql_conn, s, Args) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Server, Sql, Reason])
    end.

%% @doc 取出查询结果中的第一行第一列
-spec get_one(any(), Sql::string()) -> list() | null.
get_one(Server, Sql) ->
    case mysql:fetch(Server, mysql_conn, Sql, undefined) of
        {data, {_, _, [], _, _}} -> null;
        {data, {_, _, [R], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Server, Sql, Reason])
    end.

%% @doc 取出查询结果中的第一行
get_row(Server, Sql) ->
    case mysql:fetch(Server, mysql_conn, Sql, undefined) of
        {data, {_, _, [], _, _}} -> [];
        {data, {_, _, [R], _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Server, Sql, Reason]);
        _ -> mysql_halt([Server, Sql, "db_mysqlutil:get_row error"])
    end.

%% @doc 取出查询结果中的所有行
get_all(Server, Sql) ->
    case mysql:fetch(Server, mysql_conn, Sql, undefined) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Server, Sql, Reason])
    end.

%% @doc 显示人可以看得懂的错误信息
mysql_halt([Server, Sql, Reason]) ->
    %?TEST_STACK_TRACE({Server,Sql,Reason}),
%%     ?DEBUG("~p,~p,~p",[Server,Sql,Reason]),
    io:format("~n~s~n",[Sql]),
    erlang:error({db_error, [Server, Sql, Reason]}).

%% @doc 生成插入语句
-spec make_insert_sql(Table_name::atom(), FieldList::[atom(),...], ValueList::[any(),...]) -> string().
make_insert_sql(Table_name, FieldList, ValueList) ->
    L = make_conn_sql(FieldList, ValueList, []),
    concat(["insert into `", Table_name, "` set ", L]).

%% @doc 生成插入集语句
-spec make_inserts_sql(Table_name::atom(), FieldList::[atom(),...], ValueList::[[any(),...],...]) -> string().
make_inserts_sql(TableName, FieldList, ValuesList) ->
    FieldStr = concat_struct(FieldList, "`"),
    ValuesStr= concat_structs(ValuesList, "'"),
    concat(["insert into `", TableName, "` ",FieldStr," values", ValuesStr]).

concat_struct(List, Split) ->
    concat_struct(List, "(", Split).

concat_struct([F|[]], Str, Split) ->
    concat([Str, Split,F, Split,") "]);
concat_struct([F|Re], Str, Split) ->
    Str2 = concat([Str, Split,F, Split,", "]),
    concat_struct(Re, Str2, Split).

concat_structs(List, Split) ->
    concat_structs(List, "", Split).

concat_structs([F|[]], Str, Split) ->
    concat([Str, concat_struct(F, Split)]);
concat_structs([F|Re], Str, Split) ->
    Str2 = concat([Str, concat_struct(F, Split), ", "]),
    concat_structs(Re, Str2, Split).



%%组合mysql update语句
%% 使用方式db_mysqlutil:make_update_sql(test,["row","r"],["测试",123],"id",1) 相当 update `test` set row='测试', r = '123' where id = '1'
%%Table:表名
%%Field：字段
%%Data:数据
%%Key:键
%%Data:值
make_update_sql(Table_name, Field, Data, Key, Value) ->
    L = make_conn_sql(Field, Data, []),
    concat(["update `", Table_name, "` set ",L," where ",Key,"= '",com_type:to_list(Value),"'"]).

make_conn_sql([], _, L ) ->
    L ;
make_conn_sql(_, [], L ) ->
    L ;
make_conn_sql([F | T1], [D | T2], []) ->
    L  = ["`", com_type:to_list(F), "`='",get_sql_val(D),"'"],
    make_conn_sql(T1, T2, L);
make_conn_sql([F | T1], [D | T2], L) ->
    L1  = L ++ [",`", com_type:to_list(F),"`='",get_sql_val(D),"'"],
    make_conn_sql(T1, T2, L1).

get_sql_val(Val) ->
    Val2 = unicode:characters_to_binary(Val, utf8),
    re:replace(com_type:to_list(Val2),"'","''",[global,{return,list}]).

make_insert_sql(Table_name, Field_Value_List) ->
    concat(["insert into `", Table_name, "` ", set2strings(Field_Value_List)]).

make_replace_sql(Table_name, Field_Value_List) ->
    concat(["replace into `", Table_name, "` ", set2strings(Field_Value_List)]).

make_update_sql(Table_name, Field_Value_List, Where_List) ->
    concat(["update `", Table_name, "` ", set2strings(Field_Value_List), where2string(Where_List)]).

where2string([]) ->
    "";
where2string(WhereList) ->
    Fun =
    fun({Field, Val}) ->
        concat(["`", Field, "` = '", Val, "'"]);
       ({Field,in,Vals}) ->
           concat(["`", Field, "` in (",string:join(Vals,", "), ")"]);
       ({Field,like,String}) ->
           concat(["`", Field, "` linke '", String, "'"])
    end,
    Strs = lists:map(Fun, WhereList),
    concat([" where ",string:join(Strs, " and ")]).

order2string([]) ->
    "";
order2string(OrderList) ->
    Fun =
    fun({Field}) ->
        concat(["`", Field, "` asc"]);
       ({Field,Dir}) ->
           concat(["`", Field, "` ", Dir]);
       (Field) ->
           concat(["`", Field, "` asc"])
    end,
    Strs = lists:map(Fun, OrderList),
    concat([" order by ", string:join(Strs, ", "), " "]).

limit2string(Limit_Num) ->
    case Limit_Num of
        [] -> "";
        [{Begin,End}] ->
            concat([" limit ",Begin, " ",End]);
        [Num] ->
            concat([" limit ",Num]);
        Num when erlang:is_integer(Num) ->
            concat([" limit ",Num])
    end.

set2strings(Field_Value_List) ->
    Fun =
    fun({Field, Val}) ->
        case catch concat(["`", Field, "` = '", Val, "'"]) of
            {'EXIT',_R} ->
                erlang:exit({set2strings, Field, Val});
            Ret -> Ret
        end;
       ({Field,sub,Val}) ->
           concat(["`", Field, "` -= '", Val, "'"]);
       ({Field,add,Val}) ->
           concat(["`", Field, "` += '", Val,"'"])
    end,
    Strs = lists:map(Fun, Field_Value_List),
    concat([" set ", string:join(Strs, ", "), " "]).


make_delete_sql(Table_name, Where_List) ->
    concat(["delete from `", Table_name, "` ", where2string(Where_List), ""]).

make_select_sql(Table_name, Fields_sql, Where_List) ->
    make_select_sql(Table_name, Fields_sql, Where_List, [], []).

make_select_sql(Table_name, Fields_sql, Where_List, Order_List, Limit_Num) ->
    WhereSql = where2string(Where_List),
    OrderSql = order2string(Order_List),
    LimitSql = limit2string(Limit_Num),
    concat(["select ", Fields_sql," from `", Table_name, "` ", WhereSql, OrderSql, LimitSql]).


make_createtable_sql(Table) ->
    FieldsSql = get_fields_sql(Table#table.fields),
    Sql = "CREATE TABLE " ++ com_type:to_list(Table#table.name) ++ "(" ++
                                                                   FieldsSql ++ ") engine=" ++ com_type:to_list(Table#table.engine)
                                                                                               ++ " DEFAULT CHARACTER SET=" ++
                                                                                                  com_type:to_list(Table#table.encoding) ++ " COLLATE=" ++ com_type:to_list(Table#table.collate),
    Sql.

get_fields_sql(Fields) ->
    Fun = fun(Field, []) ->
        get_field_sql(Field);
             (Field, S)  ->
                 S ++ ", " ++ get_field_sql(Field)
    end,
    lists:foldl(Fun, [], Fields).

get_field_sql(Field) ->
    Name = com_type:to_list(Field#table_field.name),
    DataType = com_type:to_list(Field#table_field.datatype),
    NULLString = case Field#table_field.isnull of true -> " NULL "; false -> " NOT NULL " end,
    Name ++ " " ++ DataType ++ "(" ++ com_type:to_list(Field#table_field.datalength) ++ ")" ++ NULLString.

analyze_reason(Reason) ->
    case match_table_notdexist(Reason) of
        true ->notexist;
        false->
            case match_table_not_field(Reason) of
                true -> notfield;
                false->
                    other
            end
    end.


match_table_notdexist(Reason) ->
    match(Reason, "doesn't exist").

match_table_not_field(Reason) ->
    match(Reason, "field list").

match(Reason, RE) ->
    String = com_type:to_list(Reason),
    case re:run(String, RE) of
        {match,_} -> true;
        _ -> false
    end.


concat(List) ->
    lists:concat([com_type:to_list(L)||L<-List]).

dbvalue(Val) ->
    lists:concat(["'",Val,"'"]).