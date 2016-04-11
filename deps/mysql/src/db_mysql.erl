%%%--------------------------------------
%%% @created : 2011.07.27
%%% @deprecated: mysql 数据库操作
%%%--------------------------------------
-module(db_mysql).
-include_lib("common/include/common.hrl").
-include("mysql.hrl").
-include("db_helper.hrl").

-export([insert/3, insert/4, insert2/3, insert2/4, inserts/4, insert_result/1]).

-export([replace/3]).

-export([update/4, update/6]).

-export([select_all/4, select_all/6, select_count/3, select_one/4, select_one/6, select_row/4, select_row/6]).

-export([delete/3, get_fields/2, function/3, transaction/2, execute/2]).

-export([alter_table/4, fields_repair/4, show_tables/1, create_table/3]).

%% @doc 插入数据表
insert(Server, Table_name, FieldList, ValueList) ->
	Sql = db_mysqlutil:make_insert_sql(Table_name, FieldList, ValueList),
	db_mysqlutil:execute(Server, Sql).
insert(Server, Table_name, Field_Value_List) ->
	Sql = db_mysqlutil:make_insert_sql(Table_name, Field_Value_List),
	db_mysqlutil:execute(Server, Sql).

%% @doc 插入数据表
-spec insert2(Server::atom(), Table_name::any(), FieldList::[atom(),...], ValueList::[any(),...]) ->
	{ok, any()} | {false, notexist|notfield|other}.
insert2(Server, Table_name, FieldList, ValueList) ->
	Sql = db_mysqlutil:make_insert_sql(Table_name, FieldList, ValueList),
	db_mysqlutil:execute2(Server, Sql).

%% @doc 插入数据表
-spec insert2(Server::atom(), Table_name::any(), Field_Value_List::[{atom(),any()},...]) ->
	{ok, any()} | {false, notexist|notfield|other}.
insert2(Server, Table_name, Field_Value_List) ->
	Sql = db_mysqlutil:make_insert_sql(Table_name, Field_Value_List),
	db_mysqlutil:execute2(Server, Sql).

%% @doc 插入数据集
inserts(Server, TableName, FieldList, ValuesLists) ->
	Sql = db_mysqlutil:make_inserts_sql(TableName, FieldList, ValuesLists),
	db_mysqlutil:execute(Server, Sql).

%% 修改数据表(replace方式)
replace(Server, Table_name, Field_Value_List) ->
	Sql = db_mysqlutil:make_replace_sql(Table_name, Field_Value_List),
	db_mysqlutil:execute(Server, Sql).

%% 修改数据表(update方式)
update(Server, Table_name, Field_Value_List, Where_List) ->
	Sql = db_mysqlutil:make_update_sql(Table_name, Field_Value_List, Where_List),
	db_mysqlutil:execute(Server, Sql).

update(Server, Table_name, Field, Data, Key, Value) ->
	Sql = db_mysqlutil:make_update_sql(Table_name, Field, Data, Key, Value),
	db_mysqlutil:execute(Server, Sql).

%% 获取一个数据字段
select_one(Server, Table_name, Fields_sql, Where_List) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List),
	db_mysqlutil:get_one(Server, Sql).

select_one(Server, Table_name, Fields_sql, Where_List, Order_List, Limit_num) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List, Order_List, Limit_num),
	db_mysqlutil:get_one(Server, Sql).

%% 获取一条数据记录
select_row(Server, Table_name, Fields_sql, Where_List) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List),
	db_mysqlutil:get_row(Server, Sql).

select_row(Server, Table_name, Fields_sql, Where_List, Order_List, Limit_num) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List, Order_List, Limit_num),
	db_mysqlutil:get_row(Server, Sql).

%% 获取记录个数 
select_count(Server, Table_name, Where_List) ->
	select_row(Server, Table_name, "count(1)", Where_List).

%% 获取所有数据
select_all(Server, Table_name, Fields_sql, Where_List) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List),
	db_mysqlutil:get_all(Server, Sql).
select_all(Server, Table_name, Fields_sql, Where_List, Order_List, Limit_num) ->
	Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List, Order_List, Limit_num),
	db_mysqlutil:get_all(Server, Sql).

%% 删除数据
delete(Server, Table_name, Where_List) ->
	Sql = db_mysqlutil:make_delete_sql(Table_name, Where_List),
	db_mysqlutil:execute(Server, Sql).

get_fields(Server, Table) ->
	case is_exist_table(Server, Table) of
		true ->
			SQL = io_lib:format("DESC ~s", [com_type:to_list(Table)]),
			Result = db_mysqlutil:execute(Server, SQL),
			Result2 =
			[#db_table_field{name=com_type:to_atom(Name),type=db_to_datatype(Type),null=com_type:to_atom(Null),key=db_to_key(com_type:to_atom(Key)),default=db_to_default(db_to_datatype(Type),Default)}||
				[Name,Type,Null,Key,Default,_] <-
					Result
			],
			{true, Result2};
		false->
			{false, not_table}
	end.

db_to_key('PRI') ->
	'PRI';
db_to_key(_) ->
	''.

db_to_datatype(DataType)->
	MatchList = [
		{DataType, <<"text">>, ?DB_DATATYPE_TERM},
		{DataType, <<"varchar">>, ?DB_DATATYPE_STRING},
		{DataType, <<"int">>, ?DB_DATATYPE_NUMBER},
		{DataType, <<"smallint">>, ?DB_DATATYPE_SMALL}
	],
	Fun = fun({DT,Content,Ret}) ->
		case match_datatype(DT,Content) of
			true->?break(Ret);
			false->?continue
		end
	end,
	com_lists:while(Fun, MatchList, false).

match_datatype(DataType,Content) ->
	case re:run(DataType, Content, [{capture, all, binary}]) of
		{match,_} ->true;
		_________ ->false
	end.

db_to_default(_, <<>>) ->
	[];
db_to_default(_, undefined) ->
	[];
db_to_default(?DB_DATATYPE_NUMBER, Default) ->
	case catch(com_type:to_integer(Default)) of
		{_,_} -> 0;
		Number-> Number
	end;
db_to_default(?DB_DATATYPE_STRING, Default) ->
	com_type:to_list(Default);
db_to_default(?DB_DATATYPE_TERM, _) ->
	false.


alter_table(Server, Table, {"", ColumnName, ColumnDT}, BeforeColumn) ->
	SQL = io_lib:format("ALTER TABLE `~s` ADD COLUMN `~s` ~s NULL AFTER `~s`", [com_type:to_list(Table),com_type:to_list(ColumnName), com_type:to_list(ColumnDT), BeforeColumn]),
	db_mysqlutil:execute(Server, SQL);

alter_table(Server, Table, {Old, NowColumnName, NowColumnDT}, BeforeColumn) ->
	SQL = io_lib:format("ALTER TABLE `~s` CHANGE COLUMN `~s` ~s ~s NULL AFTER `~s`", [com_type:to_list(Table),com_type:to_list(Old), com_type:to_list(NowColumnName), com_type:to_list(NowColumnDT), BeforeColumn]),
	db_mysqlutil:execute(Server, SQL).

show_tables(Server) ->
	db_mysqlutil:execute(Server, "SHOW   TABLES").

is_exist_table(Server, Table) ->
	case db_mysqlutil:execute(Server, "SHOW   TABLES   LIKE   '"++ com_type:to_list(Table) ++"'") of
		[] -> false;
		_  -> true
	end.

function(Server, FunctionName, FunctionArgs) ->
	SQL = db_mysqlutil:function(FunctionName, FunctionArgs),
	db_mysqlutil:execute(Server, SQL).

create_table(DB, Table, Fields) ->
	Fun = fun(Field, {[], ""}) ->
		L =
		case Field#db_table_field.key of
			'PRI' ->
				[Field#db_table_field.name];
			'' ->
				[]
		end,
		{L, tostring_by_field(Field)};
			 (Field, {L, S}) ->
				 L2 =
				 case Field#db_table_field.key of
					 'PRI' ->
						 [Field#db_table_field.name|L];
					 '' ->
						 L
				 end,
				 {L2, S ++ ", " ++tostring_by_field(Field)}
	end,
	{Pris, FieldsString} = lists:foldl(Fun, {[], ""}, Fields),
	PrisString = case Pris of
					 [] -> "";
					 __ -> ",PRIMARY KEY (" ++ lists:foldl(fun(F, "") -> "`" ++ com_type:to_list(F) ++ "`";
															  (F, Str) -> Str ++ ", `" ++ com_type:to_list(F)++"`"
														   end,
														   "",
														   Pris) ++
						 ")"
				 end,
	SQL = "CREATE TABLE `"++ com_type:to_list(Table) ++"` (" ++ FieldsString ++PrisString ++ ") ENGINE=InnoDB AUTO_INCREMENT=32 DEFAULT CHARSET=utf8",
	?LAGER_DEBUG("create ~s", [SQL]),
	db_mysqlutil:execute(DB, SQL).

tostring_by_field(#db_table_field{name=Name,null=Null,type=?DB_DATATYPE_TERM,key=''}) ->
	io_lib:format("`~s` ~s ~s ", [com_type:to_list(Name),datatype_to_mysql_datatype(?DB_DATATYPE_TERM), datanull_to_mysql_datanull(Null)]);
tostring_by_field(#db_table_field{name=Name,null=Null,type=?DB_DATATYPE_NUMBER,key='PRI',auto='YES'}) ->
	io_lib:format("`~s` ~s ~s ~s", [com_type:to_list(Name),datatype_to_mysql_datatype(?DB_DATATYPE_NUMBER), datanull_to_mysql_datanull(Null), "AUTO_INCREMENT"]);
tostring_by_field(#db_table_field{name=Name,null=Null,type=?DB_DATATYPE_TERM,key='PRI'}) ->
	io_lib:format("`~s` ~s ~s ", [com_type:to_list(Name),datatype_to_mysql_datatype(?DB_DATATYPE_STRING), datanull_to_mysql_datanull(Null)]);
tostring_by_field(#db_table_field{name=Name,null=Null,type=Type, default=Default}) ->
	io_lib:format("`~s` ~s ~s ~s", [com_type:to_list(Name),datatype_to_mysql_datatype(Type), datanull_to_mysql_datanull(Null),default_to_mysql_default(Type,Default)]).

-define(REPAIR_RESULT(__DelFields, __AddFields, __ChangeFields, __DelPri, __AddPri),
	{__DelFields, __AddFields, __ChangeFields, __DelPri, __AddPri}).
fields_repair(DB, Table, OldFields, NewFields) ->
	Fun1 = fun(Field, ?REPAIR_RESULT(DelFields, AddFields, ChangeFields, DelPri, AddPri)) ->
		case lists:keyfind(Field#db_table_field.name, #db_table_field.name, OldFields) of
			false->
				case Field#db_table_field.key of
					'PRI' ->
						?REPAIR_RESULT(DelFields, [Field|AddFields], ChangeFields, DelPri, [Field#db_table_field.name|AddPri]);
					'' ->
						?REPAIR_RESULT(DelFields, [Field|AddFields], ChangeFields, DelPri, AddPri)
				end;
			Field->
				case Field#db_table_field.key of
					'PRI' ->
						?REPAIR_RESULT(DelFields, AddFields, ChangeFields, [Field#db_table_field.name|DelPri], [Field#db_table_field.name|AddPri]);
					'' ->
						?REPAIR_RESULT(DelFields, AddFields, ChangeFields, DelPri, AddPri)
				end;
			OldField->
				case {OldField#db_table_field.key, Field#db_table_field.key} of
					{'PRI', ''} ->
						?REPAIR_RESULT(DelFields, AddFields, [Field|ChangeFields], [OldField#db_table_field.name|DelPri], AddPri);
					{'', 'PRI'} ->
						?REPAIR_RESULT(DelFields, AddFields, [Field|ChangeFields], DelPri, [Field#db_table_field.name|AddPri]);
					{'PRI','PRI'} ->
						?REPAIR_RESULT(DelFields, AddFields, [Field|ChangeFields], [OldField#db_table_field.name|DelPri], [Field#db_table_field.name|AddPri]);
					{'',  ''} ->
						?REPAIR_RESULT(DelFields, AddFields, [Field|ChangeFields], DelPri, AddPri);
					{_,_} ->
						?REPAIR_RESULT(DelFields, AddFields, [Field|ChangeFields], DelPri, AddPri)
				end
		end
	end,
	RepairResult = lists:foldl(Fun1, ?REPAIR_RESULT([], [], [], [], []), NewFields),
	Fun2 = fun(Field, ?REPAIR_RESULT(DelFields, AddFields, ChangeFields, DelPri, AddPri)) ->
		case lists:keyfind(Field#db_table_field.name, #db_table_field.name, NewFields) of
			false->
				case Field#db_table_field.key of
					'PRI' ->
						?REPAIR_RESULT([Field#db_table_field.name|DelFields], AddFields, ChangeFields, [Field#db_table_field.name|DelPri], AddPri);
					_ ->
						?REPAIR_RESULT([Field#db_table_field.name|DelFields], AddFields, ChangeFields, DelPri, AddPri)
				end;
			_ ->
				?REPAIR_RESULT(DelFields, AddFields, ChangeFields, DelPri, AddPri)
		end
	end,
	RepairResult2 = lists:foldl(Fun2, RepairResult, OldFields),
	SQL = get_sql_by_repair(Table, RepairResult2),
	db_mysqlutil:execute(DB, SQL),
	ok.

get_sql_by_repair(Table, ?REPAIR_RESULT(DelFields, AddFields, ChangeFields, DelPri, AddPri)) ->
	DelFieldsString = delfields_to_string(DelFields),
	AddFieldsString = addfields_to_string(AddFields),
	ChangeFieldsString = changefields_to_string(ChangeFields),
	DelPriString = delpri_to_string(DelPri),
	AddPriString = addpri_to_string(AddPri),
	S = "ALTER TABLE " ++ com_type:to_list(Table) ++ DelFieldsString ++ AddFieldsString ++ ChangeFieldsString ++ DelPriString ++ AddPriString,
	lists:sublist(S, erlang:length(S) - 2).

delfields_to_string(Fields) ->
	delfields_to_string(Fields, "").
delfields_to_string([], Str) ->
	Str;
delfields_to_string([Field|Fields], Str) ->
	S = Str ++ " DROP COLUMN `" ++ com_type:to_list(Field) ++"` , ",
	delfields_to_string(Fields, S).

addfields_to_string([]) ->
	"";
addfields_to_string(Fields)->
	addfields_to_string(Fields, "").
addfields_to_string([], Str) ->
	Str;
addfields_to_string([Field|Fields], Str) ->
	S = io_lib:format("~s ADD COLUMN ~s , ", [Str, tostring_by_field(Field)]),
	addfields_to_string(Fields, S).

changefields_to_string([]) ->
	"";
changefields_to_string(Fields) ->
	changefields_to_string(Fields, "").
changefields_to_string([], Str) ->
	Str;
changefields_to_string([Field|Fields],Str) ->
	S = io_lib:format("~s MODIFY COLUMN ~s , ", [Str, tostring_by_field(Field)]),
	changefields_to_string(Fields, S).

delpri_to_string([]) ->
	"";
delpri_to_string(_DelPri) ->
	" DROP PRIMARY KEY , ".

addpri_to_string([]) ->
	"";
addpri_to_string(AddPri) ->
	Fun = fun(Field, "") ->
		"`" ++ com_type:to_list(Field) ++ "` ";
			 (Field, Str) ->
				 Str ++ ", `" ++ com_type:to_list(Field) ++ "`"
	end,
	S = lists:foldl(Fun, "", AddPri),
	" ADD PRIMARY KEY (" ++ S ++ "), ".

datatype_to_mysql_datatype(?DB_DATATYPE_INT) ->
	"int(255)";
datatype_to_mysql_datatype(?DB_DATATYPE_SMALL) ->
	"smallint(255)";
datatype_to_mysql_datatype(?DB_DATATYPE_NUMBER) ->
	"bigint(255)";
datatype_to_mysql_datatype(?DB_DATATYPE_STRING) ->
	"varchar(255) CHARACTER SET utf8";
datatype_to_mysql_datatype(?DB_DATATYPE_ATOM) ->
	"varchar(255) CHARACTER SET utf8";
datatype_to_mysql_datatype(?DB_DATATYPE_STERM) ->
	"varchar(255) CHARACTER SET utf8";
datatype_to_mysql_datatype(?DB_DATATYPE_TERM) ->
	"text CHARACTER SET utf8".

default_to_mysql_default(?DB_DATATYPE_INT, []) ->
	"";
default_to_mysql_default(?DB_DATATYPE_INT, Default) ->
	" DEFAULT " ++ com_type:to_list(Default);
default_to_mysql_default(?DB_DATATYPE_SMALL, []) ->
	"";
default_to_mysql_default(?DB_DATATYPE_SMALL, Default) ->
	" DEFAULT " ++ com_type:to_list(Default);
default_to_mysql_default(?DB_DATATYPE_NUMBER, []) ->
	"";
default_to_mysql_default(?DB_DATATYPE_NUMBER, Default) ->
	" DEFAULT " ++ com_type:to_list(Default);
default_to_mysql_default(?DB_DATATYPE_STRING, []) ->
	"";
default_to_mysql_default(?DB_DATATYPE_ATOM, '') ->
	"";
default_to_mysql_default(?DB_DATATYPE_ATOM, Default) ->
	" DEFAULT " ++ com_type:to_list(Default);
default_to_mysql_default(?DB_DATATYPE_STRING, Default) ->
	" DEFAULT " ++ com_type:to_list(Default);
default_to_mysql_default(?DB_DATATYPE_STERM, _Default) ->
	"";
default_to_mysql_default(?DB_DATATYPE_TERM, _Default) ->
	"".

datanull_to_mysql_datanull('YES') ->
	" NULL";
datanull_to_mysql_datanull(_) ->
	" NOT NULL".


%% 事务处理
transaction(Server, Fun) ->
	db_mysqlutil:transaction(Server, Fun).

%% 执行SQL语句
execute(Server, Sql) ->
	db_mysqlutil:execute(Server, Sql).


insert_result(_Result) ->
	ok.