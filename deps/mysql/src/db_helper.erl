%% @author Administrator
%% @doc @todo Add description to db_helper.


-module(db_helper).

-include_lib("common/include/common.hrl").
-include("db_helper.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([insert/3,
         insert/4,
         select/3,
         select/4,
         select/5,
         update/4,
         delete/3,
         update/5]).

-export([function/3]).

-export([max/3]).

-export([amendment/3]).

%% @doc 插入数据
%% @see insert/4
insert(DB, Table, KeyValue) ->
    Module = get_db_module(),
    stat_db:record(Table,insert),
    Module:insert(DB, Table, KeyValue).

%% @doc 插入数据
insert(DB, Table, KeyValue, true) ->
    catch(insert(DB, Table, KeyValue));
insert(DB, Table, KeyValue, false)->
    insert(DB, Table, KeyValue).

%% @doc 搜索数据
select(DB, Table, Fields) ->
	Module = get_db_module(),
	FieldsString = tostring_by_fields(Fields),
    stat_db:record(Table,select),
	Module:select_all(DB, Table, FieldsString, [], [], []).

%% @doc 搜索数据
select(DB, Table, Fields, Limit) ->
	Module = get_db_module(),
	FieldsString = tostring_by_fields(Fields),
    stat_db:record(Table,select),
	Module:select_all(DB, Table, FieldsString, [], [], Limit).

%% @doc 搜索数据
select(DB, Table, Fields, ByWhere, Limit) ->
	Module = get_db_module(),
	FieldsString = tostring_by_fields(Fields),
    stat_db:record(Table,select),
	Module:select_all(DB, Table, FieldsString, ByWhere,[], Limit).

%% @doc 取表字段最大值
max(DB, Table, Field) ->
	Module = get_db_module(),
	[[Max]] = Module:functions(DB, Table, [{max, Field}]),
  Max.

%% @doc 更新数据
update(DB, Table, ByWhere, FieldsNamesValues) ->
	Module = get_db_module(),
    stat_db:record(Table,update),
	Module:update(DB, Table, FieldsNamesValues, ByWhere).

%% @doc 更新数据
update(DB, Table, ByWhere, FieldsNamesValues, true) ->
    catch(update(DB, Table, ByWhere, FieldsNamesValues));
update(DB, Table, ByWhere, FieldsNamesValues, false) ->
    update(DB, Table, ByWhere, FieldsNamesValues).

%% @doc 删除数据
delete(DB, Table, ByWhere) ->
	Module = get_db_module(),
    stat_db:record(Table,delete),
	Module:delete(DB, Table, ByWhere).

%% @doc 函数调用
function(Db, FunctionName, FunctionArgs) ->
    Module = get_db_module(),
    Module:function(Db, FunctionName, FunctionArgs).

%% @doc 修正表，表字段
amendment(DB, Table, Fields) ->
	Module = get_db_module(),
	case Module:get_fields(DB, Table) of
		{false, not_table} ->
			Module:create_table(DB, Table, Fields),
			table_new;
		{false, not_database} ->
			database_new;
		{true, ExistFields} ->
        Fields2 = fix_fields(Fields),
			case is_same_elements(ExistFields, Fields2) of
				true ->
					no_need;
				false->
					Module:fields_repair(DB, Table, ExistFields, Fields2),
					repair
			end
	end.
%% ====================================================================
%% Internal functions
%% ====================================================================

get_db_module() ->
	db_mysql.


-define(FIELDS(__Same,__DBExtra,__DefineExtra),{__Same,__DBExtra,__DefineExtra}).
is_same_elements(DBFields, DefineFields) ->
%	Fun1 = fun(#db_table_field{name=Name,key = Key,type = DataBaseType}=Field) ->
%				   case lists:keyfind(Name, #db_table_field.name, List2) of
%					   Field->true;
%             #db_table_field{key = Key,name = Name,type = Type} ->
%                case Type of
%                  term ->
%                    DataBaseType == string;
%                  _ ->
%                    false
%                end;
%					   _____->false
%				   end
%		   end,
%	IsTrue1 = lists:all(Fun1, List1),
%	Fun2 = fun(#db_table_field{name=Name}=Field) ->
%				   case lists:keyfind(Name, #db_table_field.name, List1) of
%					   Field->true;
%					   _____->false
%				   end
%		   end,
%	IsTrue2 = lists:all(Fun2, List2),
%	IsTrue1 andalso IsTrue2.
  F =
  fun(#db_table_field{name = FieldName} = DBField,?FIELDS(AccInSame,AccInDBExtra,AccInDefineExtra))->
    case lists:keytake(FieldName,#db_table_field.name,AccInDefineExtra) of
      false ->
        ?FIELDS(AccInSame,[DBField | AccInDBExtra],AccInDefineExtra);
      {value,DefineField,NewAccInDefineExtra} ->
        case is_same_field(DBField,DefineField) of
          true ->
            ?FIELDS([DBField | AccInSame],AccInDBExtra,NewAccInDefineExtra);
          false ->
            ?FIELDS(AccInSame,[DBField | AccInDBExtra],AccInDefineExtra)
        end
    end
  end,
  case lists:foldl(F,?FIELDS([],[],DefineFields),DBFields) of
    ?FIELDS(_Same,[],[]) ->
      true; 
    _ ->
      false
  end.

is_same_field(Field,Field)->
  true;
is_same_field(
  #db_table_field{name = Name,key = Key,type = string},
  #db_table_field{name = Name,key = Key,type = term}) when Key == 'PRI' ->
  true;
is_same_field(_DBField,_DefineField)->
  false.

	
tostring_by_fields(Fields) ->
  string:join(["`" ++ com_type:to_list(Field) ++ "`" || Field <- Fields],",").

fix_fields(Fields) ->
    fix_fields(Fields, []).

fix_fields([#db_table_field{type=?DB_DATATYPE_TERM}=Field|Fields],L) ->
    fix_fields(Fields,[Field#db_table_field{default=[]}|L]);
fix_fields([Field|Fields],L) ->
    fix_fields(Fields,[Field|L]);
fix_fields([],L) ->
    L.



	
