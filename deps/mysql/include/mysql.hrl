%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 六月 2015 下午12:05
%%%-------------------------------------------------------------------
-record(mysql_result,
{fieldinfo=[],
 rows=[],
 affectedrows=0,
 error=""}).
-define(MYSQL_ENCODING_UTF8, utf8).

-define(MYSQL_ENGINE_INNODB, innodb).
-record(table_field, {name,datatype,datalength, comment="", default, isnull=true,iskey=false, isrd=false}).
-record(table, {db, name, record, fields=[], encoding = 'utf8', engine = 'InnoDB', collate = 'utf8_general_ci'}).

-define(MYSQL_DATA_TYPE_INT, int).
-define(MYSQL_DATA_TYPE_VARCHAR, varchar).
-define(MYSQL_DATA_TYPE_BITINT, bitint).
-define(MYSQL_DATA_TYPE_TEXT, text).

-type query_result() :: any().

-type mysql_result() :: any().