%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 六月 2015 下午12:07
%%%-------------------------------------------------------------------
-author("root").
-record(db_table_field, {name,type,key='',null='NO',default,auto='NO'}).

-define(DB_DATATYPE_SMALL, small).
-define(DB_DATATYPE_TEMP, temp).
-define(DB_DATATYPE_INT, int).
-define(DB_DATATYPE_TERM, term).
-define(DB_DATATYPE_STERM, sterm).
-define(DB_DATATYPE_STRING,string).
-define(DB_DATATYPE_NUMBER, number).
-define(DB_DATATYPE_ATOM,atom).