%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 五月 2015 下午3:43
%%%-------------------------------------------------------------------
-author("root").

-define(stream_int64, int64).
-define(stream_int32, int32).
-define(stream_int16, int16).
-define(stream_int8, int8).
-define(stream_string, string).
-define(stream_array, array).
-define(stream_struct, struct).

-define(read(__BINARY), stream_reader:new(__BINARY)).
-define(read_int8(), stream_reader:int8()).
-define(read_int16(), stream_reader:int16()).
-define(read_int32(), stream_reader:int32()).
-define(read_int64(), stream_reader:int64()).
-define(read_string(), stream_reader:string()).
-define(read_struct(__STRUCT), stream_reader:struct(__STRUCT)).
-define(read_array(__STRUCT), stream_reader:array(__STRUCT)).

-define(write(), stream_writer:to_binary()).
-define(write_int8(__INT8), stream_writer:int8(__INT8)).
-define(write_int16(__INT16), stream_writer:int16(__INT16)).
-define(write_int32(__INT32), stream_writer:int32(__INT32)).
-define(write_int64(__INT64), stream_writer:int64(__INT64)).
