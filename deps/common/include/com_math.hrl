%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 五月 2015 下午4:26
%%%-------------------------------------------------------------------
%%自然对数的底
-define(E, 2.718281828459).

-define(IS_INT8(__INT8), (__INT8 >=0 andalso __INT8 =< 127)).
-define(IS_INT16(__INT16), (__INT16 >=0 andalso __INT16 =< 65535)).
-define(IS_INT32(__INT32), (__INT32 >=0 andalso __INT32 =< 2147483647)).