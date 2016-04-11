%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%  协议数据写入模块<br/>
%%% how to use it<br/>
%%%    就可以按之所需 stream_reader:int64(Int64) 方式来写入数据了<br/>
%%%                 stream_reader:struct({Int64,Int8}, {int64,int8})<br/>
%%%                 stream_reader:array([{Int64,Int8}],{int64,int8})<br/>
%%%    最后 stream_reader:to_binary() 返回Binary<br/>
%%% @end
%%% Created : 11. 五月 2015 上午11:37
%%%-------------------------------------------------------------------
-module(stream_writer).
-author("root").

%% API
-export([free/0,int64/1,int32/1,int16/1,int8/1,string/1, utf8_str/1,binary/1,array/2, struct/2]).
-export([to_binary/0]).

%% @doc 释放流
free() ->
    set_binary(<<>>).

%% @doc 写入int64
int64(Int64) when is_integer(Int64)->
    set_binary(<<(get_binary())/binary,Int64:64>>),
    <<Int64:64>>.

%% @doc 写入int32
int32(Int32)->
    set_binary(<<(get_binary())/binary,(com_type:to_integer(Int32)):32>>),
    <<(com_type:to_integer(Int32)):32>>.

%% @doc 写入int16
int16(Int16)->
    set_binary(<<(get_binary())/binary,(com_type:to_integer(Int16)):16>>),
    <<(com_type:to_integer(Int16)):16>>.

%% @doc 写入int8
int8(Int8) ->
    set_binary(<<(get_binary())/binary,(com_type:to_integer(Int8)):8>>),
    <<(com_type:to_integer(Int8)):8>>.

%% @doc 写入字符串，字符串字节长度16位
string(Str) ->
    Bin = com_type:to_binary(Str),
    Len = byte_size(Bin),
    Binary = <<(get_binary())/binary,Len:16,Bin/binary>>,
    set_binary(Binary),
    Binary.

%% @doc 写入字符串（支持中文utf8）
utf8_str(Str) ->
    Bin = unicode:characters_to_binary(Str,utf8),
    Len = byte_size(Bin),
    Binary = <<(get_binary())/binary,Len:16,Bin/binary>>,
    set_binary(Binary),
    Binary.

%% @doc 写入二进制字节 字节长度为32位
binary(SrcBinary) ->
    Bin = com_type:to_binary(SrcBinary),
    Len = byte_size(Bin),
    Binary = <<(get_binary())/binary,Len:32,Bin/binary>>,
    set_binary(Binary),
    Binary.

%% @doc 写入数据，数组长度为16位,支持结构／简单类型的元素
array([], _Type) ->
    set_binary(<<(get_binary())/binary,0:16>>),
    <<0:16>>;
array(List,{struct,Struct})->
    Length = length(List),
    set_binary(<<(get_binary())/binary,Length:16>>),
    Fun=
    fun(Element) ->
        ?MODULE:struct(Element,Struct)
    end,
    lists:foreach(Fun, List);
array(List, Struct) when is_tuple(Struct) ->
    array(List,{struct,Struct});
array(List, Type) ->
    Length = length(List),
    set_binary(<<(get_binary())/binary,Length:16>>),
    Fun=
    fun(Element) ->
        ?MODULE:Type(Element)
    end,
    lists:foreach(Fun, List).

%% @doc 写入结构数据
struct(Data, Struct) ->
    Fun=
    fun({TypeData,{Type,Value}})->
        ?MODULE:Type(TypeData,Value);
       ({TypeData,Type}) ->
           ?MODULE:Type(TypeData)
    end,
    lists:foreach(Fun, lists:zip(tuple_to_list(Data),tuple_to_list(Struct))).

%% @doc 清空写入流，返回流二进制数据
to_binary() ->
    Binary = get_binary(),
    del_binary(),
    Binary.


-define(STREAM, {?MODULE, stream}).
get_binary() ->
    com_util:dic_get(?STREAM, <<>>).

set_binary(New) ->
    com_util:dic_set(?STREAM, New).

del_binary() ->
    com_util:dic_erase(?STREAM).

