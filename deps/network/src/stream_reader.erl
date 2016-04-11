%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%  协议数据读取模块<br/>
%%% how to use it<br/>
%%%    先把数据流 stream_reader:new(BinaryStream) 初始好（必需！！！！）<br/>
%%%    就可以按之所需 Int64 = stream_reader:int64() 方式来读取数据了<br/>
%%%                 Struct = stream_reader:struct({int64,int8})<br/>
%%%                 Array  = stream_reader:array({int64,int8})<br/>
%%%    最后 stream_reader:free()<br/>
%%% @end
%%% Created : 11. 五月 2015 上午10:49
%%%-------------------------------------------------------------------
-module(stream_reader).
-author("root").


%% API
-export([new/1, free/0]).
-export([int64/0,int32/0,int16/0,int8/0, bitstring/0, string/0, utf8_str/0, binary/0, binary/1, array/1, array/2, struct/1, remain/0]).

new(New)->
    set_binary(New).

free() ->
    del_binary().

int64() ->
    <<Int32H:32,Int32L:32,Remain/binary>> = get_binary(),
    set_binary(Remain),
    Int32H bsl 32 bor Int32L.

int32() ->
    <<Int32:32,Remain/binary>> = get_binary(),
    set_binary(Remain),
    Int32.


int16() ->
    <<Int16:16,Remain/binary>> = get_binary(),
    set_binary(Remain),
    Int16.

int8() ->
    <<Int8:8,Remain/binary>> = get_binary(),
    set_binary(Remain),
    Int8.

bitstring() ->
    <<Size:16,Bin1/binary>> = get_binary(),
    <<Binary:Size/binary-unit:8,Rest/binary>> = Bin1,
    set_binary(Rest),
    Binary.

string() ->
    Binary = bitstring(),
    com_type:to_list(Binary).

utf8_str() ->
    Str = string(),
    unicode:characters_to_list(erlang:list_to_binary(Str),utf8).

binary() ->
    <<Size:32,Bin1/binary>> = get_binary(),
    <<Str:Size/binary-unit:8,Rest/binary>> = Bin1,
    set_binary(Rest),
    Str.
binary(Len) ->
    Bin = get_binary(),
    BinSize = byte_size(Bin),
    case BinSize < Len of
        true->
            io:format("binary(~p) out , realsize ~p",[Len, BinSize]),{error,out_binarysize};
        _ ->
            <<Str:Len/binary-unit:8,Rest/binary>> = Bin,
            set_binary(Rest),
            Str
    end.

array(Struct) ->
    array(Struct, fun(Ret) -> Ret end).

array(Struct, FormatFun) ->
    <<Size:16,Bin1/binary>> = get_binary(),
    set_binary(Bin1),
    Fun = fun(_) ->
        Ret = struct(Struct),
        FormatFun(Ret)
    end,
    lists:map(Fun, lists:seq(1,Size)).

struct(Struct) when is_tuple(Struct)->
    Fun =
    fun({Type,TypeParam},L) ->
        Ret = ?MODULE:Type(TypeParam),
        [Ret|L];
       (Type,L)->
           Ret = ?MODULE:Type(),
           [Ret|L]
    end,
    Rets = lists:foldl(Fun, [], tuple_to_list(Struct)),
    list_to_tuple(lists:reverse(Rets));

struct(Value) ->
    ?MODULE:Value().

remain() ->
    Bin = get_binary(),
    set_binary(<<>>),
    Bin.


-define(STREAM, {?MODULE, stream}).
get_binary() ->
    com_util:dic_get(?STREAM, <<>>).

set_binary(New) ->
    com_util:dic_set(?STREAM, New).

del_binary() ->
    com_util:dic_erase(?STREAM).
