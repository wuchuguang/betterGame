%%% @Created : 2015.05.05
-module(proto_util).

-export([read_packet/1]).


read_packet(Binary)->
    do_read_packet(Binary,[]).

do_read_packet(<<>>,Result)->
    Result;
do_read_packet(<<Size:32,Command:32,0:32,Extra/binary>>,Result)->
    BitSize = (Size - 12) * 8,
    <<Data:BitSize,Extra1/binary>> = Extra,
    do_read_packet(Extra1,[{cmd,Command, tool:to_binary(Data)} | Result]).
