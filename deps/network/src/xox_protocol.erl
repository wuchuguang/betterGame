%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 五月 2015 下午3:03
%%%-------------------------------------------------------------------
-module(xox_protocol).
-include("protocol.hrl").
-author("root").

%% API
-export([input/1, %%->readBytesCount
         decode/1, %%
         encode/1]).

-export([getcmd/1]).

input(_Stream) ->
    0.

decode(Stream) ->
    stream_reader:new(Stream),
    Len = stream_reader:int32(),
    Cmd = stream_reader:int32(),
    _Flag= stream_reader:int16(),
    Body = stream_reader:binary(Len),
    #xox_protocol{cmd = Cmd, bodys = Body}.

getcmd(Stream) ->
    stream_reader:new(Stream),
    _Len= stream_reader:int32(),
    Cmd = stream_reader:int32(),
    stream_reader:free(),
    Cmd.

encode(#xox_protocol{}) ->
    ok.
