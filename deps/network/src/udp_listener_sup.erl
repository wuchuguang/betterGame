%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十一月 2015 下午2:29
%%%-------------------------------------------------------------------
-module(udp_listener_sup).
-include_lib("common/include/common.hrl").
-include_lib("network/include/network.hrl").
%% API
-export([startup/2, get_name/1, init/1]).

-export([receiver/1, send/3]).

-record(udp_module, {module, ip, port, protocol}).

startup(FatherPID, NetBoot) ->
    case gen_udp:open(NetBoot#network_boot.port, [{ip, com_socket:str2ip(NetBoot#network_boot.ip)}, {reuseaddr, true}]) of
        {ok, Socket} ->
            PID = spawn(?MODULE, receiver, [#udp_module{module = NetBoot#network_boot.module,
                                                        protocol = NetBoot#network_boot.protocol}]),
            gen_udp:controlling_process(Socket, PID),
            PID;
        Error ->
            Error
    end.

send(IP, Port, Data) ->
    case gen_udp:open(0, [{reuseaddr, true}]) of
        {ok, Socket} ->
            Pack = pack(Data),
            gen_udp:send(Socket, IP, Port, Pack),
            receive
                {udp, SocketIn, IPIn, PortIn, PacketIn} ->
                    io:format("send receive SocketIn ~p, IPIn ~p, PortIn ~p, PacketIn ~p",[SocketIn, IPIn, PortIn, PacketIn])
            end
    end.

%%     boot:start_child(FatherPID, #boot{module = ?MODULE, type = ?boot_type_supervisor,params = Boot}).

pack(Data) when is_integer(Data) ->
    list_to_binary(lists:map(fun(_) -> 1 end, lists:seq(1, Data)));
pack(Bin) ->
    Bin.

receiver(Bin) ->
    receive
        {udp, Socket, IP, InPortNo, Packet} ->
            io:format("socket ~p ip ~p inportno ~p packet ~p~n",[Socket, IP, InPortNo, Packet]),
            gen_udp:send(Socket, IP, InPortNo, Packet),
            receiver(Packet);
        stop ->
            io:format("stop ~p~n",[?LINE])
    end.

get_name(_) ->
    ?MODULE.

init(_) ->
    [].
