%%%-----------------------------------
%%% @Module  : server_reader
%%% @Created : 2015.05.05
%%% @Description: 读取客户端 
%%%-----------------------------------
-module(server_reader).
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start_client/3, start_link/1, test_link/3, packprotocol/2, unpackprotocol/1, recv/1, send/2, send/3, close/1, close_asyn/1, set_agent/2]).

-include_lib("common/include/common.hrl").
-include_lib("network/include/network.hrl").

%%flash843安全沙箱
-define(FL_POLICY_REQ, <<"<polic">>).
-define(FL_POLICY_FILE, <<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>">>).

%% @doc 启动一个socket工作进程
-spec start_client(pid(), port(), #network_boot{}) -> {ok, pid()}.
start_client(ServerPid, Socket, Network) ->
    {ok, ConnPID} = start_link({Socket, Network}),
    ok = gen_tcp:controlling_process(Socket, ConnPID),
    server_reader:set_agent(ConnPID, ServerPid),
    server_reader:recv(ConnPID),
    {ok, ConnPID}.

%% @doc 测试TCP网络通不通的～～
test_link(UseIP, LinkIP, Port) ->
    gen_tcp:connect(LinkIP, Port, [{ip,com_socket:str2ip(UseIP)},binary,{active,false}]).

start_link(Args) ->
    boot:start(#boot{hasname = ?false, module = ?MODULE, type = ?boot_type_worker, params = Args}).

packprotocol(Cmd, Binary) ->
    {Len, BodyBin} = packcheckflag(Binary),
    <<Len:?HEADER_LEN, Cmd:?CMD_LEN, ?ENCODE_CODE:?ENCODE_CODE_LEN, BodyBin/binary>>.

packcheckflag(Binary) ->
    Len = checkflag(Binary),
    BodyBin = <<Binary/binary, Len:8>>,
    {byte_size(BodyBin), BodyBin}.

unpackprotocol(Binary) ->
    <<Len:?HEADER_LEN, Cmd:?CMD_LEN, ?ENCODE_CODE:?ENCODE_CODE_LEN, BodyBin/binary>> = Binary,
    {Cmd, BodyBin}.

%% TODO 下面是验证协议的代码。现在关闭了
%% unpackcheckflag(<<>>) ->
%%     {?false, protocol_data_fail};
%% unpackcheckflag(IntactBinary) ->
%%     Len = byte_size(IntactBinary),
%%     DataLen = (Len - 1) * 8,
%%     <<DataBody:DataLen/bitstring, Check:8>> = IntactBinary,
%%     Flag = checkflag(DataBody),
%%     case Check =:= Flag of
%%         ?true ->
%%             {?true, DataBody};
%%         ?false ->
%%             {?false, protocol_flag_fail}
%%     end.
unpackcheckflag(Binary) ->
    {?true, Binary}.

checkflag(Binary) ->
    Len = byte_size(Binary),
    {_, Data} =
    lists:foldl(
        fun(_, {<<Byte:8, RemainBinary/binary>>, DataLen}) ->
            {RemainBinary, Byte + DataLen}
        end,
        {Binary, 0}, lists:seq(1, Len)),
    Data band 16#000f.%%取低8位

recv(PID) ->
    gen_server:cast(PID, recv).

close(BeClose) ->
    Process = get_connection(BeClose),
    gen_server:call(Process, close).

close_asyn(BeClose) ->
    Process = get_connection(BeClose),
    gen_server:cast(Process, close).


send(Connection, Data) ->
    send(Connection, Data, send_no_delay).

send(Connection, Data, Option) ->
    {Module,Line,Binary} = send_data_debug_match(Data),
    case get_connection(Connection) of
        ?undefined ->
            ?LOG_ERROR("connection undefined send ~p option ~p cmd ~p from(~p, ~p)", [Connection, Option, xox_protocol:getcmd(Data),Module,Line]),
            ok;
        false->
            ?LOG_ERROR("connection undefined send ~p option ~p cmd ~p from(~p, ~p)", [Connection, Option, xox_protocol:getcmd(Data),Module,Line]),
            ok;
        Conn ->
            catch Conn ! {Option, Binary},
            ok
    end.

send_data_debug_match(Data) when is_binary(Data)->
    {none,none,Data};
send_data_debug_match({Data}) when is_binary(Data) ->
    {none,none,Data};
send_data_debug_match({Module, Line, Data}) ->
    {Module, Line, Data}.



set_agent(Conn, Agent) ->
    Fun =
    fun(OldConn) ->
        {ok, OldConn#connection{agent = Agent}}
    end,
    gen_server:cast(get_connection(Conn), {apply, Fun}).

init({Socket, #network_boot{module = Module}}) ->
    ?SYSTE_PROCESS(?true),
    Connection = #connection{socket = Socket, process = ?self,
                             module = Module,
                             ip = com_socket:socket2ip(Socket),
                             port = com_socket:socket2port(Socket)},
    connection:insert(Connection),
    case catch Module:onConnect(Connection) of
        ok ->
            {ok, Connection};
        #connection{} = NewConnection ->
            connection:insert(NewConnection),
            {ok, NewConnection};
        {ok, #connection{} = NewConnection} ->
            connection:insert(NewConnection),
            {ok, NewConnection};
        NotSupp ->
            ?LAGER_ERROR("~p:onConnect(~p) reply ~p notsupp",[Module, Connection, NotSupp]),
            {ok, Connection}
    end.

%% @hidden
handle_call(close, _From, Connection) ->
    {stop, normal, ok, Connection};
handle_call(Msg, _From, Connection) ->
    {reply, Msg, Connection}.

%% @hidden
handle_cast(recv, Connection) ->
    self() ! {loop, 0},
    recv_data(Connection);
handle_cast(close, Connection) ->
    {stop, normal, Connection};
handle_cast({apply, F}, State) when is_function(F, 0) ->
    F(),
    {noreply, State};
handle_cast({apply, F}, State) ->
    case F(State) of
        {ok, #connection{} = NewState} ->
            {noreply, NewState};
        ok ->
            {noreply, State};
        Other ->
            ?LAGER_DEBUG("handle cast other ~w", [Other]),
            {noreply, State}
    end;

handle_cast(_Msg, Connection) ->
    {noreply, Connection}.

%%
%% @hidden
%%
%% @doc 安全沙窗处理
handle_info({inet_async, Socket, Ref, {ok, ?FL_POLICY_REQ}}, #connection{ref = Ref, socket = Socket} = State) ->
    Len = 23 - ?HEADER_LENGTH,
    async_recv(Socket, Len, ?TCP_TIMEOUT),
    gen_tcp:send(Socket, ?FL_POLICY_FILE),
    ?LAGER_DEBUG("here"),
    {stop, normal, State};

%% {inet_async,#Port<0.849>,8,
%% {ok,<<"dddddsda">>}} {connection,
%% #Port<0.849>,1,8,0,
%% 66,<<>>,
%% binary2protocol,
%% undefined}
%% @doc 协议处理
handle_info({inet_async, Socket, Ref, {ok, FullBinary}}, #connection{ref = Ref, buffer = LastBinary, module = Module} = State) ->
    case <<LastBinary/binary, FullBinary/binary>> of
        <<Len:?HEADER_LEN, Cmd:?CMD_LEN, ?ENCODE_CODE:?ENCODE_CODE_LEN, IntactBinary/binary>> when byte_size(IntactBinary) == Len ->
            case unpackcheckflag(IntactBinary) of
                {true, BinaryFinal} ->
                    process_packet(State, {Cmd, BinaryFinal});
                {false, ErrBin} ->
                    ?LOG_ERROR("unpackcheck_flag fail ~p", [ErrBin]),
                    {stop, normal, State}
            end;
        <<Len:?HEADER_LEN, _Cmd:?CMD_LEN, ?ENCODE_CODE:?ENCODE_CODE_LEN, Reamin/binary>> = Buff ->
            NeedLen = Len - byte_size(Reamin),
            NewRef = async_recv(Socket, NeedLen, ?TCP_TIMEOUT),
            {noreply, State#connection{ref = NewRef, buffer = Buff}};
        NotSupportStream ->
            case Module:not_support_stream(NotSupportStream) of
                skip ->
                    {noreply, State};
                debug->
                    ?LOG_ERROR("inet_async debug not_support_stream ~p", [NotSupportStream]),
                    {noreply, State};
                Error ->
                    ?LOG_ERROR("inet_async ~p not support stream ~p", [Error, NotSupportStream]),
                    {stop, State}
            end
    end;

handle_info({inet_async, Socket, Ref, {error, timeout}}, #connection{module = Module, socket = Socket, ref = Ref, timeout = Timeout} = State) ->
    case Module:heart_timeout(Timeout) of
        true ->
            LastPacketTime = get(last_packet_time),
            ?LAGER_DEBUG("inet_async timeout! DiffTime:~w", [com_time:unixtime() - LastPacketTime]),
            {stop, normal, State};
        false ->
            recv_data(State#connection{timeout = Timeout + 1})
    end;

%%% @doc etimedout 异常处理
%do_info({inet_async, Socket,Ref,{error,etimedout}},#connection{ref = Ref,socket =Socket} = State)->
%	{stop,normal,State};	
%
%%% @doc closed 异常处理
handle_info({inet_async, Socket, Ref, {error, closed}}, #connection{ref = Ref, socket = Socket, module = Module} = State) ->
    ?LAGER_DEBUG("~p connection down error closed", [Module]),
    {stop, normal, State};


%% @doc 广泛的error 异常处理
handle_info({inet_async, Socket, Ref, {error, Error}}, #connection{ref = Ref, socket = Socket, module = Module} = State) ->
    ?LAGER_DEBUG("inet_async  module:~p error:~w,State:~w", [Module, Error, State]),
    {stop, normal, State};

%% @doc 发送缓冲区中的消息
%% 加入一分钟超时机制，如果超过一分钟没有收到任何消息包，则进程退出,并不发送地图
%%

handle_info({loop, Times}, #connection{module = Module, timeout_times = TimeoutTimes} = State) ->
    case Module:is_timeout(TimeoutTimes) of
        ?true ->
            ?LAGER_DEBUG("yj_connection time_out:~w", [State]),
            {stop, normal, State};
        ?false ->
            try
                send_buffer(State, 0)
            catch
                _:Reason1 ->
                    ?LAGER_DEBUG("yj_connection send_buffer error:~w", [Reason1])
            end,
            %% 自身消息发送循环
            erlang:send_after(?SEND_MSG_TIME_TICK, self(), {loop, Times + 1}),
            try
                case Module:handle_loop(Times, State) of
                    ok ->
                        {noreply, State#connection{timeout_times = TimeoutTimes + 1}};
                    close ->
                        ?LAGER_DEBUG("yj_connection close handle loop", []),
                        {stop, normal, State};
                    Other ->
                        ?LAGER_DEBUG("yj_connection handle loop fail", []),
                        {noreply, State#connection{timeout_times = TimeoutTimes + 1}}
                end
            catch
                _:Reason ->
                    ?LAGER_DEBUG("~w handle_loop error,Reason:~w,State:~w,player_id:~w", [?MODULE, Reason, State]),
                    {noreply, State#connection{timeout_times = TimeoutTimes + 1}}
            end
    end;

%% @doc 发送消息，无消息缓冲，直接发送
handle_info({send_no_delay, Bin}, State) when is_binary(Bin) ->
    connection:send_count(State#connection.socket,erlang:byte_size(Bin)),
    add_to_send_buffer(State, Bin, -1),
    {noreply, State};

%% @doc 发送消息，有消息缓冲
handle_info({send, Bin}, State) when is_binary(Bin) ->
    add_to_send_buffer(State, Bin, ?MAX_BUFFER),
    {noreply, State};

handle_info({inet_reply, Socket, ok},
            #connection{socket = Socket} = State) ->
    {noreply, State};

handle_info({inet_reply, Socket, Result},
            #connection{socket = Socket} = State) ->
    ?LAGER_DEBUG("socket is error, result is~p", [Result]),
    {stop, normal, State};

handle_info(Info, State) ->
    ?LAGER_DEBUG("yj_connection handle_info unknown(~p) Info~nState:~p~n", [Info, State]),
    {stop, normal, State}.

%% @hidden
code_change(_oldvsn, Status, _extra) ->
    {ok, Status}.

%% @hidden
terminate(_Reason, #connection{socket = Socket, module = Module} = Connection) ->
    gen_tcp:close(Socket),
    connection:delete(Socket),
    try
        Module:onClose(Connection),
        Module:terminate(_Reason)
    catch _:Reason ->
        ?LAGER_DEBUG("~w terminate fail:~w ~p", [Module, Reason, erlang:get_stacktrace()])
    end.

%%
%% Local Function
%%
async_recv(Socket, Length, Timeout) when is_port(Socket) ->
    case prim_inet:async_recv(Socket, Length, Timeout) of
        {error, Reason} ->
            ?LOG_ERROR("socket async_recv ~p", [Reason]),
            throw({Reason});
        {ok, Res} -> Res
    end.

recv_data(#connection{socket = Socket} = State) ->
    Ref = async_recv(Socket, ?HEADER_LENGTH, ?TCP_TIMEOUT),
    {noreply, State#connection{ref = Ref, buffer = <<>>}}.

%% @doc 获取发送缓冲队列
get_send_buffer() ->
    case get({?MODULE, send_buffer}) of
        undefined ->
            [];
        SendBuffer ->
            SendBuffer
    end.

%% @doc 设置发送缓冲队列
set_send_buffer(BufferList) ->
    put({?MODULE, send_buffer}, BufferList).

%% @doc 添加消息到缓冲队列中，如果 队列中的数据量大于 LimitSize，则直接发送
add_to_send_buffer(#connection{socket = Socket, module = Module}, Binary, LimitSize) ->
    Module:send_packet(Binary),
%%   ?DEBUG("Commands:~w",[proto_util:read_packet(Binary)]),
    do_send_buffer(Socket, [Binary | get_send_buffer()], LimitSize).


send_buffer(#connection{socket = Socket}, Size) ->
    do_send_buffer(Socket, get_send_buffer(), Size).

do_send_buffer(Socket, BufferList, LimitSize) ->
    Size = iolist_size(BufferList),
    case Size > 0 andalso Size >= LimitSize of
        true ->
%			CompressSize = ?CONFIG(compress,0),
%			case CompressSize =/= 0 andalso Size >= CompressSize of
%				true ->
%					Data = zlib:compress(lists:reverse(BufferList)),	
%					DataSize = erlang:byte_size(Data) + 4,
%			%		stat_socket:rec_compress(Size,DataSize),
%					%?DEBUG("compress:~p,~p,~p,~p",[Size,DataSize,(Size - DataSize)/Size,Value]),
%					do_send(Socket,<<DataSize:16,?COMPRESS_COMMAND:16,Data/binary>>);
%				false ->	
            do_send(Socket, lists:reverse(BufferList)),
%			end,
            set_send_buffer([]);
        false ->
            set_send_buffer(BufferList)
    end.

do_send(Socket, Data) ->
    try erlang:port_command(Socket, Data)
    catch
        _:Reason ->
            ?LOG_ERROR("Reason=====>~w ~n", [Reason])
    end.

%process_packet(#connection{cmd = ?PP_PING,socket = Socket} = State,?PP_PING,_Binary)->
%    L = case get(last_ping) of undefined->os:timestamp();D -> D end,
%    C = os:timestamp(),
%%     ?DEBUG("---- ~w",[timer:now_diff(C, L)div 1000 - 1000]),
%    put(last_ping, C),
%    lib_send:send_one(Socket, <<0,0,0,12,0,0,39,26,0,0,0,0>>),
%    recv_data(State);
process_packet(#connection{module = Module, socket = Socket, agent = Agent} = Connection, IntactBinary) ->
    {_Cmd, CmdData} = IntactBinary,
    connection:recv_count(Socket,erlang:bit_size(CmdData)),
    case catch Module:onMessage(Connection, IntactBinary) of
        ok ->
            recv_data(Connection);
        {change_agent, NewAgent} ->
            recv_data(Connection#connection{agent = NewAgent});
        {change_agent, NewAgent, NewModule} ->
            recv_data(Connection#connection{agent = NewAgent, module = NewModule});
        close ->
            ?LAGER_DEBUG("close connection,module:~p", [Module]),
            {stop, close, Connection};
        ?err_match(Error) ->
            ?LOG_ERROR("process_packet msg(~p,~p) module(~p) error(~p)", [Socket, Agent, Module, Error]),
            {stop, Error, Connection};
        ?exit(Way, Reason) ->
            ?LOG_ERROR("process_packet msg(~p,~p) module(~p) error(~p,~p)", [Socket, Agent, Module, Way, Reason]),
            {stop, normal, Connection};
        {stop, normal} ->
            close_asyn(Connection),
            {noreply,Connection};
        Other ->
            ?LOG_ERROR("process_packet module ~p error(other,~p)", [Module, Other]),
            {stop, normal, Connection}
    end.



get_connection(Socket) when is_port(Socket) ->
    get_connection(connection:get_by_socket(Socket));
get_connection(#connection{process = Process}) ->
    Process;
get_connection(false) ->
    false;
get_connection(Connection) ->
    Connection.

