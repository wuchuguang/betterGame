%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 五月 2015 上午11:37
%%%-------------------------------------------------------------------

-define(socket_map_tab, socket_map_tab).
-record(socket_map_tab, {socket::port(), pid::pid(), create_time=com_time:unixtime(true)::integer()}).

-record(network_boot,{acceptor_count::integer(),protocol=xox_protocol::atom(),ip=undefined::term()|list(),port::integer(),module::atom()}).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true},{delay_send,true}, {send_timeout, 5000}, {keepalive, false}, {backlog, 64}, {exit_on_close, true}]).
-define(TCP_TIMEOUT, 10*1000).   % 解析协议超时时间

-define(HEADER_LENGTH, ((?HEADER_LEN+?ENCODE_CODE_LEN+?CMD_LEN) div 8)). % 消息头长度

-define(BODY_CHECK_FLAG_LEN, 8).
-define(HEADER_LEN, 32).    %消息头标识长度
-define(CMD_LEN, 32).

-define(ENCODE_CODE,0).     % 加密消息标识
-define(ENCODE_CODE_LEN, 16).%这个加密的消息标识长度

-define(MAX_PACKET_LENGTH,1024000).
-define(SEND_MSG_TIME_TICK,200).
-define(MAX_BUFFER,1300).

%%socket接收进程状态
-record(connection, {socket::port(),
                     process::pid(),
                     ip::term()|string(),
                     port::integer(),
                     timeout = 0::integer(),
                     ref::any(),
                     body_len = 0::integer(),
                     timeout_times = 0::integer(),
                     buffer = <<>>::binary(),
                     module = undefined::atom(),
                     agent = undefined::any()}).
