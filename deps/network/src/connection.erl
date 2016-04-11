%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     记录当前所有socket连接
%%% @end
%%% Created : 30. 五月 2015 下午2:18
%%%-------------------------------------------------------------------
-module(connection).
-include_lib("common/include/common.hrl").
-include_lib("network/include/network.hrl").

%% API
-export([start/1, get_name/1, init/1, handle_cast/2, handle_call/3, terminate/2, insert/1, delete/1, get_by_socket/1]).

-export([size/0, recv_count/2, recv_count/1, send_count/2, send_count/1]).

-define(RECV_SOCKET_DIC(__SOCKET), {rsd,__SOCKET}).
-define(RECV_SOCKETS_DIC, rsds).

-define(SEND_SOCKET_DIC(__SOCKET), {ssd,__SOCKET}).
-define(SEND_SOCKETS_DIC, ssds).


%% @doc 启动一个socket连接集进程
start(PID) ->
    boot:start_child(PID, #boot{module = ?MODULE, type = ?boot_type_worker}).

recv_count(Socket, Size) ->
    boot:cast(?MODULE, {recv_count, Socket, Size}).

size() ->
    ets:info(?MODULE, size).

recv_count(Tag) ->
    boot:call(?MODULE, {recv_data, Tag}).

send_count(Socket, Size) ->
    boot:cast(?MODULE, {send_count, Socket, Size}).

send_count(Tag) ->
    boot:call(?MODULE, {send_data, Tag}).

%% @doc 进程名
get_name(_) ->
    ?MODULE.

%% @doc 始初化
init(_) ->
    ets:new(?MODULE, [{keypos, #connection.socket},public,set,named_table]),
    {ok, ok}.

handle_cast({recv_count, Socket, Size}, ok) ->
    {OldSize,OldCount} = com_util:dic_get(?RECV_SOCKET_DIC(Socket), {0,0}),
    com_util:dic_set(?RECV_SOCKET_DIC(Socket), {OldSize + Size, OldCount+1}),
    {OldSizes, OldCounts} = com_util:dic_get(?RECV_SOCKETS_DIC, {0,0}),
    com_util:dic_set(?RECV_SOCKETS_DIC, {OldSizes + Size, OldCounts+1}),
    {noreply, ok};

handle_cast({send_count, Socket, Size}, ok) ->
    {OldSize,OldCount} = com_util:dic_get(?SEND_SOCKET_DIC(Socket), {0,0}),
    com_util:dic_set(?SEND_SOCKET_DIC(Socket), {OldSize + Size, OldCount+1}),
    {OldSizes, OldCounts} = com_util:dic_get(?SEND_SOCKETS_DIC, {0,0}),
    com_util:dic_set(?SEND_SOCKETS_DIC, {OldSizes + Size, OldCounts+1}),
    {noreply, ok};

handle_cast(NotKnow, State) ->
    {noreply, State}.

handle_call({send_data, all}, _, ok) ->
    {reply, com_util:dic_get(?SEND_SOCKETS_DIC, {0,0}), ok};

handle_call({recv_data, all}, _, ok) ->
    {reply, com_util:dic_get(?RECV_SOCKETS_DIC, {0,0}), ok};

handle_call(NotKnow, _, State) ->
    {reply, notknow, State}.

%% @doc 消毁
terminate(shutdown, _) ->
    ok;
terminate(normal, _) ->
    ok;
terminate(Reason, _) ->
    ?LAGER_ERROR("terminate reason ~p", [Reason]),
    ok.

%% @doc 新增连接
insert(Connection) ->
    ets:insert(?MODULE, Connection).

%% @doc 删除连接
delete(Socket) when is_port(Socket) ->
    ets:delete(?MODULE, Socket).

%% @doc 根据socket获取连接
-spec get_by_socket(port()) -> false | #connection{}.
get_by_socket(Socket) ->
    case ets:lookup(?MODULE, Socket) of
        [Conn|_] -> Conn;
        _ -> false
    end.
