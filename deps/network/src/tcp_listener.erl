%%%-----------------------------------
%%% @Module  : tcp_listener
%%% @Created : 2015.05.05
%%% @Description: tcp listerner监听
%%%-----------------------------------
-module(tcp_listener).
-include_lib("common/include/common.hrl").
-include_lib("network/include/network.hrl").
-behaviour(gen_server).
-export([start_link/4,get_name/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).

%% @hidden
get_name(#network_boot{port = Port})->
    boot:listname([?MODULE,Port]).

%% @doc 启动监听进程
start_link(AcceptorCount, IP,Port,Module) ->
    gen_server:start_link(?MODULE, {AcceptorCount, IP,Port,Module}, []).

%% @hidden
init(#network_boot{
    acceptor_count = AcceptorCount,
    ip=IP,port = Port,module = Module} = BootArgs) ->
    process_flag(trap_exit, true),
    FinalTcpOptions = fix_opts_ip(IP, ?TCP_OPTIONS),
    ?LAGER_DEBUG("listen ~p ~p", [Port, FinalTcpOptions]),
    case catch gen_tcp:listen(Port, FinalTcpOptions) of
        {ok, LSock} ->
            lists:foreach(
                fun (_) ->
                    {ok, _APID} =
                    tcp_acceptor_sup:start_child(LSock,BootArgs)
                end,
                lists:duplicate(AcceptorCount, dummy)),
            ?LAGER_DEBUG("Module ~p listen:~p",[Module, Port]),
            {ok, LSock};
        Error ->
            ?LOG_ERROR("cannot listen ~p error ~p stackstrace ~p", [FinalTcpOptions, Error,erlang:get_stacktrace()]),
            {stop, {cannot_listen, Error}}
    end.

%% @hidden
handle_call(_Request, _From, State) ->
    {reply, State, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.
%% @hidden
terminate(_Reason, State) ->
    %{ok, {IPAddress, Port}} = inet:sockname(State),
    gen_tcp:close(State),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

fix_opts_ip(IP, Opts) ->
    case IP of
        undefined->
            Opts;
        IP when is_tuple(IP)->
            [{ip,IP}|Opts];
        IP when is_list(IP) ->
            [{ip,com_socket:str2ip(IP)}|Opts]
    end.