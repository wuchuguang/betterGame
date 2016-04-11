%%%-----------------------------------
%%% @doc @Module  : tcp_acceptor
%%%      @Created : 2015.05.05
%%% @end @Description: tcp acceptor
%%%-----------------------------------
-module(tcp_acceptor).
-behaviour(gen_server).

-export([get_name/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("common/include/common.hrl").
-record(state, {sock, ref,boot_args}).

%% @doc 不用的。ｆａｌｓｅ
get_name(_)->
    ?MODULE.
%% @hidden
init({LSock,BootArgs}) ->
    ?SYSTE_PROCESS(?true),
    gen_server:cast(self(), accept),
    {ok, #state{sock=LSock,boot_args = BootArgs}}.
%% @hidden
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
%% @hidden
handle_cast(accept, State) ->
    accept(State);
%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.
%% @hidden
handle_info({inet_async, LSock, Ref, {ok, Sock}}, State = #state{sock=LSock, ref=Ref,boot_args = BootArgs}) ->
    case set_sockopt(LSock, Sock) of
        ok -> ok;
        {error, Reason} ->
            ?LAGER_ERROR("inet_async ~p",[Reason]),
            exit({set_sockopt, Reason})
    end,
    start_client(Sock,BootArgs),
    accept(State);

handle_info({inet_async, LSock, Ref, {error, closed}}, State=#state{sock=LSock, ref=Ref}) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.
%% @hidden
terminate(_Reason, State) ->
    gen_tcp:close(State#state.sock),
    ok.
%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------私有函数--------------

set_sockopt(LSock, Sock) ->
    true = inet_db:register_socket(Sock, inet_tcp),
    case prim_inet:getopts(LSock, [active, nodelay, keepalive, priority, tos]) of
        {ok, Opts} ->
            Opts2 = [{delay_send,false} | Opts],
            case prim_inet:setopts(Sock, Opts2) of
                ok    -> ok;
                Error -> 
                    gen_tcp:close(Sock),
                    Error
            end;
        Error ->
            gen_tcp:close(Sock),
            Error
    end.


accept(State = #state{sock=LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> {noreply, State#state{ref=Ref}};
        Error     ->
            ?LAGER_ERROR("accept error ~p",[Error]),
            {stop, {cannot_accept, Error}, State}
    end.

%% @doc 开启客户端服务
start_client(Sock,BootArgs) ->
    ?LAGER_DEBUG("start_client"),
    {ok, Child} = Client = tcp_client_sup:start_child(BootArgs,Sock),
    ok = gen_tcp:controlling_process(Sock, Child),
    server_reader:recv(Child),
    Client.
