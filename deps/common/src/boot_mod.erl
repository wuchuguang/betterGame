%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 五月 2015 下午2:29
%%%-------------------------------------------------------------------
-module(boot_mod).

-include_lib("common/include/common.hrl").
-include_lib("common/include/task.hrl").

-export([mod/1]).

-define(BOOT_MOD(__Module),{?MODULE,__Module}).
%%
%% gen_server export
%%
-export([module/1,init/2, handle_call/4, handle_cast/3, handle_info/3, terminate/3, code_change/4]).
%%
%% Debug export
%%

-callback get_name(Param :: term())-> Name::atom().

-callback init(Param ::term())->
    {ok, State::tuple()} | {stop,Reason::term()}.
-callback handle_call(Request::term(),From::term(),State::tuple())->
    {reply, Reply::term(), NewState::tuple()} |
    {noreply, NewState::tuple()} |
    {stop, Reason::term(), State::tuple()}.
-callback handle_cast(Msg::term(),State::tuple())->
    {noreply, NewState::tuple()} |
    {stop, Reason::term(), NewState::tuple()}.
-callback handle_info(Msg::term(),State::tuple())->
    {noreply, NewState::tuple()} |
    {stop, Reason::term(), NewState::tuple()}.
-callback terminate(Reason::term(),State::tuple())->
    Any ::term().


mod(Module)->
    ?BOOT_MOD(Module).

module(?BOOT_MOD(Module)) ->
    Module;
module(NotMatch) ->
    ?LOG_ERROR("module(~p) erorr ~p ",[NotMatch,erlang:get_stacktrace()]),
    error({not_match,NotMatch}).


%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Param,?BOOT_MOD(Module))->
    random:seed(now()),
    try
        ?amc(Module:init(Param))
    catch _:Reason ->
        ?LOG_ERROR("init error Module(~p) Reason(~p) stacktrace(~p)",[Module,Reason, erlang:get_stacktrace()]),
        {stop,Reason}
    end.



%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({'apply',ResponseFun},From,State,?BOOT_MOD(Module)) ->
    try
        case ResponseFun(State) of
            {ok,Reply} ->
                {reply,Reply,State};
            {ok,Reply,NewState} ->
                {reply,Reply,NewState};
            {error,_ErrReason} = Error ->
                {reply,Error,State}
        end
    catch
        _:Reason ->
            ?LOG_ERROR("Module(~p)Function(handle_call)Msg(~p)From(~p)Reason(~p)",[Module,{'apply',ResponseFun}, From,Reason]),
            {reply, ok, State}
    end;

handle_call('i',_From,State,?BOOT_MOD(_Module)) ->
    {reply, State, State};
handle_call(Request, From, State,?BOOT_MOD(Module)) ->
    try
        Module:handle_call(Request,From,State)
    catch
        _:Reason ->
            ?LOG_ERROR("Module(~p)Function(handle_call)From(~p)Reason(~p) stacktrace(~p)",[Module,From,Reason,erlang:get_stacktrace()]),
            {reply, ok, State}
    end.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State,?BOOT_MOD(Module)) ->
    try
        Module:handle_cast(Msg, State)
    catch
        _:Reason ->
            ?LOG_ERROR("Module(~p)Function(handle_cast)Reason(~p) stackState(~p)",[Module,Reason,erlang:get_stacktrace()]),
            {noreply, State}
    end.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'asyn_apply',ResponseFun,From,OnResponseFun},State,?BOOT_MOD(_Module))->
    try
        case ResponseFun(State) of
            {ok,Reply} ->
                From ! {'asyn_apply_reply',OnResponseFun,Reply},
                {noreply,State};
            {ok,Reply,NewState} ->
                From ! {'asyn_apply_reply',OnResponseFun,Reply},
                {noreply,NewState};
            {error,_ErrReason} ->
                {noreply,State}
        end
    catch

        _:_Reason ->
            {noreply, State}
    end;

handle_info({'asyn_apply_reply',OnResponseFun,Reply},State,?BOOT_MOD(_Module))->
    try
        case OnResponseFun(Reply,State) of
            ok ->
                {noreply,State};
            {ok,NewState} ->
                {noreply,NewState};
            {error,_ErrReason} ->
                {noreply,State}
        end
    catch

        _:_Reason ->
            {noreply, State}
    end;

handle_info({'asyn_run',OnAsynRunFun},State,?BOOT_MOD(Module))->
    try
        case OnAsynRunFun(State) of
            ok ->
                {noreply,State};
            {ok,NewState} ->
                {noreply,NewState};
            {error,_ErrReason} ->
                {noreply,State}
        end
    catch

        _:Reason ->
            ?LOG_ERROR("Module(~p)Function(handle_info)Msg(~p)Reason(~p)",[Module,{'asyn_run',OnAsynRunFun}, Reason]),
            {noreply, State}
    end;

handle_info(?task_hook(Task), State,?BOOT_MOD(Module)) ->
    ?amc(task:exec_time(Task), [{undef, ?format("Module ~p undefined",[Module])}]),
    {noreply, State};

handle_info({msg, FromModule, FromLine, MSG}, State, ?BOOT_MOD(Module)) ->
    ?amc(Module:handle_msg(MSG, State ),
         [{undef,?format("~p:handle_msg/2 not defined",[Module])},
          {error,?format("msg FromModule(~p) FromLine(~p) Msg(~p)",[FromModule,FromLine,MSG])}]);


handle_info(Info, State,?BOOT_MOD(Module)) ->
    try
        Module:handle_info(Info, State)
    catch
        _:Reason ->
            ?LAGER_ERROR("handle_info error ~p ~p ~p ~p",[Module,State, Reason,erlang:get_stacktrace()]),
            {noreply, State}
    end.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(StopReason, State,?BOOT_MOD(Module)) ->
    ?LOG_ERROR("Module ~p terminate ~p",[Module, StopReason]),
    try
        Module:terminate(StopReason, State)
    catch
        _:Reason ->
            ?LAGER_DEBUG("Module ~p terminate ~p stacktrace ~p",[Module, Reason,erlang:get_stacktrace()]),
            ok
    end.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra,?BOOT_MOD(_Module)) ->
    {ok, State}.

