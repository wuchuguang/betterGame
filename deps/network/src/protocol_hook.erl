%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 六月 2015 上午9:43
%%%-------------------------------------------------------------------
-module(protocol_hook).
-author("root").
-include_lib("common/include/common.hrl").

%% API
-export([init/0, close/0, add/2, add/3, run/2]).


init() ->
    ets:new(?MODULE, [set, public, named_table, {keypos,1}]).

close() ->
    ets:delete(?MODULE).

add(Cmd, ProcessMFA) ->
    ets:insert(?MODULE, {Cmd,ProcessMFA}).

add(Cmd, ProcessMFA, Proc) ->
    ets:insert(?MODULE, {Cmd, ProcessMFA, Proc}).

run(Cmd, Data) ->
    case ets:lookup(?MODULE, Cmd) of
        [] ->
            ok;
        [{_,MFA}] ->
            format_error(catch exec(MFA,Data), MFA, Cmd, Data);
        [{_,MFA,Proc}] ->
            Fun =
            fun() ->
                format_error(catch exec(MFA,Data), MFA, Cmd, Data)
            end,
            gen_server:cast(Proc, Fun)
    end.

exec({Module,Function}, Data)  when is_atom(Module), is_atom(Function) ->
    Module:Function(Data),true;
exec({Module,Function,ExtArgs}, Data) when is_atom(Module),is_atom(Function),is_list(ExtArgs) ->
    apply(Module, Function, [Data|ExtArgs]),true;
exec(Function, _Data) when is_function(Function,0) ->
    Function(),true;
exec(Function, Data) when is_function(Function,1) ->
    Function(Data),true;
exec(_NotMatch, _Data) ->
    false.


format_error(false, MFA, Cmd, _Data) ->
    ?LOG_ERROR("mfa ~p cmd ~p not run",[MFA, Cmd]);
format_error({'EXIT',Reason},MFA,Cmd,_) ->
    ?LOG_ERROR("mfa ~p cmd ~p reason ~p stacktrace ~p", [MFA, Cmd, Reason, erlang:get_stacktrace()]);
format_error(_,_,_,_) ->
    ok.
