%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 五月 2015 下午1:54
%%%-------------------------------------------------------------------
-module(boot).

-include_lib("common/include/common.hrl").

-behaviour(supervisor).

-define(SUPERVISOR_TEMPLETE,?MODULE).

-define(GEN_SERVER, gen_server2).
-define(SUPERVISOR, supervisor2).

%%
%% @doc supervisor callback export
%%

-export([init/1]).

%%
%% @doc API export
%%
-export([child_count/1, stop/1, call/2, cast/2, state/1, start/1,start_child/2,listname/1]).

%%
%% callback
%%
-callback init(Param :: term())-> ChildList :: list().
-callback get_name(Param :: term())-> Name :: atom().
%%
%% API Functions
%%

call(PID, Msg) ->
    ?GEN_SERVER:call(PID, Msg).

cast(PID, Msg) ->
    ?GEN_SERVER:cast(PID, Msg).

child_count(SupName) ->
    ?SUPERVISOR:count_children(SupName).

stop(PID) ->
    ok.

state(PID) ->
    ?GEN_SERVER:get_state(PID).

listname(List) ->
    com_type:to_atom(string:join([com_type:to_list(Element) || Element<-List],"_")).

start(Param)->
    case catch do_boot(Param) of
        ?exit(R) ->
            throw({Param, 'EXIT',R});
        Ret -> Ret
    end.

do_boot(#boot{module = Module,params = Param, type = ?boot_type_supervisor,hasname = ?true})->
    ?SUPERVISOR:start_link({local,?amc(Module:get_name(Param),[{debug,?format("module ~p",[Module])}])},?SUPERVISOR_TEMPLETE,{Module,Param});
do_boot(#boot{module = Module,params = Param, type = ?boot_type_supervisor,hasname = ?false})->
    ?SUPERVISOR:start_link(?SUPERVISOR_TEMPLETE,{Module,Param});

do_boot(#boot{module = {simple_worker,Module}}=Boot)->
    do_boot(Boot#boot{module = Module});
do_boot(#boot{module = Module,params = Param, type = ?boot_type_worker,hasname = ?true})->
    GenServerTemplete = ?amc(boot_mod:mod(Module)),
    Name = {local,?amc(Module:get_name(Param))},
    ?GEN_SERVER:start_link(Name,GenServerTemplete,Param,[]);

do_boot(#boot{module = Module,params = Param, type = ?boot_type_worker,hasname = ?false})->
    GenServerTemplete = ?amc(boot_mod:mod(Module)),
    ?GEN_SERVER:start_link(GenServerTemplete,Param,[]);
do_boot(#boot{module = Module,params = Param, type = ?boot_type_simple_worker}) ->
    GenServerTemplete = ?amc(boot_mod:mod(Module)),
    ?GEN_SERVER:start_link(GenServerTemplete,Param,[]);
do_boot(Err) ->
    ?LOG_ERROR("do_boot err ~p",[Err]).

start_child(Father,Config)->
    supervisor:start_child(Father,get_child_spec(Config)).

%%
%% supervisor Functions
%%

init({Module,Param})->
    {Restart,ChildSpecList} = get_child_specs(?amc(Module:init(Param)),[]),
    {ok,{
        {Restart,3,10},
        ChildSpecList
    }}.


get_child_specs([],ChildSpecList)->
    {one_for_one,lists:reverse(ChildSpecList)};

get_child_specs([{Module,simple_worker}],_ChildSpecList)->
    {simple_one_for_one,[{Module,{?MODULE,start,[]},transient,brutal_kill,worker,[Module]}]};

get_child_specs([ #boot{module = Module,type = Type}= Config | T],ChildSpecList)->
    {RealType,Timeout} =
        case Type of
            ?boot_type_supervisor ->
                {supervisor,infinity};
            ?boot_type_worker ->
                {worker,100}
        end,
    Result = {make_ref(),{?MODULE,start,[Config]},transient,Timeout,RealType,[Module]},
    get_child_specs(T,[Result | ChildSpecList]).

get_child_spec(#boot{type = ?boot_type_worker,module = Module}=Boot)->
    {make_ref(),{?MODULE,start,[Boot]},temporary,100,worker,[Module]};

get_child_spec(#boot{type = ?boot_type_supervisor,module = Module}=Boot)->
    {make_ref(),{?MODULE,start,[Boot]},transient,infinity,supervisor,[Module]};

get_child_spec(#boot{type = ?boot_type_simple_worker}=Boot)->
    [Boot];

get_child_spec(Err) ->
    ?LOG_ERROR("get_child_spec notsupport Err ~p",[Err]).

