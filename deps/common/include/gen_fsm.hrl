%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 六月 2015 下午3:25
%%%-------------------------------------------------------------------
-author("root").


-define(next_state(NStateName, NStateData), {next_state, NStateName, NStateData}).

-define(next_state(NStateName, NStateData, Time1), {next_state, NStateName, NStateData, Time1}).

-define(init_ok(StateName, StateData), {ok, StateName, StateData}).