%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 六月 2015 下午1:47
%%%-------------------------------------------------------------------
-module(stat_db).
-include_lib("common/include/common.hrl").
-author("root").

%% API
-export([startup/1, init/1, get_name/1, record/2]).

-export([print_table/1, print_event/2]).

-record(table_event, {name, select=0, insert = 0, update = 0, delete = 0, time = com_time:unixtime()}).

startup(PID) ->
    boot:start_child(PID, #boot{module = ?MODULE,type = ?boot_type_worker,params = {}}).

get_name(_) ->
    ?MODULE.

init(_) ->
    ets:new(?MODULE, [public, set, named_table, {keypos,1}]),
    {ok, ok}.

record(Table, Event) ->
    try
        EventPos = event_position(Event),
        ets:update_counter(?MODULE, Table, {EventPos, 1})
    catch
        _:_ ->
            NewData = event_position_set(#table_event{name = Table}, Event, 1),
            ets:insert(?MODULE, NewData)
    end.

print_table(Table) ->
    case ets:lookup(?MODULE, Table) of
        [] ->
            ?LAGER_DEBUG("table ~p select ~p insert ~p update ~p delete ~p", [Table, 0, 0, 0, 0]),
            ok;
        [Value] ->
            ?LAGER_DEBUG("table ~p select ~p insert ~p update ~p delete ~p", [Table,
                                                                            Value#table_event.select,
                                                                            Value#table_event.insert,
                                                                            Value#table_event.update,
                                                                            Value#table_event.delete])
    end.

print_event(Event, Options) ->
    Fun =
    fun(Table, List) ->
        Value = event_value(Table, Event),
        [{Table#table_event.name,Value}|List]
    end,
    Values = lists:foldl(Fun, [], ets:tab2list(?MODULE)),
    Sort =
    fun({_,Small},{_,Big}) ->
        Small > Big
    end,
    lists:sort(Sort, Values).

event_position(select) ->
    #table_event.select;
event_position(insert) ->
    #table_event.insert;
event_position(update) ->
    #table_event.update;
event_position(delete) ->
    #table_event.delete.

event_value(Tuple, Event) ->
    erlang:element(event_position(Event),Tuple).

event_position_set(Tuple, Event, Value) ->
    erlang:setelement(event_position(Event),Tuple, Value).