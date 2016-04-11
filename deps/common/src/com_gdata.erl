%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十一月 2015 下午4:32
%%%-------------------------------------------------------------------
-module(com_gdata).
-include("common.hrl").

-record(gdata, {tag , val, increment = fun(Last) -> Last + 1 end}).
-export([init/1, handle_call/3, terminate/2]).
%% API
-export([startup/1,next/1, increment/2, set/2, get/1]).

startup(Tag) ->
    Boot = #boot{module = ?MODULE, params = Tag, hasname = ?false, type = ?boot_type_worker},
    com_gdata_sup:start_child(Boot).

call(Tag, Msg) ->
    boot:call(tag2proc(Tag), Msg).

-define(PROC(__TAG), {?MODULE, __TAG}).
tag2proc(Tag) ->
    com_proc:register_get(?PROC(Tag)).

init(Tag) ->
    com_proc:register_new(?PROC(Tag), self()),
    {ok, #gdata{tag=Tag}}.

next(Tag) ->
    call(Tag, next).

increment(Tag, Fun) ->
    call(Tag, {set_increment, Fun}).

set(Tag, Val) ->
    call(Tag, {set, Val}).

get(Tag) ->
    call(Tag, get).


handle_call(next, _, Gdata) ->
    NewVal = (Gdata#gdata.increment)(Gdata#gdata.val),
    {reply, NewVal, Gdata#gdata{val = NewVal}};

handle_call({set_increment, Fun}, _, Gdata) when is_function(Fun, 1)->
    {reply, Gdata#gdata.increment, Gdata#gdata{increment = Fun}};

handle_call(get, _, Gdata) ->
    {reply, Gdata#gdata.val, Gdata};

handle_call({set, Val}, _, Gdata) ->
    {reply, Val, Gdata#gdata{val = Val}};

handle_call(_NotKnow, _, Gdata) ->
    {reply, notknow, Gdata}.

terminate(shutdown, _) ->
    com_proc:register_free(self()),
    ok;
terminate(normal, _) ->
    com_proc:register_free(self()),
    ok;
terminate(Reason, G) ->
    com_proc:register_free(self()),
    ?LAGER_ERROR("terminate reason ~p state ~p", [Reason, G]),
    ok.

