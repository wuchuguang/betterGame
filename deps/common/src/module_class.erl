%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十二月 2015 下午12:00
%%%-------------------------------------------------------------------
-module(module_class).
-include("meta.hrl").
-include("module_class_def.hrl").

%% API
-export([parse_transform/2]).

-record(class, {name, file, extends, implement, private=[], protected=[], public=[], final=[], abstract=[]}).

parse_transform(Forms, Opts) ->
    io:format("old forms ~p~n", [Forms]),
    Fun =
        fun(#attribute{name = module,value = Name},{Attrs,Class}) ->
            {Attrs,Class#class{name = Name}};
            (#attribute{name = file,value = File}, {Attrs, Class}) ->
                {Attrs, Class#class{file = File}};
            (#attribute{name = ?TYPE,value = ?EXTENDS(Extends)}=Attr, {Attrs, Class}) ->
                {[?EXTENDS(Extends)|Attrs],Class};
            (#attribute{name = ?TYPE}=Attr, {Attrs, Class}) ->
                attribute(Attr, Attrs, Class);
            (#function{}=Func, {Attrs,Class}) ->
                function(Func, Attrs,Class);
            ({eof,_}, {_Attrs,Class}) ->
                Class
        end,
    NewForms = lists:foldl(Fun, {[],#class{}}, Forms),
    io:format("new forms ~p~n", [NewForms]),
    NewForms.


attribute(#attribute{name = ?TYPE,value = ?PRIVATE}, Attrs,Class) ->
    {[{ppp,?PRIVATE}|Attrs], Class};
attribute(#attribute{name = ?TYPE,value = ?PROTECTED}, Attrs,Class) ->
    {[{ppp,?PROTECTED}|Attrs],Class};
attribute(#attribute{name = ?TYPE,value = ?PUBLIC}, Attrs,Class) ->
    {[{ppp,?PUBLIC}|Attrs],Class};
attribute(#attribute{name = ?TYPE,value = ?FINAL},Attrs, Class) ->
    {[{af,?FINAL}|Attrs], Class};
attribute(#attribute{name = ?TYPE,value = ?ABSTRACT},Attrs, Class) ->
    {[{af,?ABSTRACT}|Attrs], Class};
attribute(#attribute{name = ?TYPE,value = ?OVERRIDE},Attrs, Class) ->
    {[override|Attrs], Class}.

function(Func, Attrs, Class) ->
    io:format("func ~p attrs ~p~n", [Func, Attrs]),
    Extends = proplists:get_value(extends, Attrs, object),
    Override = proplists:get_value(?OVERRIDE, Attrs, false),
    PPP      = proplists:get_value(ppp, Attrs, ?PUBLIC),
    AF       = proplists:get_value(af, Attrs, ?RELOAD),
    case Override of
        true ->
            Extends
    end,
    {[{extends,Extends}], Class}.

class2form(Class) ->
    M0 = meta:new(Class#class.name),
    M1 = meta:add_funcs(M0,Class#class.private, false),
    M2 = meta:add_funcs(M1,Class#class.protected,true),
    M3 = meta:add_funcs(M2,Class#class.public, true),
    MFinal = M3,
    meta:to_forms(MFinal).