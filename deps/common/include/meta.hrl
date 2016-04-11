%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 五月 2015 下午7:35
%%%-------------------------------------------------------------------
-ifndef(__META__HRL__).
-define(__META__HRL__, true).
-define(parse_transform(__MODULE), -compile({parse_transform, __MODULE})).

-record(attribute, {line::integer(), name::atom(), value::any()}).

-record(function, {line::integer(), name::any(), args_count::any(), clauses::any()}).

-record(clause, {line::integer(), args::any(), guard::any(), body::any()}).

-record(tuple, {line::integer(), elements::any()}).

-record(atom, {line::integer(), value::any()}).

-record(var, {line::integer(), value::any()}).

-record(integer, {line::integer(), value::any()}).

-record(meta_mod, {module::atom(),
                   file::list(),
                   includes = []::list(),
                   exports = []::list(),
                   records = []::list(),
                   attributes=[]::list(),
                   forms = []::list(),
                   eof={eof, 999}::{eof,integer()},
                   export_all = false::boolean()}).
-endif.