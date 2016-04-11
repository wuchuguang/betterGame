%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 五月 2015 下午4:37
%%%-------------------------------------------------------------------
-module(com_type).
-author("root").

%% API
-export([to_atom/1,to_list/1,to_binary/1,to_float/1,to_integer/1,to_bool/1,to_tuple/1]).

-export([to_erl/1]).

-export([type/1]).

type(Data) when is_atom(Data) -> atom;
type(Data) when is_integer(Data) -> integer;
type(Data) when is_binary(Data) -> binary;
type(Data) when is_list(Data) -> list.

to_erl(Str) ->
    case erl_scan:string(Str++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.


%% @doc convert other type to atom
to_atom(Msg) when is_atom(Msg) ->
    Msg;
to_atom(Msg) when is_binary(Msg) ->
    to_atom(binary_to_list(Msg));
to_atom(Msg) when is_list(Msg) ->
    case catch(list_to_existing_atom(Msg)) of
        {'EXIT', _} -> erlang:list_to_atom(Msg);
        Atom when is_atom(Atom) -> Atom
    end;
to_atom(_A) ->
    throw({other_value,_A}).  %%list_to_atom("").

%% @doc convert other type to list
to_list(Msg) when is_list(Msg) ->
    Msg;
to_list(Msg) when is_atom(Msg) ->
    atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) ->
    binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) ->
    integer_to_list(Msg);
to_list(Msg) when is_float(Msg) ->
    com_math:f2s(Msg);
to_list(Msg) when is_tuple(Msg) ->
    lists:concat(tuple_to_list(Msg));
to_list(Msg) when is_pid(Msg) ->
    pid_to_list(Msg);
to_list(_M) ->
    throw({other_value,_M}).

%% @doc convert other type to binary
to_binary(Msg) when is_binary(Msg) ->
    Msg;
to_binary(Msg) when is_atom(Msg) ->
    list_to_binary(atom_to_list(Msg));
%%atom_to_binary(Msg, utf8);
to_binary(Msg) when is_list(Msg) ->
    list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) ->
    list_to_binary(integer_to_list(Msg));
to_binary(Msg) when is_float(Msg) ->
    list_to_binary(com_math:f2s(Msg));
to_binary(_Msg) ->
    throw({other_value,_Msg}).

%% @doc convert other type to float
to_float(Msg)->
    Msg2 = to_list(Msg),
    list_to_float(Msg2).

%% @doc convert other type to integer
-spec to_integer(Msg :: any()) -> integer().
to_integer(Msg) when is_integer(Msg) ->
    Msg;
to_integer(Msg) when is_binary(Msg) ->
    Msg2 = binary_to_list(Msg),
    list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) ->
    list_to_integer(Msg);
to_integer(Msg) when is_float(Msg) ->
    round(Msg);
to_integer(Msg) when is_integer(Msg) ->
    Msg;
to_integer(_Msg) ->
    exit({other_value,_Msg}).

to_bool(D) when is_integer(D) ->
    D =/= 0;
to_bool(D) when is_list(D) ->
    length(D) =/= 0;
to_bool(D) when is_binary(D) ->
    to_bool(binary_to_list(D));
to_bool(D) when is_boolean(D) ->
    D;
to_bool(_D) ->
    throw({other_value,_D}).

%% @doc convert other type to tuple
to_tuple(T) when is_tuple(T) -> T;
to_tuple(T) -> {T}.

