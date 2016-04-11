%%%-----------------------------------
%%% @Module  : tcp_listener_sup
%%% @Created : 2015.05.05
%%% @deprecated:
%%%     tcp listerner 监控树
%%%-----------------------------------
-module(tcp_listener_sup).
-include_lib("common/include/common.hrl").
-include_lib("network/include/network.hrl").


-export([get_name/1]).

-export([init/1]).
%% @hidden
get_name(#network_boot{port = Port,module = M})->
    if
        Port < 0 ->
            exit({listener_port_err, Port});
        Port > 65535 ->
            exit({listener_port_err, Port});
        Port == ?undefined->
            exit({listener_port_err, Port});
        not is_integer(Port) ->
            exit({listener_port_err, Port});
        true ->
            ok
    end,
    if
        M == ?undefined ->
            exit({listener_module_err, M});
        not is_atom(M) ->
            exit({listener_module_err, M});
        true ->
            ok
    end,
    Name = ?catch_exp(boot:listname([tcp_listener_sup,Port])),
    case com_proc:exist(Name) of
        true ->
            exit({listener_port_useed, Port});
        false->
            Name
    end.

%% @hidden
init(BootArgs) ->
    [
        #boot{module = tcp_acceptor_sup,type = ?boot_type_supervisor,params = BootArgs},
        #boot{module = tcp_listener,type = ?boot_type_worker,params = BootArgs},
        #boot{module = tcp_client_sup,type = ?boot_type_supervisor,params = BootArgs}
    ].    
