%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 五月 2015 下午6:41
%%%-------------------------------------------------------------------
-export([
    onConnect/1,
    onMessage/2,
    send_packet/1,
    handle_loop/2,
    terminate/1,
    not_support_stream/1,
    onClose/1,
    is_timeout/1,
    heart_timeout/1]).