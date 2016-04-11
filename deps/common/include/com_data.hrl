%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 六月 2015 下午3:37
%%%-------------------------------------------------------------------
-author("root").


-record(data_fshandle, {handle::any(), ver::any(), sessionKey::any(), imno::any(), is_use::any()}).

-record(data_roster_status, {host_user::any(), ver=0::any(), status::any()}).