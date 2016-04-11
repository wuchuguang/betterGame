%%% ---
%%% @author by ybz
%%% @copyright © 2015-2016 深圳市客所思电子科技有限公司. All rights reserved.
%%% @doc 定义全局协议头文件
%%% 
%%% @end
%%% ---

-record(xox_protocol, {cmd=0::integer(),flag::any(),bodys::any(),checkflag::boolean()}).


%% 定义登录相关的协议
-define (WEB2LOGIN_REGISTER_ACCOUNT_REQUEST,      4097).      %% (web平台向Login服务器请求注册XOX-IM账号)
-define (WEB2LOGIN_REGISTER_ACCOUNT_REQUEST_ASK,  4098).      %% (web平台向Login服务器请求注册XOX-IM账号的回复)
-define (LOGIN2IMS_REGISTER_ACCOUNT_REQUEST ,     4099).      %% (Login服务器向IMS服务器请求注册XOX-IM账号)
-define (LOGIN2IMS_REGISTER_ACCOUNT_REQUEST_ASK , 4100).      %% (Login服务器向IMS服务器请求注册XOX-IM账号的回复)
-define (IMS2LOGIN_SERVER_START_REQUEST,          4101).      %% (IMS服务器向Login服务器通知IMS服务启动) (IMS服务器的IP+PORT,LOGIN服务器可通过SOCKET连接获取)
-define (IMS2LOGIN_SERVER_START_REQUEST_ASK,      4102).      %% (IMS服务器向Login服务器通知IMS服务启动的回复)
-define (LOGIN2WEB_SERVER_START_REQUEST,          4103).      %% (LOGIN服务器向WEB服务器通知LOGIN服务启动)  (LOGIN服务器的IP+PORT,WEB平台可通过SOCKET连接获取)
-define (LOGIN2WEB_SERVER_START_REQUEST_ASK,      4104).      %% (LOGIN服务器向WEB服务器通知LOGIN服务启动的回复)
-define (LOGIN2WEB_IMSSERVER_START_REQUEST,       4105).      %% (LOGIN服务器向WEB服务器通知IMS服务启动)
-define (LOGIN2WEB_IMSSERVER_START_REQUEST_ASK,   4106).      %% (LOGIN服务器向WEB服务器通知IMS服务启动的回复)
-define (CLIENT2WEB_LOGIN_REQUES,                 4107).      %% (客户端向WEB服务器请求LOGIN服务器信息)
-define (CLIENT2WEB_LOGIN_REQUEST_ASK,            4108).      %% (客户端向WEB服务器请求LOGIN服务器信息的回复)
-define (CLIENT2LOGIN_LOGIN_REQUEST,              4109).      %% (客户端向登录服务器请求分配IMS服务器信息)
-define (CLIENT2LOGIN_LOGIN_REQUEST_ASK,          4110).      %% (客户端向登录服务器请求分配IMS服务器信息的回复)
-define (IMS2LOGIN_INFO_NOTIFY ,                4113).      %% (IMS服务器向LOGIN服务器定时通知IMS服务器当前在线人数)
-define (LOGIN2IMS_CHECK_EJABBERD_REQUEST,        4114).      %% (登录服务器向IM服务器请求Ejabber账号密码)
-define (LOGIN2IMS_CHECK_EJABBERD_REQUEST_ASK,    4115).      %% (登录服务器向IM服务器请求Ejabber账号密码的回复)
-define(IMS2LOGIN_MUC_SYNC, 5012). %% IM服务器向它的Login发送同步ＭＵＣ（多人聊天：群）数据（新群，群成员数量，删群，改名）
-define(IMS2WEB, 6000).
-define(IMS2WEB_ASK, 6001).

