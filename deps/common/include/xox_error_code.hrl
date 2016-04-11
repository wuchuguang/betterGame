    %%% ---
%%% @author by ybz
%%% @copyright © 2015-2016 深圳市客所思电子科技有限公司. All rights reserved.
%%% @doc 定义全局协议头文件
%%% 
%%% @end
%%% ---

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%%
%% 定义登录相关的错误消息
%% 区间［100010000 -- 100010300]
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++%%

-define(ERR_CODE_REGISTER_FAILED,               16#00008001).                %%  注册账号不成功
-define(ERR_CODE_APP2LOGIN_NOT_IMS_FOR_ASK,     16#00008002).                %%  没有可返回的ＩＭＳ服务器
-define(ERR_CODE_NOT_FIND_ACCOUNT,              16#00008003).                %%  没有找到这个用户信息


%% 远程桌面服务器错误消息号
-define(ERR_CODE_SESSIONKEY_NOT_MATCH,              16#00008004).            %%  房间SESSIONKEY不匹配
-define(ERR_CODE_SESSIONKEY_NOT_FIND_ROOMID,        16#00008005).            %%  没找到房间ID

-define(ERR_CODE_SAV_LOGIN_SESSION_FAIL,            16#00020000).            %% 登录Session无法验证
-define(ERR_CODE_SAV_LOGIN_SESSION_TIMEOUT,         16#00020001).            %% 登录Session过期
-define(ERR_CODE_SAV_LOGIN_SESSION_INVALID,         16#00020002).            %% 登录Session无效
-define(ERR_CODE_SAV_LOGIN_SESSION_OVERTIME,        16#00020003).            %% 登录Session超时
-define(ERR_CODE_SAV_REG_EXIST,                     16#00020004).            %% 注册失败，已注册过了
-define(ERR_CODE_SAV_NOT_EXIST,                     16#00020005).            %% 登录失败，非法入侵