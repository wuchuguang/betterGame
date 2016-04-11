%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 十二月 2015 下午2:20
%%%-------------------------------------------------------------------
-define(TYPE, class_attribute).
-define(QUALIFIER(__QUALIFIER), -?TYPE(__QUALIFIER)).
-define(PRIVATE, private).
-define(PROTECTED, protected).
-define(PUBLIC, public).
-define(FINAL, final).
-define(RELOAD, reload).
-define(ABSTRACT, abstract).
-define(EXTENDS(__CLASS), {extends, __CLASS}).
-define(OVERRIDE, override).