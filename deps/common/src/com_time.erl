%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 五月 2015 下午4:23
%%%-------------------------------------------------------------------
-module(com_time).
-include_lib("common/include/common.hrl").
-include("com_time.hrl").


-define(DIFF_SECONDS_0000_1900, 62167219200).

-compile(export_all).

%% 此方法很准
localtime_to_seconds(DateTime) ->
    UDateTime = erlang:localtime_to_universaltime(DateTime, false),
    calendar:datetime_to_gregorian_seconds(UDateTime) - ?DIFF_SECONDS_0000_1900.

%%此方法与seconds_to_localtime()配套

%% 此方法只是一个马甲
localtime_to_seconds_ext(DateTime) ->
    localtime_to_seconds(DateTime).

localtime()->
    seconds_to_localtime(unixtime()).

%% -----------------------------------------------------------------
%% 判断是否同一天
%% -----------------------------------------------------------------
is_same_date(Seconds1, Seconds2) ->
    NDay = (Seconds1+28800) div 86400,	%% 28800秒是八小时
    ODay = (Seconds2+28800) div 86400,
    NDay=:=ODay.

datatime_offset_second(DateTime, DayOffset)->
    DaySeconds  = localtime_to_seconds_ext(DateTime),
    DaySeconds + DayOffset*?SECONDS_1_DAY.

datatime_offset_day(DateTime, DayOffset)->
    DaySeconds = datatime_offset_second(DateTime, DayOffset),
    seconds_to_localtime(DaySeconds).

time_offset_day(Time, DayOffset)->
    Time + DayOffset*?SECONDS_1_DAY.

utc_sec_now() ->
    calendar:time_to_seconds(utc_dt_now()).
utc_dt_now() ->
    calendar:universal_time().


%% 获取当天0点秒数
get_today_start() ->
    Now = unixtime(),
    Now-((Now+28800) rem 86400) .

%% 获取当前整点秒数
get_hour_start() ->
    Now = unixtime(),
    Now-((Now+28800) rem 3600) .

%% 获取本周开始的秒数
%% 此函数可能存在问题，非周一的时候，取值与get_today_start()相同，使用前请验证一下。
%% 可使用 get_week_both()方法
get_week_start() ->
    {ThisWeek,_NextWeek}= get_week_both(com_util:unixtime()),
    ThisWeek.

%% 获取下周开始的秒数
get_next_week_start() ->
    get_week_start() + (7*24*3600).

%% 获取本月开始的秒数
get_month_start() ->
    {Days, _Time} = calendar:seconds_to_daystime(com_util:unixtime() + ?DIFF_SECONDS_0000_1900),
    {Y,M,_D} = calendar:gregorian_days_to_date(Days),
    MonthStartDateTime = {{Y,M,1}, {0,0,0}},
    localtime_to_seconds(MonthStartDateTime).

ymd_to_unixtime({Y, M, D}) ->
    ymdhms_to_unixtime({{Y, M, D},{0,0,0}}).

ymdhms_to_unixtime({{Y,M,D},{H,Min,S}}) ->
    calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H,Min,S}}) - ?DIFF_SECONDS_0000_1900 - (8 * 3600).

%% 获取下一月开始的秒数

get_next_month_start() ->
    {{Y,M,_D}, _} = calendar:local_time(),
    {M1,Y1} =
        case M == 12 of
            true ->
                TempY1 = Y+1,
                TempM1 = 1,
                {TempY1,TempM1};
            false->
                TempY1 = Y,
                TempM1 = M + 1,
                {TempY1,TempM1}
        end,
    calendar:datetime_to_gregorian_seconds({{Y1, M1, 1}, {0, 0, 0}}) - ?DIFF_SECONDS_0000_1900 - (8 * 3600).

%% -----------------------------------------------------------------
%% 判断是否同一星期
%% -----------------------------------------------------------------
is_same_week(Seconds1, Seconds2) ->
    {{Year1, Month1, Day1}, Time1} = seconds_to_localtime(Seconds1),
    % 星期几
    Week1  = calendar:day_of_the_week(Year1, Month1, Day1),
    % 从午夜到现在的秒数
    Diff1  = calendar:time_to_seconds(Time1),
    Monday = Seconds1 - Diff1 - (Week1-1)*?SECONDS_1_DAY,
    Sunday = Seconds1 + (?SECONDS_1_DAY-Diff1) + (7-Week1)*?SECONDS_1_DAY,
    if ((Seconds2 >= Monday) and (Seconds2 < Sunday)) -> true;
        true -> false
    end.

%% -----------------------------------------------------------------
%% 判断是否同一月份
%% -----------------------------------------------------------------
is_same_month(Seconds1, Seconds2) ->
    {{Year1, Month1, _Day1}, _Time1} = seconds_to_localtime(Seconds1),
    {{Year2, Month2, _Day2}, _Time2} = seconds_to_localtime(Seconds2),

    if
        ((Year1 =:= Year2) and (Month1 =:= Month2)) -> true;
        true -> false
    end.

%%获取两个时间之间的描述
get_seconds_between_time(Time1, Time2)->
    S1 = calendar:datetime_to_gregorian_seconds(Time1),
    S2 = calendar:datetime_to_gregorian_seconds(Time2),
    S2-S1.

%% -----------------------------------------------------------------
%% 获取当天0点和第二天0点
%% -----------------------------------------------------------------
get_midnight_seconds(Seconds) ->
    {{_Year, _Month, _Day}, Time} = seconds_to_localtime(Seconds),
    % 从午夜到现在的秒数
    Diff   = calendar:time_to_seconds(Time),
    % 获取当天0点
    Today  = Seconds - Diff,
    % 获取第二天0点
    NextDay = Seconds + (?SECONDS_1_DAY-Diff),
    {Today, NextDay}.

%%获取指定时间的周一和下周一开始时间
get_week_both(Now) ->
    {T1, _} 		= get_midnight_seconds(Now),
    WeekDay	 		= get_date(Now),
    WeekTime 		= T1 - (WeekDay-1)*86400,
    NextWeekTime 	= WeekTime + 604800,
    {WeekTime, NextWeekTime}.


%% 获取下一天开始的时间
get_next_day_seconds(Now) ->
    {{_Year, _Month, _Day}, Time} = com_util:seconds_to_localtime(Now),
    % 从午夜到现在的秒数
    Diff = calendar:time_to_seconds(Time),
    Now + (?SECONDS_1_DAY - Diff).

get_next_day_seconds() ->
    get_next_day_seconds(com_util:unixtime()).

%% -----------------------------------------------------------------
%% 计算相差的天数
%% -----------------------------------------------------------------
get_diff_days(Seconds1, Seconds2) ->
    {{Year1, Month1, Day1}, _} = seconds_to_localtime(Seconds1),
    {{Year2, Month2, Day2}, _} = seconds_to_localtime(Seconds2),
    Days1 = calendar:date_to_gregorian_days(Year1, Month1, Day1),
    Days2 = calendar:date_to_gregorian_days(Year2, Month2, Day2),
    abs(Days2-Days1).
%%DiffDays=abs(Days2-Days1),
%%DiffDays + 1.

%% 获取从午夜到现在的秒数
get_today_current_second() ->
    {_, Time} = calendar:now_to_local_time(erlang:now()),
    NowSec = calendar:time_to_seconds(Time),
    NowSec.

%% 获取从现在到第二天午夜的秒数
get_next_midnight_second() ->
    Now = unixtime(),
    get_next_day_seconds(Now) - Now.

%%判断今天星期几
get_date() ->
    calendar:day_of_the_week(date()).

get_date(Now) ->
    {{Year, Month, Day}, _} = seconds_to_localtime(Now),
    calendar:day_of_the_week(Year, Month, Day).


%%获取上一周的开始时间和结束时间
get_pre_week_duringtime() ->
    OrealTime =  calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    {Year,Month,Day} = date(),
    CurrentTime = calendar:datetime_to_gregorian_seconds({{Year,Month,Day}, {0,0,0}})-OrealTime-8*60*60,%%从1970开始时间值
    WeekDay = calendar:day_of_the_week(Year,Month,Day),
    Day1 =
        case WeekDay of %%上周的时间
            1 -> 7;
            2 -> 7+1;
            3 -> 7+2;
            4 -> 7+3;
            5 -> 7+4;
            6 -> 7+5;
            7 -> 7+6
        end,
    StartTime = CurrentTime - Day1*24*60*60,
    EndTime = StartTime+7*24*60*60,
    {StartTime,EndTime}.

%%获取本周的开始时间和结束时间
get_this_week_duringtime() ->
    OrealTime =  calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    {Year,Month,Day} = date(),
    CurrentTime = calendar:datetime_to_gregorian_seconds({{Year,Month,Day}, {0,0,0}})-OrealTime-8*60*60,%%从1970开始时间值
    WeekDay = calendar:day_of_the_week(Year,Month,Day),
    Day1 =
        case WeekDay of %%上周的时间
            1 -> 0;
            2 -> 1;
            3 -> 2;
            4 -> 3;
            5 -> 4;
            6 -> 5;
            7 -> 6
        end,
    StartTime = CurrentTime - Day1*24*60*60,
    EndTime = StartTime+7*24*60*60,
    {StartTime,EndTime}.

%%获取当天日期，yyyymmdd格式
get_today() ->
    {{Year, Month, Day}, _} = calendar:local_time(),
    Year*10000+Month*100+Day.

now()->
    now(false).

now(true)->
    erlang:now();
now(false)->
    mod_timer:now().

%% @doc 取得当前的unix时间戳(sec)
unixtime()->
    unixtime(true).
unixtime(true) ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S;
unixtime(false) ->
    ?amc(mod_timer:now_seconds()).

%% @doc 获取当前unix时间戳(ms)
longunixtime()->
    longunixtime(true).
longunixtime2()->
    {M, S, Micro} = os:timestamp(),
    M * 1000000000 + S * 1000 + Micro div 1000.

longunixtime(true) ->
    {M, S, Micro} = erlang:now(),
    M * 1000000000 + S * 1000 + Micro div 1000;
longunixtime(false) ->
    mod_timer:now_milseconds().

%% 时间函数
%% -----------------------------------------------------------------
%% 根据1970年以来的秒数获得日期
%% -----------------------------------------------------------------
seconds_to_localtime(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds+?DIFF_SECONDS_0000_1900),
    calendar:universal_time_to_local_time(DateTime).

seconds_to_ymd(Seconds) ->
    {YMD,_HMS} = seconds_to_localtime(Seconds),
    YMD.

seconds_to_hms(Seconds) ->
    {_YMD,HMS} = seconds_to_localtime(Seconds),
    HMS.

seconds_to_hms_str(Seconds) ->
    {H,M,S} = seconds_to_hms(Seconds),
    lists:concat([string:right(tool:to_list(H), 2, 48),":",string:right(tool:to_list(M), 2, 48),":",string:right(tool:to_list(S), 2, 48)]).

seconds_to_ymd_str(Seconds) ->
    {YYYY,MM,DD} = seconds_to_ymd(Seconds),
    lists:concat([YYYY,"-",string:right(tool:to_list(MM), 2, 48),"-",string:right(tool:to_list(DD), 2, 48)]).

seconds_to_ymdhms_str(Seconds) ->
    lists:concat([seconds_to_ymd_str(Seconds), " ", seconds_to_hms_str(Seconds)]).

%%% ======================================================================
%%% API 导出接口
%%% ======================================================================

%% -----------------------------------------------------------------------
%% @doc return_2columns
%%
%% @spec 获得时间格式字符串 "2010-12-12 08:20:32"
%%
%% @end
%% -----------------------------------------------------------------------
return_2columns(X) ->
    case length(X) of
        1 ->
            "0" ++ X;
        _ ->
            X
    end.

%% -----------------------------------------------------------------------
%% @doc get_current_time
%%
%% @spec 获得时间格式字符串 "2010-12-12 08:20:32"
%%
%% @end
%% -----------------------------------------------------------------------
get_current_time() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
    L = lists:map(fun(X) ->
        X2=integer_to_list(X),
        return_2columns(X2)
    end,
        [Y, M, D, H, Mi, S]
    ),
    [Y2, M2, D2, H2, Mi2, S2] = L,
    Y2 ++ "-" ++ M2 ++ "-" ++ D2 ++ " " ++ H2 ++ ":" ++ Mi2 ++ ":" ++ S2.

%% -----------------------------------------------------------------------
%% @doc get_current_time_2
%%
%% @spec
%%      获得时间格式字符串 "2010_12_12_08"
%% @end
%% -----------------------------------------------------------------------
get_current_time_2() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:local_time(),
    L = lists:map(fun(X) ->
        X2=integer_to_list(X),
        return_2columns(X2)
    end,
        [Y, M, D, H, Mi, S]
    ),
    [Y2, M2, D2, H2, _, _] = L,
    Y2 ++ "_" ++ M2 ++ "_" ++ D2 ++ "_" ++ H2 .
