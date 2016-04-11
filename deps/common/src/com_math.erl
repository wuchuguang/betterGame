%%%-------------------------------------------------------------------
%%% @author root
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 五月 2015 下午4:25
%%%-------------------------------------------------------------------
-module(com_math).
-include_lib("common/include/common.hrl").
-include("com_math.hrl").

-export([lnx/1,f2s/1,round/1,ceil/1, pagecount/2, rffi/1, random/2,random_chars/2]).

lnx(X) ->
    math:log10(X) / math:log10(?E).

%% @doc convert float to string,  f2s(1.5678) -> 1.57
f2s(N) when is_integer(N) ->
    integer_to_list(N) ++ ".00";
f2s(F) when is_float(F) ->
    [A] = io_lib:format("~.2f", [F]),
    A.

%% 向下取整
round(Float) when erlang:is_float(Float)->
    trunc(Float);
round(Int) ->
    Int.

%% 向上取整
ceil(Float) when erlang:is_float(Float)->
    trunc(Float)+1;
ceil(Int) ->
    Int.

%% Room four five into  四舍五入
rffi(Float)when erlang:is_float(Float) ->
    Int = trunc(Float),
    case Float+0.5 >= Int of
        true -> Int+1;
        false-> Int
    end;
rffi(Int) ->
    Int.

pagecount(All, Page) ->
    case All rem Page of
        0 -> All div Page;
        _ -> All div Page + 1
    end.


%% 生成随机数字
random(Max,Max) ->
    Max;
random(Min,Max) when Min < Max ->
    Min+random:uniform(Max-Min);
random(Max,Min) ->
    random(Min,Max).

-define(lowercase, ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O",
"P","Q","R","S","T","U","V","W","X","J","Z"]).
-define(uppercase, ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o",
"p","q","r","s","t","u","v","w","x","j","z"]).
-define(number,["0","1","2","3","4","5","6","7","8","9"]).
random_chars(CharsCount, Options) ->
    ?catch_exp(random_chars_(CharsCount, Options)).
random_chars_(CharsCount, Options) ->
    Types = proplists:get_value(type,Options,[lowercase,uppercase,number]),
    TypesFun =
    fun(lowercase,Chars) ->
           ?lowercase++Chars;
       (uppercase,Chars) ->
           ?uppercase++Chars;
       (number,Chars) ->
           ?number++Chars
    end,
    AllChars = lists:foldl(TypesFun, [], Types),
    AllowRepetition = proplists:get_value(repeat,Options, true),
    ?ifdo(AllowRepetition orelse length(AllChars)>=CharsCount,
    ?return(random_chars_(CharsCount,AllChars,AllowRepetition))),
    ?return({error,chars_not_full}).

random_chars_(CharsCount,Chars,AllowRepetition) ->
    Fun = fun(_,{Ret,RemainChars})->
        Nth = random(1,length(RemainChars)),
        ?ifdo_else(
            AllowRepetition,
            {[lists:nth(Nth,RemainChars)|Ret],RemainChars},
            begin
                {Ele,RemainChars2} = extact_element(Nth,RemainChars),
                {[Ele|Ret],RemainChars2}
            end
        )
        end,
    {Ret, _} = lists:foldl(Fun,{[],Chars},lists:seq(1,CharsCount)),
    lists:flatten(Ret).

extact_element(Nth,List) ->
    extact_element_(Nth,List,[],0).

extact_element_(Nth,[Ele|List],HList,Nth) ->
    {Ele,lists:reverse(HList)++List};
extact_element_(Nth,[Ele|List],HList,Las) ->
    extact_element_(Nth,List,[Ele|HList],Las+1).
