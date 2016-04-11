%%%-----------------------------------
%%% @Module  : util
%%% @Created : 2010.10.05
%%% @Description: 公共函数
%%%-----------------------------------
-module(com_util).
-include_lib("common/include/common.hrl").
%%-compile(export_all).
-export([
	after_do/2,
	new_call/1,
	ifttt/3,
	make_sure/1,
	while/2,
	implode/2,
	explode/2,
	explode/3,
	list_page/3,
	single_page/2,
	classify_list/2,
	short_run_list/3,
	short_foldl_list/3,
	list_sub_elem/2,
	right_string/3,
	left_string/3,
	seq/2,
	seq2/2,
	mixture/2,
	thing_to_list/1,
	log/5,
	md5/1,
	rand/1,
	rand/2,
	rand_real_time/2,
	random_list/1,
	random_list2/1,
	random_delete/1,
	random_delete/2,
	random_true/3,
	get_random_list/2,
	operation_div/2,
	ceil/1,
	ceil/2,
	floor/1,
	rffi/1,
	decimal/2,
	decimal/3,
	sleep/1,
	sleep/2,
	get_list/2,
	list_deletex/2,
	list_elementx/2,
	upset_list/1,
	upset_list/2,
	upset_list/3,
	extract_member/2,
	extract_member/3,
	list_to_tuple/2,
	list_to_tuple/3,
	repeat_count/2,
	repeat_count/3,
	remove_nth/2,
	remove_element/2,
	remove_repeat/1,
	remove_repeat/2,
	combination/2,
	cumsum/2,
	c/2,
	map/3,
	for/3,
	for/4,
	for_new/4,
	for2/2,
	for2/3,
	ele_tail/2,
	term_to_bitstring/1,
	term_to_string/1,
	string_to_term/1,
	list_to_string/1,
	bitstring_to_term/1,
	run_string/1,
	find_elements/4,
	check_same_day/1,
	filter_list/3,
	filter_replicat/2,
	get_chinese_count/1,
	list2utfstring/1,
	lists_nth/2,
	lists_nth_replace/3,
	recover/2,
	get_pos_num/1,
	get_pos_num2/1,
	get_max_num/2,
	make_sure_list/2,
	compile_base_data/3,
	register_fun/3,
	execute_registered_fun/1,
	get_day_start/1,
	to_record/2,
	find_pos/2,
	find_pos_temp/4,
	element_operate/3,
	element_value/3,
	get_tuple_data_list/3,
	print_record/2,
	power/2,
	power/3,
	ternary_operator/3,
	point_to_line/1,
	point_to_line/3,
	next_point/2,
	get_intersection_list/2,
	get_unite_list/2,
	get_subtract_list/2,
	get_data_type/1,
	dic_get/1,
	dic_get/2,
	dic_set/2,
	dic_erase/1,
	check/1,
	make_ticket/4,
	merge/1,
	concat/2
]).


%%汉字unicode编码范围 0x4e00 - 0x9fa5
-define(UNICODE_CHINESE_BEGIN, (4*16*16*16 + 14*16*16)).
-define(UNICODE_CHINESE_END,   (9*16*16*16 + 15*16*16 + 10*16 + 5)).

-define(DIFF_SECONDS_0000_1900, 62167219200).

ifttt(This, Then, That) ->
	case This of
		true -> Then;
		false-> That
	end.

make_sure(Fun) when is_function(Fun) ->
	Fun();
make_sure({M,F,A}) ->
	apply(M, F, A).

while(Fun,InitData) ->
	case Fun(InitData) of
		?break->InitData;
		?break(Break) -> Break;
		?continue->
			while(Fun,InitData);
		?continue(Continue) ->
			while(Fun,Continue)
	end.


%% 在List中的每两个元素之间插入一个分隔符
implode(_S, [])->
	[<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [thing_to_list(H) | NList],
    implode(S, T, [S | L]).

%% 字符->列
explode(S, B)->
    re:split(B, S, [{return, list}]).
explode(S, B, int) ->
    [list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

classify_list(Fun, List) when is_function(Fun, 1)->
	InnerFun = fun(Element, Result) ->
					   Key = Fun(Element),
					   case lists:keytake(Key, 1, Result) of
						   false->
							   [{Key,Element}|Result];
						   {value,{Key,Elements},RemainResult} ->
							   [{Key,[Element|Elements]}|RemainResult]
					   end
			   end,
	lists:foldl(InnerFun, [], List);
classify_list(Position, List) ->
	Fun = fun(Element, Result) ->
				  Key = element(Position, Element),
				  case lists:keytake(Key, 1, Result) of
					  false->
						  [{Key,[Element]}|Result];
					  {value,{Key,Elements},RemainResult} ->
						  [{Key,[Element|Elements]}|RemainResult]
				  end
		  end,
	lists:foldl(Fun, [], List).

short_run_list(_,[],R) ->
	R;
short_run_list(Fun, [H|L], R) ->
	case Fun(H) of
		{continue,NewR} ->
			short_run_list(Fun, L, NewR);
		{break, NewR} ->
			NewR;
		NewR when R =/= NewR -> NewR;
		_ ->
			short_run_list(Fun, L, R)
	end.

short_foldl_list(_, [], Ret) ->
	Ret;
short_foldl_list(Fun,[H|L], Ret) ->
	case Fun(H, Ret) of
		{continue, NewRet} ->
			short_foldl_list(Fun, L, NewRet);
		{break, NewRet} ->
			NewRet;
		{true, NewRet} ->
			short_foldl_list(Fun, L, NewRet);
		{false,NewRet} ->
			NewRet
	end.

list_sub_elem(List, 0) ->
    List;
list_sub_elem([], _) ->
    [];
list_sub_elem([_|List], Count) ->
    list_sub_elem(List, Count-1).

right_string(String, Len, FillWord) ->
	StrLen = length(String),
	if
		StrLen > Len -> string:sub_string(String, StrLen-Len+1);
		StrLen < Len ->
			LenList = lists:seq(1, Len-StrLen),
			Fun = fun(_,S) ->
						  S ++ tool:to_list(FillWord)
				  end,
			RightString = lists:foldl(Fun, "", LenList),
			RightString ++ String;
		StrLen =:= Len -> String
	end.
left_string(String, Len, FillWord) ->
	StrLen = length(String),
	if
		StrLen > Len -> string:sub_string(String, 1, Len);
		StrLen < Len ->
			LenList = lists:seq(1, Len-StrLen),
			Fun = fun(_,S) ->
						  S ++ tool:to_list(FillWord)
				  end,
			RightString = lists:foldl(Fun, "", LenList),
			String ++ RightString;
		StrLen =:= Len -> String
	end.
			

						 
seq(From, To)	 ->
	Max = max(From, To),
	Min = min(From, To),
	lists:seq(Min, Max).

seq2(From,To) ->
	case From > To of
		true ->
			lists:reverse(lists:seq(To, From));
		false->
			lists:seq(From, To)
	end.

mixture(L1,L2) when is_list(L1),is_list(L2) ->
	Fun = fun(E,{R0,[L|L0]}) ->
				  {R0++[E,L],L0};
			 (E,{R0,[]}) ->
				  {R0++[E],[]}
		  end,
	{R,L} = lists:foldl(Fun, {[],L2}, L1),
	R++L.

thing_to_list(X) when is_bitstring(X) -> bitstring_to_list(X);
thing_to_list(X) when is_tuple(X) -> tuple_to_list(X);
thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X)    -> X.

%% 日志记录函数
log(T, F, A, Mod, Line) ->
    {ok, Fl} = file:open("logs/error_log.txt", [write, append]),
    Format = list_to_binary("#" ++ T ++" ~s[~w:~w] " ++ F ++ "\r\n~n"),
    {{Y, M, D},{H, I, S}} = erlang:localtime(),
    Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
    io:format(Fl, unicode:characters_to_list(Format), [Date, Mod, Line] ++ A),
    file:close(Fl).    



%% 转换成HEX格式的md5
md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).


%% 产生一个介于Min到Max之间的随机整数

rand(N)->
	rand(1,N).

rand(Same, Same) -> Same;
rand(Min, Max) ->
    M = Min - 1,
	if
		Max - M =< 0 ->
			0;
		true ->
    		%% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
    		case get("rand_seed") of
        		undefined ->
            		RandSeed = erlang:now(),
            		random:seed(RandSeed),
            		put("rand_seed", RandSeed);
        		_ -> skip
    		end,
    		%% random:seed(erlang:now()),
			random:uniform(Max - M) + M
	end.

rand_real_time(Same,Same) -> Same;
rand_real_time(Min,Max) ->
	M = Min - 1,
	if
		Max - M =< 0 ->
			0;
		true ->
    		%% 将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
    		RandSeed = mod_rand:get_seed(),
    		random:seed(RandSeed),
    		%% random:seed(erlang:now()),
			random:uniform(Max - M) + M
	end.

random_list([]) ->false;
random_list(List) ->
	lists:nth(com_util:rand(1, length(List)), List).

random_list2([]) ->
	{false,[]};
random_list2(List) ->
	Nth = rand(1,length(List)),
	NthData = lists:nth(Nth, List),
	{NthData,lists:delete(NthData, List)}.

random_delete(RemainList) ->
	Pos = com_util:rand(1, length(RemainList)),
	remove_nth(RemainList, Pos).

random_delete(List, DelCount) ->
	Fun = fun(_, RemainList) ->
				  random_delete(RemainList)
		  end,
	lists:foldl(Fun, List, lists:seq(1, DelCount)).

random_true(Min,Max,Ratio) ->
    rand(Min, Max) =< Ratio.

%%随机从集合中选出指定个数的元素length(List) >= Num
%%[1,2,3,4,5,6,7,8,9]中选出三个不同的数字[1,2,4]
get_random_list(List,Num) ->
	ListSize = length(List),
	F = fun(N,List1) ->
				Random = rand(1,(ListSize-N+1)),
				Elem = lists:nth(Random, List1),
				List2 = lists:delete(Elem, List1),
				List2
		end,
	Result = lists:foldl(F, List, lists:seq(1, Num)),
	List -- Result.

operation_div(_Divider1,0) ->
	0;
operation_div(Divider1,Divider2) ->
	Divider1 div Divider2.

%%向上取整
ceil(N) when is_integer(N)->
	N;
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

ceil(_Dividend, 0) ->
	0;
ceil(Dividend, Divisor) ->
    ceil(Dividend/Divisor).

%%向下取整
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

%%四舍五进
rffi(X) ->
    T = ceil(X),
    T1= (1 - (T - X)) * 10,
    case T1  >= 5 of
        true -> T;
        false-> floor(X)
    end.

%%取小数点位数 eg. 1/3 2  -> 0.33(四舍五入法)
decimal(Value, Point) ->
	Power = power(10, Point),
	rffi(Value * Power) / Power.

decimal(F, S, Point) ->
	case S =/= 0 of
		true ->
			decimal(F / S, Point);
		false->
			0
	end.


sleep(T) ->
    receive
    after T -> ok
    end.

sleep(T, F) ->
    receive
    after T -> F()
    end.

get_list([], _) ->
    [];
get_list(X, F) ->
    F(X).

list_deletex(List, Element) ->
	Fun = fun(OldElement) ->
				  OldElement =/= Element
		  end,
	lists:filter(Fun, List).

list_elementx(List, Element) ->
	[Element|list_deletex(List, Element)].

after_do(AfterTimeMS, {M,F,A}) ->
	spawn(fun() -> after_do_loop(AfterTimeMS,{M,F,A}) end).

after_do_loop(AfterTimeMS,{M,F,A}) ->
	receive
		_ ->
			igore
	after AfterTimeMS ->
			apply(M, F, A)
	end.

new_call(Fun) ->
	Loop = fun() ->
				   receive
					   {temp_single, From} ->
						   From ! {recv_data, Fun()}
				   end
		   end,
	PID = spawn(Loop),
	PID ! {temp_single, self()},
	receive
		{recv_data, Data} ->
			Data
	end.
		

%%打乱列表函数  List =[] 返回 打乱后列表 List2
upset_list(List) ->
    upset_list(List, []).

upset_list(List, Result) ->
    upset_list(List, rand(1, length(List)), Result).

upset_list(_List, 0, Result) ->
    Result;
upset_list(List, Nth, Result) ->
    {NthData, Remain} = extract_member(rand(1, Nth), List),
    upset_list(Remain, length(Remain), [NthData|Result]).
            
%%提取列表中第Nth个元素，{NthData, RemainList}
extract_member(Nth, List) ->
    extract_member(Nth, List, []).

extract_member(1, List1, List2) ->
    case List1 of
        [R] ->
            {R, List2};
        [R|L] ->
            {R, List2 ++ L}
    end;

extract_member(Nth, [R|List1], List2) ->
    extract_member(Nth - 1, List1, List2++[R]).

%%根数Tuple元素数量，把List转成Tuple 返回：{TupleList, RemainList}
list_to_tuple(List, TupleCount) ->
    list_to_tuple(List, TupleCount, []).
list_to_tuple(List, TupleCount, Result) when length(List) >= TupleCount ->
    {List1, List2} = lists:split(TupleCount, List),
    list_to_tuple(List2, TupleCount, [list_to_tuple(List1)|Result]);
list_to_tuple(List, _TupleCount, Result) ->
    {lists:reverse(Result), List}.

%%计算元素在列表中出现几次(5, [1,2,3,5,5,5,3]) ----> 3
repeat_count(Element, List) ->
    repeat_count(Element, List, 0).
repeat_count(Element, List, Count) ->
    case lists:member(Element, List) of
        false-> Count;
        true ->
            repeat_count(Element, lists:delete(Element, List), Count+1)
    end.

remove_nth(RemainList, Pos) ->
	Head = lists:sublist(RemainList, 1, Pos-1),
	End  = lists:sublist(RemainList, Pos+1, length(RemainList)),
	Head ++ End.

%%删除指定元素返回：(5, [1,2,4,4,5,5,4,5])   -> [1,2,4,4,4]
remove_element(Element, List) ->
    case lists:member(Element, List) of
        false->List;
        true -> remove_element(Element, lists:delete(Element, List))
    end.

%%删除重复元素 [1,2,3,2,3,4] ---> [1,2,3,4]
remove_repeat(List) ->
    remove_repeat(List, []).

remove_repeat([], Result) ->
    Result;
remove_repeat([L|List], Result) ->
    case lists:member(L, Result) of
        false-> remove_repeat(List, Result++[L]);
        true -> remove_repeat(List, Result)
    end.

combination(NumberCount, Count) ->
    trunc(c(NumberCount, Count) / c(Count, Count)).

c(Max, Count) ->
    lists:foldl(fun(C, Sum) ->
                        Sum * (Max - C)
                end, Max, lists:seq(1, Count-1)) .

    
    
%%累加前两个数之和，找出第几个数的值。
cumsum(InitValue, Nth) ->
	NthList = lists:seq(1, Nth),
	Fun = fun(_,{Last1,Last2}) ->
				  {Last1+Last2,Last1}
		  end,
	{FinalValue,_} = lists:foldl(Fun, {InitValue,0}, NthList),
	FinalValue.
    
    
map(F, [H|List], Skip) ->
	case F(H) of
		Skip ->
			map(F, List, Skip);
		Ret  ->
			[Ret|map(F, List, Skip)]
	end;
map(F, [], _Skip) when is_function(F, 1) -> [].

%% for循环
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).

%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> 
	{ok, State};
for(Max, Max, F, State) ->F(Max, State);
for(I, Max, F, State)   -> {ok, NewState} = F(I, State), for(I+1, Max, F, NewState).


for_new(Min, Max, _F, State) when (Min > Max) -> 
	{ok, State};
for_new(Min, Max, F, State) -> 
	{ok, NewState} = F(Min, State), 
	for_new(Min+1, Max, F, NewState).

for2(F, State) ->
	for2(go_on, F, State).
for2(stop, _F, State) ->
	State;
for2(go_on, F, State) ->
	{IsGoOn, NewState} = F(State),
	for2(IsGoOn, F, NewState).

%% 取列表Ele后面的元素
ele_tail(_Ele, []) ->
	[];
ele_tail(Ele, [Ele|T]) ->
	T;
ele_tail(Ele, [_|T]) ->
	ele_tail(Ele, T).

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%%将列表转换为string [a,b,c] -> "a,b,c"
list_to_string(List) ->
	case List == [] orelse List == "" of
		true -> "";
		false ->
			F = fun(E) ->
						tool:to_list(E)++","
				end,
			L1 = [F(E)||E <- List] ,
			L2 = lists:concat(L1),
			string:substr(L2,1,length(L2)-1)
	end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).

run_string(String) ->
    {ok,Scanned,_} = erl_scan:string(String),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    erl_eval:exprs(Parsed,[]).




%%根据Key去查List中元素的Nth位相等的匹配到的元素Fun(Element)返回
find_elements(Key, Nth, List, Fun) ->
	InnerFun = fun(Element) ->
					   case element(Nth, Element) =:= Key of
						   true ->
							   {true, Fun(Element)};
						   false->
							   false
					   end
			   end,
	lists:filtermap(InnerFun, List).


%%以e=2.718281828459L为底的对数


check_same_day(Timestamp)->
	NDay = (com_util:unixtime()+8*3600) div 86400,
	ODay = (Timestamp+8*3600) div 86400,
	NDay=:=ODay.

%%对list进行去重，排序
%%Replicat 0不去重，1去重
%%Sort 0不排序，1排序
filter_list(List,Replicat,Sort) ->
	if Replicat == 0 andalso Sort == 0 ->
		   List;
	   true ->
		   if Replicat == 1 andalso Sort == 1 ->
				  lists:usort(List);
			  true ->
				   if Sort == 1 ->
						  lists:sort(List);
					  true ->
						  lists:reverse(filter_replicat(List,[]))
				   end
		   end
	end.

%%list去重
filter_replicat([],List) ->
	List;
filter_replicat([H|Rest],List) ->
	Bool = lists:member(H, List),
	List1 = 
	if Bool == true ->
		   [[]|List];
	   true ->
		   [H|List]
	end,
	List2 = lists:filter(fun(T)-> T =/= [] end, List1),
	filter_replicat(Rest,List2).


%% ------------------------------------------------------
%% desc   获取字符串汉字和非汉字的个数  
%% parm   UTF8String  			UTF8编码的字符串
%% return {汉字个数,非汉字个数}
%% -------------------------------------------------------
get_chinese_count(UTF8String)->
	UnicodeList = unicode:characters_to_list(list_to_binary(UTF8String)),
	Fun = fun(Num,{Sum})->
				  case Num >= ?UNICODE_CHINESE_BEGIN  andalso  Num =< ?UNICODE_CHINESE_END of
					  true->
						  {Sum+1};
					  false->
						  {Sum}
				  end
		  end,
	{ChineseCount} = lists:foldl(Fun, {0}, UnicodeList),
	OtherCount = length(UnicodeList) - ChineseCount,
	{ChineseCount,OtherCount}.

list2utfstring(List) ->
	unicode:characters_to_list(erlang:list_to_binary(List), utf8).

%% 与lists:nth一样，不过多了0判断和N>length(List)情况的判断
lists_nth(0, _) -> [];
lists_nth(1, [H|_]) -> H;
lists_nth(_, []) -> [];
lists_nth(N, [_|T]) when N > 1 ->
    lists_nth(N - 1, T).

%% 替换列表第n个元素
lists_nth_replace(N, L, V) ->
	lists_nth_replace(N, L, V, []).
lists_nth_replace(0, L, _V, _OH) -> L;
lists_nth_replace(1, [_H|T], V, OH) -> recover(OH, [V|T]);
lists_nth_replace(_, [], _V, OH) -> recover(OH, []);
lists_nth_replace(N, [H|T], V, OH) when N > 1 ->
    lists_nth_replace(N - 1, T, V, [H|OH]).

recover([], Hold) ->Hold;
recover([H|T], Hold) ->
	recover(T, [H|Hold]).

%% 如果参数小于0，则取0
get_pos_num(Num) ->
	if Num < 0 ->
		   0;
	   true ->
		   Num
	end.

%% 如果参数小于1，则取1
get_pos_num2(Num) ->
	if Num < 1 ->
		   1;
	   true ->
		   Num
	end.

get_max_num(Num, Max) ->
	if Num > Max ->
		   Max;
	   true ->
		   Num
	end.

make_sure_list(List, Where) ->
	if is_list(List) -> 
		   List; 
	   true ->
		   ?LOG_ERROR("List=~p, Where=~p~n", [List, Where]),
		   []
	end.


compile_base_data(Table, ModName, IDPoses) ->
	ModNameString = com_util:term_to_string(ModName),
	HeadString = 
		"-module("++ModNameString++").
		-compile(export_all).
		",
	BaseDataList = db_base:select_all(Table, "*", []),
	ContentString = 
	lists:foldl(fun(BaseData0, PreString) ->
						FunChange = 
							fun(Field) ->
									 if is_integer(Field) -> Field; 
										true -> 
											case com_util:bitstring_to_term(Field) of
												undefined ->
													Field;
												Term ->
													Term
											end
									 end
							end,
						BaseData = [FunChange(Item)||Item <- BaseData0],
						Base =list_to_tuple([Table|BaseData]),
						BaseString = com_util:term_to_string(Base),
						IDs = [element(Pos, Base)||Pos<-IDPoses],
						IDList0 = lists:foldl(fun(ID, PreString2)->
													 IDList = 
															if erlang:is_integer(ID) ->
																integer_to_list(ID);
															true ->
																ID
													 	end,
													 PreString2++","++IDList
											 end, [], IDs),
						[_|IDList] = IDList0,
						PreString ++ 
							"get(" ++ 
							IDList ++ 
							") ->" ++ 
							BaseString ++
							";
							"
				end
				, "", BaseDataList),
	
	_List0 = [",_"||_Pos<-IDPoses],
	[_|_List] = lists:flatten(_List0),
	ErrorString = "get("++_List++") -> undefined.
	",
	FinalString = HeadString++ContentString++ErrorString,
	%% ?PRINT("string=~s~n",[FinalString]),
	try
        {Mod,Code} = dynamic_compile:from_string(FinalString),
        code:load_binary(Mod, ModNameString++".erl", Code)
    catch
        Type:Error -> ?LOG_ERROR("Error compiling (~p): ~p~n", [Type, Error])
    end,
	ok.

%% 注册函数
register_fun(Fun, Times, Key) ->
	case get({register_fun, Key}) of
		[_|_] = RegisteredFuns ->
			put({register_fun, Key}, [{Fun, Times}|RegisteredFuns]);
		_ ->
			put({register_fun, Key}, [{Fun, Times}])
	end.

%% 执行注册函数
execute_registered_fun(Key) ->
	case get({register_fun, Key}) of
		[_|_] = Funs ->
			NewFuns = 
				lists:foldl(fun({Fun, Times}, Pre) ->
									try Fun() of _ -> ok			%% try执行
									catch _:R -> ?LOG_ERROR("R=~p, stack=~p~n", [R, erlang:get_stacktrace()]) end,
									case Times of
										1 -> Pre;					%% 已执行完相应次数
										loop -> [{Fun, loop}|Pre];	%% 循环执行
										_ -> [{Fun, Times-1}|Pre]	%% 剩余次数减一
									end
							end, [], Funs),
			put({register_fun, Key}, NewFuns);
		_Other ->
				 skip
	end.

get_day_start(Time) ->
	Time-((Time+28800) rem 86400).

to_record(RecordName, RecordData) when is_tuple(RecordData) ->
	to_record(RecordName, tuple_to_list(RecordData));

to_record(RecordName, RecordData) when is_list(RecordData) ->
	list_to_tuple([RecordName | RecordData]).

%% 查询Member在List的位置，不存在返回false 存在返回位置
find_pos(Member, List) ->
    find_pos_temp(Member, List, 1, false).

find_pos_temp(_,[],_,false) ->
    false;
find_pos_temp(_Member, _List, Pos, true) ->
    Pos;
find_pos_temp(Member, [M|List], Pos, false) ->
    if
        M == Member -> find_pos_temp(Member, List, Pos, true);
        true        -> find_pos_temp(Member, List, Pos+1, false)
    end.

list_page(List, PageCount,single) ->
	{Page, RemainList} = single_page(List, PageCount),
	{Page, RemainList};

list_page(List, PageCount,all) ->
	PageTotal = com_util:ceil(length(List) / PageCount),
	Fun = fun(_, {RemainList,Pages}) ->
				  {Page, RemainList2} = single_page(RemainList, PageCount),
				  {RemainList2, [Page|Pages]}
		  end,
	{_, Pages} = lists:foldl(Fun, {List,[]}, lists:seq(1, PageTotal)),
	list_to_tuple(lists:reverse(Pages)).

single_page(List, PageCount) ->
	single_page(List, PageCount, []).
single_page([], _, Page) ->
	{lists:reverse(Page), []};
single_page([Hd|List], PageCount, Page) when PageCount > 0 ->
	single_page(List, PageCount-1, [Hd|Page]);
single_page(List, 0, Page) ->
	{lists:reverse(Page), List}.
	

%% @spec Tuple is a record 
%% @spec Fields is  record's fields 
%% @spec List is [{field, Operate, value},...] Operate = set | sub | add
%% @spec return NewTuple
element_operate(Tuple,Fields,List)->
    TupleList = lists:nthtail(1, tuple_to_list(Tuple)),
    lists:foldl(fun({K,O,V}, Tuple0) ->
                        case O of
                            add ->
                                Pos = find_pos(K,Fields),
                                OldV = lists:nth(Pos, TupleList),
                                setelement(Pos+1, Tuple0, OldV+V);
                            sub ->
                                Pos = find_pos(K,Fields),
                                OldV = lists:nth(Pos, TupleList),
                                setelement(Pos+1, Tuple0, OldV-V);
                            set ->
                                Pos = find_pos(K,Fields),
                                setelement(Pos+1, Tuple0, V)
                        end;
                   ({K,V}, Tuple0) ->
                        Pos = find_pos(K,Fields),
                        setelement(Pos+1, Tuple0, V)
                end,
                Tuple,
                lists:filter(fun({K,_,_}) -> lists:member(K, Fields);
                                ({K,_}) ->lists:member(K, Fields)
                             end, List)).

element_value(Tuple, Fields, Attr) ->
    case get_tuple_data_list(Tuple, Fields, [Attr]) of
        [] -> not_value;
        [Data] -> Data
    end.

get_tuple_data_list(Tuple, Fields, GetFieldsList) ->
    [_|Data] = tuple_to_list(Tuple),
    FD = lists:zip(Fields, Data),
    lists:foldl(fun(Field, List) -> case lists:keyfind(Field, 1, FD) of
                                        false -> List;
                                        {_,D} -> List ++ [D]
                                    end
                end, [], GetFieldsList).
    

print_record(Fields,Tuple) ->
    [_|Value] = tuple_to_list(Tuple),
    com_util:term_to_string(lists:zip(Fields, Value)).



%%乘方，X^Y
power(X, Y) ->
    power(1, X,Y).

power(Sun, _X, 0) ->
    Sun;
power(Sun, X, Y) ->
    power(Sun * X, X, Y - 1).

%%三目运算。
ternary_operator(true, Expression2, _Expression3) -> Expression2;
ternary_operator(false,_Expression2, Expression3) -> Expression3;
ternary_operator(Fun,Expression2, Expression3) when is_function(Fun)->
    ternary_operator(Fun(), Expression2, Expression3).



point_to_line(PointList) ->
    [One|Other] = PointList,
    point_to_line(One, Other, []).


point_to_line(One, [Two|Other], PointLine) when One == Two->
    point_to_line(Two, Other, PointLine ++ [Two]);
point_to_line(One, [Two|_Other]=Data, PointLine) ->
    point_to_line(next_point(One, Two), Data, PointLine ++ [One]);
point_to_line(_, _, PointLine) ->
    PointLine.

next_point({X1, Y1}, {X2, Y2}) ->
    {next_point(X1,X2), next_point(Y1,Y2)};
next_point(X1, X2) when X1 == X2->
    X1;
next_point(X1, X2) when X1 > X2->
    X1-1;
next_point(X1, X2) when X1 < X2->
    X1+1.


%%交集
get_intersection_list(A, B) when is_list(A) andalso is_list(B) ->
    lists:filter(fun(X) -> lists:member(X, A) end, B).

%%并集
get_unite_list(A, B) when is_list(A) andalso is_list(B) ->
    C = A ++ B,
    lists:usort(C).

%%差集
get_subtract_list(A, B) when is_list(A) andalso is_list(B) ->
    Insection = get_intersection_list(A, B),
    Unite = get_unite_list(A, B),
    lists:filter(fun(X) -> lists:member(X, Insection) =:= false end, Unite). 

get_data_type(Data) when is_list(Data) -> list;
get_data_type(Data) when is_integer(Data) -> integer;
get_data_type(Data) when is_binary(Data) -> binary;
get_data_type(Data) when is_function(Data) -> function;
get_data_type(Data) when is_tuple(Data) -> tuple;
get_data_type(Data) when is_atom(Data) -> atom;
get_data_type(Data) when is_bitstring(Data) -> bitstring;
get_data_type(Data) when is_boolean(Data) -> boolean;
get_data_type(Data) when is_float(Data) -> float;
get_data_type(Data) when is_number(Data) -> number;
get_data_type(Data) when is_pid(Data) -> pid;
get_data_type(Data) when is_port(Data) -> port;
get_data_type(_Data) ->not_know.

%% @doc 获取某个进程字典的信息
dic_get(Pos) ->
  dic_get(Pos, undefined).
%% @doc 获取某个进程字典的信息，如果不存在，返回Def
dic_get(Pos, Def) ->
  case get(Pos) of
    undefined ->
      Def;
    Res ->
      Res
  end.
%% @doc 设置进程字典
dic_set(Pos, Value)->
  put(Pos, Value).

%% @doc 清除进程字典
dic_erase(Pos) ->
    erase(Pos).


check(Name) when is_atom(Name)->
  erlang:whereis(Name) =/= undefined;
check(PID) when is_pid(PID)->
  erlang:is_process_alive(PID).
make_ticket(Time,Platform,Account,Shadow)->
	base64:encode(<<(list_to_binary(Time))/binary,(erlang:md5([Time,Platform,Account,Shadow]))/binary>>).

merge(L)->
	merge(L,[]).
merge({Key,Val},Old)->
	case lists:keyfind(Key, 1, Old) of
		false ->
			[{Key,Val}|Old];
		{Key,OldVal}->
			lists:keyreplace(Key, 1, Old, {Key,OldVal + Val})
	end;
merge([],Res)->
	Res;
merge([H|T],L)->
	merge(T,merge(H,L)).

concat(List, binary) ->
	Fun =
	fun(Ele, Bin) ->
		<<Bin/binary, (com_type:to_binary(Ele))/binary>>
	end,
	lists:foldl(Fun, <<>>,List);
concat(List, string) ->
	Fun =
	fun(Ele) ->
		com_type:to_list(Ele)
	end,
	lists:concat(lists:map(Fun, List)).