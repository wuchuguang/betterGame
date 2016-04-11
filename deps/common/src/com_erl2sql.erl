-module(com_erl2sql).

-export([new/3,test/1]).

-define(ERR_MSG(__Reason,__Param),throw({error,[{reason,__Reason},{param,__Param}]})).

-define(DEFAULT_OPTIONS,[
    {save_type,syn},
    {export,true},
    {is_key,false},
    {show_change,true},
    {export_update,false}
]).
-define(DATA_TYPES,[number,string,term,sterm,atom,int,small]).
-define(SAVE_TYPES,[temp,syn]).
-define(DB_INTERFACE,"db_helper").
-define(NONE,none).
-define(NEXT_LINE,"\n").

-record(make_src_info,{function,db_name,table,param,opts,fields_dict}).

-define(format_line(__DATA), ?format_line("~p",[__DATA])).
-define(format_line(__FORMAT, __DATA), io:format("line(~p)~n"++__FORMAT++"~n", [?LINE]++__DATA)).

-define(format(__DATA), ?format("~n~p~n",[__DATA])).
-define(format(__FORMAT, __DATA), io:format(__FORMAT++"~n", __DATA)).

new(DbName, Module, ErlFormat)->
    try
        render({DbName, Module, ErlFormat})
    catch _:Reason ->
        io:format("~p,~p",[Reason,erlang:get_stacktrace()])
    end.

test(File) ->
    new(test_wcg, wcg_test, [{id,[{is_key,true},{data_type,number}]},
        {type,[{data_type,number},{default,1}]},
        {savuuid,[{data_type,number}]},
        {audio_name,[{data_type,string}]},
        {audio_file,[{data_type,string}]},
        {audio_duration,[{data_type,int}]},
        {fs_no,[{data_type,int}]},
        {fs_handle,[{data_type,int}]},
        {fs_session,[{data_type,string}]},
        {share_link,[{data_type,number}]},
        {up_time,[{data_type,int}]}]),
    ok.

render({DBName,Module, ErlFormat})->
    generate([{Module, ErlFormat, [{db,DBName}]}],[{db_name,DBName}]).

merge_defines(Defines)->
    merge_defines(Defines,[]).

merge_defines([],Result)->
    lists:reverse(Result);
merge_defines([{include,DefineFile} | T],Result)->
    {ok,Defines} = file:consult(DefineFile),
    RealDefines = merge_defines(Defines),
    merge_defines(T,RealDefines ++ Result);

merge_defines([H | T],Result)->
    merge_defines(T,[H | Result]).

generate(Defines,Opts)->
    GenerateInfo = do_generate(Defines,dict:new()),
    render_generate_info(GenerateInfo,Opts).

do_generate([],Result)->
    Result;
do_generate([Define | T],Result)->
    do_generate(T,render_define(Define,Result)).

render_define({Table,Fields},Result)->
    render_define({Table,Fields,[]},Result);

render_define({Table,Fields,Opts},Result)->
    GlobalOpts = filter_global_opts(Opts ++ [{save_type,temp}]),
    FieldsDict = parse_fields(Fields,GlobalOpts,dict:new()),
    F =
        fun(Type,Values,AccIn)->
            GlobalRenderInfo = global_render(Type,Values,Table,GlobalOpts,FieldsDict),
            AccIn1 = render_info_fold(
                fun(Type1,RenderResult,RenderAccIn)->
                    dict:append_list({global_render,Type1},RenderResult,RenderAccIn)
                end,AccIn,GlobalRenderInfo),
            RenderInfo = render(Type,Values,Table,GlobalOpts,FieldsDict),
            render_info_fold(
                fun(Type2,RenderResult,RenderAccIn)->
                    dict:append_list({render,Table,Type2},RenderResult,RenderAccIn)
                end,AccIn1,RenderInfo)
        end,
    dict:fold(F,Result,FieldsDict);

render_define(Other,_Result)->
    ?ERR_MSG(define_invalid,{other,Other}).

filter_global_opts(Opts)->
    filter_global_opts(Opts,dict:new(),[]).

filter_global_opts([],_Buffer,Result)->
    Result;
filter_global_opts([H = {db,_DB} | T],Buffer,Result)->
    case dict:find(save_db,Buffer) of
        error ->
            filter_global_opts(T,dict:store(save_db,true,Buffer),[{save_type,H} | Result]);
        {ok,true} ->
            filter_global_opts(T,Buffer,Result)
    end;
filter_global_opts([{save_type,temp} = H | T],Buffer,Result)->
    case dict:find(save_db,Buffer) of
        error ->
            filter_global_opts(T,Buffer,[H | Result]);
        {ok,true} ->
            filter_global_opts(T,Buffer,Result)
    end;
filter_global_opts([H | T],Buffer,Result)->
    filter_global_opts(T,Buffer,[H |Result]).

parse_fields([],_GlobalOpts,FieldDict)->
    FieldDict;
parse_fields([{Field,Options} | T],GlobalOpts,FieldDict)->
    FinalOptions1 = merge_opts(Options ++ ?DEFAULT_OPTIONS),
    FinalOptions2 = get_default_options(FinalOptions1),
    parse_fields(T,GlobalOpts,parse_field(Field,merge_opts(FinalOptions1 ++ FinalOptions2),FieldDict)).

merge_opts(Opts)->
    merge_opts(Opts,[]).

merge_opts([],Results)->
    Results;
merge_opts([H | T],Results)->
    case is_valid_option(H) of
        true ->
            case check_option(H,Results) of
                false ->
                    merge_opts(T,[H |Results]);
                true ->
                    merge_opts(T,Results);
                {new_option,NewOption} ->
                    merge_opts(T,[NewOption | Results])
            end;
        false ->
            merge_opts(T,Results)
    end.

get_default_options(Opts)->
    get_default_options(Opts,[]).
get_default_options([],Results)->
    Results;
get_default_options([Opt | T],Results)->
    case get_default_option(Opt) of
        ?NONE ->
            get_default_options(T,Results);
        NewOpt ->
            get_default_options(T,[NewOpt | Results])
    end.

get_default_option({data_type,DataType})->
    case DataType of
        int -> {default,0};
        small -> {default,0};
        number -> {default,0};
        string -> {default,"\"\""};
        term -> {default,[]};
        atom -> {default,"\'\'"};
        sterm -> {default,[]}
    end;
get_default_option(_)->
    ?NONE.

is_valid_option({data_type,DataType})->
    lists:member(DataType,?DATA_TYPES);
is_valid_option({is_key,_IsKey})->
    true;
is_valid_option({is_index,_IsKey})->
    true;
is_valid_option({save_type,SaveType})->
    lists:member(SaveType,?SAVE_TYPES);
is_valid_option({show_change,_ShowChange})->
    true;
is_valid_option({export_update,_ExportUpdate})->
    true;
is_valid_option({default,_Default})->
    true;
is_valid_option({export,_Export})->
    true;
is_valid_option(_)->
    false.

parse_field(_Field,[],FieldDict)->
    FieldDict;
parse_field(Field,[Option | T],FieldDict)->
    parse_field(Field,T,do_parse_field(Option,Field,FieldDict)).

do_parse_field({export,Export},Field,FieldDict)->
    case Export of
        true ->
            dict:append(export,Field,FieldDict);
        false ->
            FieldDict
    end;
do_parse_field({data_type,DataType},Field,FieldDict)->
    dict:append(data_types,{Field,DataType},FieldDict);
do_parse_field({is_key,IsKey},Field,FieldDict)->
    case IsKey of
        true ->
            dict:append(keys,Field,FieldDict);
        false ->
            FieldDict
    end;
do_parse_field({is_index,IsKey},Field,FieldDict)->
    case IsKey of
        true ->
            dict:append(indexs,Field,FieldDict);
        false ->
            FieldDict
    end;
do_parse_field({save_type,SaveType},Field,FieldDict)->
    case SaveType of
        syn ->
            dict:append(syn_fields,Field,FieldDict);
        temp ->
            dict:append(temp_fields,Field,FieldDict)
    end;
do_parse_field({export_update,ExportUpdate},Field,FieldDict)->
    case ExportUpdate of
        true ->
            dict:append(export_update_field,Field,FieldDict);
        false ->
            FieldDict
    end;
do_parse_field({show_change,ShowChange},Field,FieldDict)->
    case ShowChange of
        true ->
            dict:append(change_fields,Field,FieldDict);
        false ->
            FieldDict
    end;
do_parse_field({default,Default},Field,FieldDict)->
    dict:append(default,{Field,Default},FieldDict);
do_parse_field(Option,Field,_FieldDict)->
%  io:format("here:~p,~p~n",[Option,Field]), 
    ?ERR_MSG(no_parse_option,[{field,Field},{option,Option}]).

check_option({Option,_Params},Results)->
    lists:keymember(Option,1,Results).


fetch_fields_dict(Key,FieldsDict,Default)->
    case dict:find(Key,FieldsDict) of
        error ->
            Default;
        {ok,Value} ->
            Value
    end.


render_info_new()->
    dict:new().

render_info_fold(F,AccIn,RenderInfo)->
    dict:fold(F,AccIn,RenderInfo).

%export_record(RecordName,FieldName,Result)->
%  export_record(RecordName,FieldName,none,Result).
%export_record(RecordName,FieldName,Default,Result)->
%  dict:append({record,RecordName},{FieldName,Default},Result).

export_fun(Module,FunName,Arity,Result)->
    dict:append({funs,Module},{com_type:to_atom(FunName),Arity},Result).
append_src(FileName,Data,Result)->
    dict:append({src_code,FileName},Data,Result).
append_include(Data,Result)->
    dict:append(include_code,Data,Result).


make_src_template(#make_src_info{function = Fun,opts = Opts,table = Table} = MakeSrcInfo,RenderInfo)->
    DBName =
        case lists:keyfind(save_type,1,Opts) of
            {_,temp} ->
                'temp';
            {_,{db,R}} ->
                R
        end,
    {FunNames,Src} = Fun(MakeSrcInfo#make_src_info{db_name = DBName}),
    FinalFunNames =
        case is_list(FunNames) of
            true ->
                FunNames;
            false ->
                [FunNames]
        end,
    RenderInfo1 = lists:foldl(
        fun({FunName,Arity},AccIn)->
            export_fun(Table,FunName,Arity,AccIn)
        end,RenderInfo,FinalFunNames),
    append_src(Table,Src,RenderInfo1).


render(export,Values,Table,Opts,FieldsDict)->
    render_record(Table,Values,Opts,FieldsDict,render_info_new());

render(export_update_field,Values,Table,Opts,FieldsDict)->
    F =
        fun(Value,AccIn)->
            make_src_template(#make_src_info{
                function = fun update_fun_src/1,
                table = Table,param = Value,
                opts = Opts,fields_dict = FieldsDict},AccIn)
        end,
    lists:foldl(F,render_info_new(),Values);

render(syn_fields,Values,Table,Opts,FieldsDict)->
    GenerateInfo = render_record(get_db_record_name(Table),Values,Opts,FieldsDict,render_info_new()),
    render_db_method(Table,Values,Opts,FieldsDict,GenerateInfo);

render(temp_fields,_Values,_Table,_Opts,_FieldsDict)->
    render_info_new();
render(keys,[Value],Table,Opts,FieldsDict)->
    make_src_template(#make_src_info{function = fun get_key_src/1,table = Table,param = Value,opts = Opts,fields_dict = FieldsDict},render_info_new());
render(data_types,Values,Table,_Opts,_FieldsDict)->
    GenerateInfo = export_fun(Table,data_type,1,render_info_new()),
    F =
        fun({Field,Value})->
            "%% @doc 属性值类型\ndata_type(" ++ com_type:to_list(Field) ++ ")->" ++ ?NEXT_LINE ++
                "\t" ++ com_type:to_list(Value)
        end,
    append_src(Table,string:join(lists:map(F,Values),";" ++ ?NEXT_LINE) ++ "." ++ ?NEXT_LINE,GenerateInfo);
render(default,Values,Table,_Opts,FieldsDict)->
    GenerateInfo = export_fun(Table,default,1,render_info_new()),
    DataTypes = fetch_fields_dict(data_types,FieldsDict,[]),
    GenerateInfo1 = lists:foldl(
        fun({Field,Value},AccIn)->
            {_,DataType} = lists:keyfind(Field,1,DataTypes),
            Value1 =
                case DataType of
                    term ->
                        com_util:term_to_bitstring(Value);
                    sterm->
                        com_util:term_to_bitstring(Value);
                    atom->
                        com_type:to_list(Value);
                    _ ->
                        Value
                end,
            append_src(Table,
                "%% @doc 表属性默认值\ndefault(" ++ com_type:to_list(Field) ++ ")->" ++ ?NEXT_LINE ++
                    "\t" ++ com_type:to_list(Value1) ++ ";" ++ ?NEXT_LINE,AccIn)
        end,GenerateInfo,Values),
    append_src(Table,"default(_Other)-> undefined." ++ ?NEXT_LINE,GenerateInfo1);
render(change_fields,Values,Table,Opts,FieldsDict)->
    make_src_template(
        #make_src_info{
            function = fun get_change_fields/1,table = Table,
            param = Values,opts = Opts,fields_dict = FieldsDict},render_info_new());
render(Other,Values,Table,_Opts,_FieldsDict)->
    ?ERR_MSG(render_invalid,[{type,Other},{values,Values},{table,Table}]).

get_change_fields(#make_src_info{table = Table,param = Fields})->
    TableName = com_type:to_list(Table),
    {
        {get_changed,2},
            "%% @doc 对比两个结构，返回差异属性键值列表\nget_changed(OldPlayer,NewPlayer) when erlang:is_record(OldPlayer," ++ TableName ++ "),erlang:is_record(NewPlayer," ++ TableName ++ ")->" ++ ?NEXT_LINE ++
            "\tFields = [" ++ string:join([com_type:to_list(Field) || Field <- Fields],",") ++ "]," ++ ?NEXT_LINE ++
            "\tFun = " ++ ?NEXT_LINE ++
            "\tfun(Field, L) ->" ++ ?NEXT_LINE ++
            "\t\tA = ?MODULE:Field(OldPlayer)," ++ ?NEXT_LINE ++
            "\t\tcase ?MODULE:Field(NewPlayer) of" ++ ?NEXT_LINE ++
            "\t\t\tA -> L;" ++ ?NEXT_LINE ++
            "\t\t\tB -> [{Field,B} | L]" ++ ?NEXT_LINE ++
            "\t\tend" ++ ?NEXT_LINE ++
            "\tend," ++ ?NEXT_LINE ++
            "\tlists:foldl(Fun,[],Fields)." ++ ?NEXT_LINE
    }.

get_key_src(#make_src_info{param = Value})->
    {{get_key,0},"%% @doc 获取表键名\nget_key()-> " ++ com_type:to_list(Value) ++ "." ++ ?NEXT_LINE}.

update_fun_src(#make_src_info{db_name = 'temp',table = Table,param = Param})->
    FieldName = com_type:to_list(Param),
    Doc = "%% @doc 更新缓存数据属性" ++ FieldName++"\n",
    FunName = "update_" ++ FieldName,
    EtsTableName = get_ets_table_name(Table),
    Src = Doc ++ FunName ++ "(Key,Value)->" ++ ?NEXT_LINE ++
        "\tets:update_element(?" ++ EtsTableName ++ ",Key,{get_index(" ++ FieldName ++ "),Value})." ++ ?NEXT_LINE,
    {[{com_type:to_atom(FunName),2}],Src};
update_fun_src(#make_src_info{db_name = DbName,table = Table,param = Param} = MakeSrcInfo)->
    FieldName = com_type:to_list(Param),
    TableName = com_type:to_list(Table),
    Doc = "%% @doc 同步数据属性" ++ FieldName++"\n",
    FunName = "syn_update_" ++ FieldName,
    Src = Doc++FunName ++ "(Key,Value)->" ++ ?NEXT_LINE ++
        "\t" ++ ?DB_INTERFACE ++ ":update(" ++ com_type:to_list(DbName) ++ "," ++ TableName ++ ",[{get_key(),Key}],[" ++ FieldName ++ "],[Value])," ++ ?NEXT_LINE ++
        "\t" ++ "update_" ++ FieldName ++ "(Key,Value)." ++ ?NEXT_LINE,
    {EtsFunName,EtsSrc} = update_fun_src(MakeSrcInfo#make_src_info{db_name = 'temp'}),
    {[{com_type:to_atom(FunName),2}] ++ EtsFunName,Src ++ EtsSrc}.

render_record(Table,Values,Opts,FieldsDict,GenerateInfo)->
    GenerateInfo1 = render_record(Table,Values,Opts,FieldsDict,"-record(" ++ get_record_name(Table) ++ ",{" ++ ?NEXT_LINE,GenerateInfo),
    MakeSrcInfo = #make_src_info{table = Table,param = Values,opts = Opts,fields_dict = FieldsDict},
    GenerateInfo2 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_name_from_index_src/1},GenerateInfo1),
    GenerateInfo3 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_index_from_name_src/1},GenerateInfo2),
    GenerateInfo4 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_all_fields_src/1},GenerateInfo3),
    GenerateInfo5 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_i_src/1},GenerateInfo4),
    GenerateInfo6 = make_src_template(MakeSrcInfo#make_src_info{function = fun is_record_src/1},GenerateInfo5),
    GenerateInfo7 = make_src_template(MakeSrcInfo#make_src_info{function = fun percentage_src/1},GenerateInfo6),
    GenerateInfo8 = make_src_template(MakeSrcInfo#make_src_info{function = fun add_src/1},GenerateInfo7),
    GenerateInfo8.

render_record(_Table,[],_Opts,_FieldsDict,IncludeBuffer,GenerateInfo)->
    append_include(IncludeBuffer,GenerateInfo);
render_record(Table,[Value | T],Opts,FieldsDict,IncludeBuffer,GenerateInfo)->
    Defaults = fetch_fields_dict(default,FieldsDict,[]),
    DataTypes = fetch_fields_dict(data_types,FieldsDict,[]),
    MakeSrcInfo = #make_src_info{table = Table,param = Value,opts = Opts,fields_dict = FieldsDict},
    DefaultValue =
        case lists:keyfind(Value,1,Defaults) of
            false ->
                ?NONE;
            {_,R} ->
                {_,DataType} = lists:keyfind(Value,1,DataTypes),
                case DataType of
                    term ->
                        com_util:term_to_bitstring(R);
                    atom ->
                        com_type:to_list(R);
                    sterm->
                        com_util:term_to_bitstring(R);
                    _ ->
                        R
                end
        end,
    NewIncludeBuffer =
        case T of
            [] ->
                IncludeBuffer ++ "\t\t" ++ com_type:to_list(Value) ++ " = " ++ com_type:to_list(DefaultValue) ++ ?NEXT_LINE ++ "})." ++ ?NEXT_LINE;
            _ ->
                IncludeBuffer ++ "\t\t" ++ com_type:to_list(Value) ++ " = " ++ com_type:to_list(DefaultValue) ++ "," ++ ?NEXT_LINE
        end,
    GenerateInfo1 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_value_src/1},GenerateInfo),
    GenerateInfo2 = make_src_template(MakeSrcInfo#make_src_info{function = fun set_value_src/1},GenerateInfo1),
    render_record(Table,T,Opts,FieldsDict,NewIncludeBuffer,GenerateInfo2).

get_value_src(#make_src_info{table = Table,param = Value})->
    FieldName = com_type:to_list(Value),

    {{Value,1},
            "%% @doc 获取属性<b>"++FieldName++"</b>值\n"++FieldName ++ "(" ++ get_param_name(Table) ++ ")->" ++ ?NEXT_LINE ++
            "\t" ++ get_param_name(Table) ++ "#" ++ get_record_name(Table) ++ "." ++ FieldName ++ "." ++ ?NEXT_LINE}.

set_value_src(#make_src_info{table = Table,param = Value})->
    FieldName = com_type:to_list(Value),
    {{Value,2},
            "%% @doc 设置属性<b>"++FieldName++"</b>值\n"++FieldName ++ "(" ++ get_param_name(Table) ++ ",Value)->" ++ ?NEXT_LINE ++
            "\t" ++ get_param_name(Table) ++ "#" ++ get_record_name(Table) ++ "{" ++ com_type:to_list(Value) ++ " = Value}." ++ ?NEXT_LINE}.

get_index_from_name_src(#make_src_info{table = Table,param = Fields})->
    {{get_index,1},string:join(lists:map(
        fun(Field)->
            FieldName = com_type:to_list(Field),
            "%% @hidden\nget_index(" ++ FieldName ++ ")->" ++ ?NEXT_LINE ++
                "\t#" ++ com_type:to_list(Table) ++ "." ++ FieldName
        end,Fields),";" ++ ?NEXT_LINE) ++ "." ++ ?NEXT_LINE}.

get_name_from_index_src(#make_src_info{table = Table,param = Fields})->
    {{get_name,1},string:join(lists:map(
        fun(Field)->
            FieldName = com_type:to_list(Field),
            "%% @hidden\nget_name(#" ++ com_type:to_list(Table) ++ "." ++ FieldName ++ ")->" ++ ?NEXT_LINE ++
                "\t" ++ FieldName
        end,Fields),";" ++ ?NEXT_LINE) ++ "." ++ ?NEXT_LINE}.
get_all_fields_src(#make_src_info{param = Values})->
    {{get_all_fields,0},"%% @doc 结构属性名列表\nget_all_fields()->" ++ ?NEXT_LINE ++
        "\t" ++ io_lib:format("~p",[Values]) ++ "." ++ ?NEXT_LINE}.
get_i_src(#make_src_info{table = Table})->
    ParamName = get_param_name(Table),
    {[{i,1},{i,2}],
            "i(" ++ ParamName  ++ ")->" ++ ?NEXT_LINE ++
            "\ti(" ++ ParamName ++ ",get_all_fields())." ++ ?NEXT_LINE ++
            "i(" ++ ParamName ++ ",Keys) when erlang:is_list(Keys) ->" ++ ?NEXT_LINE ++
            "\t[i(" ++ ParamName ++ ",Key) || Key <- Keys];" ++ ?NEXT_LINE ++
            "i(" ++ ParamName ++ ",Key)->" ++ ?NEXT_LINE ++
            "\t{Key,?MODULE:Key(" ++ ParamName ++ ")}." ++ ?NEXT_LINE}.

is_record_src(#make_src_info{table = Table})->
    {{is_record,1},"%% @doc 是否为"++com_type:to_list(Table)++"结构\nis_record(Record)-> erlang:is_record(Record," ++ com_type:to_list(Table) ++ ")." ++ ?NEXT_LINE}.

percentage_src(_MakeSrcInfo)->
    {{percentage,2},
            "%%　@doc 计算数值型百分比加成\npercentage(R,[{F,V} | L])->" ++ ?NEXT_LINE ++
            "\tN=" ++ ?NEXT_LINE ++
            "\ttry" ++ ?NEXT_LINE ++
            "\t\tOldValue = ?MODULE:F(R)," ++ ?NEXT_LINE ++
            "\t\t?MODULE:F(R,erlang:trunc(OldValue  * (1 + V / 100)))" ++ ?NEXT_LINE ++
            "\tcatch _:_Reason ->" ++ ?NEXT_LINE ++
%%            "\t\t?LAGER_DEBUG(\"percentage error:~p,field:~p,value:~p\",[Reason,F,V])," ++ ?NEXT_LINE ++
            "\t\tR" ++ ?NEXT_LINE ++
            "\tend," ++ ?NEXT_LINE ++
            "\tpercentage(N,L);" ++ ?NEXT_LINE ++
            "percentage(R,[])->" ++ ?NEXT_LINE ++
            "\tR." ++ ?NEXT_LINE}.

add_src(_MakeSrcInfo)->
    {{add,2},
            "%%　@doc 计算数值型加成\nadd(R,[{F,V} | L])->" ++ ?NEXT_LINE ++
            "\tN=" ++ ?NEXT_LINE ++
            "\ttry" ++ ?NEXT_LINE ++
            "\t\tOldValue = ?MODULE:F(R)," ++ ?NEXT_LINE ++
            "\t\t?MODULE:F(R,erlang:trunc(OldValue + V))" ++ ?NEXT_LINE ++
            "\tcatch _:_Reason ->" ++ ?NEXT_LINE ++
%%            "\t\t?LAGER_DEBUG(\"add error:~p,field:~p,value:~p\",[Reason,F,V])," ++ ?NEXT_LINE ++
            "\t\tR" ++ ?NEXT_LINE ++
            "\tend," ++ ?NEXT_LINE ++
            "\tadd(N,L);" ++ ?NEXT_LINE ++
            "%%　@doc 计算数值型加成\nadd(R,[])->" ++ ?NEXT_LINE ++
            "\tR." ++ ?NEXT_LINE}.

size_src(#make_src_info{table = Table})->
    {{size,0},
            "%% @doc 获取表数据数量\nsize()->" ++ ?NEXT_LINE ++
            "\tets:info(?" ++ get_ets_table_name(Table) ++ ",size)." ++ ?NEXT_LINE
    }.


match_ets(#make_src_info{table = Table}) ->
    {{match_ets,1},
            "%% @doc 搜索数据库缓存\nmatch_ets(Match) ->" ++ ?NEXT_LINE ++
            "\tets:match_object(?"++ get_ets_table_name(Table) ++ ", Match)." ++ ?NEXT_LINE
    }.

match_db(#make_src_info{db_name = DBName, table = Table}) ->
    TableName = com_type:to_list(Table),
    DBTable = get_db_record_name(Table),
    DBTableName = com_type:to_list(DBTable),
    DbName = com_type:to_list(DBName),
    {{match_db, 1},
            "%% @doc 搜索数据库\nmatch_db(Match)->" ++ ?NEXT_LINE ++
            "\tNewMatch = lists:map(fun({F,V}) when erlang:is_integer(F) -> {get_name(F),V};\n\t\t({F,O,V}) when erlang:is_integer(F) ->  {get_name(F),O,V};\n\t\t(M) ->M end, Match),\n" ++
            "\tResults = " ++ ?DB_INTERFACE ++ ":select(" ++ DbName ++ "," ++ TableName ++ ", " ++ get_module(DBTableName)++":get_all_fields(), NewMatch, [])," ++ ?NEXT_LINE ++
            "\t[to_record(Result) || Result <- Results]." ++ ?NEXT_LINE
    }.

render_db_method(Table,Values,Opts,FieldsDict,GenerateInfo)->
    GenerateInfo0 = append_include("-define(" ++ get_ets_table_name(Table) ++ ",ets_table_name_" ++ com_type:to_list(Table) ++ ")." ++ ?NEXT_LINE,GenerateInfo),
    MakeSrcInfo = #make_src_info{table = Table,param = Values,opts = Opts,fields_dict = FieldsDict},
    GenerateInfo1 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_insert_src/1},GenerateInfo0),
    GenerateInfo2 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_update_src/1},GenerateInfo1),
    GenerateInfo3 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_delete_src/1},GenerateInfo2),
    GenerateInfo4 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_select_src/1},GenerateInfo3),
    GenerateInfo5 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_to_db_src/1},GenerateInfo4),
    GenerateInfo6 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_to_ets_src/1},GenerateInfo5),
    GenerateInfo7 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_data_src/1},GenerateInfo6),
    GenerateInfo8 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_init_src/1},GenerateInfo7),
    GenerateInfo9 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_to_record_src/1},GenerateInfo8),
    GenerateInfo10 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_load_src/1},GenerateInfo9),
    GenerateInfo11 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_amendment_src/1},GenerateInfo10),
    GenerateInfo12 = make_src_template(MakeSrcInfo#make_src_info{function = fun get_max_src/1},GenerateInfo11),
    GenerateInfo13 = make_src_template(MakeSrcInfo#make_src_info{function = fun size_src/1},GenerateInfo12),
    GenerateInfo14 = make_src_template(MakeSrcInfo#make_src_info{function = fun match_ets/1},GenerateInfo13),
    GenerateInfo15 = make_src_template(MakeSrcInfo#make_src_info{function = fun match_db/1},GenerateInfo14),
    GenerateInfo15.


get_max_src(#make_src_info{db_name = 'temp'})->
    {[],""};
get_max_src(#make_src_info{db_name = DbName,table = Table})->
    TableName = com_type:to_list(Table),
    {[{max_key,0}],
            "%% @doc 表ｋｅｙ最大值\nmax_key()->" ++ ?NEXT_LINE ++
            "\tdb_helper:max(" ++ com_type:to_list(DbName) ++ "," ++ TableName ++ ",get_key())." ++ ?NEXT_LINE}.

get_insert_src(#make_src_info{db_name = 'temp',table = Table})->
    TableName = com_type:to_list(Table),
    EtsTableName = get_ets_table_name(Table),
    {[{insert,1}],
            "%% @doc 向缓存插入数据\ninsert(Value) when erlang:is_record(Value," ++ TableName ++ ")->" ++ ?NEXT_LINE ++
            "\tets:insert(?" ++ EtsTableName ++ ",Value)." ++ ?NEXT_LINE};
get_insert_src(#make_src_info{db_name = DbName,table = Table} = MakeSrcInfo)->
    DBTable = get_db_record_name(Table),
    TableName = com_type:to_list(Table),
    DBTableName = com_type:to_list(DBTable),
    {EtsFunName,EtsSrc} = get_insert_src(MakeSrcInfo#make_src_info{db_name = 'temp'}),
    {[{syn_insert,1},{syn_insert,2}] ++ EtsFunName,
            "%% @doc 向数据库插入数据\nsyn_insert(Value)->" ++ ?NEXT_LINE ++
            "\tsyn_insert(Value,false)." ++ ?NEXT_LINE ++
            "syn_insert(Value,IsThrowError) when erlang:is_record(Value," ++ DBTableName ++ ")->" ++ ?NEXT_LINE ++
            "\t" ++ ?DB_INTERFACE ++ ":insert(" ++ com_type:to_list(DbName) ++ "," ++ TableName ++ "," ++
            get_module(DBTableName) ++ ":i(Value),not IsThrowError);" ++ ?NEXT_LINE ++
            "syn_insert(Value,IsThrowError) when erlang:is_record(Value," ++ TableName ++ ")->" ++ ?NEXT_LINE ++
            "\tDB = to_db(Value)," ++ ?NEXT_LINE ++
            "\tsyn_insert(DB,IsThrowError)," ++ ?NEXT_LINE ++
            "\tinsert(Value)." ++ ?NEXT_LINE ++ EtsSrc}.

get_update_src(#make_src_info{db_name = 'temp',table = Table})->
    TableName = com_type:to_list(Table),
    EtsTableName = get_ets_table_name(Table),
    {[{update,1}],
            "%% @doc 更新缓存 \nupdate(Value) when erlang:is_record(Value," ++ TableName ++ ")->" ++ ?NEXT_LINE ++
            "\tets:insert(?" ++ EtsTableName ++ ",Value)." ++ ?NEXT_LINE} ;
get_update_src(#make_src_info{db_name = DbName,table = Table} = MakeSrcInfo)->
    DBTable = get_db_record_name(Table),
    TableName = com_type:to_list(Table),
    DBTableName = com_type:to_list(DBTable),
    {EtsFunName,EtsSrc} = get_update_src(MakeSrcInfo#make_src_info{db_name = 'temp'}),
    {[{syn_update,1},{syn_update,2}] ++ EtsFunName,
            "%% @doc 更新数据库表 \nsyn_update(Value)->" ++ ?NEXT_LINE ++
            "\tsyn_update(Value,false)." ++ ?NEXT_LINE ++
            "syn_update(Value,IsThrowError) when erlang:is_record(Value," ++ DBTableName ++ ")->" ++ ?NEXT_LINE ++
            "\tKey = get_key()," ++ ?NEXT_LINE ++
            "\tKeyIndex = " ++ get_module(DBTableName) ++ ":get_index(Key)," ++ ?NEXT_LINE ++
            "\t" ++ ?DB_INTERFACE ++ ":update(" ++ com_type:to_list(DbName) ++ "," ++ TableName ++ "," ++ ?NEXT_LINE ++
            "\t\t[{Key,erlang:element(KeyIndex,Value)}]," ++ ?NEXT_LINE ++
            "\t\t" ++ get_module(DBTableName) ++ ":i(Value),not IsThrowError);" ++ ?NEXT_LINE ++
            "syn_update(Value,IsThrowError) when erlang:is_record(Value," ++ TableName ++ ")->" ++ ?NEXT_LINE ++
            "\tDB = to_db(Value)," ++ ?NEXT_LINE ++
            "\tsyn_update(DB,IsThrowError)," ++ ?NEXT_LINE ++
            "\tupdate(Value)." ++ ?NEXT_LINE ++ EtsSrc}.


get_delete_src(#make_src_info{db_name = 'temp',table = Table})->
    EtsTableName = get_ets_table_name(Table),
    {[{delete,1}],
            "%% @doc 删除缓存\ndelete(Value)->" ++ ?NEXT_LINE ++
            "\tets:delete(?" ++ EtsTableName ++ ",Value)." ++ ?NEXT_LINE};
get_delete_src(#make_src_info{db_name = DbName,table = Table} = MakeSrcInfo)->
    TableName = com_type:to_list(Table),
    {EtsFunName,EtsSrc} = get_delete_src(MakeSrcInfo#make_src_info{db_name = 'temp'}),
    {[{syn_delete,1}] ++ EtsFunName,
            "%% @doc 删除数据\nsyn_delete(Value)->" ++ ?NEXT_LINE ++
            "\tKey = get_key()," ++ ?NEXT_LINE ++
            "\t" ++ ?DB_INTERFACE ++ ":delete(" ++ com_type:to_list(DbName) ++ "," ++ TableName ++ ",[{Key,Value}])," ++ ?NEXT_LINE ++
            "\tdelete(Value)." ++ ?NEXT_LINE ++ EtsSrc}.

get_select_src(#make_src_info{db_name = 'temp'})->
    {[],""};
get_select_src(#make_src_info{db_name = DBName,table = Table})->
    TableName = com_type:to_list(Table),
    DBTable = get_db_record_name(Table),
    DBTableName = com_type:to_list(DBTable),
    DbName = com_type:to_list(DBName),
    {[{select,0},{select,1},{select_fields,1}],
            "%% @doc 查询数据库\nselect()->" ++ ?NEXT_LINE ++
            "\t" ++ ?DB_INTERFACE ++ ":select(" ++ DbName ++ "," ++ TableName ++ "," ++ get_module(DBTableName) ++ ":get_all_fields())." ++ ?NEXT_LINE ++
            "select(Value)->" ++ ?NEXT_LINE ++
            "\tKey = get_key()," ++ ?NEXT_LINE ++
            "\t" ++ ?DB_INTERFACE ++ ":select(" ++ DbName ++ "," ++ TableName ++ "," ++
            get_module(DBTableName) ++ ":get_all_fields(),[{Key,Value}],[1])." ++ ?NEXT_LINE ++
            "select_fields(Fields)->" ++ ?NEXT_LINE ++
            "\t" ++ ?DB_INTERFACE ++ ":select(" ++ DbName ++ "," ++ TableName ++ ",Fields)." ++ ?NEXT_LINE}.

get_to_db_src(#make_src_info{db_name = 'temp'})->
    {[],""};
get_to_db_src(#make_src_info{table = Table,param = DBFields,fields_dict = FieldsDict})->
    DBTable = get_db_record_name(Table),
    TableName = com_type:to_list(Table),
    DBTableName = com_type:to_list(DBTable),
    DataTypes = fetch_fields_dict(data_types,FieldsDict,[]),
    F =
        fun(Field,AccIn)->
            {Field,Type} = keyfind(Field,1,DataTypes),
            FieldName = com_type:to_list(Field),
            Result =
                case Type of
                    term ->
                        "\t\t" ++ FieldName ++ " = com_util:term_to_bitstring(" ++ FieldName ++ "(Value))";
                    sterm->
                        "\t\t" ++ FieldName ++ " = com_util:term_to_bitstring(" ++ FieldName ++ "(Value))";
                    _ ->
                        "\t\t" ++ FieldName ++ " = " ++ FieldName ++ "(Value)"
                end,
            [Result | AccIn]
        end,
    KeyValues = string:join(lists:foldr(F,[],DBFields),"," ++ ?NEXT_LINE),
    {{to_db,1},"%% @doc 转换成数据库表结构\nto_db(Value) when erlang:is_record(Value," ++ DBTableName ++ ")-> Value;" ++ ?NEXT_LINE ++
        "to_db(Value) when erlang:is_record(Value," ++ TableName ++ ")->" ++ ?NEXT_LINE ++
        "\t#" ++ DBTableName ++ "{" ++ ?NEXT_LINE ++
        KeyValues ++ "}." ++ ?NEXT_LINE}.
get_to_ets_src(#make_src_info{db_name = 'temp'})->
    {[],""};
get_to_ets_src(#make_src_info{table = Table,param = DBFields,fields_dict = FieldsDict})->
    DBTable = get_db_record_name(Table),
    TableName = com_type:to_list(Table),
    DBTableName = com_type:to_list(DBTable),
    DataTypes = fetch_fields_dict(data_types,FieldsDict,[]),
    F =
        fun(Field,AccIn)->
            {Field,Type} = lists:keyfind(Field,1,DataTypes),
            FieldName = com_type:to_list(Field),
            Result =
                case Type of
                    atom ->
                        "\t\t" ++ FieldName ++ " = com_type:to_atom(" ++ get_module(DBTableName) ++ ":" ++  FieldName ++ "(Value))";
                    term ->
                        "\t\t" ++ FieldName ++ " = com_util:bitstring_to_term(" ++ get_module(DBTableName) ++ ":" ++  FieldName ++ "(Value))";
                    sterm ->
                        "\t\t" ++ FieldName ++ " = com_util:bitstring_to_term(" ++ get_module(DBTableName) ++ ":" ++  FieldName ++ "(Value))";
                    string->
                        "\t\t" ++ FieldName ++ " = com_type:to_list(" ++ get_module(DBTableName) ++ ":" ++  FieldName ++ "(Value))";
                    _ ->
                        "\t\t" ++ FieldName ++ " = " ++ get_module(DBTableName) ++ ":" ++  FieldName ++ "(Value)"
                end,
            [Result | AccIn]
        end,
    KeyValues = string:join(lists:foldr(F,[],DBFields),"," ++ ?NEXT_LINE),
    {{to_ets,1},
            "%% @doc 转换成缓存表结构\nto_ets(Value) when erlang:is_record(Value," ++ TableName ++ ")-> Value;" ++ ?NEXT_LINE ++
            "to_ets(Value) when erlang:is_record(Value," ++ DBTableName ++ ")-> " ++ ?NEXT_LINE ++
            "\t#" ++ TableName ++ "{" ++ ?NEXT_LINE ++
            KeyValues ++ "}." ++ ?NEXT_LINE}.

get_data_src(#make_src_info{table = Table,db_name = DbName})->
    EtsTableName = get_ets_table_name(Table),
    Src1 = "%% @doc 获取数据\nget_data(Key)->" ++ ?NEXT_LINE ++
        "\tget_data(Key,#" ++ com_type:to_list(Table) ++ "{})." ++ ?NEXT_LINE ++
        "%% @doc 获取数据\nget_data(Key,Default)->" ++ ?NEXT_LINE ++
        "\tcase lookup(Key) of" ++ ?NEXT_LINE ++
        "\t\tnone ->" ++ ?NEXT_LINE,
    Src2 =
        case DbName of
            temp ->
                "\t\t\tDefault;" ++ ?NEXT_LINE;
            _ ->
                "\t\t\tcase select(Key) of" ++ ?NEXT_LINE ++
                    "\t\t\t\t[] -> Default;" ++ ?NEXT_LINE ++
                    "\t\t\t\t[Value] ->" ++ ?NEXT_LINE ++
                    "\t\t\t\t\tEtsRecord = to_record(Value)," ++ ?NEXT_LINE ++
                    "\t\t\t\t\tets:insert(?" ++ EtsTableName ++ ",EtsRecord)," ++ ?NEXT_LINE ++
                    "\t\t\t\t\tEtsRecord" ++ ?NEXT_LINE ++
                    "\t\t\tend;" ++ ?NEXT_LINE
        end,
    Src3 = "\t\tR -> R" ++ ?NEXT_LINE ++
        "\tend." ++ ?NEXT_LINE ++
        "%% @doc 获取缓存数据\nlookup(Key)->" ++ ?NEXT_LINE ++
        "\tlookup(Key,none)." ++ ?NEXT_LINE ++
        "%% @doc 获取缓存数据\nlookup(Key,Default)->" ++ ?NEXT_LINE ++
        "\tcase ets:lookup(?" ++ EtsTableName ++ ",Key) of" ++ ?NEXT_LINE ++
        "\t\t[] -> Default;" ++ ?NEXT_LINE ++
        "\t\t[R] -> R" ++ ?NEXT_LINE ++
        "\tend." ++ ?NEXT_LINE,
    {[{get_data,1},{get_data,2},{lookup,1},{lookup,2}],
            Src1 ++ Src2 ++ Src3}.


get_init_src(#make_src_info{table = Table})->
    EtsTableName = get_ets_table_name(Table),
    {[{init,0},{ets_i,0},{foldl,2},{foldr,2},{all_ets_data,0}],
            "%% @doc 初始化\ninit()->" ++ ?NEXT_LINE ++
            "\tets:new(?" ++ EtsTableName ++ ",[named_table,{keypos,get_index(get_key())},set,public])." ++ ?NEXT_LINE ++
            "%% @doc 缓存表属性\nets_i()->" ++ ?NEXT_LINE ++
            "\tets:i(?" ++ EtsTableName ++ ")." ++ ?NEXT_LINE ++
            "%% @doc 迭代缓存表\nfoldl(F,Acc)->" ++ ?NEXT_LINE ++
            "\tets:foldl(F,Acc,?" ++ EtsTableName ++ ")." ++ ?NEXT_LINE ++
            "foldr(F,Acc)->" ++ ?NEXT_LINE ++
            "\tets:foldr(F,Acc,?" ++ EtsTableName ++ ")." ++ ?NEXT_LINE ++
            "%% @doc 获取缓存所有数据\nall_ets_data()->" ++ ?NEXT_LINE ++
            "\tets:tab2list(?" ++ EtsTableName ++ ")." ++ ?NEXT_LINE}.

get_to_record_src(#make_src_info{db_name = 'temp'})->
    {[],""};
get_to_record_src(#make_src_info{table = Table})->
    DBTable = get_db_record_name(Table),
    DBTableName = com_type:to_list(DBTable),
    {{to_record,1},"%% @doc 数据转结构\nto_record(Values)->" ++ ?NEXT_LINE ++
        "\tRecord = com_util:to_record(" ++ DBTableName ++ ",Values)," ++ ?NEXT_LINE ++
        "\tto_ets(Record)." ++ ?NEXT_LINE}.

get_load_src(#make_src_info{db_name = 'temp'})->
    {[],""};
get_load_src(#make_src_info{table = Table})->
    DBTable = get_db_record_name(Table),
    {[{load,2},{load,1},{load,0}],
            "%% @doc 加载数据库数据到缓存\nload(Fun,Fields)->" ++ ?NEXT_LINE ++
            "\tlists:foreach(Fun,select_fields(Fields))." ++ ?NEXT_LINE ++
            "%% @doc 加载数据库数据到缓存\nload(Fun)->" ++ ?NEXT_LINE ++
            "\tF = " ++ ?NEXT_LINE ++
            "\tfun(Data)->" ++ ?NEXT_LINE ++
            "\t\tRecord = to_record(Data)," ++ ?NEXT_LINE ++
            "\t\tcase Fun(Record) of" ++ ?NEXT_LINE ++
            "\t\t\tnone-> insert(Record);" ++ ?NEXT_LINE ++
            "\t\t\tignore-> skip;" ++ ?NEXT_LINE ++
            "\t\t\tR -> insert(R)" ++ ?NEXT_LINE ++
            "\t\tend" ++ ?NEXT_LINE ++
            "\tend," ++ ?NEXT_LINE ++
            "\tload(F," ++ get_module(DBTable) ++ ":get_all_fields())." ++ ?NEXT_LINE ++
            "load()->" ++ ?NEXT_LINE ++
            "\tload(fun(_)->none end)." ++ ?NEXT_LINE}.

get_amendment_src(#make_src_info{db_name = 'temp'})->
    {[],""};
get_amendment_src(#make_src_info{db_name = DbName,table = Table})->
    DBTable = get_db_record_name(Table),
    TableName = com_type:to_list(Table),
    DBTableName = com_type:to_list(DBTable),
    Module = get_module(DBTableName),
    {{amendment,0},
            "%% @doc 修正数据库表结构\namendment()->" ++ ?NEXT_LINE ++
            "\tdb_helper:amendment(" ++ com_type:to_list(DbName) ++ "," ++ TableName ++ ",get_fields())." ++ ?NEXT_LINE ++
            "get_fields()->" ++ ?NEXT_LINE ++
            "\tKey = get_key()," ++ ?NEXT_LINE ++
            "\tF =" ++ ?NEXT_LINE ++
            "\tfun(Field)->" ++ ?NEXT_LINE ++
            "\t\tDBRecord = #db_table_field{name = Field,type = data_type(Field),key='',null='NO',default=default(Field)}," ++ ?NEXT_LINE ++
            "\t\tcase Field of" ++ ?NEXT_LINE ++
            "\t\t\tKey -> DBRecord#db_table_field{key = 'PRI'};" ++ ?NEXT_LINE ++
            "\t\t\t_ -> DBRecord" ++ ?NEXT_LINE ++
            "\t\tend" ++ ?NEXT_LINE ++
            "\tend," ++ ?NEXT_LINE ++
            "\tlists:map(F," ++ Module ++ ":get_all_fields())." ++ ?NEXT_LINE}.

get_param_name(Str) when is_list(Str) ->
    StrList = string:tokens(Str,"_"),
    F =
        fun([H | T])->
            [string:to_upper(H) | T]
        end,
    lists:concat([F(S) || S <- StrList]);
get_param_name(Param)->
    get_param_name(com_type:to_list(Param)).

get_db_record_name(Table)->
    com_type:to_atom("db_" ++ com_type:to_list(Table)).

get_record_name(Table)->
    com_type:to_list(Table).

get_ets_table_name(Table)->
    "ETS_TABLE_" ++ string:to_upper(com_type:to_list(Table)).

global_render(export,_Values,Table,Opts,_FieldsDict)->
    RenderInfo = dict:append(module,{Table,Table},render_info_new()),
    RenderInfo1 = dict:append(include,Table,RenderInfo),
    R =
        case lists:keyfind(save_type,1,Opts) of
            {_,temp} ->
                temp;
            {_,{db,DbName}} ->
                DbName
        end,
    RenderInfo2 = dict:append({db,R},Table,RenderInfo1),
    case get({db,R}) of
        undefined ->
            put({db,R},true),
            dict:append(db,R,RenderInfo2);
        _ ->
            RenderInfo2
    end;

global_render(syn_fields,_Values,Table,_Opts,_FieldsDict)->
    dict:append(module,{Table,get_db_record_name(Table)},render_info_new());
global_render(_Type,_Values,_Table,_Opts,_FieldsDict)->
    render_info_new().

render_generate_info(GenerateInfo,Opts)->
    Include = generate_include_file(GenerateInfo),
    generate_src_file(GenerateInfo,Include).

generate_include_file(GenerateInfo)->
    IncludeFiles = fetch_fields_dict({global_render,include},GenerateInfo,[]),
    lists:map(
        fun(File)->
            fetch_fields_dict({render,File,include_code},GenerateInfo,"")
        end,IncludeFiles).

generate_src_file(GenerateInfo,Include)->
    generate_record_data(GenerateInfo, Include).

generate_record_data(GenerateInfo, Include)->
    Modules = fetch_fields_dict({global_render,module},GenerateInfo,[]),
    lists:foreach(
        fun({Module,FileName})->
            BodyCode = fetch_fields_dict({render,Module,{src_code,FileName}},GenerateInfo,""),
            FunList = fetch_fields_dict({render,Module,{funs,FileName}},GenerateInfo,[]),
            ModuleName = com_type:to_list(FileName),
            Result =
                "-module(" ++ get_module(ModuleName) ++ ")." ++ ?NEXT_LINE ++
                Include ++ "-record(db_table_field, {name,type,key='',null='NO',default,auto='NO'}).\n" ++
                    "-define(MODULE, "++get_module(ModuleName)++").\n"++
                "-export([" ++ ?NEXT_LINE ++string:join([generate_fun(Fun) || Fun <- FunList],"," ++ ?NEXT_LINE) ++
                ?NEXT_LINE ++ "\t\t])." ++ ?NEXT_LINE ++
                BodyCode,
            Characters = unicode:characters_to_list(Result),
            dynamic_compile:load_from_string(Characters)
        end,Modules),
    ok.


generate_fun({FunName,Arity})->
    "\t\t\t" ++ com_type:to_list(FunName) ++ "/" ++ com_type:to_list(Arity).


get_module(ModuleName)->
    "record_" ++ com_type:to_list(ModuleName).

keyfind(Key, Pos, List) ->
    case lists:keyfind(Key,Pos, List) of
        false->
            ?format_line("keyfind(~p,~p,~p) false",[Key, Pos, List]),halt();
        Ret -> Ret
    end.