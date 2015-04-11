%% @author mxr
%% @doc @todo Add description to user_default.


-module(user_default).

-define(TEST, true).%% test true

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         ch_fun_avg/4
         ,run_commands/1
         ,get_file_lines/1
         ,get_code/1
         ,int_ceil/1
         ,int_pow/2
         ,count_char_nums/1
        ]).



%% @doc 执行方法，求平均数据
-spec ch_fun_avg(M :: atom(), F :: function(), A :: list(), CheckCount :: integer()) -> any().
ch_fun_avg(M, F, A, CheckCount) when CheckCount > 1 ->
    TList = erlang:t1(lists:reverse(tc_loop(M, F, A, CheckCount))),
    Len = length(TList),
    Min = lists:min(TList),
    Max = lists:max(TList),
    Med = lists:nth(round(Len/2), lists:sort(TList)),
    Avg = round(lists:sum(TList)/Len),
    io:format("Range:~b - ~b mics \n"
             "Median: ~b mics \n"
             "Average: ~b mics \n", 
             [Min, Max, Med, Avg]).

tc_loop(M, F, A, Num) ->
    tc_loop(M, F, A, Num, []).
tc_loop(_M, _F, _A, 0, TList) -> TList;
tc_loop(M, F, A, Num, TList) ->
    case timer:tc(M, F, A) of
        {_T, {'EXIT', Reason}} -> exit(Reason);
        {T, _Value} -> tc_loop(M, F, A, Num-1, [T|TList])
    end.

%% @doc 执行系统命令
-spec run_commands(CommandList :: list()) -> any().
run_commands(CommandList) ->
    Result = os:cmd(string:join(CommandList, " ")),
    io:format("Result:~s\n", [Result]).

%% @doc 遍历存在的文件，返回行数和以行文单位的数据列表
-spec get_file_lines(File :: atom()) -> {Lines :: integer(), DataList :: list()}. 
get_file_lines(File) ->
    get_file_lines(File, 0, []).
get_file_lines(File, Lines, DataList) ->
    case file:read_line(File) of
        eof -> {Lines, lists:reverse(DataList)};
        {ok, Data} ->
            get_file_lines(File, Lines + 1, [Data | DataList])
    end.

%% @doc 从Beam中提取代码
-spec get_code(Beam :: atom()) -> term().
get_code(Beam) ->
    {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Beam,[abstract_code]),
    io:fwrite("~s~n", [erl_prettypr:format(erl_syntax:form_list(AC))]).


%% @doc  Return the ceiling of F as an integer. The ceiling is defined as
%%       F when F == trunc(F);
%%       trunc(F) when F &lt; 0;
%%       trunc(F) + 1 when F &gt; 0.
-spec int_ceil(F::float()) -> integer().
int_ceil(X) ->
    T = trunc(X),
    case (X - T) of
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.


%% @doc  Moderately efficient way to exponentiate integers.
%%       int_pow(10, 2) = 100.
-spec int_pow(X::integer(), N::integer()) -> Y::integer().
int_pow(_X, 0) ->
    1;
int_pow(X, N) when N > 0 ->
    int_pow(X, N, 1).
int_pow(X, N, R) when N < 2 ->
    R * X;
int_pow(X, N, R) ->
    int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).

%% @doc 统计一段文字里的中文和字符个数
-spec count_char_nums(String :: string()) -> {EnNum :: integer(), ZhNum :: integer()}. 
count_char_nums(String) ->
    List = unicode:characters_to_list(list_to_binary(String)),
    count_char_nums(List, 0, 0).
count_char_nums([], EnNum, ZhNum) -> {EnNum, ZhNum};
count_char_nums([E|Tail], EnNum, ZhNum) when E =< 255 -> 
    count_char_nums(Tail, EnNum + 1, ZhNum);
count_char_nums([E|Tail], EnNum, ZhNum) when E >= 16#4e00 andalso E =< 16#9fff -> 
    count_char_nums(Tail, EnNum, ZhNum + 1);
count_char_nums([_E|Tail], EnNum, ZhNum) -> 
    count_char_nums(Tail, EnNum, ZhNum).

%% ====================================================================
%% Internal functions
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

count_char_nums_test() ->
    A = "haha哈哈唔知abd",
    ?assertEqual({7, 4}, count_char_nums(A)).

-endif.

