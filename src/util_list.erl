%% @author winecat
%% @doc fun for list


-module(util_list).

%% include files
%% ---------------------------------
-include("util.hrl").


%% API functions
%% ---------------------------------
-export([
         rand/2
         ,rand_list/1
         ,get_rand_list/2
         ,nth_replace/3
         ,suffle_list/1
         ,rand_by_weight/2
         ,delete_list/2
         ,index_of/2
         ,md5/1
         ,merge_kv/1
        ]).


%% @doc 产生一个介于Min到Max之间的随机整数
-spec rand(Min, Max) -> Random | type_error() when
    Min :: integer(),
    Max :: integer(),
    Random :: integer().
rand(Min, Min) -> Min;
rand(Min, Max) when Min > Max -> ?TYPE_ERROR("params error:Max > Min", [Min, Max]);
rand(Min, Max) ->
    case erlang:get(rand_seed) of
        undefined ->
            Seed = erlang:now(),
            random:seed(Seed),
            erlang:put(rand_seed, Seed),
            ok;
        _ -> ignore
    end,
    M = Min - 1,
    random:uniform(Max - M) + M.

%% @doc 从一个list中随机取出一项
-spec rand_list(List) -> Result when
    List :: list(),
    Result :: term() | type_error().
rand_list([]) -> ?TYPE_ERROR("rand list is empty", []);
rand_list([Elem]) -> Elem;
rand_list(List) ->
    Len = length(List),
    rand_list(Len, List).

%% @doc 从列表中不重复选择N个元素，返回选中元素构成的新列表,当N>=length(List)时，只能返回长度为N的list
-spec get_rand_list(N, List) -> RandList when
    N :: integer(),
    List :: list(),
    RandList :: list().
get_rand_list(0, _List) -> [];
get_rand_list(N, List) ->
    Len = length(List),
    case Len =< N of
        true -> List;
        false ->
            get_rand_list(N, Len, List, [])
    end.
get_rand_list(N, _Len, _List, RandList) when N =< 0 -> RandList;
get_rand_list(N, Len, List, RandList) ->
    Elem = rand_list(Len, List),
    get_rand_list(N-1, Len-1, lists:delete(Elem, List), [Elem|RandList]).


%% @doc 替换list中的第N个元素
-spec nth_replace(List, Nth, NewVal) -> NewList  | type_error() when
    List :: list(),
    Nth :: integer(),
    NewVal :: term(),
    NewList :: list().
nth_replace(List, Nth, NewVal) when Nth > 0 ->
    Len = length(List),
    case Len < Nth of
        true -> ?TYPE_ERROR("nth_raplace nth is more than the length of list", [Len, Nth]);
        false when Nth > Len/2 ->%%翻转一次
            lists:reverse(nth_replace(lists:reverse(List), Len-Nth+1, NewVal, [], 1));
        false ->
            nth_replace(List, Nth, NewVal, [], 1)
    end.
nth_replace([_Elem|Tail], Nth, NewVal, ReverseList, Nth) ->
    lists:reverse(ReverseList, [NewVal|Tail]);
nth_replace([Elem|Tail], Nth, NewVal, ReverseList, Count) ->
    nth_replace(Tail, Nth, NewVal, [Elem|ReverseList], Count+1).

%% @doc 随机返回一个乱序的list
-spec suffle_list(List :: list()) -> NList :: list().
suffle_list(List) ->
    RandMax = length(List) * 10,
    TmpList = [{rand(0, RandMax), X} || X <- List],
    SortList = lists:keysort(1, TmpList),
    [E || {_, E} <- SortList].

%% @doc 根据权重列表随机对应的elem，循环Count次
-spec rand_by_weight(Count, List) -> ResultList when
    Count :: integer(),
    List :: list({Id, Weight}),
    ResultList :: list({Id, Num}),
    Weight :: integer(),
    Id :: integer(),
    Num :: integer().
rand_by_weight(0, _List) -> [];
rand_by_weight(_Count, []) -> [];
rand_by_weight(Count, List) -> do_rand_by_weight(Count, List).

%% @doc 把List1从List2中全部删除掉，返回一个新的不包含List1的任何一个元素的list
-spec delete_list(List1 :: list(), List2 :: list()) -> NList :: list().
delete_list([], List2) -> List2;
delete_list(_List1 = [Elem|Tail], List2) ->
    delete_list(Tail, lists:delete(Elem, List2)).

%% @doc 元素位于列表中的位置，从1开始;没有找到的话返回0
-spec index_of(List :: list(), Elem :: term()) -> Index :: 0 | integer().
index_of(List, Elem) ->
    string:str(List, [Elem]).

%% @doc 生成16位格式的md5值
-spec md5(String :: list()) -> Md5Bin :: binary().
md5(String) ->
    list_to_binary([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(String))]).

%% @doc 合并相同的键值对
-spec merge_kv(List :: list({Key :: term(), Val :: integer()})) -> MergeList :: list().
merge_kv(List) ->
    merge_kv(List, []).
merge_kv([], RTList) -> RTList;
merge_kv([{Key, Val1}|Tail], RTList) ->
    case lists:keytake(Key, 1, RTList) of
        false ->
            merge_kv(Tail, [{Key, Val1}|RTList]);
        {value, {_Key, Val2}, TailRTList} ->
            merge_kv(Tail, [{Key, Val1 + Val2}|TailRTList])
    end.

%% Internal functions
%% ---------------------------------
%% @doc 从一个list中随机出一个elem返回，
%% @doc 必须保证 Len =< length(List)
rand_list(Len, List) ->
    Index = rand(1, Len),
    lists:nth(Index, List).

do_rand_by_weight(Count, List) ->
    {TotalWeight, WeightList, ListLen} = sort_weight(List),
    rand_weight(Count, {WeightList, TotalWeight, ListLen}, []).

rand_weight(Count, _WeightInfo, RList) when Count =< 0 -> RList;
rand_weight(Count, WeightInfo = {WeightList, TotalWeight, ListLen}, RList) ->
    RandWeight = util:rand(1, TotalWeight),
    {ItemId, _W} = find_weight(WeightList, 0, ListLen, RandWeight),
    %%?DEBUG(" RandWeight:~w~n WeightList:~w~n TotalWeight:~w~n ListLen:~w~n RandElem:~w~n", [RandWeight, WeightList, TotalWeight, ListLen, {ItemId, _W}]),
    NRList = case lists:keyfind(ItemId, 1, RList) of
                 false -> [{ItemId, 1}|RList];
                 {_ItemId, Num} -> lists:keyreplace(ItemId, 1, RList, {ItemId, Num+1})
             end,
    rand_weight(Count-1, WeightInfo, NRList).
%%排序,权重总和
sort_weight(List) ->
    {TotalWeight, WeightList, ListLen} = 
        lists:foldl(fun({Elem, Weight}, {LW, WList, Len}) ->
                            RW = LW + Weight,
                            {RW, [{Elem, Weight, RW}|WList], Len + 1}
                    end, {0, [], 0}, List),
    {TotalWeight, lists:reverse(WeightList), ListLen}.
%%二分查找
find_weight([{ItemId, W, _RW}], _Left, _Right, _WNum) -> {ItemId, W};
find_weight([{ItemId1, W1, RW1}, {_ItemId2, _W2, _RW2}], _Left, _Right, WNum) when WNum =< RW1 -> {ItemId1, W1};
find_weight([{_ItemId1, _W1, _RW1}, {ItemId2, W2, _RW2}], _Left, _Right, _WNum) -> {ItemId2, W2};
find_weight([{First, W, _}|_Tail], 0, 1, _WNum)->
    {First, W};
find_weight(List, Left, Right, WNum) ->
    Middle = trunc((Left + Right) / 2),
    {ItemId, W, RW} = lists:nth(Middle, List),
    LW = RW - W,
    case WNum > LW andalso WNum =< RW of
        true -> 
            {ItemId, W};
        false when WNum =< LW ->
            find_weight(List, Left, Middle - 1, WNum);
        false ->
            find_weight(List, Middle + 1, Right, WNum)
    end.
