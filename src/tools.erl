%% @author winecat
%% @doc @todo Add description to tools.


-module(tools).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% error macro
-type type_error() :: 'erlang exception error'.
-define(TYPE_ERROR(MSG, ARGS), erlang:error(MSG, ARGS)).

-define(ONE_DAY_SECONDS, 84600).	%% one day seconds
-define(ONE_HOUR_SECONDS, 3600).	%% one hoer seconds
-define(ONE_MINUTE_SECONDS, 60).	%% one minute seconds
-define(ONE_WEEK_SECONDS, 592200).	%% one week seconds
-define(ZERO_TO_1970_DAYS, 719528).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         floor/1
         ,ceil/1
         ,at_least/2
         ,at_most/2
         ,check_range/3
        
         ,seconds_to_datetime/1
         ,unixtime/0
         ,unixtime/1
         ,datetime_to_seconds/1
         ,is_same_day/2
         ,is_today/1
         ,day_of_the_week/0
         ,day_of_the_week/1
         ,unixtime_to_now/1
        
         ,rand/2
         ,rand_list/1
         ,get_rand_list/2
         ,nth_replace/3
         ,suffle_list/1
         ,rand_by_weight/2
         ,delete_list/2
         ,index_of/2
         ,md5/1
         ,ip2bitstring/1
         ,ip2bitstring/4
         ,merge_kv/1
         ,calc_earth_distance/2
        ]).

%% floor/2
%% -----------------------------------------------------------
%% @doc 向下取整
-spec floor(X) -> FloorX when
    X :: integer(),
    FloorX :: integer().
%% -----------------------------------------------------------
floor(X) ->
    Tmp = erlang:trunc(X),
    case X < Tmp of
        true -> Tmp -1;
        false -> Tmp
    end.

%% ceil/2
%% -----------------------------------------------------------
%% @doc 向上取整
-spec ceil(X) -> CeilX when
    X :: integer(),
    CeilX :: integer().
%% -----------------------------------------------------------
ceil(X) ->
    Tmp = erlang:trunc(X),
    case X > Tmp of
        true -> Tmp +1;
        false -> Tmp
    end.
    
%% at_least/2
%% -----------------------------------------------------------
%% @doc 最小值限制
-spec at_least(Val :: number(), Min :: number()) -> Result :: number().
%% -----------------------------------------------------------
at_least(Val, Min) ->
    erlang:max(Val, Min).

%% at_most/2
%% -----------------------------------------------------------
%% @doc 最大值限制
-spec at_most(Val :: number(), Max :: number()) -> Result :: number().
%% -----------------------------------------------------------
at_most(Val, Max) ->
    erlang:min(Val, Max).

%% check_range/3
%% -----------------------------------------------------------
%% @doc 做取值范围限制
-spec check_range(Val :: number(), Min :: number(), Max :: number()) -> Result :: number().
%% -----------------------------------------------------------
check_range(_Val, Min, Max) when Min > Max -> ?TYPE_ERROR("the value of Min is more than Max, Min/Max : ~w/~w", [Min, Max]);
check_range(Val, Min, Max) ->
    if
        Val > Max -> Max;
        Val < Min -> Min;
        true -> Val
    end.
    
%% seconds_to_datetime/1
%% -----------------------------------------------------------
%% @doc 将unix时间戳转换成当地日期时间
-spec seconds_to_datetime(UnixTime) -> {{Year, Month, Day}, {Hour, Minute, Second}} when
    UnixTime :: integer(),
    Year :: integer(),
    Month :: integer(),
    Day :: integer(),
    Hour :: integer(),
    Minute :: integer(),
    Second :: integer().
%% -----------------------------------------------------------
seconds_to_datetime(Unixtime) ->
    Local = erlang:universaltime_to_localtime({{1970, 1, 1}, {0, 0, 0}}),
    LocalStamp = calendar:datetime_to_gregorian_seconds(Local),
    TimeStamp = Unixtime + LocalStamp,
    calendar:gregorian_seconds_to_datetime(TimeStamp).

%% unixtime/0
%% -----------------------------------------------------------
%% @doc 当前的unix时间戳(s)
-spec unixtime() -> Second when
    Second :: integer().
%% -----------------------------------------------------------
unixtime() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

%% unixtime/1
%% -----------------------------------------------------------
%% @doc 根据传入不同的type，返回不同的时间戳(s)
-spec unixtime(Type) -> Second when
    Type :: atom(),
    Second :: integer().
%% -----------------------------------------------------------
unixtime(ms) ->
    {S1, S2, S3} = erlang:now(),
    trunc(S1 * 1000000000 + S2 * 1000 + S3 / 1000);

%% @doc 取得当前时间所在小时0分0秒时间戳
unixtime(hour) ->
    {M, S, MS} = now(),
    {_, {_Hour, Minute, Second}} = calendar:now_to_local_time({M, S, MS}),
    M * 1000000 + S - calendar:time_to_seconds({0, Minute, Second});

%% @doc 取得下一个小时0分0秒时间戳
unixtime(nexthour) ->
    unixtime(hour) + 3600;

%% @doc 获取当天0时0分0秒的时间戳（这里是相对于当前时区而言，后面的unixtime调用都基于这个函数)
unixtime(today) ->
    {M, S, MS} = now(),
    {_, Time} = calendar:now_to_local_time({M, S, MS}), %% 性能几乎和之前的一样
    M * 1000000 + S - calendar:time_to_seconds(Time);

%% @doc 获取明天0时0分0秒的时间戳（这里是相对于当前时区而言)
unixtime(tomorrow) ->
    unixtime(today) + ?ONE_DAY_SECONDS;

%% @doc 获取昨天0时0分0秒的时间戳（这里是相对于当前时区而言)
unixtime(yesterday) ->
    unixtime(today) - ?ONE_DAY_SECONDS;

%% @doc 获取本周周一00:00:00的时间戳
unixtime(thisweek) ->
    {Date, _} = calendar:local_time(),
    Week = calendar:day_of_the_week(Date),
    Today = unixtime(today),
    Today - (Week - 1) * ?ONE_DAY_SECONDS;

%% @doc 获取某时间戳的00:00:00的时间戳当Unixtime为0时，返回值有可能是负值，因为这里有时区偏移值（例如北京时间就可能是-28800
unixtime({today, Unixtime}) ->
    Base = unixtime(today),  %% 当前周一
    case Unixtime > Base of
        false -> Base - tools:ceil((Base - Unixtime) / ?ONE_DAY_SECONDS) * ?ONE_DAY_SECONDS;
        true -> (Unixtime - Base) div ?ONE_DAY_SECONDS * ?ONE_DAY_SECONDS + Base
    end;

%% @doc 获取某时间戳的第二天的00:00:00的时间戳
unixtime({tomorrow, UnixTime}) ->
    unixtime({today, UnixTime}) + ?ONE_DAY_SECONDS;

%% @doc 获取某时间戳的前一天的00:00:00的时间戳
unixtime({yesterday, UnixTime}) ->
    unixtime({today, UnixTime}) - ?ONE_DAY_SECONDS;

%% @doc 获取某时间戳所在周的周一00:00:00的时间戳
unixtime({thisweek, UnixTime}) ->
    {Date, _} = tools:seconds_to_datetime(UnixTime),
    Week = calendar:day_of_the_week(Date),
    Today = unixtime({today, UnixTime}),
    Today - (Week - 1) * ?ONE_DAY_SECONDS;

%% @doc 当前距离每天某个时刻的时间
%% 如当前9:00 距离10:00为3600秒 返回3600
%% 如当前时间 23:00 距离 1:00为7200秒 返回7200
unixtime({to_nexttime, X}) ->
    UnixTime = unixtime(),
    unixtime({nexttime, UnixTime, X}) - UnixTime;
unixtime({to_nexttime, UnixTime, X}) ->
    unixtime({nexttime, UnixTime, X}) - UnixTime;

%% @doc 获取未来下一个时间点
unixtime({nexttime, UnixTime, {HH, MM, SS}}) ->
    unixtime({nexttime, UnixTime, HH * 3600 + MM * 60 + SS});
unixtime({nexttime, UnixTime, X}) when is_integer(X) ->
    TodayStartTime = unixtime({today, UnixTime}),
    BaseTime = TodayStartTime + X,
    case BaseTime > UnixTime of
        true -> BaseTime;
        false -> BaseTime + ?ONE_DAY_SECONDS
    end;
unixtime({nexttime, X}) ->
    unixtime({nexttime, unixtime(), X});

%% @doc 获取这个某个时间戳所在星期 WW HH:MM:SS 的时间戳，如果WW为8，等同于获取该天HH:MM:SS的时间戳
unixtime({thisweektime, UnixTime,{_WeekDay = 8, HH, MM, SS}}) ->
    {Date, _} = tools:seconds_to_datetime(UnixTime),
    tools:datetime_to_seconds({Date, {HH, MM, SS}});
unixtime({thisweektime, UnixTime, {WeekDay, HH, MM, SS}}) when WeekDay >= 1 andalso WeekDay =< 7->
    WeekStart = util:unixtime({thisweek, UnixTime}),
    WeekStart + (WeekDay - 1) * ?ONE_DAY_SECONDS + HH * ?ONE_HOUR_SECONDS + MM * ?ONE_MINUTE_SECONDS + SS;

%% @doc 获取某个时间戳所在星期下个 WW HH:MM:SS 的时间戳，如果WW为8，等同于获取下个HH:MM:SS的时间戳
unixtime({nextweektime, UnixTime, {_WeekDay = 8, HH, MM, SS}}) ->
    WeekTime = unixtime({thisweektime, UnixTime, {8, HH, MM, SS}}),
    case WeekTime >= UnixTime of
        true ->
            WeekTime;
        false ->
            WeekTime + ?ONE_DAY_SECONDS
    end;
unixtime({nextweektime, UnixTime, {WeekDay, HH, MM, SS}}) when WeekDay >= 1 andalso WeekDay =< 7->
    WeekTime = unixtime({thisweektime, UnixTime, {WeekDay, HH, MM, SS}}),
    case WeekTime >= UnixTime of
        true ->
            WeekTime;
        false ->
            WeekTime + ?ONE_WEEK_SECONDS
    end.

%% datetime_to_seconds/1
%% -----------------------------------------------------------
%% @doc 将日期转换unix时间戳
%% DateTime = {{2014,5,20},{16,14,57}} = {{Y, M, D}, {h, m, s}} 
%% 719528 * 24 * 3600 ==  calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}) .
-spec datetime_to_seconds(DateTime) -> integer() when
    DateTime :: {Year, Month, Day, Hour, Minute, Second} | {{Year, Month, Day}, {Hour, Minute, Second}},
    Year :: integer(),
    Month :: integer(),
    Day :: integer(),
    Hour :: integer(),
    Minute :: integer(),
    Second :: integer().
%% -----------------------------------------------------------
datetime_to_seconds(0) -> 0;
datetime_to_seconds({Year,Month,Day,Hour,Minute,Second}) ->
    datetime_to_seconds({{Year, Month, Day}, {Hour, Minute, Second}});
datetime_to_seconds(DateTime = {{_Year, _Month, _Day}, {_Hour, _Minute, _Second}}) ->
    case calendar:local_time_to_universal_time_dst(DateTime) of
        [] -> false;
        [_, Udate] -> 
            calendar:datetime_to_gregorian_seconds(Udate) - ?ZERO_TO_1970_DAYS * 24 * 3600;
        [Udate] ->
            calendar:datetime_to_gregorian_seconds(Udate) - ?ZERO_TO_1970_DAYS * 24 * 3600
    end.

%% is_same_day/2
%% -----------------------------------------------------------
%% @doc 判断两个时间戳是否是同一天
-spec is_same_day(TimeOne, TimeTwo) -> Result when
    TimeOne :: integer() | {{Year, Month, Day}, {Hour, Minute, Second}},
    TimeTwo :: integer() | {{Year, Month, Day}, {Hour, Minute, Second}},
    Year :: integer(),
    Month :: integer(),
    Day :: integer(),
    Hour :: integer(),
    Minute :: integer(),
    Second :: integer(),
    Result :: boolean().
%% -----------------------------------------------------------
is_same_day(TimeOne, TimeTwo) when is_integer(TimeOne) andalso is_integer(TimeTwo) ->
    Day1 = tools:unixtime({today, TimeOne}),
    Day2 = tools:unixtime({today, TimeTwo}),
    Day1 =:= Day2;
is_same_day(TimeStamp1 = {_,_,_}, TimeStamp2 = {_, _, _}) ->
    {{Y1, M1, D1}, _} = calendar:now_to_local_time(TimeStamp1),
    case calendar:now_to_local_time(TimeStamp2) of
        {{Y1, M1, D1}, _} -> true;
        _ -> false
    end;
is_same_day(_,_) -> false.

%% is_today/1
%% -----------------------------------------------------------
%% @doc 判断是否是今天的时间戳
-spec is_today(UnixTime :: integer()) -> Result :: boolean().
%% -----------------------------------------------------------
is_today(Time) ->
    Now = util:unixtime(),
    is_same_day(Time, Now).

%% day_of_the_week/0
%% -----------------------------------------------------------
%% @doc 判断今天周几
-spec day_of_the_week() -> Date :: integer().
%% -----------------------------------------------------------
day_of_the_week() ->
    calendar:day_of_the_week(date()).

%% day_of_the_week/1
%% -----------------------------------------------------------
%% @doc 判断对应的时间戳是周几
-spec day_of_the_week(UnixTime :: integer()) -> Date :: integer().
%% -----------------------------------------------------------
day_of_the_week(UnixTime) ->
    Now = unixtime_to_now(UnixTime),
    {Date, _Time} = calendar:now_to_local_time(Now),
    calendar:day_of_the_week(Date).

%% unixtime_to_now/1
%% -----------------------------------------------------------
%% @doc UnixTime时间戳转换成erlang:now()格式(模糊时间)
-spec unixtime_to_now(UnixTime :: integer()) -> {MegaSeconds :: integer(), Seconds :: integer(), 0}.
%% -----------------------------------------------------------
unixtime_to_now(UnixTime) ->
    MegaSeconds = UnixTime div 1000000,
    Seconds = UnixTime rem 1000000,
    {MegaSeconds, Seconds, 0}.

%% rand/2
%% -----------------------------------------------------------
%% @doc 产生一个介于Min到Max之间的随机整数
-spec rand(Min, Max) -> Random | type_error() when
    Min :: integer(),
    Max :: integer(),
    Random :: integer().
%% -----------------------------------------------------------
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

%% rand_list/1
%% -----------------------------------------------------------
%% @doc 从一个list中随机取出一项
-spec rand_list(List) -> Result when
    List :: list(),
    Result :: term() | type_error().
%% -----------------------------------------------------------
rand_list([]) -> ?TYPE_ERROR("rand list is empty", []);
rand_list([Elem]) -> Elem;
rand_list(List) ->
    Len = length(List),
    rand_list(Len, List).

%% get_rand_list/2
%% -----------------------------------------------------------
%% @doc 从列表中不重复选择N个元素，返回选中元素构成的新列表,当N>=length(List)时，只能返回长度为N的list
-spec get_rand_list(N, List) -> RandList when
    N :: integer(),
    List :: list(),
    RandList :: list().
%% -----------------------------------------------------------
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


%% nth_replace/3
%% -----------------------------------------------------------
%% @doc 替换list中的第N个元素
-spec nth_replace(List, Nth, NewVal) -> NewList  | type_error() when
    List :: list(),
    Nth :: integer(),
    NewVal :: term(),
    NewList :: list().
%% -----------------------------------------------------------
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

%% suffle_list/1
%% -----------------------------------------------------------
%% @doc 随机返回一个乱序的list
-spec suffle_list(List :: list()) -> NList :: list().
%% -----------------------------------------------------------
suffle_list(List) ->
    RandMax = length(List) * 10,
    TmpList = [{rand(0, RandMax), X} || X <- List],
    SortList = lists:keysort(1, TmpList),
    [E || {_, E} <- SortList].

%% rand_by_weight/2
%% -----------------------------------------------------------
%% @doc 根据权重列表随机对应的elem，循环Count次
-spec rand_by_weight(Count, List) -> ResultList when
    Count :: integer(),
    List :: list({Id, Weight}),
    ResultList :: list({Id, Num}),
    Weight :: integer(),
    Id :: integer(),
    Num :: integer().
%% -----------------------------------------------------------
rand_by_weight(0, _List) -> [];
rand_by_weight(_Count, []) -> [];
rand_by_weight(Count, List) -> do_rand_by_weight(Count, List).

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

%% delete_list/2
%% -----------------------------------------------------------
%% @doc 把List1从List2中全部删除掉，返回一个新的不包含List1的任何一个元素的list
-spec delete_list(List1 :: list(), List2 :: list()) -> NList :: list().
 %% -----------------------------------------------------------
delete_list([], List2) -> List2;
delete_list(_List1 = [Elem|Tail], List2) ->
    delete_list(Tail, lists:delete(Elem, List2)).

%% index_of/2
%% -----------------------------------------------------------
%% @doc 元素位于列表中的位置，从1开始;没有找到的话返回0
-spec index_of(List :: list(), Elem :: term()) -> Index :: 0 | integer().
%% -----------------------------------------------------------
index_of(List, Elem) ->
    string:str(List, [Elem]).

%% md5/1
%% -----------------------------------------------------------
%% @doc 生成16位格式的md5值
-spec md5(String :: list()) -> Md5Bin :: binary().
%% -----------------------------------------------------------
md5(String) ->
    list_to_binary([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(String))]).

%% ip2bitstring/1
%% -----------------------------------------------------------
%% @doc IP值转换成bitstring
-spec ip2bitstring({N1 :: integer(), N2 :: integer(), N3 :: integer(), N4 :: integer()}) -> IpBitString :: bitstring().
%% -----------------------------------------------------------
ip2bitstring({N1, N2, N3, N4}) ->
    erlang:list_to_bitstring(integer_to_list(N1) ++ "." ++ integer_to_list(N2) ++ "." ++ integer_to_list(N3) ++ "." ++ integer_to_list(N4));
ip2bitstring(_) -> <<"">>.

ip2bitstring(N1, N2, N3, N4) -> ip2bitstring({N1, N2, N3, N4}).

%% merge_kv/1
%% -----------------------------------------------------------
%% @doc 合并相同的键值对
-spec merge_kv(List :: list({Key :: term(), Val :: integer()})) -> MergeList :: list().
%% -----------------------------------------------------------
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

%% calc_earth_distance/2
%% -----------------------------------------------------------
%% @doc 计算两个经纬度之间的距离 Lon1 第一点的精度 Lat1 第一点的纬度 Lon2 第二点的精度 Lat2 第二点的纬度
-spec calc_earth_distance({Lon1, Lat1}, {Lon2, Lat2}) -> Distance when
    Lon1 :: float(),
    Lat1 :: float(),
    Lon2 :: float(),
    Lat2 :: float(),
    Distance :: float().
%% -----------------------------------------------------------
calc_earth_distance({Lon1, Lat1}, {Lon2, Lat2}) ->
    EARTH_RADIUS = 6378137, %%赤道半径,单位米
    A = rad(Lat1) - rad(Lat2),
    B = rad(Lon1) - rad(Lon2),
    Distance = 2 * math:asin(math:sqrt(math:pow(math:sin(A/2), 2) + math:cos(rad(Lat1)) * math:cos(rad(Lat2)) * math:pow(math:sin(B / 2),2))) * EARTH_RADIUS,
    erlang:abs(Distance).
rad(L) ->
    L * math:pi() / 180.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc 从一个list中随机出一个elem返回，
%% @doc 必须保证 Len =< length(List)
rand_list(Len, List) ->
    Index = rand(1, Len),
    lists:nth(Index, List).

