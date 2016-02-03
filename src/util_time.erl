%% @author winecat
%% @doc @todo Add description to util_time.


-module(util_time).

%% include files
%% ---------------------------------
-include("util.hrl").

%% API functions
%% ---------------------------------
-export([
         seconds_to_datetime/1
         ,unixtime/0
         ,unixtime/1
         ,datetime_to_seconds/1
         ,is_same_day/2
         ,is_today/1
         ,day_of_the_week/0
         ,day_of_the_week/1
         ,unixtime_to_now/1         
        ]).



%% @doc 将unix时间戳转换成当地日期时间
-spec seconds_to_datetime(UnixTime) -> {{Year, Month, Day}, {Hour, Minute, Second}} when
    UnixTime :: integer(),
    Year :: integer(),
    Month :: integer(),
    Day :: integer(),
    Hour :: integer(),
    Minute :: integer(),
    Second :: integer().
seconds_to_datetime(Unixtime) ->
    Now = unixtime_to_now(Unixtime),
    calendar:now_to_local_time(Now).

%% @doc 当前的unix时间戳(s)
-spec unixtime() -> Second when
    Second :: integer().
unixtime() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

%% @doc 根据传入不同的type，返回不同的时间戳(s)
-spec unixtime(Type) -> Second when
    Type :: atom(),
    Second :: integer().
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
    {_, Time} = calendar:now_to_local_time({M, S, MS}), 
    M * 1000000 + S - calendar:time_to_seconds(Time);

%% @doc 获取明天0时0分0秒的时间戳（这里是相对于当前时区而言)
unixtime(tomorrow) ->
    unixtime(today) + ?ONE_DAY_SECONDS;

%% @doc 获取昨天0时0分0秒的时间戳（这里是相对于当前时区而言)
unixtime(yesterday) ->
    unixtime(today) - ?ONE_DAY_SECONDS;

%% @doc 获取本周周一00:00:00的时间戳
unixtime(thisweek) ->
    unixtime({thisweek, util:unixtime()});

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
    Week = day_of_the_week(UnixTime),
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

%% @doc 判断是否是今天的时间戳
-spec is_today(UnixTime :: integer()) -> Result :: boolean().
is_today(Time) ->
    Now = util:unixtime(),
    is_same_day(Time, Now).

%% @doc 判断今天周几
-spec day_of_the_week() -> Date :: integer().
day_of_the_week() ->
    calendar:day_of_the_week(date()).

%% @doc 判断对应的时间戳是周几
-spec day_of_the_week(UnixTime :: integer()) -> Date :: integer().
day_of_the_week(UnixTime) ->
    Now = unixtime_to_now(UnixTime),
    {Date, _Time} = calendar:now_to_local_time(Now),
    calendar:day_of_the_week(Date).

%% @doc UnixTime时间戳转换成erlang:now()格式(模糊时间)
-spec unixtime_to_now(UnixTime :: integer()) -> {MegaSeconds :: integer(), Seconds :: integer(), 0}.
unixtime_to_now(UnixTime) ->
    MegaSeconds = UnixTime div 1000000,
    Seconds = UnixTime rem 1000000,
    {MegaSeconds, Seconds, 0}.

%% Internal functions
%% ---------------------------------


