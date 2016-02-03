%% @author winecat
%% @doc fun for digital


-module(util_digital).

%% include files
%% ---------------------------------
-include("util.hrl").


%% API functions
%% ---------------------------------
-export([
         floor/1
         ,ceil/1
         ,at_least/2
         ,at_most/2
         ,check_range/3
        ]).


%% @doc 向下取整
-spec floor(X) -> FloorX when
    X :: integer(),
    FloorX :: integer().
floor(X) ->
    Tmp = erlang:trunc(X),
    case X < Tmp of
        true -> Tmp -1;
        false -> Tmp
    end.

%% @doc 向上取整
-spec ceil(X) -> CeilX when
    X :: integer(),
    CeilX :: integer().
ceil(X) ->
    Tmp = erlang:trunc(X),
    case X > Tmp of
        true -> Tmp +1;
        false -> Tmp
    end.

%% @doc 最小值限制
-spec at_least(Val :: number(), Min :: number()) -> Result :: number().
at_least(Val, Min) ->
    erlang:max(Val, Min).

%% @doc 最大值限制
-spec at_most(Val :: number(), Max :: number()) -> Result :: number().
at_most(Val, Max) ->
    erlang:min(Val, Max).

%% @doc 做取值范围限制
-spec check_range(Val :: number(), Min :: number(), Max :: number()) -> Result :: number().
check_range(_Val, Min, Max) when Min > Max -> 
    ?TYPE_ERROR("the value of Min is more than Max, Min/Max : ~w/~w", [Min, Max]);
check_range(Val, Min, Max) ->
    if
        Val > Max -> Max;
        Val < Min -> Min;
        true -> Val
    end.

%% Internal functions
%% ---------------------------------


