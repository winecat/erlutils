%% @author winecat
%% @doc fun for points


-module(util_points).

%% include files
%% ---------------------------------


%% API functions
%% ---------------------------------
-export([
         calc_earth_distance/2
        ]).




%% @doc 计算两个经纬度之间的距离 Lon1 第一点的精度 Lat1 第一点的纬度 Lon2 第二点的精度 Lat2 第二点的纬度
-spec calc_earth_distance({Lon1, Lat1}, {Lon2, Lat2}) -> Distance when
    Lon1 :: float(),
    Lat1 :: float(),
    Lon2 :: float(),
    Lat2 :: float(),
    Distance :: float().
calc_earth_distance({Lon1, Lat1}, {Lon2, Lat2}) ->
    EARTH_RADIUS = 6378137, %%赤道半径,单位米
    A = rad(Lat1) - rad(Lat2),
    B = rad(Lon1) - rad(Lon2),
    Distance = 2 * math:asin(math:sqrt(math:pow(math:sin(A/2), 2) + math:cos(rad(Lat1)) * math:cos(rad(Lat2)) * math:pow(math:sin(B / 2),2))) * EARTH_RADIUS,
    erlang:abs(Distance).
rad(L) ->
    L * math:pi() / 180.

%% Internal functions
%% ---------------------------------


