%% @author winecat
%% @doc fun for ip


-module(util_ip).

%% include files
%% ---------------------------------


%% API functions
%% ---------------------------------
-export([
         ip2bitstring/1
         ,ip2bitstring/4
         ,ip2integer/1
         ,integer2ip/1
        ]).


%% @doc IP值转换成bitstring
-spec ip2bitstring({N1 :: integer(), N2 :: integer(), N3 :: integer(), N4 :: integer()}) -> IpBitString :: bitstring().
ip2bitstring({N1, N2, N3, N4}) ->
    erlang:list_to_bitstring(integer_to_list(N1) ++ "." ++ integer_to_list(N2) ++ "." ++ integer_to_list(N3) ++ "." ++ integer_to_list(N4));
ip2bitstring(_) -> <<"">>.

ip2bitstring(N1, N2, N3, N4) -> ip2bitstring({N1, N2, N3, N4}).


%% @doc ip地址转成数值
%% tips:
%% "11111111,00000000,00000000" = integer_to_list(16711680,2).  
%% "1111111100000000" = integer_to_list(65280,2).   
%% "1000000000000000000000000" = integer_to_list(16777216,2).
%% "10000000000000000" = integer_to_list(65536,2).   
%% "100000000" = integer_to_list(256,2).  
-spec ip2integer({N1 :: integer(), N2 :: integer(), N3 :: integer(), N4 :: integer()}) -> Result :: integer().
ip2integer({N1, N2, N3, N4}) ->
    (N1 * 16777216) + (N2 * 65536) + (N3 * 256) + N4.

%% @doc 数值转成ip地址
-spec integer2ip(Integer :: integer()) -> {N1 :: integer(), N2 :: integer(), N3 :: integer(), N4 :: integer()}.
integer2ip(Integer) ->
    {Integer bsr 24, (Integer band 16711680) bsr 16, (Integer band 65280) bsr 8, Integer band 255}.


%% Internal functions
%% ---------------------------------


