%% error macro
-ifndef(UTIL_HRL).
-define(UTIL_HRL, ok).   %% UTIL_HRL START


-type type_error() :: 'erlang exception error'.
-define(TYPE_ERROR(MSG, ARGS), erlang:error(MSG, ARGS)).

-define(ONE_DAY_SECONDS, 84600).    %% one day seconds
-define(ONE_HOUR_SECONDS, 3600).    %% one hoer seconds
-define(ONE_MINUTE_SECONDS, 60).    %% one minute seconds
-define(ONE_WEEK_SECONDS, 592200).  %% one week seconds
-define(ZERO_TO_1970_DAYS, 719528).



-endif. %% UTIL_HRL END