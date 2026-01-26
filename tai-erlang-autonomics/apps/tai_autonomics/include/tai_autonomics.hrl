%%%-------------------------------------------------------------------
%% @doc TAI Autonomics header file
%%
%% Common definitions and macros
%%
%% @end
%%%-------------------------------------------------------------------

-ifndef(TAI_AUTONOMICS_HRL).
-define(TAI_AUTONOMICS_HRL, true).

%% Receipt types
-define(RECEIPT_TYPE_TRANSITION, <<"transition">>).
-define(RECEIPT_TYPE_REFUSAL, <<"refusal">>).
-define(RECEIPT_TYPE_ACTION_ATTEMPT, <<"action_attempt">>).
-define(RECEIPT_TYPE_ACTION_RESULT, <<"action_result">>).

%% Governor states
-define(GOVERNOR_STATE_BOOT, boot).
-define(GOVERNOR_STATE_STABLE, stable).
-define(GOVERNOR_STATE_WARNING, warning).
-define(GOVERNOR_STATE_INTERVENING, intervening).
-define(GOVERNOR_STATE_REFUSING, refusing).

%% Metric snapshot record (shared between modules)
-record(metric_snapshot, {
    timestamp :: integer(),
    node :: atom(),
    memory :: map(),
    processes :: map(),
    message_queues :: map(),
    governors :: map(),
    errors :: map()
}).

-endif.
