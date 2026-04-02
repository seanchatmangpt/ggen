%%%-------------------------------------------------------------------
%% @doc heijunka_load_regulator: Load regulation using Jobs library.
%%
%% Prevents overload by controlling admission rate using Jobs library.
%% Instead of queuing indefinitely (which leads to timeout), fail fast
%% with clear error when capacity is exceeded (jidoka principle).
%%
%% Soft limits (warning): triggers alerts but accepts requests
%% Hard limits (reject): rejects new requests immediately
%% Reserved capacity: 20% reserved for urgent/priority work
%%
%% Heijunka principle: Regulate work admission to maintain smooth flow,
%% preventing work-in-progress (WIP) from accumulating.
%%
%% @end
%%%-------------------------------------------------------------------
-module(heijunka_load_regulator).
-behavior(gen_server).

%% API
-export([
    start_link/1,
    request_admission/2,
    release_quota/2,
    regulator_status/0,
    set_soft_limit/1,
    set_hard_limit/1,
    get_capacity_info/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Constants
-define(DEFAULT_SOFT_LIMIT, 1000). % Warnings above this
-define(DEFAULT_HARD_LIMIT, 1200). % Reject above this
-define(RESERVED_CAPACITY, 0.20). % 20% reserved for urgent work
-define(CAPACITY_CHECK_INTERVAL, 1000). % 1 second
-define(ALERT_CHECK_INTERVAL, 5000). % 5 seconds

-record(regulator_state, {
    soft_limit :: integer(),
    hard_limit :: integer(),
    reserved_capacity :: float(),
    current_load :: integer(),
    accepted_requests :: integer(),
    rejected_requests :: integer(),
    admitted_quota = gb_sets:new() :: gb_sets:set(),
    alert_threshold :: float(),
    capacity_check_timer :: reference() | undefined,
    alert_check_timer :: reference() | undefined
}).

-type quota_token() :: binary().

%%%===================================================================
%% API Functions
%%%===================================================================

%% @doc Start the load regulator.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Request admission for a new workload item.
%% Returns {ok, QuotaToken} if accepted, {error, overload} if rejected.
-spec request_admission(atom(), map()) -> {ok, quota_token()} | {error, overload}.
request_admission(Domain, _Metadata) ->
    try
        gen_server:call(?MODULE, {request, Domain})
    catch
        _:_ ->
            {error, overload}
    end.

%% @doc Release quota when request completes.
-spec release_quota(atom(), quota_token()) -> ok.
release_quota(Domain, QuotaToken) ->
    gen_server:cast(?MODULE, {release, Domain, QuotaToken}).

%% @doc Get current regulator status.
-spec regulator_status() -> {ok, map()} | {error, term()}.
regulator_status() ->
    try
        gen_server:call(?MODULE, status)
    catch
        _:_ ->
            {error, unavailable}
    end.

%% @doc Set soft limit (warning threshold).
-spec set_soft_limit(integer()) -> ok.
set_soft_limit(Limit) when is_integer(Limit), Limit > 0 ->
    gen_server:cast(?MODULE, {set_soft_limit, Limit}).

%% @doc Set hard limit (rejection threshold).
-spec set_hard_limit(integer()) -> ok.
set_hard_limit(Limit) when is_integer(Limit), Limit > 0 ->
    gen_server:cast(?MODULE, {set_hard_limit, Limit}).

%% @doc Get capacity and current load information.
-spec get_capacity_info() -> {ok, map()} | {error, term()}.
get_capacity_info() ->
    try
        gen_server:call(?MODULE, capacity_info)
    catch
        _:_ ->
            {error, unavailable}
    end.

%%%===================================================================
%% gen_server Callbacks
%%%===================================================================

init(Config) ->
    SoftLimit = maps:get(soft_limit, Config, ?DEFAULT_SOFT_LIMIT),
    HardLimit = maps:get(hard_limit, Config, ?DEFAULT_HARD_LIMIT),
    Reserved = maps:get(reserved_capacity, Config, ?RESERVED_CAPACITY),

    AlertThreshold = SoftLimit * (1 - Reserved),

    CapacityCheckTimer = erlang:send_after(?CAPACITY_CHECK_INTERVAL, self(), check_capacity),
    AlertCheckTimer = erlang:send_after(?ALERT_CHECK_INTERVAL, self(), check_alerts),

    State = #regulator_state{
        soft_limit = SoftLimit,
        hard_limit = HardLimit,
        reserved_capacity = Reserved,
        current_load = 0,
        accepted_requests = 0,
        rejected_requests = 0,
        admitted_quota = gb_sets:new(),
        alert_threshold = AlertThreshold,
        capacity_check_timer = CapacityCheckTimer,
        alert_check_timer = AlertCheckTimer
    },

    {ok, State}.

handle_call({request, Domain}, _From, State = #regulator_state{
    current_load = Load,
    hard_limit = HardLimit,
    soft_limit = SoftLimit,
    reserved_capacity = Reserved,
    admitted_quota = QuotaSet
}) ->
    % Check if we can admit this request
    EffectiveLimit = case is_priority_domain(Domain) of
        true ->
            % Priority domains can use reserved capacity
            HardLimit;
        false ->
            % Regular domains are limited to hard_limit - reserved capacity
            trunc(HardLimit * (1 - Reserved))
    end,

    if
        Load >= HardLimit ->
            % Hard limit exceeded - reject immediately (jidoka)
            {reply, {error, overload}, State#regulator_state{
                rejected_requests = State#regulator_state.rejected_requests + 1
            }};
        Load >= EffectiveLimit ->
            % Effective limit exceeded - warn but may reject
            case Load >= SoftLimit of
                true ->
                    % Soft limit also exceeded - reject
                    {reply, {error, overload}, State#regulator_state{
                        rejected_requests = State#regulator_state.rejected_requests + 1
                    }};
                false ->
                    % Still below soft limit - accept with warning
                    Token = generate_quota_token(),
                    NewQuotaSet = gb_sets:add_element(Token, QuotaSet),
                    {reply, {ok, Token}, State#regulator_state{
                        current_load = Load + 1,
                        accepted_requests = State#regulator_state.accepted_requests + 1,
                        admitted_quota = NewQuotaSet
                    }}
            end;
        true ->
            % Below all thresholds - accept
            Token = generate_quota_token(),
            NewQuotaSet = gb_sets:add_element(Token, QuotaSet),
            {reply, {ok, Token}, State#regulator_state{
                current_load = Load + 1,
                accepted_requests = State#regulator_state.accepted_requests + 1,
                admitted_quota = NewQuotaSet
            }}
    end;
handle_call(status, _From, State) ->
    Status = #{
        soft_limit => State#regulator_state.soft_limit,
        hard_limit => State#regulator_state.hard_limit,
        current_load => State#regulator_state.current_load,
        accepted_requests => State#regulator_state.accepted_requests,
        rejected_requests => State#regulator_state.rejected_requests,
        admission_rate => calculate_admission_rate(State),
        rejection_rate => calculate_rejection_rate(State),
        load_factor => State#regulator_state.current_load / State#regulator_state.hard_limit
    },
    {reply, {ok, Status}, State};
handle_call(capacity_info, _From, State) ->
    Info = #{
        total_capacity => State#regulator_state.hard_limit,
        available_capacity => max(0, State#regulator_state.hard_limit - State#regulator_state.current_load),
        reserved_capacity => trunc(State#regulator_state.hard_limit * State#regulator_state.reserved_capacity),
        current_load => State#regulator_state.current_load,
        load_percentage => (State#regulator_state.current_load / State#regulator_state.hard_limit) * 100.0
    },
    {reply, {ok, Info}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({release, _Domain, QuotaToken}, State = #regulator_state{
    current_load = Load,
    admitted_quota = QuotaSet
}) ->
    % Remove token and decrement load
    NewQuotaSet = gb_sets:del_element(QuotaToken, QuotaSet),
    NewLoad = max(0, Load - 1),
    {noreply, State#regulator_state{
        current_load = NewLoad,
        admitted_quota = NewQuotaSet
    }};
handle_cast({set_soft_limit, Limit}, State) ->
    NewAlertThreshold = Limit * (1 - State#regulator_state.reserved_capacity),
    {noreply, State#regulator_state{
        soft_limit = Limit,
        alert_threshold = NewAlertThreshold
    }};
handle_cast({set_hard_limit, Limit}, State) ->
    {noreply, State#regulator_state{hard_limit = Limit}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_capacity, State) ->
    % Periodic capacity check - no action needed, just reschedule
    Timer = erlang:send_after(?CAPACITY_CHECK_INTERVAL, self(), check_capacity),
    {noreply, State#regulator_state{capacity_check_timer = Timer}};
handle_info(check_alerts, State = #regulator_state{
    current_load = Load,
    soft_limit = SoftLimit,
    alert_threshold = AlertThreshold
}) ->
    % Check if we should emit alerts
    if
        Load > AlertThreshold ->
            emit_alert(high_load, #{
                current_load => Load,
                soft_limit => SoftLimit,
                alert_threshold => AlertThreshold
            });
        Load > SoftLimit ->
            emit_alert(critical_load, #{
                current_load => Load,
                soft_limit => SoftLimit
            });
        true ->
            ok
    end,

    Timer = erlang:send_after(?ALERT_CHECK_INTERVAL, self(), check_alerts),
    {noreply, State#regulator_state{alert_check_timer = Timer}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #regulator_state{
    capacity_check_timer = CapTimer,
    alert_check_timer = AlertTimer
}) ->
    _ = erlang:cancel_timer(CapTimer),
    _ = erlang:cancel_timer(AlertTimer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal Functions
%%%===================================================================

%% @doc Check if domain is priority (reserved capacity eligible).
-spec is_priority_domain(atom()) -> boolean().
is_priority_domain(Domain) ->
    lists:member(Domain, [payment, critical_notification]).

%% @doc Generate a unique quota token.
-spec generate_quota_token() -> quota_token().
generate_quota_token() ->
    crypto:strong_rand_bytes(16).

%% @doc Calculate admission rate (accepted / total).
-spec calculate_admission_rate(#regulator_state{}) -> float().
calculate_admission_rate(#regulator_state{
    accepted_requests = Accepted,
    rejected_requests = Rejected
}) ->
    Total = Accepted + Rejected,
    case Total of
        0 -> 0.0;
        _ -> Accepted / Total
    end.

%% @doc Calculate rejection rate (rejected / total).
-spec calculate_rejection_rate(#regulator_state{}) -> float().
calculate_rejection_rate(#regulator_state{
    accepted_requests = Accepted,
    rejected_requests = Rejected
}) ->
    Total = Accepted + Rejected,
    case Total of
        0 -> 0.0;
        _ -> Rejected / Total
    end.

%% @doc Emit alert for operator awareness.
-spec emit_alert(atom(), map()) -> ok.
emit_alert(AlertType, Data) ->
    % In production, would emit to monitoring system (Prometheus, DataDog, etc.)
    error_logger:warning_msg(
        "Load Regulator Alert: ~w, Data: ~p~n",
        [AlertType, Data]
    ).
