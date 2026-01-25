%%%-------------------------------------------------------------------
%% @doc light_governors - Minimal FSM implementations for AtomVM
%%
%% Optimized for 256MB RAM edge devices (vs BEAM's 100KB per governor).
%% Key reductions:
%% - No audit trail (store locally only, optional async persist)
%% - Simplified state tracking (2-3 states max)
%% - No ETS tables (list-based state)
%% - Reduced buffer sizes (20 events vs 100 events)
%% - Simple message loop (no gen_server overhead)
%%
%% Memory targets:
%% - 25KB per governor (vs 100KB+ on BEAM)
%% - Startup: 0.35s (vs 2.5s on BEAM)
%% - Latency p99: 8ms (vs 45ms on BEAM)
%%
%% @end
%%%-------------------------------------------------------------------
-module(light_governors).

%% Public API
-export([
    start_link/1,
    check_entitlement/3,
    check_billing/3,
    check_quota/3,
    get_state/1,
    halt/1
]).

%% Internal state machine loop
-export([init_loop/3]).

%% Compile-time constants (optimize for edge)
-define(MAX_BUFFER_SIZE, 20).  % 20 events max (vs 100 on BEAM)
-define(STATE_TIMEOUT, infinity).  % No idle timeout (battery-aware)
-define(MIN_HEAP_SIZE, 128).  % Minimal heap for AtomVM
-define(POLL_INTERVAL, 1000).  % 1s poll for state persistence

%%===================================================================
%% Type Definitions
%%===================================================================

-type governor_type() :: entitlement | billing | quota | compliance | subscription.
-type request() :: {Type :: atom(), Tenant :: binary(), Data :: map()}.
-type reply() :: {ok, map()} | {error, term()}.
-type state() :: #{
    type := governor_type(),
    config := map(),
    buffer := [request()],
    metrics := #{
        requests => non_neg_integer(),
        errors => non_neg_integer(),
        last_check => pos_integer()
    },
    tenant_state := map()
}.

%%===================================================================
%% Public API
%%===================================================================

%% @doc Start a lightweight governor FSM
%% Example: start_link(entitlement)
%% Returns: {ok, Pid} | {error, Reason}
-spec start_link(governor_type()) -> {ok, pid()} | {error, term()}.
start_link(Type) ->
    Pid = spawn_link(?MODULE, init_loop, [Type, init_state(Type), self()]),
    {ok, Pid}.

%% @doc Check entitlement rule (simple list scan)
-spec check_entitlement(pid(), binary(), map()) -> reply().
check_entitlement(Pid, Tenant, Data) ->
    Pid ! {check, entitlement, Tenant, Data, self()},
    receive
        {reply, Result} -> Result
    after 5000 -> {error, timeout}
    end.

%% @doc Check billing constraint
-spec check_billing(pid(), binary(), map()) -> reply().
check_billing(Pid, Tenant, Data) ->
    Pid ! {check, billing, Tenant, Data, self()},
    receive
        {reply, Result} -> Result
    after 5000 -> {error, timeout}
    end.

%% @doc Check quota constraint
-spec check_quota(pid(), binary(), map()) -> reply().
check_quota(Pid, Tenant, Data) ->
    Pid ! {check, quota, Tenant, Data, self()},
    receive
        {reply, Result} -> Result
    after 5000 -> {error, timeout}
    end.

%% @doc Get current state (for monitoring)
-spec get_state(pid()) -> state().
get_state(Pid) ->
    Pid ! {get_state, self()},
    receive
        {state, State} -> State
    after 5000 -> error
    end.

%% @doc Halt governor (cleanup)
-spec halt(pid()) -> ok.
halt(Pid) ->
    Pid ! halt,
    ok.

%%===================================================================
%% Internal Functions - State Machine Loop
%%===================================================================

%% @private
%% @doc Main FSM loop (replaces gen_server overhead)
-spec init_loop(governor_type(), state(), pid()) -> no_return().
init_loop(Type, State, ParentPid) ->
    loop(State#{type => Type, parent => ParentPid}).

%% @private
%% @doc Main message loop (minimal overhead)
-spec loop(state()) -> no_return().
loop(State = #{type := Type, buffer := Buffer, metrics := Metrics}) ->
    receive
        {check, entitlement, Tenant, Data, ReplyTo} ->
            {Result, NewState} = handle_entitlement_check(State, Tenant, Data),
            ReplyTo ! {reply, Result},
            UpdatedMetrics = Metrics#{requests => maps.get(requests, Metrics, 0) + 1},
            loop(State#{metrics => UpdatedMetrics});

        {check, billing, Tenant, Data, ReplyTo} ->
            {Result, NewState} = handle_billing_check(State, Tenant, Data),
            ReplyTo ! {reply, Result},
            UpdatedMetrics = Metrics#{requests => maps.get(requests, Metrics, 0) + 1},
            loop(State#{metrics => UpdatedMetrics});

        {check, quota, Tenant, Data, ReplyTo} ->
            {Result, NewState} = handle_quota_check(State, Tenant, Data),
            ReplyTo ! {reply, Result},
            UpdatedMetrics = Metrics#{requests => maps.get(requests, Metrics, 0) + 1},
            loop(State#{metrics => UpdatedMetrics});

        {get_state, ReplyTo} ->
            ReplyTo ! {state, State},
            loop(State);

        halt ->
            % Optional: persist state before exit
            maybe_persist_state(State),
            exit(normal);

        _ ->
            % Ignore unknown messages
            loop(State)
    after ?STATE_TIMEOUT ->
        % No timeout - run indefinitely
        loop(State)
    end.

%%===================================================================
%% Check Handlers - Minimal Logic
%%===================================================================

%% @private Entitlement check: verify tenant entitlements
-spec handle_entitlement_check(state(), binary(), map()) ->
    {reply(), state()}.
handle_entitlement_check(State = #{tenant_state := TenantState}, Tenant, Data) ->
    case maps:get(Tenant, TenantState, undefined) of
        undefined ->
            % New tenant: bootstrap entitlements
            DefaultEntitlements = #{
                tier => maps:get(tier, Data, free),
                features => [],
                limits => #{calls_per_day => 1000}
            },
            NewTenantState = TenantState#{Tenant => DefaultEntitlements},
            {
                {ok, DefaultEntitlements},
                State#{tenant_state => NewTenantState}
            };

        Entitlements ->
            % Check feature entitlement
            Feature = maps:get(feature, Data, unknown),
            case lists:member(Feature, maps:get(features, Entitlements, [])) of
                true ->
                    {{ok, Entitlements}, State};
                false ->
                    {{error, {not_entitled, Feature}}, State}
            end
    end.

%% @private Billing check: verify credit and rate limits
-spec handle_billing_check(state(), binary(), map()) ->
    {reply(), state()}.
handle_billing_check(State = #{tenant_state := TenantState}, Tenant, Data) ->
    case maps:get(Tenant, TenantState, undefined) of
        undefined ->
            % New tenant: bootstrap billing
            DefaultBilling = #{
                account_id => Tenant,
                credit => 100,
                rate_limit => 100,  % requests per minute
                overage => false
            },
            NewTenantState = TenantState#{Tenant => DefaultBilling},
            {{ok, DefaultBilling}, State#{tenant_state => NewTenantState}};

        Billing ->
            % Check rate limit
            RateLimit = maps:get(rate_limit, Billing, 100),
            Current = maps:get(current_rate, Billing, 0),
            case Current < RateLimit of
                true ->
                    UpdatedBilling = Billing#{current_rate => Current + 1},
                    NewTenantState = TenantState#{Tenant => UpdatedBilling},
                    {{ok, Billing}, State#{tenant_state => NewTenantState}};
                false ->
                    {{error, rate_limited}, State}
            end
    end.

%% @private Quota check: verify usage quotas
-spec handle_quota_check(state(), binary(), map()) ->
    {reply(), state()}.
handle_quota_check(State = #{tenant_state := TenantState}, Tenant, Data) ->
    case maps:get(Tenant, TenantState, undefined) of
        undefined ->
            % New tenant: bootstrap quota
            DefaultQuota = #{
                account_id => Tenant,
                api_calls => 0,
                storage_mb => 0,
                limits => #{
                    api_calls => 10000,
                    storage_mb => 500
                }
            },
            NewTenantState = TenantState#{Tenant => DefaultQuota},
            {{ok, DefaultQuota}, State#{tenant_state => NewTenantState}};

        Quota ->
            % Check quota usage
            Limits = maps:get(limits, Quota, #{}),
            Usage = maps:get(api_calls, Quota, 0),
            MaxCalls = maps:get(api_calls, Limits, 10000),

            case Usage < MaxCalls of
                true ->
                    UpdatedQuota = Quota#{api_calls => Usage + 1},
                    NewTenantState = TenantState#{Tenant => UpdatedQuota},
                    {{ok, Quota}, State#{tenant_state => NewTenantState}};
                false ->
                    {{error, quota_exceeded}, State}
            end
    end.

%%===================================================================
%% Utility Functions
%%===================================================================

%% @private Initialize empty state
-spec init_state(governor_type()) -> state().
init_state(Type) ->
    #{
        type => Type,
        config => init_config(Type),
        buffer => [],
        metrics => #{
            requests => 0,
            errors => 0,
            last_check => erlang:system_time(millisecond)
        },
        tenant_state => #{}
    }.

%% @private Initialize type-specific config
-spec init_config(governor_type()) -> map().
init_config(entitlement) ->
    #{
        rules => [],
        cache_ttl => 3600,  % 1 hour
        default_tier => free
    };
init_config(billing) ->
    #{
        currency => <<"USD">>,
        billing_period => monthly,
        grace_period => 2592000  % 30 days
    };
init_config(quota) ->
    #{
        reset_period => daily,
        burst_limit => 2,
        backoff => exponential
    };
init_config(compliance) ->
    #{
        audit_retention => 2592000,  % 30 days
        report_interval => 86400  % 1 day
    };
init_config(subscription) ->
    #{
        trial_period => 604800,  % 7 days
        auto_renew => true,
        grace_period => 86400  % 1 day
    };
init_config(_) ->
    #{}.

%% @private Optional: persist state (disabled in minimal version)
-spec maybe_persist_state(state()) -> ok.
maybe_persist_state(_State) ->
    % Persistence optional in minimal version
    % Could use compression module if enabled:
    % compression:save_state(State, "state.bin.gz")
    ok.

%%%===================================================================
%% Memory Analysis Helper Functions (for profiling)
%%%===================================================================

%% @doc Calculate approximate memory usage (debugging)
-spec memory_usage(state()) -> #{atom() => non_neg_integer()}.
memory_usage(#{buffer := Buffer, tenant_state := TenantState}) ->
    BufferSize = erlang:external_size(Buffer),
    TenantSize = erlang:external_size(TenantState),
    #{
        buffer => BufferSize,
        tenant_state => TenantSize,
        total => BufferSize + TenantSize
    }.

