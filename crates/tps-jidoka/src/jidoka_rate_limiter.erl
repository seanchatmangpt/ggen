%%%-------------------------------------------------------------------
%%% @doc Jidoka Rate Limiter - Token Bucket Flow Control
%%%
%%% Implements the token bucket algorithm for rate limiting.
%%% Jidoka principle: Don't accept work when overloaded. Fail-fast
%%% rather than queuing requests that can't be processed.
%%%
%%% Algorithm:
%%% - Tokens refill at fixed rate (rate_limit per second)
%%% - Burst capacity limits maximum tokens (burst_size)
%%% - Each request consumes N tokens
%%% - If insufficient tokens: reject request (fail-fast, don't queue)
%%%
%%% Configuration:
%%% - rate_limit: Requests per second (default: 1000)
%%% - burst_size: Maximum tokens in bucket (default: rate_limit/2)
%%% - timeout_ms: Request timeout (default: 5000)
%%% @end
%%%-------------------------------------------------------------------

-module(jidoka_rate_limiter).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([acquire/1, acquire/2]).
-export([status/0, reset/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

%% Internal state record
-record(state, {
    rate_limit :: float(),        % Tokens per second
    burst_size :: float(),        % Max tokens in bucket
    tokens :: float(),            % Current tokens
    last_refill :: integer(),     % Last refill timestamp (microseconds)
    rejected :: integer(),        % Count of rejected requests
    accepted :: integer()         % Count of accepted requests
}).

-define(DEFAULT_RATE_LIMIT, 1000).
-define(DEFAULT_BURST_SIZE, 500).
-define(DEFAULT_TIMEOUT_MS, 5000).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the rate limiter server.
%% Config:
%% - rate_limit: Tokens per second (default: 1000)
%% - burst_size: Max tokens (default: rate_limit/2)
%% - timeout_ms: Request timeout (default: 5000)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Acquire a single token from the rate limiter.
%% Returns: ok | {error, rate_limit_exceeded}
%%
%% Jidoka principle: If rate limit exceeded, immediately return error
%% rather than queueing the request. Fail-fast prevents overload.
-spec acquire(pos_integer()) -> ok | {error, rate_limit_exceeded}.
acquire(Tokens) ->
    acquire(Tokens, ?DEFAULT_TIMEOUT_MS).

%% @doc Acquire tokens with custom timeout.
-spec acquire(pos_integer(), pos_integer()) -> ok | {error, rate_limit_exceeded}.
acquire(Tokens, TimeoutMs) ->
    try
        gen_server:call(?MODULE, {acquire, Tokens}, TimeoutMs)
    catch
        exit:{timeout, _} ->
            logger:warning("Rate limiter timeout: tokens=~p", [Tokens]),
            {error, rate_limit_exceeded}
    end.

%% @doc Get rate limiter status.
%% Returns: {current_tokens, float(), accepted, integer(), rejected, integer()}
-spec status() -> {float(), integer(), integer()}.
status() ->
    gen_server:call(?MODULE, status).

%% @doc Reset rate limiter (clear rejected/accepted counters).
-spec reset() -> ok.
reset() ->
    gen_server:call(?MODULE, reset).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

%% @private Initialize the rate limiter.
init(Config) ->
    RateLimit = maps:get(rate_limit, Config, ?DEFAULT_RATE_LIMIT),
    BurstSize = maps:get(burst_size, Config, ?DEFAULT_BURST_SIZE),

    logger:info(
        "Rate limiter initialized: rate=~p/s, burst=~p",
        [RateLimit, BurstSize]
    ),

    State = #state{
        rate_limit = float(RateLimit),
        burst_size = float(BurstSize),
        tokens = float(BurstSize),  % Start with full bucket
        last_refill = erlang:system_time(microsecond),
        rejected = 0,
        accepted = 0
    },

    {ok, State}.

%% @private Handle synchronous calls.
handle_call({acquire, Tokens}, _From, State) ->
    NewState = refill_tokens(State),
    case has_tokens(NewState, Tokens) of
        true ->
            UpdatedState = NewState#state{
                tokens = NewState#state.tokens - Tokens,
                accepted = NewState#state.accepted + 1
            },
            logger:debug(
                "Rate limiter accepted: tokens=~p, remaining=~.1f",
                [Tokens, UpdatedState#state.tokens]
            ),
            {reply, ok, UpdatedState};
        false ->
            UpdatedState = NewState#state{
                rejected = NewState#state.rejected + 1
            },
            logger:warning(
                "Rate limiter exceeded: requested=~p, available=~.1f/~.1f",
                [Tokens, NewState#state.tokens, NewState#state.burst_size]
            ),
            {reply, {error, rate_limit_exceeded}, UpdatedState}
    end;

handle_call(status, _From, State) ->
    NewState = refill_tokens(State),
    {reply, {NewState#state.tokens, NewState#state.accepted, NewState#state.rejected}, NewState};

handle_call(reset, _From, State) ->
    NewState = State#state{rejected = 0, accepted = 0},
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private Handle asynchronous calls (none used).
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private Handle info messages (none used).
handle_info(_Info, State) ->
    {noreply, State}.

%% @private Cleanup on termination.
terminate(_Reason, _State) ->
    ok.

%% @private Code change handler.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Private Functions
%%%===================================================================

%% @private Refill tokens based on elapsed time.
%% Tokens = min(burst_size, current_tokens + (elapsed_ms * rate_limit / 1000))
refill_tokens(#state{rate_limit = Rate, burst_size = Burst, tokens = Tokens, last_refill = LastRefill} = State) ->
    Now = erlang:system_time(microsecond),
    ElapsedUs = Now - LastRefill,

    %% Calculate tokens to add: (elapsed_time_seconds * rate_limit)
    %% Using microseconds: elapsed_us / 1_000_000 * rate_limit
    TokensToAdd = (ElapsedUs / 1_000_000) * Rate,

    NewTokens = min(Burst, Tokens + TokensToAdd),
    State#state{tokens = NewTokens, last_refill = Now}.

%% @private Check if bucket has enough tokens.
has_tokens(#state{tokens = Tokens}, RequestedTokens) ->
    Tokens >= RequestedTokens.

%%%===================================================================
%%% Monitor Process (Optional Periodic Logging)
%%%===================================================================

%% @doc Start a monitoring process that logs rate limiter stats periodically.
-spec start_monitor() -> {ok, pid()}.
start_monitor() ->
    Pid = spawn_link(fun() -> monitor_loop() end),
    {ok, Pid}.

%% @private Monitoring loop.
monitor_loop() ->
    IntervalMs = 10000,  % Log every 10 seconds
    receive
        stop -> ok
    after
        IntervalMs ->
            {Tokens, Accepted, Rejected} = status(),
            logger:info(
                "Rate limiter status: tokens=~.1f, accepted=~p, rejected=~p",
                [Tokens, Accepted, Rejected]
            ),
            monitor_loop()
    end.
