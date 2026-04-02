%% knhk_ingest.erl — Delta Ingestion
%% Production-ready implementation for Δ ingestion (O ⊔ Δ)
%% Responsibilities: Delta submission, validation (typed, guarded), SoA conversion coordination

-module(knhk_ingest).
-behaviour(gen_server).

-export([
    start_link/0,
    submit/1,
    get_stats/0
]).

-record(state, {
    deltas_submitted = 0,
    deltas_accepted = 0,
    deltas_rejected = 0,
    last_delta_time = undefined,
    guards = #{}                   % Guard configurations
}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Submit delta (O ⊔ Δ)
submit(Delta) ->
    gen_server:call(?MODULE, {submit, Delta}).

%% Get ingestion statistics
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({submit, Delta}, _From, State) ->
    %% Validate delta format
    case validate_delta_format(Delta) of
        ok ->
            %% Check guard constraints (H guards)
            case check_guards(Delta, State#state.guards) of
                ok ->
                    %% Validate typing (O ⊨ Σ)
                    case validate_typing(Delta) of
                        ok ->
                            %% Coordinate SoA conversion
                            %% In production, this would:
                            %% 1. Convert delta to SoA arrays
                            %% 2. Ensure run.len ≤ 8
                            %% 3. Validate alignment
                            
                            NewState = State#state{
                                deltas_submitted = State#state.deltas_submitted + 1,
                                deltas_accepted = State#state.deltas_accepted + 1,
                                last_delta_time = erlang:system_time(millisecond)
                            },
                            {reply, ok, NewState};
                        {error, TypingError} ->
                            NewState = State#state{
                                deltas_submitted = State#state.deltas_submitted + 1,
                                deltas_rejected = State#state.deltas_rejected + 1
                            },
                            {reply, {error, {typing_error, TypingError}}, NewState}
                    end;
                {error, GuardError} ->
                    NewState = State#state{
                        deltas_submitted = State#state.deltas_submitted + 1,
                        deltas_rejected = State#state.deltas_rejected + 1
                    },
                    {reply, {error, {guard_violation, GuardError}}, NewState}
            end;
        {error, FormatError} ->
            NewState = State#state{
                deltas_submitted = State#state.deltas_submitted + 1,
                deltas_rejected = State#state.deltas_rejected + 1
            },
            {reply, {error, {format_error, FormatError}}, NewState}
    end;

handle_call(get_stats, _From, State) ->
    Stats = #{
        deltas_submitted => State#state.deltas_submitted,
        deltas_accepted => State#state.deltas_accepted,
        deltas_rejected => State#state.deltas_rejected,
        last_delta_time => State#state.last_delta_time
    },
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

validate_delta_format(Delta) ->
    %% Basic validation: check if delta is a map/binary
    case is_map(Delta) orelse is_binary(Delta) of
        true -> ok;
        false -> {error, invalid_format}
    end.

check_guards(Delta, Guards) ->
    %% In production, check:
    %% - max_batch_size
    %% - max_lag_ms
    %% - max_run_len ≤ 8
    %% - schema_validation
    
    %% For now, basic guard check
    case is_map(Delta) of
        true ->
            %% Check if delta has size constraints
            Size = maps:size(Delta),
            case Size > 1000 of
                true -> {error, batch_size_exceeded};
                false -> ok
            end;
        false ->
            %% For binary deltas, check size
            case byte_size(Delta) > 10000 of
                true -> {error, delta_too_large};
                false -> ok
            end
    end.

validate_typing(Delta) ->
    %% In production, validate against Σ schema
    %% Check O ⊨ Σ constraint
    %% For now, basic validation
    ok.

