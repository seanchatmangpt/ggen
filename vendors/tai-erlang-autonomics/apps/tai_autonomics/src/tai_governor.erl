%%%-------------------------------------------------------------------
%% @doc tai_governor: Generic governor gen_statem
%%
%% States: boot, stable, warning, intervening, refusing
%% Invariants:
%%   - Entitlement gate enforcement
%%   - Max 1 in-flight action per tenant
%%   - Postpone storm events while intervening
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_governor).
-behaviour(gen_statem).

%% API
-export([start_link/2, signal/2, get_state/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3]).
-export([boot/3, stable/3, warning/3, intervening/3, refusing/3]).

-include("tai_autonomics.hrl").

-record(state, {
    tenant_id :: binary(),
    governor_id :: binary(),
    current_state :: atom(),
    in_flight_action :: undefined | {ActionId :: binary(), Pid :: pid()},
    postponed_events :: [term()]
}).

%%%===================================================================
%% API
%%%===================================================================

-spec start_link(TenantId, GovernorId) -> {ok, pid()} | {error, term()}
  when TenantId :: binary(),
       GovernorId :: binary().
start_link(TenantId, GovernorId) ->
    gen_statem:start_link(
        {via, gproc, {n, l, {tai_governor, TenantId}}},
        ?MODULE,
        {TenantId, GovernorId},
        []
    ).

-spec signal(Pid, Signal) -> {ok, State, Receipt} | {error, Reason}
  when Pid :: pid(),
       Signal :: map(),
       State :: atom(),
       Receipt :: map(),
       Reason :: atom().
signal(Pid, Signal) ->
    gen_statem:call(Pid, {signal, Signal}).

-spec get_state(Pid) -> {ok, State}
  when Pid :: pid(),
       State :: atom().
get_state(Pid) ->
    gen_statem:call(Pid, get_state).

%%%===================================================================
%% gen_statem callbacks
%%%===================================================================

-spec callback_mode() -> [state_functions | state_enter].
callback_mode() -> [state_functions, state_enter].

init({TenantId, GovernorId}) ->
    State = #state{
        tenant_id = TenantId,
        governor_id = GovernorId,
        current_state = ?GOVERNOR_STATE_BOOT,
        in_flight_action = undefined,
        postponed_events = []
    },
    {ok, ?GOVERNOR_STATE_BOOT, State}.

boot(enter, _OldState, State) ->
    {keep_state, State};
boot({call, From}, {signal, Signal}, State) ->
    %% Check entitlement
    case check_entitlement(State#state.tenant_id) of
        {ok, active} ->
            NewState = State#state{current_state = ?GOVERNOR_STATE_STABLE},
            {next_state, ?GOVERNOR_STATE_STABLE, NewState, [{reply, From, {ok, ?GOVERNOR_STATE_STABLE, create_receipt(State, Signal, ?GOVERNOR_STATE_STABLE)}}]};
        {ok, inactive} ->
            RefusalReceipt = tai_receipts:create_refusal(entitlement_inactive),
            {keep_state, State, [{reply, From, {error, entitlement_inactive}}]}
    end.

stable(enter, _OldState, State) ->
    {keep_state, State};
stable({call, From}, {signal, Signal}, State) ->
    case evaluate_signal(Signal) of
        {ok, no_action} ->
            Receipt = create_receipt(State, Signal, ?GOVERNOR_STATE_STABLE),
            {keep_state, State, [{reply, From, {ok, ?GOVERNOR_STATE_STABLE, Receipt}}]};
        {ok, action_required, ActionSpec} ->
            case State#state.in_flight_action of
                undefined ->
                    ActionId = generate_action_id(),
                    {ok, WorkerPid} = tai_actions:execute_async(
                        State#state.tenant_id,
                        ActionId,
                        ActionSpec
                    ),
                    NewState = State#state{
                        in_flight_action = {ActionId, WorkerPid},
                        current_state = ?GOVERNOR_STATE_INTERVENING
                    },
                    Receipt = create_receipt(State, Signal, ?GOVERNOR_STATE_INTERVENING),
                    {next_state, ?GOVERNOR_STATE_INTERVENING, NewState, [{reply, From, {ok, ?GOVERNOR_STATE_INTERVENING, Receipt}}]};
                _ ->
                    %% Postpone: action already in flight
                    NewState = State#state{
                        postponed_events = [{signal, Signal, From} | State#state.postponed_events]
                    },
                    {keep_state, NewState, [{reply, From, {ok, ?GOVERNOR_STATE_STABLE, create_receipt(State, Signal, ?GOVERNOR_STATE_STABLE)}}]}
            end;
        {ok, warning, _} ->
            NewState = State#state{current_state = ?GOVERNOR_STATE_WARNING},
            Receipt = create_receipt(State, Signal, ?GOVERNOR_STATE_WARNING),
            {next_state, ?GOVERNOR_STATE_WARNING, NewState, [{reply, From, {ok, ?GOVERNOR_STATE_WARNING, Receipt}}]}
    end;
stable({call, From}, get_state, State) ->
    {keep_state, State, [{reply, From, {ok, ?GOVERNOR_STATE_STABLE}}]}.

warning(enter, _OldState, State) ->
    {keep_state, State};
warning({call, From}, {signal, Signal}, State) ->
    case evaluate_signal(Signal) of
        {ok, no_action} ->
            NewState = State#state{current_state = ?GOVERNOR_STATE_STABLE},
            Receipt = create_receipt(State, Signal, ?GOVERNOR_STATE_STABLE),
            {next_state, ?GOVERNOR_STATE_STABLE, NewState, [{reply, From, {ok, ?GOVERNOR_STATE_STABLE, Receipt}}]};
        {ok, action_required, ActionSpec} ->
            case State#state.in_flight_action of
                undefined ->
                    ActionId = generate_action_id(),
                    {ok, WorkerPid} = tai_actions:execute_async(
                        State#state.tenant_id,
                        ActionId,
                        ActionSpec
                    ),
                    NewState = State#state{
                        in_flight_action = {ActionId, WorkerPid},
                        current_state = ?GOVERNOR_STATE_INTERVENING
                    },
                    Receipt = create_receipt(State, Signal, ?GOVERNOR_STATE_INTERVENING),
                    {next_state, ?GOVERNOR_STATE_INTERVENING, NewState, [{reply, From, {ok, ?GOVERNOR_STATE_INTERVENING, Receipt}}]};
                _ ->
                    NewState = State#state{
                        postponed_events = [{signal, Signal, From} | State#state.postponed_events]
                    },
                    {keep_state, NewState, [{reply, From, {ok, ?GOVERNOR_STATE_WARNING, create_receipt(State, Signal, ?GOVERNOR_STATE_WARNING)}}]}
            end
    end;
warning({call, From}, get_state, State) ->
    {keep_state, State, [{reply, From, {ok, ?GOVERNOR_STATE_WARNING}}]}.

intervening(enter, _OldState, State) ->
    {keep_state, State};
intervening({call, From}, {signal, Signal}, State) ->
    %% Postpone all signals while intervening
    NewState = State#state{
        postponed_events = [{signal, Signal, From} | State#state.postponed_events]
    },
    Receipt = create_receipt(State, Signal, ?GOVERNOR_STATE_INTERVENING),
    {keep_state, NewState, [{reply, From, {ok, ?GOVERNOR_STATE_INTERVENING, Receipt}}]};
intervening(info, {action_complete, ActionId, Result}, State) ->
    case State#state.in_flight_action of
        {ActionId, _Pid} ->
            NewState = State#state{
                in_flight_action = undefined,
                current_state = ?GOVERNOR_STATE_STABLE
            },
            %% Process postponed events
            process_postponed_events(NewState),
            {next_state, ?GOVERNOR_STATE_STABLE, NewState};
        _ ->
            {keep_state, State}
    end;
intervening({call, From}, get_state, State) ->
    {keep_state, State, [{reply, From, {ok, ?GOVERNOR_STATE_INTERVENING}}]}.

refusing(enter, _OldState, State) ->
    {keep_state, State};
refusing({call, From}, {signal, _Signal}, State) ->
    RefusalReceipt = tai_receipts:create_refusal(governor_refusing),
    {keep_state, State, [{reply, From, {error, refusing}}]};
refusing({call, From}, get_state, State) ->
    {keep_state, State, [{reply, From, {ok, ?GOVERNOR_STATE_REFUSING}}]}.

terminate(_Reason, _State, _Data) ->
    ok.

%%%===================================================================
%% Internal functions
%%%===================================================================

-spec check_entitlement(TenantId) -> {ok, active | inactive}
  when TenantId :: binary().
check_entitlement(TenantId) ->
    %% Check with entitlement governor via gproc
    %% Look for entitlement governors registered with pattern: {entitlement_governor, TenantId, EntitlementId}
    %% We'll check if any entitlement governor exists for this tenant and is in 'entitled' state
    case gproc:where({n, l, {entitlement_governor, TenantId}}) of
        undefined ->
            %% Try alternative pattern: check for any entitlement governor with this tenant
            %% Use select to find all entitlement governors for this tenant
            Pattern = [{{n, l, {'$1', '$2', '$3'}}, [{'==', '$1', {const, entitlement_governor}}, {'==', '$2', TenantId}], ['$3']}],
            case gproc:select(Pattern) of
                [] ->
                    {ok, inactive};
                EntitlementIds when is_list(EntitlementIds) ->
                    %% Check if any entitlement is in 'entitled' state
                    case lists:any(fun(EntitlementId) ->
                        case gproc:where({n, l, {entitlement_governor, TenantId, EntitlementId}}) of
                            undefined ->
                                false;
                            Pid ->
                                try
                                    case gen_statem:call(Pid, {get_state, EntitlementId}, 5000) of
                                        {ok, entitled} -> true;
                                        _ -> false
                                    end
                                catch
                                    _:_ -> false
                                end
                        end
                    end, EntitlementIds) of
                        true ->
                            {ok, active};
                        false ->
                            {ok, inactive}
                    end
            end;
        Pid ->
            %% Found a governor directly registered with tenant ID
            try
                case gen_statem:call(Pid, get_state, 5000) of
                    {ok, entitled} ->
                        {ok, active};
                    _ ->
                        {ok, inactive}
                end
            catch
                _:_ ->
                    {ok, inactive}
            end
    end.

-spec evaluate_signal(Signal) -> {ok, no_action} | {ok, action_required, ActionSpec} | {ok, warning, map()}
  when Signal :: map(),
       ActionSpec :: map().
evaluate_signal(Signal) ->
    Value = maps:get(value, Signal, 0.0),
    Metric = maps:get(metric, Signal, <<>>),
    case Value > 80.0 of
        true ->
            {ok, action_required, #{
                type => <<"scale_up">>,
                metric => Metric,
                value => Value
            }};
        false ->
            case Value > 60.0 of
                true ->
                    {ok, warning, #{metric => Metric, value => Value}};
                false ->
                    {ok, no_action}
            end
    end.

-spec create_receipt(State, Signal, NewState) -> map()
  when State :: #state{},
       Signal :: map(),
       NewState :: atom().
create_receipt(State, Signal, NewState) ->
    tai_receipts:create_transition_receipt(
        State#state.tenant_id,
        State#state.governor_id,
        <<"state_transition">>,
        NewState,
        Signal
    ).

-spec generate_action_id() -> binary().
generate_action_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    list_to_binary(io_lib:format("~32.16.0b", [Id])).

-spec process_postponed_events(State) -> ok
  when State :: #state{}.
process_postponed_events(State) ->
    lists:foreach(fun({signal, Signal, From}) ->
        gen_statem:reply(From, {ok, State#state.current_state, create_receipt(State, Signal, State#state.current_state)})
    end, State#state.postponed_events),
    ok.
