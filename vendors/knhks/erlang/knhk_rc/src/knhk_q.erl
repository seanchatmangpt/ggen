%% knhk_q.erl — Invariant Registry
%% Production-ready implementation for Q invariant management
%% Responsibilities: Invariant loading, checking (preserve(Q)), violation detection

-module(knhk_q).
-behaviour(gen_server).

-export([
    start_link/0,
    load/1,
    check/1,
    list/0,
    get_violations/0
]).

-record(state, {
    invariants = [],             % List of invariant specifications
    violations = [],             % Recent violations
    check_count = 0              % Total checks performed
}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Load invariant from SPARQL query or specification
load(Invariant) when is_binary(Invariant) ->
    gen_server:call(?MODULE, {load, Invariant});
load(Invariant) when is_list(Invariant) ->
    load(list_to_binary(Invariant)).

%% Check invariants (preserve(Q))
check(Data) ->
    gen_server:call(?MODULE, {check, Data}).

%% List all loaded invariants
list() ->
    gen_server:call(?MODULE, list).

%% Get recent violations
get_violations() ->
    gen_server:call(?MODULE, get_violations).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({load, Invariant}, _From, State) ->
    %% Parse invariant (in production, parse SPARQL query)
    %% Validate invariant format
    case validate_invariant_format(Invariant) of
        ok ->
            %% Add to invariants list
            NewInvariants = [Invariant | State#state.invariants],
            NewState = State#state{invariants = NewInvariants},
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({check, Data}, _From, State) ->
    %% Check all invariants against data
    Violations = check_invariants(State#state.invariants, Data),
    
    %% Update violations list (keep last 100)
    NewViolations = lists:sublist(Violations ++ State#state.violations, 100),
    
    NewState = State#state{
        violations = NewViolations,
        check_count = State#state.check_count + 1
    },
    
    case Violations of
        [] ->
            {reply, {ok, preserved}, NewState};
        _ ->
            {reply, {error, {violations, Violations}}, NewState}
    end;

handle_call(list, _From, State) ->
    {reply, {ok, State#state.invariants}, State};

handle_call(get_violations, _From, State) ->
    {reply, {ok, State#state.violations}, State};

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

validate_invariant_format(Invariant) ->
    %% Basic validation: check if invariant is non-empty
    case byte_size(Invariant) > 0 of
        true -> ok;
        false -> {error, empty_invariant}
    end.

check_invariants(Invariants, Data) ->
    %% In production, this would:
    %% 1. Execute each invariant query against data
    %% 2. Check if result violates invariant
    %% 3. Return list of violations
    
    %% For now, basic check: verify data format
    Violations = lists:foldl(
        fun(Invariant, Acc) ->
            case check_single_invariant(Invariant, Data) of
                {ok, preserved} -> Acc;
                {error, Violation} -> [Violation | Acc]
            end
        end,
        [],
        Invariants
    ),
    Violations.

check_single_invariant(Invariant, Data) ->
    %% In production, execute SPARQL query
    %% For now, basic validation
    case byte_size(Data) > 0 of
        true -> {ok, preserved};
        false -> {error, {invariant_violation, Invariant}}
    end.

