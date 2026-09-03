%%====================================================================
%% interleaved_workflow - Interleaved Parallel Routing (WP17)
%%====================================================================
%% @doc WP17: Interleaved parallel routing. Multiple paths exist
%% but only one can execute at a time (mutex behavior).

-module(interleaved_workflow).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, trigger/3]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

%% API
-export([start_link/0, start_link/1, fire/2, marking/1, case_status/1]).

-include("include/gen_yawl.hrl").

-record(state, {
    locked_by = undefined :: undefined | atom(),
    completed_paths = [] :: [atom()]
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    start_link(#{}).

start_link(InitialData) ->
    gen_pnet:start_link(?MODULE, InitialData, []).

fire(Pid, Transition) ->
    gen_pnet:call(Pid, {fire, Transition}).

marking(Pid) ->
    gen_pnet:marking(Pid).

case_status(Pid) ->
    gen_pnet:call(Pid, case_status).

%%====================================================================
%% Structure Callbacks
%%====================================================================

place_lst() ->
    [
        p_input,
        p_start,
        p_mutex,
        p_path_a,
        p_path_b,
        p_merge,
        p_output
    ].

trsn_lst() ->
    [t_start, t_path_a, t_path_b, t_merge].

init_marking(p_input, _UsrInfo) ->
    [#case_token{
        case_id = generate_case_id(),
        spec_id = <<"interleaved">>,
        started_at = os:system_time(nanosecond)
    }];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_start) -> [p_input];
preset(t_path_a) -> [p_mutex, p_start];
preset(t_path_b) -> [p_mutex, p_start];
preset(t_merge) -> [p_path_a, p_path_b].

is_enabled(t_path_a, _Mode, _State) ->
    true;
is_enabled(t_path_b, _Mode, _State) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

%% Fire transitions
fire(t_start, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_start,
        status = completed,
        completed_at = os:system_time(nanosecond)
    },
    {produce, #{
        p_start => [Token],
        p_mutex => [Token]  %% Mutex token available
    }};

fire(t_path_a, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_path_a,
        status = completed,
        completed_at = os:system_time(nanosecond)
    },
    {produce, #{
        p_path_a => [Token],
        p_mutex => [Token]  %% Return mutex token
    }};

fire(t_path_b, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_path_b,
        status = completed,
        completed_at = os:system_time(nanosecond)
    },
    {produce, #{
        p_path_b => [Token],
        p_mutex => [Token]  %% Return mutex token
    }};

fire(t_merge, _Mode, _UsrInfo) ->
    {produce, #{
        p_merge => [#work_item_token{
            work_item_id = generate_id(),
            task_id = t_merge,
            status = completed,
            completed_at = os:system_time(nanosecond)
        }],
        p_output => [#case_token{
            case_id = generate_case_id(),
            spec_id = <<"interleaved">>,
            started_at = os:system_time(nanosecond)
        }]
    }}.

%%====================================================================
%% Interface Callbacks
%%====================================================================

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

handle_call({fire, Transition}, _From, NetState) ->
    State = get_state(NetState),
    Marking = gen_pnet:get_usr_info(NetState),
    Preset = preset(Transition),

    case can_fire(Transition, Preset, Marking, State) of
        true ->
            {produce, ProduceMap} = fire(Transition, Marking, NetState),
            UpdatedMarking = apply_produce(ProduceMap, Marking),
            UpdatedState = update_completed(State, Transition),
            {reply, ok, set_state(NetState, UpdatedMarking, UpdatedState)};
        false ->
            {reply, {error, not_enabled_or_locked}, NetState}
    end;

handle_call(case_status, _From, NetState) ->
    Marking = gen_pnet:get_usr_info(NetState),
    Status = case maps:get(p_output, Marking, []) of
        [] -> active;
        _ -> completed
    end,
    {reply, Status, NetState};

handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_request}, NetState}.

handle_cast(_Request, NetState) ->
    noreply.

handle_info(_Info, NetState) ->
    noreply.

init(NetArg) ->
    #state{}.

terminate(_Reason, _NetState) ->
    ok.

trigger(_Place, _Token, _NetState) ->
    pass.

%%====================================================================
%% Internal
%%====================================================================

get_state(NetState) ->
    case gen_pnet:get_usr_info(NetState) of
        State when is_record(State, state) -> State;
        _ -> #state{}
    end.

set_state(NetState, Marking, State) ->
    NetState#{marking => Marking, state => State}.

can_fire(Transition, Preset, Marking, State) ->
    HasPreset = lists:any(fun(P) -> maps:get(P, Marking, []) =/= [] end, Preset),
    if
        HasPreset =:= false -> false;
        Transition =:= t_path_a; Transition =:= t_path_b ->
            %% Check mutex availability
            case State#state.locked_by of
                undefined -> true;
                Locked -> Locked =:= Transition
            end;
        true -> true
    end.

apply_produce(ProduceMap, Marking) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        Current = maps:get(Place, Acc, []),
        Acc#{Place => Current ++ Tokens}
    end, Marking, ProduceMap).

update_completed(State, t_path_a) ->
    State#state{completed_paths = [path_a | State#state.completed_paths]};
update_completed(State, t_path_b) ->
    State#state{completed_paths = [path_b | State#state.completed_paths]};
update_completed(State, _) ->
    State.

generate_case_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)}))).

generate_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({make_ref(), os:system_time(nanosecond)}))).
