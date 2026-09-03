%%====================================================================
%% cancellation_workflow - Cancellation Workflow (WP19+WP20)
%%====================================================================
%% @doc WP19: Cancel Task, WP20: Cancel Case patterns.
%% Demonstrates region-based cancellation and full case cancellation.

-module(cancellation_workflow).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, trigger/3]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

%% API
-export([start_link/0, start_link/1, fire/2, marking/1, case_status/1,
         cancel_task/2, cancel_case/1]).

-include("include/gen_yawl.hrl").

-record(state, {
    status = active :: active | cancelled | completed,
    cancelled_tasks = [] :: [atom()]
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

cancel_task(Pid, TaskId) ->
    gen_pnet:call(Pid, {cancel_task, TaskId}).

cancel_case(Pid) ->
    gen_pnet:call(Pid, cancel_case).

%%====================================================================
%% Structure Callbacks
%%====================================================================

place_lst() ->
    [
        p_input,
        p_start,
        p_task_a,
        p_task_b,
        p_task_c,
        p_cancel_trigger,
        p_output
    ].

trsn_lst() ->
    [
        t_start,
        t_task_a,
        t_task_b,
        t_task_c,
        t_cancel
    ].

init_marking(p_input, _UsrInfo) ->
    [#case_token{
        case_id = generate_case_id(),
        spec_id = <<"cancellation">>,
        started_at = os:system_time(nanosecond)
    }];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_start) -> [p_input];
preset(t_task_a) -> [p_start];
preset(t_task_b) -> [p_start];
preset(t_task_c) -> [p_start];
preset(t_cancel) -> [p_cancel_trigger].

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
        p_task_a => [Token],
        p_task_b => [Token],
        p_task_c => [Token]
    }};

fire(t_task_a, _Mode, _UsrInfo) ->
    {produce, #{p_task_a => [#work_item_token{
        work_item_id = generate_id(),
        task_id = t_task_a,
        status = completed,
        completed_at = os:system_time(nanosecond)
    }]}};

fire(t_task_b, _Mode, _UsrInfo) ->
    {produce, #{p_task_b => [#work_item_token{
        work_item_id = generate_id(),
        task_id = t_task_b,
        status = completed,
        completed_at = os:system_time(nanosecond)
    }]}};

fire(t_task_c, _Mode, _UsrInfo) ->
    {produce, #{p_task_c => [#work_item_token{
        work_item_id = generate_id(),
        task_id = t_task_c,
        status = completed,
        completed_at = os:system_time(nanosecond)
    }]}};

fire(t_cancel, _Mode, _UsrInfo) ->
    %% Cancellation trigger
    {produce, #{p_cancel_trigger => [#work_item_token{
        work_item_id = generate_id(),
        task_id = t_cancel,
        status = completed,
        completed_at = os:system_time(nanosecond)
    }]}}.

%%====================================================================
%% Interface Callbacks
%%====================================================================

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

handle_call({fire, Transition}, _From, NetState) ->
    State = get_state(NetState),
    case State#state.status of
        cancelled ->
            {reply, {error, case_cancelled}, NetState};
        active ->
            Preset = preset(Transition),
            Marking = gen_pnet:get_usr_info(NetState),
            case can_fire(Transition, Preset, Marking) of
                true ->
                    {produce, ProduceMap} = fire(Transition, Marking, NetState),
                    UpdatedMarking = apply_produce(ProduceMap, Marking),
                    {reply, ok, set_state(NetState, UpdatedMarking, State)};
                false ->
                    {reply, {error, not_enabled}, NetState}
            end
    end;

handle_call({cancel_task, TaskId}, _From, NetState) ->
    %% WP19: Cancel specific task
    Marking = gen_pnet:get_usr_info(NetState),
    State = get_state(NetState),

    Place = task_to_place(TaskId),
    UpdatedMarking = Marking#{Place => []},
    UpdatedState = State#state{
        cancelled_tasks = [TaskId | State#state.cancelled_tasks]
    },

    {reply, ok, set_state(NetState, UpdatedMarking, UpdatedState)};

handle_call(cancel_case, _From, NetState) ->
    %% WP20: Cancel entire case - remove all tokens
    EmptyMarking = #{},
    CancelledState = #state{status = cancelled, cancelled_tasks = []},
    {reply, ok, set_state(NetState, EmptyMarking, CancelledState)};

handle_call(case_status, _From, NetState) ->
    State = get_state(NetState),
    {reply, State#state.status, NetState};

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

can_fire(Transition, Preset, Marking) ->
    lists:any(fun(P) -> maps:get(P, Marking, []) =/= [] end, Preset).

apply_produce(ProduceMap, Marking) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        Current = maps:get(Place, Acc, []),
        Acc#{Place => Current ++ Tokens}
    end, Marking, ProduceMap).

task_to_place(task_a) -> p_task_a;
task_to_place(task_b) -> p_task_b;
task_to_place(task_c) -> p_task_c;
task_to_place(TaskId) ->
    list_to_existing_atom("p_" ++ atom_to_list(TaskId)).

generate_case_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)}))).

generate_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({make_ref(), os:system_time(nanosecond)}))).
