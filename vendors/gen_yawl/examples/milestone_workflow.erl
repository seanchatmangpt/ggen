%%====================================================================
%% milestone_workflow - Milestone Pattern Workflow (WP18)
%%====================================================================
%% @doc WP18: Milestone pattern. A task is only enabled after a
%% prerequisite milestone is reached.

-module(milestone_workflow).
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
    milestone_reached = false :: boolean()
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
        p_prerequisite,
        p_milestone,
        p_milestone_task,
        p_output
    ].

trsn_lst() ->
    [t_prerequisite, t_milestone, t_milestone_task].

init_marking(p_input, _UsrInfo) ->
    [#case_token{
        case_id = generate_case_id(),
        spec_id = <<"milestone">>,
        started_at = os:system_time(nanosecond)
    }];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_prerequisite) -> [p_input];
preset(t_milestone) -> [p_prerequisite];
preset(t_milestone_task) -> [p_milestone].

is_enabled(t_milestone_task, _Mode, _UsrInfo) ->
    %% Only enabled after milestone reached
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

%% Fire transitions
fire(t_prerequisite, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_prerequisite,
        status = completed,
        completed_at = os:system_time(nanosecond)
    },
    {produce, #{p_prerequisite => [Token]}};

fire(t_milestone, _Mode, _UsrInfo) ->
    %% Reaching milestone
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_milestone,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{milestone => reached}
    },
    {produce, #{p_milestone => [Token]}};

fire(t_milestone_task, _Mode, _UsrInfo) ->
    %% Milestone-dependent task
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_milestone_task,
        status = completed,
        completed_at = os:system_time(nanosecond)
    },
    {produce, #{
        p_milestone_task => [Token],
        p_output => [#case_token{
            case_id = generate_case_id(),
            spec_id = <<"milestone">>,
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
    Preset = preset(Transition),
    Marking = gen_pnet:get_usr_info(NetState),

    case can_fire(Transition, Preset, Marking, State) of
        true ->
            {produce, ProduceMap} = fire(Transition, Marking, NetState),
            UpdatedMarking = apply_produce(ProduceMap, Marking),
            UpdatedState = update_state(State, Transition),
            {reply, ok, set_state(NetState, UpdatedMarking, UpdatedState)};
        false ->
            {reply, {error, not_enabled}, NetState}
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

can_fire(t_milestone_task, Preset, Marking, State) ->
    %% Milestone task requires milestone to be reached
    HasPreset = lists:any(fun(P) -> maps:get(P, Marking, []) =/= [] end, Preset),
    HasPreset andalso State#state.milestone_reached;
can_fire(_Transition, Preset, _Marking, _State) ->
    lists:any(fun(P) -> maps:get(P, _Marking, []) =/= [] end, Preset).

apply_produce(ProduceMap, Marking) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        Current = maps:get(Place, Acc, []),
        Acc#{Place => Current ++ Tokens}
    end, Marking, ProduceMap).

update_state(State, t_milestone) ->
    State#state{milestone_reached = true};
update_state(State, _) ->
    State.

generate_case_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)}))).

generate_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({make_ref(), os:system_time(nanosecond)}))).
