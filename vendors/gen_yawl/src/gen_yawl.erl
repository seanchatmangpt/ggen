%% -*- erlang -*-
%%
%% gen_yawl - YAWL Workflow Engine OTP Behavior
%% Minimal working version
%%
%% @author gen_yawl contributors
%% @version 1.0.0
%% @copyright 2025
%%
%% @doc YAWL workflow engine behavior built on gen_pnet.
%%
%% This module implements a YAWL (Yet Another Workflow Language) workflow
%% engine as an Erlang OTP behavior, using gen_pnet as the underlying
%% Petri net execution engine.
%%
%% @end
%% -------------------------------------------------------------------

-module(gen_yawl).
-behaviour(gen_pnet).

%%====================================================================
%% Exports
%%====================================================================

%% gen_pnet Structure Callbacks
-export([place_lst/0,
         trsn_lst/0,
         init_marking/2,
         preset/1,
         is_enabled/3,
         fire/3,
         trigger/3]).

%% gen_pnet Interface Callbacks
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("gen_pnet/include/gen_pnet.hrl").
-include("gen_yawl.hrl").

%%====================================================================
%% gen_pnet Structure Callbacks
%%====================================================================

%% @doc Returns the list of all places in the workflow net.
-spec place_lst() -> [atom()].

place_lst() ->
    UsrInfo = get_usr_info(),
    Spec = maps:get(spec, UsrInfo),
    generate_places(Spec).

%% @doc Returns the list of all transitions in the workflow net.
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    UsrInfo = get_usr_info(),
    Spec = maps:get(spec, UsrInfo),
    generate_transitions(Spec).

%% @doc Returns the initial marking for a given place.
-spec init_marking(Place :: atom(), UsrInfo :: map()) -> [term()].

init_marking(Place, UsrInfo) ->
    Spec = maps:get(spec, UsrInfo),
    InputCond = Spec#yawl_spec.input_condition,
    CaseId = maps:get(case_id, UsrInfo),
    SpecId = Spec#yawl_spec.id,
    CaseData = maps:get(case_data, UsrInfo, #{}),

    case Place of
        InputCond ->
            [#case_token{
                case_id = CaseId,
                spec_id = SpecId,
                started_at = os:system_time(nanosecond),
                case_data = CaseData
            }];
        _ ->
            []
    end.

%% @doc Returns the preset places for a given transition.
-spec preset(Trsn :: atom()) -> [atom()].

preset(Transition) ->
    UsrInfo = get_usr_info(),
    Spec = maps:get(spec, UsrInfo),
    TaskId = transition_to_task_id(Transition, Spec),

    case TaskId of
        undefined -> [];
        _ ->
            TaskDef = maps:get(TaskId, Spec#yawl_spec.tasks, undefined),
            get_preset_places(TaskDef, Spec)
    end.

%% @doc Determines if a transition is enabled in a given mode.
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: map()) -> boolean().

is_enabled(Transition, Mode, UsrInfo) ->
    Spec = maps:get(spec, UsrInfo),
    TaskId = transition_to_task_id(Transition, Spec),

    case TaskId of
        undefined -> false;
        _ ->
            TaskDef = maps:get(TaskId, Spec#yawl_spec.tasks, undefined),
            check_enabled(TaskDef, Transition, Mode, UsrInfo)
    end.

%% @doc Fires a transition, producing tokens on output places.
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: map()) ->
          abort | {produce, map()}.

fire(Transition, Mode, UsrInfo) ->
    Spec = maps:get(spec, UsrInfo),
    CaseId = maps:get(case_id, UsrInfo),

    TaskId = transition_to_task_id(Transition, Spec),

    case TaskId of
        undefined ->
            abort;
        _ ->
            TaskDef = maps:get(TaskId, Spec#yawl_spec.tasks),
            execute_transition(TaskDef, Transition, Mode, CaseId, Spec)
    end.

%% @doc Trigger callback for side effects when tokens arrive.
-spec trigger(Place :: atom(), Token :: term(), NetState :: #net_state{}) ->
          pass | drop.

trigger(_Place, _Token, _NetState) ->
    pass.

%%====================================================================
%% gen_pnet Interface Callbacks
%%====================================================================

%% @doc Initializes the workflow instance.
-spec init(Args :: map()) -> map().

init(Args) ->
    Spec = maps:get(spec, Args),
    CaseId = case maps:get(case_id, Args, undefined) of
        undefined -> generate_case_id();
        Id -> Id
    end,
    CaseData = maps:get(case_data, Args, #{}),

    #{
        spec => Spec,
        case_id => CaseId,
        case_data => CaseData,
        work_items => #{},
        completed_tasks => sets:new(),
        receipts => []
    }.

%% @doc Handles synchronous calls to the workflow instance.
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: #net_state{}) ->
          {reply, term(), #net_state{}} | {reply, term(), #net_state{}, map()} | {noreply, #net_state{}} | {noreply, #net_state{}, map()}.

handle_call(get_state, _From, NetState) ->
    UsrInfo = NetState#net_state.usr_info,
    {reply, {ok, UsrInfo}, NetState};

handle_call(get_case_id, _From, NetState) ->
    UsrInfo = NetState#net_state.usr_info,
    CaseId = maps:get(case_id, UsrInfo),
    {reply, {ok, CaseId}, NetState};

handle_call(get_work_items, _From, NetState) ->
    UsrInfo = NetState#net_state.usr_info,
    WorkItems = maps:get(work_items, UsrInfo),
    {reply, {ok, WorkItems}, NetState};

handle_call({complete_work_item, WorkItemId, Result}, _From, NetState) ->
    UsrInfo = NetState#net_state.usr_info,
    WorkItems = maps:get(work_items, UsrInfo),
    case maps:get(WorkItemId, WorkItems, undefined) of
        undefined ->
            {reply, {error, not_found}, NetState};
        WorkItem ->
            UpdatedWorkItem = WorkItem#work_item{
                status = completed,
                completed_at = os:system_time(nanosecond),
                data = maps:merge(WorkItem#work_item.data, Result)
            },
            UpdatedWorkItems = WorkItems#{WorkItemId => UpdatedWorkItem},
            UpdatedUsrInfo = UsrInfo#{work_items => UpdatedWorkItems},

            {reply, ok, NetState#net_state{
                usr_info = UpdatedUsrInfo
            }}
    end;

handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_request}, NetState}.

%% @doc Handles asynchronous casts to the workflow instance.
-spec handle_cast(Request :: term(), NetState :: #net_state{}) ->
          {noreply, #net_state{}} | {noreply, #net_state{}, map()} | {stop, term(), #net_state{}}.

handle_cast(_Request, NetState) ->
    {noreply, NetState}.

%% @doc Handles asynchronous info messages.
-spec handle_info(Info :: term(), NetState :: #net_state{}) ->
          {noreply, #net_state{}} | {noreply, #net_state{}, map()} | {stop, term(), #net_state{}}.

handle_info(_Info, NetState) ->
    {noreply, NetState}.

%% @doc Handles code changes during hot reload.
-spec code_change(OldVsn :: term(), NetState :: #net_state{}, Extra :: term()) ->
          {ok, #net_state{}} | {error, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%% @doc Cleanup when the workflow instance terminates.
-spec terminate(Reason :: term(), NetState :: #net_state{}) -> ok.

terminate(_Reason, _NetState) ->
    ok.

%%====================================================================
%% Internal Functions - Place/Transition Generation
%%====================================================================

%% @private Generate place list from specification
-spec generate_places(Spec :: #yawl_spec{}) -> [atom()].

generate_places(#yawl_spec{tasks = Tasks,
                           input_condition = InputCond,
                           output_condition = OutputCond}) ->
    %% Base places
    BasePlaces = [InputCond, OutputCond],

    %% Task places
    TaskPlaces = [?YAWL_PLACE(TaskId) || TaskId <- maps:keys(Tasks)],

    %% Join places for AND/OR joins
    JoinPlaces = lists:filtermap(
        fun(TaskId) ->
            TaskDef = maps:get(TaskId, Tasks),
            case TaskDef#task_def.join_type of
                'and' -> {true, ?YAWL_PLACE(and_join, TaskId)};
                'or' -> {true, ?YAWL_PLACE(or_join, TaskId)};
                _ -> false
            end
        end,
        maps:keys(Tasks)
    ),

    BasePlaces ++ TaskPlaces ++ JoinPlaces.

%% @private Generate transition list from specification
-spec generate_transitions(Spec :: #yawl_spec{}) -> [atom()].

generate_transitions(#yawl_spec{tasks = Tasks}) ->
    %% Main task transitions (skip empty tasks)
    lists:filtermap(
        fun(TaskId) ->
            TaskDef = maps:get(TaskId, Tasks),
            case TaskDef#task_def.kind of
                empty -> false;
                _ -> {true, ?YAWL_TRANSITION(TaskId)}
            end
        end,
        maps:keys(Tasks)
    ).

%% @private Convert transition name to task ID
-spec transition_to_task_id(Transition :: atom(), Spec :: #yawl_spec{}) ->
          atom() | undefined.

transition_to_task_id(Transition, _Spec) ->
    %% Extract task ID from transition name
    AtomList = atom_to_list(Transition),
    case AtomList of
        "t_" ++ Rest ->
            %% Check if there's a split type prefix
            case string:split(Rest, "_", leading) of
                [_SplitType, TaskIdStr] -> list_to_existing_atom(TaskIdStr);
                [TaskIdStr] -> list_to_existing_atom(TaskIdStr)
            end;
        _ ->
            undefined
    end.

%% @private Get preset places for a task
-spec get_preset_places(TaskDef :: #task_def{} | undefined, Spec :: #yawl_spec{}) ->
          [atom()].

get_preset_places(undefined, _Spec) ->
    [];

get_preset_places(TaskDef, Spec) ->
    TaskId = TaskDef#task_def.id,
    JoinType = TaskDef#task_def.join_type,

    %% Find all incoming flows to this task
    IncomingFlows = [F || F <- Spec#yawl_spec.flows, F#flow.to =:= TaskId],

    %% For AND/OR joins, tokens go through join place first
    case JoinType of
        'and' ->
            JoinPlace = ?YAWL_PLACE(and_join, TaskId),
            [JoinPlace];
        'or' ->
            JoinPlace = ?YAWL_PLACE(or_join, TaskId),
            [JoinPlace];
        _ ->
            %% Sequence or XOR join - direct from predecessor places
            [?YAWL_PLACE(F#flow.from) || F <- IncomingFlows]
    end.

%% @private Check if a transition is enabled
-spec check_enabled(TaskDef :: #task_def{} | undefined,
                    Transition :: atom(),
                    Mode :: map(),
                    UsrInfo :: map()) ->
          boolean().

check_enabled(undefined, _Transition, _Mode, _UsrInfo) ->
    false;

check_enabled(TaskDef, Transition, Mode, _UsrInfo) ->
    JoinType = TaskDef#task_def.join_type,
    Preset = preset(Transition),

    case JoinType of
        'and' ->
            %% AND join: all preset places must have tokens
            lists:all(fun(P) -> maps:get(P, Mode, []) =/= [] end, Preset);
        'or' ->
            %% OR join: at least one active branch must have token
            lists:any(fun(P) -> maps:get(P, Mode, []) =/= [] end, Preset);
        _ ->
            %% Sequence or XOR join: at least one preset place has token
            lists:any(fun(P) -> maps:get(P, Mode, []) =/= [] end, Preset)
    end.

%% @private Execute a transition and produce output tokens
-spec execute_transition(TaskDef :: #task_def{} | undefined,
                         Transition :: atom(),
                         Mode :: map(),
                         CaseId :: binary(),
                         Spec :: #yawl_spec{}) ->
          abort | {produce, map()}.

execute_transition(undefined, _Transition, _Mode, _CaseId, _Spec) ->
    abort;

execute_transition(TaskDef, _Transition, _Mode, CaseId, Spec) ->
    TaskId = TaskDef#task_def.id,
    SplitType = TaskDef#task_def.split_type,

    %% Find outgoing flows from this task
    OutgoingFlows = [F || F <- Spec#yawl_spec.flows, F#flow.from =:= TaskId],

    %% Generate output based on split type
    case SplitType of
        'and' ->
            %% AND split: produce tokens on all outgoing branches
            ProduceMap = maps:from_list(
                [{?YAWL_PLACE(F#flow.to), [#work_item{
                    id = generate_id(),
                    case_id = CaseId,
                    task_id = F#flow.to,
                    status = enabled,
                    enabled_at = os:system_time(nanosecond)
                }]} || F <- OutgoingFlows]
            ),
            {produce, ProduceMap};
        'xor' ->
            %% XOR split: select one branch
            SelectedFlow = select_first_branch(OutgoingFlows, #{}),
            case SelectedFlow of
                undefined ->
                    {produce, #{}};
                Flow ->
                    {produce, #{?YAWL_PLACE(Flow#flow.to) => [#work_item{
                        id = generate_id(),
                        case_id = CaseId,
                        task_id = Flow#flow.to,
                        status = enabled,
                        enabled_at = os:system_time(nanosecond)
                    }]}}
            end;
        'or' ->
            %% OR split: select branches based on conditions
            SelectedFlows = select_matching_branches(OutgoingFlows, #{}),
            ProduceMap = maps:from_list(
                [{?YAWL_PLACE(F#flow.to), [#work_item{
                    id = generate_id(),
                    case_id = CaseId,
                    task_id = F#flow.to,
                    status = enabled,
                    enabled_at = os:system_time(nanosecond)
                }]} || F <- SelectedFlows]
            ),
            {produce, ProduceMap};
        sequence ->
            %% Sequence: produce token on single output (if any)
            case OutgoingFlows of
                [] ->
                    {produce, #{}};
                [Flow | _] ->
                    {produce, #{?YAWL_PLACE(Flow#flow.to) => [#work_item{
                        id = generate_id(),
                        case_id = CaseId,
                        task_id = Flow#flow.to,
                        status = enabled,
                        enabled_at = os:system_time(nanosecond)
                    }]}}
            end
    end.

%% @private Select first branch with true condition (or default)
-spec select_first_branch([#flow{}], map()) -> #flow{} | undefined.

select_first_branch([], _Data) ->
    undefined;
select_first_branch(Flows, Data) ->
    %% Sort by priority, then check conditions
    SortedFlows = lists:sort(
        fun(A, B) -> A#flow.priority =< B#flow.priority end,
        Flows
    ),
    %% Find first flow with true condition (or default)
    case lists:search(
        fun(F) ->
            case F#flow.condition of
                undefined -> true;
                Cond -> Cond(Data)
            end
        end,
        SortedFlows
    ) of
        {value, Flow} -> Flow;
        false ->
            %% Return default if exists
            case lists:search(fun(F) -> F#flow.is_default end, Flows) of
                {value, DefaultFlow} -> DefaultFlow;
                false -> undefined
            end
    end.

%% @private Select all branches with true conditions
-spec select_matching_branches([#flow{}], map()) -> [#flow{}].

select_matching_branches(Flows, Data) ->
    lists:filter(
        fun(F) ->
            case F#flow.condition of
                undefined -> true;
                Cond -> Cond(Data)
            end
        end,
        Flows
    ).

%% @private Generate a unique case ID
-spec generate_case_id() -> binary().

generate_case_id() ->
    Timestamp = os:system_time(nanosecond),
    Random = crypto:strong_rand_bytes(8),
    Hash = crypto:hash(md5, <<Timestamp:64, Random/binary>>),
    binary:encode_hex(Hash).

%% @private Generate a unique ID
-spec generate_id() -> binary().

generate_id() ->
    Timestamp = os:system_time(nanosecond),
    Random = crypto:strong_rand_bytes(8),
    Hash = crypto:hash(md5, <<Timestamp:64, Random/binary>>),
    binary:encode_hex(Hash).

%% @private Get user info from process dictionary (placeholder)
-spec get_usr_info() -> map().

get_usr_info() ->
    %% This is called during gen_pnet callbacks
    %% For now, return a minimal placeholder
    #{
        spec => undefined,
        case_id => <<>>,
        case_data => #{},
        work_items => #{},
        completed_tasks => sets:new(),
        receipts => []
    }.
