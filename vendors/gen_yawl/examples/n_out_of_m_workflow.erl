%%====================================================================
%% n_out_of_m_workflow - N-out-of-M Join Pattern (WP14)
%%====================================================================
%% @doc WP14: N-out-of-M pattern. Continuation triggers when N out of
%% M branches complete, without waiting for all branches.

-module(n_out_of_m_workflow).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, trigger/3]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

%% API
-export([start_link/1, fire/2, marking/1, case_status/1]).

-include("include/gen_yawl.hrl").

-record(state, {
    required :: pos_integer(),
    total :: pos_integer(),
    completed = 0 :: non_neg_integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link(InitialData) ->
    Required = maps:get(required, InitialData, 2),
    Total = maps:get(total, InitialData, 3),
    gen_pnet:start_link(?MODULE, #{
        required => Required,
        total => Total,
        case_data => InitialData
    }, []).

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
        p_split,
        p_branch_1,
        p_branch_2,
        p_branch_3,
        p_continue,
        p_output
    ].

trsn_lst() ->
    [t_split, t_branch_1, t_branch_2, t_branch_3, t_continue].

init_marking(p_input, UsrInfo) ->
    CaseData = maps:get(case_data, UsrInfo, #{}),
    [#case_token{
        case_id = generate_case_id(),
        spec_id = <<"n-out-of-m">>,
        started_at = os:system_time(nanosecond),
        case_data = CaseData
    }];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_split) -> [p_input];
preset(t_branch_1) -> [p_split];
preset(t_branch_2) -> [p_split];
preset(t_branch_3) -> [p_split];
preset(t_continue) -> [p_split].

is_enabled(t_continue, _Mode, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

%% Fire transitions
fire(t_split, _Mode, UsrInfo) ->
    Required = maps:get(required, UsrInfo, 2),
    Total = maps:get(total, UsrInfo, 3),
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_split,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{required => Required, total => Total}
    },
    %% Create M branch tokens
    Branches = case Total of
        1 -> [p_branch_1];
        2 -> [p_branch_1, p_branch_2];
        3 -> [p_branch_1, p_branch_2, p_branch_3]
    end,
    ProduceList = [{p_split, [Token]} | [{B, [Token]} || B <- Branches]],
    {produce, maps:from_list(ProduceList)};

fire(t_branch_1, _Mode, _UsrInfo) ->
    {produce, #{p_branch_1 => [#work_item_token{
        work_item_id = generate_id(),
        task_id = t_branch_1,
        status = completed,
        completed_at = os:system_time(nanosecond)
    }]}};

fire(t_branch_2, _Mode, _UsrInfo) ->
    {produce, #{p_branch_2 => [#work_item_token{
        work_item_id = generate_id(),
        task_id = t_branch_2,
        status = completed,
        completed_at = os:system_time(nanosecond)
    }]}};

fire(t_branch_3, _Mode, _UsrInfo) ->
    {produce, #{p_branch_3 => [#work_item_token{
        work_item_id = generate_id(),
        task_id = t_branch_3,
        status = completed,
        completed_at = os:system_time(nanosecond)
    }]}};

fire(t_continue, _Mode, _UsrInfo) ->
    {produce, #{
        p_continue => [#work_item_token{
            work_item_id = generate_id(),
            task_id = t_continue,
            status = completed,
            completed_at = os:system_time(nanosecond)
        }],
        p_output => [#case_token{
            case_id = generate_case_id(),
            spec_id = <<"n-out-of-m">>,
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

    case can_fire(Transition, Preset, Marking) of
        true ->
            case Transition of
                t_continue ->
                    %% Check if N branches completed
                    Completed = count_completed_branches(Marking),
                    case Completed >= State#state.required of
                        true ->
                            {produce, ProduceMap} = fire(Transition, Marking, NetState),
                            UpdatedMarking = apply_produce(ProduceMap, Marking),
                            {reply, ok, set_state(NetState, UpdatedMarking, State)};
                        false ->
                            {reply, {error, threshold_not_met}, NetState}
                    end;
                _ ->
                    {produce, ProduceMap} = fire(Transition, Marking, NetState),
                    UpdatedMarking = apply_produce(ProduceMap, Marking),
                    UpdatedState = increment_completed(State, Transition),
                    {reply, ok, set_state(NetState, UpdatedMarking, UpdatedState)}
            end;
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
    Required = maps:get(required, NetArg, 2),
    Total = maps:get(total, NetArg, 3),
    #state{required = Required, total = Total}.

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
        _ -> #state{required = 2, total = 3}
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

count_completed_branches(Marking) ->
    lists:foldl(fun(Place, Count) ->
        case maps:get(Place, Marking, []) of
            [_ | _] -> Count + 1;
            [] -> Count
        end
    end, 0, [p_branch_1, p_branch_2, p_branch_3]).

increment_completed(State, t_branch_1) ->
    State#state{completed = State#state.completed + 1};
increment_completed(State, t_branch_2) ->
    State#state{completed = State#state.completed + 1};
increment_completed(State, t_branch_3) ->
    State#state{completed = State#state.completed + 1};
increment_completed(State, _) ->
    State.

generate_case_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)}))).

generate_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({make_ref(), os:system_time(nanosecond)}))).
