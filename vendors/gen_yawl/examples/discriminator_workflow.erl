%%====================================================================
%% discriminator_workflow - Discriminator Pattern Workflow (WP9)
%%====================================================================
%% @doc WP9: Discriminator pattern. First arriving branch triggers
%% continuation, subsequent branch tokens are withdrawn.

-module(discriminator_workflow).
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
    discriminator_triggered = false :: boolean(),
    completed_branches = [] :: [atom()]
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
        p_split,
        p_branch_1,
        p_branch_2,
        p_branch_3,
        p_continue,
        p_output
    ].

trsn_lst() ->
    [t_split, t_branch_1, t_branch_2, t_branch_3, t_continue].

init_marking(p_input, _UsrInfo) ->
    [#case_token{
        case_id = generate_case_id(),
        spec_id = <<"discriminator">>,
        started_at = os:system_time(nanosecond)
    }];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_split) -> [p_input];
preset(t_branch_1) -> [p_split];
preset(t_branch_2) -> [p_split];
preset(t_branch_3) -> [p_split];
preset(t_continue) -> [p_branch_1, p_branch_2, p_branch_3].

is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

%% Fire transitions
fire(t_split, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_split,
        status = completed,
        completed_at = os:system_time(nanosecond)
    },
    {produce, #{
        p_split => [Token],
        p_branch_1 => [Token],
        p_branch_2 => [Token],
        p_branch_3 => [Token]
    }};

fire(t_branch_1, _Mode, _UsrInfo) ->
    {produce, #{p_branch_1 => [#work_item_token{
        work_item_id = generate_id(),
        task_id = t_branch_1,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{branch => branch_1}
    }]}};

fire(t_branch_2, _Mode, _UsrInfo) ->
    {produce, #{p_branch_2 => [#work_item_token{
        work_item_id = generate_id(),
        task_id = t_branch_2,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{branch => branch_2}
    }]}};

fire(t_branch_3, _Mode, _UsrInfo) ->
    {produce, #{p_branch_3 => [#work_item_token{
        work_item_id = generate_id(),
        task_id = t_branch_3,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{branch => branch_3}
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
            spec_id = <<"discriminator">>,
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

    case can_fire(Transition, Preset, Marking) of
        true ->
            case Transition of
                t_branch_1 when State#state.discriminator_triggered =:= false ->
                    %% First branch triggers discriminator
                    {produce, ProduceMap} = fire(Transition, Marking, NetState),
                    UpdatedMarking = apply_produce(ProduceMap, Marking),
                    UpdatedState = State#state{
                        discriminator_triggered = true,
                        completed_branches = [branch_1]
                    },
                    {reply, ok, set_state(NetState, UpdatedMarking, UpdatedState)};
                t_branch_2 when State#state.discriminator_triggered =:= false ->
                    {produce, ProduceMap} = fire(Transition, Marking, NetState),
                    UpdatedMarking = apply_produce(ProduceMap, Marking),
                    UpdatedState = State#state{
                        discriminator_triggered = true,
                        completed_branches = [branch_2]
                    },
                    {reply, ok, set_state(NetState, UpdatedMarking, UpdatedState)};
                t_branch_3 when State#state.discriminator_triggered =:= false ->
                    {produce, ProduceMap} = fire(Transition, Marking, NetState),
                    UpdatedMarking = apply_produce(ProduceMap, Marking),
                    UpdatedState = State#state{
                        discriminator_triggered = true,
                        completed_branches = [branch_3]
                    },
                    {reply, ok, set_state(NetState, UpdatedMarking, UpdatedState)};
                t_branch_N when State#state.discriminator_triggered =:= true ->
                    %% Subsequent branches - tokens are withdrawn (no production)
                    UpdatedState = State#state{
                        completed_branches = [Transition | State#state.completed_branches]
                    },
                    {reply, ok, set_state(NetState, Marking, UpdatedState)};
                _ ->
                    {produce, ProduceMap} = fire(Transition, Marking, NetState),
                    UpdatedMarking = apply_produce(ProduceMap, Marking),
                    {reply, ok, set_state(NetState, UpdatedMarking, State)}
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
    lists:any(fun(P) -> maps:get(P, Marking, []) =/= [] end, Preset)
    andalso not (Transition =:= t_branch_N).

apply_produce(ProduceMap, Marking) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        Current = maps:get(Place, Acc, []),
        Acc#{Place => Current ++ Tokens}
    end, Marking, ProduceMap).

generate_case_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)}))).

generate_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({make_ref(), os:system_time(nanosecond)}))).
