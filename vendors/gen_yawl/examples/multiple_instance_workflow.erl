%%====================================================================
%% multiple_instance_workflow - Multiple Instances Pattern (WP12)
%%====================================================================
%% @doc WP12: Multiple instances (parallel) pattern. Creates N
%% parallel instances of a task that can execute concurrently.

-module(multiple_instance_workflow).
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
    instance_count = 0 :: non_neg_integer(),
    target_count = 1 :: pos_integer()
}).

%%====================================================================
%% API
%%====================================================================

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
        p_instance,
        p_join,
        p_output
    ].

trsn_lst() ->
    [t_split, t_instance, t_join].

init_marking(p_input, UsrInfo) ->
    Count = maps:get(count, UsrInfo, 3),
    [#case_token{
        case_id = generate_case_id(),
        spec_id = <<"multiple-instance">>,
        started_at = os:system_time(nanosecond),
        case_data = #{count => Count}
    }];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_split) -> [p_input];
preset(t_instance) -> [p_split];
preset(t_join) -> [p_instance].

is_enabled(t_join, _Mode, _State) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

%% Fire transitions
fire(t_split, Mode, _UsrInfo) ->
    Count = get_instance_count(Mode),
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_split,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{instance_count => Count}
    },
    {produce, #{
        p_split => [Token],
        p_instance => lists:duplicate(Count, Token)  %% Multiple tokens
    }};

fire(t_instance, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_instance,
        status = completed,
        completed_at = os:system_time(nanosecond)
    },
    {produce, #{p_instance => [Token]}};

fire(t_join, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_join,
        status = completed,
        completed_at = os:system_time(nanosecond)
    },
    {produce, #{
        p_join => [Token],
        p_output => [#case_token{
            case_id = generate_case_id(),
            spec_id = <<"multiple-instance">>,
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
            {produce, ProduceMap} = fire(Transition, Marking, NetState),
            UpdatedMarking = apply_produce(ProduceMap, Marking),
            UpdatedState = update_state(State, Transition, Marking),
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
    Count = maps:get(count, NetArg, 3),
    #state{target_count = Count}.

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
    andalso Transition =/= t_join
    orelse (Transition =:= t_join andalso lists:any(fun(P) -> maps:get(P, Marking, []) =/= [] end, Preset)).

apply_produce(ProduceMap, Marking) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        Current = maps:get(Place, Acc, []),
        Acc#{Place => Current ++ Tokens}
    end, Marking, ProduceMap).

get_instance_count(Mode) ->
    case maps:get(p_input, Mode, []) of
        [#case_token{case_data = Data} | _] ->
            maps:get(count, Data, 3);
        _ ->
            3
    end.

update_state(State, t_instance, Marking) ->
    %% Count instances
    InstanceCount = length(maps:get(p_instance, Marking, [])),
    State#state{instance_count = InstanceCount};
update_state(State, _, _) ->
    State.

generate_case_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)}))).

generate_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({make_ref(), os:system_time(nanosecond)}))).
