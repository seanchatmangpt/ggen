%%====================================================================
%% deferred_choice_workflow - Deferred Choice Workflow (WP16)
%%====================================================================
%% @doc WP16: Deferred Choice pattern. Multiple candidate work items
%% are created, but only the first to respond via external event proceeds.
%% Others are cancelled.

-module(deferred_choice_workflow).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, trigger/3]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

%% API
-export([start_link/0, start_link/1, fire/2, marking/1, case_status/1,
         external_event/2]).

-include("include/gen_yawl.hrl").

-record(state, {
    candidates = [] :: [atom()],
    selected = undefined :: atom() | undefined
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

external_event(Pid, Event) ->
    gen_pnet:call(Pid, {external_event, Event}).

%%====================================================================
%% Structure Callbacks
%%====================================================================

place_lst() ->
    [p_input, p_enable, p_option_a, p_option_b, p_merge, p_output].

trsn_lst() ->
    [t_enable, t_option_a, t_option_b, t_merge].

init_marking(p_input, _UsrInfo) ->
    [#case_token{
        case_id = generate_case_id(),
        spec_id = <<"deferred-choice">>,
        started_at = os:system_time(nanosecond)
    }];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_enable) -> [p_input];
preset(t_option_a) -> [p_enable];
preset(t_option_b) -> [p_enable];
preset(t_merge) -> [p_option_a, p_option_b].

is_enabled(t_option_a, _Mode, _UsrInfo) -> true;
is_enabled(t_option_b, _Mode, _UsrInfo) -> true;
is_enabled(_Trsn, _Mode, _UsrInfo) -> true.

%% Fire transitions
fire(t_enable, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_enable,
        status = completed,
        completed_at = os:system_time(nanosecond)
    },
    {produce, #{p_enable => [Token]}};

fire(t_option_a, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_option_a,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{selected_option => option_a}
    },
    {produce, #{p_option_a => [Token]}};

fire(t_option_b, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_option_b,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{selected_option => option_b}
    },
    {produce, #{p_option_b => [Token]}};

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
            spec_id = <<"deferred-choice">>,
            started_at = os:system_time(nanosecond)
        }]
    }}.

%%====================================================================
%% Interface Callbacks
%%====================================================================

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

handle_call({fire, Transition}, _From, NetState) ->
    Preset = preset(Transition),
    Marking = gen_pnet:get_usr_info(NetState),
    State = get_state(NetState),
    case can_fire(Transition, Preset, Marking) of
        true ->
            {produce, ProduceMap} = fire(Transition, Marking, NetState),
            UpdatedMarking = apply_produce(ProduceMap, Marking),
            {reply, ok, set_state(NetState, UpdatedMarking, State)};
        false ->
            {reply, {error, not_enabled}, NetState}
    end;

handle_call({external_event, {select_option, Option}}, _From, NetState) ->
    State = get_state(NetState),
    Marking = gen_pnet:get_usr_info(NetState),

    case State#state.selected of
        undefined ->
            %% First selection - proceed with chosen option
            Transition = list_to_existing_atom("t_" ++ atom_to_list(Option)),
            case can_fire(Transition, [p_enable], Marking) of
                true ->
                    {produce, ProduceMap} = fire(Transition, Marking, NetState),
                    UpdatedMarking = apply_produce(ProduceMap, Marking),
                    UpdatedState = State#state{selected = Option},
                    {reply, ok, set_state(NetState, UpdatedMarking, UpdatedState)};
                false ->
                    {reply, {error, not_enabled}, NetState}
            end;
        _AlreadySelected ->
            %% Already selected - ignore
            {reply, {error, already_selected}, NetState}
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
    %% Store both marking and our state
    NetState#{marking => Marking, state => State}.

can_fire(Transition, Preset, Marking) ->
    lists:any(fun(P) -> maps:get(P, Marking, []) =/= [] end, Preset).

apply_produce(ProduceMap, Marking) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        Current = maps:get(Place, Acc, []),
        Acc#{Place => Current ++ Tokens}
    end, Marking, ProduceMap).

generate_case_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)}))).

generate_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({make_ref(), os:system_time(nanosecond)}))).
