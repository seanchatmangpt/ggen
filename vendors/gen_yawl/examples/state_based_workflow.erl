%%====================================================================
%% state_based_workflow - State-Based Choice Workflow (WP15)
%%====================================================================
%% @doc WP15: State-based choice pattern. Routing decision depends
%% on data state (risk level) captured in case data.

-module(state_based_workflow).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, trigger/3]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

%% API
-export([start_link/1, fire/2, marking/1, case_status/1]).

-include("include/gen_yawl.hrl").

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
        p_evaluate,
        p_standard,
        p_expedited,
        p_output
    ].

trsn_lst() ->
    [t_evaluate, t_standard, t_expedited].

init_marking(p_input, UsrInfo) ->
    CaseData = maps:get(case_data, UsrInfo, #{}),
    [#case_token{
        case_id = generate_case_id(),
        spec_id = <<"state-based">>,
        started_at = os:system_time(nanosecond),
        case_data = CaseData
    }];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_evaluate) -> [p_input];
preset(t_standard) -> [p_evaluate];
preset(t_expedited) -> [p_evaluate].

%% Enablement based on risk level in case data
is_enabled(t_standard, _Mode, UsrInfo) ->
    Risk = get_risk_level(UsrInfo),
    Risk =:= low orelse Risk =:= medium;
is_enabled(t_expedited, _Mode, UsrInfo) ->
    Risk = get_risk_level(UsrInfo),
    Risk =:= high orelse Risk =:= critical;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

%% Fire transitions
fire(t_evaluate, Mode, _UsrInfo) ->
    Risk = get_risk_from_mode(Mode),
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_evaluate,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{risk => Risk}
    },
    {produce, #{p_evaluate => [Token]}};

fire(t_standard, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_standard,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{processing => standard}
    },
    {produce, #{p_standard => [Token]}};

fire(t_expedited, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_expedited,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{processing => expedited}
    },
    {produce, #{p_expedited => [Token]}};

%% Auto-fire output after standard/expedited
fire(Transition, _Mode, _UsrInfo) when Transition =:= t_standard; Transition =:= t_expedited ->
    %% This is handled in the callback for auto-proceeding to output
    {produce, #{
        p_output => [#case_token{
            case_id = generate_case_id(),
            spec_id = <<"state-based">>,
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
    case can_fire(Transition, Preset, Marking, NetState) of
        true ->
            {produce, ProduceMap} = fire(Transition, Marking, NetState),
            UpdatedMarking = apply_produce(ProduceMap, Marking),
            {reply, ok, NetState#{marking => UpdatedMarking}};
        false ->
            {reply, {error, not_enabled}, NetState)
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
    NetArg.

terminate(_Reason, _NetState) ->
    ok.

trigger(_Place, _Token, _NetState) ->
    pass.

%%====================================================================
%% Internal
%%====================================================================

get_risk_level(UsrInfo) ->
    case maps:get(p_input, UsrInfo, []) of
        [#case_token{case_data = Data} | _] ->
            maps:get(risk, Data, low);
        _ ->
            maps:get(risk, UsrInfo, low)
    end.

get_risk_from_mode(Mode) ->
    case maps:get(p_input, Mode, []) of
        [#case_token{case_data = Data} | _] ->
            maps:get(risk, Data, low);
        _ ->
            low
    end.

can_fire(Transition, Preset, Marking, NetState) ->
    HasPreset = lists:any(fun(P) -> maps:get(P, Marking, []) =/= [] end, Preset),
    if
        HasPreset =:= false -> false;
        true -> is_enabled(Transition, Marking, NetState)
    end.

apply_produce(ProduceMap, Marking) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        Current = maps:get(Place, Acc, []),
        Acc#{Place => Current ++ Tokens}
    end, Marking, ProduceMap).

generate_case_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)}))).

generate_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({make_ref(), os:system_time(nanosecond)}))).
