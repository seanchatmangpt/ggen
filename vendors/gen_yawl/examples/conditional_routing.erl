%%====================================================================
%% conditional_routing - Conditional Routing Workflow (XOR-split)
%%====================================================================
%% @doc WP4+WP5: Exclusive Choice and Simple Merge pattern.
%% Routes transactions based on amount to different approval paths:
%% - < 1000: auto_approve
%% - 1000-9999: manual_review
%% - >= 10000: reject

-module(conditional_routing).
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
%% API Functions
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
%% gen_pnet Structure Callbacks
%%====================================================================

place_lst() ->
    [
        p_input,
        p_check_amount,
        p_auto_approve,
        p_manual_review,
        p_reject,
        p_done,
        p_output
    ].

trsn_lst() ->
    [
        t_check_amount,
        t_auto_approve,
        t_manual_review,
        t_reject,
        t_done
    ].

init_marking(p_input, UsrInfo) ->
    CaseData = maps:get(case_data, UsrInfo, #{}),
    [#case_token{
        case_id = generate_case_id(),
        spec_id = <<"conditional-routing">>,
        started_at = os:system_time(nanosecond),
        case_data = CaseData
    }];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_check_amount) -> [p_input];
preset(t_auto_approve) -> [p_check_amount];
preset(t_manual_review) -> [p_check_amount];
preset(t_reject) -> [p_check_amount];
preset(t_done) -> [p_auto_approve, p_manual_review, p_reject].

%% Enablement checks conditions for routing
is_enabled(t_auto_approve, Mode, UsrInfo) ->
    %% Amount < 1000
    Amount = get_amount(Mode, UsrInfo),
    Amount < 1000;
is_enabled(t_manual_review, Mode, UsrInfo) ->
    %% 1000 <= Amount < 10000
    Amount = get_amount(Mode, UsrInfo),
    Amount >= 1000 andalso Amount < 10000;
is_enabled(t_reject, Mode, UsrInfo) ->
    %% Amount >= 10000
    Amount = get_amount(Mode, UsrInfo),
    Amount >= 10000;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

%% Fire transitions based on conditions
fire(t_check_amount, Mode, _UsrInfo) ->
    Amount = get_amount(Mode, #{}),
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_check_amount,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data = #{amount => Amount}
    },
    {produce, #{p_check_amount => [Token]}};

fire(t_auto_approve, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_auto_approve,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data = #{decision => auto_approved}
    },
    {produce, #{p_auto_approve => [Token]}};

fire(t_manual_review, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_manual_review,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{decision => manual_review_required}
    },
    {produce, #{p_manual_review => [Token]}};

fire(t_reject, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_reject,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{decision => rejected}
    },
    {produce, #{p_reject => [Token]}};

fire(t_done, _Mode, _UsrInfo) ->
    {produce, #{
        p_done => [#work_item_token{
            work_item_id = generate_id(),
            task_id = t_done,
            status = completed,
            completed_at = os:system_time(nanosecond)
        }],
        p_output => [#case_token{
            case_id = generate_case_id(),
            spec_id = <<"conditional-routing">>,
            started_at = os:system_time(nanosecond)
        }]
    }}.

%%====================================================================
%% gen_pnet Interface Callbacks
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
    NetArg.

terminate(_Reason, _NetState) ->
    ok.

trigger(_Place, _Token, _NetState) ->
    pass.

%%====================================================================
%% Internal Functions
%%====================================================================

get_amount(Mode, UsrInfo) ->
    case maps:get(p_input, Mode, []) of
        [#case_token{case_data = Data} | _] ->
            maps:get(amount, Data, 0);
        [] ->
            maps:get(amount, UsrInfo, 0);
        _ ->
            0
    end.

can_fire(Transition, Preset, Marking, NetState) ->
    %% Check preset has tokens
    HasPreset = lists:any(fun(P) -> maps:get(P, Marking, []) =/= [] end, Preset),
    if
        HasPreset =:= false -> false;
        true -> is_enabled(Transition, Marking, NetState)
    end.

apply_produce(ProduceMap, Marking) ->
    maps:fold(
        fun(Place, Tokens, Acc) ->
            Current = maps:get(Place, Acc, []),
            Acc#{Place => Current ++ Tokens}
        end,
        Marking,
        ProduceMap
    ).

generate_case_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)}))).

generate_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({make_ref(), os:system_time(nanosecond)}))).
