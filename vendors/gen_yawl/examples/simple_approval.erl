%%====================================================================
%% simple_approval - Basic Sequential Approval Workflow
%%====================================================================
%% @doc WP1: Sequence pattern example. A document flows through
%% three sequential stages: draft -> review -> approve.

-module(simple_approval).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, trigger/3]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

%% API
-export([start_link/0, start_link/1, fire/2, marking/1, case_status/1]).

-include("include/gen_yawl.hrl").

%%====================================================================
%% API Functions
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
%% gen_pnet Structure Callbacks
%%====================================================================

place_lst() ->
    [p_input, p_draft, p_review, p_approve, p_output].

trsn_lst() ->
    [t_draft, t_review, t_approve].

init_marking(p_input, _UsrInfo) ->
    [#case_token{
        case_id = generate_case_id(),
        spec_id = <<"simple-approval">>,
        started_at = os:system_time(nanosecond),
        case_data = #{}
    }];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_draft) -> [p_input];
preset(t_review) -> [p_draft];
preset(t_approve) -> [p_review].

is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

fire(t_draft, _Mode, UsrInfo) ->
    Receipt = create_receipt(UsrInfo, t_draft),
    {produce, #{
        p_draft => [#work_item_token{
            work_item_id = generate_id(),
            task_id = t_draft,
            status = enabled,
            enabled_at = os:system_time(nanosecond)
        }],
        receipts => [Receipt]
    }};
fire(t_review, _Mode, UsrInfo) ->
    Receipt = create_receipt(UsrInfo, t_review),
    {produce, #{
        p_review => [#work_item_token{
            work_item_id = generate_id(),
            task_id = t_review,
            status = enabled,
            enabled_at = os:system_time(nanosecond)
        }],
        receipts => [Receipt]
    }};
fire(t_approve, _Mode, UsrInfo) ->
    Receipt = create_receipt(UsrInfo, t_approve),
    {produce, #{
        p_approve => [#work_item_token{
            work_item_id = generate_id(),
            task_id = t_approve,
            status = enabled,
            enabled_at = os:system_time(nanosecond)
        }],
        receipts => [Receipt]
    }}.

%%====================================================================
%% gen_pnet Interface Callbacks
%%====================================================================

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

handle_call({fire, Transition}, _From, NetState) ->
    %% Fire the transition by consuming from preset and producing to postset
    Preset = preset(Transition),
    Marking = gen_pnet:get_usr_info(NetState),
    case can_fire(Transition, Preset, Marking) of
        true ->
            {produce, ProduceMap} = fire(Transition, Marking, NetState),
            UpdatedState = handle_trigger(ProduceMap, NetState),
            gen_pnet:continue(self()),
            {reply, ok, UpdatedState};
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

can_fire(_Transition, Preset, Marking) ->
    lists:all(fun(P) -> maps:get(P, Marking, []) =/= [] end, Preset).

handle_trigger(ProduceMap, NetState) ->
    %% Apply production map to marking
    CurrentMarking = gen_pnet:get_usr_info(NetState),
    UpdatedMarking = maps:fold(
        fun(Place, Tokens, Acc) ->
            Current = maps:get(Place, Acc, []),
            Acc#{Place => Current ++ Tokens}
        end,
        CurrentMarking,
        ProduceMap
    ),
    NetState#{marking => UpdatedMarking}.

create_receipt(NetState, EventType) ->
    CaseId = extract_case_id(NetState),
    #receipt{
        id = generate_id(),
        prev_hash = <<>>,
        current_hash = crypto:hash(blake3, term_to_binary({EventType, os:system_time(nanosecond)})),
        timestamp = os:system_time(nanosecond),
        event_type = EventType,
        case_id = CaseId,
        justification = #{}
    }.

extract_case_id(NetState) ->
    Marking = gen_pnet:get_usr_info(NetState),
    case maps:get(p_input, Marking, []) of
        [#case_token{case_id = CaseId} | _] -> CaseId;
        _ -> generate_case_id()
    end.

generate_case_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)}))).

generate_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({make_ref(), os:system_time(nanosecond)}))).
