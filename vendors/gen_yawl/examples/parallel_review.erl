%%====================================================================
%% parallel_review - Parallel Review Workflow
%%====================================================================
%% @doc WP2+WP3: Parallel Split and Synchronization pattern.
%% A document is submitted for parallel review by three departments:
%% legal, tech, and finance. All three must complete before finalization.

-module(parallel_review).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, trigger/3]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

%% API
-export([start_link/0, fire/2, marking/1, case_status/1]).

-include("include/gen_yawl.hrl").

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_pnet:start_link(?MODULE, #{}, []).

fire(Pid, Transition) ->
    gen_pnet:call(Pid, {fire, Transition}).

marking(Pid) ->
    gen_pnet:marking(Pid).

case_status(Pid) ->
    gen_pnet:call(Pid, case_status).

%%====================================================================
%% gen_pnet Structure Callbacks
%%====================================================================

%% Places represent workflow states
place_lst() ->
    [
        p_input,
        p_submit,
        p_legal_review,
        p_tech_review,
        p_finance_review,
        p_finalize,
        p_output
    ].

%% Transitions represent tasks
trsn_lst() ->
    [
        t_submit,
        t_legal_review,
        t_tech_review,
        t_finance_review,
        t_finalize
    ].

%% Initial marking: token in input
init_marking(p_input, _UsrInfo) ->
    [#case_token{
        case_id = generate_case_id(),
        spec_id = <<"parallel-review">>,
        started_at = os:system_time(nanosecond)
    }];
init_marking(_Place, _UsrInfo) ->
    [].

%% Presets: which places feed into each transition
preset(t_submit) ->
    [p_input];
preset(t_legal_review) ->
    [p_legal_review];
preset(t_tech_review) ->
    [p_tech_review];
preset(t_finance_review) ->
    [p_finance_review];
preset(t_finalize) ->
    %% AND-join: wait for all three reviews
    [p_legal_review, p_tech_review, p_finance_review].

%% All transitions are enabled when preset has tokens
is_enabled(t_finalize, Mode, _UsrInfo) ->
    %% Special check: AND-join requires ALL presets to have tokens
    Preset = preset(t_finalize),
    lists:all(fun(P) -> maps:get(P, Mode, []) =/= [] end, Preset);
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

%% Fire transitions
fire(t_submit, _Mode, UsrInfo) ->
    %% WP2: AND-split - produce tokens on ALL review branches
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_submit,
        status = completed,
        completed_at = os:system_time(nanosecond)
    },
    {produce, #{
        p_submit => [Token],
        p_legal_review => [Token],
        p_tech_review => [Token],
        p_finance_review => [Token]
    }};

fire(t_legal_review, _Mode, _UsrInfo) ->
    {produce, #{
        p_legal_review => [#work_item_token{
            work_item_id = generate_id(),
            task_id = t_legal_review,
            status = completed,
            completed_at = os:system_time(nanosecond)
        }]
    }};

fire(t_tech_review, _Mode, _UsrInfo) ->
    {produce, #{
        p_tech_review => [#work_item_token{
            work_item_id = generate_id(),
            task_id = t_tech_review,
            status = completed,
            completed_at = os:system_time(nanosecond)
        }]
    }};

fire(t_finance_review, _Mode, _UsrInfo) ->
    {produce, #{
        p_finance_review => [#work_item_token{
            work_item_id = generate_id(),
            task_id = t_finance_review,
            status = completed,
            completed_at = os:system_time(nanosecond)
        }]
    }};

fire(t_finalize, _Mode, _UsrInfo) ->
    %% WP3: AND-join synchronization complete
    {produce, #{
        p_finalize => [#work_item_token{
            work_item_id = generate_id(),
            task_id = t_finalize,
            status = completed,
            completed_at = os:system_time(nanosecond)
        }],
        p_output => [#case_token{
            case_id = generate_case_id(),
            spec_id = <<"parallel-review">>,
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
    case can_fire(Transition, Preset, Marking) of
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

can_fire(t_finalize, Preset, Marking) ->
    %% AND-join: need ALL presets
    lists:all(fun(P) -> maps:get(P, Marking, []) =/= [] end, Preset);
can_fire(_Transition, Preset, Marking) ->
    %% Single preset needed
    lists:any(fun(P) -> maps:get(P, Marking, []) =/= [] end, Preset).

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
