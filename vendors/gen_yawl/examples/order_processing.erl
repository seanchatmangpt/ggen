%%====================================================================
%% order_processing - Complex Order Processing Workflow
%%====================================================================
%% @doc Complex workflow demonstrating multiple patterns:
%% - WP1: Sequence (validate -> process -> ship)
%% - WP4+WP5: XOR-split (inventory check: in stock vs backorder)
%% - WP10: Arbitrary cycle (loop back to validate if items missing)
%% - WP11: Implicit termination

-module(order_processing).
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
        p_validate,
        p_check_items,
        p_reserve_stock,
        p_ship,
        p_notify_customer,
        p_output
    ].

trsn_lst() ->
    [
        t_validate,
        t_check_items,
        t_reserve_stock,
        t_ship,
        t_notify_customer
    ].

init_marking(p_input, UsrInfo) ->
    CaseData = maps:get(case_data, UsrInfo, #{}),
    [#case_token{
        case_id = generate_case_id(),
        spec_id = <<"order-processing">>,
        started_at = os:system_time(nanosecond),
        case_data = CaseData
    }];
init_marking(_Place, _UsrInfo) ->
    [].

%% Presets define workflow structure
preset(t_validate) -> [p_input];
preset(t_check_items) -> [p_validate];
preset(t_reserve_stock) -> [p_check_items];
preset(t_ship) -> [p_reserve_stock];
preset(t_notify_customer) -> [p_ship].

%% Enablement with cycle support (WP10)
is_enabled(t_reserve_stock, Mode, _UsrInfo) ->
    %% Check if items are in stock
    Items = get_items(Mode),
    case Items of
        [] -> false;  %% Cycle: empty items, go back to validate
        _ -> true
    end;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

%% Fire transitions
fire(t_validate, Mode, _UsrInfo) ->
    Items = get_items(Mode),
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_validate,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{items => Items}
    },
    {produce, #{p_validate => [Token]}};

fire(t_check_items, Mode, UsrInfo) ->
    Items = get_items(Mode),
    MaxCycles = maps:get(max_cycles, UsrInfo, 10),
    CycleCount = get_cycle_count(Mode, 0),

    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_check_items,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{
            items => Items,
            in_stock => length(Items) > 0,
            cycle_count => CycleCount,
            max_cycles => MaxCycles
        }
    },

    %% WP10: Cycle back if items empty and under max cycles
    case Items of
        [] when CycleCount < MaxCycles ->
            %% Add items and cycle back
            {produce, #{
                p_check_items => [Token#work_item_token{
                    data => Token#work_item_token.data#{needs_cycle => true}
                }]
            }};
        _ ->
            {produce, #{p_check_items => [Token]}}
    end;

fire(t_reserve_stock, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_reserve_stock,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{stock_reserved => true}
    },
    {produce, #{p_reserve_stock => [Token]}};

fire(t_ship, _Mode, _UsrInfo) ->
    Token = #work_item_token{
        work_item_id = generate_id(),
        task_id = t_ship,
        status = completed,
        completed_at = os:system_time(nanosecond),
        data => #{shipped => true}
    },
    {produce, #{p_ship => [Token]}};

fire(t_notify_customer, _Mode, _UsrInfo) ->
    {produce, #{
        p_notify_customer => [#work_item_token{
            work_item_id = generate_id(),
            task_id = t_notify_customer,
            status = completed,
            completed_at = os:system_time(nanosecond)
        }],
        p_output => [#case_token{
            case_id = generate_case_id(),
            spec_id = <<"order-processing">>,
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
            %% Check for cycle (WP10)
            case needs_cycle_back(ProduceMap) of
                true ->
                    %% Add token back to validate
                    MarkingWithCycle = UpdatedMarking#{
                        p_validate => (maps:get(p_validate, UpdatedMarking, [])) ++
                            [#work_item_token{
                                work_item_id = generate_id(),
                                task_id = cycle_back,
                                status = enabled,
                                enabled_at = os:system_time(nanosecond)
                            }]
                    },
                    {reply, ok, NetState#{marking => MarkingWithCycle}};
                false ->
                    {reply, ok, NetState#{marking => UpdatedMarking}}
            end;
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
%% Internal Functions
%%====================================================================

get_items(Mode) ->
    case maps:get(p_input, Mode, []) of
        [#case_token{case_data = Data} | _] ->
            maps:get(items, Data, []);
        _ ->
            []
    end.

get_cycle_count(Mode, Default) ->
    case maps:get(p_check_items, Mode, []) of
        [#work_item_token{data = Data} | _] ->
            maps:get(cycle_count, Data, Default);
        _ ->
            Default
    end.

can_fire(Transition, Preset, Marking, NetState) ->
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

needs_cycle_back(ProduceMap) ->
    case maps:get(p_check_items, ProduceMap, []) of
        [#work_item_token{data = Data} | _] ->
            maps:get(needs_cycle, Data, false);
        _ ->
            false
    end.

generate_case_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)}))).

generate_id() ->
    binary:encode_hex(crypto:hash(md5, term_to_binary({make_ref(), os:system_time(nanosecond)}))).
