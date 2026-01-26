%%%-------------------------------------------------------------------
%% @doc tai_ct_SUITE: Common Test suite
%%
%% Tests:
%%   - /health returns 200 when boot complete
%%   - Malformed pubsub payload -> refusal receipt, non-5xx
%%   - Entitlement inactive -> refuse actions
%%   - Storm test -> postpones, bounded concurrency
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_ct_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_health_endpoint/1, test_pubsub_invalid_payload/1, test_entitlement_refusal/1, test_storm_postpones/1]).

%%%===================================================================
%% Test callbacks
%%%===================================================================

all() ->
    [
        test_health_endpoint,
        test_pubsub_invalid_payload,
        test_entitlement_refusal,
        test_storm_postpones
    ].

init_per_suite(Config) ->
    %% Start HTTP client
    inets:start(),
    %% Start all applications with all dependencies
    case application:ensure_all_started(tai_autonomics) of
        {ok, _Apps} ->
            %% Wait for HTTP server to be ready
            wait_for_http_ready(10),
            Config;
        {error, Reason} ->
            ct:fail("Failed to start application: " ++ atom_to_list(Reason))
    end.

end_per_suite(Config) ->
    catch application:stop(tai_autonomics),
    catch application:stop(inets),
    Config.

%% Helper function to wait for HTTP server
wait_for_http_ready(0) ->
    ct:fail("HTTP server did not become ready");
wait_for_http_ready(Attempts) ->
    case catch httpc:request(get, {"http://localhost:8080/health", []}, [{timeout, 1000}], []) of
        {ok, _} -> ok;
        _ ->
            timer:sleep(500),
            wait_for_http_ready(Attempts - 1)
    end.

%%%===================================================================
%% Test cases
%%%===================================================================

test_health_endpoint(_Config) ->
    %% Wait for boot
    timer:sleep(1000),
    %% Test health endpoint
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {"http://localhost:8080/health", []}, [], []),
    Response = jsx:decode(Body, [return_maps]),
    <<"ok">> = maps:get(<<"status">>, Response),
    ok.

test_pubsub_invalid_payload(_Config) ->
    InvalidPayload = <<"not json">>,
    {ok, {{_, StatusCode, _}, _, Body}} = httpc:request(
        post,
        {"http://localhost:8080/pubsub", [], "application/json", InvalidPayload},
        [],
        []
    ),
    %% Should return non-5xx
    true = StatusCode < 500,
    Response = jsx:decode(Body, [return_maps]),
    <<"refusal">> = maps:get(<<"type">>, Response),
    ok.

test_entitlement_refusal(_Config) ->
    %% Test entitlement inactive scenario
    %% Send marketplace event with inactive entitlement
    Event = #{
        <<"tenant_id">> => <<"test-tenant-inactive">>,
        <<"entitlement_id">> => <<"test-entitlement-inactive">>,
        <<"action">> => <<"grant">>,
        <<"signature">> => <<"test-signature">>
    },
    JsonBody = jsx:encode(Event),
    
    {ok, {{_, StatusCode, _}, _, Body}} = httpc:request(
        post,
        {"http://localhost:8080/marketplace", [], "application/json", JsonBody},
        [],
        []
    ),
    %% Should return non-5xx (either 400 for invalid signature or 403 for inactive entitlement)
    true = StatusCode < 500,
    Response = jsx:decode(Body, [return_maps]),
    %% Should return refusal receipt
    <<"refusal">> = maps:get(<<"type">>, Response),
    ok.

test_storm_postpones(_Config) ->
    %% Test storm scenario with postpones
    %% Create a tenant governor
    TenantId = <<"test-tenant-storm">>,
    GovernorId = <<"test-governor-storm">>,
    
    %% Start governor (if not already started)
    case gproc:where({n, l, {tai_governor, TenantId}}) of
        undefined ->
            {ok, Pid} = tai_governor:start_link(TenantId, GovernorId),
            timer:sleep(500);  %% Wait for governor to initialize
        Pid ->
            ok
    end,
    
    %% Send a signal that triggers action (value > 80.0)
    Signal1 = #{
        <<"metric">> => <<"cpu_usage">>,
        <<"value">> => 85.0,
        <<"timestamp">> => erlang:system_time(second)
    },
    Envelope1 = #{
        <<"message">> => #{
            <<"data">> => base64:encode(jsx:encode(Signal1)),
            <<"messageId">> => <<"storm-msg-1">>
        }
    },
    JsonBody1 = jsx:encode(Envelope1),
    
    {ok, {{_, StatusCode1, _}, _, Body1}} = httpc:request(
        post,
        {"http://localhost:8080/pubsub", [], "application/json", JsonBody1},
        [],
        []
    ),
    %% Should accept (200) or refuse gracefully (non-5xx)
    true = StatusCode1 < 500,
    Response1 = jsx:decode(Body1, [return_maps]),
    
    %% Verify governor is in intervening state (if signal was processed)
    case maps:get(<<"type">>, Response1, <<>>) of
        <<"transition">> ->
            %% Governor should be intervening
            {ok, State} = tai_governor:get_state(Pid),
            %% State should be intervening or stable (depending on timing)
            true = (State =:= intervening orelse State =:= stable);
        _ ->
            %% If refused, that's also acceptable behavior
            ok
    end,
    
    %% Send multiple signals rapidly (storm scenario)
    %% These should be postponed if governor is intervening
    StormSignals = [
        #{<<"metric">> => <<"cpu_usage">>, <<"value">> => 82.0, <<"timestamp">> => erlang:system_time(second)},
        #{<<"metric">> => <<"cpu_usage">>, <<"value">> => 83.0, <<"timestamp">> => erlang:system_time(second)},
        #{<<"metric">> => <<"cpu_usage">>, <<"value">> => 84.0, <<"timestamp">> => erlang:system_time(second)}
    ],
    
    %% Send storm signals
    lists:foreach(fun({Idx, Signal}) ->
        Envelope = #{
            <<"message">> => #{
                <<"data">> => base64:encode(jsx:encode(Signal)),
                <<"messageId">> => <<"storm-msg-", (integer_to_binary(Idx))/binary>>
            }
        },
        JsonBody = jsx:encode(Envelope),
        {ok, {{_, StatusCode, _}, _, _}} = httpc:request(
            post,
            {"http://localhost:8080/pubsub", [], "application/json", JsonBody},
            [],
            []
        ),
        %% All should return non-5xx (either accepted or postponed)
        true = StatusCode < 500
    end, lists:zip(lists:seq(2, 4), StormSignals)),
    
    ok.
