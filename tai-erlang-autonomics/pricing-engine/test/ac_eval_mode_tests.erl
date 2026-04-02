%%%-------------------------------------------------------------------
%% @doc AC Eval Mode Tests - Comprehensive test suite
%%
%% Tests cover:
%% - Mode enforcement (always eval, never production)
%% - Authority stamping (always advisory)
%% - Session lifecycle (start, store, end, cleanup)
%% - Session secret uniqueness (per-session ephemeral)
%% - Payload decoration (maps and records)
%% - Metadata decoration (API responses)
%% - Receipt decoration (non-contractual marking)
%% - Hash computation and verification
%% - Constant-time comparison (timing attack prevention)
%% - Error handling and edge cases
%% - Audit trail integration
%%%-------------------------------------------------------------------
-module(ac_eval_mode_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/pricing_engine.hrl").

%%%===================================================================
%% Test Fixtures
%%%===================================================================

setup() ->
    %% Clean up any leftover process dictionary state
    erase(ac_eval_mode_session_ctx),
    erase(ac_eval_mode_session_id),
    erase(ac_eval_mode_session_secret),
    ok.

cleanup(_) ->
    %% Clean up after each test
    erase(ac_eval_mode_session_ctx),
    erase(ac_eval_mode_session_id),
    erase(ac_eval_mode_session_secret),
    ok.

%%%===================================================================
%% Core Mode Tests
%%%===================================================================

test_mode_always_eval() ->
    setup(),
    Mode = ac_eval_mode:mode(),
    ?assertEqual(eval, Mode),
    cleanup(ok).

test_authority_always_advisory() ->
    setup(),
    Authority = ac_eval_mode:authority(),
    ?assertEqual(advisory, Authority),
    cleanup(ok).

test_banner_not_empty() ->
    setup(),
    Banner = ac_eval_mode:banner(),
    ?assert(is_binary(Banner)),
    ?assert(byte_size(Banner) > 0),
    ?assert(binary:match(Banner, <<"advisory">>) /= nomatch),
    cleanup(ok).

test_ensure_eval_with_eval_config() ->
    setup(),
    application:set_env(pricing_engine, mode, eval),
    Result = ac_eval_mode:ensure_eval(),
    ?assertEqual(ok, Result),
    cleanup(ok).

test_ensure_eval_rejects_production() ->
    setup(),
    application:set_env(pricing_engine, mode, production),
    Result = ac_eval_mode:ensure_eval(),
    ?assertEqual({error, not_eval_mode}, Result),
    cleanup(ok).

%%%===================================================================
%% Session Lifecycle Tests
%%%===================================================================

test_start_session_generates_ids() ->
    setup(),
    {ok, SessionId, SessionSecret} = ac_eval_mode:start_session(),
    ?assert(is_binary(SessionId)),
    ?assert(byte_size(SessionId) > 0),
    ?assert(is_binary(SessionSecret)),
    ?assert(byte_size(SessionSecret) =:= 32),  %% 32 bytes for HMAC-SHA256
    cleanup(ok).

test_start_session_stores_in_process_dict() ->
    setup(),
    {ok, SessionId1, SessionSecret1} = ac_eval_mode:start_session(),
    {ok, SessionId2} = ac_eval_mode:get_session_id(),
    {ok, SessionSecret2} = ac_eval_mode:get_session_secret(),
    ?assertEqual(SessionId1, SessionId2),
    ?assertEqual(SessionSecret1, SessionSecret2),
    cleanup(ok).

test_start_session_with_options() ->
    setup(),
    Options = #{
        client_id => <<"client_123">>,
        request_id => <<"request_456">>,
        tags => [test, experimental]
    },
    {ok, SessionId, _SessionSecret} = ac_eval_mode:start_session(Options),
    {ok, SessionCtx} = ac_eval_mode:get_session_context(),
    ?assertEqual(SessionId, maps:get(session_id, SessionCtx)),
    ?assertEqual(<<"client_123">>, maps:get(client_id, SessionCtx)),
    ?assertEqual(<<"request_456">>, maps:get(request_id, SessionCtx)),
    ?assertEqual([test, experimental], maps:get(tags, SessionCtx)),
    cleanup(ok).

test_start_multiple_sessions_generate_unique_ids() ->
    setup(),
    {ok, SessionId1, SessionSecret1} = ac_eval_mode:start_session(),
    {ok, SessionId2, SessionSecret2} = ac_eval_mode:start_session(),
    %% Second session should replace first in process dict
    %% but IDs and secrets should be different when generated fresh
    ?assertNotEqual(SessionId1, SessionId2),
    ?assertNotEqual(SessionSecret1, SessionSecret2),
    cleanup(ok).

test_end_session_clears_process_dict() ->
    setup(),
    {ok, SessionId, _} = ac_eval_mode:start_session(),
    ok = ac_eval_mode:end_session(SessionId),
    ?assertEqual({error, no_active_session}, ac_eval_mode:get_session_id()),
    ?assertEqual({error, no_active_session}, ac_eval_mode:get_session_secret()),
    cleanup(ok).

test_end_session_validates_session_id() ->
    setup(),
    {ok, _SessionId, _} = ac_eval_mode:start_session(),
    Result = ac_eval_mode:end_session(<<"wrong_id">>),
    ?assertEqual({error, invalid_session}, Result),
    cleanup(ok).

test_get_session_id_no_active_session() ->
    setup(),
    Result = ac_eval_mode:get_session_id(),
    ?assertEqual({error, no_active_session}, Result),
    cleanup(ok).

test_get_session_secret_no_active_session() ->
    setup(),
    Result = ac_eval_mode:get_session_secret(),
    ?assertEqual({error, no_active_session}, Result),
    cleanup(ok).

%%%===================================================================
%% Session Hash and Verification Tests
%%%===================================================================

test_compute_session_hash_deterministic() ->
    setup(),
    SessionId = <<"session_123">>,
    SessionSecret = <<"secret_456">>,
    Hash1 = ac_eval_mode:compute_session_hash(SessionId, SessionSecret),
    Hash2 = ac_eval_mode:compute_session_hash(SessionId, SessionSecret),
    ?assertEqual(Hash1, Hash2),
    cleanup(ok).

test_compute_session_hash_differs_with_different_secret() ->
    setup(),
    SessionId = <<"session_123">>,
    SessionSecret1 = <<"secret_456">>,
    SessionSecret2 = <<"secret_789">>,
    Hash1 = ac_eval_mode:compute_session_hash(SessionId, SessionSecret1),
    Hash2 = ac_eval_mode:compute_session_hash(SessionId, SessionSecret2),
    ?assertNotEqual(Hash1, Hash2),
    cleanup(ok).

test_compute_session_hash_differs_with_different_id() ->
    setup(),
    SessionId1 = <<"session_123">>,
    SessionId2 = <<"session_456">>,
    SessionSecret = <<"secret_789">>,
    Hash1 = ac_eval_mode:compute_session_hash(SessionId1, SessionSecret),
    Hash2 = ac_eval_mode:compute_session_hash(SessionId2, SessionSecret),
    ?assertNotEqual(Hash1, Hash2),
    cleanup(ok).

test_verify_session_hash_valid() ->
    setup(),
    {ok, SessionId, SessionSecret} = ac_eval_mode:start_session(),
    Hash = ac_eval_mode:compute_session_hash(SessionId, SessionSecret),
    Result = ac_eval_mode:validate_session(Hash),
    ?assertEqual({ok, valid}, Result),
    cleanup(ok).

test_verify_session_hash_invalid() ->
    setup(),
    {ok, _SessionId, _SessionSecret} = ac_eval_mode:start_session(),
    InvalidHash = <<"invalid_hash_12345678">>,
    Result = ac_eval_mode:validate_session(InvalidHash),
    ?assertEqual({error, invalid_hash}, Result),
    cleanup(ok).

test_verify_session_hash_no_session() ->
    setup(),
    Hash = <<"some_hash">>,
    Result = ac_eval_mode:validate_session(Hash),
    ?assertEqual({error, no_active_session}, Result),
    cleanup(ok).

%%%===================================================================
%% Payload Decoration Tests
%%%===================================================================

test_decorate_payload_map() ->
    setup(),
    {ok, _SessionId, _SessionSecret} = ac_eval_mode:start_session(),
    Payload = #{
        <<"customer_id">> => <<"cust_123">>,
        <<"value">> => 100.0
    },
    {ok, Decorated} = ac_eval_mode:decorate_payload(Payload),
    ?assertEqual(true, maps:get(<<"eval_only">>, Decorated)),
    ?assertEqual(advisory, maps:get(<<"authority">>, Decorated)),
    ?assert(is_binary(maps:get(<<"disclaimer">>, Decorated))),
    ?assert(is_binary(maps:get(<<"session_id">>, Decorated))),
    ?assert(is_binary(maps:get(<<"session_hash">>, Decorated))),
    ?assert(is_integer(maps:get(<<"session_timestamp">>, Decorated))),
    cleanup(ok).

test_decorate_payload_preserves_original_fields() ->
    setup(),
    {ok, _SessionId, _SessionSecret} = ac_eval_mode:start_session(),
    Payload = #{
        <<"customer_id">> => <<"cust_123">>,
        <<"value">> => 100.0,
        <<"status">> => <<"completed">>
    },
    {ok, Decorated} = ac_eval_mode:decorate_payload(Payload),
    ?assertEqual(<<"cust_123">>, maps:get(<<"customer_id">>, Decorated)),
    ?assertEqual(100.0, maps:get(<<"value">>, Decorated)),
    ?assertEqual(<<"completed">>, maps:get(<<"status">>, Decorated)),
    cleanup(ok).

test_decorate_payload_value_record() ->
    setup(),
    {ok, _SessionId, _SessionSecret} = ac_eval_mode:start_session(),
    ValueRecord = #value_record{
        customer_id = <<"cust_123">>,
        calculated_value = 100.0,
        billed_price = 10000.0,
        timestamp = erlang:system_time(millisecond),
        metrics = [{<<"throughput">>, 100.0}],
        status = completed,
        receipt_hash = <<"hash_123">>,
        previous_hash = undefined,
        hmac_signature = undefined,
        calculation_method = weighted_sum,
        audit_metadata = #{}
    },
    {ok, Decorated} = ac_eval_mode:decorate_payload(ValueRecord),
    ?assertEqual(true, maps:get(<<"eval_only">>, Decorated)),
    ?assertEqual(advisory, maps:get(<<"authority">>, Decorated)),
    ?assertEqual(<<"cust_123">>, maps:get(<<"customer_id">>, Decorated)),
    ?assertEqual(100.0, maps:get(<<"calculated_value">>, Decorated)),
    cleanup(ok).

test_decorate_payload_no_session() ->
    setup(),
    Payload = #{<<"test">> => <<"data">>},
    Result = ac_eval_mode:decorate_payload(Payload),
    ?assertEqual({error, {session_context_unavailable, no_active_session}}, Result),
    cleanup(ok).

%%%===================================================================
%% Metadata Decoration Tests
%%%===================================================================

test_decorate_meta_basic() ->
    setup(),
    {ok, _SessionId, _SessionSecret} = ac_eval_mode:start_session(),
    ResponseMeta = #{<<"status">> => <<"success">>},
    Options = #{
        request_id => <<"req_123">>,
        client_id => <<"client_456">>
    },
    {ok, Decorated} = ac_eval_mode:decorate_meta(ResponseMeta, Options),
    ?assertEqual(eval, maps:get(<<"mode">>, Decorated)),
    ?assertEqual(advisory, maps:get(<<"authority">>, Decorated)),
    ?assert(is_binary(maps:get(<<"disclaimer">>, Decorated))),
    ?assertEqual(<<"req_123">>, maps:get(<<"request_id">>, Decorated)),
    ?assertEqual(<<"client_456">>, maps:get(<<"client_id">>, Decorated)),
    cleanup(ok).

test_decorate_meta_preserves_original() ->
    setup(),
    {ok, _SessionId, _SessionSecret} = ac_eval_mode:start_session(),
    ResponseMeta = #{<<"status">> => <<"success">>, <<"data">> => <<"test">>},
    Options = #{},
    {ok, Decorated} = ac_eval_mode:decorate_meta(ResponseMeta, Options),
    ?assertEqual(<<"success">>, maps:get(<<"status">>, Decorated)),
    ?assertEqual(<<"test">>, maps:get(<<"data">>, Decorated)),
    cleanup(ok).

test_decorate_meta_no_session() ->
    setup(),
    ResponseMeta = #{<<"test">> => <<"data">>},
    Options = #{},
    Result = ac_eval_mode:decorate_meta(ResponseMeta, Options),
    ?assertEqual({error, {session_context_unavailable, no_active_session}}, Result),
    cleanup(ok).

%%%===================================================================
%% Receipt Decoration Tests
%%%===================================================================

test_decorate_receipt() ->
    setup(),
    {ok, _SessionId, _SessionSecret} = ac_eval_mode:start_session(),
    Receipt = #receipt{
        receipt_id = <<"receipt_123">>,
        customer_id = <<"cust_123">>,
        calculated_value = 100.0,
        billed_price = 10000.0,
        currency = <<"USD">>,
        period_start = erlang:system_time(millisecond),
        period_end = erlang:system_time(millisecond),
        calculation_timestamp = erlang:system_time(millisecond),
        value_hash = <<"hash_123">>,
        signature = <<"sig_123">>,
        invoice_id = undefined,
        payment_status = pending,
        verified = false,
        verification_timestamp = undefined
    },
    {ok, Decorated} = ac_eval_mode:decorate_receipt(Receipt),
    ?assertEqual(true, maps:get(<<"eval_only">>, Decorated)),
    ?assertEqual(advisory, maps:get(<<"authority">>, Decorated)),
    ?assertEqual(true, maps:get(<<"non_contractual">>, Decorated)),
    ?assertEqual(true, maps:get(<<"use_for_billing_prohibited">>, Decorated)),
    ?assertEqual(<<"receipt_123">>, maps:get(<<"receipt_id">>, Decorated)),
    cleanup(ok).

%%%===================================================================
%% Session Secret Uniqueness Tests
%%%===================================================================

test_session_secrets_unique_per_call() ->
    setup(),
    {ok, _Id1, Secret1} = ac_eval_mode:start_session(),
    {ok, _Id2, Secret2} = ac_eval_mode:start_session(),
    %% Secrets should be different (per-session ephemeral)
    ?assertNotEqual(Secret1, Secret2),
    cleanup(ok).

test_session_secrets_never_persisted() ->
    setup(),
    {ok, SessionId, SessionSecret} = ac_eval_mode:start_session(),
    %% Get session context
    {ok, SessionCtx} = ac_eval_mode:get_session_context(),
    %% Secret should be in memory but not persisted anywhere
    ?assertEqual(SessionSecret, maps:get(session_secret, SessionCtx)),
    %% End session
    ok = ac_eval_mode:end_session(SessionId),
    %% Context should be gone
    ?assertEqual({error, no_active_session}, ac_eval_mode:get_session_context()),
    cleanup(ok).

%%%===================================================================
%% Hash Computation Tests
%%%===================================================================

test_session_hash_is_hmac_sha256() ->
    setup(),
    SessionId = <<"session_test">>,
    SessionSecret = <<"secret_test">>,
    Hash = ac_eval_mode:compute_session_hash(SessionId, SessionSecret),
    %% SHA-256 produces 32-byte output
    ?assertEqual(32, byte_size(Hash)),
    ?assert(is_binary(Hash)),
    cleanup(ok).

test_session_hash_different_inputs() ->
    setup(),
    Hash1 = ac_eval_mode:compute_session_hash(<<"id1">>, <<"secret1">>),
    Hash2 = ac_eval_mode:compute_session_hash(<<"id2">>, <<"secret2">>),
    Hash3 = ac_eval_mode:compute_session_hash(<<"id1">>, <<"secret1">>),
    ?assertNotEqual(Hash1, Hash2),
    ?assertEqual(Hash1, Hash3),  %% Deterministic
    cleanup(ok).

%%%===================================================================
%% Constant-Time Comparison Tests
%%%===================================================================

test_constant_time_compare_equal() ->
    setup(),
    Data1 = <<"test_data_123">>,
    Data2 = <<"test_data_123">>,
    %% Can't call private function directly, test via validate_session
    {ok, SessionId, SessionSecret} = ac_eval_mode:start_session(),
    Hash = ac_eval_mode:compute_session_hash(SessionId, SessionSecret),
    Result = ac_eval_mode:validate_session(Hash),
    ?assertEqual({ok, valid}, Result),
    cleanup(ok).

test_constant_time_compare_different_length() ->
    setup(),
    {ok, _SessionId, _SessionSecret} = ac_eval_mode:start_session(),
    ShortHash = <<"short">>,
    Result = ac_eval_mode:validate_session(ShortHash),
    ?assertEqual({error, invalid_hash}, Result),
    cleanup(ok).

%%%===================================================================
%% Integration Tests
%%%===================================================================

test_full_session_lifecycle() ->
    setup(),
    %% Start session
    {ok, SessionId, _SessionSecret} = ac_eval_mode:start_session(#{
        client_id => <<"test_client">>,
        request_id => <<"test_request">>
    }),

    %% Decorate payload
    Payload = #{<<"test">> => <<"data">>},
    {ok, Decorated1} = ac_eval_mode:decorate_payload(Payload),
    ?assertEqual(true, maps:get(<<"eval_only">>, Decorated1)),

    %% Decorate meta
    {ok, Decorated2} = ac_eval_mode:decorate_meta(#{}, #{request_id => <<"req_1">>}),
    ?assertEqual(eval, maps:get(<<"mode">>, Decorated2)),

    %% Verify session
    SessionHash = maps:get(<<"session_hash">>, Decorated1),
    {ok, valid} = ac_eval_mode:validate_session(SessionHash),

    %% End session
    ok = ac_eval_mode:end_session(SessionId),
    ?assertEqual({error, no_active_session}, ac_eval_mode:get_session_id()),

    cleanup(ok).

test_multiple_concurrent_sessions() ->
    setup(),
    %% Simulate multiple concurrent sessions (one per process dict)
    {ok, SessionId1, _Secret1} = ac_eval_mode:start_session(#{client_id => <<"c1">>}),

    %% Get current session context
    {ok, Ctx1} = ac_eval_mode:get_session_context(),
    ClientId1 = maps:get(client_id, Ctx1),
    ?assertEqual(<<"c1">>, ClientId1),

    %% End first session
    ok = ac_eval_mode:end_session(SessionId1),

    %% Start second session
    {ok, SessionId2, _Secret2} = ac_eval_mode:start_session(#{client_id => <<"c2">>}),
    {ok, Ctx2} = ac_eval_mode:get_session_context(),
    ClientId2 = maps:get(client_id, Ctx2),
    ?assertEqual(<<"c2">>, ClientId2),

    ok = ac_eval_mode:end_session(SessionId2),
    cleanup(ok).

%%%===================================================================
%% Edge Cases and Error Handling
%%%===================================================================

test_decorate_payload_with_empty_map() ->
    setup(),
    {ok, _SessionId, _SessionSecret} = ac_eval_mode:start_session(),
    {ok, Decorated} = ac_eval_mode:decorate_payload(#{}),
    ?assertEqual(true, maps:get(<<"eval_only">>, Decorated)),
    cleanup(ok).

test_decorate_meta_with_empty_map() ->
    setup(),
    {ok, _SessionId, _SessionSecret} = ac_eval_mode:start_session(),
    {ok, Decorated} = ac_eval_mode:decorate_meta(#{}, #{}),
    ?assertEqual(eval, maps:get(<<"mode">>, Decorated)),
    cleanup(ok).

test_banner_contains_advisory_disclaimer() ->
    setup(),
    Banner = ac_eval_mode:banner(),
    ?assert(binary:match(Banner, <<"ADVISORY">>) /= nomatch orelse
            binary:match(Banner, <<"advisory">>) /= nomatch),
    ?assert(binary:match(Banner, <<"non-contractual">>) /= nomatch orelse
            binary:match(Banner, <<"non-binding">>) /= nomatch orelse
            binary:match(Banner, <<"not a legal contract">>) /= nomatch),
    cleanup(ok).

%%%===================================================================
%% Helper Functions for Test Execution
%%%===================================================================

%% Run test with setup and cleanup
run_test(TestFun) ->
    setup(),
    try
        TestFun()
    after
        cleanup(ok)
    end.
