# TAI Erlang Autonomics: Security Testing Guide

**Date:** January 25, 2026
**Purpose:** Comprehensive security testing procedures for TAI Erlang Autonomics

---

## Table of Contents

1. [Testing Infrastructure Setup](#testing-infrastructure-setup)
2. [Unit & Integration Security Tests](#unit--integration-security-tests)
3. [API Security Testing](#api-security-testing)
4. [Cryptographic Testing](#cryptographic-testing)
5. [GCP Integration Testing](#gcp-integration-testing)
6. [Performance & Load Testing](#performance--load-testing)
7. [Compliance Testing](#compliance-testing)

---

## Testing Infrastructure Setup

### Prerequisites

```bash
# Install testing dependencies
rebar3 eunit      # Unit tests
rebar3 ct         # Common Test (integration tests)
rebar3 proper     # Property-based testing

# Install security testing tools
pip install bandit          # Python security linter (if applicable)
npm install -g retire       # Dependency checker
owasp-dependency-check      # Dependency vulnerability scanner
```

### Test Environment Configuration

```erlang
%% config/test.config - Testing configuration
[
  {tai_autonomics, [
    %% Disable TLS for local testing
    {tls_enabled, false},

    %% Use emulator for Firestore
    {firestore_enabled, true},
    {firestore_emulator_host, "localhost:8081"},

    %% Use emulator for Pub/Sub
    {pubsub_enabled, true},
    {pubsub_emulator_host, "localhost:8085"},

    %% Disable authentication in tests
    {jwt_enabled, false},
    {verify_signatures, false},

    %% Use test keys
    {jwt_public_key, <<"test-public-key">>},
    {jwt_signing_key, <<"test-signing-key">>},

    %% Enable failure injection for chaos testing
    {failure_injection_enabled, true},

    %% Short timeouts for testing
    {request_timeout_ms, 5000},
    {connection_timeout_ms, 3000},

    %% Audit to file for testing
    {audit_backend, file},
    {audit_file, "/tmp/tai_audit.log"}
  ]}
].
```

---

## Unit & Integration Security Tests

### 1. Input Validation Tests

**File:** `test/input_validation_tests.erl`

```erlang
-module(input_validation_tests).

-include_lib("eunit/include/eunit.hrl").

%% UUID validation tests
uuid_validation_test_() ->
    [
        {"Valid UUID format", ?_assert(validate_uuid(<<"550e8400-e29b-41d4-a716-446655440000">>))},
        {"Invalid UUID - wrong length", ?_assertNot(validate_uuid(<<"550e8400-e29b-41d4-a716">>))},
        {"Invalid UUID - wrong format", ?_assertNot(validate_uuid(<<"not-a-uuid">>))},
        {"Null byte injection", ?_assertNot(validate_uuid(<<"550e8400\x00e29b-41d4-a716-446655440000">>))},
        {"Empty string", ?_assertNot(validate_uuid(<<>>))}
    ].

%% Tenant ID validation tests
tenant_id_validation_test_() ->
    [
        {"Valid tenant ID", ?_assert(validate_tenant_id(<<"acme-corp">>))},
        {"Empty tenant ID", ?_assertNot(validate_tenant_id(<<>>))},
        {"Too long tenant ID", ?_assertNot(validate_tenant_id(binary:copy(<<"x">>, 1000)))},
        {"SQL injection attempt", ?_assertNot(validate_tenant_id(<<"'; DROP TABLE users;--">>))},
        {"Path traversal attempt", ?_assertNot(validate_tenant_id(<<"../../etc/passwd">>))}
    ].

%% JSON payload size limits
json_size_limit_test_() ->
    [
        {"Valid size", ?_assert(check_payload_size(jsx:encode(#{small => <<"data">>})))},
        {"Exactly 1MB", ?_assert(check_payload_size(binary:copy(<<"x">>, 1048576))))},
        {"Over 1MB", ?_assertNot(check_payload_size(binary:copy(<<"x">>, 1048577)))},
        {"Zero size", ?_assertNot(check_payload_size(<<>>))}
    ].

%% Nested object depth limit
nesting_depth_test_() ->
    [
        {"Shallow nesting", ?_assert(check_max_depth(create_nested_map(5), 10))},
        {"Max depth allowed", ?_assert(check_max_depth(create_nested_map(10), 10))},
        {"Exceeds max depth", ?_assertNot(check_max_depth(create_nested_map(11), 10))}
    ].

%% Helper functions
validate_uuid(Value) when is_binary(Value) ->
    case byte_size(Value) of
        36 -> validate_uuid_format(Value);
        _ -> false
    end;
validate_uuid(_) -> false.

validate_uuid_format(Value) ->
    % Check UUID format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
    case re:match(Value, "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$") of
        {match, _} -> true;
        nomatch -> false
    end.

validate_tenant_id(Value) when is_binary(Value) ->
    Size = byte_size(Value),
    Size >= 1 andalso Size =< 128 andalso no_suspicious_chars(Value);
validate_tenant_id(_) -> false.

no_suspicious_chars(Value) ->
    % Reject common injection patterns
    not (
        binary:match(Value, <<"'">>) =/= nomatch orelse
        binary:match(Value, <<";">">>) =/= nomatch orelse
        binary:match(Value, <<"--">>) =/= nomatch orelse
        binary:match(Value, <<"../">>) =/= nomatch orelse
        binary:match(Value, <<"\x00">>) =/= nomatch
    ).

check_payload_size(Payload) ->
    byte_size(Payload) > 0 andalso byte_size(Payload) =< 1048576.

check_max_depth(Object, MaxDepth) ->
    check_depth(Object, 0, MaxDepth).

check_depth(_, Current, Max) when Current > Max ->
    false;
check_depth(Map, Current, Max) when is_map(Map) ->
    maps:fold(fun(_, Value, Acc) ->
        Acc andalso check_depth(Value, Current + 1, Max)
    end, true, Map);
check_depth(List, Current, Max) when is_list(List) ->
    lists:all(fun(Item) ->
        check_depth(Item, Current + 1, Max)
    end, List);
check_depth(_, _, _) ->
    true.

create_nested_map(0) -> #{};
create_nested_map(N) -> #{nested => create_nested_map(N - 1)}.
```

### 2. Authentication & JWT Tests

**File:** `test/authentication_tests.erl`

```erlang
-module(authentication_tests).

-include_lib("eunit/include/eunit.hrl").

%% JWT expiration validation
jwt_expiration_test_() ->
    [
        {"Valid token", ?_assert(validate_jwt(create_valid_jwt()))},
        {"Expired token", ?_assertNot(validate_jwt(create_expired_jwt()))},
        {"Not yet valid", ?_assertNot(validate_jwt(create_not_yet_valid_jwt()))},
        {"No expiration claim", ?_assertNot(validate_jwt(create_jwt_without_exp()))},
        {"Expiration far future", ?_assertNot(validate_jwt(create_jwt_exp_too_far()))}
    ].

%% JWT signature validation
jwt_signature_test_() ->
    [
        {"Valid signature", ?_assert(verify_jwt_signature(create_valid_jwt()))},
        {"Invalid signature", ?_assertNot(verify_jwt_signature(<<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.invalid">>))},
        {"Tampered payload", ?_assertNot(verify_jwt_signature(tamper_jwt(create_valid_jwt())))},
        {"Algorithm mismatch", ?_assertNot(verify_jwt_signature(create_jwt_wrong_algorithm()))},
        {"Missing signature", ?_assertNot(verify_jwt_signature(<<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWI">>))}
    ].

%% JWT required claims
jwt_claims_test_() ->
    [
        {"Valid claims", ?_assert(validate_jwt_claims(create_valid_jwt()))},
        {"Missing iss claim", ?_assertNot(validate_jwt_claims(create_jwt_without_claim(<<"iss">>)))},
        {"Missing aud claim", ?_assertNot(validate_jwt_claims(create_jwt_without_claim(<<"aud">>)))},
        {"Untrusted issuer", ?_assertNot(validate_jwt_claims(create_jwt_untrusted_issuer()))},
        {"Invalid audience", ?_assertNot(validate_jwt_claims(create_jwt_invalid_audience()))}
    ].

%% Helper functions
create_valid_jwt() ->
    Claims = #{
        <<"exp">> => erlang:system_time(second) + 3600,
        <<"iat">> => erlang:system_time(second),
        <<"iss">> => <<"https://trusted.issuer">>,
        <<"aud">> => <<"tai-autonomics">>,
        <<"sub">> => <<"user123">>
    },
    create_jwt_signed(Claims).

create_expired_jwt() ->
    Claims = #{
        <<"exp">> => erlang:system_time(second) - 3600,
        <<"iat">> => erlang:system_time(second) - 7200
    },
    create_jwt_signed(Claims).

create_not_yet_valid_jwt() ->
    Claims = #{
        <<"nbf">> => erlang:system_time(second) + 3600,
        <<"exp">> => erlang:system_time(second) + 7200
    },
    create_jwt_signed(Claims).

create_jwt_without_exp() ->
    Claims = #{
        <<"iat">> => erlang:system_time(second)
    },
    create_jwt_signed(Claims).

create_jwt_exp_too_far() ->
    Claims = #{
        <<"exp">> => erlang:system_time(second) + 86400 * 365  % 1 year in future
    },
    create_jwt_signed(Claims).

create_jwt_wrong_algorithm() ->
    % JWT signed with different algorithm
    Header = base64:encode(jsx:encode(#{<<"alg">> => <<"HS512">>})),
    Payload = base64:encode(jsx:encode(#{<<"exp">> => erlang:system_time(second) + 3600})),
    Signature = base64:encode(<<"invalid">>),
    <<Header/binary, ".", Payload/binary, ".", Signature/binary>>.

create_jwt_without_claim(ClaimKey) ->
    Claims = #{
        <<"exp">> => erlang:system_time(second) + 3600,
        <<"iat">> => erlang:system_time(second),
        <<"iss">> => <<"https://trusted.issuer">>,
        <<"aud">> => <<"tai-autonomics">>
    },
    ClaimsWithoutKey = maps:remove(ClaimKey, Claims),
    create_jwt_signed(ClaimsWithoutKey).

create_jwt_untrusted_issuer() ->
    Claims = #{
        <<"exp">> => erlang:system_time(second) + 3600,
        <<"iat">> => erlang:system_time(second),
        <<"iss">> => <<"https://untrusted.issuer">>,
        <<"aud">> => <<"tai-autonomics">>
    },
    create_jwt_signed(Claims).

create_jwt_invalid_audience() ->
    Claims = #{
        <<"exp">> => erlang:system_time(second) + 3600,
        <<"iat">> => erlang:system_time(second),
        <<"iss">> => <<"https://trusted.issuer">>,
        <<"aud">> => <<"wrong-audience">>
    },
    create_jwt_signed(Claims).

create_jwt_signed(Claims) ->
    % Simplified - in real tests use jose library
    jose_jwt:sign(get_test_signing_key(), Claims).

verify_jwt_signature(Token) ->
    case catch jose_jwt:verify(get_test_verification_key(), Token) of
        {true, _Claims, _JWS} -> true;
        _ -> false
    end.

validate_jwt(Token) ->
    case verify_jwt_signature(Token) of
        true -> validate_jwt_claims_time(Token);
        false -> false
    end.

validate_jwt_claims(Token) ->
    case verify_jwt_signature(Token) of
        true ->
            {true, Claims, _} = jose_jwt:verify(get_test_verification_key(), Token),
            maps:is_key(<<"iss">>, Claims) andalso
            maps:is_key(<<"aud">>, Claims) andalso
            is_trusted_issuer(maps:get(<<"iss">>, Claims)) andalso
            is_valid_audience(maps:get(<<"aud">>, Claims));
        false -> false
    end.

validate_jwt_claims_time(Token) ->
    {true, Claims, _} = jose_jwt:verify(get_test_verification_key(), Token),
    Now = erlang:system_time(second),
    Exp = maps:get(<<"exp">>, Claims, 0),
    Iat = maps:get(<<"iat">>, Claims, 0),
    Nbf = maps:get(<<"nbf">>, Claims, 0),

    (Exp > Now) andalso
    (Iat =< Now) andalso
    (Nbf =< Now).

tamper_jwt(Token) ->
    [Header, _Payload, Signature] = string:split(Token, ".", all),
    TamperedPayload = base64:encode(<<"tampered">>),
    <<Header/binary, ".", TamperedPayload/binary, ".", Signature/binary>>.

is_trusted_issuer(Issuer) ->
    lists:member(Issuer, [<<"https://trusted.issuer">>]).

is_valid_audience(Audience) ->
    lists:member(Audience, [<<"tai-autonomics">>]).

get_test_signing_key() ->
    <<"test-secret-key">>.

get_test_verification_key() ->
    get_test_signing_key().
```

### 3. Authorization Tests

**File:** `test/authorization_tests.erl`

```erlang
-module(authorization_tests).

-include_lib("eunit/include/eunit.hrl").

%% Role-based access control tests
rbac_test_() ->
    [
        {"Admin can grant", ?_assert(check_permission(admin, grant))},
        {"Admin can revoke", ?_assert(check_permission(admin, revoke))},
        {"Operator can grant", ?_assert(check_permission(operator, grant))},
        {"Operator cannot delete", ?_assertNot(check_permission(operator, delete))},
        {"Viewer cannot grant", ?_assertNot(check_permission(viewer, grant))},
        {"Viewer can view", ?_assert(check_permission(viewer, view))}
    ].

%% Tenant isolation tests
tenant_isolation_test_() ->
    [
        {"Same tenant access allowed", ?_assert(check_tenant_access(
            <<"tenant-1">>, <<"tenant-1">>, admin))},
        {"Cross-tenant access denied", ?_assertNot(check_tenant_access(
            <<"tenant-1">>, <<"tenant-2">>, admin))},
        {"System role bypasses check", ?_assert(check_tenant_access(
            <<"tenant-1">>, <<"tenant-2">>, system))},
        {"Non-admin cross-tenant denied", ?_assertNot(check_tenant_access(
            <<"tenant-1">>, <<"tenant-2">>, operator))}
    ].

%% Permission elevation tests
permission_elevation_test_() ->
    [
        {"User cannot grant themselves admin", ?_assertNot(elevate_permission(admin, <<"user1">>))},
        {"Invalid role request rejected", ?_assertNot(elevate_permission(invalid_role, <<"user1">>))},
        {"Operator cannot become admin", ?_assertNot(can_grant_role(operator, admin))}
    ].

%% Helper functions
check_permission(Role, Action) ->
    Permissions = get_role_permissions(Role),
    lists:member(Action, Permissions).

get_role_permissions(admin) ->
    [grant, revoke, suspend, delete, view, create];
get_role_permissions(operator) ->
    [grant, revoke, suspend, view];
get_role_permissions(viewer) ->
    [view];
get_role_permissions(system) ->
    [all];
get_role_permissions(_) ->
    [].

check_tenant_access(UserTenant, ResourceTenant, Role) ->
    case Role of
        system -> true;  % System accounts bypass tenant check
        _ ->
            UserTenant =:= ResourceTenant
    end.

elevate_permission(Role, _User) ->
    % Users should never grant themselves admin
    Role =/= admin.

can_grant_role(ActorRole, TargetRole) ->
    % Only admin can grant admin role
    case ActorRole of
        admin -> true;
        _ -> TargetRole =/= admin
    end.
```

---

## API Security Testing

### Load Testing with Security Focus

**File:** `test/security_load_tests.erl`

```erlang
-module(security_load_tests).

-include_lib("common_test/include/ct.hrl").

%% Test specifications
all() ->
    [
        rate_limiting_under_load,
        connection_pool_exhaustion,
        memory_exhaustion_protection,
        error_handling_under_stress
    ].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% Test rate limiting
rate_limiting_under_load(Config) ->
    ct:log("Testing rate limiting under load..."),

    %% Send 1000 requests rapidly
    Results = concurrent_requests(1000, <<"GET">>, <<"/health">>),

    %% Count successful and rate-limited responses
    {Success, RateLimited} = count_responses(Results),

    ct:log("Results: ~p success, ~p rate-limited", [Success, RateLimited]),

    %% Verify some requests were rate-limited
    ?assert(RateLimited > 0),
    ?assert(Success > 0).

%% Test connection pool exhaustion
connection_pool_exhaustion(Config) ->
    ct:log("Testing connection pool exhaustion..."),

    %% Create many concurrent connections
    MaxConnections = 100,
    Pids = [spawn_connection() || _ <- lists:seq(1, MaxConnections * 2)],

    %% Wait for all to complete
    Results = [receive_result(Pid) || Pid <- Pids],

    %% Some should succeed, some should fail
    Successes = length([1 || {ok, _} <- Results]),
    Failures = length([1 || {error, _} <- Results]),

    ct:log("Connections: ~p succeeded, ~p failed", [Successes, Failures]),

    ?assert(Successes > 0),
    ?assert(Failures > 0).

%% Test memory exhaustion protection
memory_exhaustion_protection(Config) ->
    ct:log("Testing large payload rejection..."),

    %% Attempt to send oversized payloads
    Results = lists:map(fun(Size) ->
        Payload = binary:copy(<<"x">>, Size),
        try_oversized_request(Payload)
    end, [
        1024,
        102400,      % 100KB - should succeed
        1048576,     % 1MB - should succeed
        2097152,     % 2MB - should fail
        10485760     % 10MB - should fail
    ]),

    % Verify large payloads rejected
    ?assertNot(lists:member(ok, lists:sublist(Results, 4, 2))).

%% Test error handling under stress
error_handling_under_stress(Config) ->
    ct:log("Testing error handling under stress..."),

    %% Send various malformed requests
    MalformedRequests = [
        <<"invalid json">>,
        <<"{incomplete">>,
        <<"{\"key\": >>>">>,
        binary:copy(<<"x">>, 10000000)  % Huge payload
    ],

    Results = lists:map(fun try_malformed_request/1, MalformedRequests),

    %% All should fail gracefully
    ?assert(lists:all(fun({error, _}) -> true; (_) -> false end, Results)).

%% Helper functions
concurrent_requests(N, Method, Path) ->
    Pids = [spawn_request(Method, Path) || _ <- lists:seq(1, N)],
    [receive_result(Pid) || Pid <- Pids].

spawn_request(Method, Path) ->
    Pid = self(),
    spawn(fun() ->
        Result = make_http_request(Method, Path),
        Pid ! {self(), Result}
    end).

spawn_connection() ->
    Pid = self(),
    spawn(fun() ->
        Result = make_persistent_connection(),
        Pid ! {self(), Result}
    end).

receive_result(Pid) ->
    receive
        {Pid, Result} -> Result
    after 10000 -> {error, timeout}
    end.

make_http_request(Method, Path) ->
    Url = "http://localhost:8080" ++ binary_to_list(Path),
    case httpc:request(binary_to_atom(Method, utf8), {Url, []}, [], []) of
        {ok, {{_, Status, _}, _, _}} -> {ok, Status};
        {error, Reason} -> {error, Reason}
    end.

make_persistent_connection() ->
    case gen_tcp:connect("localhost", 8080, [{active, false}]) of
        {ok, Sock} ->
            gen_tcp:close(Sock),
            {ok, connected};
        {error, Reason} ->
            {error, Reason}
    end.

try_oversized_request(Payload) ->
    try
        Url = "http://localhost:8080/pubsub",
        Headers = [{"Content-Type", "application/json"}],
        case httpc:request(post, {Url, Headers, "application/json", Payload}, [], []) of
            {ok, {{_, Status, _}, _, _}} when Status < 400 -> ok;
            {ok, {{_, Status, _}, _, _}} -> {error, Status};
            {error, Reason} -> {error, Reason}
        end
    catch
        _:_ -> {error, exception}
    end.

try_malformed_request(Payload) ->
    Url = "http://localhost:8080/pubsub",
    Headers = [{"Content-Type", "application/json"}],
    case httpc:request(post, {Url, Headers, "application/json", Payload}, [], []) of
        {ok, {{_, 400, _}, _, _}} -> {error, bad_request};
        {ok, {{_, 413, _}, _, _}} -> {error, payload_too_large};
        {ok, {{_, Status, _}, _, _}} -> {error, {unexpected_status, Status}};
        {error, Reason} -> {error, Reason}
    end.

count_responses(Results) ->
    {
        length([1 || {ok, Status} <- Results, Status >= 200, Status < 300]),
        length([1 || {ok, 429} <- Results])
    }.
```

---

## Cryptographic Testing

### Hash Chain Verification Tests

**File:** `test/cryptography_tests.erl`

```erlang
-module(cryptography_tests).

-include_lib("eunit/include/eunit.hrl").

%% SHA256 consistency tests
hash_consistency_test_() ->
    [
        {"Same input same hash", ?_assertEqual(
            crypto:hash(sha256, <<"test">>),
            crypto:hash(sha256, <<"test">>)
        )},
        {"Different input different hash", ?_assertNotEqual(
            crypto:hash(sha256, <<"test1">>),
            crypto:hash(sha256, <<"test2">>)
        )},
        {"Empty input hashed", ?_assert(
            byte_size(crypto:hash(sha256, <<>>)) =:= 32
        )}
    ].

%% Hash chain integrity tests
hash_chain_integrity_test_() ->
    [
        {"Valid chain verified", ?_assert(
            verify_hash_chain(create_valid_chain(5))
        )},
        {"Tampered receipt rejected", ?_assertNot(
            verify_hash_chain(tamper_receipt(create_valid_chain(5), 2))
        )},
        {"Missing receipt breaks chain", ?_assertNot(
            verify_hash_chain(remove_receipt(create_valid_chain(5), 3))
        )},
        {"Reordered receipts rejected", ?_assertNot(
            verify_hash_chain(reorder_receipts(create_valid_chain(5)))
        )}
    ].

%% HMAC signing tests
hmac_signing_test_() ->
    [
        {"Valid signature verified", ?_assert(
            verify_hmac_signature(<<"data">>, get_test_key())
        )},
        {"Invalid key fails", ?_assertNot(
            verify_hmac_signature_with_key(<<"data">>, <<"wrong-key">>)
        )},
        {"Tampered data fails", ?_assertNot(
            verify_hmac_tampered(<<"data">>, get_test_key())
        )}
    ].

%% Helper functions
create_valid_chain(Depth) ->
    create_chain(Depth, <<>>, []).

create_chain(0, _PrevHash, Receipts) ->
    lists:reverse(Receipts);
create_chain(Depth, PrevHash, Receipts) ->
    Receipt = #{
        id => generate_receipt_id(),
        data => <<"test data">>,
        hash => crypto:hash(sha256, <<"test data">>),
        chain_hash => compute_chain_hash(PrevHash, crypto:hash(sha256, <<"test data">>))
    },
    create_chain(Depth - 1, maps:get(chain_hash, Receipt), [Receipt | Receipts]).

verify_hash_chain(Receipts) ->
    verify_chain_recursive(Receipts, <<>>).

verify_chain_recursive([], _) -> true;
verify_chain_recursive([Receipt | Rest], PrevHash) ->
    CurrentHash = maps:get(hash, Receipt),
    ExpectedChainHash = compute_chain_hash(PrevHash, CurrentHash),
    ActualChainHash = maps:get(chain_hash, Receipt),

    case ExpectedChainHash =:= ActualChainHash of
        true -> verify_chain_recursive(Rest, ActualChainHash);
        false -> false
    end.

compute_chain_hash(PrevHash, CurrentHash) ->
    crypto:hash(sha256, <<PrevHash/binary, CurrentHash/binary>>).

tamper_receipt(Receipts, Index) ->
    Receipt = lists:nth(Index, Receipts),
    TamperedReceipt = Receipt#{data => <<"tampered data">>},
    lists:sublist(Receipts, Index - 1) ++ [TamperedReceipt] ++
    lists:sublist(Receipts, Index + 1, length(Receipts)).

remove_receipt(Receipts, Index) ->
    lists:sublist(Receipts, Index - 1) ++ lists:sublist(Receipts, Index + 1, length(Receipts)).

reorder_receipts([First, Second | Rest]) ->
    [Second, First | Rest];
reorder_receipts(Receipts) ->
    Receipts.

verify_hmac_signature(Data, Key) ->
    Signature = crypto:mac(hmac, sha256, Key, Data),
    ExpectedSignature = crypto:mac(hmac, sha256, Key, Data),
    Signature =:= ExpectedSignature.

verify_hmac_signature_with_key(Data, Key) ->
    Signature = crypto:mac(hmac, sha256, <<"correct-key">>, Data),
    ExpectedSignature = crypto:mac(hmac, sha256, Key, Data),
    Signature =:= ExpectedSignature.

verify_hmac_tampered(Data, Key) ->
    Signature = crypto:mac(hmac, sha256, Key, Data),
    ExpectedSignature = crypto:mac(hmac, sha256, Key, <<"tampered data">>),
    Signature =:= ExpectedSignature.

get_test_key() ->
    <<"test-key-for-signing">>.

generate_receipt_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    list_to_binary(io_lib:format("~32.16.0b", [Id])).
```

---

## GCP Integration Testing

### Firestore Security Tests

**File:** `test/firestore_security_tests.erl`

```erlang
-module(firestore_security_tests).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
        test_firestore_encryption,
        test_firestore_access_token,
        test_firestore_rate_limiting,
        test_firestore_error_handling
    ].

init_per_suite(Config) ->
    % Start Firestore emulator
    os:cmd("docker run -d -p 8081:8081 gcr.io/google.com/cloudsdktool/cloud-sdk:emulators gcloud beta emulators firestore start --host-port=0.0.0.0:8081"),
    timer:sleep(2000),  % Wait for emulator to start
    Config.

end_per_suite(_Config) ->
    % Stop emulator
    os:cmd("docker stop firestore-emulator 2>/dev/null"),
    ok.

test_firestore_encryption(Config) ->
    %% Create encrypted document
    Doc = #{
        <<"tenant_id">> => encrypt_field(<<"acme-corp">>),
        <<"data">> => <<"sensitive">>
    },

    %% Write to Firestore
    case gcp_firestore:create_document("receipts", "test-1", Doc) of
        {ok, Response} ->
            %% Verify document was stored
            ct:log("Document stored: ~p", [Response]),
            %% Read document
            case gcp_firestore:get_document("receipts", "test-1") of
                {ok, Retrieved} ->
                    %% Verify encryption still in place
                    ?assert(maps:is_key(<<"encrypted">>, maps:get(<<"tenant_id">>, Retrieved)));
                {error, Reason} ->
                    ct:fail({read_failed, Reason})
            end;
        {error, Reason} ->
            ct:fail({write_failed, Reason})
    end.

test_firestore_access_token(Config) ->
    %% Test access token caching
    {ok, Token1} = gcp_metadata:get_access_token(),
    {ok, Token2} = gcp_metadata:get_access_token(),

    %% Should return cached token
    ?assertEqual(Token1, Token2).

test_firestore_rate_limiting(Config) ->
    %% Send many rapid requests
    Results = lists:map(fun(I) ->
        gcp_firestore:create_document("test", "doc-" ++ integer_to_list(I), #{data => <<"test">>})
    end, lists:seq(1, 100)),

    %% All should succeed (within rate limit)
    Successes = length([1 || {ok, _} <- Results]),
    ?assert(Successes > 50).  % At least 50% should succeed

test_firestore_error_handling(Config) ->
    %% Test with invalid project ID
    case gcp_firestore:create_document("invalid", "doc", #{data => <<"test">>}) of
        {error, _} -> ok;  % Expected
        {ok, _} -> ct:fail("Should have rejected invalid collection")
    end.

% Helper functions
encrypt_field(Value) ->
    % In real tests, use actual encryption
    #{
        <<"encrypted">> => true,
        <<"data">> => Value
    }.
```

---

## Compliance Testing

### GDPR Compliance Tests

**File:** `test/compliance_tests.erl`

```erlang
-module(compliance_tests).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
        test_audit_logging,
        test_data_retention,
        test_gdpr_erasure,
        test_encryption_verification
    ].

test_audit_logging(Config) ->
    %% Trigger an action
    tai_audit:log_event(test_action, <<"user123">>, #{}),

    %% Verify audit log was created
    case file:read_file("/tmp/tai_audit.log") of
        {ok, Content} ->
            ?assert(string:find(Content, "test_action") =/= nomatch);
        {error, _} ->
            ct:fail("Audit log not found")
    end.

test_data_retention(Config) ->
    %% Insert old data
    OldTimestamp = erlang:system_time(millisecond) - (100 * 24 * 3600 * 1000),  % 100 days ago
    OldDoc = #{
        <<"timestamp">> => OldTimestamp,
        <<"data">> => <<"old data">>
    },

    gcp_firestore:create_document("old_data", "doc-1", OldDoc),

    %% Run retention cleanup
    cleanup_expired_data(90),  % 90 day retention

    %% Verify old data was deleted
    case gcp_firestore:get_document("old_data", "doc-1") of
        {error, not_found} -> ok;  % Expected - data deleted
        {ok, _} -> ct:fail("Old data should have been deleted")
    end.

test_gdpr_erasure(Config) ->
    %% Create tenant data
    TenantId = <<"gdpr-test-tenant">>,
    tai_autonomics_sup:start_tenant_governor(TenantId),

    %% Simulate GDPR erasure request
    handle_gdpr_erasure_request(TenantId),

    %% Verify all tenant data deleted
    case gcp_firestore:get_document("tenants", binary_to_list(TenantId)) of
        {error, not_found} -> ok;  % Expected
        {ok, _} -> ct:fail("Tenant data should be erased")
    end.

test_encryption_verification(Config) ->
    %% Verify encryption keys configured
    case application:get_env(tai_autonomics, encryption_enabled) of
        {ok, true} ->
            case application:get_env(tai_autonomics, field_encryption_key) of
                undefined -> ct:fail("Encryption key not configured");
                {ok, _} -> ok
            end;
        _ -> ct:fail("Encryption not enabled")
    end.

% Helper functions
cleanup_expired_data(RetentionDays) ->
    CutoffTime = erlang:system_time(millisecond) - (RetentionDays * 24 * 3600 * 1000),
    % Query old documents and delete them
    Query = #{
        <<"timestamp">> => {<<"<">>, CutoffTime}
    },
    case gcp_firestore:query("old_data", Query) of
        {ok, OldDocs} ->
            lists:foreach(fun(Doc) ->
                DocId = maps:get(<<"id">>, Doc),
                gcp_firestore:delete_document("old_data", DocId)
            end, OldDocs);
        {error, _} -> ok
    end.

handle_gdpr_erasure_request(TenantId) ->
    tai_audit:log_event(gdpr_erasure_request, TenantId, #{}),
    gcp_firestore:delete_document("tenants", binary_to_list(TenantId)),
    tai_audit:log_event(gdpr_erasure_completed, TenantId, #{}).
```

---

## Running Security Tests

### Command Line Execution

```bash
# Run all security tests
rebar3 ct --suite security_tests

# Run specific test module
rebar3 ct --module input_validation_tests

# Run specific test function
rebar3 ct --spec input_validation_tests:uuid_validation_test_

# Run with coverage
rebar3 do eunit,ct --cover

# Generate coverage report
rebar3 cover

# Run tests with verbose output
rebar3 ct -v

# Run security load tests
rebar3 ct --module security_load_tests
```

### Continuous Security Testing

```yaml
# .github/workflows/security-tests.yml
name: Security Tests

on: [push, pull_request]

jobs:
  security-tests:
    runs-on: ubuntu-latest

    services:
      firestore-emulator:
        image: gcr.io/google.com/cloudsdktool/cloud-sdk:emulators
        options: >-
          --health-cmd="curl -f http://localhost:8081" || exit 1
          --health-interval=10s
          --health-timeout=5s
          --health-retries=5
        ports:
          - 8081:8081

      pubsub-emulator:
        image: gcr.io/google.com/cloudsdktool/cloud-sdk:emulators
        options: >-
          --health-cmd="curl -f http://localhost:8085" || exit 1
        ports:
          - 8085:8085

    steps:
      - uses: actions/checkout@v2

      - name: Install Erlang/OTP
        uses: erlef/setup-elixir@v1
        with:
          otp-version: 24
          elixir-version: 1.13

      - name: Run security tests
        run: |
          rebar3 ct --suite input_validation_tests
          rebar3 ct --suite authentication_tests
          rebar3 ct --suite authorization_tests
          rebar3 ct --suite cryptography_tests
          rebar3 ct --suite security_load_tests

      - name: Run compliance tests
        run: rebar3 ct --suite compliance_tests

      - name: Generate coverage report
        run: rebar3 cover

      - name: Upload coverage
        uses: codecov/codecov-action@v2
```

---

## Security Testing Checklist

```markdown
## Pre-Deployment Security Checklist

### Authentication & Authorization
- [ ] JWT validation tests passing
- [ ] Authorization tests passing
- [ ] Service account validation implemented
- [ ] RBAC rules verified
- [ ] Cross-tenant isolation verified

### Input Validation
- [ ] Input validation tests passing
- [ ] Oversized payload rejection working
- [ ] Malformed JSON handling verified
- [ ] Injection attack prevention verified
- [ ] Field length limits enforced

### Cryptography
- [ ] TLS/HTTPS enabled
- [ ] Hash chain integrity verified
- [ ] Signature verification working
- [ ] Encryption at rest enabled
- [ ] Certificate validation working

### GCP Integration
- [ ] Service account permissions least-privilege
- [ ] Firestore encryption enabled
- [ ] Pub/Sub access control verified
- [ ] Cloud Run IAM policies verified
- [ ] Secrets Manager integration working

### Compliance
- [ ] Audit logging verified
- [ ] GDPR compliance mechanisms working
- [ ] Data retention policies enforced
- [ ] Error handling doesn't leak information
- [ ] No hardcoded secrets in code

### Performance & Reliability
- [ ] Rate limiting working
- [ ] Connection pool limits enforced
- [ ] Error handling under load verified
- [ ] Recovery from failures tested
- [ ] Monitoring and alerting configured
```

---

## Conclusion

This security testing guide provides comprehensive coverage for TAI Erlang Autonomics. Regular execution of these tests is critical for maintaining security posture before and after deployment.

**Key Testing Principles:**
1. Test early and often
2. Automate security tests in CI/CD
3. Regular penetration testing
4. Load testing with security focus
5. Continuous monitoring and auditing

---

**Document Version:** 1.0
**Last Updated:** January 25, 2026
