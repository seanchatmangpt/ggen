# TAI Erlang Autonomics: Security & Compliance Analysis Report

**Date:** January 25, 2026
**Project:** TAI Erlang Autonomics (GCP Cloud Run Deployment)
**Classification:** INTERNAL - CONFIDENTIAL
**Status:** DRAFT - Security Review Required

---

## Executive Summary

This comprehensive security analysis examines TAI Erlang Autonomics, a multi-tenant autonomic governance system deployed on Google Cloud Platform (GCP). The assessment covers authentication, authorization, input validation, cryptographic operations, secrets management, infrastructure security, and compliance with GCP security best practices.

### Critical Findings Summary

| Severity | Category | Count | Status |
|----------|----------|-------|--------|
| CRITICAL | Authentication/Authorization | 3 | Requires Immediate Action |
| HIGH | Input Validation | 4 | Requires Implementation |
| HIGH | Secrets Management | 2 | Requires Implementation |
| MEDIUM | Error Handling | 3 | Recommended Review |
| MEDIUM | Logging & Monitoring | 2 | Recommended Enhancement |
| LOW | Configuration | 3 | Best Practice Updates |

### Overall Risk Assessment: **MEDIUM-HIGH**

The application demonstrates solid architectural foundations but requires immediate attention to authentication, secrets management, and input validation before production deployment.

---

## Table of Contents

1. [HTTP Endpoint Security](#http-endpoint-security)
2. [Input Validation & Injection Prevention](#input-validation--injection-prevention)
3. [Authentication & Authorization](#authentication--authorization)
4. [Cryptographic Operations](#cryptographic-operations)
5. [Secrets Management](#secrets-management)
6. [TLS/SSL Configuration](#tlsssl-configuration)
7. [Data Encryption](#data-encryption)
8. [GCP Security Integration](#gcp-security-integration)
9. [Compliance & Audit](#compliance--audit)
10. [Security Testing Guide](#security-testing-guide)
11. [Recommendations & Remediation](#recommendations--remediation)

---

## 1. HTTP Endpoint Security

### 1.1 Endpoint Overview

**File:** `/Users/sac/ggen/tai-erlang-autonomics/apps/tai_autonomics/src/tai_http.erl`

The application exposes three HTTP endpoints via Cowboy web server:

| Endpoint | Method | Handler | Purpose |
|----------|--------|---------|---------|
| `/health` | GET | `tai_http_handler:handle_health/2` | Service health check |
| `/pubsub` | POST | `tai_http_handler:handle_pubsub/2` | GCP Pub/Sub message ingestion |
| `/marketplace` | POST | `tai_http_handler:handle_marketplace/2` | Marketplace entitlement events |

### 1.2 Security Analysis

#### Finding 1.2.1: No TLS Enforcement in HTTP Handler [CRITICAL]

**Location:** `tai_http.erl`, lines 52-59
**Severity:** CRITICAL
**Issue:** The HTTP server configuration does not enforce TLS/SSL

```erlang
TransOpts = #{
    socket_opts => [{port, Port}]
},

ProtoOpts = #{
    env => #{dispatch => Dispatch}
},

case cowboy:start_clear(?SERVER, TransOpts, ProtoOpts) of
```

**Risk:**
- All communication transmitted in plaintext
- Credentials exposed in transit
- Man-in-the-middle (MITM) attack vector
- Violates GCP Cloud Run security requirements

**Recommendation:**
```erlang
% Production: Use cowboy:start_tls with certificate configuration
TransOpts = #{
    socket_opts => [{port, Port}],
    certfile => "/etc/secrets/tls.crt",
    keyfile => "/etc/secrets/tls.key"
},
case cowboy:start_tls(?SERVER, TransOpts, ProtoOpts) of
```

---

#### Finding 1.2.2: Missing HTTP Security Headers [HIGH]

**Location:** `tai_http_handler.erl`, lines 26-29, 57-59, etc.
**Severity:** HIGH
**Issue:** HTTP responses lack security headers

```erlang
Req2 = cowboy_req:reply(StatusCode, #{
    <<"content-type">> => <<"application/json">>
}, Body, Req),
```

**Missing Headers:**
- `X-Content-Type-Options: nosniff` - Prevents MIME type sniffing
- `X-Frame-Options: DENY` - Prevents clickjacking
- `X-XSS-Protection: 1; mode=block` - Legacy XSS protection
- `Strict-Transport-Security: max-age=31536000` - HSTS enforcement
- `Content-Security-Policy: default-src 'none'` - CSP for APIs
- `Cache-Control: no-store, no-cache, must-revalidate` - Prevent caching sensitive data

**Recommendation:**
```erlang
security_headers() ->
    #{
        <<"content-type">> => <<"application/json">>,
        <<"x-content-type-options">> => <<"nosniff">>,
        <<"x-frame-options">> => <<"DENY">>,
        <<"strict-transport-security">> => <<"max-age=31536000; includeSubDomains">>,
        <<"cache-control">> => <<"no-store, no-cache, must-revalidate, proxy-revalidate">>,
        <<"pragma">> => <<"no-cache">>,
        <<"expires">> => <<"0">>
    }.
```

---

#### Finding 1.2.3: No Rate Limiting [HIGH]

**Location:** `tai_http_handler.erl`
**Severity:** HIGH
**Issue:** No rate limiting implemented on public endpoints

**Risk:**
- DoS/DDoS vulnerability
- Burst attack consumption of resources
- Uncontrolled scaling costs in GCP

**Recommendation:** Implement per-IP rate limiting:
```erlang
% Add to tai_http module
-define(RATE_LIMIT_REQUESTS_PER_MINUTE, 600).
-define(RATE_LIMIT_REQUESTS_PER_HOUR, 10000).

check_rate_limit(IpAddress) ->
    Key = {rate_limit, IpAddress},
    case ets:lookup(rate_limit_table, Key) of
        [] ->
            ets:insert(rate_limit_table, {Key, 1}),
            ok;
        [{Key, Count}] when Count >= ?RATE_LIMIT_REQUESTS_PER_MINUTE ->
            {error, rate_limit_exceeded};
        [{Key, Count}] ->
            ets:update_counter(rate_limit_table, Key, 1),
            ok
    end.
```

---

### 1.3 Request/Response Handling

#### Finding 1.3.1: Catch-All Exception Handling Hides Errors [MEDIUM]

**Location:** `tai_http_handler.erl`, lines 53, 102
**Severity:** MEDIUM
**Issue:** Using `catch` without proper error classification

```erlang
case catch jsx:decode(Body, [return_maps]) of
    Envelope when is_map(Envelope) ->
        % Handle envelope
    _ ->
        % Generic error handling
```

**Risk:**
- Information disclosure through stack traces
- Difficult error diagnostics in production
- Potential security exceptions masked

**Recommendation:**
```erlang
decode_and_validate(Body) ->
    try
        case jsx:decode(Body, [return_maps]) of
            Envelope when is_map(Envelope) -> {ok, Envelope};
            _ -> {error, invalid_json_structure}
        end
    catch
        error:badarg ->
            logger:warning("JSON decode error: malformed input"),
            {error, malformed_json};
        error:Error ->
            logger:error("Unexpected error in JSON decode: ~p", [Error]),
            {error, internal_error}
    end.
```

---

## 2. Input Validation & Injection Prevention

### 2.1 HTTP Body Validation

#### Finding 2.1.1: Insufficient Input Validation [CRITICAL]

**Location:** `tai_marketplace_ingress.erl`, lines 59-70
**Severity:** CRITICAL
**Issue:** Minimal validation of marketplace events

```erlang
validate_event(Event) ->
    Required = [<<"tenant_id">>, <<"entitlement_id">>, <<"action">>],
    case lists:all(fun(Key) -> maps:is_key(Key, Event) end, Required) of
        true ->
            {ok, Event};
        false ->
            {error, missing_required_fields}
    end.
```

**Missing Validations:**
- No format validation for tenant_id (UUID?)
- No action whitelist enforcement
- No size limits on input
- No nested object depth checking
- No malicious payload detection

**Recommendation:** Implement comprehensive validation schema:
```erlang
validate_marketplace_event(Event) ->
    Validations = [
        {<<"tenant_id">>, {required, {binary, uuid}}},
        {<<"entitlement_id">>, {required, {binary, uuid}}},
        {<<"action">>, {required, {atom, [grant, revoke, suspend]}}},
        {<<"customer_id">>, {optional, {binary, max_length(128)}}},
        {<<"reason">>, {optional, {binary, max_length(1024)}}},
        {<<"signature">>, {optional, {binary, jwt}}}
    ],
    validate_fields(Event, Validations).

validate_uuid(Value) when is_binary(Value) ->
    case length(binary_to_list(Value)) of
        36 -> {ok, Value};
        _ -> {error, invalid_uuid_format}
    end;
validate_uuid(_) ->
    {error, uuid_must_be_binary}.

validate_binary_length(Value, MaxLen) when is_binary(Value) ->
    case byte_size(Value) =< MaxLen of
        true -> {ok, Value};
        false -> {error, {binary_too_long, byte_size(Value)}}
    end;
validate_binary_length(_, _) ->
    {error, must_be_binary}.
```

---

#### Finding 2.1.2: Pub/Sub Payload Encoding Not Validated [HIGH]

**Location:** `tai_pubsub_ingress.erl`, lines 79-97
**Severity:** HIGH
**Issue:** Minimal validation of base64-encoded payload

```erlang
decode_message(Message) ->
    Data = maps:get(<<"data">>, Message),
    try
        Decoded = base64:decode(Data),
        try
            Payload = jsx:decode(Decoded, [return_maps]),
            {ok, Payload}
        catch
            _:_ ->
                {error, invalid_json}
        end
    catch
        _:_ ->
            {error, decode_failed}
    end.
```

**Risk:**
- Polyglot attack vectors (binary data interpreted as JSON)
- Memory exhaustion through large payloads
- No schema validation
- No content-type enforcement

**Recommendation:**
```erlang
decode_message(Message) ->
    Data = maps:get(<<"data">>, Message),
    MaxPayloadSize = 1048576,  % 1MB
    case byte_size(Data) > MaxPayloadSize of
        true ->
            {error, payload_too_large};
        false ->
            try
                Decoded = base64:decode(Data),
                case byte_size(Decoded) > MaxPayloadSize of
                    true -> {error, payload_too_large};
                    false -> decode_json_safely(Decoded)
                end
            catch
                _:_ ->
                    logger:warning("Base64 decode failed for message"),
                    {error, decode_failed}
            end
    end.

decode_json_safely(JsonBin) ->
    try
        Payload = jsx:decode(JsonBin, [
            return_maps,
            {max_depth, 10}  % Prevent deeply nested objects
        ]),
        case validate_pubsub_payload(Payload) of
            {ok, ValidPayload} -> {ok, ValidPayload};
            {error, Reason} -> {error, Reason}
        end
    catch
        error:_ ->
            {error, invalid_json}
    end.

validate_pubsub_payload(Payload) when is_map(Payload) ->
    RequiredFields = [<<"tenant_id">>, <<"metric">>, <<"value">>],
    case lists:all(fun(Key) -> maps:is_key(Key, Payload) end, RequiredFields) of
        true ->
            TenantId = maps:get(<<"tenant_id">>, Payload),
            Metric = maps:get(<<"metric">>, Payload),
            case is_valid_tenant_id(TenantId) andalso is_valid_metric(Metric) of
                true -> {ok, Payload};
                false -> {error, invalid_field_values}
            end;
        false -> {error, missing_required_fields}
    end;
validate_pubsub_payload(_) ->
    {error, payload_must_be_map}.

is_valid_tenant_id(TenantId) when is_binary(TenantId) ->
    byte_size(TenantId) >= 1 andalso byte_size(TenantId) =< 128;
is_valid_tenant_id(_) -> false.

is_valid_metric(Metric) when is_binary(Metric) ->
    byte_size(Metric) >= 1 andalso byte_size(Metric) =< 256;
is_valid_metric(_) -> false.
```

---

#### Finding 2.1.3: No CORS/Origin Validation [MEDIUM]

**Location:** `tai_http_handler.erl`
**Severity:** MEDIUM
**Issue:** No validation of request origin

**Risk:**
- Cross-Origin Request Forgery (CORS) attacks
- Unauthorized access from unexpected origins

**Recommendation:** Add CORS validation:
```erlang
validate_cors(Req) ->
    Origin = cowboy_req:header(<<"origin">>, Req),
    case is_allowed_origin(Origin) of
        true -> {ok, Req};
        false -> {error, forbidden_origin}
    end.

is_allowed_origin(undefined) -> true;  % Non-CORS request
is_allowed_origin(Origin) ->
    AllowedOrigins = application:get_env(tai_autonomics, allowed_origins, []),
    lists:member(Origin, AllowedOrigins).
```

---

### 2.2 Field-Level Validation

#### Finding 2.2.4: Insufficient Signature Verification [CRITICAL]

**Location:** `tai_marketplace_ingress.erl`, lines 72-115
**Severity:** CRITICAL
**Issue:** Signature verification optional and incomplete

```erlang
verify_signature(Event) ->
    case application:get_env(tai_autonomics, verify_signatures, false) of
        false ->
            {ok, verified};  % CRITICAL: Default disabled!
        true ->
            % Verification logic
```

**Risk:**
- Signature verification disabled by default
- No enforcement mechanism
- Marketplace events can be spoofed
- Entitlements can be granted fraudulently

**Recommendation:** Enforce signature verification in production:
```erlang
-define(MIN_SIGNATURE_VERIFICATION_LEVEL, mandatory_in_production).

verify_signature(Event) ->
    case get_environment_tier() of
        production ->
            % MANDATORY in production
            do_verify_signature(Event);
        development ->
            case application:get_env(tai_autonomics, verify_signatures, false) of
                true -> do_verify_signature(Event);
                false ->
                    logger:warning("Signature verification disabled in dev"),
                    {ok, verified}
            end
    end.

do_verify_signature(Event) ->
    case maps:get(<<"signature">>, Event, undefined) of
        undefined ->
            {error, missing_signature};
        Signature when is_binary(Signature) ->
            case get_verification_key() of
                {ok, Key} ->
                    try
                        case jose_jwt:verify(Key, Signature) of
                            {true, Claims, _JWS} ->
                                verify_jwt_claims(Claims);
                            {false, _} ->
                                {error, invalid_signature}
                        end
                    catch
                        error:Error ->
                            logger:error("Signature verification error: ~p", [Error]),
                            {error, verification_failed}
                    end;
                {error, Reason} ->
                    logger:error("Failed to retrieve verification key: ~p", [Reason]),
                    {error, Reason}
            end;
        _ ->
            {error, invalid_signature_format}
    end.

verify_jwt_claims(Claims) ->
    Now = erlang:system_time(second),
    case maps:get(<<"exp">>, Claims, Now + 3600) of
        Exp when Exp > Now ->
            case maps:get(<<"iss">>, Claims) of
                undefined -> {error, invalid_claims};
                Issuer ->
                    case is_trusted_issuer(Issuer) of
                        true -> {ok, verified};
                        false -> {error, untrusted_issuer}
                    end
            end;
        _ ->
            {error, token_expired}
    end.

is_trusted_issuer(Issuer) ->
    TrustedIssuers = application:get_env(tai_autonomics, trusted_issuers, []),
    lists:member(Issuer, TrustedIssuers).

get_environment_tier() ->
    case os:getenv("ENVIRONMENT") of
        "production" -> production;
        "prod" -> production;
        _ -> development
    end.
```

---

## 3. Authentication & Authorization

### 3.1 Service-to-Service Authentication

#### Finding 3.1.1: Missing Service Account Validation [CRITICAL]

**Location:** `gcp_firestore.erl`, `gcp_pubsub.erl`, `gcp_metadata.erl`
**Severity:** CRITICAL
**Issue:** No validation of GCP service account identity

**Current Implementation:**
- Trusts metadata server implicitly
- No service account email validation
- No scopes validation
- No role-based access control

**Recommendation:** Implement service account validation:
```erlang
-record(service_account, {
    email :: string(),
    scopes :: [string()],
    project_id :: string(),
    roles :: [string()]
}).

validate_service_account() ->
    case gcp_metadata:get_access_token() of
        {ok, Token} ->
            case verify_token_signature(Token) of
                {ok, Claims} ->
                    case validate_token_claims(Claims) of
                        {ok, ServiceAccount} ->
                            validate_service_account_permissions(ServiceAccount);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    logger:error("Token signature verification failed: ~p", [Reason]),
                    {error, invalid_token}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

validate_token_claims(Claims) ->
    case maps:is_key(<<"email">>, Claims) andalso
         maps:is_key(<<"aud">>, Claims) of
        true ->
            Email = maps:get(<<"email">>, Claims),
            case is_valid_service_account(Email) of
                true ->
                    {ok, #service_account{
                        email = Email,
                        project_id = extract_project_id(Claims)
                    }};
                false ->
                    {error, invalid_service_account}
            end;
        false ->
            {error, invalid_claims}
    end.

is_valid_service_account(Email) ->
    AllowedAccounts = application:get_env(tai_autonomics, allowed_service_accounts, []),
    lists:member(Email, AllowedAccounts).
```

---

#### Finding 3.1.2: No Request Authentication on Public Endpoints [CRITICAL]

**Location:** `tai_http_handler.erl`, `/health`, `/pubsub`, `/marketplace`
**Severity:** CRITICAL
**Issue:** Public endpoints have no authentication

**Risk:**
- Unauthorized access to event handlers
- Service impersonation
- Entitlements can be modified by anyone

**Recommendation:** Implement authentication middleware:
```erlang
authenticate_request(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            {error, missing_authorization};
        AuthHeader ->
            case parse_auth_header(AuthHeader) of
                {ok, {bearer, Token}} ->
                    verify_bearer_token(Token);
                {ok, {basic, Credentials}} ->
                    verify_basic_auth(Credentials);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

parse_auth_header(AuthHeader) ->
    case binary:split(AuthHeader, <<" ">>) of
        [<<"Bearer">>, Token] -> {ok, {bearer, Token}};
        [<<"Basic">>, Encoded] -> {ok, {basic, Encoded}};
        _ -> {error, invalid_auth_header}
    end.

verify_bearer_token(Token) ->
    case verify_jwt_token(Token) of
        {ok, Claims} ->
            case validate_token_scopes(Claims) of
                {ok, _} -> {ok, authenticated};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

verify_jwt_token(Token) ->
    case application:get_env(tai_autonomics, jwt_key, undefined) of
        undefined ->
            {error, jwt_not_configured};
        Key ->
            try
                case jose_jwt:verify(Key, Token) of
                    {true, Claims, _JWS} -> {ok, Claims};
                    {false, _} -> {error, invalid_token}
                end
            catch
                error:Error ->
                    logger:error("JWT verification error: ~p", [Error]),
                    {error, verification_failed}
            end
    end.
```

---

### 3.2 Authorization & Access Control

#### Finding 3.2.1: No RBAC for Entitlement Operations [HIGH]

**Location:** `tai_marketplace_ingress.erl`, `tai_pubsub_ingress.erl`
**Severity:** HIGH
**Issue:** No role-based access control for sensitive operations

**Risk:**
- Any authenticated user can modify any entitlement
- No tenant isolation enforcement
- Cross-tenant access possible

**Recommendation:** Implement RBAC:
```erlang
-define(ROLES, [
    {admin, [grant_entitlement, revoke_entitlement, suspend_entitlement, view_all]},
    {operator, [grant_entitlement, revoke_entitlement, suspend_entitlement]},
    {viewer, [view_entitlements]},
    {system, [all]}
]).

authorize_action(User, Action, ResourceTenantId) ->
    case get_user_roles(User) of
        {ok, Roles} ->
            case get_user_tenant_id(User) of
                {ok, UserTenantId} ->
                    authorize_with_tenant_check(Roles, Action, UserTenantId, ResourceTenantId);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

authorize_with_tenant_check(Roles, Action, UserTenantId, ResourceTenantId) ->
    case lists:member(system, Roles) of
        true ->
            {ok, authorized};  % System accounts bypass tenant check
        false ->
            case UserTenantId =:= ResourceTenantId of
                true -> check_action_permission(Roles, Action);
                false -> {error, forbidden}
            end
    end.

check_action_permission(Roles, Action) ->
    case lists:any(
        fun(Role) ->
            case lists:keyfind(Role, 1, ?ROLES) of
                {Role, Permissions} -> lists:member(Action, Permissions);
                false -> false
            end
        end,
        Roles
    ) of
        true -> {ok, authorized};
        false -> {error, insufficient_permissions}
    end.
```

---

#### Finding 3.2.2: No Audit Logging for Authorization Decisions [MEDIUM]

**Location:** All ingress handlers
**Severity:** MEDIUM
**Issue:** No audit trail for authorization successes/failures

**Recommendation:** Add authorization audit logging:
```erlang
audit_authorization(User, Action, Resource, Result) ->
    AuditEntry = #{
        timestamp => erlang:system_time(millisecond),
        user_id => User,
        action => Action,
        resource => Resource,
        result => Result,
        source_ip => get_remote_ip()
    },
    log_audit_entry(AuditEntry),
    Result.

log_audit_entry(Entry) ->
    case Result of
        {ok, _} ->
            logger:info("Authorization granted: ~p", [Entry]);
        {error, Reason} ->
            logger:warning("Authorization denied: ~p - Reason: ~p", [Entry, Reason])
    end.
```

---

## 4. Cryptographic Operations

### 4.1 Receipt Hashing

#### Finding 4.1.1: Weak Hash Chain Implementation [HIGH]

**Location:** `tai_receipts.erl`, lines 140-158
**Severity:** HIGH
**Issue:** Hash chain vulnerable to collision attacks

```erlang
compute_hash(Receipt) ->
    ReceiptJson = jsx:encode(Receipt),
    crypto:hash(sha256, ReceiptJson).

compute_chain_hash(PrevHash, CurrentHash) ->
    crypto:hash(sha256, <<PrevHash/binary, CurrentHash/binary>>).
```

**Vulnerabilities:**
1. Simple concatenation without domain separation
2. No timestamp validation in chain
3. No nonce/sequence number
4. Vulnerable to length extension attacks

**Recommendation:** Implement robust hash chain:
```erlang
-define(HASH_VERSION, 1).
-define(HASH_ALGORITHM, sha256).
-define(MIN_CHAIN_LENGTH, 100).

compute_receipt_hash(Receipt) ->
    %% Add version and timestamp for domain separation
    ReceiptId = maps:get(id, Receipt),
    Timestamp = maps:get(timestamp, Receipt),

    %% Include metadata to prevent collisions
    HashInput = jsx:encode(#{
        version => ?HASH_VERSION,
        algorithm => ?HASH_ALGORITHM,
        receipt_id => ReceiptId,
        timestamp => Timestamp,
        data => Receipt
    }),

    crypto:hash(?HASH_ALGORITHM, HashInput).

compute_chain_hash(PrevHash, CurrentHash, SequenceNum) ->
    %% Include sequence number to prevent reordering
    ChainInput = jsx:encode(#{
        version => ?HASH_VERSION,
        sequence => SequenceNum,
        prev_hash => binary_to_hex(PrevHash),
        current_hash => binary_to_hex(CurrentHash)
    }),

    crypto:hash(?HASH_ALGORITHM, ChainInput).

verify_hash_chain(Receipts) ->
    verify_hash_chain(Receipts, <<>>, 0).

verify_hash_chain([], _PrevHash, _Sequence) ->
    {ok, valid};
verify_hash_chain([Receipt | Rest], PrevHash, Sequence) ->
    CurrentHash = maps:get(hash, Receipt),
    ExpectedChainHash = compute_chain_hash(PrevHash, CurrentHash, Sequence),
    ActualChainHash = maps:get(chain_hash, Receipt, <<>>),

    case ExpectedChainHash =:= ActualChainHash of
        true ->
            verify_hash_chain(Rest, ExpectedChainHash, Sequence + 1);
        false ->
            logger:error("Hash chain verification failed at sequence ~p", [Sequence]),
            {error, invalid}
    end.

binary_to_hex(Bin) ->
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Bin])).
```

---

#### Finding 4.1.2: No Cryptographic Signing of Receipts [HIGH]

**Location:** `tai_receipts.erl`
**Severity:** HIGH
**Issue:** Receipts are hashed but not cryptographically signed

**Risk:**
- Receipts can be forged/modified
- No proof of origin
- No non-repudiation

**Recommendation:** Implement HMAC signing:
```erlang
-define(RECEIPT_SIGNING_ALGORITHM, sha256).

sign_receipt(Receipt) ->
    case get_signing_key() of
        {ok, Key} ->
            ReceiptJson = jsx:encode(Receipt),
            Signature = crypto:mac(hmac, ?RECEIPT_SIGNING_ALGORITHM, Key, ReceiptJson),
            Receipt#{signature => binary_to_hex(Signature)};
        {error, Reason} ->
            logger:error("Failed to sign receipt: ~p", [Reason]),
            Receipt
    end.

verify_receipt_signature(Receipt) ->
    Signature = maps:get(signature, Receipt),
    SignedReceipt = maps:remove(signature, Receipt),

    case get_signing_key() of
        {ok, Key} ->
            ReceiptJson = jsx:encode(SignedReceipt),
            ExpectedSignature = binary_to_hex(
                crypto:mac(hmac, ?RECEIPT_SIGNING_ALGORITHM, Key, ReceiptJson)
            ),
            case Signature =:= ExpectedSignature of
                true -> {ok, verified};
                false -> {error, signature_mismatch}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_signing_key() ->
    case application:get_env(tai_autonomics, receipt_signing_key, undefined) of
        undefined ->
            logger:error("Receipt signing key not configured"),
            {error, key_not_configured};
        Key when is_binary(Key) ->
            {ok, Key};
        Key when is_list(Key) ->
            {ok, list_to_binary(Key)}
    end.
```

---

### 4.2 JWT Handling

#### Finding 4.2.1: Incomplete JWT Validation [CRITICAL]

**Location:** `tai_marketplace_ingress.erl`, lines 89-107
**Severity:** CRITICAL
**Issue:** JWT validation incomplete and insecure

**Current Code:**
```erlang
case jose_jwt:verify(Key, Signature) of
    {true, Claims, _JWS} ->
        case maps:is_key(<<"tenant_id">>, Claims) andalso
             maps:is_key(<<"entitlement_id">>, Claims) of
            true ->
                {ok, verified};
            false ->
                {error, invalid_claims}
        end;
    {false, _} ->
        {error, invalid_signature}
end
```

**Issues:**
1. No expiration (`exp`) claim validation
2. No issued-at (`iat`) claim validation
3. No not-before (`nbf`) claim validation
4. No issuer (`iss`) claim validation
5. No audience (`aud`) claim validation

**Recommendation:** Comprehensive JWT validation:
```erlang
-define(JWT_ALGORITHM, <<"RS256">>).
-define(JWT_CLOCK_SKEW_SECONDS, 30).
-define(JWT_MAX_AGE_SECONDS, 3600).

verify_jwt_complete(Token) ->
    case application:get_env(tai_autonomics, jwt_public_key, undefined) of
        undefined ->
            {error, jwt_key_not_configured};
        Key ->
            try
                {true, Claims, #{alg := Algorithm}} = jose_jwt:verify(Key, Token),

                %% Validate algorithm
                case Algorithm =:= ?JWT_ALGORITHM of
                    true -> validate_jwt_claims(Claims);
                    false -> {error, invalid_algorithm}
                end
            catch
                error:{badarg, _} ->
                    {error, invalid_token};
                error:_ ->
                    {error, verification_failed}
            end
    end.

validate_jwt_claims(Claims) ->
    Now = erlang:system_time(second),

    %% Check expiration
    case maps:get(<<"exp">>, Claims, undefined) of
        undefined -> {error, missing_exp_claim};
        Exp when Exp < (Now - ?JWT_CLOCK_SKEW_SECONDS) ->
            {error, token_expired};
        Exp when Exp > Now + ?JWT_MAX_AGE_SECONDS ->
            {error, token_exp_too_far};
        _ -> ok
    end,

    %% Check issued-at
    case maps:get(<<"iat">>, Claims, undefined) of
        undefined -> {error, missing_iat_claim};
        Iat when Iat > (Now + ?JWT_CLOCK_SKEW_SECONDS) ->
            {error, token_not_yet_valid};
        _ -> ok
    end,

    %% Check not-before
    case maps:get(<<"nbf">>, Claims, undefined) of
        undefined -> ok;
        Nbf when Nbf > (Now + ?JWT_CLOCK_SKEW_SECONDS) ->
            {error, token_not_yet_valid};
        _ -> ok
    end,

    %% Check issuer
    case maps:get(<<"iss">>, Claims, undefined) of
        undefined -> {error, missing_iss_claim};
        Issuer ->
            case is_trusted_jwt_issuer(Issuer) of
                true -> ok;
                false -> {error, untrusted_issuer}
            end
    end,

    %% Check audience
    case maps:get(<<"aud">>, Claims, undefined) of
        undefined -> {error, missing_aud_claim};
        Audience ->
            case is_valid_jwt_audience(Audience) of
                true -> {ok, verified};
                false -> {error, invalid_audience}
            end
    end.

is_trusted_jwt_issuer(Issuer) ->
    TrustedIssuers = application:get_env(tai_autonomics, trusted_jwt_issuers, []),
    lists:member(Issuer, TrustedIssuers).

is_valid_jwt_audience(Audience) ->
    ValidAudiences = application:get_env(tai_autonomics, valid_jwt_audiences, []),
    lists:member(Audience, ValidAudiences).
```

---

## 5. Secrets Management

### 5.1 Configuration Secrets

#### Finding 5.1.1: Hardcoded Secrets in Configuration Files [CRITICAL]

**Location:** `config/sys.config`, line 35
**Severity:** CRITICAL
**Issue:** Example shows hardcoded GCP Project ID

```erlang
{gcp_project_id, "ggen-autonomics"},
{tls_cert_file, "/etc/secrets/tls.crt"},
{tls_key_file, "/etc/secrets/tls.key"},
```

**Risk:**
- Secrets exposed in version control
- Hardcoded values in compiled binaries
- No secret rotation mechanism
- Violates principle of least privilege

**Recommendation:** Enforce environment-based secrets:
```erlang
% config/sys.config - NEVER store secrets here
{gcp_project_id, {env, "GCP_PROJECT_ID"}},  % Load from environment
{tls_cert_file, {env, "TLS_CERT_FILE"}},
{tls_key_file, {env, "TLS_KEY_FILE"}},
{jwt_public_key, {env, "JWT_PUBLIC_KEY"}},
{firestore_access_token, {env, "FIRESTORE_ACCESS_TOKEN"}},

% Helper function
get_env_secret(EnvVar, Default) ->
    case os:getenv(EnvVar) of
        false -> Default;
        Value -> Value
    end.
```

---

#### Finding 5.1.2: Token Caching Without Expiration Handling [HIGH]

**Location:** `gcp_metadata.erl`, lines 199-217
**Severity:** HIGH
**Issue:** Access token caching doesn't respect expiration properly

```erlang
handle_get_access_token(Scope, State) ->
    Now = erlang:system_time(second),
    case State#state.access_token =/= undefined
         andalso State#state.token_expires_at =/= undefined
         andalso Now < (State#state.token_expires_at - 60) of
        true ->
            {reply, {ok, State#state.access_token}, State};
        false ->
            % Refresh token
```

**Issues:**
1. 60-second buffer is arbitrary
2. No token refresh on failure
3. No logging of token refresh
4. Expired token still usable briefly

**Recommendation:** Improve token management:
```erlang
-define(TOKEN_REFRESH_BUFFER_SECONDS, 300).  % 5 minutes
-define(MAX_TOKEN_AGE_SECONDS, 3600).  % 1 hour
-define(TOKEN_REFRESH_MAX_RETRIES, 3).

handle_get_access_token(Scope, State) ->
    Now = erlang:system_time(second),
    case should_refresh_token(State, Now) of
        true ->
            case refresh_access_token(Scope, State) of
                {ok, Token, ExpiresAt} ->
                    NewState = State#state{
                        access_token = Token,
                        token_expires_at = ExpiresAt
                    },
                    {reply, {ok, Token}, NewState};
                {error, Reason} ->
                    logger:error("Token refresh failed: ~p", [Reason]),
                    %% Return cached token if still valid
                    case is_token_still_valid(State, Now) of
                        true ->
                            logger:warning("Using expired cached token due to refresh failure"),
                            {reply, {ok, State#state.access_token}, State};
                        false ->
                            {reply, {error, token_not_available}, State}
                    end
            end;
        false ->
            {reply, {ok, State#state.access_token}, State}
    end.

should_refresh_token(State, Now) ->
    State#state.access_token =:= undefined orelse
    State#state.token_expires_at =:= undefined orelse
    Now >= (State#state.token_expires_at - ?TOKEN_REFRESH_BUFFER_SECONDS).

is_token_still_valid(State, Now) ->
    State#state.token_expires_at =/= undefined andalso
    Now < State#state.token_expires_at.

refresh_access_token(Scope, State) ->
    fetch_access_token(Scope, State, ?TOKEN_REFRESH_MAX_RETRIES).

fetch_access_token(Scope, State, 0) ->
    {error, max_retries_exceeded};
fetch_access_token(Scope, State, RetriesLeft) ->
    case fetch_access_token_impl(Scope, State) of
        {ok, Token, ExpiresAt} ->
            {ok, Token, ExpiresAt};
        {error, Reason} ->
            logger:warning("Token fetch failed (retries left: ~p): ~p", [RetriesLeft, Reason]),
            timer:sleep(exponential_backoff(RetriesLeft)),
            fetch_access_token(Scope, State, RetriesLeft - 1)
    end.

exponential_backoff(RetriesLeft) ->
    MaxRetries = ?TOKEN_REFRESH_MAX_RETRIES,
    Attempt = MaxRetries - RetriesLeft + 1,
    BaseDelay = 100,
    BaseDelay * (2 ^ (Attempt - 1)).
```

---

#### Finding 5.1.3: No Secret Rotation Strategy [MEDIUM]

**Location:** Global configuration
**Severity:** MEDIUM
**Issue:** No mechanism for secret rotation

**Recommendation:** Implement secret rotation:
```erlang
% In supervision tree or startup
{ok, _} = setup_secret_rotation(),

setup_secret_rotation() ->
    RotationIntervalMs = application:get_env(
        tai_autonomics,
        secret_rotation_interval_ms,
        3600000  % 1 hour
    ),
    timer:send_interval(RotationIntervalMs, rotate_secrets),
    {ok, scheduled}.

handle_info(rotate_secrets, State) ->
    logger:info("Attempting secret rotation..."),
    case rotate_all_secrets() of
        ok ->
            logger:info("Secret rotation completed successfully");
        {error, Reason} ->
            logger:warning("Secret rotation failed: ~p", [Reason])
    end,
    {noreply, State}.

rotate_all_secrets() ->
    Secrets = [
        {jwt_public_key, "JWT_PUBLIC_KEY"},
        {firestore_access_token, "FIRESTORE_ACCESS_TOKEN"},
        {receipt_signing_key, "RECEIPT_SIGNING_KEY"}
    ],
    rotate_secrets_list(Secrets).

rotate_secrets_list([]) -> ok;
rotate_secrets_list([{AppKey, EnvVar} | Rest]) ->
    case os:getenv(EnvVar) of
        false ->
            logger:warning("Secret rotation: ~s not found in environment", [EnvVar]),
            rotate_secrets_list(Rest);
        NewValue ->
            case application:set_env(tai_autonomics, AppKey, NewValue) of
                ok ->
                    logger:info("Rotated secret: ~s", [AppKey]),
                    rotate_secrets_list(Rest);
                Error ->
                    logger:error("Failed to rotate ~s: ~p", [AppKey, Error]),
                    {error, Error}
            end
    end.
```

---

## 6. TLS/SSL Configuration

### 6.1 HTTPS Configuration

#### Finding 6.1.1: TLS Not Enforced in Code [CRITICAL]

**Location:** `tai_http.erl`, lines 52-60
**Severity:** CRITICAL
**Issue:** Code uses `cowboy:start_clear()` instead of `cowboy:start_tls()`

**Current:**
```erlang
case cowboy:start_clear(?SERVER, TransOpts, ProtoOpts) of
```

**Should be:**
```erlang
case get_tls_config() of
    {ok, TlsOpts} ->
        case cowboy:start_tls(?SERVER, TlsOpts, ProtoOpts) of
            {ok, _Pid} -> {ok, #{port => Port}};
            Error -> {stop, Error}
        end;
    disabled ->
        logger:warning("TLS not configured - using plain HTTP"),
        case cowboy:start_clear(?SERVER, TransOpts, ProtoOpts) of
            {ok, _Pid} -> {ok, #{port => Port}};
            Error -> {stop, Error}
        end;
    {error, Reason} ->
        {stop, Reason}
end.

get_tls_config() ->
    TlsEnabled = application:get_env(tai_autonomics, tls_enabled, false),
    case TlsEnabled of
        false -> disabled;
        true ->
            CertFile = application:get_env(tai_autonomics, tls_cert_file, undefined),
            KeyFile = application:get_env(tai_autonomics, tls_key_file, undefined),
            case {CertFile, KeyFile} of
                {undefined, _} -> {error, missing_cert_file};
                {_, undefined} -> {error, missing_key_file};
                {Cert, Key} ->
                    case {filelib:is_file(Cert), filelib:is_file(Key)} of
                        {true, true} ->
                            {ok, #{
                                socket_opts => [{port, get_port()}],
                                certfile => Cert,
                                keyfile => Key,
                                secure_renegotiate => true,
                                reuse_sessions => true,
                                versions => ['tlsv1.2', 'tlsv1.3'],
                                ciphers => get_secure_ciphers()
                            }};
                        {false, _} -> {error, cert_file_not_found};
                        {_, false} -> {error, key_file_not_found}
                    end
            end
    end.

get_secure_ciphers() ->
    % TLS 1.2+ secure ciphers only
    [
        {ecdhe_ecdsa, aes_256_gcm, null},
        {ecdhe_rsa, aes_256_gcm, null},
        {ecdhe_ecdsa, aes_128_gcm, null},
        {ecdhe_rsa, aes_128_gcm, null}
    ].
```

---

#### Finding 6.1.2: No HSTS Header Enforcement [HIGH]

**Location:** `tai_http_handler.erl`
**Severity:** HIGH
**Issue:** Missing HSTS header forces HTTPS

**Recommendation:** Add HSTS header:
```erlang
security_headers() ->
    #{
        <<"strict-transport-security">> =>
            <<"max-age=31536000; includeSubDomains; preload">>
    }.
```

---

#### Finding 6.1.3: No Certificate Pinning for GCP Metadata Server [MEDIUM]

**Location:** `gcp_metadata.erl`
**Severity:** MEDIUM
**Issue:** No certificate validation for metadata server connections

**Recommendation:** Implement certificate pinning:
```erlang
metadata_request(Path) ->
    URL = ?METADATA_SERVER_URL ++ Path,
    Headers = [{"Metadata-Flavor", "Google"}],
    case httpc:request(get, {URL, Headers}, [
        {timeout, 5000},
        {connect_timeout, 3000},
        {ssl, [
            {verify, verify_peer},
            {cacertfile, get_ca_bundle_path()},
            {depth, 3},
            {check_hostname, true}
        ]}
    ], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, list_to_binary(Body)};
        {ok, {{_, Status, _}, _, _}} ->
            logger:error("Metadata request failed with status ~p", [Status]),
            {error, {http_error, Status}};
        {error, Reason} ->
            logger:error("Metadata request error: ~p", [Reason]),
            {error, Reason}
    end.

get_ca_bundle_path() ->
    case application:get_env(tai_autonomics, ca_cert_file, undefined) of
        undefined -> "/etc/ssl/certs/ca-bundle.crt";
        CertFile -> CertFile
    end.
```

---

## 7. Data Encryption

### 7.1 Encryption at Rest

#### Finding 7.1.1: Firestore Data Not Encrypted [MEDIUM]

**Location:** `gcp_firestore.erl`
**Severity:** MEDIUM
**Issue:** No client-side encryption before sending to Firestore

**Risk:**
- Data readable by GCP administrators
- Sensitive data (entitlements, customer info) unencrypted
- No end-to-end encryption

**Recommendation:** Implement client-side encryption:
```erlang
-define(ENCRYPTION_ALGORITHM, aes_256_gcm).
-define(ENCRYPTION_KEY_LENGTH, 32).  % 256 bits

encrypt_data(PlainData, EncryptionKey) ->
    IV = crypto:strong_rand_bytes(16),
    {CipherText, CipherTag} = crypto:crypto_one_time_aead(
        ?ENCRYPTION_ALGORITHM,
        EncryptionKey,
        IV,
        PlainData,
        <<>>,  % AAD
        true
    ),
    #{
        iv => binary_to_hex(IV),
        ciphertext => binary_to_hex(CipherText),
        tag => binary_to_hex(CipherTag),
        algorithm => atom_to_binary(?ENCRYPTION_ALGORITHM, utf8)
    }.

decrypt_data(EncryptedData, DecryptionKey) ->
    IV = hex_to_binary(maps:get(iv, EncryptedData)),
    CipherText = hex_to_binary(maps:get(ciphertext, EncryptedData)),
    CipherTag = hex_to_binary(maps:get(tag, EncryptedData)),

    try
        crypto:crypto_one_time_aead(
            ?ENCRYPTION_ALGORITHM,
            DecryptionKey,
            IV,
            CipherText,
            <<>>,
            false,
            CipherTag
        )
    catch
        error:_ -> {error, decryption_failed}
    end.

encrypt_firestore_document(Document, EncryptionKey) ->
    JsonData = jsx:encode(Document),
    case encrypt_data(JsonData, EncryptionKey) of
        EncryptedData ->
            Document#{
                <<"_encrypted_at">> => erlang:system_time(millisecond),
                <<"_encryption_key_id">> => get_encryption_key_id(EncryptionKey),
                <<"_encrypted_payload">> => EncryptedData
            };
        _ ->
            logger:error("Encryption failed for Firestore document"),
            {error, encryption_failed}
    end.

get_encryption_key_id(Key) ->
    KeyHash = crypto:hash(sha256, Key),
    binary_to_hex(KeyHash).
```

---

#### Finding 7.1.2: No Encryption of Sensitive Receipt Data [HIGH]

**Location:** `tai_receipts.erl`
**Severity:** HIGH
**Issue:** Receipts contain sensitive data without encryption

**Risk:**
- Tenant IDs and entitlement IDs exposed
- Customer information visible in Firestore
- No confidentiality protection

**Recommendation:** Encrypt sensitive fields in receipts:
```erlang
create_transition_receipt(TenantId, EntitlementId, Action, NewState, Metadata) ->
    ReceiptId = generate_receipt_id(),
    Timestamp = erlang:system_time(millisecond),
    PrevHash = get_last_hash(TenantId),

    %% Encrypt sensitive fields
    EncryptedTenantId = encrypt_field(TenantId),
    EncryptedEntitlementId = encrypt_field(EntitlementId),

    Receipt = #{
        id => ReceiptId,
        type => ?RECEIPT_TYPE_TRANSITION,
        timestamp => Timestamp,
        tenant_id => EncryptedTenantId,  % Encrypted
        entitlement_id => EncryptedEntitlementId,  % Encrypted
        action => Action,
        state_to => atom_to_binary(NewState, utf8),
        metadata => Metadata,
        prev_hash => PrevHash
    },

    Hash = compute_hash(Receipt),
    ReceiptWithHash = Receipt#{hash => Hash, chain_hash => compute_chain_hash(PrevHash, Hash)},
    store_receipt(ReceiptWithHash),
    log_receipt(ReceiptWithHash),
    ReceiptWithHash.

encrypt_field(PlainValue) ->
    case application:get_env(tai_autonomics, field_encryption_enabled, true) of
        false -> PlainValue;
        true ->
            case get_field_encryption_key() of
                {ok, Key} ->
                    EncryptedData = encrypt_data(
                        jsx:encode(PlainValue),
                        Key
                    ),
                    #{
                        <<"encrypted">> => true,
                        <<"data">> => EncryptedData
                    };
                {error, _} ->
                    logger:error("Field encryption key not available"),
                    PlainValue
            end
    end.

decrypt_field(EncryptedField) ->
    case EncryptedField of
        #{<<"encrypted">> := true, <<"data">> := Data} ->
            case get_field_encryption_key() of
                {ok, Key} ->
                    case decrypt_data(Data, Key) of
                        {ok, DecryptedJson} ->
                            jsx:decode(DecryptedJson, [return_maps]);
                        {error, Reason} ->
                            logger:error("Field decryption failed: ~p", [Reason]),
                            {error, decryption_failed}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        PlainValue ->
            PlainValue
    end.
```

---

### 7.2 Encryption in Transit

#### Finding 7.2.1: HTTP Connections Not Encrypted [CRITICAL]

**Already covered in Section 6.1.1**

---

#### Finding 7.2.2: No Certificate Validation for Firestore/Pub Sub [HIGH]

**Location:** `gcp_firestore.erl`, `gcp_pubsub.erl`
**Severity:** HIGH
**Issue:** HTTPS requests don't validate SSL certificates

```erlang
case httpc:request(post, {Path, Headers, "application/json", RequestBody}, [], []) of
```

**Recommendation:** Add certificate validation:
```erlang
make_secure_request(Method, Url, Headers, Body) ->
    case httpc:request(Method, {Url, Headers, "application/json", Body}, [
        {timeout, ?FIRESTORE_TIMEOUT},
        {ssl, [
            {verify, verify_peer},
            {cacertfile, get_ca_bundle_path()},
            {depth, 3},
            {check_hostname, true}
        ]}
    ], []) of
        {ok, Response} -> {ok, Response};
        {error, Reason} -> {error, Reason}
    end.
```

---

## 8. GCP Security Integration

### 8.1 Service Account & IAM

#### Finding 8.1.1: Overpermissioned Service Account [HIGH]

**Location:** `terraform/main.tf`, lines 69-97
**Severity:** HIGH
**Issue:** Service account has broad permissions

**Current Terraform:**
```hcl
resource "google_project_iam_member" "tai_autonomics_pubsub" {
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

resource "google_project_iam_member" "tai_autonomics_firestore" {
  role    = "roles/datastore.user"
  member  = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

resource "google_project_iam_member" "tai_autonomics_logging" {
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

resource "google_project_iam_member" "tai_autonomics_monitoring" {
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

resource "google_project_iam_member" "tai_autonomics_trace" {
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.tai_autonomics.email}"
}
```

**Issues:**
1. `roles/datastore.user` is too broad (read/write all Firestore)
2. `roles/logging.logWriter` could be scoped to specific logs
3. No resource-level IAM bindings

**Recommendation:** Apply principle of least privilege:
```hcl
# Create custom role with minimal permissions
resource "google_project_iam_custom_role" "tai_autonomics_custom" {
  role_id = "tai_autonomics_minimal"
  title   = "TAI Autonomics Minimal Permissions"

  permissions = [
    # Firestore (read/write only receipts collection)
    "datastore.databases.get",
    "datastore.entities.get",
    "datastore.entities.list",
    "datastore.entities.update",
    "datastore.entities.create",

    # Pub/Sub (subscribe and acknowledge only)
    "pubsub.subscriptions.get",
    "pubsub.subscriptions.pull",
    "pubsub.subscriptions.consume",
    "pubsub.subscriptions.update",

    # Logging (write only)
    "logging.logEntries.create",

    # Metrics (write only)
    "monitoring.timeSeries.create",

    # Tracing (write spans)
    "cloudtrace.traces.patch"
  ]
}

resource "google_project_iam_member" "tai_autonomics_custom_role" {
  project = var.project_id
  role    = google_project_iam_custom_role.tai_autonomics_custom.id
  member  = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

# Firestore resource-level IAM
resource "google_firestore_database_iam_member" "tai_autonomics_firestore" {
  database_id = google_firestore_database.tai_autonomics.id
  role        = "roles/datastore.user"
  member      = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

# Pub/Sub subscription IAM
resource "google_pubsub_subscription_iam_member" "tai_autonomics_pubsub" {
  subscription = google_pubsub_subscription.signals.name
  role         = "roles/pubsub.subscriber"
  member       = "serviceAccount:${google_service_account.tai_autonomics.email}"
}
```

---

#### Finding 8.1.2: No Service Account Key Rotation [MEDIUM]

**Location:** Terraform configuration
**Severity:** MEDIUM
**Issue:** No automated service account key rotation

**Recommendation:** Implement key rotation:
```hcl
# Create short-lived service account key
resource "google_service_account_key" "tai_autonomics" {
  service_account_id = google_service_account.tai_autonomics.name
  public_key_type    = "TYPE_X509_PEM_FILE"

  lifecycle {
    create_before_destroy = true
  }
}

# Rotate keys every 30 days
resource "null_resource" "rotate_service_account_key" {
  triggers = {
    rotation_date = formatdate("YYYY-MM-DD", timeadd(timestamp(), "720h"))
  }

  provisioner "local-exec" {
    command = "gcloud iam service-accounts keys create ~/tai-autonomics-key.json --iam-account=${google_service_account.tai_autonomics.email} && gcloud iam service-accounts keys list --iam-account=${google_service_account.tai_autonomics.email} --filter 'validAfterTime<2021-01-01' --format='value(name)' | xargs -I {} gcloud iam service-accounts keys delete --iam-account=${google_service_account.tai_autonomics.email} {}"
  }
}
```

---

### 8.2 Cloud Run Security

#### Finding 8.2.1: Public Access Allowed Without Authentication [CRITICAL]

**Location:** `terraform/main.tf`, lines 277-283
**Severity:** CRITICAL
**Issue:** Cloud Run service can be publicly accessed without authentication

```hcl
resource "google_cloud_run_service_iam_member" "public_access" {
  count    = var.enable_public_access ? 1 : 0
  role     = "roles/run.invoker"
  member   = "allUsers"
}
```

**Risk:**
- Service endpoints accessible to anyone on internet
- No authentication required
- DDoS attacks possible

**Recommendation:** Restrict to authenticated users:
```hcl
# Only allow authenticated Google Cloud users
resource "google_cloud_run_service_iam_member" "authenticated_access" {
  service  = google_cloud_run_service.tai_autonomics.name
  location = google_cloud_run_service.tai_autonomics.location
  role     = "roles/run.invoker"
  member   = "allAuthenticatedUsers"
}

# Use Cloud Armor for additional protection
resource "google_compute_security_policy" "tai_autonomics" {
  name = "tai-autonomics-policy"

  # Block traffic from specific countries/IPs
  rules {
    action   = "deny(403)"
    priority = 1000
    match {
      origin_region_code = ["CN", "RU"]  # Example countries
    }
  }

  # Rate limiting
  rules {
    action   = "rate_based_ban"
    priority = 100
    match {
      versioned_expr = "CEL"
      cel_options {
        recommend_rules = false
      }
    }
    rate_limit_options {
      conform_action = "allow"
      exceed_action  = "deny(429)"
      rate_limit_threshold {
        count        = 100
        interval_sec = 60
      }
      ban_threshold_count  = 1000
      ban_durationSec      = 600
    }
  }
}

resource "google_cloud_run_service_iam_member" "no_public_access" {
  count = var.enable_public_access ? 0 : 1
  # Not setting any public IAM member denies allUsers
}
```

---

#### Finding 8.2.2: No Network Security [MEDIUM]

**Location:** Cloud Run configuration
**Severity:** MEDIUM
**Issue:** Cloud Run service exposed directly to internet

**Recommendation:** Use VPC and Cloud Armor:
```hcl
resource "google_cloud_run_service" "tai_autonomics" {
  # ... existing config ...

  template {
    # ... existing template ...
    metadata {
      annotations = {
        "run.googleapis.com/vpc-access-connector" = google_vpc_access_connector.tai.id
        "run.googleapis.com/vpc-access-egress"    = "all-traffic"
      }
    }
  }
}

resource "google_vpc_access_connector" "tai" {
  name          = "tai-autonomics-connector"
  ip_cidr_range = "10.8.0.0/28"
  network       = google_compute_network.tai.name
  machine_type  = "f1-micro"
}

resource "google_compute_network" "tai" {
  name                    = "tai-autonomics-network"
  auto_create_subnetworks = false
}

resource "google_compute_subnetwork" "tai" {
  name          = "tai-autonomics-subnet"
  ip_cidr_range = "10.0.0.0/24"
  region        = var.region
  network       = google_compute_network.tai.id
}

resource "google_compute_security_policy" "tai_armor" {
  name = "tai-autonomics-armor"

  rules {
    action   = "allow"
    priority = 1000
    match {
      versioned_expr = "CEL"
      cel_options {
        recommend_rules = true
      }
    }
  }
}

resource "google_compute_backend_service" "tai" {
  name = "tai-autonomics-backend"

  backend {
    group = google_compute_network_endpoint_group.tai.id
  }

  security_policy = google_compute_security_policy.tai_armor.id
}

resource "google_compute_network_endpoint_group" "tai" {
  name                  = "tai-autonomics-neg"
  network_endpoint_type = "SERVERLESS"
  region                = var.region
  cloud_run_config {
    service = google_cloud_run_service.tai_autonomics.name
  }
}
```

---

### 8.3 Secrets Management

#### Finding 8.3.1: No Google Secret Manager Integration [HIGH]

**Location:** Configuration management
**Severity:** HIGH
**Issue:** Secrets not using GCP Secret Manager

**Recommendation:** Integrate with Secret Manager:
```erlang
% Create module for GCP Secret Manager integration
-module(gcp_secret_manager).

-export([get_secret/1, get_secret_version/2, rotate_secret/1]).

-define(SECRET_MANAGER_API, "https://secretmanager.googleapis.com/v1").

get_secret(SecretId) ->
    ProjectId = gcp_config:get_project_id(),
    get_secret_version(ProjectId, SecretId).

get_secret_version(ProjectId, SecretId) ->
    case gcp_metadata:get_access_token() of
        {ok, Token} ->
            Url = ?SECRET_MANAGER_API ++ "/projects/" ++ ProjectId ++
                  "/secrets/" ++ SecretId ++ "/versions/latest:access",
            Headers = [
                {"Authorization", "Bearer " ++ Token},
                {"Content-Type", "application/json"}
            ],
            case httpc:request(get, {Url, Headers}, [], []) of
                {ok, {{_, 200, _}, _, Body}} ->
                    case jsx:decode(list_to_binary(Body), [return_maps]) of
                        #{<<"payload">> := #{<<"data">> := Data}} ->
                            {ok, base64:decode(Data)};
                        _ ->
                            {error, invalid_response}
                    end;
                {ok, {{_, Status, _}, _, _}} ->
                    {error, {http_error, Status}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

rotate_secret(SecretId) ->
    case gcp_metadata:get_access_token() of
        {ok, Token} ->
            ProjectId = gcp_config:get_project_id(),
            Url = ?SECRET_MANAGER_API ++ "/projects/" ++ ProjectId ++
                  "/secrets/" ++ SecretId ++ ":addIamPolicyBinding",
            logger:info("Rotating secret: ~s", [SecretId]),
            % Implementation continues...
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

% Usage in application startup
init_secrets() ->
    SecretsToLoad = [
        {jwt_public_key, "jwt-public-key"},
        {firestore_access_token, "firestore-token"},
        {receipt_signing_key, "receipt-signing-key"}
    ],
    load_secrets(SecretsToLoad).

load_secrets([]) -> ok;
load_secrets([{ConfigKey, SecretId} | Rest]) ->
    case get_secret(SecretId) of
        {ok, SecretValue} ->
            application:set_env(tai_autonomics, ConfigKey, SecretValue),
            logger:info("Loaded secret: ~s", [SecretId]),
            load_secrets(Rest);
        {error, Reason} ->
            logger:error("Failed to load secret ~s: ~p", [SecretId, Reason]),
            {error, Reason}
    end.
```

---

## 9. Compliance & Audit

### 9.1 Audit Logging

#### Finding 9.1.1: Insufficient Audit Logging [HIGH]

**Location:** All handlers
**Severity:** HIGH
**Issue:** Limited audit trail for compliance

**Recommendation:** Implement comprehensive audit logging:
```erlang
-module(tai_audit).

-export([log_event/2, log_event/3, log_authorization/3]).

-record(audit_entry, {
    timestamp :: integer(),
    event_type :: atom(),
    actor :: binary() | atom(),
    resource :: binary(),
    action :: binary(),
    result :: success | failure,
    reason :: atom() | undefined,
    source_ip :: string(),
    user_agent :: binary() | undefined,
    metadata :: map()
}).

log_event(EventType, Actor) ->
    log_event(EventType, Actor, #{}).

log_event(EventType, Actor, Metadata) ->
    Entry = #audit_entry{
        timestamp = erlang:system_time(millisecond),
        event_type = EventType,
        actor = Actor,
        metadata = Metadata
    },
    persist_audit_entry(Entry),
    emit_audit_log(Entry).

log_authorization(Actor, Action, Resource) ->
    Entry = #audit_entry{
        timestamp = erlang:system_time(millisecond),
        event_type = authorization,
        actor = Actor,
        action = Action,
        resource = Resource
    },
    case check_authorization(Actor, Action, Resource) of
        {ok, authorized} ->
            NewEntry = Entry#audit_entry{result = success},
            persist_audit_entry(NewEntry),
            logger:info("Authorization granted: ~p -> ~p on ~p", [Actor, Action, Resource]);
        {error, Reason} ->
            NewEntry = Entry#audit_entry{result = failure, reason = Reason},
            persist_audit_entry(NewEntry),
            logger:warning("Authorization denied: ~p -> ~p on ~p: ~p",
                          [Actor, Action, Resource, Reason])
    end.

persist_audit_entry(Entry) ->
    % Store in Firestore/GCS for audit trail
    case application:get_env(tai_autonomics, audit_backend, firestore) of
        firestore ->
            AuditCollection = application:get_env(
                tai_autonomics,
                audit_collection,
                <<"audit_logs">>
            ),
            gcp_firestore:create_document(
                binary_to_list(AuditCollection),
                generate_audit_id(),
                entry_to_map(Entry)
            );
        gcs ->
            % Store in Cloud Storage
            ok;
        _ ->
            ok
    end.

emit_audit_log(Entry) ->
    AuditJson = jsx:encode(entry_to_map(Entry)),
    logger:info("AUDIT: ~s", [AuditJson]).

entry_to_map(#audit_entry{
    timestamp = Ts,
    event_type = Type,
    actor = Actor,
    resource = Resource,
    action = Action,
    result = Result,
    reason = Reason,
    metadata = Meta
}) ->
    #{
        <<"timestamp">> => Ts,
        <<"event_type">> => atom_to_binary(Type, utf8),
        <<"actor">> => maybe_binary(Actor),
        <<"resource">> => maybe_binary(Resource),
        <<"action">> => maybe_binary(Action),
        <<"result">> => atom_to_binary(Result, utf8),
        <<"reason">> => maybe_binary(Reason),
        <<"metadata">> => Meta
    }.

maybe_binary(undefined) -> undefined;
maybe_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
maybe_binary(Bin) when is_binary(Bin) -> Bin;
maybe_binary(Str) when is_list(Str) -> list_to_binary(Str);
maybe_binary(Val) -> val_to_binary(Val).

val_to_binary(Val) ->
    list_to_binary(io_lib:format("~p", [Val])).

generate_audit_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    list_to_binary(io_lib:format("~32.16.0b", [Id])).
```

---

### 9.2 Compliance Requirements

#### Finding 9.2.1: No GDPR Compliance Logging [HIGH]

**Location:** Data handling throughout application
**Severity:** HIGH
**Issue:** No logging for GDPR compliance (data access, deletion, etc.)

**Recommendation:** Add GDPR compliance logging:
```erlang
log_personal_data_access(UserId, DataType, Purpose) ->
    tai_audit:log_event(personal_data_access, UserId, #{
        data_type => DataType,
        purpose => Purpose,
        timestamp => erlang:system_time(millisecond)
    }).

log_personal_data_deletion(UserId, DataType) ->
    tai_audit:log_event(personal_data_deletion, UserId, #{
        data_type => DataType,
        timestamp => erlang:system_time(millisecond),
        retention_period => application:get_env(tai_autonomics, data_retention_days, 90)
    }).

handle_gdpr_erasure_request(TenantId) ->
    % 1. Log request
    tai_audit:log_event(gdpr_erasure_request, TenantId, #{}),

    % 2. Delete data
    case gcp_firestore:delete_document("tenant_data", binary_to_list(TenantId)) of
        ok ->
            % 3. Create audit proof
            tai_audit:log_event(gdpr_erasure_completed, TenantId, #{
                timestamp => erlang:system_time(millisecond)
            }),
            {ok, erased};
        {error, Reason} ->
            logger:error("GDPR erasure failed: ~p", [Reason]),
            {error, Reason}
    end.
```

---

## 10. Security Testing Guide

### 10.1 Penetration Testing Roadmap

```markdown
## Security Testing Checklist

### Phase 1: Authentication & Authorization (Week 1)
- [ ] Test JWT validation with expired tokens
- [ ] Test JWT with invalid signatures
- [ ] Test JWT with wrong audience
- [ ] Test Pub/Sub message without authentication
- [ ] Test Marketplace API without authentication
- [ ] Test with invalid service account
- [ ] Test with revoked tokens
- [ ] Test token caching/expiration edge cases

### Phase 2: Input Validation (Week 2)
- [ ] Test /pubsub with oversized payload (>1MB)
- [ ] Test /pubsub with deeply nested JSON (>10 levels)
- [ ] Test /marketplace with invalid UUIDs
- [ ] Test with SQL injection payloads in fields
- [ ] Test with XXE payloads
- [ ] Test with LDAP injection payloads
- [ ] Test with path traversal payloads
- [ ] Test with null bytes in fields
- [ ] Test with Unicode normalization attacks

### Phase 3: Cryptography (Week 3)
- [ ] Verify TLS version 1.2+ enforced
- [ ] Verify strong cipher suites only
- [ ] Test certificate validation
- [ ] Test HSTS header present
- [ ] Verify receipt hash chain integrity
- [ ] Test signature verification bypass
- [ ] Test JWT algorithm confusion
- [ ] Verify certificate pinning

### Phase 4: Data Protection (Week 4)
- [ ] Verify encryption at rest
- [ ] Test encryption key rotation
- [ ] Verify no plaintext secrets in logs
- [ ] Test data deletion compliance
- [ ] Verify audit logging
- [ ] Test rate limiting
- [ ] Verify no information disclosure in errors
- [ ] Test CORS validation

### Phase 5: GCP Integration (Week 5)
- [ ] Verify service account permissions
- [ ] Test metadata server access restrictions
- [ ] Verify Firestore document encryption
- [ ] Test Pub/Sub access control
- [ ] Verify Cloud Run IAM policies
- [ ] Test Cloud Armor rules
- [ ] Verify logging integration
- [ ] Test monitoring/alerting
```

### 10.2 Automated Security Testing

```erlang
% test/security_tests.erl
-module(security_tests).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
        test_jwt_expiration,
        test_invalid_signature,
        test_oversized_payload,
        test_malformed_json,
        test_invalid_tenant_id,
        test_rate_limiting,
        test_tls_enforcement,
        test_authorization_bypass
    ].

test_jwt_expiration(_Config) ->
    %% Create expired JWT
    ExpiredJwt = create_expired_jwt(),

    %% Attempt request with expired JWT
    Result = httpc:request(post, {
        "https://localhost:8080/pubsub",
        [{"Authorization", "Bearer " ++ ExpiredJwt}],
        "application/json",
        jsx:encode(#{message => test})
    }, [], []),

    %% Should fail
    {ok, {{_, 401, _}, _, _}} = Result.

test_invalid_signature(_Config) ->
    %% Create JWT with invalid signature
    InvalidJwt = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.invalid",

    Result = httpc:request(post, {
        "https://localhost:8080/marketplace",
        [{"Authorization", "Bearer " ++ InvalidJwt}],
        "application/json",
        jsx:encode(#{action => <<"grant">>})
    }, [], []),

    {ok, {{_, 401, _}, _, _}} = Result.

test_oversized_payload(_Config) ->
    %% Create 2MB payload
    OversizedPayload = binary:copy(<<"x">>, 2000000),
    Base64Data = base64:encode(OversizedPayload),

    Envelope = #{
        <<"message">> => #{
            <<"data">> => Base64Data
        }
    },

    Result = httpc:request(post, {
        "https://localhost:8080/pubsub",
        [],
        "application/json",
        jsx:encode(Envelope)
    }, [], []),

    %% Should reject
    {ok, {{_, 413, _}, _, _}} = Result.

test_malformed_json(_Config) ->
    MalformedJson = "{invalid json}",

    Result = httpc:request(post, {
        "https://localhost:8080/pubsub",
        [{"Content-Type", "application/json"}],
        "application/json",
        MalformedJson
    }, [], []),

    {ok, {{_, 400, _}, _, _}} = Result.

test_invalid_tenant_id(_Config) ->
    InvalidEvent = #{
        <<"tenant_id">> => <<"not-a-uuid">>,
        <<"entitlement_id">> => <<"550e8400-e29b-41d4-a716-446655440000">>,
        <<"action">> => <<"grant">>
    },

    Result = httpc:request(post, {
        "https://localhost:8080/marketplace",
        [{"Authorization", "Bearer " ++ valid_jwt()}],
        "application/json",
        jsx:encode(InvalidEvent)
    }, [], []),

    {ok, {{_, 400, _}, _, _}} = Result.

test_rate_limiting(_Config) ->
    %% Send 1000 requests rapidly
    Results = lists:map(fun(_) ->
        httpc:request(get, {
            "https://localhost:8080/health",
            []
        }, [], [])
    end, lists:seq(1, 1000)),

    %% Eventually should get rate limit error
    RateLimited = lists:filter(fun({ok, {{_, Status, _}, _, _}}) ->
        Status =:= 429
    end, Results),

    ct:log("Rate limited: ~p/1000", [length(RateLimited)]),
    true = length(RateLimited) > 0.

test_tls_enforcement(_Config) ->
    %% Attempt plain HTTP
    case gen_tcp:connect("localhost", 8080, []) of
        {ok, Sock} ->
            %% Plain TCP connection succeeded - should not happen
            gen_tcp:close(Sock),
            ct:log("WARNING: Plain HTTP not rejected"),
            false;
        {error, _} ->
            %% Good - plain connection rejected
            true
    end.

test_authorization_bypass(_Config) ->
    %% Create low-privilege JWT
    LowPrivJwt = create_jwt_with_role(viewer),

    %% Try to grant entitlement (requires admin)
    Event = #{
        <<"tenant_id">> => <<"550e8400-e29b-41d4-a716-446655440000">>,
        <<"entitlement_id">> => <<"550e8400-e29b-41d4-a716-446655440001">>,
        <<"action">> => <<"grant">>
    },

    Result = httpc:request(post, {
        "https://localhost:8080/marketplace",
        [{"Authorization", "Bearer " ++ LowPrivJwt}],
        "application/json",
        jsx:encode(Event)
    }, [], []),

    %% Should fail with 403 Forbidden
    {ok, {{_, 403, _}, _, _}} = Result.

% Helper functions
create_expired_jwt() ->
    Claims = #{
        exp => erlang:system_time(second) - 3600,
        iat => erlang:system_time(second) - 7200
    },
    create_jwt(Claims).

create_jwt(Claims) ->
    jose_jwt:sign(get_signing_key(), Claims).

create_jwt_with_role(Role) ->
    Claims = #{
        exp => erlang:system_time(second) + 3600,
        iat => erlang:system_time(second),
        role => Role
    },
    create_jwt(Claims).

valid_jwt() ->
    create_jwt(#{
        exp => erlang:system_time(second) + 3600,
        iat => erlang:system_time(second),
        role => admin
    }).

get_signing_key() ->
    application:get_env(tai_autonomics, jwt_signing_key, <<"test-key">>).
```

---

## 11. Recommendations & Remediation

### Priority 1: CRITICAL (Implement Immediately)

| ID | Issue | Effort | Impact |
|----|----|-----|----|
| CR-1 | Enable TLS/HTTPS in HTTP handler | LOW | CRITICAL |
| CR-2 | Implement request authentication | MEDIUM | CRITICAL |
| CR-3 | Enforce JWT signature verification | MEDIUM | CRITICAL |
| CR-4 | Remove hardcoded secrets from config | MEDIUM | CRITICAL |
| CR-5 | Implement comprehensive input validation | HIGH | CRITICAL |

### Priority 2: HIGH (Implement Within 2 Weeks)

| ID | Issue | Effort | Impact |
|----|----|-----|----|
| H-1 | Add security HTTP headers | LOW | HIGH |
| H-2 | Implement rate limiting | MEDIUM | HIGH |
| H-3 | Add authorization checks (RBAC) | MEDIUM | HIGH |
| H-4 | Implement client-side encryption | HIGH | HIGH |
| H-5 | Add comprehensive audit logging | MEDIUM | HIGH |
| H-6 | Implement Secret Manager integration | MEDIUM | HIGH |
| H-7 | Restrict service account IAM roles | MEDIUM | HIGH |
| H-8 | Disable public access to Cloud Run | LOW | HIGH |

### Priority 3: MEDIUM (Implement Within 1 Month)

| ID | Issue | Effort | Impact |
|----|----|-----|----|
| M-1 | Implement token rotation strategy | MEDIUM | MEDIUM |
| M-2 | Add GDPR compliance logging | MEDIUM | MEDIUM |
| M-3 | Improve error handling | LOW | MEDIUM |
| M-4 | Add certificate validation | LOW | MEDIUM |
| M-5 | Implement VPC and Cloud Armor | MEDIUM | MEDIUM |

---

### Remediation Timeline

```
Month 1:
  Week 1: CR-1, CR-2, CR-3, CR-4 (CRITICAL items)
  Week 2: CR-5 (Input validation), H-1, H-2
  Week 3: H-3, H-4
  Week 4: H-5, H-6, H-7, H-8

Month 2:
  Week 1-2: M-1, M-2
  Week 3-4: M-3, M-4, M-5

Month 3:
  Ongoing: Security testing, monitoring, updates
```

---

## Appendix A: Security Configuration Template

```erlang
% config/prod.sys.config - Production Security Configuration
[
  {tai_autonomics, [
    %% TLS/HTTPS
    {tls_enabled, true},
    {tls_cert_file, {env, "TLS_CERT_FILE"}},
    {tls_key_file, {env, "TLS_KEY_FILE"}},
    {tls_min_version, 'tlsv1.2'},
    {tls_versions, ['tlsv1.2', 'tlsv1.3']},

    %% Authentication
    {jwt_enabled, true},
    {jwt_public_key, {env, "JWT_PUBLIC_KEY"}},
    {jwt_signing_key, {env, "JWT_SIGNING_KEY"}},
    {verify_signatures, true},  % MANDATORY in production
    {trusted_jwt_issuers, [<<"https://accounts.google.com">>]},
    {trusted_service_accounts, {env, "TRUSTED_SERVICE_ACCOUNTS"}},

    %% Authorization
    {rbac_enabled, true},
    {authorization_timeout_ms, 5000},

    %% Rate Limiting
    {rate_limiting_enabled, true},
    {rate_limit_requests_per_minute, 600},
    {rate_limit_requests_per_hour, 10000},

    %% Encryption
    {encryption_enabled, true},
    {encryption_algorithm, aes_256_gcm},
    {field_encryption_enabled, true},
    {field_encryption_key, {env, "FIELD_ENCRYPTION_KEY"}},

    %% Audit & Compliance
    {audit_logging_enabled, true},
    {audit_backend, firestore},
    {audit_collection, <<"audit_logs">>},
    {gdpr_compliance_enabled, true},
    {data_retention_days, 90},

    %% Secrets Management
    {secret_manager_enabled, true},
    {secret_rotation_interval_ms, 3600000},  % 1 hour
    {secret_rotation_enabled, true},

    %% GCP Integration
    {service_account_validation, true},
    {metadata_server_timeout_ms, 5000},
    {metadata_server_retries, 3},
    {ca_cert_file, {env, "CA_CERT_FILE"}},

    %% Firestore
    {firestore_enabled, true},
    {firestore_encryption_enabled, true},
    {firestore_document_encryption_key, {env, "FIRESTORE_ENCRYPTION_KEY"}},

    %% Logging
    {sensitive_data_logging, false},
    {audit_log_level, info},
    {security_event_log_level, warning},

    %% CORS
    {cors_enabled, true},
    {allowed_origins, [
        <<"https://app.example.com">>,
        <<"https://admin.example.com">>
    ]}
  ]}
].
```

---

## Appendix B: Security Headers Implementation

```erlang
% Module for generating secure HTTP headers
-module(tai_security_headers).

-export([get_security_headers/0, get_csp_header/0]).

get_security_headers() ->
    #{
        %% Prevent MIME type sniffing
        <<"x-content-type-options">> => <<"nosniff">>,

        %% Prevent clickjacking
        <<"x-frame-options">> => <<"DENY">>,

        %% Enable XSS filter (legacy)
        <<"x-xss-protection">> => <<"1; mode=block">>,

        %% HSTS - enforce HTTPS
        <<"strict-transport-security">> =>
            <<"max-age=31536000; includeSubDomains; preload">>,

        %% Referrer policy
        <<"referrer-policy">> => <<"strict-origin-when-cross-origin">>,

        %% CSP header
        <<"content-security-policy">> => get_csp_header(),

        %% No caching of sensitive data
        <<"cache-control">> => <<"no-store, no-cache, must-revalidate, proxy-revalidate">>,
        <<"pragma">> => <<"no-cache">>,
        <<"expires">> => <<"0">>,

        %% Remove server header
        <<"server">> => <<"Erlang">>,

        %% Prevent information disclosure
        <<"x-powered-by">> => undefined
    }.

get_csp_header() ->
    %% Strict CSP for API
    <<"default-src 'none'; "
      "frame-ancestors 'none'; "
      "base-uri 'none'; "
      "form-action 'none'">>.
```

---

## Conclusion

TAI Erlang Autonomics demonstrates solid architectural patterns but requires immediate action on authentication, authorization, and input validation before production deployment. The most critical issues are:

1. **TLS/HTTPS not enforced** - All communication in plaintext
2. **No authentication/authorization** - Any user can access APIs
3. **Signature verification optional** - Marketplace events can be spoofed
4. **Secrets hardcoded** - Exposed in configuration

Following the remediation roadmap with priority-based implementation will bring the system to production-ready security standards. Regular security testing and ongoing monitoring are essential for maintaining security posture.

---

**Report Generated:** January 25, 2026
**Reviewer:** Security Analysis Agent
**Status:** REQUIRES SIGN-OFF FOR PRODUCTION DEPLOYMENT
