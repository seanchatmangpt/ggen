# SECURITY REMEDIATION ROADMAP: CRITICAL PATH TO PRODUCTION

**Status**: 🔴 CRITICAL - 8 vulnerabilities must be fixed before revenue launch
**Date**: 2026-01-25
**Priority**: HIGHEST - This is the security gate that blocks all other work

---

## Overview: Why This Matters

**The core question**: "Can an attacker grant themselves high-value entitlements without authorization?"

**Current answer**: YES - Multiple attack vectors exist

**After remediation**: NO - All attack vectors closed

This roadmap addresses the 8 most dangerous vulnerabilities that enable value inflation fraud.

---

## Vulnerability Priority Matrix

| Priority | Vulnerability | Risk | Effort | Timeline | Blocker? |
|---|---|---|---|---|---|
| 1 | No TLS/HTTPS (plaintext HTTP) | CRITICAL | LOW | 2 days | YES |
| 2 | No request authentication | CRITICAL | MEDIUM | 1 week | YES |
| 3 | Signature verification optional | CRITICAL | MEDIUM | 1 week | YES |
| 4 | Incomplete JWT validation | CRITICAL | MEDIUM | 1 week | YES |
| 5 | No rate limiting | CRITICAL | LOW | 3 days | YES |
| 6 | Poor input validation | HIGH | MEDIUM | 1 week | PARTIAL |
| 7 | Missing security headers | HIGH | LOW | 1 day | NO |
| 8 | No audit logging | MEDIUM-HIGH | MEDIUM | 1 week | NO |

**Total Effort**: 4-5 weeks of engineering
**Cost**: 1-2 senior engineers for 1 month
**Blocker**: YES - Cannot launch without all 8

---

## Vulnerability 1: No TLS/HTTPS

### Risk Assessment

**Severity**: 🔴 CRITICAL
**CVSS Score**: 9.8 (Network accessible, authentication not required)
**Business Impact**: All customer data transmitted in plaintext

**Attack Scenario**:
1. Attacker intercepts network traffic (WiFi, ISP, BGP hijack)
2. Reads JWT token in plaintext
3. Replays token to grant entitlements
4. Reads customer entitlement IDs
5. Reads value amounts

### Current State
```erlang
% VULNERABLE: Plain HTTP
case cowboy:start_clear(?SERVER, TransOpts, ProtoOpts) of
```

### Remediation

**Step 1: Get TLS Certificate**
- Self-signed: Use in dev/test (1 hour)
- Let's Encrypt: Free, auto-renewing (1 hour)
- Google-managed: GCP Cloud Run handles automatically (included)

**Recommended**: GCP Cloud Run manages TLS automatically if DNS configured

**Step 2: Code Change**
```erlang
% FIXED: Use TLS
case get_tls_config() of
    {ok, TlsOpts} ->
        case cowboy:start_tls(?SERVER, TlsOpts, ProtoOpts) of
            {ok, _Pid} -> {ok, #{port => Port}};
            Error -> {stop, Error}
        end;
    Error -> {stop, Error}
end.

get_tls_config() ->
    TlsEnabled = application:get_env(tai_autonomics, tls_enabled, false),
    case TlsEnabled of
        false -> {error, tls_disabled};
        true ->
            CertFile = application:get_env(tai_autonomics, tls_cert_file),
            KeyFile = application:get_env(tai_autonomics, tls_key_file),
            case {CertFile, KeyFile} of
                {undefined, _} -> {error, missing_cert_file};
                {_, undefined} -> {error, missing_key_file};
                {Cert, Key} ->
                    {ok, #{
                        socket_opts => [{port, get_port()}],
                        certfile => Cert,
                        keyfile => Key,
                        secure_renegotiate => true,
                        versions => ['tlsv1.2', 'tlsv1.3'],
                        ciphers => [
                            {ecdhe_ecdsa, aes_256_gcm, null},
                            {ecdhe_rsa, aes_256_gcm, null}
                        ]
                    }}
            end
    end.
```

**Step 3: Configuration**
```erlang
% config/sys.config
{tai_autonomics, [
    {tls_enabled, true},
    {tls_cert_file, {env, "TLS_CERT_FILE"}},
    {tls_key_file, {env, "TLS_KEY_FILE"}},
    {tls_versions, ['tlsv1.2', 'tlsv1.3']},
    {tls_min_version, 'tlsv1.2'}
]}
```

**Step 4: GCP Cloud Run Configuration**
```bash
# Cloud Run handles HTTPS automatically
# All requests automatically redirected HTTP → HTTPS
# Certificate managed by Google (no manual action needed)

gcloud run deploy tai-autonomics \
  --allow-unauthenticated \  # Controlled by IAM, not TLS
  --region us-central1
```

**Step 5: Testing**
```erlang
-module(tls_test).

test_https_required() ->
    % Should fail with plain HTTP
    {error, _} = httpc:request(get, {
        "http://localhost:8080/health", []
    }, [], []),

    % Should succeed with HTTPS
    {ok, {{_, 200, _}, _, _}} = httpc:request(get, {
        "https://localhost:8080/health", []
    }, [{ssl, [
        {verify, verify_peer},
        {cacertfile, "/etc/ssl/certs/ca-bundle.crt"}
    ]}], []).
```

**Step 6: Verification Checklist**
- [ ] Code change merged and reviewed
- [ ] `rebar3 compile` passes
- [ ] Integration test passes
- [ ] Deployed to staging
- [ ] Plain HTTP connection rejected
- [ ] HTTPS connection accepted
- [ ] HSTS header present

**Timeline**: 2 days
**Owner**: Backend engineer
**Block for go-live**: YES

---

## Vulnerability 2: No Request Authentication

### Risk Assessment

**Severity**: 🔴 CRITICAL
**CVSS Score**: 10.0 (Unauthenticated access to sensitive operations)
**Business Impact**: Anyone can POST to `/marketplace` to grant entitlements

**Attack Scenario**:
```bash
# Attacker runs this from anywhere
curl -X POST https://api.example.com/marketplace \
  -H "Content-Type: application/json" \
  -d '{
    "tenant_id": "victim_org",
    "entitlement_id": "ent_12345",
    "action": "grant",
    "value": 1000000
  }'

# Response: 200 OK - Entitlement granted!
# No authentication required - CRITICAL BUG
```

### Current State
```erlang
% VULNERABLE: No auth check
handle_marketplace(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            handle_marketplace_body(Req, State)
        % No check for Authorization header!
```

### Remediation

**Step 1: Add Authentication Middleware**
```erlang
% File: apps/tai_autonomics/src/tai_http_handler.erl

-spec authenticate_request(Req) -> {ok, Req} | {error, Reason}
  when Req :: cowboy_req:req(),
       Reason :: missing_auth | invalid_token | verification_failed.

authenticate_request(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            {error, missing_auth};
        AuthHeader ->
            parse_and_verify_auth(AuthHeader)
    end.

parse_and_verify_auth(AuthHeader) ->
    case binary:split(AuthHeader, <<" ">>) of
        [<<"Bearer">>, Token] ->
            verify_bearer_token(Token);
        _ ->
            {error, invalid_auth_header}
    end.

verify_bearer_token(Token) ->
    case application:get_env(tai_autonomics, jwt_public_key, undefined) of
        undefined ->
            logger:error("JWT public key not configured"),
            {error, jwt_key_not_configured};
        Key ->
            try
                {true, Claims, _Jws} = jose_jwt:verify(Key, Token),
                {ok, Claims}
            catch
                error:_ ->
                    {error, verification_failed}
            end
    end.
```

**Step 2: Apply Authentication to Protected Endpoints**
```erlang
% File: apps/tai_autonomics/src/tai_http_handler.erl

handle_marketplace(Req, State) ->
    % REQUIRED: Authenticate first
    case authenticate_request(Req) of
        {ok, Claims} ->
            handle_marketplace_authenticated(Claims, Req, State);
        {error, Reason} ->
            Refusal = tai_receipts:create_refusal(Reason),
            cowboy_req:reply(401, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(Refusal), Req),
            {ok, Req, State}
    end.

handle_marketplace_authenticated(Claims, Req, State) ->
    % Now process the request with authenticated user
    case cowboy_req:read_body(Req) of
        {ok, Body, Req2} ->
            handle_marketplace_body(Body, Claims, Req2, State);
        {error, Reason} ->
            Reply = #{status => error, reason => Reason},
            cowboy_req:reply(400, #{}, jsx:encode(Reply), Req),
            {ok, Req, State}
    end.
```

**Step 3: Configuration**
```erlang
% config/sys.config
{tai_autonomics, [
    {jwt_enabled, true},
    {jwt_public_key, {env, "JWT_PUBLIC_KEY"}},
    {jwt_algorithm, <<"RS256">>},
    {trusted_jwt_issuers, [<<"https://accounts.google.com">>]}
]}
```

**Step 4: Testing**
```erlang
test_authentication() ->
    ValidJwt = create_valid_jwt(),
    InvalidJwt = "invalid.jwt.token",

    % Test 1: No auth header
    {ok, {{_, 401, _}, _, _}} = httpc:request(post, {
        "https://localhost:8080/marketplace",
        [],
        "application/json",
        jsx:encode(#{action => grant})
    }, [], []),

    % Test 2: Invalid JWT
    {ok, {{_, 401, _}, _, _}} = httpc:request(post, {
        "https://localhost:8080/marketplace",
        [{"Authorization", "Bearer " ++ InvalidJwt}],
        "application/json",
        jsx:encode(#{action => grant})
    }, [], []),

    % Test 3: Valid JWT
    {ok, {{_, 200, _}, _, _}} = httpc:request(post, {
        "https://localhost:8080/marketplace",
        [{"Authorization", "Bearer " ++ ValidJwt}],
        "application/json",
        jsx:encode(#{
            tenant_id => <<"tenant_123">>,
            entitlement_id => <<"ent_456">>,
            action => <<"grant">>
        })
    }, [], []).
```

**Timeline**: 1 week
**Owner**: Backend engineer
**Block for go-live**: YES

---

## Vulnerability 3: Signature Verification Optional

### Risk Assessment

**Severity**: 🔴 CRITICAL
**CVSS Score**: 9.8 (Authentication bypass)
**Business Impact**: Marketplace events can be forged

**Attack Scenario**:
1. In production, signature verification is disabled by default
2. Attacker sends fake marketplace event with forged signature
3. No verification happens
4. Entitlement granted without authentic source

**Current Code (VULNERABLE)**:
```erlang
verify_signature(Event) ->
    case application:get_env(tai_autonomics, verify_signatures, false) of
        false ->
            {ok, verified};  % BUG: Default is to NOT verify!
        true ->
            do_verify_signature(Event)
    end.
```

### Remediation

**Step 1: Make Signature Verification Mandatory in Production**
```erlang
% File: apps/tai_autonomics/src/tai_marketplace_ingress.erl

verify_signature(Event) ->
    case get_environment_tier() of
        production ->
            % MANDATORY in production
            case do_verify_signature(Event) of
                {ok, verified} -> {ok, verified};
                {error, Reason} ->
                    logger:warning("Signature verification failed: ~p", [Reason]),
                    {error, signature_verification_failed}
            end;
        development ->
            % Optional in dev (for testing)
            case application:get_env(tai_autonomics, verify_signatures, false) of
                true -> do_verify_signature(Event);
                false ->
                    logger:info("Signature verification skipped in dev"),
                    {ok, verified}
            end
    end.

get_environment_tier() ->
    case os:getenv("ENVIRONMENT") of
        "production" -> production;
        "prod" -> production;
        _ -> development
    end.

do_verify_signature(Event) ->
    case maps:get(<<"signature">>, Event, undefined) of
        undefined ->
            {error, missing_signature};
        Signature ->
            case get_verification_key() of
                {ok, Key} ->
                    try
                        {true, _Claims, _Jws} = jose_jwt:verify(Key, Signature),
                        {ok, verified}
                    catch
                        error:_ ->
                            {error, invalid_signature}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_verification_key() ->
    case application:get_env(tai_autonomics, marketplace_verification_key) of
        undefined ->
            {error, verification_key_not_configured};
        Key ->
            {ok, Key}
    end.
```

**Step 2: Configuration**
```erlang
% config/prod.sys.config
{tai_autonomics, [
    {verify_signatures, true},  % MANDATORY
    {marketplace_verification_key, {env, "MARKETPLACE_VERIFICATION_KEY"}}
]}

% config/dev.sys.config
{tai_autonomics, [
    {verify_signatures, false}  % OK for dev/test only
]}
```

**Step 3: Testing**
```erlang
test_signature_verification() ->
    % Test in production mode
    application:set_env(tai_autonomics, environment, production),

    % Test 1: No signature
    {error, missing_signature} = verify_signature(#{action => grant}),

    % Test 2: Invalid signature
    {error, invalid_signature} = verify_signature(#{
        action => grant,
        signature => <<"invalid.jwt">>
    }),

    % Test 3: Valid signature
    ValidSignature = create_marketplace_jwt(),
    {ok, verified} = verify_signature(#{
        action => grant,
        signature => ValidSignature
    }).
```

**Timeline**: 1 week
**Owner**: Backend engineer
**Block for go-live**: YES

---

## Vulnerability 4: Incomplete JWT Validation

### Risk Assessment

**Severity**: 🔴 CRITICAL
**CVSS Score**: 8.5 (Authentication bypass)
**Business Impact**: Expired/old/forged tokens accepted

**Current Code (VULNERABLE)**:
```erlang
case jose_jwt:verify(Key, Signature) of
    {true, Claims, _JWS} ->
        case maps:is_key(<<"tenant_id">>, Claims) of
            true -> {ok, verified};
            false -> {error, invalid_claims}
        end;
    {false, _} ->
        {error, invalid_signature}
end.
% BUG: No expiration check, no iat check, no issuer check, no audience check!
```

### Remediation

**Step 1: Comprehensive JWT Validation**
```erlang
% File: apps/tai_autonomics/src/tai_jwt.erl

-define(JWT_ALGORITHM, <<"RS256">>).
-define(JWT_CLOCK_SKEW_SECONDS, 30).
-define(JWT_MAX_AGE_SECONDS, 3600).

-spec verify_jwt_complete(Token) -> {ok, Claims} | {error, Reason}
  when Token :: binary(),
       Claims :: map(),
       Reason :: atom().

verify_jwt_complete(Token) ->
    case application:get_env(tai_autonomics, jwt_public_key, undefined) of
        undefined ->
            {error, jwt_key_not_configured};
        Key ->
            try
                {true, Claims, #{alg := Algorithm}} = jose_jwt:verify(Key, Token),
                validate_jwt_complete(Algorithm, Claims)
            catch
                error:{badarg, _} ->
                    {error, invalid_token};
                error:_ ->
                    {error, verification_failed}
            end
    end.

validate_jwt_complete(Algorithm, Claims) ->
    case Algorithm =:= ?JWT_ALGORITHM of
        false -> {error, invalid_algorithm};
        true -> validate_jwt_claims(Claims)
    end.

validate_jwt_claims(Claims) ->
    Now = erlang:system_time(second),

    % 1. Check expiration (required)
    case validate_expiration(Claims, Now) of
        {error, Reason} -> {error, Reason};
        ok ->
            % 2. Check issued-at (required)
            case validate_issued_at(Claims, Now) of
                {error, Reason} -> {error, Reason};
                ok ->
                    % 3. Check not-before (optional)
                    case validate_not_before(Claims, Now) of
                        {error, Reason} -> {error, Reason};
                        ok ->
                            % 4. Check issuer (required)
                            case validate_issuer(Claims) of
                                {error, Reason} -> {error, Reason};
                                ok ->
                                    % 5. Check audience (required)
                                    case validate_audience(Claims) of
                                        {error, Reason} -> {error, Reason};
                                        ok -> {ok, Claims}
                                    end
                            end
                    end
            end
    end.

validate_expiration(Claims, Now) ->
    case maps:get(<<"exp">>, Claims, undefined) of
        undefined ->
            {error, missing_exp_claim};
        Exp when Exp < (Now - ?JWT_CLOCK_SKEW_SECONDS) ->
            {error, token_expired};
        Exp when Exp > Now + ?JWT_MAX_AGE_SECONDS ->
            logger:warning("Token expiry too far in future: ~p", [Exp]),
            {error, token_exp_too_far};
        _ ->
            ok
    end.

validate_issued_at(Claims, Now) ->
    case maps:get(<<"iat">>, Claims, undefined) of
        undefined ->
            {error, missing_iat_claim};
        Iat when Iat > (Now + ?JWT_CLOCK_SKEW_SECONDS) ->
            {error, token_not_yet_valid};
        Iat when Now - Iat > 86400 ->  % Older than 24 hours
            logger:warning("Token issued too long ago: ~p", [Iat]),
            {error, token_too_old};
        _ ->
            ok
    end.

validate_not_before(Claims, Now) ->
    case maps:get(<<"nbf">>, Claims, undefined) of
        undefined ->
            ok;  % Optional
        Nbf when Nbf > (Now + ?JWT_CLOCK_SKEW_SECONDS) ->
            {error, token_not_yet_valid};
        _ ->
            ok
    end.

validate_issuer(Claims) ->
    case maps:get(<<"iss">>, Claims, undefined) of
        undefined ->
            {error, missing_iss_claim};
        Issuer ->
            case is_trusted_jwt_issuer(Issuer) of
                true -> ok;
                false ->
                    logger:warning("Untrusted issuer: ~p", [Issuer]),
                    {error, untrusted_issuer}
            end
    end.

validate_audience(Claims) ->
    case maps:get(<<"aud">>, Claims, undefined) of
        undefined ->
            {error, missing_aud_claim};
        Audience ->
            case is_valid_jwt_audience(Audience) of
                true -> ok;
                false ->
                    logger:warning("Invalid audience: ~p", [Audience]),
                    {error, invalid_audience}
            end
    end.

is_trusted_jwt_issuer(Issuer) ->
    TrustedIssuers = application:get_env(tai_autonomics, trusted_jwt_issuers, []),
    lists:member(Issuer, TrustedIssuers).

is_valid_jwt_audience(Audience) ->
    ValidAudiences = application:get_env(tai_autonomics, valid_jwt_audiences, []),
    lists:member(Audience, ValidAudiences).
```

**Step 2: Configuration**
```erlang
% config/sys.config
{tai_autonomics, [
    {jwt_public_key, {env, "JWT_PUBLIC_KEY"}},
    {trusted_jwt_issuers, [
        <<"https://accounts.google.com">>,
        <<"https://accounts.example.com">>
    ]},
    {valid_jwt_audiences, [
        <<"tai-autonomics-api">>
    ]}
]}
```

**Step 3: Integration**
```erlang
% In authentication flow
authenticate_request(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            {error, missing_auth};
        AuthHeader ->
            case parse_auth_header(AuthHeader) of
                {ok, Token} ->
                    % Use comprehensive JWT validation
                    case tai_jwt:verify_jwt_complete(Token) of
                        {ok, Claims} -> {ok, Claims};
                        {error, Reason} -> {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.
```

**Timeline**: 1 week
**Owner**: Backend engineer
**Block for go-live**: YES

---

## Vulnerability 5: No Rate Limiting

### Risk Assessment

**Severity**: 🔴 CRITICAL
**CVSS Score**: 7.5 (DoS, brute force)
**Business Impact**: DDoS attacks, rapid entitlement granting

**Attack Scenario**:
1. Attacker sends 10,000 requests/sec
2. Marketplace grants 10,000 fake entitlements/sec
3. No rate limiting stops it
4. Massive fraud in seconds

### Remediation

**Step 1: Implement Rate Limiting**
```erlang
% File: apps/tai_autonomics/src/tai_rate_limiter.erl

-module(tai_rate_limiter).
-behaviour(gen_server).

-define(RATE_LIMIT_REQUESTS_PER_MINUTE, 600).
-define(RATE_LIMIT_REQUESTS_PER_HOUR, 10000).
-define(BUCKET_SIZE_MINUTES, 5).

-record(state, {
    buckets :: ets:table(),
    hourly_buckets :: ets:table()
}).

-export([start_link/0, check_limit/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(rate_limit_buckets, [named_table, set, {heir, self(), undefined}]),
    ets:new(rate_limit_hourly, [named_table, set, {heir, self(), undefined}]),
    {ok, #state{buckets = rate_limit_buckets, hourly_buckets = rate_limit_hourly}}.

check_limit(IpAddress) ->
    case check_minute_limit(IpAddress) of
        ok ->
            case check_hour_limit(IpAddress) of
                ok -> ok;
                limited -> {error, hour_limit_exceeded}
            end;
        limited ->
            {error, minute_limit_exceeded}
    end.

check_minute_limit(IpAddress) ->
    Key = {minute, IpAddress},
    case ets:lookup(rate_limit_buckets, Key) of
        [] ->
            ets:insert(rate_limit_buckets, {Key, 1}),
            ok;
        [{Key, Count}] when Count >= ?RATE_LIMIT_REQUESTS_PER_MINUTE ->
            limited;
        [{Key, Count}] ->
            ets:update_counter(rate_limit_buckets, Key, 1),
            ok
    end.

check_hour_limit(IpAddress) ->
    Key = {hour, IpAddress},
    case ets:lookup(rate_limit_hourly, Key) of
        [] ->
            ets:insert(rate_limit_hourly, {Key, 1}),
            ok;
        [{Key, Count}] when Count >= ?RATE_LIMIT_REQUESTS_PER_HOUR ->
            limited;
        [{Key, Count}] ->
            ets:update_counter(rate_limit_hourly, Key, 1),
            ok
    end.
```

**Step 2: Apply Rate Limiting to Endpoints**
```erlang
% In HTTP handler
handle_marketplace(Req, State) ->
    IpAddress = get_client_ip(Req),
    case tai_rate_limiter:check_limit(IpAddress) of
        ok ->
            % Proceed with request
            handle_marketplace_authenticated(...);
        {error, minute_limit_exceeded} ->
            cowboy_req:reply(429, #{
                <<"retry-after">> => <<"60">>,
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"rate_limit_exceeded">>}), Req),
            {ok, Req, State};
        {error, hour_limit_exceeded} ->
            cowboy_req:reply(429, #{
                <<"retry-after">> => <<"3600">>,
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"hourly_limit_exceeded">>}), Req),
            {ok, Req, State}
    end.

get_client_ip(Req) ->
    case cowboy_req:header(<<"x-forwarded-for">>, Req) of
        undefined ->
            {IP, _Port} = cowboy_req:peer(Req),
            inet:ntoa(IP);
        Header ->
            % Take first IP (before any proxies)
            [First | _] = binary:split(Header, <<",">>),
            string:trim(First)
    end.
```

**Step 3: Monitoring**
```erlang
% Monitor rate limit violations
monitor_rate_limits() ->
    case ets:tab2list(rate_limit_buckets) of
        Buckets ->
            Violations = [B || {_, Count} = B <- Buckets, Count >= 600],
            case length(Violations) > 10 of
                true ->
                    logger:warning("Rate limit violations: ~p IPs hitting limits",
                                  [length(Violations)]);
                false ->
                    ok
            end
    end.
```

**Timeline**: 3 days
**Owner**: Backend engineer
**Block for go-live**: YES

---

## Vulnerability 6: Poor Input Validation

**Severity**: 🔴 HIGH
**Status**: Implement after critical items
**Timeline**: 1 week

Validate:
- Tenant ID: UUID format
- Entitlement ID: UUID format
- Action: Whitelist [grant, revoke, suspend]
- Value: Numeric bounds

---

## Vulnerability 7: Missing Security Headers

**Severity**: 🟠 HIGH
**Status**: Implement after critical items
**Timeline**: 1 day

Add headers:
- X-Content-Type-Options: nosniff
- X-Frame-Options: DENY
- Strict-Transport-Security: max-age=31536000
- Content-Security-Policy: default-src 'none'

---

## Vulnerability 8: No Audit Logging

**Severity**: 🟠 MEDIUM-HIGH
**Status**: Implement after critical items
**Timeline**: 1 week

Log:
- Every API request (auth attempt)
- Every authorization decision
- Every receipt created
- Every error

---

## Implementation Schedule

```
WEEK 1:
├─ Day 1-2: Vulnerability 1 (TLS/HTTPS)
├─ Day 3-4: Vulnerability 5 (Rate limiting)
└─ Day 5: Testing & code review

WEEK 2:
├─ Day 1-3: Vulnerability 2 (Request authentication)
├─ Day 4-5: Testing & code review
└─ Pair: Vulnerability 3 (Signature verification)

WEEK 3:
├─ Day 1-3: Vulnerability 4 (Complete JWT validation)
├─ Day 4-5: Testing & code review
└─ Pair: Vulnerability 6 (Input validation)

WEEK 4:
├─ Day 1: Vulnerability 7 (Security headers)
├─ Day 2-3: Vulnerability 8 (Audit logging)
├─ Day 4: Integration testing (all 8 together)
└─ Day 5: Security review + penetration test

WEEK 5:
├─ Fix any findings from pen test
├─ Final review
└─ Staging deployment + validation
```

---

## Testing Strategy

### Unit Tests
- Each vulnerability has unit tests
- Verify: Exploit blocked, legitimate traffic allowed

### Integration Tests
- Test all 8 together
- Verify: No side effects or conflicts

### Security Tests
- Penetration testing (third-party)
- Verify: All vulnerabilities patched

### Load Tests
- Rate limiting accuracy under load
- Verify: Not dropping legitimate traffic

---

## Sign-Off Criteria

**Cannot proceed to revenue launch without:**

- [ ] All 8 vulnerabilities fixed
- [ ] All tests passing
- [ ] Code review approved (2+ reviewers)
- [ ] Penetration test passed (0 CRITICAL findings)
- [ ] Deployed to staging + validated
- [ ] Security team sign-off

---

## Risk Acceptance

If any vulnerability is NOT fixed before launch:
- Accept: 🔴 CRITICAL RISK - Company bankruptcy risk
- Recommendation: DO NOT ACCEPT - Fix all items

---

**Owner**: Security Lead
**Timeline**: 4-5 weeks
**Blocker**: YES - Cannot launch without all 8
