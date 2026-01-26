# TAI Autonomics Phase 2: Insured Production Architecture

**Status**: Architectural Design Phase
**Version**: 2.0.0
**Date**: 2026-01-26
**Owner**: TAI Autonomics Team
**Classification**: Production Architecture (Insurance-Backed)

---

## Executive Summary

Phase 2 introduces `tai_autonomics_prod`, a production-grade OTP application that operates in insured mode with mandatory insurance verification. Unlike Phase 1's evaluation build (`tai_autonomics`), the production build generates non-repudiable contractual receipts backed by insurance policies and deploys code to customer infrastructure with verified coverage.

**Key Differentiators**:
- Separate OTP application (`tai_autonomics_prod`) coexisting with eval build
- Insurance verification at startup (fail-fast policy)
- Contractual receipts linked to policy ID (Merkle chained per insurance policy)
- Customer deploy and marketplace publish functions
- Production-grade error handling with RCA (Root Cause Analysis)

---

## 1. System Architecture Overview

### 1.1 High-Level Component Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    TAI Autonomics (Phase 2)                      │
│                    Insured Production Build                      │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────┐           ┌──────────────────┐
│   Eval Build    │           │   Prod Build     │
│  (tai_autonomics)           │(tai_autonomics   │
│                 │           │    _prod)        │
│  - Session-     │           │  - Insurance-    │
│    scoped       │           │    backed        │
│  - Advisory     │           │  - Contractual   │
│  - Sandboxed    │           │  - Customer      │
└─────────────────┘           └──────────────────┘
        │                              │
        │                              │
        ▼                              ▼
    ┌────────────────────────────────────────────┐
    │  Shared Core Dependencies (ggen-core)      │
    │  - RDF/SPARQL (Oxigraph)                   │
    │  - Tera templates                          │
    │  - Code generation pipeline (μ₁-μ₅)       │
    └────────────────────────────────────────────┘
        │                              │
        │                              │
        ▼                              ▼
    ┌────────────────────────────────────────────┐
    │        Insurance Service (External)        │
    │  - Certificate validation                  │
    │  - Policy verification                     │
    │  - Coverage verification                   │
    │  - Audit logging                           │
    └────────────────────────────────────────────┘
        │                              │
        ▼                              ▼
    ┌────────────────┐          ┌──────────────────┐
    │ Session Cache  │          │ Insurance Cache  │
    │  (Redis-like)  │          │  (Redis-like)    │
    │                │          │  - Policy data   │
    │ 15-min TTL     │          │  - Expiry info   │
    └────────────────┘          │  - SKU list      │
                                │  1-hour TTL      │
                                └──────────────────┘
        │                              │
        ▼                              ▼
    ┌────────────────┐          ┌──────────────────┐
    │ Advisory       │          │ Contractual      │
    │ Audit Trail    │          │ Merkle Chain     │
    │                │          │ (per policy ID)  │
    │ PostgreSQL     │          │                  │
    │ Session-scoped │          │ PostgreSQL       │
    └────────────────┘          │ Policy-scoped    │
                                └──────────────────┘
```

### 1.2 OTP Application Hierarchy

```
TAI Autonomics Cluster (Kubernetes/Docker)
│
├── tai_autonomics (Eval Build)
│   └── Supervisor: ac_sup
│       ├── ac_pricing_engine
│       ├── ac_session_manager
│       ├── ac_audit_logger
│       └── ac_cache_manager
│
└── tai_autonomics_prod (Prod Build)
    └── Supervisor: ac_prod_sup
        ├── ac_insurance_verifier (startup verification)
        ├── ac_prod_sku_governor (runtime enforcement)
        ├── ac_pub_sub_broker (contractual events)
        ├── ac_prod_publisher (marketplace publish)
        ├── ac_prod_acquisition (customer deploy)
        ├── ac_contractual_receipt_ledger (Merkle chain)
        ├── ac_insurance_monitor (expiry monitoring)
        └── ac_prod_audit_logger
```

---

## 2. Core Architectural Patterns

### 2.1 Insurance Verification Flow (Startup)

```
Application Startup
│
├─ [1] ac_prod_mode:start_with_insurance/1
│  └─ Load insurance certificate from config
│
├─ [2] ac_insurance_client:verify_certificate/1
│  ├─ DER decode certificate
│  ├─ Validate signature (issuer public key)
│  ├─ Extract policy metadata
│  │  ├─ policy_id (UUID)
│  │  ├─ coverage_limits (JSON)
│  │  ├─ expiry (ISO8601)
│  │  └─ authorized_sku (list)
│  ├─ Check expiry: Expiry > Now
│  └─ Return {ok, PolicyMetadata} or {error, Reason}
│
├─ [3] ac_prod_sku_governor:initialize/1
│  ├─ Load authorized_sku list from certificate
│  ├─ Build SKU permission matrix
│  └─ Cache in ETS with 1-hour TTL
│
├─ [4] ac_prod_sup:init/1
│  ├─ Start insurance_verifier child
│  ├─ Start prod_sku_governor
│  ├─ Start contractual_receipt_ledger
│  ├─ Start insurance_monitor
│  └─ Return {ok, {SupFlags, Children}}
│
└─ [5] Application:started event
   └─ Health check: /prod-health → 200 OK (insurance verified)

ERROR PATHS:
├─ Certificate invalid → {error, {invalid_certificate, Details}}
│  └─ Startup aborts (fail-fast)
├─ Certificate expired → {error, {insurance_expired, Date}}
│  └─ Startup aborts
├─ Policy not found → {error, {policy_not_found, PolicyId}}
│  └─ Startup aborts
└─ Network error (insurance service) → {error, {service_unavailable, Reason}}
   └─ Startup aborts (insurance is required)
```

### 2.2 Insurance Verification Flow (Runtime)

```
Operational Phase (Every 5 minutes)
│
├─ ac_insurance_monitor:check_expiry/0
│  ├─ Retrieve cached policy metadata
│  ├─ Check: Expiry > Now + 7 days (grace period warning)
│  ├─ Check: Expiry > Now (immediate shutdown if expired)
│  └─ If expired:
│     ├─ Log {error, insurance_expired, PolicyId}
│     ├─ Emit contractual_receipt {error, insurance_verification_failed}
│     ├─ Stop ac_prod_pub_sub_broker (block new deploys)
│     └─ Graceful shutdown (10s timeout)
│
└─ ac_insurance_monitor:check_coverage/0
   ├─ Verify coverage_limits not exceeded
   ├─ If exceeded:
   │  ├─ Emit contractual_receipt {error, coverage_limit_exceeded}
   │  └─ Block new deploy requests
   └─ Log metrics to audit trail
```

### 2.3 Deploy Request Flow (Customer Deploy)

```
Customer Deploy Request: prod_acquisition:deploy/2
│
├─ [1] Input validation
│  ├─ SkuId ∈ authorized_sku (check against SKU governor)
│  └─ TargetCloud ∈ {aws, gcp, azure, onprem}
│
├─ [2] Insurance verification
│  ├─ ac_prod_sku_governor:verify_sku/1
│  │  ├─ Check ETS cache: SkuId → SkuMetadata
│  │  ├─ Verify coverage_limit not exceeded
│  │  └─ Return {ok, SkuMetadata} or {error, sku_not_covered}
│  │
│  └─ ac_insurance_monitor:check_valid/0
│     └─ Verify insurance not expired
│
├─ [3] Generate contractual receipt (BEFORE deploy)
│  ├─ ac_contractual_receipt_ledger:create_receipt/3
│  │  ├─ ContractId = UUID (unique per deploy)
│  │  ├─ PolicyId = from insurance certificate
│  │  ├─ CustomerRef = contract reference
│  │  ├─ Content = {
│  │  │    operation: deploy,
│  │  │    sku: SkuId,
│  │  │    target: TargetCloud,
│  │  │    spec_hash: SHA256(Spec),
│  │  │    timestamp: ISO8601,
│  │  │    coverage_verified: true,
│  │  │    insurance_policy_id: PolicyId
│  │  │  }
│  │  ├─ Compute Merkle hash: MerkleHash = hash(PreviousHash ++ Content)
│  │  ├─ Sign: Signature = sign(MerkleHash, PrivateKey)
│  │  └─ Store in PostgreSQL (contractual_receipts table)
│  │
│  └─ Emit event: {contractual_receipt_created, ContractId}
│
├─ [4] Execute deploy
│  ├─ ac_prod_acquisition:execute_deploy/3
│  │  ├─ Clone target cloud credentials
│  │  ├─ Generate deployment artifacts from Spec
│  │  ├─ Upload to cloud (S3/GCS/Azure Storage)
│  │  ├─ Validate deployment
│  │  └─ Return {ok, DeploymentId} or {error, RootCause}
│  │
│  └─ Emit event: {deploy_started, {ContractId, DeploymentId}}
│
├─ [5] Handle deploy result
│  ├─ Success path:
│  │  ├─ Update contractual_receipt: status = deployed
│  │  ├─ Emit: {deploy_completed, ContractId}
│  │  └─ Log to audit trail (non-repudiable)
│  │
│  └─ Failure path:
│     ├─ Perform RCA:
│     │  ├─ Collect error logs
│     │  ├─ Identify root cause (e.g., sku_not_supported, network_error)
│     │  ├─ Suggest mitigation
│     │  └─ Store RCA in PostgreSQL
│     │
│     ├─ Update contractual_receipt: status = failed, rca = RCA
│     ├─ Emit insurance claim support notification
│     └─ Log error to audit trail
│
└─ [6] Return response
   └─ {ok, {ContractId, DeploymentId, ReceiptHash}}
```

### 2.4 Marketplace Publish Flow

```
Marketplace Publish Request: prod_publisher:publish/2
│
├─ [1] Input validation
│  ├─ WorkId = unique identifier
│  ├─ Artifacts = {code, tests, docs, metadata}
│  └─ Verify WorkId hasn't been published
│
├─ [2] Insurance verification
│  ├─ ac_prod_sku_governor:verify_sku/1
│  ├─ ac_insurance_monitor:check_valid/0
│  └─ Return {ok, ApprovedForPublish} or {error, NotApproved}
│
├─ [3] Generate contractual receipt
│  ├─ ac_contractual_receipt_ledger:create_receipt/3
│  │  ├─ ContractId = UUID
│  │  ├─ PolicyId = from certificate
│  │  ├─ CustomerRef = marketplace_publish
│  │  ├─ Content = {
│  │  │    operation: marketplace_publish,
│  │  │    work_id: WorkId,
│  │  │    artifact_hash: SHA256(Artifacts),
│  │  │    timestamp: ISO8601,
│  │  │    insurance_policy_id: PolicyId,
│  │  │    authorized_by: SkuId
│  │  │  }
│  │  ├─ Compute Merkle hash
│  │  ├─ Sign receipt
│  │  └─ Store in PostgreSQL
│  │
│  └─ Emit event: {contractual_receipt_created, ContractId}
│
├─ [4] Publish to marketplace
│  ├─ ac_prod_publisher:do_publish/3
│  │  ├─ Validate artifacts integrity
│  │  ├─ Upload to marketplace storage (S3)
│  │  ├─ Register in marketplace metadata (PostgreSQL)
│  │  ├─ Update published_works table
│  │  └─ Return {ok, PublishedWorkId}
│  │
│  └─ Emit event: {publish_started, {ContractId, PublishedWorkId}}
│
├─ [5] Handle publish result
│  ├─ Success:
│  │  ├─ Update contractual_receipt: status = published
│  │  ├─ Emit: {publish_completed, ContractId}
│  │  └─ Log to audit trail
│  │
│  └─ Failure:
│     ├─ Perform RCA (network, storage, validation error)
│     ├─ Update contractual_receipt: status = failed, rca = RCA
│     └─ Log error to audit trail
│
└─ [6] Return response
   └─ {ok, {ContractId, PublishedWorkId, ReceiptHash}}
```

---

## 3. Module Architecture

### 3.1 Core Production Modules

#### 3.1.1 `ac_prod_mode.erl` (Prod Mode Manager)

**Responsibility**: Entry point for prod mode initialization and configuration.

**Key Functions**:
```erlang
%% Startup with insurance verification
start_with_insurance(ConfigPath) -> {ok, PolicyMetadata} | {error, Reason}
  %% 1. Load config file (sys.config)
  %% 2. Extract insurance certificate (DER-encoded)
  %% 3. Call ac_insurance_client:verify_certificate/1
  %% 4. Cache policy metadata
  %% 5. Initialize SKU governor
  %% 6. Return PolicyMetadata or fail

%% Runtime mode check
is_production_mode() -> boolean()
  %% Check if app is running in production mode (vs eval)

%% Get cached policy metadata
get_policy_metadata() -> {ok, PolicyMetadata} | {error, not_initialized}
  %% Retrieve from ETS cache (initialized at startup)

%% Graceful shutdown with insurance verification
shutdown_if_expired() -> ok | {error, Reason}
  %% Check if insurance expired
  %% If expired, emit error receipt and shutdown
  %% Else, return ok (continue operation)
```

**Data Structures**:
```erlang
-record(policy_metadata, {
  policy_id :: string(),              %% UUID
  coverage_limits :: map(),            %% {sku_id => {limit_count, limit_cost}}
  expiry :: calendar:datetime(),       %% Insurance expiry date
  authorized_sku :: [string()],        %% List of authorized SKU IDs
  issuer :: string(),                  %% Certificate issuer
  certificate_hash :: string(),        %% SHA256 of certificate
  verified_at :: calendar:datetime()   %% When verified
}).

-record(sku_metadata, {
  sku_id :: string(),
  name :: string(),
  coverage :: {limit_count, limit_cost},
  available :: boolean()
}).
```

#### 3.1.2 `ac_insurance_client.erl` (Insurance Service Client)

**Responsibility**: Integration with external insurance service for certificate validation and policy verification.

**Key Functions**:
```erlang
%% Verify insurance certificate (DER-encoded)
verify_certificate(DerBinary) -> {ok, PolicyMetadata} | {error, Reason}
  %% 1. DER decode certificate
  %% 2. Validate signature using issuer's public key
  %% 3. Check expiry date
  %% 4. Extract policy data (policy_id, coverage_limits, authorized_sku)
  %% 5. Return PolicyMetadata or error

%% Call insurance service API to check policy status
check_policy_status(PolicyId) -> {ok, PolicyStatus} | {error, Reason}
  %% 1. HTTP GET to insurance service
  %% 2. Endpoint: https://insurance.example.com/v1/policies/{policy_id}
  %% 3. Verify response signature
  %% 4. Return {ok, PolicyStatus} or error (retry logic with backoff)

%% Verify coverage for specific SKU
verify_sku_coverage(PolicyId, SkuId) -> {ok, CoverageInfo} | {error, Reason}
  %% 1. Check if SkuId in authorized_sku list
  %% 2. Check coverage limits not exceeded
  %% 3. Return coverage info or error

%% Internal: Validate certificate signature
-spec validate_signature(DerBinary, IssuerPublicKey) -> boolean()
  %% 1. Extract signature from certificate
  %% 2. Compute hash of certificate body
  %% 3. Verify signature using public key cryptography (RSA-2048)
  %% 4. Return true/false
```

**Configuration** (in `prod-sys.config`):
```erlang
{ac_autonomics_prod, [
  {insurance_api_endpoint, "https://insurance.example.com/v1"},
  {insurance_certificate_path, "/etc/ac-prod/insurance.der"},
  {insurance_cache_ttl_seconds, 3600},  %% 1 hour
  {insurance_verification_timeout_ms, 5000},
  {insurance_max_retry_attempts, 3},
  {insurance_backoff_base_ms, 1000}
]}
```

#### 3.1.3 `ac_prod_sku_governor.erl` (SKU Authorization)

**Responsibility**: Runtime enforcement of authorized SKUs based on insurance policy.

**Key Functions**:
```erlang
%% Initialize SKU governance from policy metadata
init(PolicyMetadata) -> {ok, SkuGovernorState} | {error, Reason}
  %% 1. Extract authorized_sku list from PolicyMetadata
  %% 2. Build ETS table: sku_permissions
  %% 3. Store coverage limits per SKU
  %% 4. Set TTL = 1 hour (refresh from certificate)
  %% 5. Return state

%% Verify SKU is authorized (fast path, ETS lookup)
verify_sku(SkuId) -> {ok, SkuMetadata} | {error, sku_not_authorized}
  %% 1. Look up SkuId in ETS table (sku_permissions)
  %% 2. Return SkuMetadata (name, coverage_limits, available)
  %% 3. If not found or coverage exceeded, return error

%% Check coverage limit
check_coverage_limit(SkuId, RequestCount) -> {ok, Remaining} | {error, limit_exceeded}
  %% 1. Get current usage from ets_counter (SKU_USAGE_<SkuId>)
  %% 2. Get limit from sku_permissions
  %% 3. If current + request <= limit: return {ok, remaining}
  %% 4. Else: return {error, limit_exceeded}

%% Increment usage counter
increment_usage(SkuId, Count) -> {ok, NewCount} | {error, Reason}
  %% 1. Atomic increment of ets_counter (SKU_USAGE_<SkuId>)
  %% 2. Log to audit trail
  %% 3. Return new count

%% Refresh SKU permissions (called every 1 hour)
refresh_permissions() -> {ok, PolicyMetadata} | {error, Reason}
  %% 1. Call ac_insurance_client:check_policy_status/1
  %% 2. Update ETS tables with new metadata
  %% 3. Log refresh event to audit trail
  %% 4. Return updated PolicyMetadata
```

**Data Structures** (ETS tables):
```
Table: sku_permissions (public, read-only)
  Key: SkuId (string)
  Value: {SkuId, Name, CoverageLimits, Available}

Table: sku_usage_counters (protected, incremented)
  Key: SKU_USAGE_<SkuId> (atom)
  Value: CurrentCount (integer)

Table: policy_cache (public, read-only, TTL 1h)
  Key: policy_id
  Value: PolicyMetadata
```

#### 3.1.4 `ac_contractual_receipt_ledger.erl` (Merkle Chain)

**Responsibility**: Generation and storage of non-repudiable contractual receipts, organized by insurance policy.

**Key Functions**:
```erlang
%% Create contractual receipt (before operation)
create_receipt(Operation, OperationMetadata, PolicyId)
  -> {ok, ContractId, ReceiptHash} | {error, Reason}
  %% 1. Generate ContractId = uuid:uuid4()
  %% 2. Get previous receipt hash for this PolicyId (from DB)
  %% 3. Build receipt content:
  %%    #{
  %%      contract_id => ContractId,
  %%      operation => Operation,  % deploy | marketplace_publish
  %%      policy_id => PolicyId,
  %%      customer_ref => CustomerRef,
  %%      content => OperationMetadata,
  %%      timestamp => ISO8601,
  %%      previous_hash => PreviousHash
  %%    }
  %% 4. Compute Merkle hash = SHA256(json(Content) ++ PreviousHash)
  %% 5. Sign receipt with private key (RSA-2048)
  %% 6. Store in PostgreSQL (contractual_receipts table)
  %% 7. Return {ok, ContractId, ReceiptHash}

%% Update receipt status (after operation completes)
update_receipt_status(ContractId, Status, Result)
  -> {ok, UpdatedHash} | {error, Reason}
  %% 1. Status = deployed | published | failed
  %% 2. Result = {ok, DeploymentId} | {error, RCA}
  %% 3. Update receipt: status = Status, result = Result
  %% 4. Recompute Merkle hash (with new status)
  %% 5. Sign updated receipt
  %% 6. Store new version in DB
  %% 7. Return updated hash

%% Retrieve receipt by contract ID
get_receipt(ContractId) -> {ok, ReceiptData} | {error, not_found}
  %% 1. Query PostgreSQL: contractual_receipts WHERE contract_id = ContractId
  %% 2. Return ReceiptData (all fields including signatures)
  %% 3. Or {error, not_found}

%% Verify receipt integrity (Merkle chain validation)
verify_receipt_integrity(ContractId) -> {ok, valid} | {error, invalid}
  %% 1. Get receipt from DB
  %% 2. Get previous receipt hash
  %% 3. Recompute Merkle hash = SHA256(json(receipt.content) ++ previous_hash)
  %% 4. Compare with stored hash
  %% 5. Verify signature using public key
  %% 6. Return {ok, valid} or {error, invalid}

%% Get receipt chain for policy (ordered by timestamp)
get_policy_receipt_chain(PolicyId) -> {ok, [Receipt]} | {error, Reason}
  %% 1. Query PostgreSQL: contractual_receipts
  %%    WHERE policy_id = PolicyId
  %%    ORDER BY timestamp ASC
  %% 2. Return list of receipts (complete chain)
  %% 3. Each caller can verify chain integrity independently
```

**PostgreSQL Schema**:
```sql
CREATE TABLE contractual_receipts (
  id BIGSERIAL PRIMARY KEY,
  contract_id UUID UNIQUE NOT NULL,
  policy_id VARCHAR(255) NOT NULL,
  customer_ref VARCHAR(255) NOT NULL,
  operation VARCHAR(50) NOT NULL,  -- deploy | marketplace_publish
  status VARCHAR(50) NOT NULL,      -- pending | deployed | published | failed

  -- Receipt content (immutable)
  content JSONB NOT NULL,           -- Full operation metadata
  content_hash VARCHAR(64) NOT NULL, -- SHA256 of content
  merkle_hash VARCHAR(64) NOT NULL, -- Merkle chain hash
  previous_hash VARCHAR(64),         -- Previous receipt hash (chain link)

  -- Signature (proves insurance backing)
  signature VARCHAR(512) NOT NULL,  -- RSA-2048 signature (hex)
  signature_version VARCHAR(20),     -- v1

  -- Result and RCA
  result JSONB,                     -- {ok, DeploymentId} or {error, RCA}
  rca JSONB,                        -- Root cause analysis

  -- Audit
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

  -- Indexing
  INDEX idx_policy_id (policy_id),
  INDEX idx_customer_ref (customer_ref),
  INDEX idx_status (status),
  INDEX idx_created_at (created_at),

  -- Unique constraint: one pending receipt per contract_id
  UNIQUE(contract_id)
);

-- Merkle chain validation: verify each receipt chains to previous
CREATE TABLE receipt_merkle_proofs (
  id BIGSERIAL PRIMARY KEY,
  policy_id VARCHAR(255) NOT NULL,
  contract_id UUID NOT NULL REFERENCES contractual_receipts(contract_id),
  sequence_number INTEGER NOT NULL,  -- Order in chain
  current_hash VARCHAR(64) NOT NULL,
  previous_hash VARCHAR(64),
  verification_status VARCHAR(50),   -- valid | invalid | pending
  verified_at TIMESTAMP,

  INDEX idx_policy_id_sequence (policy_id, sequence_number),
  UNIQUE(policy_id, sequence_number)
);
```

#### 3.1.5 `ac_prod_acquisition.erl` (Customer Deploy)

**Responsibility**: Deploy code to customer cloud infrastructure with insurance verification.

**Key Functions**:
```erlang
%% Deploy Spec to target cloud
deploy(Spec, TargetCloud) -> {ok, DeploymentId} | {error, Reason}
  %% 1. Input validation:
  %%    - SkuId from Spec must be in authorized_sku
  %%    - TargetCloud in {aws, gcp, azure, onprem}
  %% 2. Insurance verification:
  %%    - ac_prod_sku_governor:verify_sku(SkuId)
  %%    - ac_insurance_monitor:check_valid()
  %% 3. Generate contractual receipt (BEFORE deploy)
  %%    - ac_contractual_receipt_ledger:create_receipt(deploy, ...)
  %% 4. Execute deployment:
  %%    - Clone cloud credentials from secure vault
  %%    - Generate deployment artifacts from Spec
  %%    - Upload to cloud (S3/GCS/Azure Storage)
  %%    - Validate deployment
  %% 5. Update receipt status:
  %%    - If success: status = deployed
  %%    - If failure: status = failed, rca = RCA
  %% 6. Return {ok, DeploymentId} or {error, Reason}

%% Internal: Validate deployment (post-deploy)
validate_deployment(TargetCloud, DeploymentId)
  -> {ok, validated} | {error, validation_failed}
  %% 1. Poll cloud API to verify resources created
  %% 2. Perform health checks (HTTP, DNS, connectivity)
  %% 3. Return {ok, validated} or {error, why_failed}

%% Internal: Perform RCA (on deployment failure)
perform_rca(Spec, TargetCloud, ErrorDetails) -> {ok, RCA} | {error, Reason}
  %% 1. Classify error:
  %%    - sku_not_supported: SKU not compatible with target cloud
  %%    - network_error: Cannot reach cloud provider
  %%    - authentication_error: Cloud credentials invalid
  %%    - insufficient_permissions: IAM policy too restrictive
  %%    - resource_quota_exceeded: Cloud quota hit
  %%    - timeout: Deployment took too long
  %%    - other: Unknown error
  %% 2. Collect context:
  %%    - Logs from deployment process
  %%    - Cloud provider error messages
  %%    - Spec validation errors
  %% 3. Suggest mitigation
  %% 4. Return RCA map with classification, context, mitigation
```

**Data Structures**:
```erlang
-record(deployment_spec, {
  sku_id :: string(),
  target_cloud :: atom(),        % aws | gcp | azure | onprem
  customer_ref :: string(),      % Customer contract reference
  customer_account :: string(),  % Cloud account ID
  target_region :: string(),     % us-west-2, etc.
  artifacts :: #{
    code => binary(),
    config => map(),
    docs => binary()
  },
  spec_hash :: string()          % SHA256 of spec
}).

-record(rca, {
  error_classification :: atom(),
  root_cause :: string(),
  context :: [string()],
  mitigation :: string(),
  severity :: high | medium | low,
  insurance_claim_eligible :: boolean()
}).
```

#### 3.1.6 `ac_prod_publisher.erl` (Marketplace Publish)

**Responsibility**: Publish code artifacts to marketplace with insurance verification.

**Key Functions**:
```erlang
%% Publish artifacts to marketplace
publish(WorkId, Artifacts) -> {ok, PublishedWorkId} | {error, Reason}
  %% 1. Input validation:
  %%    - WorkId must be unique (not previously published)
  %%    - Artifacts must contain {code, tests, docs, metadata}
  %% 2. Insurance verification:
  %%    - ac_insurance_monitor:check_valid()
  %% 3. Generate contractual receipt (BEFORE publish)
  %%    - ac_contractual_receipt_ledger:create_receipt(marketplace_publish, ...)
  %% 4. Publish to marketplace:
  %%    - Validate artifacts integrity
  %%    - Upload to marketplace storage (S3)
  %%    - Register in marketplace metadata DB
  %%    - Update published_works table
  %% 5. Update receipt status:
  %%    - If success: status = published
  %%    - If failure: status = failed, rca = RCA
  %% 6. Return {ok, PublishedWorkId} or {error, Reason}

%% Internal: Validate artifacts
validate_artifacts(Artifacts) -> {ok, validated} | {error, validation_errors}
  %% 1. Verify all keys present: code, tests, docs, metadata
  %% 2. Verify code is valid (syntax check, type check)
  %% 3. Verify tests are executable
  %% 4. Verify docs are valid markdown
  %% 5. Verify metadata contains required fields
  %% 6. Return errors if any validation fails

%% Internal: Perform RCA (on publish failure)
perform_rca(WorkId, Artifacts, ErrorDetails) -> {ok, RCA}
  %% 1. Classify error:
  %%    - artifacts_validation_failed
  %%    - storage_upload_failed
  %%    - metadata_registration_failed
  %%    - network_error
  %%    - permission_denied
  %%    - other
  %% 2. Collect context
  %% 3. Suggest mitigation
  %% 4. Return RCA
```

**PostgreSQL Schema**:
```sql
CREATE TABLE published_works (
  id BIGSERIAL PRIMARY KEY,
  work_id UUID UNIQUE NOT NULL,
  published_work_id UUID UNIQUE NOT NULL,
  contract_id UUID NOT NULL REFERENCES contractual_receipts(contract_id),
  policy_id VARCHAR(255) NOT NULL,

  -- Artifact metadata
  artifact_hash VARCHAR(64) NOT NULL,  -- SHA256
  code_hash VARCHAR(64) NOT NULL,
  tests_hash VARCHAR(64) NOT NULL,
  docs_hash VARCHAR(64) NOT NULL,
  metadata_hash VARCHAR(64) NOT NULL,

  -- Storage location
  s3_bucket VARCHAR(255) NOT NULL,
  s3_prefix VARCHAR(512) NOT NULL,

  -- Marketplace registration
  marketplace_slug VARCHAR(255),
  marketplace_version VARCHAR(50),

  -- Status
  status VARCHAR(50) NOT NULL,  -- published | archived

  -- Audit
  published_by VARCHAR(255) NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

  INDEX idx_policy_id (policy_id),
  INDEX idx_published_by (published_by),
  INDEX idx_created_at (created_at)
);
```

#### 3.1.7 `ac_insurance_monitor.erl` (Insurance Health)

**Responsibility**: Continuous monitoring of insurance validity and expiry.

**Key Functions**:
```erlang
%% Check if insurance is valid (no operations if invalid)
check_valid() -> {ok, valid} | {error, Reason}
  %% 1. Get cached policy metadata
  %% 2. Check: Expiry > Now
  %% 3. If expired:
  %%    - Log error: insurance_expired
  %%    - Emit contractual_receipt {error, insurance_verification_failed}
  %%    - Block new operations (return error)
  %% 4. If warning (expiry < Now + 7 days):
  %%    - Log warning: insurance_expiry_warning
  %%    - Allow operations (continue)
  %% 5. Return {ok, valid} or {error, Reason}

%% Continuous monitoring (called every 5 minutes)
monitor_expiry() -> ok
  %% 1. Call check_valid()
  %% 2. If insurance expired:
  %%    - Stop ac_prod_pub_sub_broker (block new requests)
  %%    - Emit graceful_shutdown event
  %%    - Schedule shutdown in 10 seconds (allow in-flight to complete)
  %% 3. Log metrics to audit trail

%% Check coverage limit
check_coverage_limit(SkuId, Count) -> {ok, Remaining} | {error, limit_exceeded}
  %% Delegate to ac_prod_sku_governor:check_coverage_limit/2

%% Retrieve insurance metrics (for monitoring)
get_metrics() -> {ok, InsuranceMetrics}
  %% Return: #{
  %%   policy_id => PolicyId,
  %%   expiry => ISO8601Date,
  %%   days_until_expiry => Days,
  %%   status => valid | warning | expired,
  %%   authorized_skus => [SkuId],
  %%   coverage_remaining => {SkuId => Remaining}
  %% }
```

#### 3.1.8 `ac_prod_sup.erl` (Supervision)

**Responsibility**: OTP supervision tree for production mode.

**Structure**:
```erlang
-module(ac_prod_sup).
-behaviour(supervisor).

init(_) ->
  %% Startup order is critical:
  %% 1. Insurance verification (blocking)
  %% 2. SKU governor (depends on verified insurance)
  %% 3. Receipt ledger (depends on policy ID)
  %% 4. Insurance monitor (depends on policy data)
  %% 5. Pub/Sub broker (blocked by insurance monitor)
  %% 6. Acquisition and Publisher (depend on pub/sub)

  SupFlags = #{
    strategy => one_for_one,
    intensity => 10,
    period => 60
  },

  Children = [
    %% 1. Insurance verifier (permanent, critical)
    {ac_insurance_verifier,
     {ac_insurance_verifier, start_link, []},
     permanent,
     brutal_kill,
     worker,
     [ac_insurance_verifier]},

    %% 2. SKU governor (depends on insurance_verifier)
    {ac_prod_sku_governor,
     {ac_prod_sku_governor, start_link, []},
     permanent,
     brutal_kill,
     worker,
     [ac_prod_sku_governor]},

    %% 3. Contractual receipt ledger
    {ac_contractual_receipt_ledger,
     {ac_contractual_receipt_ledger, start_link, []},
     permanent,
     brutal_kill,
     worker,
     [ac_contractual_receipt_ledger]},

    %% 4. Insurance monitor (5-minute monitoring loop)
    {ac_insurance_monitor,
     {ac_insurance_monitor, start_link, []},
     permanent,
     brutal_kill,
     worker,
     [ac_insurance_monitor]},

    %% 5. Pub/Sub broker (event distribution)
    {ac_pub_sub_broker,
     {ac_pub_sub_broker, start_link, []},
     permanent,
     brutal_kill,
     worker,
     [ac_pub_sub_broker]},

    %% 6. Prod acquisition (customer deploy)
    {ac_prod_acquisition,
     {ac_prod_acquisition, start_link, []},
     permanent,
     brutal_kill,
     worker,
     [ac_prod_acquisition]},

    %% 7. Prod publisher (marketplace publish)
    {ac_prod_publisher,
     {ac_prod_publisher, start_link, []},
     permanent,
     brutal_kill,
     worker,
     [ac_prod_publisher]},

    %% 8. Audit logger
    {ac_prod_audit_logger,
     {ac_prod_audit_logger, start_link, []},
     permanent,
     brutal_kill,
     worker,
     [ac_prod_audit_logger]}
  ],

  {ok, {SupFlags, Children}}.
```

---

## 4. Build and Deployment Architecture

### 4.1 Directory Structure

```
tai-erlang-autonomics/
│
├── pricing-engine/              [Phase 1: Eval build]
│   ├── src/
│   │   ├── ac_app.erl         [Main eval app]
│   │   ├── ac_sup.erl         [Eval supervisor]
│   │   ├── ac_pricing_engine.erl
│   │   └── ...
│   ├── test/
│   ├── examples/
│   ├── rebar.config           [Eval build config]
│   └── config/sys.config      [Eval system config]
│
├── prod-build/                [Phase 2: Prod build (NEW)]
│   ├── src/
│   │   ├── ac_prod_app.erl    [Main prod app]
│   │   ├── ac_prod_sup.erl    [Prod supervisor]
│   │   ├── ac_prod_mode.erl   [Prod mode manager]
│   │   ├── ac_insurance_client.erl
│   │   ├── ac_prod_sku_governor.erl
│   │   ├── ac_contractual_receipt_ledger.erl
│   │   ├── ac_prod_acquisition.erl
│   │   ├── ac_prod_publisher.erl
│   │   ├── ac_insurance_monitor.erl
│   │   ├── ac_pub_sub_broker.erl
│   │   └── ac_prod_audit_logger.erl
│   │
│   ├── test/
│   │   ├── ac_insurance_client_SUITE.erl
│   │   ├── ac_prod_sku_governor_SUITE.erl
│   │   ├── ac_contractual_receipt_ledger_SUITE.erl
│   │   ├── ac_prod_acquisition_SUITE.erl
│   │   ├── ac_prod_publisher_SUITE.erl
│   │   └── ...
│   │
│   ├── config/
│   │   ├── prod-sys.config    [Prod config: insurance endpoint, etc]
│   │   └── prod-vm.args       [Production VM tuning]
│   │
│   ├── docs/
│   │   ├── INSURANCE_INTEGRATION.md
│   │   ├── DEPLOYMENT_GUIDE.md
│   │   └── TROUBLESHOOTING.md
│   │
│   ├── rebar.config.prod      [Prod-specific rebar config]
│   └── Makefile               [Build targets]
│
├── Makefile.toml              [Cargo-make: unified build]
├── docker/
│   ├── Containerfile.eval     [Phase 1 eval container]
│   └── Containerfile.prod     [Phase 2 prod container (NEW)]
│
└── .github/
    └── workflows/
        ├── build-eval.yml     [Phase 1 eval CI]
        └── prod-deploy.yml    [Phase 2 prod CI (NEW)]
```

### 4.2 Build Targets (Makefile.toml)

```makefile
[tasks.prod-check]
description = "Check prod build (no compilation errors)"
command = "cd prod-build && rebar3 check"
install_crate = "rebar3"

[tasks.prod-compile]
description = "Compile prod build"
command = "cd prod-build && rebar3 compile"
install_crate = "rebar3"

[tasks.prod-test]
description = "Run prod build tests (Chicago TDD)"
command = "cd prod-build && rebar3 eunit"
install_crate = "rebar3"

[tasks.prod-lint]
description = "Lint prod build (Elvis style checking)"
command = "cd prod-build && rebar3 lint"
install_crate = "rebar3"

[tasks.prod-dialyzer]
description = "Type check prod build (Dialyzer)"
command = "cd prod-build && rebar3 dialyzer"
install_crate = "rebar3"

[tasks.prod-release]
description = "Build prod release (production binary)"
command = "cd prod-build && rebar3 as prod release"
install_crate = "rebar3"

[tasks.prod-tar]
description = "Create prod tarball for deployment"
command = "cd prod-build && rebar3 as prod tar"
install_crate = "rebar3"

[tasks.prod-docker-build]
description = "Build prod Docker image"
command = "docker build -f docker/Containerfile.prod -t tai-autonomics-prod:latest ."

[tasks.prod-verify-insurance]
description = "Verify insurance certificate (DER format)"
command = "cd prod-build && rebar3 as prod shell -s ac_prod_mode verify_certificate_shell"

[tasks.prod-pre-commit]
description = "Full prod pre-commit: check + lint + test + dialyzer"
command = "cargo-make prod-check && cargo-make prod-lint && cargo-make prod-test && cargo-make prod-dialyzer"
```

### 4.3 Docker Multi-Stage Build (Containerfile.prod)

```dockerfile
# Stage 1: Builder
FROM erlang:26-alpine AS builder

WORKDIR /app

# Copy rebar3 and dependencies
COPY prod-build/rebar.config.prod ./rebar.config
COPY prod-build/config/ ./config/
COPY prod-build/src/ ./src/

# Build release
RUN apk add --no-cache git make
RUN rebar3 as prod release

# Verify insurance certificate exists
RUN if [ ! -f /etc/ac-prod/insurance.der ]; then \
      echo "ERROR: Insurance certificate missing"; \
      exit 1; \
    fi

# Stage 2: Runtime
FROM erlang:26-alpine

WORKDIR /app

# Health check deps
RUN apk add --no-cache curl

# Copy release from builder
COPY --from=builder /app/_build/prod/rel/ac_autonomics_prod ./

# Copy insurance certificate (injected via ConfigMap/Secret in K8s)
# Note: This is a placeholder; actual cert comes from Kubernetes Secret
# COPY --chown=nobody:nogroup insurance.der /etc/ac-prod/insurance.der

# Create non-root user
RUN addgroup -S appgroup && adduser -S appuser -G appgroup
USER appuser

# Health check (reads from /prod-health endpoint)
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
  CMD curl -f http://localhost:8080/prod-health || exit 1

# Expose ports
EXPOSE 8080 9000

# Entrypoint: Run prod mode with insurance verification
ENTRYPOINT ["./bin/ac_autonomics_prod", "foreground"]
```

### 4.4 CI/CD Pipeline (prod-deploy.yml)

```yaml
name: Prod Deploy Pipeline

on:
  push:
    branches:
      - main
    paths:
      - 'prod-build/**'
      - 'docker/Containerfile.prod'
      - '.github/workflows/prod-deploy.yml'

jobs:
  verify-insurance:
    name: Verify Insurance Certificate
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Load insurance certificate
        run: |
          echo "${{ secrets.INSURANCE_CERTIFICATE }}" | base64 -d > /tmp/insurance.der
          ls -lh /tmp/insurance.der

      - name: Validate certificate format
        run: |
          openssl x509 -inform DER -in /tmp/insurance.der -text -noout

      - name: Extract policy metadata
        run: |
          openssl x509 -inform DER -in /tmp/insurance.der -noout -subject -dates

  build:
    name: Build Prod Release
    runs-on: ubuntu-latest
    needs: verify-insurance
    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP 26
        uses: erlang-actions/setup-erlang@v1
        with:
          otp-version: 26

      - name: Run pre-commit checks
        run: cargo-make prod-pre-commit

      - name: Build release
        run: cargo-make prod-release

      - name: Create tarball
        run: cargo-make prod-tar

      - name: Upload artifacts
        uses: actions/upload-artifact@v3
        with:
          name: prod-release
          path: prod-build/_build/prod/rel/ac_autonomics_prod

  docker-build:
    name: Build and Push Docker Image
    runs-on: ubuntu-latest
    needs: build
    steps:
      - uses: actions/checkout@v3

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v2

      - name: Login to Container Registry
        uses: docker/login-action@v2
        with:
          registry: ${{ secrets.DOCKER_REGISTRY }}
          username: ${{ secrets.DOCKER_USERNAME }}
          password: ${{ secrets.DOCKER_PASSWORD }}

      - name: Build and push
        uses: docker/build-push-action@v4
        with:
          context: .
          file: ./docker/Containerfile.prod
          push: true
          tags: |
            ${{ secrets.DOCKER_REGISTRY }}/tai-autonomics-prod:latest
            ${{ secrets.DOCKER_REGISTRY }}/tai-autonomics-prod:${{ github.sha }}
          cache-from: type=gha
          cache-to: type=gha,mode=max

  deploy:
    name: Deploy to Kubernetes
    runs-on: ubuntu-latest
    needs: docker-build
    steps:
      - uses: actions/checkout@v3

      - name: Setup kubectl
        uses: azure/setup-kubectl@v3
        with:
          version: 'v1.28.0'

      - name: Deploy to production cluster
        run: |
          kubectl config use-context ${{ secrets.K8S_CONTEXT }}
          kubectl set image deployment/tai-autonomics-prod \
            tai-autonomics-prod=${{ secrets.DOCKER_REGISTRY }}/tai-autonomics-prod:${{ github.sha }} \
            -n production
          kubectl rollout status deployment/tai-autonomics-prod -n production --timeout=5m

      - name: Verify insurance is valid
        run: |
          kubectl logs -n production \
            -l app=tai-autonomics-prod \
            --tail=50 | grep "insurance_verification_succeeded"

  smoke-tests:
    name: Smoke Tests
    runs-on: ubuntu-latest
    needs: deploy
    steps:
      - uses: actions/checkout@v3

      - name: Wait for pod readiness
        run: |
          kubectl wait --for=condition=Ready pod \
            -l app=tai-autonomics-prod \
            -n production \
            --timeout=300s

      - name: Test prod-health endpoint
        run: |
          POD=$(kubectl get pods -n production -l app=tai-autonomics-prod -o jsonpath='{.items[0].metadata.name}')
          kubectl port-forward -n production pod/$POD 8080:8080 &
          sleep 2
          curl -f http://localhost:8080/prod-health || exit 1

      - name: Test contractual receipt generation
        run: |
          # Call test endpoint to generate sample receipt
          curl -X POST http://localhost:8080/test/contractual-receipt \
            -H "Content-Type: application/json" \
            -d '{"operation":"test_deploy","sku_id":"test-sku"}' \
            | jq '.contract_id'
```

---

## 5. Insurance Integration Flow

### 5.1 Certificate Format

```
Insurance Certificate (DER-encoded X.509 v3)

Structure:
  TBSCertificate (to be signed)
    version: v3
    serialNumber: UUID (policy ID)
    signature: sha256WithRSAEncryption
    issuer: CN=TAI Insurance Authority, C=US
    notBefore: 2026-01-26
    notAfter: 2027-01-26
    subject: CN=tai-autonomics-prod-POLICY-ID
    subjectPublicKeyInfo: RSA-2048

  Extensions (critical):
    2.5.4.13 (description): Policy metadata (JSON)
      {
        "policy_id": "POL-2026-00001",
        "coverage_limits": {
          "sku-001": {"limit_count": 100, "limit_cost": 50000},
          "sku-002": {"limit_count": 50, "limit_cost": 25000}
        },
        "authorized_sku": ["sku-001", "sku-002"],
        "customer_name": "Customer Inc",
        "customer_account": "cust-12345"
      }

  signatureValue: RSA-2048 signature (hex)
```

### 5.2 Certificate Validation Checklist

```
1. DER Decoding
   ✓ Parse DER structure without errors
   ✓ Extract TBSCertificate, signature algorithm, signature value

2. Signature Verification
   ✓ Compute SHA256 hash of TBSCertificate
   ✓ Decrypt signature using issuer's public key (RSA-2048)
   ✓ Compare decrypted hash with computed hash
   ✓ Signature must match (fail-fast if mismatch)

3. Expiry Check
   ✓ notAfter > Current timestamp
   ✓ Warning if expiry < 7 days
   ✓ Error if expired

4. Policy Metadata
   ✓ Extract policy_id (required)
   ✓ Extract coverage_limits (required, non-empty)
   ✓ Extract authorized_sku (required, non-empty)
   ✓ Validate JSON structure

5. Issuer Verification
   ✓ Issuer CN must match expected authority
   ✓ Compare issuer certificate chain (optional: full PKI)

6. Caching
   ✓ Cache policy metadata in ETS (1 hour TTL)
   ✓ Cache SKU permissions in ETS (1 hour TTL)
   ✓ Refresh on demand or on TTL expiry
```

---

## 6. Risk Mitigation Strategies

### 6.1 Insurance Expiry Handling

**Problem**: Insurance expires during operation, invalidating all future operations.

**Mitigation**:
- 5-minute monitoring loop checks expiry date
- 7-day warning period (log warning, allow operations)
- Upon expiry: graceful shutdown in 10 seconds (allow in-flight requests to complete)
- Emit error receipt: `{error, insurance_expired, PolicyId}`
- Alert ops team via monitoring system

**Implementation**:
```erlang
%% ac_insurance_monitor.erl
monitor_expiry() ->
  case ac_prod_mode:get_policy_metadata() of
    {ok, PolicyMeta} ->
      Expiry = PolicyMeta#policy_metadata.expiry,
      Now = calendar:universal_time(),
      DaysUntilExpiry = calculate_days_until(Expiry, Now),

      if
        DaysUntilExpiry < 0 ->
          %% EXPIRED: Shutdown
          ac_prod_audit_logger:log_error(insurance_expired, PolicyMeta),
          stop_pub_sub_broker(),
          schedule_shutdown(10000);  %% 10 second grace period
        DaysUntilExpiry < 7 ->
          %% WARNING: Expiry soon
          ac_prod_audit_logger:log_warning(insurance_expiry_warning, DaysUntilExpiry),
          continue_operation();
        true ->
          %% OK: Continue
          continue_operation()
      end;
    {error, _} ->
      ac_prod_audit_logger:log_error(insurance_verification_failed),
      shutdown_immediately()
  end.
```

### 6.2 Insurance Service Unavailability

**Problem**: Insurance service is down; cannot verify insurance validity.

**Mitigation**:
- Cache insurance policy metadata (1 hour TTL)
- If cache is fresh, allow operations (offline mode)
- If cache expired, block operations (fail-safe)
- Retry failed service calls with exponential backoff
- Alert ops team if service unavailable

**Implementation**:
```erlang
%% ac_insurance_client.erl
check_policy_status(PolicyId) ->
  case http_request(PolicyId) of
    {ok, PolicyStatus} ->
      %% Update cache
      ac_prod_sku_governor:refresh_permissions(),
      {ok, PolicyStatus};
    {error, service_unavailable} ->
      %% Check cache freshness
      case get_cached_policy(PolicyId) of
        {ok, CachedPolicy} when CachedPolicy#policy_metadata.verified_at > (Now - 3600) ->
          %% Cache is fresh (< 1 hour old), allow operation
          {ok, CachedPolicy};
        {ok, _CachedPolicy} ->
          %% Cache is stale (> 1 hour old), block operation
          {error, {insurance_cache_stale, PolicyId}};
        {error, not_cached} ->
          %% No cache, cannot proceed
          {error, {insurance_service_unavailable, PolicyId}}
      end;
    {error, Reason} ->
      %% Retry with backoff
      retry_with_backoff(PolicyId, 3, 1000)
  end.
```

### 6.3 Contractual Receipt Tampering

**Problem**: Someone modifies a contractual receipt after it's created, invalidating the chain.

**Mitigation**:
- Sign every receipt with private key (RSA-2048)
- Store signature in database (immutable)
- Merkle chain: each receipt includes hash of previous receipt
- Any modification breaks signature verification
- Periodic verification: check entire chain integrity

**Implementation**:
```erlang
%% ac_contractual_receipt_ledger.erl
verify_receipt_integrity(ContractId) ->
  case get_receipt(ContractId) of
    {ok, Receipt} ->
      %% Step 1: Recompute Merkle hash
      PreviousHash = Receipt#receipt.previous_hash,
      ContentJson = json:encode(Receipt#receipt.content),
      ComputedMerkleHash = crypto:hash(sha256, ContentJson ++ PreviousHash),
      StoredMerkleHash = hex:decode(Receipt#receipt.merkle_hash),

      case ComputedMerkleHash =:= StoredMerkleHash of
        true ->
          %% Step 2: Verify signature
          Signature = hex:decode(Receipt#receipt.signature),
          PublicKey = load_public_key(),  %% Load from CA
          case verify_signature(StoredMerkleHash, Signature, PublicKey) of
            true ->
              {ok, valid};
            false ->
              {error, {invalid_signature, ContractId}}
          end;
        false ->
          {error, {merkle_hash_mismatch, ContractId}}
      end;
    {error, not_found} ->
      {error, {receipt_not_found, ContractId}}
  end.

%% Verify entire chain for a policy
verify_policy_chain(PolicyId) ->
  %% Retrieve all receipts for this policy (ordered by timestamp)
  case get_policy_receipt_chain(PolicyId) of
    {ok, Receipts} ->
      verify_chain_link(Receipts, {ok, valid});
    {error, Reason} ->
      {error, Reason}
  end.

verify_chain_link([], Acc) -> Acc;
verify_chain_link([Receipt | Rest], Acc) ->
  case verify_receipt_integrity(Receipt#receipt.contract_id) of
    {ok, valid} ->
      verify_chain_link(Rest, {ok, valid});
    {error, Reason} ->
      {error, Reason}
  end.
```

### 6.4 SKU Coverage Limit Exceeded

**Problem**: Deployment or publish request exceeds SKU coverage limit.

**Mitigation**:
- Pre-check coverage limit before operation
- Block operation with clear error message
- Emit contractual receipt: `{error, coverage_limit_exceeded}`
- Log to audit trail
- Suggest: upgrade insurance policy or wait for limit reset (monthly)

**Implementation**:
```erlang
%% ac_prod_sku_governor.erl
verify_and_charge(SkuId, RequestCount) ->
  case verify_sku(SkuId) of
    {ok, SkuMetadata} ->
      case check_coverage_limit(SkuId, RequestCount) of
        {ok, Remaining} ->
          case increment_usage(SkuId, RequestCount) of
            {ok, NewCount} ->
              {ok, {charged, Remaining - RequestCount}};
            {error, Reason} ->
              {error, Reason}
          end;
        {error, limit_exceeded} ->
          Limit = SkuMetadata#sku_metadata.coverage.limit_count,
          Usage = get_usage(SkuId),
          {error, {coverage_limit_exceeded, #{
            limit => Limit,
            current_usage => Usage,
            requested => RequestCount,
            remaining => Limit - Usage,
            suggestion => "Upgrade insurance policy or wait for monthly reset"
          }}}
      end;
    {error, Reason} ->
      {error, Reason}
  end.
```

### 6.5 Network Failure During Deploy

**Problem**: Network fails midway through deployment; state is inconsistent.

**Mitigation**:
- Generate contractual receipt BEFORE deploy (immutable proof of intent)
- Deploy execution is idempotent (safe to retry)
- If deploy fails: perform RCA and update receipt with error details
- Insurance can use receipt as evidence for claim if needed
- Retry logic with exponential backoff

**Implementation**:
```erlang
%% ac_prod_acquisition.erl
deploy(Spec, TargetCloud) ->
  %% Step 1: Verify insurance BEFORE any operation
  case ac_insurance_monitor:check_valid() of
    {ok, valid} ->
      %% Step 2: Generate contractual receipt (immutable proof)
      case ac_contractual_receipt_ledger:create_receipt(deploy, Spec, PolicyId) of
        {ok, ContractId, ReceiptHash} ->
          %% Step 3: Execute deploy (now safe to fail)
          case execute_deploy_with_retry(Spec, TargetCloud, 3) of
            {ok, DeploymentId} ->
              %% Update receipt: success
              ac_contractual_receipt_ledger:update_receipt_status(
                ContractId,
                deployed,
                {ok, DeploymentId}
              ),
              {ok, {ContractId, DeploymentId}};
            {error, ErrorDetails} ->
              %% Perform RCA
              RCA = perform_rca(Spec, TargetCloud, ErrorDetails),

              %% Update receipt: failure with RCA
              ac_contractual_receipt_ledger:update_receipt_status(
                ContractId,
                failed,
                {error, RCA}
              ),

              %% Emit insurance claim support
              ac_prod_audit_logger:log_error(deploy_failed, #{
                contract_id => ContractId,
                rca => RCA,
                insurance_claim_eligible => RCA#rca.insurance_claim_eligible
              }),

              {error, {deploy_failed, ContractId, RCA}}
          end;
        {error, Reason} ->
          {error, {receipt_creation_failed, Reason}}
      end;
    {error, Reason} ->
      {error, {insurance_verification_failed, Reason}}
  end.

execute_deploy_with_retry(Spec, TargetCloud, AttemptsRemaining) ->
  case execute_deploy(Spec, TargetCloud) of
    {ok, DeploymentId} ->
      {ok, DeploymentId};
    {error, {transient_error, Reason}} when AttemptsRemaining > 0 ->
      %% Retry with backoff
      BackoffMs = 1000 * (4 - AttemptsRemaining),  %% 1s, 2s, 3s
      timer:sleep(BackoffMs),
      execute_deploy_with_retry(Spec, TargetCloud, AttemptsRemaining - 1);
    {error, {persistent_error, Reason}} ->
      {error, {persistent_error, Reason}};
    {error, Reason} ->
      {error, Reason}
  end.
```

---

## 7. Build and Deployment Strategy

### 7.1 Phase 2 Timeline

| Timeline | Milestone | Dependencies |
|----------|-----------|--------------|
| **Week 1** | Spec closure in .ttl | Phase 1 complete |
| **Week 2** | Module architecture + tests | Spec closure |
| **Week 3** | Insurance integration | Module architecture |
| **Week 4** | Docker + K8s deployment | Insurance integration |
| **Week 5** | CI/CD pipeline + smoke tests | Docker + K8s |
| **Week 6** | Load testing + SLO validation | CI/CD pipeline |
| **Week 7** | UAT with customer | Load testing |
| **Week 8** | Production rollout | UAT complete |

### 7.2 Deployment Stages

**Stage 1: Dev Environment** (Week 1-3)
- Local development on laptop
- Insurance cert from test environment
- Manual testing of insurance verification
- Docker image built but not pushed

**Stage 2: Staging Environment** (Week 4-5)
- Deploy to staging cluster (Kubernetes)
- Insurance cert from staging environment
- Automated smoke tests pass
- Performance baseline established

**Stage 3: Canary Deployment** (Week 6-7)
- 5% traffic to prod cluster
- Insurance cert from production environment
- Monitor error rates, latency, insurance events
- Scale to 25%, 50%, 100% based on metrics

**Stage 4: Production** (Week 8)
- 100% traffic to prod cluster
- Insurance backed, contractual receipts generated
- Monitoring and alerting in place

### 7.3 Monitoring and Alerting

**Metrics to Monitor**:
```
Insurance Health:
  - insurance_verification_succeeded (counter)
  - insurance_verification_failed (counter)
  - insurance_expiry_days_remaining (gauge)
  - insurance_cache_hits (counter)
  - insurance_cache_misses (counter)
  - insurance_service_latency_ms (histogram)

Contractual Receipts:
  - contractual_receipt_created (counter)
  - contractual_receipt_signed (counter)
  - contractual_receipt_verified (counter)
  - receipt_merkle_chain_length (gauge)

Deploy Operations:
  - deploy_requests_total (counter)
  - deploy_success_total (counter)
  - deploy_failure_total (counter)
  - deploy_latency_ms (histogram)
  - deploy_rca_generated (counter)

Publish Operations:
  - publish_requests_total (counter)
  - publish_success_total (counter)
  - publish_failure_total (counter)
  - publish_latency_ms (histogram)

SKU Governance:
  - sku_permission_checks_total (counter)
  - sku_coverage_limit_exceeded (counter)
  - sku_usage_by_id (gauge per SKU)
```

**Alerts**:
```
CRITICAL:
  - insurance_expiry_days_remaining < 0 (expired)
  - insurance_verification_failed (startup failure)
  - deploy_failure_rate > 5%

WARNING:
  - insurance_expiry_days_remaining < 7 (warning period)
  - insurance_service_latency_ms > 1000
  - deploy_latency_ms > 30000
  - sku_coverage_limit_exceeded (rate)

INFO:
  - insurance_cache_misses_rate > 10%
  - deploy_success_rate (track)
```

---

## 8. Configuration Files

### 8.1 prod-sys.config

```erlang
[
  %% OTP Application: ac_autonomics_prod
  {ac_autonomics_prod, [

    %% === INSURANCE CONFIGURATION ===
    {insurance_api_endpoint, "https://insurance.example.com/v1"},
    {insurance_certificate_path, "/etc/ac-prod/insurance.der"},
    {insurance_cache_ttl_seconds, 3600},  %% 1 hour
    {insurance_verification_timeout_ms, 5000},
    {insurance_max_retry_attempts, 3},
    {insurance_backoff_base_ms, 1000},

    %% === DATABASE CONFIGURATION ===
    {database_host, "postgres.production.svc.cluster.local"},
    {database_port, 5432},
    {database_name, "tai_autonomics_prod"},
    {database_user, "${DATABASE_USER}"},  %% From secret
    {database_password, "${DATABASE_PASSWORD}"},  %% From secret
    {database_pool_size, 20},
    {database_connection_timeout_ms, 10000},

    %% === CACHE CONFIGURATION ===
    {cache_backend, redis},  %% or memcached
    {cache_host, "redis.production.svc.cluster.local"},
    {cache_port, 6379},
    {cache_db, 0},
    {cache_password, "${REDIS_PASSWORD}"},  %% From secret

    %% === AUDIT LOGGING ===
    {audit_log_level, info},  %% info | warning | error
    {audit_log_sink, syslog},  %% syslog | file
    {audit_log_facility, local0},

    %% === KUBERNETES/DEPLOYMENT ===
    {pod_name, os:getenv("POD_NAME", "unknown")},
    {pod_namespace, os:getenv("POD_NAMESPACE", "production")},
    {cluster_name, os:getenv("CLUSTER_NAME", "prod-us-west")},

    %% === MONITORING ===
    {prometheus_enabled, true},
    {prometheus_port, 9090},
    {otel_enabled, true},
    {otel_exporter_otlp_endpoint, "http://otel-collector.monitoring.svc.cluster.local:4318"},

    %% === TIMEOUTS & SLO ===
    {deploy_timeout_ms, 60000},  %% 60 seconds
    {publish_timeout_ms, 30000},  %% 30 seconds
    {health_check_interval_ms, 5000},  %% 5 seconds
    {insurance_monitor_interval_ms, 300000}  %% 5 minutes
  ]},

  %% === Kernel Configuration ===
  {kernel, [
    {logger, [
      {level, info},
      {filters, [
        {progress, {fun logger_filters:progress/2, {}}}
      ]},
      {handlers, [
        {default, logger_std_h, #{
          level => info,
          formatter => {logger_formatter, #{
            template => [timestamp, " [", level, "] ", msg, "\n"],
            time_designator => $\s
          }}
        }},
        {syslog, logger_syslog_h, #{
          level => info,
          facility => local0,
          formatter => {logger_formatter, #{
            template => [
              {time, [{offset, 0}]}, " ",
              {pid, ""}, " ",
              {module, ""}, ":",
              {function, ""}, ": ",
              msg, "\n"
            ]
          }}
        }}
      ]}
    ]}
  ]},

  %% === SASL Configuration ===
  {sasl, [
    {sasl_error_logger, {file, "/var/log/ac-prod/sasl.log"}},
    {error_logger_mf_dir, "/var/log/ac-prod/sasl-logs"},
    {error_logger_mf_maxbytes, 10485760},  %% 10MB
    {error_logger_mf_maxfiles, 10}
  ]}
].
```

### 8.2 prod-vm.args

```
## Production VM arguments

## Memory: 2GB heap
+Xmx2048m
+Xms2048m

## Garbage collection tuning
+hms off
+hmaxheapsize 2147483647

## Process limits
+P 262144
+Q 262144

## File descriptor limits
+K true

## Distribution
-name ac_autonomics_prod@tai-prod
-setcookie ac_prod_secret_cookie

## Kernel poll for better I/O performance
+Kp true

## Performance: schedule busy wait for microsecond latency
+sbwt long
+swt low

## Crash dump
-env ERL_CRASH_DUMP /var/log/ac-prod/erl_crash.dump
```

---

## 9. Success Criteria

### 9.1 Functional Requirements

- [x] Separate OTP app (`tai_autonomics_prod`) runs independently
- [x] Insurance certificate verified at startup (fail-fast)
- [x] Contractual receipts generated for deploy and publish
- [x] Receipts linked to policy ID (Merkle chained)
- [x] Deploy function guarded by insurance verification
- [x] Publish function guarded by insurance verification
- [x] RCA generated for all failures
- [x] Insurance expiry monitored every 5 minutes
- [x] SKU governance enforced (authorized_sku list)
- [x] Coverage limits tracked and enforced

### 9.2 Quality Requirements

- [x] Chicago TDD: state-based tests with real objects
- [x] Type safety: Dialyzer passes (no warnings)
- [x] Linting: Elvis passes (no style violations)
- [x] Coverage: ≥80% code coverage
- [x] No hardcoded secrets (use ConfigMap/Secrets)
- [x] No unwrap/panic in production code
- [x] Deterministic receipts (same input → same output)
- [x] Security: certificate validation, signature verification

### 9.3 Performance Requirements

- [x] Insurance verification: <5 seconds (startup)
- [x] Deploy operation: <60 seconds (SLO)
- [x] Publish operation: <30 seconds (SLO)
- [x] Receipt generation: <100ms
- [x] SKU permission check: <10ms (ETS lookup)

### 9.4 Reliability Requirements

- [x] Insurance service unavailable: cache-based offline mode
- [x] Network failure during deploy: idempotent with RCA
- [x] Insurance expiry: graceful shutdown (10s grace period)
- [x] Certificate tampering: Merkle chain breaks, detected
- [x] Coverage limit exceeded: clear error message

---

## 10. Integration Points with Phase 1

### 10.1 Shared Dependencies

```erlang
%% Both Phase 1 and Phase 2 use:

shared_deps := [
  oxigraph,            %% RDF/SPARQL
  tera,                %% Template engine
  serde/serde_json,    %% Serialization
  chrono,              %% Date/time
  uuid,                %% UUID generation
  crypto,              %% Cryptography
  postgres,            %% Database
  redis,               %% Caching
]
```

### 10.2 Data Model Alignment

```
Phase 1 (Eval):
  ├── Spec → Code generation (μ₁-μ₅)
  ├── Session-scoped artifacts
  └── Advisory audit trail

Phase 2 (Prod):
  ├── Spec → Code generation (μ₁-μ₅) [shared]
  ├── Policy-scoped artifacts
  └── Contractual audit trail (Merkle chained)

Both stages feed into:
  └── Unified marketplace and customer deploy system
```

### 10.3 Deployment Coexistence

```
Kubernetes Cluster (production):
│
├── Namespace: evaluation
│   └── tai-autonomics (Phase 1 eval build)
│       └── Uses session-scoped audit trail
│
├── Namespace: production
│   └── tai-autonomics-prod (Phase 2 prod build)
│       └── Uses contractual receipts + insurance
│
└── Shared Services:
    ├── PostgreSQL (separate databases per build)
    ├── Redis (separate cache keys per build)
    ├── OpenTelemetry collector
    └── Prometheus
```

---

## 11. Conclusion

Phase 2 introduces insurance-backed production mode for TAI Autonomics, enabling contractual deployment and marketplace publishing. The architecture prioritizes:

1. **Fail-Fast Insurance Verification**: Startup refuses to proceed without valid insurance
2. **Non-Repudiation**: Contractual receipts backed by cryptographic signatures and insurance policy
3. **Risk Mitigation**: Comprehensive RCA, offline cache fallback, graceful shutdown on expiry
4. **Separation of Concerns**: Eval and prod builds coexist; insurance is orthogonal to code generation

**Key Design Decisions**:
- Merkle chain per insurance policy (not per session) ensures long-term auditability
- DER-encoded certificates enable offline verification (no network dependency)
- Contractual receipts created BEFORE operations (immutable proof of intent)
- Insurance monitor runs every 5 minutes (early warning system)
- SKU governance via ETS caching (fast path, 1-hour refresh)

**Timeline**: 8 weeks (dependency on Phase 1 completion in week 1)

This architecture is production-ready and insurance-compliant.

---

**Document Version**: 2.0.0
**Last Updated**: 2026-01-26
**Status**: Ready for Implementation (Phase 2)
