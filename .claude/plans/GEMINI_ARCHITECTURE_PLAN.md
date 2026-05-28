# Gemini Integration Architecture for stpnt v30.1.1

## Executive Summary

This document designs how to integrate gemini (Google's LLM) into stpnt v30.1.1, the Stewards of the Pentecost receipt-based stewardship system. The integration must preserve **determinism guarantees** (identical inputs → identical receipts), maintain **Chicago TDD** standards (real API calls, no mocks), and embed gemini evidence into BLAKE3+Ed25519 receipt chains.

**Core Constraint**: stpnt's receipts are cryptographically signed, deterministic proof objects. Gemini outputs are non-deterministic. The solution gatekeeps gemini suggestions through a deterministic validator layer, ensuring the receipt accurately reflects what happened.

---

## Part 1: Architecture Overview

### 1.1 Current stpnt Architecture

stpnt v30.1.1 has:

1. **Movable Parts** (stewardship components):
   - `WelcomeOneAnotherPart` — Welcome visitor, collect arrival event
   - `AssignStewardPart` — Assign steward via Canon rules (mechanical)
   - `ConsentGatePart` — Gate consent decision (deterministic validation)
   - `FollowUpPart` — Async obligation reminders
   - Others: Remember, Connect, Incorporate, Serve, StewardAnother

2. **Receipt System**:
   - Each Part's `actuate()` method emits a `Receipt`
   - Receipt structure: `{ type, timestamp, steward_ids, hash, signature }`
   - Hashes computed deterministically from event data
   - Signatures via Ed25519 (SECRET_KEY)
   - Receipt chain links to prior event via `prev_hash`

3. **Domain Logic**:
   - **Canon Rules**: Scripture basis, AA mappings, avatar authorization
   - **Admit/Refuse Law**: Every event gated; system refuses non-compliant transitions
   - **CTQs** (Critical-to-Quality): Ownership checks, timing constraints, steward eligibility
   - **Terminal States**: Events close obligations with `Refused`, `Declined`, `RespectfullyClosed`, `Expired`

4. **Determinism**:
   - All routing is Canon-based (mechanical, rules-driven)
   - No randomness, no external state dependencies
   - Same obligation + same stewards = same receipt hash + signature
   - Proof: receipt chain is verifiable at any future point

### 1.2 Gemini Integration Challenge

Gemini use cases in stpnt:

1. **WelcomeOneAnotherPart (Q1-Q4 routing)**:
   - Currently: Hard-coded visitor classification (newcomer, returning, sponsor, sponsor-candidate)
   - **Gemini enhancement**: Use visitor's prior engagement (if any) to suggest classification
   - **Example**: "This person last engaged 6 months ago with sponsor interest" → ask gemini for routing recommendation

2. **AssignStewardPart (Steward suggestion)**:
   - Currently: System selects steward based on availability and Canon rules
   - **Gemini enhancement**: Suggest steward based on compatibility (personality, experience, language)
   - **Example**: "Person speaks French, has addiction history, needs accountability partner" → ask gemini for steward recommendations

3. **ConsentGatePart (Risk scoring)**:
   - Currently: Pass/fail binary decision
   - **Gemini enhancement**: Risk assessment before consent decision
   - **Example**: "This obligation has unusual timing and missing sponsor context" → ask gemini for risk signal

4. **FollowUpPart (Escalation recommendation)**:
   - Currently: Timer-based escalation (timeout after 3 days)
   - **Gemini enhancement**: Context-aware escalation (if person goes silent, assess whether to escalate or re-engage)

**Problem**: Gemini API is non-deterministic. Same input may produce different suggestions on different API calls. Receipt hashes must remain identical.

**Solution**: Gate gemini through a **Deterministic Validator** layer that:
- Caches gemini suggestions keyed by (person_id, stage, context_hash)
- Routes through a deterministic decision rule based on cached suggestion
- Records gemini's participation in the receipt (timestamp, model, token usage, suggestion hash)
- Ensures receipt hash depends only on **decision logic**, not on gemini's non-deterministic output

---

## Part 2: Decision Matrix

### 2.1 Which Parts Should Use Gemini?

| Part | Current Logic | Gemini Enhancement | Priority | Rationale |
|------|---------------|-------------------|----------|-----------|
| **WelcomeOneAnotherPart** | Hard-coded visitor classification | Q1-Q4 routing recommendation | **P1** | Highest impact: visitor classification affects all downstream obligations |
| **AssignStewardPart** | Canon rules + availability | Compatibility suggestion | **P1** | Matches person to steward based on context |
| **ConsentGatePart** | Binary pass/fail | Risk assessment input | **P2** | Informs gate decision but doesn't override it |
| **FollowUpPart** | Timer-based escalation | Escalation recommendation | **P3** | Can improve follow-up quality but not critical path |
| **RememberPart** | Static event logging | (None) | **P0** | Pure logging; no routing benefit |
| **ConnectPart** | Scheduling logic | (None) | **P0** | Scheduling is deterministic; gemini not applicable |
| **IncorporatePart** | Community integration | (None) | **P0** | Community state is external; gemini not applicable |
| **ServePart** | Steward assignment to others | Steward selection refinement | **P3** | Optional enhancement for future |
| **StewardAnotherPart** | Continuation logic | (None) | **P0** | Continuation is canon-driven |

**v30.1.1 Scope**: P1 parts (Welcome, Assign). Test infrastructure for P2/P3 deferred.

### 2.2 Gemini vs. Canon (Decision Tree)

For each P1 part, the routing decision becomes:

```
┌─ Gather Context (person history, steward roster, timing, etc.)
│
├─ Query Gemini (if available, cache miss)
│  └─ Gemini suggests: [option_a, option_b, option_c, reasoning]
│
├─ Apply Deterministic Validator
│  └─ Rule: "If gemini available, rank suggestions; else use Canon fallback"
│  └─ Result: single canonical choice
│
├─ Record Gemini Participation (timestamp, model, tokens, suggestion_hash)
│
└─ Emit Receipt (includes gemini evidence if gemini was called)
   └─ Receipt hash = H(decision_logic_only, NOT gemini_output)
```

**Key**: Receipt hash depends on **what decision rule was applied**, not on **what gemini said**.

---

## Part 3: Determinism Guarantee

### 3.1 The Determinism Contract

**Invariant**: `receipt_hash(event_E) = H(deterministic_inputs_only)`

**Deterministic inputs for a Part's receipt**:
- Event data (timestamp, person_id, steward_id, etc.)
- Canon rules applied
- Decision logic (if gemini: "gemini-assisted routing" vs. "canonical routing")
- Gemini model name + version (to distinguish runs with different models)

**Non-deterministic inputs (excluded)**:
- Gemini's actual output text / suggestions
- Gemini's internal reasoning tokens
- Gemini's token counts (vary by model update)
- Network latency or retry behavior

### 3.2 Receipt Structure for Gemini-Assisted Decisions

When gemini is consulted, the receipt gains two new fields:

```json
{
  "r#type": "assign_steward_receipt",
  "timestamp": "2026-05-27T14:30:00Z",
  "hash": "blake3(deterministic_inputs)",
  "signature": "ed25519(hash, secret_key)",
  
  // NEW: Gemini participation evidence
  "gemini_used": true,
  "gemini_model": "gemini-3.1-flash-lite-preview",
  "gemini_prompt_hash": "blake3(prompt_text)",
  "gemini_suggestion_ranking": [
    {"rank": 1, "option": "steward_alice", "confidence": 0.85, "evidence_hash": "blake3(evidence)"},
    {"rank": 2, "option": "steward_bob", "confidence": 0.72, "evidence_hash": "blake3(evidence)"},
    {"rank": 3, "option": "steward_carol", "confidence": 0.61, "evidence_hash": "blake3(evidence)"}
  ],
  "gemini_decision_rule": "select_top_suggestion",
  "gemini_call_timestamp": "2026-05-27T14:29:58Z",
  "gemini_latency_ms": 2341,
  "otel_trace_id": "a1b2c3d4e5f6g7h8"
}
```

**Why this structure**:
1. **Proof of consultation**: `gemini_used=true` + `gemini_call_timestamp` prove gemini was actually called
2. **Ranking preservation**: All suggestions recorded (not just selected one), showing what gemini considered
3. **Deterministic decision rule**: `gemini_decision_rule="select_top_suggestion"` is canonical logic, not gemini output
4. **OTEL linkage**: `otel_trace_id` links receipt to gemini.complete span for verification
5. **Prompt hash**: `gemini_prompt_hash` allows later auditing of what prompt was sent (prevents tampering)

### 3.3 Determinism Test Strategy

**Test Case 1: Identical Input, Different Gemini Responses**

```rust
#[test]
fn test_receipt_hash_stable_across_gemini_nondeterminism() {
    // Setup: Same person, same steward roster, same stage
    let event = Event {
        person_id: "alice",
        timestamp: "2026-05-27T14:30:00Z",
        // ...
    };
    let ledger = Ledger::new(None);
    
    // Run 1: Gemini suggests [steward_a, steward_b, steward_c]
    let receipt_1 = part.actuate(&event, &ledger)?;
    let hash_1 = receipt_1.hash.clone();
    
    // Run 2: Same event, but gemini suggests [steward_c, steward_a, steward_b] (different order)
    let receipt_2 = part.actuate(&event, &ledger)?;
    let hash_2 = receipt_2.hash.clone();
    
    // ASSERT: Hashes are identical despite gemini's different output
    assert_eq!(hash_1, hash_2, "Receipt hash must be deterministic despite gemini's non-determinism");
    
    // ASSERT: Gemini evidence differs (to prove gemini was actually called both times)
    assert_ne!(receipt_1.gemini_suggestion_ranking, receipt_2.gemini_suggestion_ranking);
}
```

**Rationale**: This test proves the system applied the same decision logic despite gemini's different suggestions. If hashes differed, the receipt would not be deterministic.

**Test Case 2: Gemini Unavailable, Canon Fallback**

```rust
#[test]
fn test_receipt_hash_identical_gemini_fallback() {
    // Setup: Two runs, first with gemini available, second with gemini unavailable
    let event = Event { person_id: "alice", /* ... */ };
    
    // Run 1: Gemini available, used for routing
    let config_1 = GeminiConfig::with_api_key(Some("test-key"));
    let part_1 = AssignStewardPart::new(config_1);
    let receipt_1 = part_1.actuate(&event, &ledger)?;
    
    // Run 2: Gemini unavailable (API key = None), fallback to Canon
    let config_2 = GeminiConfig::with_api_key(None);
    let part_2 = AssignStewardPart::new(config_2);
    let receipt_2 = part_2.actuate(&event, &ledger)?;
    
    // ASSERT: Both routes produce the same receipt hash
    // (because the Canon fallback should match what gemini's top suggestion would be)
    assert_eq!(receipt_1.hash, receipt_2.hash, "Canon fallback must match gemini-assisted routing");
}
```

**Implication**: This enforces that gemini is a **performance optimization** (better suggestions), not a **logic divergence**. Canon fallback must be bulletproof.

### 3.4 Preventing Drift Through Receipt Validation

**Chain Verification Rule**:

```
For each receipt R in the chain:
  1. Recompute R.hash from R's input data
  2. Verify Ed25519(R.hash) == R.signature
  3. If gemini_used == true:
     a. Verify gemini_model is in approved list
     b. Verify gemini_call_timestamp is between prior_receipt.timestamp and next_receipt.timestamp
     c. Verify gemini_decision_rule matches system policy
     d. Verify gemini_prompt_hash matches canonical prompt template
```

**Enforcement**: `ggen receipt verify` command validates the entire chain; any drift detected causes verification failure.

---

## Part 4: API Integration Pattern

### 4.1 Gemini API Transport

Two integration strategies:

**Strategy A: Gemini CLI (dteam pattern)**

```rust
// Invoke `gemini -m <model> -p <prompt> --output-format json`
use dteam::llm::gemini_cli::{call_response, GeminiError};

let config = GeminiConfig::from_env()?;  // Reads GEMINI_MODEL from env
let prompt = build_steward_suggestion_prompt(&person, &steward_roster);
let raw_response = call_response(&config, &prompt, None)?;
let suggestions: Vec<StewardSuggestion> = parse_suggestions(&raw_response)?;
```

**Pros**: Reuses dteam pattern, leverages existing CLI wrapper, easy to test with subprocess mocking (for unit tests of parsing).

**Cons**: Depends on `gemini` binary being installed locally.

---

**Strategy B: Gemini REST API (native Rust)**

```rust
// Call https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent
use reqwest::Client;
use serde_json::json;

let client = Client::new();
let response = client
    .post(format!("https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent"))
    .header("x-goog-api-key", api_key)
    .json(&json!({
        "contents": [{"parts": [{"text": prompt}]}],
        "generationConfig": { "temperature": 0.7, "topK": 40, "topP": 0.95 }
    }))
    .send()
    .await?;
let result = response.json::<GeminiResponse>().await?;
```

**Pros**: Pure Rust, no external binary dependency, fine-grained control, direct OTEL instrumentation.

**Cons**: Requires managing API keys, error handling for REST peculiarities.

---

**v30.1.1 Decision**: Use **Strategy B** (REST API) for native Rust integration and better OTEL control.

### 4.2 Credentials & Environment

**Environment Variables**:

```bash
# Required (will attempt gemini; if absent, fallback to Canon)
GEMINI_API_KEY="AIzaSy..."

# Optional
GEMINI_MODEL="gemini-3.1-flash-lite-preview"  # Default: flash-lite
GEMINI_TEMPERATURE="0.7"  # Default: 0.7
GEMINI_TIMEOUT_SECONDS="10"  # Default: 10s, abort if slower
GEMINI_CACHE_DIR="/tmp/ggen-gemini-cache"  # Default: /tmp
```

**Config Loading**:

```rust
pub struct GeminiConfig {
    api_key: Option<String>,
    model: String,
    temperature: f32,
    timeout_secs: u64,
    cache_dir: PathBuf,
}

impl GeminiConfig {
    pub fn from_env() -> Result<Self> {
        Ok(Self {
            api_key: std::env::var("GEMINI_API_KEY").ok(),
            model: std::env::var("GEMINI_MODEL")
                .unwrap_or_else(|_| "gemini-3.1-flash-lite-preview".to_string()),
            temperature: std::env::var("GEMINI_TEMPERATURE")
                .ok()
                .and_then(|s| s.parse::<f32>().ok())
                .unwrap_or(0.7),
            timeout_secs: std::env::var("GEMINI_TIMEOUT_SECONDS")
                .ok()
                .and_then(|s| s.parse::<u64>().ok())
                .unwrap_or(10),
            cache_dir: std::env::var("GEMINI_CACHE_DIR")
                .unwrap_or_else(|_| "/tmp/ggen-gemini-cache".to_string())
                .into(),
        })
    }
    
    pub fn is_available(&self) -> bool {
        self.api_key.is_some()
    }
}
```

### 4.3 Rate Limiting & Quota

**Default Limits** (per v30.1.1 pilot):

```rust
pub struct GeminiQuotaManager {
    calls_per_minute: usize,
    calls_per_hour: usize,
    tokens_per_day: u64,
}

impl GeminiQuotaManager {
    pub fn default() -> Self {
        Self {
            calls_per_minute: 5,      // Conservative for pilot
            calls_per_hour: 100,      // ~2 calls per obligation on 50-obligation/hour scale
            tokens_per_day: 100_000,  // ~200 obligations * 500 tokens each
        }
    }
    
    pub async fn acquire(&mut self) -> Result<QuotaToken> {
        // Check sliding window counters
        // If over limit, return Err(QuotaExhausted)
        // Otherwise return token; on drop, decrement counter
    }
}
```

**Fallback on Quota Exhaustion**: If quota exhausted, system silently falls back to Canon routing. No error thrown; receipt records `gemini_used=false`.

### 4.4 Caching Layer

**Cache Key**: `blake3(model + prompt_text + person_id + steward_roster_hash)`

**Cache Semantics**:
- Cache hits are **exact matches** (same model, prompt, roster)
- TTL: 7 days per cached entry
- Stored as JSON files in `GEMINI_CACHE_DIR`

**Cache File**:

```json
{
  "cache_version": "1.0",
  "key": "blake3(...)",
  "model": "gemini-3.1-flash-lite-preview",
  "prompt_hash": "blake3(...)",
  "timestamp": "2026-05-27T14:29:58Z",
  "suggestions": [
    { "rank": 1, "option": "steward_a", "confidence": 0.85, "reasoning": "..." },
    { "rank": 2, "option": "steward_b", "confidence": 0.72, "reasoning": "..." }
  ],
  "token_usage": { "prompt_tokens": 450, "completion_tokens": 120, "total_tokens": 570 },
  "ttl_expires": "2026-06-03T14:29:58Z"
}
```

**Validation**: Before using cached entry, verify TTL, model, and prompt_hash haven't drifted.

### 4.5 Error Handling & Timeouts

**Error States**:

```rust
pub enum GeminiError {
    // Transient (retry)
    Timeout,
    RateLimited,
    ServiceUnavailable,
    
    // Permanent (fallback to Canon)
    ApiKeyMissing,
    InvalidApiKey,
    UnknownModel,
    InvalidPrompt,
    ResponseParseError,
}

impl GeminiClient {
    pub async fn call_with_fallback(
        &self,
        prompt: &str,
        fallback_fn: impl Fn() -> Result<Vec<Suggestion>>,
    ) -> Result<Vec<Suggestion>> {
        match self.call(prompt).await {
            Ok(suggestions) => Ok(suggestions),
            Err(e) if e.is_transient() => {
                // Retry once after 100ms
                tokio::time::sleep(Duration::from_millis(100)).await;
                self.call(prompt).await
                    .or_else(|_| fallback_fn())  // Fall back to Canon if retry fails
            }
            Err(_) => fallback_fn(),  // Permanent error: use Canon
        }
    }
}
```

**Timeout Behavior**: If gemini call exceeds `GEMINI_TIMEOUT_SECONDS`, abort and fall back to Canon. Record `gemini_used=false` in receipt; no error thrown.

---

## Part 5: Receipt Binding

### 5.1 Receipt Emission Pipeline

When a Part calls gemini, the receipt lifecycle is:

```
1. Part.hook() — Pre-event setup (metrics, logging)
2. Part.admit_refuse() — Deterministic validation (no gemini)
3. Part.actuate() — ACTUAL routing decision
   a. Gather context (person, steward roster, prior events)
   b. Build deterministic inputs hash
   c. Call gemini (if available, not in cache)
   d. Cache gemini response (with TTL)
   e. Apply deterministic_decision_rule(gemini_suggestions)
   f. Select final steward/option
   g. Build Receipt with gemini evidence (if used)
4. Emit receipt to ledger
5. Part.handle_refusal() or on_suspended() — Post-event handling (if needed)
```

### 5.2 Hash Computation

**Deterministic Hash** (used for receipt signature):

```
receipt_hash = blake3(
    concat(
        event.person_id,
        event.timestamp,
        event.steward (if assigned),
        "WelcomeOneAnotherPart",  // Part name
        "canonical_decision_rule",  // Always same (not gemini output)
        gemini_model_version (if gemini used),
        gemini_decision_rule (if gemini used),
    )
)
```

**Why**: Hash depends on what decision logic was applied and what model was used, but NOT on gemini's actual suggestions. This ensures identical input events produce identical hashes.

### 5.3 Signature Binding

**Ed25519 Signature** (using SECRET_KEY):

```rust
pub fn sign_receipt(receipt: &Receipt, secret_key: &[u8; 32]) -> Result<String> {
    let message = serde_json::to_string(&ReceiptContent {
        r#type: receipt.r#type.clone(),
        timestamp: receipt.timestamp.clone(),
        hash: receipt.hash.clone(),
        // Include deterministic inputs, NOT gemini_suggestion_ranking
    })?;
    
    let sig = ed25519_dalek::SigningKey::from_bytes(secret_key)
        .sign(message.as_bytes());
    
    Ok(base64::encode(&sig.to_bytes()))
}
```

**Verification**:

```rust
pub fn verify_receipt(receipt: &Receipt, verifying_key: &[u8; 32]) -> Result<bool> {
    let message = serde_json::to_string(&ReceiptContent { /* ... */ })?;
    let sig_bytes = base64::decode(&receipt.signature)?;
    let sig = ed25519_dalek::Signature::from_bytes(&sig_bytes);
    
    ed25519_dalek::VerifyingKey::from_bytes(verifying_key)
        .verify(message.as_bytes(), &sig)
        .map(|_| true)
        .map_err(|_| StpntError::SignatureInvalid)
}
```

### 5.4 OTEL Trace Binding

Every gemini call emits an OTEL span:

```rust
pub async fn call_gemini_with_otel(
    client: &GeminiClient,
    prompt: &str,
    person_id: &str,
) -> Result<GeminiResponse> {
    let tracer = opentelemetry::global::tracer("ggen-stpnt");
    let mut span = tracer.start("gemini.complete");
    
    span.set_attribute(Key::new("gemini.model").string(client.config.model.clone()));
    span.set_attribute(Key::new("gemini.person_id").string(person_id.to_string()));
    span.set_attribute(Key::new("gemini.prompt_hash").string(blake3_hash(prompt)));
    
    let start = Instant::now();
    let response = client.call(prompt).await?;
    let elapsed_ms = start.elapsed().as_millis() as u64;
    
    span.set_attribute(Key::new("gemini.latency_ms").i64(elapsed_ms as i64));
    span.set_attribute(Key::new("gemini.suggestion_count").i64(response.suggestions.len() as i64));
    span.set_attribute(Key::new("gemini.token_usage.prompt_tokens").i64(response.tokens.prompt_tokens as i64));
    span.set_attribute(Key::new("gemini.token_usage.completion_tokens").i64(response.tokens.completion_tokens as i64));
    
    Ok(response)
}
```

**Receipt links to OTEL**: `receipt.otel_trace_id = span.context().trace_id()`

**Verification**: Run `ggen receipt verify` with OTEL export (e.g., Jaeger), confirm gemini.complete span exists with matching trace_id.

---

## Part 6: Testing Strategy (Chicago TDD)

### 6.1 Real API Tests

**Test Infrastructure**:

```bash
# Enable in tests with:
export GEMINI_API_KEY="AIzaSy..."  # Must be set
export STPNT_TEST_GEMINI_ENABLED="true"

cargo test --test gemini_integration_tests -- --nocapture --include-ignored
```

**Test Case: Real Gemini Call (P1)**

```rust
#[tokio::test]
#[ignore]  // Requires GEMINI_API_KEY set; run with --include-ignored
async fn test_welcome_part_with_real_gemini() {
    // Skip if API key not set
    if std::env::var("GEMINI_API_KEY").is_err() {
        return;
    }
    
    // Setup
    let event = Event {
        person_id: "test_visitor_001",
        timestamp: Utc::now().to_rfc3339(),
        event: "prepare".to_string(),
        // ...
    };
    let ledger = Ledger::new(None);
    let config = GeminiConfig::from_env().expect("GEMINI_API_KEY must be set");
    let part = WelcomeOneAnotherPart::with_config(config);
    
    // Act: Call gemini for real
    let receipt = part.actuate(&event, &ledger).await
        .expect("Welcome part should actuate with real gemini");
    
    // Assert: Receipt has valid structure
    assert!(!receipt.hash.is_empty());
    assert!(!receipt.signature.is_empty());
    assert_eq!(receipt.r#type, "welcome_receipt");
    
    // Assert: Gemini evidence is present
    assert!(receipt.gemini_used);
    assert_eq!(receipt.gemini_model, "gemini-3.1-flash-lite-preview");
    assert!(receipt.gemini_suggestion_ranking.len() > 0);
    assert!(!receipt.otel_trace_id.is_empty());
    
    // Assert: OTEL span exists
    let spans = jaeger_client()
        .find_spans_by_trace_id(&receipt.otel_trace_id)
        .await
        .expect("Should find OTEL spans");
    let gemini_span = spans.iter()
        .find(|s| s.operation_name == "gemini.complete")
        .expect("Should have gemini.complete span");
    assert_eq!(
        gemini_span.get_attribute("gemini.model"),
        Some(Attribute::String("gemini-3.1-flash-lite-preview".to_string()))
    );
}
```

### 6.2 Determinism Tests

**Test Case: Identical Receipt Hash Despite Gemini Nondeterminism**

```rust
#[test]
fn test_receipt_determinism_despite_gemini_variation() {
    // Use cached gemini responses (deterministic)
    let config = GeminiConfig::from_env()
        .map(|c| c.with_cache_only(true))  // Cache hits only, no real API calls
        .unwrap_or_else(|_| GeminiConfig::new_fallback_only());
    
    let event = Event {
        person_id: "alice".to_string(),
        timestamp: "2026-05-27T14:30:00Z".to_string(),
        // ... stable inputs ...
    };
    let ledger = Ledger::new(None);
    
    // Simulate two runs with different gemini output
    let mut cache = crate::cache::GeminiCache::new();
    
    // Run 1: Gemini suggests [steward_a, steward_b, steward_c]
    cache.insert(
        "test-key",
        CachedResponse {
            suggestions: vec![
                Suggestion { rank: 1, option: "steward_a".into(), confidence: 0.85 },
                Suggestion { rank: 2, option: "steward_b".into(), confidence: 0.72 },
                Suggestion { rank: 3, option: "steward_c".into(), confidence: 0.61 },
            ],
        },
    );
    
    let part = AssignStewardPart::with_config_and_cache(config.clone(), cache.clone());
    let receipt_1 = part.actuate(&event, &ledger)
        .expect("First run should succeed");
    let hash_1 = receipt_1.hash.clone();
    
    // Run 2: Same event, but gemini suggests [steward_c, steward_a, steward_b] (different order)
    cache.insert(
        "test-key",
        CachedResponse {
            suggestions: vec![
                Suggestion { rank: 1, option: "steward_c".into(), confidence: 0.81 },
                Suggestion { rank: 2, option: "steward_a".into(), confidence: 0.78 },
                Suggestion { rank: 3, option: "steward_b".into(), confidence: 0.65 },
            ],
        },
    );
    
    let part = AssignStewardPart::with_config_and_cache(config, cache);
    let receipt_2 = part.actuate(&event, &ledger)
        .expect("Second run should succeed");
    let hash_2 = receipt_2.hash.clone();
    
    // ASSERT: Hashes are identical (deterministic)
    assert_eq!(hash_1, hash_2,
        "Receipt hash must remain deterministic despite gemini's different suggestions");
    
    // ASSERT: Gemini evidence differs (to prove gemini was consulted with different output)
    assert_ne!(receipt_1.gemini_suggestion_ranking, receipt_2.gemini_suggestion_ranking);
    
    // ASSERT: But selected steward is the same (deterministic decision rule applied)
    assert_eq!(receipt_1.current_steward, receipt_2.current_steward);
}
```

### 6.3 Fallback Tests

**Test Case: Canon Fallback When Gemini Unavailable**

```rust
#[test]
fn test_canon_fallback_when_gemini_api_key_missing() {
    // Setup with no API key
    let config = GeminiConfig::new_with_api_key(None);
    let part = AssignStewardPart::with_config(config);
    
    let event = Event {
        person_id: "bob".to_string(),
        timestamp: "2026-05-27T15:00:00Z".to_string(),
        // ...
    };
    let ledger = Ledger::new(None);
    
    // Act
    let receipt = part.actuate(&event, &ledger)
        .expect("Should succeed with Canon fallback");
    
    // Assert: Canon routing applied (no gemini evidence)
    assert!(!receipt.gemini_used);
    assert!(receipt.otel_trace_id.is_empty() || !receipt.otel_trace_id.contains("gemini"));
    assert!(receipt.gemini_suggestion_ranking.is_empty());
    
    // Assert: Steward still assigned (via Canon)
    assert!(receipt.current_steward.is_some());
}
```

### 6.4 Refusal Tests (Negative Path)

**Test Case: System Refuses Invalid Gemini Suggestion**

```rust
#[test]
fn test_refuse_invalid_steward_suggestion_from_gemini() {
    // Setup: Gemini suggests a non-existent steward
    let invalid_steward = "steward_does_not_exist";
    
    let config = GeminiConfig::from_env().unwrap_or_default();
    let mut cache = GeminiCache::new();
    cache.insert("test-key", CachedResponse {
        suggestions: vec![
            Suggestion {
                rank: 1,
                option: invalid_steward.into(),
                confidence: 0.90,
            },
        ],
    });
    
    let part = AssignStewardPart::with_config_and_cache(config, cache);
    let event = Event {
        person_id: "carol".to_string(),
        timestamp: "2026-05-27T15:15:00Z".to_string(),
        // ...
    };
    let ledger = Ledger::new(None);
    
    // Act & Assert: System refuses gemini's suggestion and falls back to Canon
    let receipt = part.actuate(&event, &ledger)
        .expect("Should succeed by refusing gemini suggestion");
    
    // Verify fallback was used
    assert_ne!(receipt.current_steward.as_deref(), Some(invalid_steward));
    // Gemini was consulted but its suggestion was rejected
    assert!(receipt.gemini_used);
}
```

### 6.5 OTEL Validation

**Test Case: OTEL Spans Present and Correct**

```rust
#[tokio::test]
#[ignore]  // Requires GEMINI_API_KEY and Jaeger
async fn test_gemini_otel_spans_complete() {
    if std::env::var("GEMINI_API_KEY").is_err() {
        return;
    }
    
    // Setup
    let config = GeminiConfig::from_env().unwrap();
    let part = AssignStewardPart::with_config(config);
    let event = Event { /* ... */ };
    let ledger = Ledger::new(None);
    
    // Act
    let receipt = part.actuate(&event, &ledger).await.unwrap();
    
    // Query Jaeger for the trace
    let jaeger = JaegerClient::new("http://localhost:16686");
    let spans = jaeger.get_trace(&receipt.otel_trace_id).await.unwrap();
    
    // Assert: gemini.complete span exists
    let gemini_span = spans.iter()
        .find(|s| s.operation_name == "gemini.complete")
        .expect("Must have gemini.complete span");
    
    // Assert: Required attributes
    assert_eq!(
        gemini_span.get_attribute("gemini.model"),
        Some(Attribute::String("gemini-3.1-flash-lite-preview".into()))
    );
    assert!(gemini_span.get_attribute("gemini.latency_ms").is_some());
    assert!(gemini_span.get_attribute("gemini.token_usage.prompt_tokens").is_some());
    assert!(gemini_span.get_attribute("gemini.token_usage.completion_tokens").is_some());
    
    // Assert: Timing consistency
    let latency_ms = gemini_span.get_attribute("gemini.latency_ms")
        .and_then(|a| a.as_i64())
        .unwrap_or(0);
    assert!(latency_ms > 100, "Real API call should take >100ms");
    assert!(latency_ms < 30000, "API call should complete <30s");
}
```

---

## Part 7: Risk Mitigation

### 7.1 Quota Exhaustion

**Risk**: API rate limits hit during high-volume stewardship flow.

**Mitigation**:
1. Conservative default quota (5 calls/min, 100 calls/hour)
2. Sliding window counter (reject new calls if over limit)
3. Fallback to Canon if quota exhausted (silent, no error)
4. Log quota exhaustion to OTEL for monitoring
5. Configuration override: `GEMINI_CALLS_PER_HOUR=1000` (with warning)

### 7.2 API Unavailability

**Risk**: Gemini API down or unreachable during critical stewardship flow.

**Mitigation**:
1. Timeout: 10-second hard limit (abort, fallback)
2. Retry logic: 1 retry after 100ms delay (then fallback)
3. Circuit breaker: If 5 consecutive failures, disable gemini for 5 minutes
4. Emit OTEL error spans (severity=error) for ops visibility
5. Log to stderr with clear messaging ("Using Canon routing due to gemini unavailability")

### 7.3 Malicious Gemini Output

**Risk**: Gemini compromised or returns adversarial suggestions (e.g., suggest self-stewardship, non-existent stewards).

**Mitigation**:
1. **Validation Layer**: Before accepting gemini suggestion, verify:
   - Suggested steward exists in roster
   - Suggested steward != person_id (no self-stewardship)
   - Suggested steward is Canon-authorized for the stage
2. **Refusal Rule**: If any suggestion fails validation, fall back to Canon
3. **Signature Binding**: Receipt signature depends on decision rule, not gemini output; tampering is detectable
4. **Audit Trail**: All gemini evidence recorded in receipt; chain verification catches drift

### 7.4 Non-Determinism Exploit

**Risk**: Attacker crafts two obligations with same inputs, waits for gemini to produce different suggestions, uses difference to forge alternate receipt chains.

**Mitigation**:
1. **Hash Determinism**: Receipt hash depends on decision logic, not gemini output. Both obligations produce same hash.
2. **Signature**: Ed25519 signature of hash prevents forgery (attacker can't sign without secret key).
3. **Chain Verification**: `ggen receipt verify` recomputes hashes; any deviation detected.
4. **Timestamp Ordering**: Ledger enforces monotonic timestamps; can't backdate receipts.

### 7.5 Cache Poisoning

**Risk**: Attacker writes malicious gemini response to cache file.

**Mitigation**:
1. Cache files stored with restrictive permissions (0600, user-readable only)
2. Cache validation: Verify TTL, model, prompt_hash on read
3. Cache invalidation: If any validation check fails, delete cache entry
4. Atomic writes: Cache updates use temp-file-then-rename (prevent torn writes)
5. Checksum: Include blake3(suggestions) in cache file; verify on read

---

## Part 8: Integration Points

### 8.1 Folder Structure

```
/Users/sac/stpnt/
├── src/
│   ├── parts/
│   │   ├── mod.rs (update to register GeminiPart)
│   │   ├── welcome_one_another.rs (add gemini support)
│   │   ├── assign_steward.rs (add gemini support)
│   │   ├── consent_gate.rs (P2 future work)
│   │   └── ...
│   ├── gemini/ (NEW)
│   │   ├── mod.rs
│   │   ├── client.rs (GeminiClient, API calls)
│   │   ├── config.rs (GeminiConfig, env loading)
│   │   ├── cache.rs (Caching layer)
│   │   ├── quota.rs (Rate limiting)
│   │   ├── types.rs (GeminiResponse, Suggestion, etc.)
│   │   ├── router.rs (Deterministic decision rules)
│   │   └── otel.rs (OTEL instrumentation)
│   ├── receipt_v2.rs (Extended Receipt with gemini_* fields)
│   ├── domain.rs (Add GeminiDecisionRule enum)
│   └── verify.rs (Update receipt verification to validate gemini evidence)
├── tests/
│   ├── gemini_determinism_tests.rs (NEW)
│   ├── gemini_integration_tests.rs (NEW, requires API key)
│   ├── gemini_fallback_tests.rs (NEW)
│   └── gemini_otel_tests.rs (NEW)
├── Cargo.toml (Add reqwest, opentelemetry, blake3)
└── .env.example (Sample GEMINI_API_KEY, GEMINI_MODEL, etc.)
```

### 8.2 Cargo Dependencies

```toml
[dependencies]
reqwest = { version = "0.11", features = ["json", "timeout"] }
tokio = { version = "1", features = ["full"] }
serde_json = "1.0"
blake3 = "1.5"
opentelemetry = { version = "0.20", features = ["trace"] }
opentelemetry-jaeger = { version = "0.19" }

[dev-dependencies]
tempfile = "3.8"
tokio-test = "0.4"
```

### 8.3 CLI Integration

**New Commands**:

```bash
# View current gemini configuration
ggen stpnt config show --section gemini

# Test gemini connectivity
ggen stpnt gemini test-connection
  # Output: "✓ Gemini API reachable, quota: 95/100 calls/hour"

# View gemini cache statistics
ggen stpnt gemini cache stats
  # Output: "Cache entries: 42, TTL expires: 2026-06-03, Size: 1.2MB"

# Clear gemini cache
ggen stpnt gemini cache clear

# Verify receipt chain with gemini evidence
ggen stpnt receipt verify --chain-file .stpnt/ledger.json
  # Includes verification of gemini_used, gemini_model, gemini_decision_rule
```

---

## Part 9: Post-v30.1.1 Roadmap

### Phase 2 (v30.2): ConsentGatePart + FollowUpPart

- **ConsentGatePart**: Risk assessment before consent decision
- **FollowUpPart**: Escalation recommendation (should we re-engage or escalate?)
- Extend test suite to P2/P3 parts
- Add ConsentRiskScore type to Receipt

### Phase 3 (v30.3): Advanced Features

- **Multi-Turn Conversation**: Gemini memory of prior interactions
- **Feedback Loop**: Learn from outcomes (which steward suggestions worked best?)
- **Personalization**: Adapt gemini prompts based on person's history
- **A/B Testing**: Compare gemini routing vs. Canon routing on metrics (retention, satisfaction)

### Phase 4 (v30.4+): Integration with Open-Ontologies

- Publish gemini routing logic as open-ontology pack
- Community contributions of specialized prompts (e.g., "sponsor matching for Spanish speakers")
- Governance: community votes on which gemini models to approve

---

## Part 10: Definition of Done (v30.1.1)

### Checklist

- [ ] `GeminiClient` implemented with reqwest + error handling
- [ ] `GeminiConfig` loads from environment, validates keys
- [ ] `GeminiCache` stores/retrieves responses with TTL + checksum
- [ ] `GeminiQuotaManager` enforces rate limits with sliding window
- [ ] `WelcomeOneAnotherPart` supports gemini-assisted Q1-Q4 routing
- [ ] `AssignStewardPart` supports gemini-assisted steward selection
- [ ] Receipt v2 schema includes `gemini_used`, `gemini_model`, `gemini_suggestion_ranking`, `gemini_decision_rule`, `otel_trace_id`
- [ ] Receipt hash computation is deterministic (identical input → identical hash, regardless of gemini output)
- [ ] Ed25519 signature binding works; `ggen receipt verify` validates signatures
- [ ] **Chicago TDD tests**: All tests use real gemini API calls (where API key available)
  - [ ] `test_welcome_part_with_real_gemini()`
  - [ ] `test_assign_steward_with_real_gemini()`
  - [ ] `test_receipt_determinism_despite_gemini_variation()`
  - [ ] `test_canon_fallback_when_gemini_unavailable()`
  - [ ] `test_refuse_invalid_steward_suggestion_from_gemini()`
  - [ ] `test_gemini_otel_spans_complete()`
- [ ] OTEL instrumentation emits `gemini.complete` spans with required attributes
- [ ] All tests pass with `cargo test --workspace`
- [ ] Lint clean: `cargo clippy --all --all-targets --all-features -- -D warnings`
- [ ] Documentation complete: README section "Gemini Integration" + code comments
- [ ] `.env.example` includes all gemini configuration variables
- [ ] `GEMINI_ARCHITECTURE_PLAN.md` comprehensive and current

### Build Gates

```bash
cargo make check        # ✓ Compilation
cargo make test         # ✓ All tests pass (no gemini required)
cargo make test-gemini  # ✓ Gemini tests pass (requires GEMINI_API_KEY)
cargo make lint         # ✓ Clippy clean
ggen receipt verify --chain-file tests/fixtures/ledger.json  # ✓ Receipt chain valid
```

---

## Appendices

### A. Example: WelcomeOneAnotherPart with Gemini

```rust
pub struct WelcomeOneAnotherPart {
    gemini: Arc<GeminiClient>,
}

impl WelcomeOneAnotherPart {
    pub fn new(config: GeminiConfig) -> Self {
        Self {
            gemini: Arc::new(GeminiClient::new(config)),
        }
    }
}

impl StewardshipPart for WelcomeOneAnotherPart {
    async fn actuate(&self, event: &Event, ledger: &Ledger) -> Result<Receipt> {
        // Step 1: Gather context
        let person_history = ledger.get_person_history(&event.person_id)?;
        let current_roster = ledger.get_steward_roster()?;
        
        // Step 2: Build deterministic inputs
        let inputs_hash = blake3::hash(
            format!("welcome|{}|{}", event.person_id, event.timestamp).as_bytes()
        );
        
        // Step 3: Query gemini for Q1-Q4 suggestion
        let prompt = format!(
            "Visitor {} last engaged on {}. Prior engagement level: {}. \
             Suggest visitor classification (Q1=newcomer, Q2=returning, Q3=sponsor, Q4=sponsor-candidate): {}",
            event.person_id,
            person_history.last_engagement_date.unwrap_or_else(|| "never".to_string()),
            person_history.engagement_level.unwrap_or_else(|| "unknown".to_string()),
            current_roster.steward_count()
        );
        
        let (gemini_used, suggestions) = match self.gemini.call(&prompt).await {
            Ok(resp) => (true, resp.suggestions),
            Err(_) => {
                // Fallback to Canon classification
                (false, vec![Suggestion {
                    rank: 1,
                    option: "Q1".into(),  // Conservative default
                    confidence: 0.5,
                }])
            }
        };
        
        // Step 4: Apply deterministic decision rule
        let selected_classification = if gemini_used {
            // Accept top suggestion (if valid)
            validate_classification(&suggestions[0].option)?
        } else {
            // Canon: use Q1 as default
            "Q1".to_string()
        };
        
        // Step 5: Build receipt
        let receipt = Receipt {
            r#type: "welcome_receipt".to_string(),
            timestamp: event.timestamp.clone(),
            hash: inputs_hash.to_hex().to_string(),
            signature: sign_receipt(&inputs_hash.to_hex(), SECRET_KEY)?,
            gemini_used,
            gemini_model: if gemini_used {
                Some("gemini-3.1-flash-lite-preview".to_string())
            } else {
                None
            },
            gemini_suggestion_ranking: if gemini_used { suggestions } else { vec![] },
            gemini_decision_rule: "select_top_suggestion".to_string(),
            // ... other fields ...
        };
        
        Ok(receipt)
    }
}
```

### B. Ledger Schema (Updated)

```json
{
  "events": [
    {
      "sequence": 1,
      "timestamp": "2026-05-27T14:30:00Z",
      "person_id": "alice",
      "event": "prepare",
      "steward": null,
      "hash": "blake3(...)",
      "signature": "ed25519(...)"
    },
    {
      "sequence": 2,
      "timestamp": "2026-05-27T14:30:15Z",
      "person_id": "alice",
      "event": "welcome",
      "steward": null,
      "hash": "blake3(...)",
      "signature": "ed25519(...)",
      "receipt": {
        "type": "welcome_receipt",
        "timestamp": "2026-05-27T14:30:15Z",
        "hash": "blake3(...)",
        "signature": "ed25519(...)",
        "gemini_used": true,
        "gemini_model": "gemini-3.1-flash-lite-preview",
        "gemini_prompt_hash": "blake3(...)",
        "gemini_suggestion_ranking": [
          {
            "rank": 1,
            "option": "Q2",
            "confidence": 0.87,
            "evidence_hash": "blake3(...)"
          },
          {
            "rank": 2,
            "option": "Q1",
            "confidence": 0.72,
            "evidence_hash": "blake3(...)"
          }
        ],
        "gemini_decision_rule": "select_top_suggestion",
        "gemini_call_timestamp": "2026-05-27T14:30:10Z",
        "gemini_latency_ms": 2341,
        "otel_trace_id": "a1b2c3d4e5f6g7h8"
      }
    }
  ]
}
```

---

**Document Version**: 1.0  
**Date**: 2026-05-27  
**Status**: PLANNING (not yet implemented)  
**Next Step**: Present to architecture review; confirm Gemini strategy and P1 parts before proceeding to implementation.

