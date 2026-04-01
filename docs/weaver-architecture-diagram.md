# Weaver Registry Integration - Architecture Diagrams

## Current State (ggen v6.0.1)

```
┌─────────────────────────────────────────────────────────────────┐
│                         ggen Services                           │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐        │
│  │ ggen-ai  │  │ ggen-cli │  │ggen-a2a  │  │ggen-mcp  │        │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘        │
│       │             │             │             │               │
│       └─────────────┴─────────────┴─────────────┘               │
│                             │                                   │
│                    OTLP Spans (gRPC/HTTP)                       │
└─────────────────────────────┼───────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    OTEL Collector (4317/4318)                   │
│                                                                 │
│  Receivers: otlp (gRPC:4317, HTTP:4318)                        │
│  Processors: batch, memory_limiter                              │
│  Exporters: otlp/jaeger, logging, debug                         │
└─────────────────────────────┼───────────────────────────────────┘
                              │
            ┌─────────────────┴─────────────────┐
            │                                   │
            ▼                                   ▼
┌──────────────────────┐          ┌──────────────────────┐
│      Jaeger          │          │    Prometheus        │
│   (Trace Storage)    │          │   (Metrics)          │
│   UI: 16686          │          │   UI: 9090           │
└──────────────────────┘          └──────────────────────┘
```

**Problem:** No validation of span attributes against schema.

---

## Target State (With Weaver Live-Check)

```
┌─────────────────────────────────────────────────────────────────┐
│                         ggen Services                           │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐        │
│  │ ggen-ai  │  │ ggen-cli │  │ggen-a2a  │  │ggen-mcp  │        │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘        │
│       │             │             │             │               │
│       └─────────────┴─────────────┴─────────────┘               │
│                             │                                   │
│                    OTLP Spans (gRPC/HTTP)                       │
└─────────────────────────────┼───────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    OTEL Collector (4317/4318)                   │
│                                                                 │
│  Receivers: otlp (gRPC:4317, HTTP:4318)                        │
│  Processors: batch, memory_limiter                              │
│  Exporters: otlp/jaeger, otlp/weaver, debug                    │
│                                                                 │
│  ┌────────────────────────────────────────────────────────┐    │
│  │ NEW: otlp/weaver exporter (fan-out)                    │    │
│  │   endpoint: weaver-live-check:4316                     │    │
│  │   compression: none (weaver limitation)                │    │
│  └────────────────────────────────────────────────────────┘    │
└─────────────────────────────┼───────────────────────────────────┘
                              │
            ┌─────────────────┼─────────────────┐
            │                 │                 │
            ▼                 ▼                 ▼
┌──────────────────────┐┌──────────────┐┌──────────────────┐
│      Jaeger          ││   Weaver     ││    Prometheus    │
│   (Trace Storage)    ││ Live-Check   ││   (Metrics)      │
│   UI: 16686          ││ (Validation) ││   UI: 9090       │
└──────────────────────┘│ 4316, 4320   │└──────────────────┘
                       └──────┬───────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│              Weaver Live-Check Container                         │
│                                                                 │
│  Volumes:                                                       │
│    - semconv/model:/semconv/model:ro  (schema registry)        │
│    - semconv/policies:/semconv/policies:ro  (policy rules)     │
│    - weaver_reports:/reports  (validation output)              │
│                                                                 │
│  Command:                                                       │
│    weaver registry live-check                                   │
│      -r /semconv/model                                         │
│      --skip-policies                                           │
│      --input-source otlp                                       │
│      --otlp-grpc-port 4316                                     │
│      --admin-port 4320                                         │
│      --report-path /reports                                    │
│                                                                 │
│  Output:                                                        │
│    - stdout: JSON violation logs                               │
│    - /reports/: JSON report files                              │
└─────────────────────────────────────────────────────────────────┘
```

---

## Span Flow: Valid vs Invalid

### Valid Span (No Violations)

```
ggen service
  │
  │ Emit: ggen.llm.generation
  │   - llm.model = "claude-3-5-sonnet" ✓
  │   - llm.prompt_tokens = 100 ✓
  │   - llm.completion_tokens = 50 ✓
  │   - llm.total_tokens = 150 ✓
  ▼
OTEL Collector
  │
  │ Fan-out to Jaeger + Weaver
  ▼
Weaver Live-Check
  │
  │ Validate against semconv/model/llm/spans.yaml
  │   - All attributes declared ✓
  │   - All types match ✓
  │   - No violations ✓
  ▼
Output:
  INFO: Span validated successfully
```

### Invalid Span (Violations)

```
ggen service
  │
  │ Emit: ggen.llm.generation
  │   - llm.model = "unknown-model" ✗ (not in enum)
  │   - totally.bogus.attribute = "invalid" ✗ (not declared)
  ▼
OTEL Collector
  │
  │ Fan-out to Jaeger + Weaver
  ▼
Weaver Live-Check
  │
  │ Validate against semconv/model/llm/spans.yaml
  │   - totally.bogus.attribute not in registry ✗
  │   - llm.model value not declared ✗
  ▼
Output:
  {
    "level": "WARN",
    "span_name": "ggen.llm.generation",
    "violations": [
      {
        "attribute": "totally.bogus.attribute",
        "rule": "attribute_not_in_registry",
        "message": "Attribute 'totally.bogus.attribute' is not declared"
      },
      {
        "attribute": "llm.model",
        "rule": "enum_value_not_declared",
        "message": "Value 'unknown-model' is not a declared enum member"
      }
    ]
  }
```

---

## CI/CD Integration

```
┌─────────────────────────────────────────────────────────────┐
│                    GitHub Actions (CI)                      │
│                                                             │
│  Workflow: .github/workflows/weaver-check.yml              │
│                                                             │
│  Steps:                                                     │
│    1. Checkout code                                         │
│    2. Install weaver                                        │
│    3. Check registry syntax                                 │
│         weaver registry check \                             │
│           -r ./semconv/model \                              │
│           -p ./semconv/policies/                            │
│    4. Resolve registry                                      │
│         weaver registry resolve \                           │
│           -r ./semconv/model \                              │
│           -o ./semconv/dist/resolved.yaml                   │
│    5. Upload resolved registry (artifact)                   │
│                                                             │
│  Result: ✅ PASS or ❌ FAIL                                  │
└─────────────────────────────────────────────────────────────┘
```

---

## Development Workflow

```
┌─────────────────────────────────────────────────────────────┐
│                    Local Development                        │
│                                                             │
│  1. Start weaver stack                                      │
│     docker compose -f docker-compose.otel-test.yml \        │
│                    -f docker-compose.weaver.yml up -d       │
│                                                             │
│  2. Run ggen with OTEL enabled                              │
│     export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318│
│     cargo run --bin ggen -- sync                            │
│                                                             │
│  3. Check for violations                                    │
│     docker logs ggen-weaver-live-check --tail 50            │
│                                                             │
│  4. Fix instrumentation if violations found                 │
│     - Add missing attributes                                │
│     - Fix attribute types                                   │
│     - Update semconv/model/ if schema is wrong              │
│                                                             │
│  5. Verify fix                                              │
│     bash weaver-verification.sh                             │
│                                                             │
│  6. Commit and push (CI validates again)                    │
└─────────────────────────────────────────────────────────────┘
```

---

## Drift Detection Workflow

```
┌─────────────────────────────────────────────────────────────┐
│                  Drift Detection (Optional)                 │
│                                                             │
│  1. Capture live telemetry from running services            │
│     curl http://localhost:4318/v1/traces > /tmp/live.json   │
│                                                             │
│  2. Infer schema from live telemetry                        │
│     weaver registry infer \                                 │
│       --input-source otlp \                                │
│       --input-file /tmp/live.json \                        │
│       -o /tmp/inferred.yaml                                │
│                                                             │
│  3. Compare with semconv/model/                             │
│     diff -u \                                              │
│       <(weaver resolve -r ./semconv/model) \               │
│       <(weaver resolve -r /tmp/inferred.yaml)              │
│                                                             │
│  4. Update semconv/model/ or fix instrumentation            │
│     - If code changed → Update semconv                      │
│     - If semconv changed → Fix code                        │
└─────────────────────────────────────────────────────────────┘
```

---

## Key Integration Points

| Component | Port | Purpose | Health Check |
|-----------|------|---------|--------------|
| **OTEL Collector** | 4317 (gRPC), 4318 (HTTP) | Receive spans | `GET http://localhost:13133` |
| **Jaeger** | 16686 (UI) | Trace visualization | `GET http://localhost:16686` |
| **Weaver Live-Check** | 4316 (OTLP), 4320 (admin) | Validate spans | `GET http://localhost:4320/` |
| **Prometheus** | 9090 (UI) | Metrics | `GET http://localhost:9090/-/healthy` |

---

## File Structure

```
ggen/
├── docker/
│   └── weaver/
│       └── Dockerfile                 # NEW: Weaver container
│
├── semconv/
│   ├── model/
│   │   ├── manifest.yaml              # ✅ Already exists
│   │   ├── llm/                       # ✅ Already exists
│   │   ├── mcp/                       # ✅ Already exists
│   │   ├── pipeline/                  # ✅ Already exists
│   │   ├── yawl/                      # ✅ Already exists
│   │   ├── a2a/                       # ✅ Already exists
│   │   └── error/                     # ✅ Already exists
│   ├── policies/
│   │   └── ggen.rego                  # ✅ Already exists
│   └── dist/
│       └── resolved.yaml              # NEW: Generated by weaver
│
├── tests/integration/
│   ├── docker-compose.otel-test.yml   # ✅ Already exists
│   ├── docker-compose.weaver.yml      # NEW: Extend with weaver
│   ├── otel-collector-with-weaver.yaml # NEW: Collector config
│   └── weaver-verification.sh         # NEW: 12-test script
│
├── scripts/weaver/
│   ├── verify-semconv.sh              # NEW: Pre-commit hook
│   └── infer-drift.sh                 # NEW: Drift detection
│
├── .github/workflows/
│   └── weaver-check.yml               # NEW: CI workflow
│
└── docs/
    ├── weaver-integration-summary.md  # ✅ Created
    ├── weaver-registry-integration-plan.md  # ✅ Created
    └── how-to-weaver-live-check.md    # NEW: User guide
```

---

**End of Architecture Diagrams**
