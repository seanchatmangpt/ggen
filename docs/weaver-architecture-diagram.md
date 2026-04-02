# Weaver Registry Integration - Architecture Diagrams

## Current State (ggen v6.0.1)

```mermaid
flowchart TB
    subgraph GGEN["ggen Services"]
        GGEN_AI["ggen-ai"]
        GGEN_CLI["ggen-cli"]
        GGEN_A2A["ggen-a2a"]
        GGEN_MCP["ggen-mcp"]

        GGEN_AI --> OTEL["OTLP Spans (gRPC/HTTP)"]
        GGEN_CLI --> OTEL
        GGEN_A2A --> OTEL
        GGEN_MCP --> OTEL
    end

    OTEL --> COLLECTOR["OTEL Collector (4317/4318)"]

    subgraph COLLECTOR_SUB["OTEL Collector"]
        RECEIVERS["Receivers: otlp (gRPC:4317, HTTP:4318)"]
        PROCESSORS["Processors: batch, memory_limiter"]
        EXPORTERS["Exporters: otlp/jaeger, logging, debug"]

        RECEIVERS --> PROCESSORS
        PROCESSORS --> EXPORTERS
    end

    COLLECTOR --> JAEGER["Jaeger (Trace Storage)<br/>UI: 16686"]
    COLLECTOR --> PROM["Prometheus (Metrics)<br/>UI: 9090"]

    style OTEL fill:#fff4e6
    style COLLECTOR fill:#e1f5ff
    style JAEGER fill:#c8e6c9
    style PROM fill:#fce4ec
```

**Problem:** No validation of span attributes against schema.

---

## Target State (With Weaver Live-Check)

```mermaid
flowchart TB
    subgraph GGEN["ggen Services"]
        GGEN_AI["ggen-ai"]
        GGEN_CLI["ggen-cli"]
        GGEN_A2A["ggen-a2a"]
        GGEN_MCP["ggen-mcp"]

        GGEN_AI --> OTEL["OTLP Spans (gRPC/HTTP)"]
        GGEN_CLI --> OTEL
        GGEN_A2A --> OTEL
        GGEN_MCP --> OTEL
    end

    OTEL --> COLLECTOR["OTEL Collector (4317/4318)"]

    subgraph COLLECTOR_SUB["OTEL Collector"]
        RECEIVERS["Receivers: otlp (gRPC:4317, HTTP:4318)"]
        PROCESSORS["Processors: batch, memory_limiter"]
        EXPORTERS["Exporters: otlp/jaeger, otlp/weaver, debug"]

        RECEIVERS --> PROCESSORS
        PROCESSORS --> EXPORTERS
    end

    COLLECTOR --> JAEGER["Jaeger (Trace Storage)<br/>UI: 16686"]
    COLLECTOR --> PROM["Prometheus (Metrics)<br/>UI: 9090"]
    COLLECTOR --> WEAVER["Weaver Live-Check (Validation)<br/>4316, 4320"]

    subgraph WEAVER_SUB["Weaver Live-Check Container"]
        VOLUMES["Volumes:<br/>- semconv/model:/semconv/model:ro<br/>- semconv/policies:/semconv/policies:ro<br/>- weaver_reports:/reports"]
        COMMAND["Command: weaver registry live-check<br/>-r /semconv/model<br/>--skip-policies<br/>--input-source otlp<br/>--otlp-grpc-port 4316<br/>--admin-port 4320<br/>--report-path /reports"]
        OUTPUT["Output:<br/>- stdout: JSON violation logs<br/>- /reports/: JSON report files"]

        VOLUMES --> COMMAND
        COMMAND --> OUTPUT
    end

    style OTEL fill:#fff4e6
    style COLLECTOR fill:#e1f5ff
    style JAEGER fill:#c8e6c9
    style PROM fill:#fce4ec
    style WEAVER fill:#ffcdd2
    style WEAVER_SUB fill:#ffebee
```

---

## Span Flow: Valid vs Invalid

### Valid Span (No Violations)

```mermaid
sequenceDiagram
    participant Service as ggen service
    participant OTEL as OTEL Collector
    participant Weaver as Weaver Live-Check

    Service->>Service: Emit: ggen.llm.generation
    Note over Service: llm.model = "claude-3-5-sonnet" ✓<br/>llm.prompt_tokens = 100 ✓<br/>llm.completion_tokens = 50 ✓<br/>llm.total_tokens = 150 ✓

    Service->>OTEL: OTLP Span
    OTEL->>OTEL: Fan-out to Jaeger + Weaver

    OTEL->>Weaver: Validate span
    Weaver->>Weaver: Validate against semconv/model/llm/spans.yaml
    Note over Weaver: All attributes declared ✓<br/>All types match ✓<br/>No violations ✓

    Weaver-->>OTEL: INFO: Span validated successfully
```

### Invalid Span (Violations)

```mermaid
sequenceDiagram
    participant Service as ggen service
    participant OTEL as OTEL Collector
    participant Weaver as Weaver Live-Check

    Service->>Service: Emit: ggen.llm.generation
    Note over Service: llm.model = "unknown-model" ✗<br/>totally.bogus.attribute = "invalid" ✗

    Service->>OTEL: OTLP Span
    OTEL->>OTEL: Fan-out to Jaeger + Weaver

    OTEL->>Weaver: Validate span
    Weaver->>Weaver: Validate against semconv/model/llm/spans.yaml
    Note over Weaver: totally.bogus.attribute not in registry ✗<br/>llm.model value not declared ✗

    Weaver-->>OTEL: JSON violation log
    Note over OTEL,Weaver: {<br/>  "level": "WARN",<br/>  "span_name": "ggen.llm.generation",<br/>  "violations": [<br/>    {<br/>      "attribute": "totally.bogus.attribute",<br/>      "rule": "attribute_not_in_registry"<br/>    },<br/>    {<br/>      "attribute": "llm.model",<br/>      "rule": "enum_value_not_declared"<br/>    }<br/>  ]<br/>}
```

---

## CI/CD Integration

```mermaid
flowchart TD
    GITHUB["GitHub Actions (CI)"] --> CHECKOUT["1. Checkout code"]
    CHECKOUT --> INSTALL["2. Install weaver"]
    INSTALL --> SYNTAX["3. Check registry syntax<br/>weaver registry check<br/>-r ./semconv/model<br/>-p ./semconv/policies/"]
    SYNTAX --> RESOLVE["4. Resolve registry<br/>weaver registry resolve<br/>-r ./semconv/model<br/>-o ./semconv/dist/resolved.yaml"]
    RESOLVE --> UPLOAD["5. Upload resolved registry (artifact)"]

    SYNTAX -->|PASS| RESULT["✅ PASS"]
    SYNTAX -->|FAIL| RESULT["❌ FAIL"]
    RESOLVE -->|PASS| RESULT
    RESOLVE -->|FAIL| RESULT

    style RESULT fill:#c8e6c9
    style SYNTAX fill:#e1f5ff
    style RESOLVE fill:#e1f5ff
```

---

## Development Workflow

```mermaid
flowchart TD
    START["Local Development"] --> STACK["1. Start weaver stack<br/>docker compose -f docker-compose.otel-test.yml<br/>-f docker-compose.weaver.yml up -d"]
    STACK --> RUN["2. Run ggen with OTEL enabled<br/>export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318<br/>cargo run --bin ggen -- sync"]
    RUN --> CHECK["3. Check for violations<br/>docker logs ggen-weaver-live-check --tail 50"]
    CHECK -->|Violations found| FIX["4. Fix instrumentation<br/>- Add missing attributes<br/>- Fix attribute types<br/>- Update semconv/model/"]
    CHECK -->|No violations| VERIFY["5. Verify fix<br/>bash weaver-verification.sh"]
    FIX --> VERIFY
    VERIFY --> COMMIT["6. Commit and push<br/>(CI validates again)"]

    style START fill:#e1f5ff
    style STACK fill:#fff4e6
    style CHECK fill:#ffcdd2
    style VERIFY fill:#c8e6c9
```

---

## Drift Detection Workflow

```mermaid
flowchart TD
    START["Drift Detection (Optional)"] --> CAPTURE["1. Capture live telemetry<br/>curl http://localhost:4318/v1/traces > /tmp/live.json"]
    CAPTURE --> INFER["2. Infer schema from live telemetry<br/>weaver registry infer<br/>--input-source otlp<br/>--input-file /tmp/live.json<br/>-o /tmp/inferred.yaml"]
    INFER --> COMPARE["3. Compare with semconv/model/<br/>diff -u<br/><(weaver resolve -r ./semconv/model)<br/><(weaver resolve -r /tmp/inferred.yaml)"]
    COMPARE --> UPDATE["4. Update semconv/model/ or fix instrumentation"]
    UPDATE --> DECISION{"What changed?"}
    DECISION -->|Code changed| UPDATE_SEMCONV["Update semconv"]
    DECISION -->|Semconv changed| FIX_CODE["Fix code"]

    style START fill:#e1f5ff
    style CAPTURE fill:#fff4e6
    style INFER fill:#e1f5ff
    style COMPARE fill:#ffcdd2
    style UPDATE fill:#c8e6c9
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
