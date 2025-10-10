# ggen-ai Architecture Diagram

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        ggen-ai Crate                         │
│                                                               │
│  ┌────────────────────────────────────────────────────────┐ │
│  │                   Public API (lib.rs)                   │ │
│  │  - LlmClient trait                                      │ │
│  │  - Generators (Template, SPARQL, Ontology, Refactor)   │ │
│  │  - Error types                                          │ │
│  └────────────────────────────────────────────────────────┘ │
│                              │                                │
│  ┌───────────────────────────┼───────────────────────────┐  │
│  │          LlmAdapter        │                           │  │
│  │  ┌──────────────────────────────────────────────────┐ │  │
│  │  │  Client Registry & Default Selection              │ │  │
│  │  └──────────────────────────────────────────────────┘ │  │
│  └───────────────────────────┼───────────────────────────┘  │
│                              │                                │
│  ┌──────────────┬────────────┼────────────┬──────────────┐  │
│  ▼              ▼            ▼            ▼              ▼  │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────────┐  │
│  │ OpenAI   │ │Anthropic │ │ Ollama   │ │ MockClient   │  │
│  │ Provider │ │ Provider │ │ Provider │ │ (Testing)    │  │
│  └──────────┘ └──────────┘ └──────────┘ └──────────────┘  │
│       │              │            │              │          │
│  ┌────┴──────────────┴────────────┴──────────────┴──────┐  │
│  │           LlmClient Trait Implementation             │  │
│  │  - async fn complete()                               │  │
│  │  - async fn stream_complete()                        │  │
│  │  - async fn embed()                                  │  │
│  └──────────────────────────────────────────────────────┘  │
│                              │                                │
│  ┌───────────────────────────┼───────────────────────────┐  │
│  │                    Generators                          │  │
│  │  ┌─────────────────────────────────────────────────┐  │  │
│  │  │ TemplateGenerator                               │  │  │
│  │  │  - generate_template()                          │  │  │
│  │  │  - generate_rest_controller()                   │  │  │
│  │  │  - generate_data_model()                        │  │  │
│  │  └─────────────────────────────────────────────────┘  │  │
│  │  ┌─────────────────────────────────────────────────┐  │  │
│  │  │ SparqlGenerator                                 │  │  │
│  │  │  - generate_query()                             │  │  │
│  │  │  - generate_from_intent()                       │  │  │
│  │  └─────────────────────────────────────────────────┘  │  │
│  │  ┌─────────────────────────────────────────────────┐  │  │
│  │  │ OntologyGenerator                               │  │  │
│  │  │  - generate_ontology()                          │  │  │
│  │  │  - generate_from_domain()                       │  │  │
│  │  └─────────────────────────────────────────────────┘  │  │
│  │  ┌─────────────────────────────────────────────────┐  │  │
│  │  │ RefactorAssistant                               │  │  │
│  │  │  - suggest_refactoring()                        │  │  │
│  │  │  - analyze_code()                               │  │  │
│  │  └─────────────────────────────────────────────────┘  │  │
│  └────────────────────────────────────────────────────────┘  │
│                              │                                │
│  ┌───────────────────────────┼───────────────────────────┐  │
│  │                  Prompt Management                     │  │
│  │  ┌─────────────────────────────────────────────────┐  │  │
│  │  │ TemplatePromptBuilder                           │  │  │
│  │  │ SparqlPrompts                                   │  │  │
│  │  │ CodePrompts                                     │  │  │
│  │  └─────────────────────────────────────────────────┘  │  │
│  └────────────────────────────────────────────────────────┘  │
│                              │                                │
│  ┌───────────────────────────┼───────────────────────────┐  │
│  │                    MCP Server                          │  │
│  │  ┌─────────────────────────────────────────────────┐  │  │
│  │  │ MCP Tools Exposure                              │  │  │
│  │  │  - generate_template                            │  │  │
│  │  │  - generate_sparql                              │  │  │
│  │  │  - generate_ontology                            │  │  │
│  │  │  - refactor_code                                │  │  │
│  │  └─────────────────────────────────────────────────┘  │  │
│  └────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## Data Flow

### 1. Template Generation Flow

```
User Request
    │
    ▼
TemplateGenerator
    │
    ▼
TemplatePromptBuilder ────> Prompt Construction
    │
    ▼
LlmAdapter ────> Provider Selection
    │
    ▼
Provider (OpenAI/Anthropic/Ollama)
    │
    ▼
HTTP Request ────> API Call
    │
    ▼
LlmResponse ────> Response Processing
    │
    ▼
Template Parsing ────> Validation
    │
    ▼
Template (ggen-core)
    │
    ▼
User Result
```

### 2. Error Handling Flow

```
Operation
    │
    ▼
Error Occurs
    │
    ▼
GgenAiError Creation
    │
    ├──> LlmProvider Error
    ├──> HTTP Error (with retry info)
    ├──> Configuration Error (with hints)
    ├──> Validation Error (with context)
    └──> Rate Limit (with retry-after)
    │
    ▼
Error Context Enrichment
    │
    ▼
Retry Logic (if retryable)
    │
    ├──> Success ────> Return Result
    └──> Max Retries ────> Return Error
```

## Recommended Architecture (After Refactoring)

```
┌─────────────────────────────────────────────────────────────┐
│                        ggen-ai Crate                         │
│                                                               │
│  ┌────────────────────────────────────────────────────────┐ │
│  │               Configuration Module (NEW)                │ │
│  │  ┌──────────────────────────────────────────────────┐  │ │
│  │  │ GgenAiConfig                                     │  │ │
│  │  │  - Provider configs (OpenAI, Anthropic, Ollama) │  │ │
│  │  │  - Logging config                                │  │ │
│  │  │  - Feature flags                                 │  │ │
│  │  └──────────────────────────────────────────────────┘  │ │
│  │  ┌──────────────────────────────────────────────────┐  │ │
│  │  │ Configuration Sources                            │  │ │
│  │  │  - TOML file (config.toml)                       │  │ │
│  │  │  - Environment variables                         │  │ │
│  │  │  - Programmatic (ConfigBuilder)                  │  │ │
│  │  └──────────────────────────────────────────────────┘  │ │
│  └────────────────────────────────────────────────────────┘ │
│                              │                                │
│  ┌────────────────────────────────────────────────────────┐ │
│  │            Enhanced Error Module (IMPROVED)            │ │
│  │  ┌──────────────────────────────────────────────────┐  │ │
│  │  │ GgenAiError (Enhanced)                           │  │ │
│  │  │  - Context-rich variants                         │  │ │
│  │  │  - Retry information                             │  │ │
│  │  │  - Helpful hints                                 │  │ │
│  │  └──────────────────────────────────────────────────┘  │ │
│  │  ┌──────────────────────────────────────────────────┐  │ │
│  │  │ Retry Logic                                      │  │ │
│  │  │  - Exponential backoff                           │  │ │
│  │  │  - Rate limit handling                           │  │ │
│  │  │  - Configurable retries                          │  │ │
│  │  └──────────────────────────────────────────────────┘  │ │
│  └────────────────────────────────────────────────────────┘ │
│                              │                                │
│  ┌────────────────────────────────────────────────────────┐ │
│  │            Providers (Config-Aware)                    │ │
│  │  - OpenAIClient::from_config(config)                   │ │
│  │  - AnthropicClient::from_config(config)                │ │
│  │  - OllamaClient::from_config(config)                   │ │
│  └────────────────────────────────────────────────────────┘ │
│                              │                                │
│  ┌────────────────────────────────────────────────────────┐ │
│  │       Testing Infrastructure (EXPANDED)                │ │
│  │  tests/                                                │ │
│  │  ├── common/                                           │ │
│  │  │   ├── fixtures.rs (Test data)                      │ │
│  │  │   └── helpers.rs (Test utilities)                  │ │
│  │  ├── providers_test.rs (Integration)                  │ │
│  │  ├── generators_test.rs (Integration)                 │ │
│  │  └── property_tests.rs (Property-based)               │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

## Component Interaction Matrix

| Component          | Depends On                        | Used By                          |
|--------------------|-----------------------------------|----------------------------------|
| **lib.rs**         | All modules                       | External crates                  |
| **config.rs**      | serde, toml                       | Providers, Generators            |
| **client.rs**      | error                             | Providers, Generators            |
| **error.rs**       | thiserror, anyhow                 | All modules                      |
| **retry.rs**       | error, tokio                      | Providers                        |
| **providers/**     | client, error, retry, config      | Generators, LlmAdapter           |
| **generators/**    | client, error, prompts            | Public API, MCP server           |
| **prompts/**       | tera                              | Generators                       |
| **mcp/**           | generators, rmcp                  | External tools                   |

## Module Dependencies

```
lib.rs
 ├── config (NEW)
 │   └── Environment variables
 ├── error
 │   └── thiserror
 ├── retry (NEW)
 │   ├── error
 │   └── tokio::time
 ├── client
 │   ├── error
 │   └── async-trait
 ├── providers/
 │   ├── adapter (MockClient)
 │   ├── openai
 │   │   ├── config (NEW)
 │   │   ├── client
 │   │   ├── error
 │   │   └── retry (NEW)
 │   ├── anthropic
 │   │   ├── config (NEW)
 │   │   ├── client
 │   │   ├── error
 │   │   └── retry (NEW)
 │   └── ollama
 │       ├── config (NEW)
 │       ├── client
 │       └── error
 ├── generators/
 │   ├── template
 │   ├── sparql
 │   ├── ontology
 │   └── refactor
 ├── prompts/
 │   ├── template
 │   ├── sparql
 │   └── code
 └── mcp/
     ├── server
     └── tools
```

## Security Architecture

```
┌─────────────────────────────────────────────┐
│          Security Boundaries                │
│                                             │
│  ┌───────────────────────────────────────┐ │
│  │  Configuration Layer                  │ │
│  │  ─────────────────────────────────── │ │
│  │  • API keys from environment only    │ │
│  │  • No hardcoded secrets              │ │
│  │  • Validation on load                │ │
│  └───────────────────────────────────────┘ │
│                    │                        │
│  ┌───────────────────────────────────────┐ │
│  │  Provider Layer                       │ │
│  │  ─────────────────────────────────── │ │
│  │  • Sanitize error messages           │ │
│  │  • Don't leak API keys in logs       │ │
│  │  • HTTPS only                        │ │
│  └───────────────────────────────────────┘ │
│                    │                        │
│  ┌───────────────────────────────────────┐ │
│  │  Input Validation                     │ │
│  │  ─────────────────────────────────── │ │
│  │  • Validate all external inputs      │ │
│  │  • Sanitize prompts                  │ │
│  │  • Limit request sizes               │ │
│  └───────────────────────────────────────┘ │
│                    │                        │
│  ┌───────────────────────────────────────┐ │
│  │  Rate Limiting                        │ │
│  │  ─────────────────────────────────── │ │
│  │  • Respect provider limits           │ │
│  │  • Implement backoff                 │ │
│  │  • Track usage                       │ │
│  └───────────────────────────────────────┘ │
└─────────────────────────────────────────────┘
```

## Testing Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   Testing Layers                        │
│                                                         │
│  ┌───────────────────────────────────────────────────┐ │
│  │  Unit Tests (src/*/tests.rs)                      │ │
│  │  • MockClient for isolation                       │ │
│  │  • Fast execution                                 │ │
│  │  • High coverage                                  │ │
│  └───────────────────────────────────────────────────┘ │
│                         │                               │
│  ┌───────────────────────────────────────────────────┐ │
│  │  Integration Tests (tests/)                       │ │
│  │  • Real provider interactions                     │ │
│  │  • Configuration testing                          │ │
│  │  • Error handling validation                      │ │
│  └───────────────────────────────────────────────────┘ │
│                         │                               │
│  ┌───────────────────────────────────────────────────┐ │
│  │  Property-Based Tests                             │ │
│  │  • Invariant checking                             │ │
│  │  • Edge case discovery                            │ │
│  │  • Fuzzing inputs                                 │ │
│  └───────────────────────────────────────────────────┘ │
│                         │                               │
│  ┌───────────────────────────────────────────────────┐ │
│  │  Test Fixtures (tests/common/)                    │ │
│  │  • Sample templates                               │ │
│  │  • Mock responses                                 │ │
│  │  • Test utilities                                 │ │
│  └───────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────┘
```

## Configuration Flow

```
Application Start
       │
       ▼
┌─────────────────┐
│ Load Config     │
│                 │
│ 1. Check for    │
│    config.toml  │──── File Found ───┐
│                 │                    │
│ 2. Load env     │                    │
│    variables    │                    │
│                 │                    │
│ 3. Use defaults │                    │
└─────────────────┘                    │
       │                               │
       ▼                               ▼
┌─────────────────┐            ┌──────────────┐
│ Merge Sources   │◄───────────│ Parse TOML   │
│                 │            └──────────────┘
│ Priority:       │
│ 1. Env vars     │
│ 2. TOML file    │
│ 3. Defaults     │
└─────────────────┘
       │
       ▼
┌─────────────────┐
│ Validate Config │
│                 │
│ • API keys set? │
│ • URLs valid?   │
│ • Models exist? │
└─────────────────┘
       │
       ▼
┌─────────────────┐
│ Initialize      │
│ Providers       │
│                 │
│ • OpenAI        │
│ • Anthropic     │
│ • Ollama        │
└─────────────────┘
       │
       ▼
┌─────────────────┐
│ Ready for Use   │
└─────────────────┘
```

## Implementation Priority Map

```
┌────────────────────────────────────────────────────────┐
│                  Implementation Phases                 │
│                                                        │
│  Phase 1: Foundation (Week 1) - HIGH PRIORITY         │
│  ════════════════════════════════════════════════════ │
│  ┌──────────────────────────────────────────────────┐ │
│  │ 1. Add config.rs module                          │ │
│  │ 2. Create config.toml.example ✓                  │ │
│  │ 3. Create .env.example ✓                         │ │
│  │ 4. Update providers to use config                │ │
│  │ 5. Add validation logic                          │ │
│  └──────────────────────────────────────────────────┘ │
│                                                        │
│  Phase 2: Resilience (Week 2) - MEDIUM PRIORITY       │
│  ════════════════════════════════════════════════════ │
│  ┌──────────────────────────────────────────────────┐ │
│  │ 1. Enhance error types                           │ │
│  │ 2. Add retry.rs module                           │ │
│  │ 3. Implement exponential backoff                 │ │
│  │ 4. Add rate limit handling                       │ │
│  │ 5. Improve error messages                        │ │
│  └──────────────────────────────────────────────────┘ │
│                                                        │
│  Phase 3: Testing (Week 3) - MEDIUM PRIORITY          │
│  ════════════════════════════════════════════════════ │
│  ┌──────────────────────────────────────────────────┐ │
│  │ 1. Create tests/ directory                       │ │
│  │ 2. Add test fixtures                             │ │
│  │ 3. Write integration tests                       │ │
│  │ 4. Add property-based tests                      │ │
│  │ 5. Increase coverage to 80%+                     │ │
│  └──────────────────────────────────────────────────┘ │
│                                                        │
│  Phase 4: Documentation (Week 4) - LOW PRIORITY       │
│  ════════════════════════════════════════════════════ │
│  ┌──────────────────────────────────────────────────┐ │
│  │ 1. Update README with examples                   │ │
│  │ 2. Add API documentation                         │ │
│  │ 3. Create migration guide                        │ │
│  │ 4. Add troubleshooting guide                     │ │
│  │ 5. Document best practices                       │ │
│  └──────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────┘
```

## Key Takeaways

### Current Strengths
- ✅ Clean trait-based architecture
- ✅ Good separation of concerns
- ✅ Proper async/await patterns
- ✅ Modular generator design
- ✅ Streaming support
- ✅ MCP integration

### Areas for Improvement
- 🔧 Configuration management (hardcoded values)
- 🔧 Error context and retry logic
- 🔧 Test infrastructure expansion
- 🔧 Security hardening (API key handling)

### Implementation Benefits
- 🎯 Better security (no hardcoded secrets)
- 🎯 Improved reliability (retry logic)
- 🎯 Easier testing (fixtures & mocks)
- 🎯 Better UX (helpful error messages)
- 🎯 Maintainability (organized tests)

---

*This architecture diagram provides a visual reference for understanding the ggen-ai codebase structure and the planned improvements.*
