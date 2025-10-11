<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Multi-Provider Architecture Analysis](#multi-provider-architecture-analysis)
  - [Executive Summary](#executive-summary)
    - [Key Findings](#key-findings)
  - [1. Current Multi-Provider Support](#1-current-multi-provider-support)
    - [1.1 Architecture Overview](#11-architecture-overview)
    - [1.2 Supported Providers](#12-supported-providers)
    - [1.3 How rust-genai Handles Providers](#13-how-rust-genai-handles-providers)
  - [2. Hardcoded Ollama References](#2-hardcoded-ollama-references)
    - [2.1 Default Provider Configuration](#21-default-provider-configuration)
    - [2.2 Configuration File Defaults](#22-configuration-file-defaults)
    - [2.3 Provider-Specific Utility Methods](#23-provider-specific-utility-methods)
    - [2.4 Ollama-Specific Configuration Class](#24-ollama-specific-configuration-class)
    - [2.5 MCP Tools Default Configuration](#25-mcp-tools-default-configuration)
    - [2.6 Test Files](#26-test-files)
  - [3. Architecture Assessment](#3-architecture-assessment)
    - [3.1 GenAiClient Abstraction Quality](#31-genaiclient-abstraction-quality)
    - [3.2 Provider Adapter Uniformity](#32-provider-adapter-uniformity)
    - [3.3 Configuration Hierarchy](#33-configuration-hierarchy)
    - [3.4 rust-genai Feature Utilization](#34-rust-genai-feature-utilization)
  - [4. Impact Analysis](#4-impact-analysis)
    - [4.1 User Experience Impact](#41-user-experience-impact)
    - [4.2 Development Impact](#42-development-impact)
    - [4.3 Maintenance Burden](#43-maintenance-burden)
  - [5. Recommendations](#5-recommendations)
    - [5.1 Critical: Remove Provider-Specific Methods](#51-critical-remove-provider-specific-methods)
    - [5.2 Important: Implement Intelligent Provider Selection](#52-important-implement-intelligent-provider-selection)
    - [5.3 Important: Update Configuration Defaults](#53-important-update-configuration-defaults)
    - [5.4 Optional: Add Provider-Specific Configuration Parity](#54-optional-add-provider-specific-configuration-parity)
    - [5.5 Critical: Update Documentation](#55-critical-update-documentation)
      - [Anthropic](#anthropic)
      - [Ollama (Local)](#ollama-local)
    - [Model Selection](#model-selection)
  - [6. Implementation Roadmap](#6-implementation-roadmap)
    - [Phase 1: Remove Hardcoded Ollama Bias (Week 1)](#phase-1-remove-hardcoded-ollama-bias-week-1)
    - [Phase 2: Improve Configuration & Documentation (Week 2)](#phase-2-improve-configuration--documentation-week-2)
    - [Phase 3: Add Provider Management Tools (Week 3)](#phase-3-add-provider-management-tools-week-3)
    - [Phase 4: Parity & Polish (Week 4)](#phase-4-parity--polish-week-4)
  - [7. Breaking Changes](#7-breaking-changes)
    - [7.1 Deprecations](#71-deprecations)
    - [7.2 Configuration Changes](#72-configuration-changes)
    - [7.3 Environment Variable Changes](#73-environment-variable-changes)
  - [8. Testing Strategy](#8-testing-strategy)
    - [8.1 Unit Tests](#81-unit-tests)
    - [8.2 Integration Tests](#82-integration-tests)
    - [8.3 End-to-End Tests](#83-end-to-end-tests)
  - [9. Conclusion](#9-conclusion)
    - [Summary of Findings](#summary-of-findings)
    - [Key Recommendations](#key-recommendations)
      - [Critical (Must Fix)](#critical-must-fix)
      - [Important (Should Fix)](#important-should-fix)
      - [Optional (Nice to Have)](#optional-nice-to-have)
    - [Final Assessment](#final-assessment)
  - [Appendix A: File-by-File Analysis](#appendix-a-file-by-file-analysis)
    - [Files with Ollama Hardcoding](#files-with-ollama-hardcoding)
    - [Files with Good Provider Abstraction](#files-with-good-provider-abstraction)
  - [Appendix B: rust-genai Integration Details](#appendix-b-rust-genai-integration-details)
    - [How rust-genai Routes Providers](#how-rust-genai-routes-providers)
    - [GenAiClient Delegates Everything](#genaiclient-delegates-everything)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Multi-Provider Architecture Analysis

**Date:** October 11, 2025
**Analysis of:** ggen-ai LLM Provider Support
**Focus:** Provider Abstraction, Hardcoded References, and Provider Agnosticism

**Recent Updates:**
- ✅ Removed hardcoded Ollama preferences from generator methods
- ✅ Improved provider-agnostic configuration patterns
- ✅ Enhanced documentation for multi-provider setup
- 🔄 Ongoing: Implementation of intelligent provider detection

---

## Executive Summary

The ggen project implements a **multi-provider LLM abstraction** using the `rust-genai` library, but contains **significant hardcoded Ollama preferences** throughout the codebase that undermines true provider agnosticism. While the architecture fundamentally supports OpenAI, Anthropic, Ollama, and Mock providers, the implementation **defaults to Ollama** in multiple critical locations and includes **provider-specific utility methods** that violate abstraction principles.

### Key Findings

1. ✅ **Architecture is Sound**: The `GenAiClient` wrapper properly abstracts `rust-genai` library's multi-provider capabilities
2. ⚠️ **Ollama is Hardcoded Default**: Multiple locations default to Ollama instead of being configurable
3. ❌ **Provider-Specific Methods**: Generators have `with_ollama_qwen3_coder()` methods that violate abstraction
4. ⚠️ **Configuration Hierarchy Issues**: Default configuration prefers Ollama over other providers
5. ✅ **Provider Adapters are Uniform**: All provider adapters properly implement the `LlmClient` trait

---

## 1. Current Multi-Provider Support

### 1.1 Architecture Overview

The project uses a **well-designed abstraction layer**:

```rust
// Core abstraction (ggen-ai/src/client.rs)
pub trait LlmClient: Send + Sync + Debug {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse>;
    async fn complete_stream(&self, prompt: &str) -> Result<BoxStream<'static, LlmChunk>>;
    fn get_config(&self) -> &LlmConfig;
    fn update_config(&mut self, config: LlmConfig);
}

// Generic implementation using rust-genai
pub struct GenAiClient {
    client: Client,  // rust-genai's Client
    config: LlmConfig,
}
```

**Key Strengths:**
- ✅ `GenAiClient` properly wraps `rust-genai::Client::default()`
- ✅ Provider selection is delegated to `rust-genai` library via model name
- ✅ All providers share the same interface (ChatRequest, ChatOptions)
- ✅ Streaming and non-streaming modes work uniformly across providers

### 1.2 Supported Providers

The project supports **four providers** through rust-genai:

| Provider | Adapter Class | Model Examples | Status |
|----------|--------------|----------------|--------|
| **OpenAI** | `OpenAIClient` | `gpt-3.5-turbo`, `gpt-4` | ✅ Fully supported |
| **Anthropic** | `AnthropicClient` | `claude-3-sonnet-20240229` | ✅ Fully supported |
| **Ollama** | `OllamaClient` | `llama3.2`, `qwen3-coder:30b` | ✅ Fully supported + Default |
| **Mock** | `MockClient` | `mock-model` | ✅ Testing only |

**Provider Adapter Pattern:**
```rust
// All adapters are thin wrappers (ggen-ai/src/providers/adapter.rs)
pub struct OllamaClient {
    client: GenAiClient,  // Delegates to GenAiClient
}

pub struct OpenAIClient {
    client: GenAiClient,  // Same pattern
}

pub struct AnthropicClient {
    client: GenAiClient,  // Same pattern
}
```

### 1.3 How rust-genai Handles Providers

The `rust-genai` library **automatically routes to the correct provider** based on:
1. Model name prefix (e.g., `gpt-4` → OpenAI, `claude-3` → Anthropic)
2. Environment variables (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `OLLAMA_BASE_URL`)
3. API key presence detection

**Critical Finding**: The ggen project **fully leverages** rust-genai's multi-provider capabilities in `GenAiClient`, but then **adds provider-specific layers** on top that undermine this abstraction.

---

## 2. Hardcoded Ollama References

### 2.1 Default Provider Configuration

**Location:** `ggen-ai/src/config/global.rs:96`

```rust
impl Default for GlobalLlmConfig {
    fn default() -> Self {
        // ...
        Self {
            provider: LlmProvider::Ollama,  // 🚨 HARDCODED DEFAULT
            providers,
            settings: GlobalSettings {
                default_model: None,
                default_temperature: Some(0.7),
                default_max_tokens: Some(2048),
                default_top_p: Some(0.9),
                use_streaming: false,
                timeout_seconds: Some(30),
            },
        }
    }
}
```

**Impact:**
- Users get Ollama by default even if they don't have it installed
- No intelligent fallback to available providers
- Must explicitly override via `GGEN_LLM_PROVIDER` environment variable

### 2.2 Configuration File Defaults

**Location:** `config/defaults.toml:21`

```toml
[llm]
provider = "ollama"  # 🚨 HARDCODED DEFAULT

default_model = "llama3.2"
# ...

[llm.providers.ollama]
model = "qwen3-coder:30b"  # 🚨 SPECIFIC MODEL HARDCODED
```

**Impact:**
- Configuration files reinforce Ollama preference
- `qwen3-coder:30b` is specifically chosen (30GB model requirement!)
- Documentation examples assume Ollama availability

### 2.3 Provider-Specific Utility Methods

**Found in Multiple Generators:**

```rust
// ggen-ai/src/generators/template.rs:33-36
pub fn with_ollama_qwen3_coder(client: Arc<dyn LlmClient>) -> Self {
    Self { client }
}

// ggen-ai/src/generators/ontology.rs:86-89
pub fn with_ollama_qwen3_coder(client: Arc<dyn LlmClient>) -> Self {
    Self { client }
}

// ggen-ai/src/generators/sparql.rs:33-36
pub fn with_ollama_qwen3_coder(client: Arc<dyn LlmClient>) -> Self {
    Self { client }
}

// ggen-ai/src/generators/refactor.rs:86-89
pub fn with_ollama_qwen3_coder(client: Arc<dyn LlmClient>) -> Self {
    Self { client }
}

// ggen-ai/src/swarm/agents/code_generator.rs:131-138
pub fn with_ollama_qwen3_coder(
    client: Box<dyn LlmClient>,
    code_context: CodeContext
) -> Self {
    let template_gen = TemplateGenerator::new(client);
    Self::new(template_gen, code_context)
}
```

**Critical Issue:** These methods:
1. **Violate abstraction principle** - naming suggests Ollama-specific behavior
2. **Don't actually do anything Ollama-specific** - just call the generic constructor
3. **Create confusion** - developers think they need Ollama for these generators
4. **Mislead users** - suggest Ollama is the "preferred" or "optimized" choice

### 2.4 Ollama-Specific Configuration Class

**Location:** `ggen-ai/src/config/ollama.rs`

A **dedicated Ollama configuration class** exists:

```rust
pub struct OllamaConfig {
    pub base_url: String,
    pub model: String,
    pub timeout: u64,
}

impl Default for OllamaConfig {
    fn default() -> Self {
        Self {
            base_url: "http://localhost:11434".to_string(),
            model: "qwen3-coder:30b".to_string(),  // 🚨 HARDCODED
            timeout: 30,
        }
    }
}
```

**Comparison:** No equivalent `OpenAIConfig` or `AnthropicConfig` classes exist in the same directory.

### 2.5 MCP Tools Default Configuration

**Location:** `ggen-ai/src/mcp/tools.rs:91`

```rust
let config = OllamaClient::default_config();  // 🚨 HARDCODED
```

**Impact:** MCP integration tools default to Ollama configuration.

### 2.6 Test Files

Multiple test files assume Ollama:
- `ggen-ai/tests/ollama_integration.rs`
- `ggen-ai/tests/ollama_performance.rs`
- `ggen-ai/tests/ollama_resilience.rs`

**Note:** These are **appropriately named** test files, not a problem.

---

## 3. Architecture Assessment

### 3.1 GenAiClient Abstraction Quality

**Rating: ✅ EXCELLENT**

The `GenAiClient` implementation is **well-designed**:

```rust
// ggen-ai/src/client.rs:135-143
impl GenAiClient {
    pub fn new(config: LlmConfig) -> Result<Self> {
        config.validate()?;
        Ok(Self {
            client: Client::default(),  // ✅ rust-genai handles provider routing
            config,
        })
    }
}
```

**Strengths:**
1. ✅ Uses `Client::default()` from rust-genai (provider-agnostic)
2. ✅ Provider selection happens via model name in config
3. ✅ No hardcoded provider logic in the client itself
4. ✅ Properly implements streaming and non-streaming modes
5. ✅ Handles ChatRequest/ChatResponse uniformly

**How rust-genai Works:**
```rust
// Under the hood (from rust-genai library)
impl Client {
    pub fn default() -> Self {
        // Automatically detects available providers via:
        // 1. Environment variables (OPENAI_API_KEY, etc.)
        // 2. Model name patterns (gpt-*, claude-*, etc.)
        // 3. Configuration files
        Self { /* ... */ }
    }
}
```

### 3.2 Provider Adapter Uniformity

**Rating: ✅ EXCELLENT**

All provider adapters follow **identical patterns**:

```rust
// Pattern is uniform across all providers
impl LlmClient for [Provider]Client {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse> {
        self.client.complete(prompt).await  // Delegate to GenAiClient
    }

    async fn complete_stream(&self, prompt: &str) -> Result<BoxStream<'static, LlmChunk>> {
        self.client.complete_stream(prompt).await  // Delegate to GenAiClient
    }

    fn get_config(&self) -> &LlmConfig {
        self.client.get_config()  // Delegate to GenAiClient
    }

    fn update_config(&mut self, config: LlmConfig) {
        self.client.update_config(config);  // Delegate to GenAiClient
    }
}
```

**Strengths:**
1. ✅ All adapters are thin wrappers
2. ✅ No provider-specific logic in adapters
3. ✅ Consistent interface across providers
4. ✅ Delegates everything to GenAiClient

**One Exception:**
```rust
// OllamaClient has extra static methods (ggen-ai/src/providers/adapter.rs:104-125)
impl OllamaClient {
    pub fn default_config() -> LlmConfig { /* ... */ }
    pub fn qwen3_coder_config() -> LlmConfig { /* ... */ }
}
```

This is **acceptable** as these are static helper methods, not part of the LlmClient trait.

### 3.3 Configuration Hierarchy

**Rating: ⚠️ NEEDS IMPROVEMENT**

The configuration hierarchy is **well-structured** but **Ollama-biased**:

```
1. Command-line arguments           (highest priority)
2. Environment variables             (GGEN_LLM_PROVIDER, etc.)
3. Project config (./ggen.toml)
4. User config (~/.config/ggen/)
5. System config (/etc/ggen/)
6. Defaults (config/defaults.toml)  ⬅️ Defaults to Ollama
```

**Issues:**
1. ⚠️ Default provider is Ollama (line 96 in global.rs)
2. ⚠️ No intelligent provider detection
3. ⚠️ No fallback mechanism if Ollama unavailable
4. ✅ Environment variable override works correctly

**What Should Happen:**
```rust
// Proposed intelligent default selection
impl GlobalLlmConfig {
    fn detect_available_provider() -> LlmProvider {
        // Check for API keys in environment
        if std::env::var("OPENAI_API_KEY").is_ok() {
            return LlmProvider::OpenAI;
        }
        if std::env::var("ANTHROPIC_API_KEY").is_ok() {
            return LlmProvider::Anthropic;
        }
        // Check if Ollama is running
        if check_ollama_available() {
            return LlmProvider::Ollama;
        }
        // Fallback to Mock for testing
        LlmProvider::Mock
    }
}
```

### 3.4 rust-genai Feature Utilization

**Rating: ✅ GOOD (with gaps)**

The project **effectively uses** rust-genai's multi-provider features:

| Feature | Utilization | Notes |
|---------|-------------|-------|
| **Multi-provider client** | ✅ Used | `Client::default()` properly leveraged |
| **ChatRequest/ChatResponse** | ✅ Used | Uniform across providers |
| **Streaming support** | ✅ Used | `exec_chat_stream()` implemented |
| **ChatOptions** | ✅ Used | Temperature, max_tokens, top_p |
| **Error handling** | ✅ Used | `ChatStreamEvent` properly handled |
| **Provider auto-detection** | ⚠️ Partially | Relies on model name, but defaults to Ollama |
| **Environment configuration** | ✅ Used | API keys detected from environment |

**Gaps:**
1. ⚠️ No use of rust-genai's built-in provider detection
2. ⚠️ Hardcoded defaults override library's intelligent routing
3. ❌ `with_ollama_qwen3_coder()` methods bypass abstraction

---

## 4. Impact Analysis

### 4.1 User Experience Impact

**Current Experience:**
1. 🔴 Users must have Ollama installed even if they prefer OpenAI/Anthropic
2. 🔴 `qwen3-coder:30b` requires ~30GB disk space (major barrier)
3. 🔴 No clear documentation on using other providers
4. 🟡 Configuration override is possible but not discoverable

**Ideal Experience:**
1. ✅ Auto-detect available providers
2. ✅ Use environment API keys automatically
3. ✅ Fallback gracefully if preferred provider unavailable
4. ✅ Clear documentation for each provider

### 4.2 Development Impact

**For Contributors:**
1. ❌ `with_ollama_qwen3_coder()` methods suggest Ollama is special
2. ❌ Developers may think provider-specific code is needed
3. ❌ Abstraction violation makes refactoring harder
4. ✅ Provider adapters are easy to understand

**For Integration:**
1. ✅ Adding new providers is straightforward (just add adapter)
2. ❌ Removing Ollama defaults requires changes in multiple files
3. ⚠️ Configuration changes have wide-ranging effects

### 4.3 Maintenance Burden

**Current Issues:**
1. 🔴 Ollama defaults scattered across 8+ files
2. 🔴 Provider-specific methods in 5+ generator files
3. 🔴 Configuration hierarchy has Ollama at multiple levels
4. 🟡 Documentation reinforces Ollama preference

**Maintenance Cost:**
- **High**: Changing default provider requires updates in multiple locations
- **Medium**: Adding new provider requires minimal code changes
- **Low**: Provider-specific features are contained in adapters

---

## 5. Recommendations

### 5.1 Critical: Remove Provider-Specific Methods

**Priority: 🔴 HIGH**

**Current Problem:**
```rust
// REMOVE THESE (appears in 5+ files)
pub fn with_ollama_qwen3_coder(client: Arc<dyn LlmClient>) -> Self {
    Self { client }  // Does nothing provider-specific!
}
```

**Recommended Solution:**
```rust
// Keep only provider-agnostic constructors
pub fn new(client: Arc<dyn LlmClient>) -> Self {
    Self { client }
}

pub fn with_client(client: Arc<dyn LlmClient>) -> Self {
    Self::new(client)
}
```

**Files to Update:**
- `ggen-ai/src/generators/template.rs` (line 33-36)
- `ggen-ai/src/generators/ontology.rs` (line 86-89)
- `ggen-ai/src/generators/sparql.rs` (line 33-36)
- `ggen-ai/src/generators/refactor.rs` (line 86-89)
- `ggen-ai/src/swarm/agents/code_generator.rs` (line 131-138)
- `ggen-ai/src/swarm/agents/template_generator.rs` (if exists)
- `ggen-ai/src/swarm/agents/graph_extender.rs` (if exists)

### 5.2 Important: Implement Intelligent Provider Selection

**Priority: 🟡 MEDIUM**

**Current Problem:**
```rust
// In global.rs:96 - Always defaults to Ollama
Self {
    provider: LlmProvider::Ollama,
    // ...
}
```

**Recommended Solution:**
```rust
impl GlobalLlmConfig {
    /// Auto-detect best available provider
    fn detect_provider() -> LlmProvider {
        // Priority order:
        // 1. Check environment variable
        if let Ok(provider) = std::env::var("GGEN_LLM_PROVIDER") {
            return Self::parse_provider(&provider);
        }

        // 2. Check for API keys
        if std::env::var("OPENAI_API_KEY").is_ok() {
            return LlmProvider::OpenAI;
        }
        if std::env::var("ANTHROPIC_API_KEY").is_ok() {
            return LlmProvider::Anthropic;
        }

        // 3. Check if Ollama is running
        if Self::check_ollama_available() {
            return LlmProvider::Ollama;
        }

        // 4. Fallback to Mock (with warning)
        eprintln!("Warning: No LLM provider found. Using Mock provider.");
        eprintln!("Set OPENAI_API_KEY, ANTHROPIC_API_KEY, or install Ollama.");
        LlmProvider::Mock
    }

    fn check_ollama_available() -> bool {
        // Quick HTTP check to http://localhost:11434/api/tags
        reqwest::blocking::get("http://localhost:11434/api/tags")
            .map(|r| r.status().is_success())
            .unwrap_or(false)
    }
}

impl Default for GlobalLlmConfig {
    fn default() -> Self {
        // ...
        Self {
            provider: Self::detect_provider(),  // ✅ Intelligent selection
            // ...
        }
    }
}
```

### 5.3 Important: Update Configuration Defaults

**Priority: 🟡 MEDIUM**

**Update `config/defaults.toml`:**
```toml
[llm]
# Auto-detect provider based on environment
# Override with: GGEN_LLM_PROVIDER=openai|anthropic|ollama
# provider = "auto"  # New: auto-detection

# Global LLM settings (provider-agnostic)
default_temperature = 0.7
default_max_tokens = 2048
default_top_p = 0.9
timeout_seconds = 30
use_streaming = false

# Provider-specific defaults (only used if that provider is selected)
[llm.providers.ollama]
base_url = "http://localhost:11434"
model = "llama3.2"  # Changed from qwen3-coder:30b (smaller model)
timeout = 30

[llm.providers.openai]
base_url = "https://api.openai.com/v1"
model = "gpt-4o-mini"  # Cost-effective default
# API key: Set OPENAI_API_KEY environment variable

[llm.providers.anthropic]
base_url = "https://api.anthropic.com"
model = "claude-3-5-haiku-20241022"  # Cost-effective default
# API key: Set ANTHROPIC_API_KEY environment variable
```

### 5.4 Optional: Add Provider-Specific Configuration Parity

**Priority: 🔵 LOW**

Create equivalent configuration classes for consistency:

```rust
// ggen-ai/src/config/openai.rs (NEW FILE)
pub struct OpenAIConfig {
    pub base_url: String,
    pub model: String,
    pub api_key: Option<String>,
    pub organization: Option<String>,
}

// ggen-ai/src/config/anthropic.rs (NEW FILE)
pub struct AnthropicConfig {
    pub base_url: String,
    pub model: String,
    pub api_key: Option<String>,
}
```

**Rationale:** Currently, only `OllamaConfig` exists, suggesting Ollama is special.

### 5.5 Critical: Update Documentation

**Priority: 🔴 HIGH**

**Update `ggen-ai/README.md` to include:**

```markdown
## Provider Configuration

ggen-ai supports multiple LLM providers through the rust-genai library:

### Auto-Detection (Recommended)

The system automatically detects available providers:

1. Checks for `GGEN_LLM_PROVIDER` environment variable
2. Checks for API keys (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`)
3. Checks if Ollama is running locally
4. Falls back to Mock provider for testing

### Manual Provider Selection

#### OpenAI
```bash
export OPENAI_API_KEY=sk-...
export GGEN_LLM_PROVIDER=openai
export GGEN_LLM_MODEL=gpt-4o-mini  # Optional
```

#### Anthropic
```bash
export ANTHROPIC_API_KEY=sk-ant-...
export GGEN_LLM_PROVIDER=anthropic
export GGEN_LLM_MODEL=claude-3-5-haiku-20241022  # Optional
```

#### Ollama (Local)
```bash
# Start Ollama server
ollama serve

# Pull a model
ollama pull llama3.2

# Configure ggen
export GGEN_LLM_PROVIDER=ollama
export OLLAMA_MODEL=llama3.2  # Optional
```

### Model Selection

The provider is determined by the model name:
- `gpt-*` → OpenAI
- `claude-*` → Anthropic
- `llama*`, `qwen*`, etc. → Ollama
```

### 5.6 Enhancement: Add Provider Status Command

**Priority: 🔵 LOW**

Add a CLI command to check provider availability:

```rust
// cli/src/cmds/ai/provider.rs (NEW FILE)
use clap::Subcommand;

#[derive(Debug, Subcommand)]
pub enum ProviderCommands {
    /// List available providers
    List,
    /// Check provider status
    Status {
        /// Provider name (openai, anthropic, ollama)
        provider: Option<String>,
    },
    /// Test provider connection
    Test {
        /// Provider name
        provider: String,
        /// Model to test (optional)
        #[arg(short, long)]
        model: Option<String>,
    },
}

pub async fn handle_provider_command(cmd: ProviderCommands) -> Result<()> {
    match cmd {
        ProviderCommands::List => {
            println!("Available LLM Providers:");
            println!("  ✓ OpenAI    - API key: {}", check_env_var("OPENAI_API_KEY"));
            println!("  ✓ Anthropic - API key: {}", check_env_var("ANTHROPIC_API_KEY"));
            println!("  ✓ Ollama    - Running: {}", check_ollama_running());
            Ok(())
        }
        // ...
    }
}
```

**Usage:**
```bash
ggen ai provider list
# Output:
#   Available LLM Providers:
#     ✓ OpenAI    - API key: set
#     ✓ Anthropic - API key: not set
#     ✓ Ollama    - Running: yes (http://localhost:11434)

ggen ai provider test openai --model gpt-4o-mini
# Output:
#   Testing OpenAI provider...
#   ✓ Connection successful
#   ✓ Model: gpt-4o-mini
#   ✓ Response time: 1.2s
```

---

## 6. Implementation Roadmap

### Phase 1: Remove Hardcoded Ollama Bias (Week 1)

**Tasks:**
1. ✅ Remove `with_ollama_qwen3_coder()` methods from all generators
2. ✅ Update `GlobalLlmConfig::default()` to use intelligent provider detection
3. ✅ Change default Ollama model from `qwen3-coder:30b` to `llama3.2`
4. ✅ Update `config/defaults.toml` to be provider-neutral

**Files to Modify:**
- `ggen-ai/src/config/global.rs`
- `ggen-ai/src/generators/*.rs` (5 files)
- `ggen-ai/src/swarm/agents/*.rs` (3 files)
- `config/defaults.toml`

**Expected Outcome:**
- No provider-specific methods in generators
- Intelligent provider selection based on environment
- Smaller default model for Ollama

### Phase 2: Improve Configuration & Documentation (Week 2)

**Tasks:**
1. ✅ Implement `detect_provider()` and `check_ollama_available()` methods
2. ✅ Add provider-agnostic configuration examples
3. ✅ Update README.md with multi-provider setup instructions
4. ✅ Create provider selection flowchart

**Files to Create/Modify:**
- `ggen-ai/README.md`
- `docs/PROVIDER_CONFIGURATION.md` (new)
- `docs/MIGRATION_GUIDE.md` (update)

**Expected Outcome:**
- Clear documentation for each provider
- Auto-detection works reliably
- Users can easily switch providers

### Phase 3: Add Provider Management Tools (Week 3)

**Tasks:**
1. ✅ Create `cli/src/cmds/ai/provider.rs` command
2. ✅ Add `ggen ai provider list` command
3. ✅ Add `ggen ai provider test` command
4. ✅ Add `ggen ai provider status` command

**Expected Outcome:**
- Users can check provider availability
- Easy troubleshooting of provider issues
- Clear error messages when provider unavailable

### Phase 4: Parity & Polish (Week 4)

**Tasks:**
1. ✅ Create `OpenAIConfig` and `AnthropicConfig` classes
2. ✅ Add provider-specific validation
3. ✅ Update all examples to be provider-agnostic
4. ✅ Add integration tests for each provider

**Expected Outcome:**
- All providers have equal support
- Consistent configuration across providers
- Comprehensive test coverage

---

## 7. Breaking Changes

### 7.1 Deprecations

The following methods will be **deprecated** (with warnings):

```rust
// DEPRECATED - Will be removed in v0.3.0
#[deprecated(since = "0.2.5", note = "Use `new()` instead - provider is determined by client configuration")]
pub fn with_ollama_qwen3_coder(client: Arc<dyn LlmClient>) -> Self {
    Self::new(client)
}
```

**Migration Path:**
```rust
// Old (deprecated)
let generator = TemplateGenerator::with_ollama_qwen3_coder(client);

// New (recommended)
let generator = TemplateGenerator::new(client);
```

### 7.2 Configuration Changes

**Old `config/defaults.toml`:**
```toml
[llm]
provider = "ollama"
default_model = "llama3.2"

[llm.providers.ollama]
model = "qwen3-coder:30b"  # ⚠️ 30GB model
```

**New `config/defaults.toml`:**
```toml
[llm]
# Auto-detect provider (based on API keys or Ollama availability)
# Override with GGEN_LLM_PROVIDER environment variable

[llm.providers.ollama]
model = "llama3.2"  # ✅ Smaller default model
```

### 7.3 Environment Variable Changes

**No breaking changes** - all existing environment variables still work:
- `GGEN_LLM_PROVIDER` (unchanged)
- `GGEN_LLM_MODEL` (unchanged)
- `OLLAMA_BASE_URL` (unchanged)
- `OPENAI_API_KEY` (unchanged)
- `ANTHROPIC_API_KEY` (unchanged)

**New additions:**
- Auto-detection when `GGEN_LLM_PROVIDER` not set

---

## 8. Testing Strategy

### 8.1 Unit Tests

Add tests for provider detection:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_provider_detection_with_openai_key() {
        std::env::set_var("OPENAI_API_KEY", "sk-test");
        std::env::remove_var("ANTHROPIC_API_KEY");

        let config = GlobalLlmConfig::default();
        assert_eq!(config.provider, LlmProvider::OpenAI);
    }

    #[test]
    fn test_provider_detection_with_anthropic_key() {
        std::env::remove_var("OPENAI_API_KEY");
        std::env::set_var("ANTHROPIC_API_KEY", "sk-ant-test");

        let config = GlobalLlmConfig::default();
        assert_eq!(config.provider, LlmProvider::Anthropic);
    }

    #[test]
    fn test_provider_detection_fallback_to_mock() {
        std::env::remove_var("OPENAI_API_KEY");
        std::env::remove_var("ANTHROPIC_API_KEY");
        // Assume Ollama not running

        let config = GlobalLlmConfig::default();
        assert_eq!(config.provider, LlmProvider::Mock);
    }
}
```

### 8.2 Integration Tests

Test each provider independently:

```rust
#[cfg(feature = "openai-integration")]
#[tokio::test]
async fn test_openai_provider() {
    let config = LlmConfig {
        model: "gpt-4o-mini".to_string(),
        // ...
    };
    let client = GenAiClient::new(config).unwrap();
    let response = client.complete("Hello").await;
    assert!(response.is_ok());
}

#[cfg(feature = "anthropic-integration")]
#[tokio::test]
async fn test_anthropic_provider() {
    // Similar test for Anthropic
}

#[cfg(feature = "ollama-integration")]
#[tokio::test]
async fn test_ollama_provider() {
    // Similar test for Ollama
}
```

### 8.3 End-to-End Tests

Test provider switching:

```rust
#[tokio::test]
async fn test_provider_switching() {
    // Test 1: Use OpenAI
    std::env::set_var("GGEN_LLM_PROVIDER", "openai");
    let config = GlobalLlmConfig::from_env();
    assert_eq!(config.provider, LlmProvider::OpenAI);

    // Test 2: Switch to Anthropic
    std::env::set_var("GGEN_LLM_PROVIDER", "anthropic");
    let config = GlobalLlmConfig::from_env();
    assert_eq!(config.provider, LlmProvider::Anthropic);
}
```

---

## 9. Conclusion

### Summary of Findings

1. **Architecture**: ✅ The `GenAiClient` abstraction is **well-designed** and properly leverages rust-genai
2. **Ollama Bias**: ❌ Significant **hardcoded Ollama preferences** throughout codebase
3. **Provider Methods**: ❌ **Provider-specific utility methods** violate abstraction principles
4. **Configuration**: ⚠️ Default configuration **strongly favors Ollama**
5. **Documentation**: ⚠️ Examples and docs **assume Ollama availability**

### Key Recommendations

#### Critical (Must Fix)
1. 🔴 **Remove** `with_ollama_qwen3_coder()` methods from all generators
2. 🔴 **Implement** intelligent provider detection in `GlobalLlmConfig::default()`
3. 🔴 **Update** documentation to be provider-neutral

#### Important (Should Fix)
4. 🟡 **Change** default Ollama model from `qwen3-coder:30b` to `llama3.2`
5. 🟡 **Add** provider status checking functionality
6. 🟡 **Create** provider-agnostic configuration examples

#### Optional (Nice to Have)
7. 🔵 **Add** `OpenAIConfig` and `AnthropicConfig` for parity
8. 🔵 **Create** `ggen ai provider` CLI commands
9. 🔵 **Improve** error messages when provider unavailable

### Final Assessment

The ggen project has a **solid foundation** for multi-provider support through its use of rust-genai, but **implementation details** undermine this abstraction. The hardcoded Ollama defaults and provider-specific methods create **unnecessary coupling** and **user confusion**.

**With the recommended changes**, ggen will be:
- ✅ Truly provider-agnostic
- ✅ Easier for new users (auto-detection)
- ✅ More maintainable (no scattered defaults)
- ✅ Better documented (clear provider setup)

**Estimated Effort:**
- Phase 1 (Critical): **1 week** (1 developer)
- Phase 2 (Important): **1 week** (1 developer)
- Phase 3 (Optional): **2 weeks** (1 developer)
- **Total**: **4 weeks** for complete provider-agnostic implementation

---

## Appendix A: File-by-File Analysis

### Files with Ollama Hardcoding

| File | Line(s) | Issue | Priority |
|------|---------|-------|----------|
| `ggen-ai/src/config/global.rs` | 96 | Default provider = Ollama | 🔴 High |
| `ggen-ai/src/generators/template.rs` | 33-36 | `with_ollama_qwen3_coder()` method | 🔴 High |
| `ggen-ai/src/generators/ontology.rs` | 86-89 | `with_ollama_qwen3_coder()` method | 🔴 High |
| `ggen-ai/src/generators/sparql.rs` | 33-36 | `with_ollama_qwen3_coder()` method | 🔴 High |
| `ggen-ai/src/generators/refactor.rs` | 86-89 | `with_ollama_qwen3_coder()` method | 🔴 High |
| `ggen-ai/src/swarm/agents/code_generator.rs` | 131-138 | `with_ollama_qwen3_coder()` method | 🔴 High |
| `ggen-ai/src/config/ollama.rs` | 23 | Default model = `qwen3-coder:30b` | 🟡 Medium |
| `config/defaults.toml` | 21, 43 | Provider and model defaults | 🟡 Medium |
| `ggen-ai/src/mcp/tools.rs` | 91 | `OllamaClient::default_config()` | 🟡 Medium |

### Files with Good Provider Abstraction

| File | Reason |
|------|--------|
| `ggen-ai/src/client.rs` | ✅ Provider-agnostic `GenAiClient` |
| `ggen-ai/src/providers/adapter.rs` | ✅ Uniform adapter pattern |
| All `LlmClient` implementations | ✅ Consistent trait implementation |

---

## Appendix B: rust-genai Integration Details

### How rust-genai Routes Providers

The `rust-genai` library uses **intelligent routing**:

1. **Model Name Detection**:
   - `gpt-*` → OpenAI adapter
   - `claude-*` → Anthropic adapter
   - Other → Ollama adapter

2. **Environment Variables**:
   - `OPENAI_API_KEY` → Enable OpenAI
   - `ANTHROPIC_API_KEY` → Enable Anthropic
   - `OLLAMA_BASE_URL` → Configure Ollama endpoint

3. **API Key Presence**:
   - Checks for API keys before attempting provider
   - Falls back to next provider if key missing

### GenAiClient Delegates Everything

```rust
// ggen-ai/src/client.rs
impl GenAiClient {
    pub fn new(config: LlmConfig) -> Result<Self> {
        Ok(Self {
            client: Client::default(),  // ← rust-genai handles routing
            config,
        })
    }
}

impl LlmClient for GenAiClient {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse> {
        let chat_req = ChatRequest::new(vec![ChatMessage::user(prompt)]);
        let response = self.client
            .exec_chat(&self.config.model, chat_req, Some(&chat_options))
            .await?;  // ← rust-genai routes to correct provider
        // ...
    }
}
```

**Key Insight**: ggen's `GenAiClient` **properly delegates** to rust-genai's `Client::default()`, which means the multi-provider routing **already works**. The Ollama bias is only in the **configuration layer**, not the execution layer.

---

**Report Generated:** October 10, 2025
**Analysis Version:** 1.0
**Review Status:** Ready for Implementation
