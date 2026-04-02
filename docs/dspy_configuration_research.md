<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [DSPy Configuration System: Comprehensive Research](#dspy-configuration-system-comprehensive-research)
  - [Table of Contents](#table-of-contents)
  - [Executive Summary](#executive-summary)
  - [1. DSPy Settings & Configuration System](#1-dspy-settings--configuration-system)
    - [1.1 Architecture Overview](#11-architecture-overview)
    - [1.2 Core API](#12-core-api)
      - [Global Configuration](#global-configuration)
      - [Temporary Context Overrides](#temporary-context-overrides)
    - [1.3 Configuration Parameters](#13-configuration-parameters)
    - [1.4 Thread Safety Mechanisms](#14-thread-safety-mechanisms)
  - [2. Context Manager Pattern (dspy.context)](#2-context-manager-pattern-dspycontext)
    - [2.1 Purpose](#21-purpose)
    - [2.2 Implementation Details](#22-implementation-details)
    - [2.3 Usage Examples](#23-usage-examples)
    - [2.4 Context Propagation to Child Threads](#24-context-propagation-to-child-threads)
  - [3. LM & RM Configuration](#3-lm--rm-configuration)
    - [3.1 Language Model (LM) Configuration](#31-language-model-lm-configuration)
      - [Supported Providers](#supported-providers)
      - [Setup Examples](#setup-examples)
      - [LM Configuration Parameters](#lm-configuration-parameters)
    - [3.2 Retrieval Model (RM) Configuration](#32-retrieval-model-rm-configuration)
      - [Purpose](#purpose)
      - [Supported Retrieval Backends](#supported-retrieval-backends)
      - [Setup Examples](#setup-examples-1)
    - [3.3 Configuration Hierarchy](#33-configuration-hierarchy)
  - [4. Compilation & Module Freezing](#4-compilation--module-freezing)
    - [4.1 Teleprompters → Optimizers](#41-teleprompters-%E2%86%92-optimizers)
    - [4.2 Compilation Process](#42-compilation-process)
    - [4.3 Module Freezing](#43-module-freezing)
    - [4.4 Compilation Example](#44-compilation-example)
  - [5. Environment Variables & Config Files](#5-environment-variables--config-files)
    - [5.1 Environment Variable Configuration](#51-environment-variable-configuration)
      - [API Key Management](#api-key-management)
      - [Provider-Specific Environment Variables](#provider-specific-environment-variables)
    - [5.2 Configuration Files (dspy-cli)](#52-configuration-files-dspy-cli)
      - [Configuration File Structure](#configuration-file-structure)
      - [Loading Configuration](#loading-configuration)
    - [5.3 Best Practices](#53-best-practices)
  - [6. Settings Propagation Through Module Chains](#6-settings-propagation-through-module-chains)
    - [6.1 Module Composition](#61-module-composition)
    - [6.2 Configuration Inheritance](#62-configuration-inheritance)
    - [6.3 Child Module Discovery](#63-child-module-discovery)
    - [6.4 Batch Processing with Configuration](#64-batch-processing-with-configuration)
  - [7. Cache Configuration System](#7-cache-configuration-system)
    - [7.1 Three-Layer Caching Architecture](#71-three-layer-caching-architecture)
    - [7.2 Default Behavior](#72-default-behavior)
    - [7.3 Configuration API](#73-configuration-api)
      - [Global Cache Configuration](#global-cache-configuration)
      - [Disable Caching](#disable-caching)
    - [7.4 Cache Directories](#74-cache-directories)
    - [7.5 Provider-Side Prompt Caching](#75-provider-side-prompt-caching)
  - [8. Usage Tracking & Cost Monitoring](#8-usage-tracking--cost-monitoring)
    - [8.1 Built-In Token Usage Tracking](#81-built-in-token-usage-tracking)
    - [8.2 Enabling Usage Tracking](#82-enabling-usage-tracking)
    - [8.3 Usage Data Structure](#83-usage-data-structure)
    - [8.4 LM History](#84-lm-history)
    - [8.5 Caching Consideration](#85-caching-consideration)
    - [8.6 Integration with Observability Platforms](#86-integration-with-observability-platforms)
      - [MLflow Integration](#mlflow-integration)
      - [Langfuse Integration](#langfuse-integration)
  - [9. Rust Configuration Architecture Recommendations](#9-rust-configuration-architecture-recommendations)
    - [9.1 Core Design Principles](#91-core-design-principles)
    - [9.2 Recommended Architecture](#92-recommended-architecture)
      - [9.2.1 Global Configuration with OnceLock](#921-global-configuration-with-oncelock)
      - [9.2.2 Thread-Local Context Overrides](#922-thread-local-context-overrides)
      - [9.2.3 Configuration Resolution](#923-configuration-resolution)
    - [9.3 Module Configuration Propagation](#93-module-configuration-propagation)
      - [9.3.1 Configuration in Module Trait](#931-configuration-in-module-trait)
      - [9.3.2 Batch Processing with Context Propagation](#932-batch-processing-with-context-propagation)
    - [9.4 Cache Configuration](#94-cache-configuration)
      - [9.4.1 Cache Architecture](#941-cache-architecture)
      - [9.4.2 Cache Configuration API](#942-cache-configuration-api)
    - [9.5 Usage Tracking](#95-usage-tracking)
      - [9.5.1 Usage Tracker Structure](#951-usage-tracker-structure)
      - [9.5.2 Integration with Configuration](#952-integration-with-configuration)
      - [9.5.3 LM Client Integration](#953-lm-client-integration)
    - [9.6 Module Freezing & Compilation](#96-module-freezing--compilation)
      - [9.6.1 Freezing Mechanism](#961-freezing-mechanism)
      - [9.6.2 Optimizer Implementation](#962-optimizer-implementation)
    - [9.7 Environment Variable Handling](#97-environment-variable-handling)
      - [9.7.1 Standardized Environment Variables](#971-standardized-environment-variables)
      - [9.7.2 Environment Variable Loading](#972-environment-variable-loading)
    - [9.8 Complete Configuration Example](#98-complete-configuration-example)
      - [9.8.1 Initialization](#981-initialization)
    - [9.9 Implementation Status & Next Steps](#99-implementation-status--next-steps)
      - [9.9.1 Current Status (Already Implemented ✅)](#991-current-status-already-implemented-)
      - [9.9.2 Needs Implementation (Recommended Additions)](#992-needs-implementation-recommended-additions)
    - [9.10 Testing Strategy](#910-testing-strategy)
      - [9.10.1 Unit Tests](#9101-unit-tests)
      - [9.10.2 Integration Tests](#9102-integration-tests)
  - [10. Sources & References](#10-sources--references)
    - [Primary Documentation](#primary-documentation)
    - [API References](#api-references)
    - [Integration Guides](#integration-guides)
    - [GitHub Resources](#github-resources)
    - [Academic Papers](#academic-papers)
    - [Tutorials & Articles](#tutorials--articles)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# DSPy Configuration System: Comprehensive Research

**Research Date**: 2026-01-11
**Target**: Python DSPy Configuration & Settings Management
**Purpose**: Design Rust DSPy configuration architecture for ggen

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [DSPy Settings & Configuration System](#dspy-settings--configuration-system)
3. [Context Manager Pattern (dspy.context)](#context-manager-pattern-dspycontext)
4. [LM & RM Configuration](#lm--rm-configuration)
5. [Compilation & Module Freezing](#compilation--module-freezing)
6. [Environment Variables & Config Files](#environment-variables--config-files)
7. [Settings Propagation Through Module Chains](#settings-propagation-through-module-chains)
8. [Cache Configuration System](#cache-configuration-system)
9. [Usage Tracking & Cost Monitoring](#usage-tracking--cost-monitoring)
10. [Rust Configuration Architecture Recommendations](#rust-configuration-architecture-recommendations)

---

## Executive Summary

Python DSPy employs a sophisticated configuration system built on three core principles:

1. **Singleton Settings with Thread-Local Overrides**: Global configuration via `dspy.configure()` with thread-safe temporary overrides via `dspy.context()`
2. **Ownership Model**: Single-thread ownership prevents concurrent configuration conflicts
3. **Automatic Propagation**: Configuration context automatically flows to child threads and async tasks

**Key Implementation Files** (Python DSPy):
- `dspy/dsp/utils/settings.py` - Core Settings singleton class
- `dspy/utils/parallelizer.py` - Parallel execution context propagation
- `dspy/utils/asyncify.py` - Async context handling

---

## 1. DSPy Settings & Configuration System

### 1.1 Architecture Overview

DSPy's configuration management uses a **singleton Settings class** that manages:
- **Global configuration** (main thread only)
- **Thread-local overrides** (via `contextvars.ContextVar`)
- **Ownership tracking** (prevents concurrent configuration)

### 1.2 Core API

#### Global Configuration

```python
import dspy

# Global configuration (only owner thread can call)
dspy.configure(
    lm=lm_instance,          # Language model
    rm=rm_instance,          # Retrieval model (optional)
    cache=True,              # Enable caching
    track_usage=True,        # Track token usage
)
```

**Recent API Change**: The older `dspy.settings.configure()` has been simplified to `dspy.configure()` in recent versions.

#### Temporary Context Overrides

```python
# Temporary override (any thread can call)
with dspy.context(lm=alternative_lm, temperature=0.9):
    # Uses alternative_lm with temperature 0.9
    result = module.forward(inputs)
# Reverts to global configuration
```

### 1.3 Configuration Parameters

Common configuration options:

| Parameter | Type | Description | Default |
|-----------|------|-------------|---------|
| `lm` | LM | Language model instance | Required |
| `rm` | RM | Retrieval model instance | None |
| `cache` | bool | Enable response caching | True |
| `track_usage` | bool | Track token usage | False |
| `max_tokens` | int | Max generation tokens | Model-specific |
| `temperature` | float | Sampling temperature | 0.7 |

### 1.4 Thread Safety Mechanisms

**Ownership Tracking**:
- First thread to call `dspy.configure()` becomes the "owner"
- Stored in `config_owner_thread_id`
- Subsequent calls from other threads raise `RuntimeError`

**Thread Isolation**:
- Uses `contextvars.ContextVar` (not `threading.local()`)
- Provides automatic thread isolation
- Proper context propagation to child tasks

**Deep Copy for Thread Safety**:
- Usage trackers are deep-copied per thread
- Prevents cross-thread contamination

---

## 2. Context Manager Pattern (dspy.context)

### 2.1 Purpose

The `dspy.context()` function enables **temporary configuration overrides** without affecting global state.

### 2.2 Implementation Details

**Storage**: Uses `contextvars.ContextVar` stored as `thread_local_overrides`

**Behavior**:
1. Preserves original thread-local overrides
2. Applies temporary overrides within context block
3. Restores original overrides on exit
4. Automatically propagates to child threads spawned with DSPy primitives

### 2.3 Usage Examples

```python
# Global configuration
dspy.configure(lm=dspy.LM("gpt-4o-mini"))

# Temporary override for specific task
with dspy.context(lm=dspy.LM("gpt-4o"), temperature=0.0):
    # Deterministic responses with GPT-4o
    response = predictor.forward(inputs)

# Back to gpt-4o-mini with default temperature
```

### 2.4 Context Propagation to Child Threads

**Key Feature**: Thread-local settings automatically propagate to child threads when using DSPy's execution primitives:

```python
# Parent thread configuration
with dspy.context(lm=custom_lm):
    # Child threads inherit custom_lm context
    results = module.batch(examples, num_threads=4)
```

**Implementation**: The `ParallelExecutor` ensures configuration isolation by inheriting the parent's configuration context while maintaining thread isolation.

---

## 3. LM & RM Configuration

### 3.1 Language Model (LM) Configuration

#### Supported Providers

DSPy supports **dozens of LLM providers** through [LiteLLM](https://docs.litellm.ai/), including:
- OpenAI (GPT-3.5, GPT-4, GPT-4o)
- Anthropic (Claude 3, Claude 3.5)
- Google (Gemini, PaLM)
- Cohere
- Ollama (local models)
- Custom providers

#### Setup Examples

**OpenAI**:
```python
import dspy

lm = dspy.LM(
    "openai/gpt-4o-mini",
    max_tokens=512,
    api_key=os.environ.get("OPENAI_API_KEY")
)
dspy.configure(lm=lm)
```

**Ollama (Local Models)**:
```python
lm = dspy.OllamaLocal(
    model="llama3",
    timeout_s=180
)
dspy.configure(lm=lm)
```

**Generic LiteLLM Pattern**:
```python
# Format: {provider_name}/{model_name}
lm = dspy.LM(
    "anthropic/claude-3-5-sonnet-20241022",
    api_key=os.environ.get("ANTHROPIC_API_KEY")
)
```

#### LM Configuration Parameters

```python
lm = dspy.LM(
    model="openai/gpt-4o-mini",
    api_key="...",              # Provider API key
    max_tokens=4096,            # Max generation tokens
    temperature=0.7,            # Sampling temperature (0.0-2.0)
    top_p=0.9,                  # Nucleus sampling
    n=1,                        # Number of completions
    stop=["END"],               # Stop sequences
    cache=True,                 # Enable caching
    cache_control_injection_points=[...]  # Prompt caching
)
```

### 3.2 Retrieval Model (RM) Configuration

#### Purpose

The retrieval layer uses `dspy.Retrieve` which utilizes the configured RM to retrieve relevant passages/documents for search queries.

#### Supported Retrieval Backends

- **Qdrant** - Vector database
- **ColBERTv2** - Neural retrieval
- **Weaviate** - Vector search
- **Custom** - User-defined retrieval

#### Setup Examples

**Qdrant**:
```python
from dspy.retrieve.qdrant import QdrantRM

qdrant_retriever = QdrantRM(
    qdrant_collection_name="my_collection",
    qdrant_client=client,
    vector_name="embedding",
    document_field="content",
    k=3  # Top-k retrieval
)

dspy.configure(lm=lm, rm=qdrant_retriever)
```

**Usage in Modules**:
```python
class RAGModule(dspy.Module):
    def __init__(self):
        super().__init__()
        self.retrieve = dspy.Retrieve(k=3)  # Uses configured RM
        self.generate = dspy.ChainOfThought("context, question -> answer")

    def forward(self, question):
        context = self.retrieve(question)
        return self.generate(context=context, question=question)
```

### 3.3 Configuration Hierarchy

```python
# Global configuration
dspy.configure(lm=default_lm, rm=default_rm)

# Temporary override (LM only)
with dspy.context(lm=alternative_lm):
    # Uses alternative_lm but still uses default_rm
    result = module.forward(inputs)

# Temporary override (both LM and RM)
with dspy.context(lm=alt_lm, rm=alt_rm):
    result = module.forward(inputs)
```

---

## 4. Compilation & Module Freezing

### 4.1 Teleprompters → Optimizers

**Historical Note**: DSPy originally called optimizers "teleprompters". The framework is making an official name update throughout the library.

**Access**: `from dspy.teleprompt import *` or `from dspy.optimizers import *`

### 4.2 Compilation Process

**Purpose**: Transform a DSPy program into an optimized version with:
- High-quality instructions
- Automatic few-shot examples
- Updated LM weights (for some optimizers)

**Common Optimizers**:

| Optimizer | Description | Optimizes |
|-----------|-------------|-----------|
| `BootstrapFewShot` | Generates complete demonstrations | Few-shot examples |
| `MIPROv2` | Multi-stage optimization | Instructions + few-shot |
| `COPRO` | Coordinate ascent prompt optimization | Instructions |
| `BootstrapFewShotWithRandomSearch` | Bootstrap with random search | Few-shot + search |

### 4.3 Module Freezing

**Mechanism**: Modules are frozen by setting `._compiled = True` attribute.

**Purpose**:
- Indicates module has completed optimizer compilation
- Prevents further parameter adjustments
- Ensures bootstrapped demonstrations aren't modified

**Implementation** (BootstrapFewShot example):

```python
class BootstrapFewShot:
    def compile(self, student, trainset):
        # Ensure student is frozen before teacher propagates demos
        student._compiled = True

        # Bootstrap demonstrations from teacher
        demonstrations = self._bootstrap(student, trainset)

        # Create optimized predictor with frozen demonstrations
        return OptimizedPredictor(
            signature=student.signature(),
            demonstrations=demonstrations
        )
```

### 4.4 Compilation Example

```python
from dspy.teleprompt import BootstrapFewShot

# Define metric
def exact_match(example, pred, trace=None):
    return example.answer == pred.answer

# Create optimizer
teleprompter = BootstrapFewShot(
    metric=exact_match,
    max_bootstrapped_demos=4,
    max_labeled_demos=16
)

# Compile program
compiled_program = teleprompter.compile(
    student=my_program,
    trainset=train_examples
)
```

---

## 5. Environment Variables & Config Files

### 5.1 Environment Variable Configuration

#### API Key Management

DSPy supports multiple methods for API key configuration:

**Method 1: Environment Variables**
```bash
export OPENAI_API_KEY="sk-..."
export ANTHROPIC_API_KEY="sk-ant-..."
export COHERE_API_KEY="..."
```

**Method 2: In-Code Environment Setup**
```python
import os
os.environ['OPENAI_API_KEY'] = 'sk-...'

lm = dspy.LM('openai/gpt-4o-mini')
dspy.configure(lm=lm)
```

**Method 3: Direct API Key Parameter**
```python
lm = dspy.LM(
    'openai/gpt-4o-mini',
    api_key='sk-...'  # Direct parameter
)
```

#### Provider-Specific Environment Variables

Each provider uses specific environment variables:

```python
# OpenAI
OPENAI_API_KEY="sk-..."

# Anthropic
ANTHROPIC_API_KEY="sk-ant-..."

# Cohere
COHERE_API_KEY="..."

# Custom base URLs
OPENAI_API_BASE="https://custom.openai.com/v1"
```

### 5.2 Configuration Files (dspy-cli)

The `dspy-cli` tool supports configuration files for managing multiple models and environments.

#### Configuration File Structure

**Location**: `.env` file at project root

**Example**:
```yaml
# Model registry
models:
  default: openai:gpt-4o-mini
  registry:
    openai:gpt-4o-mini:
      model: openai/gpt-4o-mini
      env: OPENAI_API_KEY
      max_tokens: 512
      temperature: 0.7

    anthropic:claude-3:
      model: anthropic/claude-3-sonnet-20240229
      env: ANTHROPIC_API_KEY
      max_tokens: 4096
      temperature: 0.0
```

#### Loading Configuration

```python
from dspy_cli import load_config

config = load_config()
lm = dspy.LM(config['models']['default'])
dspy.configure(lm=lm)
```

### 5.3 Best Practices

**Security**:
- Store API keys in environment variables or secure vaults
- Never commit API keys to repositories
- Use `.gitignore` for `.env` files

**Configuration Precedence**:
1. Direct parameter (highest priority)
2. Environment variable
3. Configuration file
4. Default value (lowest priority)

---

## 6. Settings Propagation Through Module Chains

### 6.1 Module Composition

DSPy modules are inspired by PyTorch's `nn.Module`, enabling hierarchical composition:

```python
class RAGPipeline(dspy.Module):
    def __init__(self):
        super().__init__()
        self.retrieve = dspy.Retrieve(k=3)
        self.summarize = dspy.ChainOfThought("context -> summary")
        self.answer = dspy.ChainOfThought("summary, question -> answer")

    def forward(self, question):
        context = self.retrieve(question)
        summary = self.summarize(context=context)
        return self.answer(summary=summary, question=question)
```

### 6.2 Configuration Inheritance

**Key Principle**: Configuration flows from parent to child modules automatically.

**Mechanism**:
1. Parent module accesses global/thread-local settings
2. Child modules inherit parent's configuration context
3. Temporary overrides via `dspy.context()` affect entire module chain

**Example**:
```python
# Global configuration
dspy.configure(lm=default_lm)

pipeline = RAGPipeline()

# All child modules use default_lm
result = pipeline.forward("What is DSPy?")

# Temporary override affects entire pipeline
with dspy.context(lm=alternative_lm):
    # retrieve, summarize, and answer all use alternative_lm
    result = pipeline.forward("What is DSPy?")
```

### 6.3 Child Module Discovery

DSPy uses **breadth-first search** to discover all sub-modules and parameters.

**Algorithm**:
```python
def named_sub_modules(module, prefix=""):
    """Find all sub-modules in the module tree."""
    for name, value in vars(module).items():
        if isinstance(value, dspy.Module):
            yield f"{prefix}{name}", value
            yield from named_sub_modules(value, f"{prefix}{name}.")
        elif isinstance(value, (list, tuple)):
            for i, item in enumerate(value):
                if isinstance(item, dspy.Module):
                    yield f"{prefix}{name}[{i}]", item
        elif isinstance(value, dict):
            for key, item in value.items():
                if isinstance(item, dspy.Module):
                    yield f"{prefix}{name}[{key}]", item
```

**Path Resolution**: If a sub-module is accessible via multiple paths, only one path is returned.

### 6.4 Batch Processing with Configuration

The `batch()` method enables parallel processing while preserving configuration:

```python
# Parent thread configuration
with dspy.context(lm=custom_lm):
    # Each worker thread inherits custom_lm context
    results = module.batch(
        examples,
        num_threads=4,
        show_progress=True
    )
```

**Implementation Details**:
- `ParallelExecutor` ensures configuration isolation
- Worker threads inherit parent's `thread_local_overrides`
- Each thread maintains isolated usage tracking

---

## 7. Cache Configuration System

### 7.1 Three-Layer Caching Architecture

DSPy implements a sophisticated caching system with three distinct layers:

| Layer | Implementation | Purpose | Scope |
|-------|---------------|---------|-------|
| **In-Memory** | `cachetools.LRUCache` | Fast repeated calls | Process-local |
| **On-Disk** | `diskcache.FanoutCache` | Persistent across runs | Machine-local |
| **Server-Side** | Provider prompt cache | Reduce LLM costs | Provider-managed |

### 7.2 Default Behavior

**Automatic Caching**: Both in-memory and on-disk caching are **enabled by default**.

**Implication**: Repeating the same LLM call returns cached results without API calls.

### 7.3 Configuration API

#### Global Cache Configuration

```python
import dspy

dspy.configure_cache(
    enable_disk_cache=True,         # On-disk caching
    enable_memory_cache=True,       # In-memory caching
    disk_size_limit_bytes=10**9,    # 1GB disk cache
    memory_max_entries=1000,        # Max in-memory entries
)
```

#### Disable Caching

**Option 1: Global Disable**
```python
dspy.configure_cache(
    enable_disk_cache=False,
    enable_memory_cache=False,
)
```

**Option 2: Per-Call Disable**
```python
lm = dspy.LM("openai/gpt-4o-mini", cache=False)

# Or temporary override
with dspy.context(lm=dspy.LM("model-name", cache=False)):
    result = module.forward(inputs)
```

### 7.4 Cache Directories

DSPy uses environment-specific cache directories:

```bash
# Old clients (dspy.OpenAI, dspy.ColBERTv2, etc.)
DSP_CACHEDIR=/path/to/cache

# New dspy.LM client
DSPY_CACHEDIR=/path/to/cache
```

### 7.5 Provider-Side Prompt Caching

**Supported Providers**: Anthropic, OpenAI (with prompt caching features)

**Configuration**:
```python
lm = dspy.LM(
    "anthropic/claude-3-5-sonnet-20241022",
    cache_control_injection_points=[
        # Specify which parts of prompt to cache
        {"type": "system", "cache": True},
        {"type": "user", "cache": True}
    ]
)
```

**Benefits**:
- Reduces API costs for repeated prompts
- Faster response times for cached segments
- Transparent to application logic

---

## 8. Usage Tracking & Cost Monitoring

### 8.1 Built-In Token Usage Tracking

**Availability**: DSPy version 2.6.16 and later

**Purpose**: Track language model usage across all module calls for cost monitoring and optimization.

### 8.2 Enabling Usage Tracking

```python
import dspy

# Enable global usage tracking
dspy.configure(
    lm=lm,
    track_usage=True
)

# Run module
result = module.forward(inputs)

# Access usage data
usage = result.get_lm_usage()
```

### 8.3 Usage Data Structure

The `get_lm_usage()` method returns a dictionary mapping each LM name to usage statistics:

```python
{
    "openai/gpt-4o-mini": {
        "completion_tokens": 256,
        "prompt_tokens": 512,
        "total_tokens": 768,
        "cost": 0.00384,  # USD cost estimate
        "requests": 1
    }
}
```

### 8.4 LM History

Every LM object maintains interaction history:

```python
lm = dspy.LM("openai/gpt-4o-mini")

# After calls, access history
for call in lm.history:
    print(f"Inputs: {call.inputs}")
    print(f"Outputs: {call.outputs}")
    print(f"Tokens: {call.token_usage}")
    print(f"Cost: ${call.cost}")
    print(f"Metadata: {call.metadata}")
```

### 8.5 Caching Consideration

**Important**: Cached responses don't count toward usage statistics.

**Workaround for Experimentation**:
```python
# Disable cache to see true token usage
with dspy.context(
    lm=dspy.LM("model-name", cache=False),
    track_usage=True
):
    result = module.forward(inputs)
    usage = result.get_lm_usage()
```

### 8.6 Integration with Observability Platforms

#### MLflow Integration

**Requirement**: MLflow >= 3.5.0

**Automatic Tracking**:
- Token usage logged in `mlflow.chat.tokenUsage` attribute
- Total token usage in `token_usage` field
- Cost estimates (if available)

```python
import mlflow
mlflow.dspy.autolog()

with mlflow.start_run():
    result = module.forward(inputs)
    # Token usage automatically logged
```

#### Langfuse Integration

**Setup**:
```python
from langfuse.decorators import langfuse_context
from openinference.instrumentation.dspy import DSPyInstrumentor

# Instrument DSPy
DSPyInstrumentor().instrument()

# Use as normal
result = module.forward(inputs)
# Traces automatically sent to Langfuse
```

**Tracked Metrics**:
- Token counts (prompt, completion, total)
- Latency
- Cost estimates
- Input/output traces
- Module call hierarchy

---

## 9. Rust Configuration Architecture Recommendations

### 9.1 Core Design Principles

Based on Python DSPy's architecture and Rust's strengths, the ggen Rust implementation should follow these principles:

1. **Type-First Design**: Constraints in types, compiler verifies correctness
2. **Result<T, E>**: All fallible operations return `Result`
3. **Zero Unwrap**: No panics in production code
4. **Thread Safety**: `Send + Sync` where appropriate
5. **Zero-Cost Abstractions**: Generic types eliminate runtime overhead

### 9.2 Recommended Architecture

#### 9.2.1 Global Configuration with OnceLock

**Current Implementation** (already in ggen):
```rust
use std::sync::OnceLock;

static GLOBAL_CONFIG: OnceLock<GlobalLlmConfig> = OnceLock::new();

pub fn init_global_config() -> &'static GlobalLlmConfig {
    GLOBAL_CONFIG.get_or_init(GlobalLlmConfig::from_env)
}

pub fn get_global_config() -> &'static GlobalLlmConfig {
    init_global_config()
}
```

**Advantages**:
- Thread-safe initialization
- Zero runtime overhead after initialization
- Immutable global state (prevents accidental modifications)

**Status**: ✅ Already implemented in `/home/user/ggen/crates/ggen-ai/src/config/global.rs`

#### 9.2.2 Thread-Local Context Overrides

**Recommendation**: Use `tokio::task_local!` for async context and `thread_local!` for sync context.

**Proposed Implementation**:

```rust
use tokio::task_local;
use std::cell::RefCell;

// Thread-local overrides for async contexts
task_local! {
    static LM_OVERRIDE: RefCell<Option<Arc<dyn LlmClient>>>;
    static TEMPERATURE_OVERRIDE: RefCell<Option<f32>>;
    static CACHE_OVERRIDE: RefCell<Option<bool>>;
}

/// Context guard that restores previous settings on drop
pub struct ContextGuard {
    previous_lm: Option<Arc<dyn LlmClient>>,
    previous_temperature: Option<f32>,
    previous_cache: Option<bool>,
}

impl Drop for ContextGuard {
    fn drop(&mut self) {
        // Restore previous values
        LM_OVERRIDE.with(|cell| *cell.borrow_mut() = self.previous_lm.clone());
        TEMPERATURE_OVERRIDE.with(|cell| *cell.borrow_mut() = self.previous_temperature);
        CACHE_OVERRIDE.with(|cell| *cell.borrow_mut() = self.previous_cache);
    }
}

/// Create a temporary configuration context
pub fn with_context<F, R>(
    lm: Option<Arc<dyn LlmClient>>,
    temperature: Option<f32>,
    cache: Option<bool>,
    f: F,
) -> R
where
    F: FnOnce() -> R,
{
    // Save previous values
    let guard = ContextGuard {
        previous_lm: LM_OVERRIDE.with(|cell| cell.borrow().clone()),
        previous_temperature: TEMPERATURE_OVERRIDE.with(|cell| *cell.borrow()),
        previous_cache: CACHE_OVERRIDE.with(|cell| *cell.borrow()),
    };

    // Apply new values
    LM_OVERRIDE.with(|cell| *cell.borrow_mut() = lm);
    TEMPERATURE_OVERRIDE.with(|cell| *cell.borrow_mut() = temperature);
    CACHE_OVERRIDE.with(|cell| *cell.borrow_mut() = cache);

    // Execute function
    let result = f();

    // Guard automatically restores on drop
    drop(guard);
    result
}
```

**Usage Example**:
```rust
// Global configuration
let global_config = get_global_config();

// Temporary override
with_context(
    Some(alternative_lm),
    Some(0.0),  // Deterministic
    None,
) || {
    // Uses alternative_lm with temperature 0.0
    let result = module.forward(inputs).await?;
    Ok(result)
})?;
// Automatically reverts to global config
```

#### 9.2.3 Configuration Resolution

**Proposed Hierarchy** (highest to lowest priority):

```rust
pub fn get_current_lm() -> Result<Arc<dyn LlmClient>, GgenAiError> {
    // 1. Thread-local override (highest priority)
    if let Some(lm) = LM_OVERRIDE.try_with(|cell| cell.borrow().clone()).ok().flatten() {
        return Ok(lm);
    }

    // 2. Global configuration
    let global = get_global_config();
    global.create_contextual_client()
}

pub fn get_current_temperature() -> f32 {
    // 1. Thread-local override
    if let Some(temp) = TEMPERATURE_OVERRIDE.try_with(|cell| *cell.borrow()).ok().flatten() {
        return temp;
    }

    // 2. Global configuration
    let global = get_global_config();
    global.settings.default_temperature.unwrap_or(0.7)
}
```

### 9.3 Module Configuration Propagation

#### 9.3.1 Configuration in Module Trait

**Current Implementation**:
```rust
#[async_trait::async_trait]
pub trait Module: Send + Sync {
    fn signature(&self) -> &Signature;

    async fn forward(
        &self,
        inputs: HashMap<String, Value>
    ) -> Result<HashMap<String, Value>, ModuleError>;
}
```

**Recommendation**: Keep trait clean, modules access configuration via helpers.

**Helper Functions**:
```rust
impl Predictor {
    async fn call_llm(&self, prompt: &str) -> Result<String, ModuleError> {
        // Resolve LM from context or global
        let lm = get_current_lm()
            .map_err(|e| ModuleError::LlmError(e.to_string()))?;

        // Resolve temperature from context or global
        let temperature = get_current_temperature();

        // Call LM with resolved configuration
        let response = lm.generate(prompt, temperature).await
            .map_err(|e| ModuleError::LlmError(e.to_string()))?;

        Ok(response)
    }
}
```

#### 9.3.2 Batch Processing with Context Propagation

**Proposed Implementation**:

```rust
use rayon::prelude::*;

impl<M: Module> Module for M {
    /// Process multiple examples in parallel while preserving context
    async fn batch(
        &self,
        examples: Vec<HashMap<String, Value>>,
        num_threads: usize,
    ) -> Result<Vec<HashMap<String, Value>>, ModuleError> {
        // Capture current context
        let lm_override = LM_OVERRIDE.try_with(|cell| cell.borrow().clone()).ok().flatten();
        let temp_override = TEMPERATURE_OVERRIDE.try_with(|cell| *cell.borrow()).ok().flatten();
        let cache_override = CACHE_OVERRIDE.try_with(|cell| *cell.borrow()).ok().flatten();

        // Parallel processing with context propagation
        let results: Result<Vec<_>, _> = examples
            .par_iter()
            .with_max_len(num_threads)
            .map(|inputs| {
                // Apply parent context in worker thread
                with_context(lm_override.clone(), temp_override, cache_override, || {
                    // Block on async forward (requires tokio runtime)
                    tokio::runtime::Handle::current()
                        .block_on(self.forward(inputs.clone()))
                })
            })
            .collect();

        results
    }
}
```

### 9.4 Cache Configuration

#### 9.4.1 Cache Architecture

**Recommendation**: Three-tier caching similar to Python DSPy.

**Proposed Structure**:

```rust
use std::sync::Arc;
use moka::sync::Cache as MemoryCache;
use sled::Db as DiskCache;

pub struct CacheManager {
    /// In-memory LRU cache
    memory_cache: Option<Arc<MemoryCache<String, String>>>,

    /// On-disk persistent cache
    disk_cache: Option<Arc<DiskCache>>,

    /// Cache configuration
    config: CacheConfig,
}

#[derive(Debug, Clone)]
pub struct CacheConfig {
    pub enable_memory: bool,
    pub enable_disk: bool,
    pub memory_max_entries: usize,
    pub disk_size_limit_bytes: u64,
    pub ttl_seconds: Option<u64>,
}

impl CacheManager {
    pub fn new(config: CacheConfig) -> Result<Self, GgenAiError> {
        let memory_cache = if config.enable_memory {
            Some(Arc::new(
                MemoryCache::builder()
                    .max_capacity(config.memory_max_entries as u64)
                    .build()
            ))
        } else {
            None
        };

        let disk_cache = if config.enable_disk {
            let cache_dir = std::env::var("DSPY_CACHE_DIR")
                .unwrap_or_else(|_| ".dspy_cache".to_string());
            Some(Arc::new(sled::open(cache_dir)?))
        } else {
            None
        };

        Ok(Self {
            memory_cache,
            disk_cache,
            config,
        })
    }

    pub fn get(&self, key: &str) -> Option<String> {
        // Try memory cache first
        if let Some(ref cache) = self.memory_cache {
            if let Some(value) = cache.get(key) {
                return Some(value);
            }
        }

        // Try disk cache
        if let Some(ref cache) = self.disk_cache {
            if let Ok(Some(value)) = cache.get(key) {
                let value_str = String::from_utf8_lossy(&value).to_string();

                // Populate memory cache
                if let Some(ref mem) = self.memory_cache {
                    mem.insert(key.to_string(), value_str.clone());
                }

                return Some(value_str);
            }
        }

        None
    }

    pub fn set(&self, key: String, value: String) -> Result<(), GgenAiError> {
        // Set in memory cache
        if let Some(ref cache) = self.memory_cache {
            cache.insert(key.clone(), value.clone());
        }

        // Set in disk cache
        if let Some(ref cache) = self.disk_cache {
            cache.insert(key.as_bytes(), value.as_bytes())?;
            cache.flush()?;
        }

        Ok(())
    }
}
```

**Integration with LM Client**:

```rust
impl GenAiClient {
    pub async fn generate_cached(
        &self,
        prompt: &str,
        temperature: f32,
    ) -> Result<String, GgenAiError> {
        // Check cache configuration
        let use_cache = CACHE_OVERRIDE
            .try_with(|cell| *cell.borrow())
            .ok()
            .flatten()
            .unwrap_or(true);  // Default: cache enabled

        if !use_cache {
            return self.generate(prompt, temperature).await;
        }

        // Generate cache key
        let cache_key = format!(
            "{}:{}:{}",
            self.config.model,
            seahash::hash(prompt.as_bytes()),
            (temperature * 100.0) as u32
        );

        // Try cache
        let cache_manager = get_cache_manager();
        if let Some(cached) = cache_manager.get(&cache_key) {
            return Ok(cached);
        }

        // Generate and cache
        let response = self.generate(prompt, temperature).await?;
        cache_manager.set(cache_key, response.clone())?;

        Ok(response)
    }
}
```

#### 9.4.2 Cache Configuration API

```rust
pub fn configure_cache(config: CacheConfig) -> Result<(), GgenAiError> {
    // Update global cache manager
    let manager = CacheManager::new(config)?;
    GLOBAL_CACHE.set(manager)
        .map_err(|_| GgenAiError::configuration("Cache already configured"))?;
    Ok(())
}

pub fn get_cache_manager() -> &'static CacheManager {
    GLOBAL_CACHE.get_or_init(|| {
        CacheManager::new(CacheConfig::default())
            .expect("Failed to initialize default cache")
    })
}

static GLOBAL_CACHE: OnceLock<CacheManager> = OnceLock::new();
```

### 9.5 Usage Tracking

#### 9.5.1 Usage Tracker Structure

```rust
use std::sync::Mutex;
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct UsageStats {
    pub prompt_tokens: u64,
    pub completion_tokens: u64,
    pub total_tokens: u64,
    pub requests: u64,
    pub cached_requests: u64,
    pub cost_usd: f64,
}

pub struct UsageTracker {
    stats: Mutex<HashMap<String, UsageStats>>,
    enabled: bool,
}

impl UsageTracker {
    pub fn new(enabled: bool) -> Self {
        Self {
            stats: Mutex::new(HashMap::new()),
            enabled,
        }
    }

    pub fn track(
        &self,
        model: &str,
        prompt_tokens: u64,
        completion_tokens: u64,
        cached: bool,
    ) {
        if !self.enabled {
            return;
        }

        let mut stats = self.stats.lock().unwrap();
        let entry = stats.entry(model.to_string()).or_default();

        entry.prompt_tokens += prompt_tokens;
        entry.completion_tokens += completion_tokens;
        entry.total_tokens += prompt_tokens + completion_tokens;
        entry.requests += 1;

        if cached {
            entry.cached_requests += 1;
        }

        // Estimate cost (model-specific pricing)
        entry.cost_usd += self.estimate_cost(model, prompt_tokens, completion_tokens);
    }

    fn estimate_cost(
        &self,
        model: &str,
        prompt_tokens: u64,
        completion_tokens: u64,
    ) -> f64 {
        // Model-specific pricing (example for GPT-4o-mini)
        match model {
            "openai/gpt-4o-mini" => {
                let prompt_cost = (prompt_tokens as f64 / 1_000_000.0) * 0.15;
                let completion_cost = (completion_tokens as f64 / 1_000_000.0) * 0.60;
                prompt_cost + completion_cost
            }
            _ => 0.0,  // Unknown model
        }
    }

    pub fn get_stats(&self) -> HashMap<String, UsageStats> {
        self.stats.lock().unwrap().clone()
    }

    pub fn reset(&self) {
        self.stats.lock().unwrap().clear();
    }
}
```

#### 9.5.2 Integration with Configuration

```rust
// Global usage tracker
static USAGE_TRACKER: OnceLock<UsageTracker> = OnceLock::new();

pub fn get_usage_tracker() -> &'static UsageTracker {
    USAGE_TRACKER.get_or_init(|| {
        let enabled = std::env::var("GGEN_TRACK_USAGE")
            .map(|v| v == "true" || v == "1")
            .unwrap_or(false);
        UsageTracker::new(enabled)
    })
}

// Enable/disable tracking
pub fn configure_usage_tracking(enabled: bool) {
    USAGE_TRACKER.set(UsageTracker::new(enabled))
        .unwrap_or_else(|_| {
            // Already configured, update existing
            // (would need interior mutability)
        });
}
```

#### 9.5.3 LM Client Integration

```rust
impl GenAiClient {
    pub async fn generate_with_tracking(
        &self,
        prompt: &str,
        temperature: f32,
    ) -> Result<String, GgenAiError> {
        let response = self.generate_cached(prompt, temperature).await?;

        // Track usage
        let tracker = get_usage_tracker();
        tracker.track(
            &self.config.model,
            estimate_tokens(prompt),  // Tokenization helper
            estimate_tokens(&response),
            false,  // Not cached (would check cache hit)
        );

        Ok(response)
    }
}
```

### 9.6 Module Freezing & Compilation

#### 9.6.1 Freezing Mechanism

**Proposed Design**:

```rust
pub trait Module: Send + Sync {
    fn signature(&self) -> &Signature;

    async fn forward(
        &self,
        inputs: HashMap<String, Value>
    ) -> Result<HashMap<String, Value>, ModuleError>;

    /// Check if module has been compiled/frozen
    fn is_compiled(&self) -> bool {
        false  // Default: not compiled
    }

    /// Freeze module (prevent further optimization)
    fn freeze(&mut self) {
        // Override in implementations that support freezing
    }
}
```

**OptimizedPredictor Implementation**:

```rust
pub struct OptimizedPredictor {
    signature: Signature,
    demonstrations: Vec<Demonstration>,
    model: String,
    temperature: f32,
    compiled: bool,  // Frozen state
}

impl Module for OptimizedPredictor {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    fn is_compiled(&self) -> bool {
        self.compiled
    }

    fn freeze(&mut self) {
        self.compiled = true;
    }

    async fn forward(
        &self,
        inputs: HashMap<String, Value>
    ) -> Result<HashMap<String, Value>, ModuleError> {
        // Use frozen demonstrations
        // ...
    }
}
```

#### 9.6.2 Optimizer Implementation

**BootstrapFewShot with Freezing**:

```rust
impl BootstrapFewShot {
    pub async fn compile(
        &self,
        student: &dyn Module,
        trainset: &[Example],
    ) -> Result<OptimizedPredictor, ModuleError> {
        // Bootstrap demonstrations
        let demonstrations = self.bootstrap(student, trainset).await?;

        // Create optimized predictor
        let mut optimized = OptimizedPredictor::new(
            student.signature().clone(),
            demonstrations,
        );

        // Freeze to prevent further modification
        optimized.freeze();

        Ok(optimized)
    }
}
```

### 9.7 Environment Variable Handling

#### 9.7.1 Standardized Environment Variables

**Proposed Standard** (following DSPy conventions):

```rust
// Provider API keys
pub const OPENAI_API_KEY: &str = "OPENAI_API_KEY";
pub const ANTHROPIC_API_KEY: &str = "ANTHROPIC_API_KEY";
pub const COHERE_API_KEY: &str = "COHERE_API_KEY";

// Model configuration
pub const GGEN_LLM_MODEL: &str = "GGEN_LLM_MODEL";
pub const GGEN_LLM_PROVIDER: &str = "GGEN_LLM_PROVIDER";
pub const GGEN_LLM_TEMPERATURE: &str = "GGEN_LLM_TEMPERATURE";
pub const GGEN_LLM_MAX_TOKENS: &str = "GGEN_LLM_MAX_TOKENS";

// Cache configuration
pub const DSPY_CACHE_DIR: &str = "DSPY_CACHE_DIR";
pub const GGEN_ENABLE_CACHE: &str = "GGEN_ENABLE_CACHE";

// Usage tracking
pub const GGEN_TRACK_USAGE: &str = "GGEN_TRACK_USAGE";

// Test mode
pub const GGEN_TEST_MODE: &str = "GGEN_TEST_MODE";
```

#### 9.7.2 Environment Variable Loading

**Current Implementation Enhancement**:

```rust
impl GlobalLlmConfig {
    pub fn from_env() -> Self {
        let mut config = Self::default();

        // Provider selection
        if let Ok(provider) = std::env::var("GGEN_LLM_PROVIDER") {
            config.provider = match provider.to_lowercase().as_str() {
                "openai" => LlmProvider::OpenAI,
                "anthropic" => LlmProvider::Anthropic,
                "ollama" => LlmProvider::Ollama,
                "mock" => LlmProvider::Mock,
                _ => LlmProvider::Ollama,
            };
        }

        // Model selection with fallback chain
        if let Ok(model) = std::env::var("GGEN_LLM_MODEL")
            .or_else(|_| std::env::var("DEFAULT_MODEL"))
        {
            config.settings.default_model = Some(model);
        }

        // Temperature with validation
        if let Ok(temp_str) = std::env::var("GGEN_LLM_TEMPERATURE") {
            if let Ok(temp) = temp_str.parse::<f32>() {
                if (0.0..=2.0).contains(&temp) {
                    config.settings.default_temperature = Some(temp);
                }
            }
        }

        // Max tokens
        if let Ok(tokens_str) = std::env::var("GGEN_LLM_MAX_TOKENS") {
            if let Ok(tokens) = tokens_str.parse::<u32>() {
                config.settings.default_max_tokens = Some(tokens);
            }
        }

        // Cache configuration
        if let Ok(cache_str) = std::env::var("GGEN_ENABLE_CACHE") {
            // Propagate to cache manager
            let enable = cache_str == "true" || cache_str == "1";
            let _ = configure_cache(CacheConfig {
                enable_memory: enable,
                enable_disk: enable,
                ..Default::default()
            });
        }

        // Usage tracking
        if let Ok(track_str) = std::env::var("GGEN_TRACK_USAGE") {
            let enable = track_str == "true" || track_str == "1";
            configure_usage_tracking(enable);
        }

        config
    }
}
```

### 9.8 Complete Configuration Example

#### 9.8.1 Initialization

```rust
use ggen_ai::config::{
    init_global_config,
    get_global_config,
    with_context,
    configure_cache,
    CacheConfig,
};
use ggen_ai::dspy::{Module, Predictor, Signature, InputField, OutputField};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize global configuration from environment
    init_global_config();

    // Configure cache
    configure_cache(CacheConfig {
        enable_memory: true,
        enable_disk: true,
        memory_max_entries: 1000,
        disk_size_limit_bytes: 1_000_000_000,  // 1GB
        ttl_seconds: Some(86400),  // 24 hours
    })?;

    // Create module
    let signature = Signature::new("QA", "Answer questions")
        .with_input(InputField::new("question", "The question", "String"))
        .with_output(OutputField::new("answer", "The answer", "String"));

    let qa_module = Predictor::new(signature);

    // Use global configuration
    let result1 = qa_module.forward(
        [("question".into(), json!("What is Rust?"))].into()
    ).await?;

    println!("Answer (global config): {}", result1["answer"]);

    // Temporary override for specific use case
    with_context(
        None,  // Keep same LM
        Some(0.0),  // Deterministic temperature
        Some(false),  // Disable cache
    ) || {
        // This block uses temperature=0.0 and no cache
        let result2 = qa_module.forward(
            [("question".into(), json!("What is 2+2?"))].into()
        ).await?;

        println!("Answer (deterministic): {}", result2["answer"]);
        Ok(())
    })?;

    // Get usage statistics
    let stats = get_usage_tracker().get_stats();
    for (model, usage) in stats {
        println!("\nModel: {}", model);
        println!("  Total tokens: {}", usage.total_tokens);
        println!("  Requests: {}", usage.requests);
        println!("  Cost: ${:.4}", usage.cost_usd);
    }

    Ok(())
}
```

### 9.9 Implementation Status & Next Steps

#### 9.9.1 Current Status (Already Implemented ✅)

- ✅ Global configuration singleton (`GlobalLlmConfig`)
- ✅ Environment variable loading (`from_env()`)
- ✅ Multiple LLM provider support
- ✅ Basic configuration structure
- ✅ Test mode detection
- ✅ Provider-specific configurations

#### 9.9.2 Needs Implementation (Recommended Additions)

**High Priority**:

1. **Thread-Local Context Overrides** 🟡
   - Implement `with_context()` using `tokio::task_local!`
   - Add `ContextGuard` for RAII cleanup
   - File: `/home/user/ggen/crates/ggen-ai/src/config/context.rs` (new)

2. **Cache Manager** 🟡
   - Three-tier caching (memory, disk, provider)
   - Configuration API
   - Integration with LLM client
   - File: `/home/user/ggen/crates/ggen-ai/src/cache.rs` (enhance existing)

3. **Usage Tracker** 🟡
   - Token counting and cost estimation
   - Per-model statistics
   - Integration with LLM calls
   - File: `/home/user/ggen/crates/ggen-ai/src/usage_tracker.rs` (new)

**Medium Priority**:

4. **Module Freezing** 🟢
   - Add `is_compiled()` and `freeze()` to Module trait
   - Implement in `OptimizedPredictor`
   - Update `BootstrapFewShot` optimizer
   - File: `/home/user/ggen/crates/ggen-ai/src/dspy/module.rs` (enhance)

5. **Configuration Resolution Helpers** 🟢
   - `get_current_lm()`, `get_current_temperature()`, etc.
   - Centralized configuration access
   - File: `/home/user/ggen/crates/ggen-ai/src/config/resolver.rs` (new)

**Low Priority**:

6. **Advanced Batch Processing** 🔵
   - Parallel execution with context propagation
   - Integration with Rayon/Tokio
   - File: `/home/user/ggen/crates/ggen-ai/src/dspy/batch.rs` (new)

7. **Observability Integration** 🔵
   - OpenTelemetry tracing
   - Metrics export
   - Cost monitoring dashboards
   - File: `/home/user/ggen/crates/ggen-ai/src/observability.rs` (new)

### 9.10 Testing Strategy

#### 9.10.1 Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_global_config_singleton() {
        let config1 = get_global_config();
        let config2 = get_global_config();

        // Same instance
        assert!(std::ptr::eq(config1, config2));
    }

    #[tokio::test]
    async fn test_context_override() {
        // Set up test environment
        std::env::set_var("GGEN_LLM_MODEL", "gpt-4o-mini");
        init_global_config();

        // Global temperature
        let global_temp = get_current_temperature();
        assert_eq!(global_temp, 0.7);

        // Override temperature
        with_context(None, Some(0.0), None) || {
            let overridden_temp = get_current_temperature();
            assert_eq!(overridden_temp, 0.0);
            Ok(())
        }).unwrap();

        // Reverted to global
        let reverted_temp = get_current_temperature();
        assert_eq!(reverted_temp, 0.7);
    }

    #[test]
    fn test_cache_configuration() {
        let config = CacheConfig {
            enable_memory: true,
            enable_disk: true,
            memory_max_entries: 500,
            disk_size_limit_bytes: 500_000_000,
            ttl_seconds: Some(3600),
        };

        configure_cache(config.clone()).unwrap();
        let manager = get_cache_manager();

        // Test cache operations
        manager.set("test_key".to_string(), "test_value".to_string()).unwrap();
        assert_eq!(manager.get("test_key"), Some("test_value".to_string()));
    }

    #[test]
    fn test_usage_tracking() {
        let tracker = UsageTracker::new(true);

        tracker.track("openai/gpt-4o-mini", 100, 50, false);
        tracker.track("openai/gpt-4o-mini", 200, 100, true);

        let stats = tracker.get_stats();
        let model_stats = stats.get("openai/gpt-4o-mini").unwrap();

        assert_eq!(model_stats.prompt_tokens, 300);
        assert_eq!(model_stats.completion_tokens, 150);
        assert_eq!(model_stats.total_tokens, 450);
        assert_eq!(model_stats.requests, 2);
        assert_eq!(model_stats.cached_requests, 1);
        assert!(model_stats.cost_usd > 0.0);
    }
}
```

#### 9.10.2 Integration Tests

```rust
#[cfg(test)]
mod integration_tests {
    use super::*;

    #[tokio::test]
    async fn test_end_to_end_configuration() {
        // Set environment
        std::env::set_var("GGEN_LLM_PROVIDER", "mock");
        std::env::set_var("GGEN_TRACK_USAGE", "true");
        std::env::set_var("GGEN_ENABLE_CACHE", "true");

        // Initialize
        init_global_config();
        configure_cache(CacheConfig::default()).unwrap();

        // Create module
        let signature = Signature::new("Test", "Test module")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"));

        let module = Predictor::new(signature);

        // Test with global config
        let result1 = module.forward(
            [("input".into(), json!("test"))].into()
        ).await.unwrap();

        // Test with context override
        with_context(None, Some(0.0), Some(false)) || {
            let result2 = module.forward(
                [("input".into(), json!("test2"))].into()
            ).await.unwrap();
            Ok(result2)
        }).unwrap();

        // Verify usage tracking
        let stats = get_usage_tracker().get_stats();
        assert!(!stats.is_empty());
    }
}
```

---

## 10. Sources & References

### Primary Documentation
- [DSPy Settings & Configuration Management](https://deepwiki.com/stanfordnlp/dspy/5.1-settings-and-configuration-management)
- [DSPy Configuration & Integration](https://deepwiki.com/stanfordnlp/dspy/5-configuration-and-integration)
- [DSPy Advanced Features](https://deepwiki.com/stanfordnlp/dspy/5-advanced-features)
- [DSPy Language Models](https://dspy.ai/learn/programming/language_models/)
- [DSPy Modules](https://dspy.ai/learn/programming/modules/)

### API References
- [DSPy Cheatsheet](https://dspy.ai/cheatsheet/)
- [DSPy LM API](https://dspy.ai/api/models/LM/)
- [DSPy configure_cache API](https://dspy.ai/api/utils/configure_cache/)
- [DSPy Optimizers](https://dspy.ai/learn/optimization/optimizers/)

### Integration Guides
- [DSPy on Databricks](https://www.databricks.com/blog/dspy-databricks)
- [Building RAG with DSPy](https://www.superteams.ai/blog/step-by-step-guide-to-building-rag-applications-using-dspy-and-llama-3)
- [Stanford DSPy with Qdrant](https://qdrant.tech/documentation/frameworks/dspy/)
- [DSPy Observability with Langfuse](https://langfuse.com/docs/integrations/dspy)
- [MLflow DSPy Tracing](https://mlflow.org/docs/latest/genai/tracing/integrations/listing/dspy/)

### GitHub Resources
- [Stanford DSPy Repository](https://github.com/stanfordnlp/dspy)
- [DSPy Releases](https://github.com/stanfordnlp/dspy/releases)
- [DSPy Issues & Discussions](https://github.com/stanfordnlp/dspy/issues)

### Academic Papers
- [DSPy: Compiling Declarative Language Model Calls into Self-Improving Pipelines](https://arxiv.org/pdf/2310.03714)

### Tutorials & Articles
- [Prompting with DSPy: A New Approach | DigitalOcean](https://www.digitalocean.com/community/tutorials/prompting-with-dspy)
- [DSPy Programming with Language Models | Medium](https://medium.com/@nimritakoul01/dspy-programming-with-language-models-9c01dceb28ed)
- [Context Engineering with DSPy | Towards Data Science](https://towardsdatascience.com/context-engineering-a-comprehensive-hands-on-tutorial-with-dspy/)

---

## Conclusion

Python DSPy's configuration system provides a robust, thread-safe architecture for managing LLM settings across complex applications. The key innovations include:

1. **Singleton with Thread-Local Overrides**: Enables global configuration while supporting temporary, isolated overrides
2. **Automatic Context Propagation**: Configuration flows seamlessly to child threads and async tasks
3. **Ownership Model**: Prevents concurrent configuration conflicts
4. **Multi-Tier Caching**: Optimizes performance and reduces costs
5. **Built-in Usage Tracking**: Provides transparency into token usage and costs

For the Rust ggen implementation, the recommended architecture leverages Rust's strengths:
- **Type safety** through `Result<T, E>` and zero unwrap
- **Thread safety** via `OnceLock`, `tokio::task_local!`, and `Arc`
- **Zero-cost abstractions** through generic types
- **Compile-time guarantees** instead of runtime checks

The proposed implementation builds on ggen's existing foundation while adding crucial features for production DSPy workflows: context management, caching, usage tracking, and module freezing.
