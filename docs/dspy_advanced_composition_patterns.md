<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [DSPy Advanced Composition Patterns and Implementation Roadmap](#dspy-advanced-composition-patterns-and-implementation-roadmap)
  - [Executive Summary](#executive-summary)
  - [1. Advanced Composition Patterns](#1-advanced-composition-patterns)
    - [1.1 Multi-Hop Question Answering](#11-multi-hop-question-answering)
    - [1.2 Self-Ask Pattern](#12-self-ask-pattern)
    - [1.3 ReWOO Pattern (Reasoning WithOut Observation)](#13-rewoo-pattern-reasoning-without-observation)
    - [1.4 STORM Pattern (Synthesis of Topic Outlines through Retrieval and Multi-perspective)](#14-storm-pattern-synthesis-of-topic-outlines-through-retrieval-and-multi-perspective)
  - [2. How Patterns Compose Basic Modules](#2-how-patterns-compose-basic-modules)
    - [2.1 Module Composition Principles](#21-module-composition-principles)
    - [2.2 Basic Module Types](#22-basic-module-types)
  - [3. Retrieve Module and RAG Integration](#3-retrieve-module-and-rag-integration)
    - [3.1 Retrieve Module Architecture](#31-retrieve-module-architecture)
    - [3.2 Supported Backends](#32-supported-backends)
    - [3.3 RAG Pipeline Architecture](#33-rag-pipeline-architecture)
    - [3.4 Performance Improvements](#34-performance-improvements)
  - [4. Common Pipeline Architectures](#4-common-pipeline-architectures)
    - [4.1 Linear Pipeline](#41-linear-pipeline)
    - [4.2 Branching Pipeline](#42-branching-pipeline)
    - [4.3 Iterative Pipeline](#43-iterative-pipeline)
    - [4.4 Hierarchical Pipeline](#44-hierarchical-pipeline)
    - [4.5 Map-Reduce Pipeline](#45-map-reduce-pipeline)
  - [5. Error Handling and Fallback Patterns](#5-error-handling-and-fallback-patterns)
    - [5.1 Built-in Error Recovery](#51-built-in-error-recovery)
    - [5.2 Rust Error Handling Strategy](#52-rust-error-handling-strategy)
    - [5.3 Fallback Strategies](#53-fallback-strategies)
    - [5.4 Validation and Constraints](#54-validation-and-constraints)
    - [5.5 Caching Pattern](#55-caching-pattern)
  - [6. Agent Pattern (ReAct)](#6-agent-pattern-react)
    - [6.1 ReAct Architecture](#61-react-architecture)
    - [6.2 Python DSPy ReAct](#62-python-dspy-react)
    - [6.3 Rust ReAct Implementation](#63-rust-react-implementation)
    - [6.4 Example Tools](#64-example-tools)
  - [7. Optimizer Patterns](#7-optimizer-patterns)
    - [7.1 MIPROv2 (Multi-prompt Instruction Proposal Optimizer v2)](#71-miprov2-multi-prompt-instruction-proposal-optimizer-v2)
    - [7.2 Ensemble Optimization](#72-ensemble-optimization)
  - [8. Implementation Roadmap for Rust](#8-implementation-roadmap-for-rust)
    - [Phase 1: Core Retrieval Infrastructure (Weeks 1-2)](#phase-1-core-retrieval-infrastructure-weeks-1-2)
    - [Phase 2: RAG Pipeline (Weeks 3-4)](#phase-2-rag-pipeline-weeks-3-4)
    - [Phase 3: Tool System & ReAct Agent (Weeks 5-6)](#phase-3-tool-system--react-agent-weeks-5-6)
    - [Phase 4: Advanced Composition Patterns (Weeks 7-9)](#phase-4-advanced-composition-patterns-weeks-7-9)
    - [Phase 5: STORM Pattern (Weeks 10-12)](#phase-5-storm-pattern-weeks-10-12)
    - [Phase 6: Advanced Optimizers (Weeks 13-15)](#phase-6-advanced-optimizers-weeks-13-15)
    - [Phase 7: Error Handling & Resilience (Weeks 16-17)](#phase-7-error-handling--resilience-weeks-16-17)
    - [Phase 8: Integration & Production Readiness (Weeks 18-20)](#phase-8-integration--production-readiness-weeks-18-20)
  - [9. Architecture Decisions](#9-architecture-decisions)
    - [9.1 Trait-Based Design](#91-trait-based-design)
    - [9.2 Async Throughout](#92-async-throughout)
    - [9.3 JSON for Intermediate Representations](#93-json-for-intermediate-representations)
    - [9.4 Error Handling Strategy](#94-error-handling-strategy)
    - [9.5 Optimization Strategy](#95-optimization-strategy)
  - [10. Testing Strategy](#10-testing-strategy)
    - [10.1 Unit Tests](#101-unit-tests)
    - [10.2 Integration Tests](#102-integration-tests)
    - [10.3 Benchmarks](#103-benchmarks)
    - [10.4 Example Test Structure](#104-example-test-structure)
  - [11. Performance Targets](#11-performance-targets)
    - [11.1 SLO Targets](#111-slo-targets)
    - [11.2 Optimization Targets](#112-optimization-targets)
    - [11.3 Resource Targets](#113-resource-targets)
  - [12. Dependencies](#12-dependencies)
    - [12.1 New Crate Dependencies](#121-new-crate-dependencies)
    - [12.2 Integration Points](#122-integration-points)
  - [13. Success Metrics](#13-success-metrics)
    - [13.1 Functionality](#131-functionality)
    - [13.2 Quality](#132-quality)
    - [13.3 Performance](#133-performance)
    - [13.4 Usability](#134-usability)
  - [Appendix A: Comparison with Python DSPy](#appendix-a-comparison-with-python-dspy)
  - [Appendix B: References](#appendix-b-references)
    - [Primary Sources](#primary-sources)
    - [Additional Resources](#additional-resources)
  - [Appendix C: Code Generation Receipts](#appendix-c-code-generation-receipts)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# DSPy Advanced Composition Patterns and Implementation Roadmap

## Executive Summary

This document provides a comprehensive analysis of DSPy's advanced composition patterns based on research of the Python DSPy framework (2026) and creates an implementation roadmap for Rust. DSPy abstracts LM pipelines as text transformation graphs where LMs are invoked through declarative modules that can be composed, optimized, and systematically improved.

**Current Rust Implementation Status**: Basic infrastructure complete (Signature, Module, Predictor, ChainOfThought, BootstrapFewShot optimizer)
**Target**: Production-ready advanced composition patterns (Multi-Hop QA, ReAct, ReWOO, STORM, RAG)

---

## 1. Advanced Composition Patterns

### 1.1 Multi-Hop Question Answering

**Description**: Multi-hop QA requires finding answers by connecting information from multiple sources, reasoning across different passages rather than finding answers directly in one location.

**Architecture**:
```
Question → Retrieve₁ → Extract₁ → Generate Query₂ → Retrieve₂ → Extract₂ → Answer
```

**Key Components**:
- **Query Generator**: Decomposes complex questions into sub-queries
- **Retriever**: Fetches relevant passages for each sub-query
- **Context Aggregator**: Combines information from multiple retrievals
- **Answer Generator**: Synthesizes final answer from aggregated context

**Python DSPy Pattern**:
```python
class MultiHopQA(dspy.Module):
    def __init__(self, passages_per_hop=3):
        self.generate_query = dspy.ChainOfThought("question -> search_query")
        self.retrieve = dspy.Retrieve(k=passages_per_hop)
        self.generate_answer = dspy.ChainOfThought("context, question -> answer")

    def forward(self, question):
        # First hop
        query = self.generate_query(question=question).search_query
        context = self.retrieve(query).passages

        # Second hop (simplified)
        query2 = self.generate_query(question=question, context=context).search_query
        context2 = self.retrieve(query2).passages

        # Combine and answer
        all_context = context + context2
        return self.generate_answer(context=all_context, question=question)
```

**Rust Implementation Requirements**:
- `Retrieve` module with pluggable backend (ColBERT, Qdrant, etc.)
- Query generation predictor
- Context aggregation strategy
- Iterative hop management

### 1.2 Self-Ask Pattern

**Description**: Self-Ask narrows the compositionality gap by having the model explicitly ask and answer follow-up questions before answering the main question. It breaks down complex questions into simpler sub-questions.

**Architecture**:
```
Question → Generate Sub-Question → Search/Answer → ... → Final Answer
```

**Key Characteristics**:
- Explicit follow-up question generation
- Intermediate question-answer pairs visible in reasoning
- SEARCH/LOOKUP actions to retrieve information
- Sequential decomposition

**Python DSPy Pattern**:
```python
class SelfAsk(dspy.Module):
    def __init__(self):
        self.generate_followup = dspy.Predict("question -> followup_question, needs_followup")
        self.search = dspy.Retrieve(k=3)
        self.answer = dspy.ChainOfThought("question, context -> answer")

    def forward(self, question):
        context = []
        current_q = question
        max_hops = 5

        for _ in range(max_hops):
            result = self.generate_followup(question=current_q)
            if not result.needs_followup:
                break

            # Search for answer to follow-up
            passages = self.search(result.followup_question).passages
            context.extend(passages)

            # Update current question
            current_q = result.followup_question

        return self.answer(question=question, context=context)
```

**Rust Implementation Requirements**:
- Follow-up question generator
- Boolean decision for "needs_followup"
- Loop management with max iterations
- Context accumulation

### 1.3 ReWOO Pattern (Reasoning WithOut Observation)

**Description**: ReWOO uses a planning-then-execution approach where the first LLM call generates reasoning thoughts and multiple actions, actions are executed in parallel, and a final model call generates the answer based on observations.

**Architecture**:
```
Question → Plan (generate all actions) → Execute (parallel tool calls) → Solve (synthesize answer)
```

**Key Characteristics**:
- **Planning Phase**: Single LLM call to generate all required actions
- **Execution Phase**: Parallel execution of tool calls
- **Solving Phase**: Final synthesis based on observations
- **Parallelism**: Actions don't depend on each other's results

**Python DSPy Pattern**:
```python
class ReWOO(dspy.Module):
    def __init__(self):
        self.planner = dspy.Predict("question -> plan, actions")
        self.solver = dspy.ChainOfThought("question, observations -> answer")

    def forward(self, question, tools):
        # Planning: Generate all actions at once
        plan = self.planner(question=question)

        # Execution: Run tools in parallel
        observations = []
        for action in plan.actions:
            result = tools[action.tool_name](action.arguments)
            observations.append(result)

        # Solving: Synthesize answer
        return self.solver(question=question, observations=observations)
```

**Rust Implementation Requirements**:
- Action planner that generates structured action lists
- Tool registry and invocation system
- Parallel execution (tokio::spawn)
- Observation collection and formatting

### 1.4 STORM Pattern (Synthesis of Topic Outlines through Retrieval and Multi-perspective)

**Description**: STORM is an advanced pattern for writing Wikipedia-like articles from scratch. It discovers diverse perspectives by retrieving and analyzing similar topics, then personifies the LLM with specific perspectives for question asking.

**Architecture**:
```
Topic → Discover Perspectives → Generate Questions per Perspective →
Retrieve & Analyze → Outline Generation → Section Writing → Article Assembly
```

**Key Characteristics**:
- Multi-stage approach with perspective discovery
- Personified LLM calls (role-playing different perspectives)
- Zero-shot prompting using DSPy framework
- Outline-driven generation
- Iterative refinement

**Python DSPy Pattern** (simplified):
```python
class STORM(dspy.Module):
    def __init__(self):
        self.perspective_generator = dspy.Predict("topic -> perspectives")
        self.question_generator = dspy.Predict("topic, perspective -> questions")
        self.retrieve = dspy.Retrieve(k=5)
        self.outline_generator = dspy.ChainOfThought("topic, research -> outline")
        self.section_writer = dspy.ChainOfThought("section_title, research -> content")

    def forward(self, topic):
        # Discover perspectives
        perspectives = self.perspective_generator(topic=topic).perspectives

        # Generate questions from each perspective
        all_research = []
        for perspective in perspectives:
            questions = self.question_generator(topic=topic, perspective=perspective)
            for q in questions:
                research = self.retrieve(q).passages
                all_research.extend(research)

        # Generate outline
        outline = self.outline_generator(topic=topic, research=all_research)

        # Write sections
        sections = []
        for section in outline.sections:
            content = self.section_writer(section_title=section, research=all_research)
            sections.append(content)

        return {"outline": outline, "sections": sections}
```

**Rust Implementation Requirements**:
- Perspective generation
- Multi-perspective question generation
- Large-scale retrieval and aggregation
- Outline structuring
- Section-by-section generation
- Article assembly

---

## 2. How Patterns Compose Basic Modules

### 2.1 Module Composition Principles

DSPy modules compose through:

1. **Sequential Composition**: Output of one module becomes input to next
2. **Parallel Composition**: Multiple modules process same input
3. **Conditional Composition**: Control flow based on module outputs
4. **Iterative Composition**: Loops with modules in the body

**Current Rust Implementation**:
```rust
#[async_trait::async_trait]
pub trait Module: Send + Sync {
    fn signature(&self) -> &Signature;
    async fn forward(&self, inputs: HashMap<String, Value>) -> Result<HashMap<String, Value>, ModuleError>;
}
```

**Composition Example** (Rust):
```rust
pub struct ComposedModule {
    module1: Arc<dyn Module>,
    module2: Arc<dyn Module>,
}

impl ComposedModule {
    async fn forward(&self, inputs: HashMap<String, Value>) -> ModuleResult<HashMap<String, Value>> {
        // Sequential composition
        let intermediate = self.module1.forward(inputs).await?;
        self.module2.forward(intermediate).await
    }
}
```

### 2.2 Basic Module Types

**From Research**:
- `dspy.Predict`: Basic prediction with signature
- `dspy.ChainOfThought`: Adds step-by-step reasoning
- `dspy.Retrieve(k)`: Searches for top-k passages
- `dspy.ReAct`: Agent that can use tools

**Current Rust Status**:
- ✅ `Predictor`: Implemented
- ✅ `ChainOfThought`: Implemented
- ❌ `Retrieve`: Not implemented
- ❌ `ReAct`: Not implemented

---

## 3. Retrieve Module and RAG Integration

### 3.1 Retrieve Module Architecture

**Core Functionality**:
```rust
pub struct Retrieve {
    backend: Arc<dyn RetrieverBackend>,
    k: usize,
    signature: Signature,
}

#[async_trait::async_trait]
pub trait RetrieverBackend: Send + Sync {
    async fn search(&self, query: &str, k: usize) -> Result<Vec<Passage>, ModuleError>;
}

pub struct Passage {
    pub text: String,
    pub score: f32,
    pub metadata: HashMap<String, Value>,
}
```

### 3.2 Supported Backends

1. **ColBERTv2**: Dense retrieval with Wikipedia indexes
2. **Qdrant**: Vector search integration
3. **Clarifai**: LLM and vector search
4. **Custom**: User-defined retrieval

**Implementation Pattern**:
```rust
pub struct QdrantRetriever {
    client: QdrantClient,
    collection_name: String,
}

#[async_trait::async_trait]
impl RetrieverBackend for QdrantRetriever {
    async fn search(&self, query: &str, k: usize) -> Result<Vec<Passage>, ModuleError> {
        // Embed query
        let embedding = self.embed(query).await?;

        // Search Qdrant
        let results = self.client
            .search_points(SearchPoints {
                collection_name: self.collection_name.clone(),
                vector: embedding,
                limit: k as u64,
                ..Default::default()
            })
            .await?;

        // Convert to Passages
        Ok(results.into_iter().map(|r| Passage {
            text: r.payload["text"].as_str().unwrap().to_string(),
            score: r.score,
            metadata: r.payload,
        }).collect())
    }
}
```

### 3.3 RAG Pipeline Architecture

**Basic RAG**:
```rust
pub struct RAG {
    retrieve: Retrieve,
    generate_answer: ChainOfThought,
    signature: Signature,
}

impl RAG {
    pub fn new(k: usize) -> Self {
        let retrieve = Retrieve::new(k);
        let answer_sig = Signature::new("QA", "Answer questions")
            .with_input(InputField::new("question", "Question", "String"))
            .with_input(InputField::new("context", "Context passages", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"));

        Self {
            retrieve,
            generate_answer: ChainOfThought::new(answer_sig),
            signature: Signature::new("RAG", "Retrieval-augmented generation"),
        }
    }
}

#[async_trait::async_trait]
impl Module for RAG {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(&self, inputs: HashMap<String, Value>) -> ModuleResult<HashMap<String, Value>> {
        // Extract question
        let question = inputs.get("question")
            .and_then(|v| v.as_str())
            .ok_or(ModuleError::MissingInput("question".into()))?;

        // Retrieve passages
        let passages = self.retrieve.forward(inputs.clone()).await?;
        let context = passages.get("passages")
            .and_then(|v| v.as_str())
            .unwrap_or("");

        // Generate answer with context
        let mut answer_inputs = inputs;
        answer_inputs.insert("context".into(), Value::String(context.to_string()));

        self.generate_answer.forward(answer_inputs).await
    }
}
```

**Multi-Stage RAG** (Improved):
- Query rewriting
- Multiple retrieval rounds
- Re-ranking
- Answer generation with citations

### 3.4 Performance Improvements

**From Research**: Optimization can improve Semantic F1 from 57% to 77%

**Optimization Strategies**:
1. **Query Optimization**: Learn better retrieval queries
2. **Passage Selection**: Optimize k value per query type
3. **Context Formatting**: Learn best way to present passages
4. **Answer Generation**: Optimize instruction templates

---

## 4. Common Pipeline Architectures

### 4.1 Linear Pipeline

```
Input → Module₁ → Module₂ → ... → ModuleN → Output
```

**Use Cases**: Simple transformations, sequential processing

### 4.2 Branching Pipeline

```
         ┌─→ ModuleA → OutputA
Input ───┼─→ ModuleB → OutputB
         └─→ ModuleC → OutputC
```

**Use Cases**: Multiple perspectives, ensemble methods

### 4.3 Iterative Pipeline

```
Input → Module → Check → [if not done: back to Module] → Output
```

**Use Cases**: Self-Ask, iterative refinement, search loops

### 4.4 Hierarchical Pipeline

```
Input → [Agent → Tool₁, Tool₂, Tool₃] → Synthesizer → Output
```

**Use Cases**: ReAct, ReWOO, complex reasoning

### 4.5 Map-Reduce Pipeline

```
Input → Split → [Module₁ || Module₂ || Module₃] → Aggregate → Output
```

**Use Cases**: STORM, distributed processing, perspective generation

---

## 5. Error Handling and Fallback Patterns

### 5.1 Built-in Error Recovery

**From Research**: `dspy.ReAct` includes built-in error recovery for failed tool calls

**Pattern**:
```python
class ReActWithRetry(dspy.Module):
    def __init__(self, max_retries=3):
        self.agent = dspy.ReAct()
        self.max_retries = max_retries

    def forward(self, question, tools):
        for attempt in range(self.max_retries):
            try:
                return self.agent(question=question, tools=tools)
            except ToolError as e:
                if attempt == self.max_retries - 1:
                    raise
                # Log and retry
                continue
```

### 5.2 Rust Error Handling Strategy

**Current Implementation**:
```rust
#[derive(Debug, thiserror::Error)]
pub enum ModuleError {
    #[error("Missing required input: {0}")]
    MissingInput(String),

    #[error("Invalid input type for field '{0}': expected {1}")]
    InvalidInputType(String, String),

    #[error("LLM error: {0}")]
    LlmError(String),

    #[error("Module error: {0}")]
    Other(String),
}
```

**Enhanced Error Handling**:
```rust
pub enum ModuleError {
    // ... existing variants ...

    #[error("Tool execution failed: {tool}, reason: {reason}")]
    ToolError { tool: String, reason: String },

    #[error("Retrieval failed: {0}")]
    RetrievalError(String),

    #[error("Validation failed: {0}")]
    ValidationError(String),

    #[error("Timeout after {0}ms")]
    Timeout(u64),
}
```

### 5.3 Fallback Strategies

**1. Model Fallback**:
```rust
pub struct ModelFallbackPredictor {
    primary: Predictor,
    fallback: Predictor,
}

impl ModelFallbackPredictor {
    async fn forward(&self, inputs: HashMap<String, Value>) -> ModuleResult<HashMap<String, Value>> {
        match self.primary.forward(inputs.clone()).await {
            Ok(result) => Ok(result),
            Err(e) => {
                warn!("Primary model failed: {}, trying fallback", e);
                self.fallback.forward(inputs).await
            }
        }
    }
}
```

**2. Timeout Handling**:
```rust
use tokio::time::{timeout, Duration};

async fn forward_with_timeout(
    module: &dyn Module,
    inputs: HashMap<String, Value>,
    timeout_ms: u64,
) -> ModuleResult<HashMap<String, Value>> {
    match timeout(Duration::from_millis(timeout_ms), module.forward(inputs)).await {
        Ok(result) => result,
        Err(_) => Err(ModuleError::Timeout(timeout_ms)),
    }
}
```

**3. Retry with Exponential Backoff**:
```rust
pub struct RetryConfig {
    max_attempts: usize,
    initial_delay_ms: u64,
    max_delay_ms: u64,
    backoff_multiplier: f32,
}

async fn forward_with_retry(
    module: &dyn Module,
    inputs: HashMap<String, Value>,
    config: &RetryConfig,
) -> ModuleResult<HashMap<String, Value>> {
    let mut delay = config.initial_delay_ms;

    for attempt in 0..config.max_attempts {
        match module.forward(inputs.clone()).await {
            Ok(result) => return Ok(result),
            Err(e) if attempt < config.max_attempts - 1 => {
                warn!("Attempt {} failed: {}, retrying in {}ms", attempt + 1, e, delay);
                tokio::time::sleep(Duration::from_millis(delay)).await;
                delay = ((delay as f32 * config.backoff_multiplier) as u64).min(config.max_delay_ms);
            }
            Err(e) => return Err(e),
        }
    }

    unreachable!()
}
```

### 5.4 Validation and Constraints

**From Research**: Built-in patterns for validation, caching, and monitoring

**Rust Pattern**:
```rust
pub struct ValidatedModule {
    inner: Arc<dyn Module>,
    validators: Vec<Arc<dyn Validator>>,
}

#[async_trait::async_trait]
pub trait Validator: Send + Sync {
    async fn validate(&self, inputs: &HashMap<String, Value>, outputs: &HashMap<String, Value>)
        -> Result<(), ValidationError>;
}

impl ValidatedModule {
    async fn forward(&self, inputs: HashMap<String, Value>) -> ModuleResult<HashMap<String, Value>> {
        let outputs = self.inner.forward(inputs.clone()).await?;

        for validator in &self.validators {
            validator.validate(&inputs, &outputs).await
                .map_err(|e| ModuleError::ValidationError(e.to_string()))?;
        }

        Ok(outputs)
    }
}
```

### 5.5 Caching Pattern

**From Research**: Self-cleaning 15-minute cache for faster responses, MD5 hashing for cache keys

```rust
use std::collections::HashMap;
use std::time::{Duration, Instant};
use md5::{Md5, Digest};

pub struct CachedModule {
    inner: Arc<dyn Module>,
    cache: Arc<tokio::sync::RwLock<HashMap<String, CacheEntry>>>,
    ttl: Duration,
}

struct CacheEntry {
    outputs: HashMap<String, Value>,
    timestamp: Instant,
}

impl CachedModule {
    fn cache_key(&self, inputs: &HashMap<String, Value>) -> String {
        let serialized = serde_json::to_string(inputs).unwrap();
        let mut hasher = Md5::new();
        hasher.update(serialized.as_bytes());
        format!("{:x}", hasher.finalize())
    }

    async fn forward(&self, inputs: HashMap<String, Value>) -> ModuleResult<HashMap<String, Value>> {
        let key = self.cache_key(&inputs);

        // Check cache
        {
            let cache = self.cache.read().await;
            if let Some(entry) = cache.get(&key) {
                if entry.timestamp.elapsed() < self.ttl {
                    debug!("Cache hit for key: {}", key);
                    return Ok(entry.outputs.clone());
                }
            }
        }

        // Execute module
        let outputs = self.inner.forward(inputs).await?;

        // Update cache
        {
            let mut cache = self.cache.write().await;
            cache.insert(key, CacheEntry {
                outputs: outputs.clone(),
                timestamp: Instant::now(),
            });
        }

        Ok(outputs)
    }
}
```

---

## 6. Agent Pattern (ReAct)

### 6.1 ReAct Architecture

**Description**: ReAct (Reasoning + Acting) combines reasoning traces with task-specific actions, allowing the model to reason about what to do and then execute tools.

**Pattern**:
```
Question → Thought → Action → Observation → ... → Final Answer
```

**Key Components**:
- **Thought Generator**: Produces reasoning steps
- **Action Selector**: Chooses which tool to use
- **Tool Executor**: Executes the selected tool
- **Observation Processor**: Formats tool results
- **Convergence Checker**: Determines when to stop

### 6.2 Python DSPy ReAct

```python
class ReAct(dspy.Module):
    def __init__(self, signature, tools, max_iters=5):
        self.signature = signature
        self.tools = {tool.name: tool for tool in tools}
        self.max_iters = max_iters
        self.react_prompt = dspy.ChainOfThought(signature)

    def forward(self, **kwargs):
        trajectory = []

        for i in range(self.max_iters):
            # Generate thought and action
            thought = self.react_prompt(**kwargs, trajectory=trajectory)

            if thought.action == "Finish":
                return thought.answer

            # Execute tool
            tool = self.tools.get(thought.action)
            if tool:
                observation = tool(thought.action_input)
            else:
                observation = f"Error: Tool {thought.action} not found"

            # Add to trajectory
            trajectory.append({
                "thought": thought.reasoning,
                "action": thought.action,
                "observation": observation
            })

        return "Max iterations reached"
```

### 6.3 Rust ReAct Implementation

```rust
pub struct ReAct {
    signature: Signature,
    tools: HashMap<String, Arc<dyn Tool>>,
    max_iters: usize,
    thought_generator: ChainOfThought,
}

#[async_trait::async_trait]
pub trait Tool: Send + Sync {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    async fn execute(&self, input: &str) -> Result<String, ToolError>;
}

pub struct Trajectory {
    pub thought: String,
    pub action: String,
    pub action_input: String,
    pub observation: String,
}

impl ReAct {
    pub fn new(tools: Vec<Arc<dyn Tool>>) -> Self {
        let tool_map: HashMap<_, _> = tools.into_iter()
            .map(|t| (t.name().to_string(), t))
            .collect();

        let sig = Signature::new("ReAct", "Reasoning and Acting agent")
            .with_input(InputField::new("question", "Question", "String"))
            .with_input(InputField::new("trajectory", "Previous steps", "String"))
            .with_output(OutputField::new("thought", "Reasoning", "String"))
            .with_output(OutputField::new("action", "Action to take", "String"))
            .with_output(OutputField::new("action_input", "Action input", "String"))
            .with_instructions(
                "You are a helpful assistant that reasons about tasks and takes actions.\n\
                 Available actions: [tool list]\n\
                 Use 'Finish' action when you have the final answer."
            );

        Self {
            signature: sig.clone(),
            tools: tool_map,
            max_iters: 5,
            thought_generator: ChainOfThought::new(sig),
        }
    }
}

#[async_trait::async_trait]
impl Module for ReAct {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(&self, inputs: HashMap<String, Value>) -> ModuleResult<HashMap<String, Value>> {
        let question = inputs.get("question")
            .and_then(|v| v.as_str())
            .ok_or(ModuleError::MissingInput("question".into()))?;

        let mut trajectory = Vec::new();

        for iter in 0..self.max_iters {
            debug!("ReAct iteration {}/{}", iter + 1, self.max_iters);

            // Build trajectory context
            let trajectory_str = trajectory.iter()
                .map(|t: &Trajectory| {
                    format!("Thought: {}\nAction: {}\nObservation: {}\n",
                            t.thought, t.action, t.observation)
                })
                .collect::<Vec<_>>()
                .join("\n");

            // Generate thought and action
            let mut thought_inputs = inputs.clone();
            thought_inputs.insert("trajectory".into(), Value::String(trajectory_str));

            let thought_output = self.thought_generator.forward(thought_inputs).await?;

            let action = thought_output.get("action")
                .and_then(|v| v.as_str())
                .ok_or(ModuleError::Other("No action in output".into()))?;

            // Check for finish
            if action == "Finish" {
                let answer = thought_output.get("thought")
                    .cloned()
                    .unwrap_or(Value::String("Done".into()));

                let mut result = HashMap::new();
                result.insert("answer".into(), answer);
                return Ok(result);
            }

            // Execute tool
            let observation = if let Some(tool) = self.tools.get(action) {
                let action_input = thought_output.get("action_input")
                    .and_then(|v| v.as_str())
                    .unwrap_or("");

                match tool.execute(action_input).await {
                    Ok(obs) => obs,
                    Err(e) => {
                        warn!("Tool {} failed: {}", action, e);
                        format!("Error executing {}: {}", action, e)
                    }
                }
            } else {
                format!("Error: Tool '{}' not found", action)
            };

            // Add to trajectory
            trajectory.push(Trajectory {
                thought: thought_output.get("thought")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string(),
                action: action.to_string(),
                action_input: thought_output.get("action_input")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string(),
                observation,
            });
        }

        Err(ModuleError::Other(format!("Max iterations ({}) reached", self.max_iters)))
    }
}
```

### 6.4 Example Tools

```rust
pub struct SearchTool {
    retriever: Arc<dyn RetrieverBackend>,
}

#[async_trait::async_trait]
impl Tool for SearchTool {
    fn name(&self) -> &str {
        "Search"
    }

    fn description(&self) -> &str {
        "Search for information about a query"
    }

    async fn execute(&self, input: &str) -> Result<String, ToolError> {
        let passages = self.retriever.search(input, 3).await?;
        Ok(passages.into_iter()
            .map(|p| p.text)
            .collect::<Vec<_>>()
            .join("\n\n"))
    }
}

pub struct CalculatorTool;

#[async_trait::async_trait]
impl Tool for CalculatorTool {
    fn name(&self) -> &str {
        "Calculator"
    }

    fn description(&self) -> &str {
        "Evaluate mathematical expressions"
    }

    async fn execute(&self, input: &str) -> Result<String, ToolError> {
        // Use evalexpr or similar
        let result = evalexpr::eval(input)
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        Ok(result.to_string())
    }
}
```

---

## 7. Optimizer Patterns

### 7.1 MIPROv2 (Multi-prompt Instruction Proposal Optimizer v2)

**Description**: Advanced optimizer that jointly optimizes instructions and few-shot examples using Bayesian Optimization.

**Algorithm**:
1. Bootstrap few-shot example candidates
2. Propose instructions grounded in task dynamics
3. Use Bayesian Optimization to find optimal combination
4. Includes minibatching for efficiency

**Key Features**:
- Optimizes both instructions AND examples
- Bayesian search over program space
- Requires 40+ trials for best results
- Needs 200+ examples to prevent overfitting

**Rust Implementation Target**:
```rust
pub struct MIPROv2 {
    metric: MetricFn,
    num_candidates: usize,
    num_trials: usize,
    minibatch_size: usize,
}

impl MIPROv2 {
    pub async fn compile(
        &self,
        student: &dyn Module,
        trainset: &[Example],
        valset: &[Example],
    ) -> Result<OptimizedPredictor, ModuleError> {
        // 1. Generate instruction candidates
        let instruction_candidates = self.propose_instructions(trainset).await?;

        // 2. Bootstrap few-shot candidates
        let demo_candidates = self.bootstrap_demos(student, trainset).await?;

        // 3. Bayesian optimization over combinations
        let best_config = self.bayesian_search(
            &instruction_candidates,
            &demo_candidates,
            student,
            valset,
        ).await?;

        // 4. Create optimized predictor
        Ok(best_config)
    }
}
```

### 7.2 Ensemble Optimization

**From Research**: Run optimizer, extract top-5 programs, build ensemble

```rust
pub struct Ensemble {
    modules: Vec<Arc<dyn Module>>,
    aggregation: AggregationStrategy,
}

pub enum AggregationStrategy {
    Majority,
    WeightedVote { weights: Vec<f32> },
    BestScore,
}

impl Ensemble {
    pub async fn forward(&self, inputs: HashMap<String, Value>) -> ModuleResult<HashMap<String, Value>> {
        // Run all modules in parallel
        let futures: Vec<_> = self.modules.iter()
            .map(|m| m.forward(inputs.clone()))
            .collect();

        let results = futures::future::join_all(futures).await;

        // Aggregate results
        self.aggregate(results)
    }
}
```

---

## 8. Implementation Roadmap for Rust

### Phase 1: Core Retrieval Infrastructure (Weeks 1-2)

**Priority**: High
**Dependencies**: None

**Deliverables**:
1. `Retrieve` module with trait-based backend
2. `RetrieverBackend` trait
3. In-memory retriever (for testing)
4. Passage struct with metadata
5. Tests with mock retrieval

**Files to Create**:
- `/home/user/ggen/crates/ggen-ai/src/dspy/retrieve.rs`
- `/home/user/ggen/crates/ggen-ai/src/dspy/retriever_backend.rs`
- `/home/user/ggen/crates/ggen-ai/tests/dspy_retrieve_tests.rs`

**Success Criteria**:
- [ ] `cargo make test` passes
- [ ] Basic retrieval with in-memory backend works
- [ ] Integration with existing Module trait
- [ ] Documentation complete

### Phase 2: RAG Pipeline (Weeks 3-4)

**Priority**: High
**Dependencies**: Phase 1

**Deliverables**:
1. Basic RAG module
2. Multi-stage RAG variant
3. Query rewriting module
4. Context aggregation strategies
5. Example with Wikipedia retrieval

**Files to Create**:
- `/home/user/ggen/crates/ggen-ai/src/dspy/rag.rs`
- `/home/user/ggen/crates/ggen-ai/examples/dspy_rag_example.rs`
- `/home/user/ggen/crates/ggen-ai/tests/dspy_rag_tests.rs`

**Success Criteria**:
- [ ] Basic RAG pipeline working
- [ ] Integration test with real LLM
- [ ] Performance benchmarks
- [ ] Documentation and examples

### Phase 3: Tool System & ReAct Agent (Weeks 5-6)

**Priority**: High
**Dependencies**: Phase 2

**Deliverables**:
1. `Tool` trait and registry
2. Built-in tools (Search, Calculator, etc.)
3. ReAct agent implementation
4. Trajectory tracking
5. Error recovery for tool failures

**Files to Create**:
- `/home/user/ggen/crates/ggen-ai/src/dspy/tool.rs`
- `/home/user/ggen/crates/ggen-ai/src/dspy/react.rs`
- `/home/user/ggen/crates/ggen-ai/src/dspy/tools/` (directory)
- `/home/user/ggen/crates/ggen-ai/examples/dspy_react_example.rs`

**Success Criteria**:
- [ ] ReAct agent working with 3+ tools
- [ ] Proper error recovery
- [ ] Convergence detection
- [ ] Integration tests

### Phase 4: Advanced Composition Patterns (Weeks 7-9)

**Priority**: Medium
**Dependencies**: Phase 3

**Deliverables**:
1. Multi-Hop QA implementation
2. Self-Ask implementation
3. ReWOO implementation
4. Composition utilities (sequential, parallel, conditional)
5. Pipeline builder API

**Files to Create**:
- `/home/user/ggen/crates/ggen-ai/src/dspy/patterns/` (directory)
- `/home/user/ggen/crates/ggen-ai/src/dspy/patterns/multihop.rs`
- `/home/user/ggen/crates/ggen-ai/src/dspy/patterns/self_ask.rs`
- `/home/user/ggen/crates/ggen-ai/src/dspy/patterns/rewoo.rs`
- `/home/user/ggen/crates/ggen-ai/src/dspy/pipeline.rs`

**Success Criteria**:
- [ ] All three patterns implemented
- [ ] Pipeline builder with fluent API
- [ ] End-to-end examples
- [ ] Performance benchmarks

### Phase 5: STORM Pattern (Weeks 10-12)

**Priority**: Medium
**Dependencies**: Phase 4

**Deliverables**:
1. Perspective generator
2. Multi-perspective question generation
3. Outline generator
4. Section writer
5. Article assembler
6. Complete STORM pipeline

**Files to Create**:
- `/home/user/ggen/crates/ggen-ai/src/dspy/patterns/storm.rs`
- `/home/user/ggen/crates/ggen-ai/examples/dspy_storm_example.rs`

**Success Criteria**:
- [ ] Generate Wikipedia-style articles
- [ ] Multi-perspective analysis working
- [ ] Outline-driven generation
- [ ] Performance optimization

### Phase 6: Advanced Optimizers (Weeks 13-15)

**Priority**: Medium
**Dependencies**: Phase 2

**Deliverables**:
1. MIPROv2 optimizer
2. Bayesian optimization infrastructure
3. Instruction proposal system
4. Ensemble optimizer
5. Optimizer composition

**Files to Create**:
- `/home/user/ggen/crates/ggen-ai/src/dspy/optimizers/` (directory)
- `/home/user/ggen/crates/ggen-ai/src/dspy/optimizers/mipro.rs`
- `/home/user/ggen/crates/ggen-ai/src/dspy/optimizers/ensemble.rs`
- `/home/user/ggen/crates/ggen-ai/src/dspy/optimizers/bayesian.rs`

**Success Criteria**:
- [ ] MIPROv2 working on example tasks
- [ ] Performance improvement demonstrated
- [ ] Ensemble methods working
- [ ] Benchmarks vs BootstrapFewShot

### Phase 7: Error Handling & Resilience (Weeks 16-17)

**Priority**: High
**Dependencies**: All previous phases

**Deliverables**:
1. Enhanced error types
2. Retry with backoff
3. Timeout handling
4. Model fallback
5. Validation framework
6. Caching layer

**Files to Update/Create**:
- `/home/user/ggen/crates/ggen-ai/src/dspy/module.rs` (update)
- `/home/user/ggen/crates/ggen-ai/src/dspy/resilience.rs`
- `/home/user/ggen/crates/ggen-ai/src/dspy/cache.rs`
- `/home/user/ggen/crates/ggen-ai/src/dspy/validation.rs`

**Success Criteria**:
- [ ] All patterns have error recovery
- [ ] Retry logic tested
- [ ] Caching provides 50%+ latency reduction
- [ ] Validation prevents bad outputs

### Phase 8: Integration & Production Readiness (Weeks 18-20)

**Priority**: High
**Dependencies**: All previous phases

**Deliverables**:
1. Complete documentation
2. End-to-end tutorials
3. Production deployment guide
4. Performance optimization
5. Monitoring and observability
6. Property-based tests
7. Benchmarks suite

**Files to Create**:
- `/home/user/ggen/docs/dspy_production_guide.md`
- `/home/user/ggen/docs/dspy_tutorial.md`
- `/home/user/ggen/crates/ggen-ai/benches/dspy_advanced_benchmarks.rs`
- `/home/user/ggen/crates/ggen-ai/tests/dspy_integration_tests.rs`

**Success Criteria**:
- [ ] All cargo make targets pass
- [ ] Documentation complete
- [ ] Performance meets SLOs
- [ ] Production deployment successful

---

## 9. Architecture Decisions

### 9.1 Trait-Based Design

**Decision**: Use traits for Module, Tool, RetrieverBackend, Optimizer
**Rationale**: Maximum composability, testability, extensibility
**Trade-offs**: More complex types, requires Arc<dyn Trait>

### 9.2 Async Throughout

**Decision**: All forward() methods are async
**Rationale**: LLM calls are I/O bound, need parallelism
**Trade-offs**: Requires tokio runtime, async complexity

### 9.3 JSON for Intermediate Representations

**Decision**: Use `HashMap<String, Value>` for module I/O
**Rationale**: Flexibility, easy debugging, JSON Schema support
**Trade-offs**: Runtime type checking, less type safety

### 9.4 Error Handling Strategy

**Decision**: `Result<T, ModuleError>` throughout, no unwrap in production
**Rationale**: Explicit error handling, composable errors
**Trade-offs**: More verbose code, requires map_err

### 9.5 Optimization Strategy

**Decision**: Separate optimizer implementations, composable
**Rationale**: Follows DSPy design, allows optimizer composition
**Trade-offs**: More complex API, learning curve

---

## 10. Testing Strategy

### 10.1 Unit Tests

- Each module independently testable
- Mock LLM responses for deterministic tests
- Property-based tests for invariants

### 10.2 Integration Tests

- End-to-end pipelines with real LLMs
- RAG with real retrieval backend
- Agent loops with real tools

### 10.3 Benchmarks

- Latency measurements
- Throughput tests
- Optimization improvement metrics
- Cache hit rates

### 10.4 Example Test Structure

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::*;

    #[tokio::test]
    async fn test_multihop_qa_pipeline() {
        // Arrange: Create test data
        let retriever = MockRetriever::new();
        let multihop = MultiHopQA::new(retriever);

        let mut inputs = HashMap::new();
        inputs.insert("question".into(), Value::String("Complex question".into()));

        // Act: Run the pipeline
        let result = multihop.forward(inputs).await;

        // Assert: Check results
        assert!(result.is_ok());
        let answer = result.unwrap();
        assert!(answer.contains_key("answer"));
    }
}
```

---

## 11. Performance Targets

### 11.1 SLO Targets

- **Retrieve Module**: < 100ms per query (local), < 500ms (remote)
- **RAG Pipeline**: < 2s end-to-end
- **ReAct Agent**: < 5s per iteration, < 30s total
- **Multi-Hop QA**: < 10s for 2-3 hops
- **STORM**: < 5min for full article generation

### 11.2 Optimization Targets

- **BootstrapFewShot**: 10-20% improvement over baseline
- **MIPROv2**: 30-50% improvement over baseline
- **Ensemble**: 15-25% improvement over single model

### 11.3 Resource Targets

- **Memory**: < 1GB per pipeline
- **CPU**: Efficient async, no blocking
- **Cache Hit Rate**: > 60% for repeated queries

---

## 12. Dependencies

### 12.1 New Crate Dependencies

```toml
[dependencies]
# Existing
genai = "0.x"
tokio = { version = "1.47", features = ["full"] }
async-trait = "0.1"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
thiserror = "2.0"
tracing = "0.1"

# New for retrieval
qdrant-client = "1.0"  # Vector database
tantivy = "0.21"       # Full-text search (alternative)

# New for optimization
argmin = "0.10"        # Bayesian optimization
ndarray = "0.16"       # Numerical arrays

# New for tools
evalexpr = "11.0"      # Expression evaluation
reqwest = "0.12"       # HTTP client for tools

# New for caching
md5 = "0.7"
```

### 12.2 Integration Points

- `ggen-ai` module system
- `ggen-core` for RDF integration
- Existing agent framework for EPIC 9 integration

---

## 13. Success Metrics

### 13.1 Functionality

- [ ] All 5 advanced patterns implemented
- [ ] ReAct agent working with tools
- [ ] RAG pipeline production-ready
- [ ] 2+ optimizers available
- [ ] Complete error handling

### 13.2 Quality

- [ ] Test coverage > 80%
- [ ] All clippy warnings resolved
- [ ] Documentation coverage 100%
- [ ] Zero unwrap/expect in production

### 13.3 Performance

- [ ] All SLO targets met
- [ ] Optimization shows measurable improvement
- [ ] Cache provides latency reduction

### 13.4 Usability

- [ ] Complete tutorials
- [ ] 10+ working examples
- [ ] API documentation
- [ ] Migration guide from Python DSPy

---

## Appendix A: Comparison with Python DSPy

| Feature | Python DSPy | Rust Implementation | Status |
|---------|-------------|---------------------|--------|
| Signature | ✅ | ✅ | Complete |
| Predictor | ✅ | ✅ | Complete |
| ChainOfThought | ✅ | ✅ | Complete |
| Retrieve | ✅ | ❌ | Phase 1 |
| ReAct | ✅ | ❌ | Phase 3 |
| Multi-Hop QA | ✅ | ❌ | Phase 4 |
| Self-Ask | ✅ | ❌ | Phase 4 |
| ReWOO | ✅ | ❌ | Phase 4 |
| STORM | ✅ | ❌ | Phase 5 |
| BootstrapFewShot | ✅ | ✅ | Complete |
| MIPROv2 | ✅ | ❌ | Phase 6 |
| Ensemble | ✅ | ❌ | Phase 6 |
| Validation | ✅ | ❌ | Phase 7 |
| Caching | ✅ | ❌ | Phase 7 |

---

## Appendix B: References

### Primary Sources

- [DSPy Official Documentation](https://dspy.ai/)
- [DSPy GitHub Repository](https://github.com/stanfordnlp/dspy)
- [DSPy Paper (arXiv:2310.03714)](https://arxiv.org/abs/2310.03714)
- [DSPy RAG Tutorial](https://dspy.ai/tutorials/rag/)
- [Building AI Agents with DSPy](https://dspy.ai/tutorials/customer_service_agent/)
- [Multi-Hop QA with DSPy and Qdrant](https://rito.hashnode.dev/building-a-multi-hop-qa-with-dspy-and-qdrant)
- [Agentic AI Design Patterns: ReAct, ReWOO, CodeAct](https://capabl.in/blog/agentic-ai-design-patterns-react-rewoo-codeact-and-beyond)
- [STORM Paper (arXiv:2402.14207)](https://arxiv.org/pdf/2402.14207)
- [MIPROv2 Documentation](https://dspy.ai/api/optimizers/MIPROv2/)
- [DSPy Optimizers Guide](https://weaviate.io/blog/dspy-optimizers)
- [Best AI Agent Frameworks 2025](https://langwatch.ai/blog/best-ai-agent-frameworks-in-2025-comparing-langgraph-dspy-crewai-agno-and-more)

### Additional Resources

- [DSPy Framework Technical Guide](https://dzone.com/articles/dspy-framework-technical-guide)
- [DSPy Cheatsheet](https://dspy.ai/cheatsheet/)
- [Stanford OVAL STORM Demo](https://www.digitalocean.com/community/tutorials/stanford-oval-storm-mistral-demo)
- [Retrieval-Reasoning Processes for Multi-hop QA](https://arxiv.org/html/2601.00536)

---

## Appendix C: Code Generation Receipts

This document will be implemented following the Big Bang 80/20 paradigm:

1. **Specification Closure**: Define TTL specifications for all modules
2. **Parallel Execution**: Use EPIC 9 for non-trivial components
3. **Deterministic Receipts**: All implementations verified with `cargo make test`

**Target Completion**: 20 weeks
**Test Coverage Target**: > 80%
**Performance Target**: All SLOs met
**Documentation Target**: 100% coverage

---

*Generated: 2026-01-11*
*Version: 1.0*
*Author: Research and Analysis of DSPy 2026 Implementation*
