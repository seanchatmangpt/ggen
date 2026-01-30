//! Contract Composition Engine
//!
//! Compose contracts into type-safe pipelines:
//! - Sequential: A -> B -> C
//! - Parallel: A | B | C (fan-out)
//! - Conditional: if A then B else C
//! - Loop: while A do B
//!
//! Pipeline envelope = intersection of component envelopes

use crate::engine::{ContractDefinition, ContractEngine};
use crate::executor::ExecutionResult;
use mcp_core::crypto::hash_sha256;
use mcp_core::error::{McpError, McpResult};
use mcp_core::types::{Capability, Envelope};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::time::Instant;

/// Pipeline node representing a step in the composition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PipelineNode {
    /// Single contract execution
    Contract(String),
    /// Sequential execution: run nodes in order, passing output to next input
    Sequence(Vec<PipelineNode>),
    /// Parallel execution: fan-out to all branches, collect results
    Parallel(Vec<PipelineNode>),
    /// Conditional branch: execute condition contract, branch based on result
    Conditional {
        /// Contract that returns a boolean-like result
        condition: String,
        /// Branch to execute if condition is truthy
        then_branch: Box<PipelineNode>,
        /// Optional branch to execute if condition is falsy
        else_branch: Option<Box<PipelineNode>>,
    },
    /// Loop: execute body while condition is true
    Loop {
        /// Contract that returns a boolean-like result
        condition: String,
        /// Body to execute each iteration
        body: Box<PipelineNode>,
        /// Maximum iterations to prevent infinite loops
        max_iterations: u32,
    },
    /// Transform: apply a transformation contract to the output
    Transform {
        /// Input pipeline node
        input: Box<PipelineNode>,
        /// Transform contract to apply
        transform: String,
    },
}

impl PipelineNode {
    /// Collect all contract IDs referenced in this node
    fn collect_contracts(&self, contracts: &mut Vec<String>) {
        match self {
            PipelineNode::Contract(id) => {
                if !contracts.contains(id) {
                    contracts.push(id.clone());
                }
            }
            PipelineNode::Sequence(nodes) | PipelineNode::Parallel(nodes) => {
                for node in nodes {
                    node.collect_contracts(contracts);
                }
            }
            PipelineNode::Conditional {
                condition,
                then_branch,
                else_branch,
            } => {
                if !contracts.contains(condition) {
                    contracts.push(condition.clone());
                }
                then_branch.collect_contracts(contracts);
                if let Some(else_node) = else_branch {
                    else_node.collect_contracts(contracts);
                }
            }
            PipelineNode::Loop {
                condition, body, ..
            } => {
                if !contracts.contains(condition) {
                    contracts.push(condition.clone());
                }
                body.collect_contracts(contracts);
            }
            PipelineNode::Transform { input, transform } => {
                input.collect_contracts(contracts);
                if !contracts.contains(transform) {
                    contracts.push(transform.clone());
                }
            }
        }
    }

    /// Get the depth of this pipeline (for visualization)
    fn depth(&self) -> usize {
        match self {
            PipelineNode::Contract(_) => 1,
            PipelineNode::Sequence(nodes) => nodes.iter().map(|n| n.depth()).sum(),
            PipelineNode::Parallel(nodes) => nodes.iter().map(|n| n.depth()).max().unwrap_or(0) + 1,
            PipelineNode::Conditional {
                then_branch,
                else_branch,
                ..
            } => {
                let then_depth = then_branch.depth();
                let else_depth = else_branch.as_ref().map(|e| e.depth()).unwrap_or(0);
                then_depth.max(else_depth) + 1
            }
            PipelineNode::Loop { body, .. } => body.depth() + 1,
            PipelineNode::Transform { input, .. } => input.depth() + 1,
        }
    }
}

/// Composed pipeline with computed effective envelope
#[derive(Debug)]
pub struct Pipeline {
    /// Pipeline name
    name: String,
    /// Root node of the pipeline
    root: PipelineNode,
    /// Effective envelope (intersection of all component envelopes)
    effective_envelope: Envelope,
    /// All contract IDs in the pipeline
    contract_ids: Vec<String>,
}

impl Pipeline {
    /// Build a pipeline from a definition
    ///
    /// Validates that all referenced contracts exist and computes the effective envelope.
    pub fn build(name: &str, root: PipelineNode, engine: &ContractEngine) -> McpResult<Self> {
        // Collect all contract IDs
        let mut contract_ids = Vec::new();
        root.collect_contracts(&mut contract_ids);

        // Validate all contracts exist
        let mut missing = Vec::new();
        for id in &contract_ids {
            if engine.get_definition(id).is_none() {
                missing.push(id.clone());
            }
        }

        if !missing.is_empty() {
            return Err(McpError::ContractError(format!(
                "Pipeline '{}' references missing contracts: {}",
                name,
                missing.join(", ")
            )));
        }

        // Compute effective envelope
        let effective_envelope = Self::compute_envelope(&root, engine)?;

        Ok(Self {
            name: name.to_string(),
            root,
            effective_envelope,
            contract_ids,
        })
    }

    /// Compute effective envelope as intersection of all component envelopes
    ///
    /// The effective envelope is the most restrictive combination of all
    /// component envelopes, ensuring the pipeline respects all constraints.
    fn compute_envelope(node: &PipelineNode, engine: &ContractEngine) -> McpResult<Envelope> {
        match node {
            PipelineNode::Contract(id) => {
                let def = engine
                    .get_definition(id)
                    .ok_or_else(|| McpError::ContractError(format!("Contract '{}' not found", id)))?;
                Ok(def.envelope.clone())
            }
            PipelineNode::Sequence(nodes) | PipelineNode::Parallel(nodes) => {
                if nodes.is_empty() {
                    return Ok(Envelope::default());
                }

                let mut result = Self::compute_envelope(&nodes[0], engine)?;
                for node in nodes.iter().skip(1) {
                    let env = Self::compute_envelope(node, engine)?;
                    result = Self::intersect_envelopes(&result, &env);
                }
                Ok(result)
            }
            PipelineNode::Conditional {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_def = engine.get_definition(condition).ok_or_else(|| {
                    McpError::ContractError(format!("Condition contract '{}' not found", condition))
                })?;

                let then_env = Self::compute_envelope(then_branch, engine)?;
                let mut result = Self::intersect_envelopes(&cond_def.envelope, &then_env);

                if let Some(else_node) = else_branch {
                    let else_env = Self::compute_envelope(else_node, engine)?;
                    result = Self::intersect_envelopes(&result, &else_env);
                }

                Ok(result)
            }
            PipelineNode::Loop {
                condition, body, ..
            } => {
                let cond_def = engine.get_definition(condition).ok_or_else(|| {
                    McpError::ContractError(format!("Condition contract '{}' not found", condition))
                })?;

                let body_env = Self::compute_envelope(body, engine)?;
                Ok(Self::intersect_envelopes(&cond_def.envelope, &body_env))
            }
            PipelineNode::Transform { input, transform } => {
                let input_env = Self::compute_envelope(input, engine)?;
                let transform_def = engine.get_definition(transform).ok_or_else(|| {
                    McpError::ContractError(format!("Transform contract '{}' not found", transform))
                })?;

                Ok(Self::intersect_envelopes(&input_env, &transform_def.envelope))
            }
        }
    }

    /// Intersect two envelopes, taking the most restrictive constraints
    fn intersect_envelopes(a: &Envelope, b: &Envelope) -> Envelope {
        // Capabilities: intersection (only capabilities present in both)
        let cap_a: HashSet<_> = a.capabilities.iter().cloned().collect();
        let cap_b: HashSet<_> = b.capabilities.iter().cloned().collect();
        let capabilities: Vec<_> = cap_a.intersection(&cap_b).cloned().collect();

        // Max operations: minimum of both (most restrictive)
        let max_operations = match (a.max_operations, b.max_operations) {
            (Some(x), Some(y)) => Some(x.min(y)),
            (Some(x), None) => Some(x),
            (None, Some(y)) => Some(y),
            (None, None) => None,
        };

        // Max memory: minimum of both
        let max_memory_bytes = match (a.max_memory_bytes, b.max_memory_bytes) {
            (Some(x), Some(y)) => Some(x.min(y)),
            (Some(x), None) => Some(x),
            (None, Some(y)) => Some(y),
            (None, None) => None,
        };

        // Max duration: minimum of both
        let max_duration_ms = match (a.max_duration_ms, b.max_duration_ms) {
            (Some(x), Some(y)) => Some(x.min(y)),
            (Some(x), None) => Some(x),
            (None, Some(y)) => Some(y),
            (None, None) => None,
        };

        // Allowed outputs: intersection
        let outputs_a: HashSet<_> = a.allowed_outputs.iter().cloned().collect();
        let outputs_b: HashSet<_> = b.allowed_outputs.iter().cloned().collect();
        let allowed_outputs: Vec<_> = if outputs_a.is_empty() {
            b.allowed_outputs.clone()
        } else if outputs_b.is_empty() {
            a.allowed_outputs.clone()
        } else {
            outputs_a.intersection(&outputs_b).cloned().collect()
        };

        // Disallowed patterns: union (all patterns from both)
        let mut disallowed_patterns = a.disallowed_patterns.clone();
        for pattern in &b.disallowed_patterns {
            if !disallowed_patterns.contains(pattern) {
                disallowed_patterns.push(pattern.clone());
            }
        }

        // Custom constraints: merge (b overrides a on conflict)
        let mut custom_constraints = a.custom_constraints.clone();
        custom_constraints.extend(b.custom_constraints.clone());

        Envelope {
            max_operations,
            max_memory_bytes,
            max_duration_ms,
            capabilities,
            allowed_outputs,
            disallowed_patterns,
            custom_constraints,
        }
    }

    /// Validate that the pipeline is well-formed
    pub fn validate(&self, engine: &ContractEngine) -> McpResult<ValidationResult> {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();
        let mut contracts_missing = Vec::new();

        // Check all contracts exist
        for id in &self.contract_ids {
            if engine.get_definition(id).is_none() {
                contracts_missing.push(id.clone());
                errors.push(format!("Contract '{}' not registered", id));
            }
        }

        // Check for empty sequences
        self.validate_node(&self.root, &mut warnings, &mut errors);

        // Check effective envelope has at least Read capability
        if !self.effective_envelope.has_capability(Capability::Read) {
            warnings.push("Effective envelope has no Read capability".to_string());
        }

        // Check for very restrictive constraints
        if let Some(max_ops) = self.effective_envelope.max_operations {
            if max_ops < 10 {
                warnings.push(format!(
                    "Effective envelope has very low max_operations: {}",
                    max_ops
                ));
            }
        }

        let valid = errors.is_empty();
        let contracts_found = self.contract_ids.len() - contracts_missing.len();

        Ok(ValidationResult {
            valid,
            warnings,
            errors,
            contracts_found,
            contracts_missing,
        })
    }

    /// Recursively validate a node
    fn validate_node(
        &self,
        node: &PipelineNode,
        warnings: &mut Vec<String>,
        errors: &mut Vec<String>,
    ) {
        match node {
            PipelineNode::Contract(_) => {}
            PipelineNode::Sequence(nodes) => {
                if nodes.is_empty() {
                    warnings.push("Empty sequence in pipeline".to_string());
                }
                for n in nodes {
                    self.validate_node(n, warnings, errors);
                }
            }
            PipelineNode::Parallel(nodes) => {
                if nodes.is_empty() {
                    warnings.push("Empty parallel block in pipeline".to_string());
                }
                if nodes.len() == 1 {
                    warnings.push("Parallel block with single branch is redundant".to_string());
                }
                for n in nodes {
                    self.validate_node(n, warnings, errors);
                }
            }
            PipelineNode::Conditional {
                then_branch,
                else_branch,
                ..
            } => {
                self.validate_node(then_branch, warnings, errors);
                if let Some(else_node) = else_branch {
                    self.validate_node(else_node, warnings, errors);
                }
            }
            PipelineNode::Loop {
                body,
                max_iterations,
                ..
            } => {
                if *max_iterations == 0 {
                    errors.push("Loop with max_iterations=0 will never execute".to_string());
                }
                if *max_iterations > 1000 {
                    warnings.push(format!(
                        "Loop with high max_iterations ({}) may cause performance issues",
                        max_iterations
                    ));
                }
                self.validate_node(body, warnings, errors);
            }
            PipelineNode::Transform { input, .. } => {
                self.validate_node(input, warnings, errors);
            }
        }
    }

    /// Execute the pipeline
    pub fn execute(
        &self,
        input: &[u8],
        engine: &mut ContractEngine,
    ) -> McpResult<PipelineResult> {
        let start = Instant::now();
        let mut stage_results = Vec::new();

        let (success, final_hash) =
            self.execute_node(&self.root, input, engine, &mut stage_results)?;

        let total_duration_us = start.elapsed().as_micros() as u64;

        Ok(PipelineResult {
            success,
            stages_executed: stage_results.len(),
            total_duration_us,
            stage_results,
            final_output_hash: final_hash,
        })
    }

    /// Execute a pipeline node recursively
    fn execute_node(
        &self,
        node: &PipelineNode,
        input: &[u8],
        engine: &mut ContractEngine,
        stage_results: &mut Vec<StageResult>,
    ) -> McpResult<(bool, String)> {
        match node {
            PipelineNode::Contract(id) => {
                let stage_start = Instant::now();

                let result = engine.execute(id, "pipeline.execute", input, Capability::Execute, || {
                    Ok(hash_sha256(input))
                })?;

                let duration_us = stage_start.elapsed().as_micros() as u64;
                let success = result.is_success();
                let output_hash = match &result {
                    ExecutionResult::Success { output_hash, .. } => output_hash.clone(),
                    ExecutionResult::Refused { refusal } => {
                        hash_sha256(refusal.reason.as_bytes())
                    }
                };

                stage_results.push(StageResult {
                    stage_name: format!("contract:{}", id),
                    contract_id: id.clone(),
                    result,
                    duration_us,
                });

                Ok((success, output_hash))
            }
            PipelineNode::Sequence(nodes) => {
                let mut current_input = input.to_vec();
                let mut last_hash = hash_sha256(input);

                for (i, node) in nodes.iter().enumerate() {
                    let (success, hash) =
                        self.execute_node(node, &current_input, engine, stage_results)?;

                    if !success {
                        return Ok((false, hash));
                    }

                    // Use the hash as the next input (simulating data flow)
                    current_input = hash.as_bytes().to_vec();
                    last_hash = hash;

                    // Early termination if we hit pipeline limits
                    if i > 100 {
                        return Err(McpError::ContractError(
                            "Pipeline exceeded maximum sequence depth".to_string(),
                        ));
                    }
                }

                Ok((true, last_hash))
            }
            PipelineNode::Parallel(nodes) => {
                let mut all_success = true;
                let mut hashes = Vec::new();

                for node in nodes {
                    let (success, hash) = self.execute_node(node, input, engine, stage_results)?;
                    all_success = all_success && success;
                    hashes.push(hash);
                }

                // Combine all output hashes
                let combined = hashes.join("|");
                let final_hash = hash_sha256(combined.as_bytes());

                Ok((all_success, final_hash))
            }
            PipelineNode::Conditional {
                condition,
                then_branch,
                else_branch,
            } => {
                // Execute condition contract
                let stage_start = Instant::now();
                let cond_result =
                    engine.execute(condition, "pipeline.condition", input, Capability::Execute, || {
                        // Simulate condition evaluation based on input hash
                        let hash = hash_sha256(input);
                        // Use first byte to determine condition (for demonstration)
                        let first_byte = u8::from_str_radix(&hash[0..2], 16).unwrap_or(0);
                        Ok(first_byte > 127)
                    })?;

                let duration_us = stage_start.elapsed().as_micros() as u64;
                let cond_success = cond_result.is_success();

                stage_results.push(StageResult {
                    stage_name: format!("condition:{}", condition),
                    contract_id: condition.clone(),
                    result: cond_result.clone(),
                    duration_us,
                });

                if !cond_success {
                    let hash = match &cond_result {
                        ExecutionResult::Refused { refusal } => {
                            hash_sha256(refusal.reason.as_bytes())
                        }
                        _ => hash_sha256(b"condition_failed"),
                    };
                    return Ok((false, hash));
                }

                // Determine which branch to take based on output
                // For simplicity, we'll use the output hash to determine branching
                let take_then = match &cond_result {
                    ExecutionResult::Success { output_hash, .. } => {
                        let first_byte =
                            u8::from_str_radix(&output_hash[0..2], 16).unwrap_or(0);
                        first_byte > 127
                    }
                    _ => false,
                };

                if take_then {
                    self.execute_node(then_branch, input, engine, stage_results)
                } else if let Some(else_node) = else_branch {
                    self.execute_node(else_node, input, engine, stage_results)
                } else {
                    // No else branch, return success with input hash
                    Ok((true, hash_sha256(input)))
                }
            }
            PipelineNode::Loop {
                condition,
                body,
                max_iterations,
            } => {
                let mut current_input = input.to_vec();
                let mut last_hash = hash_sha256(input);
                let mut iteration = 0;

                while iteration < *max_iterations {
                    // Check condition
                    let stage_start = Instant::now();
                    let cond_result = engine.execute(
                        condition,
                        "pipeline.loop_condition",
                        &current_input,
                        Capability::Execute,
                        || {
                            // Condition becomes false after a few iterations (for safety)
                            Ok(iteration < 3)
                        },
                    )?;

                    let duration_us = stage_start.elapsed().as_micros() as u64;

                    stage_results.push(StageResult {
                        stage_name: format!("loop_condition:{}", condition),
                        contract_id: condition.clone(),
                        result: cond_result.clone(),
                        duration_us,
                    });

                    // Check if condition is still true
                    let continue_loop = match &cond_result {
                        ExecutionResult::Success { output_hash, .. } => {
                            // Use hash to determine if we continue
                            let first_byte =
                                u8::from_str_radix(&output_hash[0..2], 16).unwrap_or(0);
                            first_byte > 127 && iteration < 3
                        }
                        ExecutionResult::Refused { .. } => false,
                    };

                    if !continue_loop {
                        break;
                    }

                    // Execute body
                    let (success, hash) =
                        self.execute_node(body, &current_input, engine, stage_results)?;

                    if !success {
                        return Ok((false, hash));
                    }

                    current_input = hash.as_bytes().to_vec();
                    last_hash = hash;
                    iteration += 1;
                }

                Ok((true, last_hash))
            }
            PipelineNode::Transform { input: input_node, transform } => {
                // Execute input first
                let (input_success, input_hash) =
                    self.execute_node(input_node, input, engine, stage_results)?;

                if !input_success {
                    return Ok((false, input_hash));
                }

                // Apply transform
                let stage_start = Instant::now();
                let transform_result = engine.execute(
                    transform,
                    "pipeline.transform",
                    input_hash.as_bytes(),
                    Capability::Execute,
                    || Ok(hash_sha256(input_hash.as_bytes())),
                )?;

                let duration_us = stage_start.elapsed().as_micros() as u64;
                let success = transform_result.is_success();
                let output_hash = match &transform_result {
                    ExecutionResult::Success { output_hash, .. } => output_hash.clone(),
                    ExecutionResult::Refused { refusal } => {
                        hash_sha256(refusal.reason.as_bytes())
                    }
                };

                stage_results.push(StageResult {
                    stage_name: format!("transform:{}", transform),
                    contract_id: transform.clone(),
                    result: transform_result,
                    duration_us,
                });

                Ok((success, output_hash))
            }
        }
    }

    /// Get the effective envelope
    pub fn envelope(&self) -> &Envelope {
        &self.effective_envelope
    }

    /// Get the pipeline name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// List all contracts in the pipeline
    pub fn contracts(&self) -> Vec<&str> {
        self.contract_ids.iter().map(|s| s.as_str()).collect()
    }

    /// Visualize the pipeline as ASCII art
    pub fn visualize(&self) -> String {
        let mut output = String::new();
        output.push_str(&format!("Pipeline: {}\n", self.name));
        output.push_str(&format!(
            "Effective Envelope: {} capabilities, max_ops={:?}\n",
            self.effective_envelope.capabilities.len(),
            self.effective_envelope.max_operations
        ));
        output.push_str(&format!("Contracts: {}\n", self.contract_ids.len()));
        output.push_str(&format!("Depth: {}\n\n", self.root.depth()));
        self.visualize_node(&self.root, 0, &mut output);
        output
    }

    /// Recursively visualize a node
    fn visualize_node(&self, node: &PipelineNode, indent: usize, output: &mut String) {
        let prefix = "  ".repeat(indent);

        match node {
            PipelineNode::Contract(id) => {
                output.push_str(&format!("{}[{}]\n", prefix, id));
            }
            PipelineNode::Sequence(nodes) => {
                output.push_str(&format!("{}SEQUENCE:\n", prefix));
                for (i, n) in nodes.iter().enumerate() {
                    output.push_str(&format!("{}  {} -> ", prefix, i + 1));
                    self.visualize_node_inline(n, output);
                    output.push('\n');
                }
            }
            PipelineNode::Parallel(nodes) => {
                output.push_str(&format!("{}PARALLEL:\n", prefix));
                for (i, n) in nodes.iter().enumerate() {
                    output.push_str(&format!("{}  | Branch {}: ", prefix, i + 1));
                    self.visualize_node_inline(n, output);
                    output.push('\n');
                }
            }
            PipelineNode::Conditional {
                condition,
                then_branch,
                else_branch,
            } => {
                output.push_str(&format!("{}IF [{}]:\n", prefix, condition));
                output.push_str(&format!("{}  THEN: ", prefix));
                self.visualize_node_inline(then_branch, output);
                output.push('\n');
                if let Some(else_node) = else_branch {
                    output.push_str(&format!("{}  ELSE: ", prefix));
                    self.visualize_node_inline(else_node, output);
                    output.push('\n');
                }
            }
            PipelineNode::Loop {
                condition,
                body,
                max_iterations,
            } => {
                output.push_str(&format!(
                    "{}WHILE [{}] (max={}):\n",
                    prefix, condition, max_iterations
                ));
                output.push_str(&format!("{}  DO: ", prefix));
                self.visualize_node_inline(body, output);
                output.push('\n');
            }
            PipelineNode::Transform { input, transform } => {
                output.push_str(&format!("{}TRANSFORM [{}]:\n", prefix, transform));
                output.push_str(&format!("{}  INPUT: ", prefix));
                self.visualize_node_inline(input, output);
                output.push('\n');
            }
        }
    }

    /// Visualize a node inline (single line)
    fn visualize_node_inline(&self, node: &PipelineNode, output: &mut String) {
        match node {
            PipelineNode::Contract(id) => {
                output.push_str(&format!("[{}]", id));
            }
            PipelineNode::Sequence(nodes) => {
                let parts: Vec<String> = nodes
                    .iter()
                    .map(|n| {
                        let mut s = String::new();
                        self.visualize_node_inline(n, &mut s);
                        s
                    })
                    .collect();
                output.push_str(&format!("({})", parts.join(" -> ")));
            }
            PipelineNode::Parallel(nodes) => {
                let parts: Vec<String> = nodes
                    .iter()
                    .map(|n| {
                        let mut s = String::new();
                        self.visualize_node_inline(n, &mut s);
                        s
                    })
                    .collect();
                output.push_str(&format!("({})", parts.join(" | ")));
            }
            PipelineNode::Conditional { condition, .. } => {
                output.push_str(&format!("IF[{}]", condition));
            }
            PipelineNode::Loop {
                condition,
                max_iterations,
                ..
            } => {
                output.push_str(&format!("WHILE[{},{}]", condition, max_iterations));
            }
            PipelineNode::Transform { transform, .. } => {
                output.push_str(&format!("TRANSFORM[{}]", transform));
            }
        }
    }
}

/// Validation result for a pipeline
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    /// Whether the pipeline is valid
    pub valid: bool,
    /// Warning messages (non-fatal issues)
    pub warnings: Vec<String>,
    /// Error messages (fatal issues)
    pub errors: Vec<String>,
    /// Number of contracts found
    pub contracts_found: usize,
    /// Contracts that are missing from the engine
    pub contracts_missing: Vec<String>,
}

/// Result of pipeline execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineResult {
    /// Whether all stages succeeded
    pub success: bool,
    /// Number of stages executed
    pub stages_executed: usize,
    /// Total duration in microseconds
    pub total_duration_us: u64,
    /// Results from each stage
    pub stage_results: Vec<StageResult>,
    /// Hash of the final output
    pub final_output_hash: String,
}

/// Result of a single pipeline stage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StageResult {
    /// Stage name (e.g., "contract:validate", "condition:check")
    pub stage_name: String,
    /// Contract ID that was executed
    pub contract_id: String,
    /// Execution result
    pub result: ExecutionResult,
    /// Duration in microseconds
    pub duration_us: u64,
}

/// DSL builder for constructing pipelines
pub struct PipelineBuilder {
    nodes: Vec<PipelineNode>,
}

impl Default for PipelineBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl PipelineBuilder {
    /// Create a new pipeline builder
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    /// Add a contract to the sequence
    pub fn then(mut self, contract_id: &str) -> Self {
        self.nodes.push(PipelineNode::Contract(contract_id.to_string()));
        self
    }

    /// Add a parallel branch
    pub fn parallel(mut self, branches: Vec<&str>) -> Self {
        let branch_nodes: Vec<PipelineNode> = branches
            .into_iter()
            .map(|id| PipelineNode::Contract(id.to_string()))
            .collect();
        self.nodes.push(PipelineNode::Parallel(branch_nodes));
        self
    }

    /// Add a parallel branch with nested pipelines
    pub fn parallel_pipelines(mut self, branches: Vec<PipelineNode>) -> Self {
        self.nodes.push(PipelineNode::Parallel(branches));
        self
    }

    /// Add a conditional branch
    pub fn if_then_else(
        mut self,
        condition: &str,
        then_contract: &str,
        else_contract: Option<&str>,
    ) -> Self {
        self.nodes.push(PipelineNode::Conditional {
            condition: condition.to_string(),
            then_branch: Box::new(PipelineNode::Contract(then_contract.to_string())),
            else_branch: else_contract
                .map(|id| Box::new(PipelineNode::Contract(id.to_string()))),
        });
        self
    }

    /// Add a conditional with nested pipelines
    pub fn if_then_else_pipelines(
        mut self,
        condition: &str,
        then_branch: PipelineNode,
        else_branch: Option<PipelineNode>,
    ) -> Self {
        self.nodes.push(PipelineNode::Conditional {
            condition: condition.to_string(),
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        });
        self
    }

    /// Add a loop
    pub fn while_do(mut self, condition: &str, body: &str, max_iter: u32) -> Self {
        self.nodes.push(PipelineNode::Loop {
            condition: condition.to_string(),
            body: Box::new(PipelineNode::Contract(body.to_string())),
            max_iterations: max_iter,
        });
        self
    }

    /// Add a loop with nested pipeline body
    pub fn while_do_pipeline(mut self, condition: &str, body: PipelineNode, max_iter: u32) -> Self {
        self.nodes.push(PipelineNode::Loop {
            condition: condition.to_string(),
            body: Box::new(body),
            max_iterations: max_iter,
        });
        self
    }

    /// Add a transform stage
    pub fn transform(mut self, transform: &str) -> Self {
        if let Some(last) = self.nodes.pop() {
            self.nodes.push(PipelineNode::Transform {
                input: Box::new(last),
                transform: transform.to_string(),
            });
        }
        self
    }

    /// Build the final pipeline
    pub fn build(self, name: &str, engine: &ContractEngine) -> McpResult<Pipeline> {
        let root = if self.nodes.len() == 1 {
            self.nodes.into_iter().next().unwrap()
        } else {
            PipelineNode::Sequence(self.nodes)
        };

        Pipeline::build(name, root, engine)
    }

    /// Build the root node without validation (for testing)
    pub fn build_node(self) -> PipelineNode {
        if self.nodes.len() == 1 {
            self.nodes.into_iter().next().unwrap()
        } else {
            PipelineNode::Sequence(self.nodes)
        }
    }
}

/// Create a simple sequential pipeline
///
/// # Example
/// ```ignore
/// let pipeline = pipeline!("my-pipeline" => "contract-a", "contract-b", "contract-c");
/// ```
#[macro_export]
macro_rules! pipeline {
    ($name:expr => $($contract:expr),+ $(,)?) => {{
        let nodes = vec![
            $(PipelineNode::Contract($contract.to_string())),+
        ];
        PipelineNode::Sequence(nodes)
    }};
}

/// Create a parallel pipeline
///
/// # Example
/// ```ignore
/// let pipeline = parallel!("contract-a", "contract-b", "contract-c");
/// ```
#[macro_export]
macro_rules! parallel {
    ($($contract:expr),+ $(,)?) => {{
        let nodes = vec![
            $(PipelineNode::Contract($contract.to_string())),+
        ];
        PipelineNode::Parallel(nodes)
    }};
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::ContractDefinition;
    use mcp_core::crypto::KeyPair;

    fn setup_engine() -> ContractEngine {
        let keypair = KeyPair::generate().unwrap();
        let mut engine = ContractEngine::new(keypair);

        // Register test contracts with different envelopes
        let contracts = vec![
            ("validate", create_envelope(vec![Capability::Read], Some(100), Some(1000))),
            ("process", create_envelope(vec![Capability::Read, Capability::Write], Some(50), Some(2000))),
            ("output", create_envelope(vec![Capability::Read, Capability::Write], Some(200), Some(5000))),
            ("condition", create_envelope(vec![Capability::Read, Capability::Execute], Some(1000), Some(100))),
            ("transform", create_envelope(vec![Capability::Read], Some(500), Some(3000))),
            ("loop-body", create_envelope(vec![Capability::Read, Capability::Execute], Some(100), Some(1000))),
        ];

        for (id, envelope) in contracts {
            let def = ContractDefinition::new(id, "test-family", envelope);
            engine.register(def).unwrap();
        }

        engine
    }

    fn create_envelope(
        capabilities: Vec<Capability>,
        max_ops: Option<u64>,
        max_duration: Option<u64>,
    ) -> Envelope {
        Envelope {
            capabilities,
            max_operations: max_ops,
            max_duration_ms: max_duration,
            ..Default::default()
        }
    }

    // ==================== PipelineNode Tests ====================

    #[test]
    fn test_pipeline_node_contract_collects_single() {
        let node = PipelineNode::Contract("test-contract".to_string());
        let mut contracts = Vec::new();
        node.collect_contracts(&mut contracts);

        assert_eq!(contracts.len(), 1);
        assert_eq!(contracts[0], "test-contract");
    }

    #[test]
    fn test_pipeline_node_sequence_collects_all() {
        let node = PipelineNode::Sequence(vec![
            PipelineNode::Contract("a".to_string()),
            PipelineNode::Contract("b".to_string()),
            PipelineNode::Contract("c".to_string()),
        ]);
        let mut contracts = Vec::new();
        node.collect_contracts(&mut contracts);

        assert_eq!(contracts.len(), 3);
        assert!(contracts.contains(&"a".to_string()));
        assert!(contracts.contains(&"b".to_string()));
        assert!(contracts.contains(&"c".to_string()));
    }

    #[test]
    fn test_pipeline_node_parallel_collects_all() {
        let node = PipelineNode::Parallel(vec![
            PipelineNode::Contract("x".to_string()),
            PipelineNode::Contract("y".to_string()),
        ]);
        let mut contracts = Vec::new();
        node.collect_contracts(&mut contracts);

        assert_eq!(contracts.len(), 2);
    }

    #[test]
    fn test_pipeline_node_conditional_collects_all_branches() {
        let node = PipelineNode::Conditional {
            condition: "cond".to_string(),
            then_branch: Box::new(PipelineNode::Contract("then".to_string())),
            else_branch: Some(Box::new(PipelineNode::Contract("else".to_string()))),
        };
        let mut contracts = Vec::new();
        node.collect_contracts(&mut contracts);

        assert_eq!(contracts.len(), 3);
        assert!(contracts.contains(&"cond".to_string()));
        assert!(contracts.contains(&"then".to_string()));
        assert!(contracts.contains(&"else".to_string()));
    }

    #[test]
    fn test_pipeline_node_loop_collects_condition_and_body() {
        let node = PipelineNode::Loop {
            condition: "check".to_string(),
            body: Box::new(PipelineNode::Contract("work".to_string())),
            max_iterations: 10,
        };
        let mut contracts = Vec::new();
        node.collect_contracts(&mut contracts);

        assert_eq!(contracts.len(), 2);
        assert!(contracts.contains(&"check".to_string()));
        assert!(contracts.contains(&"work".to_string()));
    }

    #[test]
    fn test_pipeline_node_transform_collects_input_and_transform() {
        let node = PipelineNode::Transform {
            input: Box::new(PipelineNode::Contract("input".to_string())),
            transform: "xform".to_string(),
        };
        let mut contracts = Vec::new();
        node.collect_contracts(&mut contracts);

        assert_eq!(contracts.len(), 2);
        assert!(contracts.contains(&"input".to_string()));
        assert!(contracts.contains(&"xform".to_string()));
    }

    #[test]
    fn test_pipeline_node_depth_contract() {
        let node = PipelineNode::Contract("test".to_string());
        assert_eq!(node.depth(), 1);
    }

    #[test]
    fn test_pipeline_node_depth_sequence() {
        let node = PipelineNode::Sequence(vec![
            PipelineNode::Contract("a".to_string()),
            PipelineNode::Contract("b".to_string()),
            PipelineNode::Contract("c".to_string()),
        ]);
        assert_eq!(node.depth(), 3);
    }

    #[test]
    fn test_pipeline_node_depth_parallel() {
        let node = PipelineNode::Parallel(vec![
            PipelineNode::Contract("a".to_string()),
            PipelineNode::Sequence(vec![
                PipelineNode::Contract("b".to_string()),
                PipelineNode::Contract("c".to_string()),
            ]),
        ]);
        assert_eq!(node.depth(), 3); // max(1, 2) + 1
    }

    // ==================== Envelope Intersection Tests ====================

    #[test]
    fn test_envelope_intersection_capabilities() {
        let a = create_envelope(
            vec![Capability::Read, Capability::Write],
            None,
            None,
        );
        let b = create_envelope(
            vec![Capability::Read, Capability::Execute],
            None,
            None,
        );

        let result = Pipeline::intersect_envelopes(&a, &b);

        assert!(result.has_capability(Capability::Read));
        assert!(!result.has_capability(Capability::Write));
        assert!(!result.has_capability(Capability::Execute));
    }

    #[test]
    fn test_envelope_intersection_max_operations() {
        let a = Envelope {
            max_operations: Some(100),
            ..Default::default()
        };
        let b = Envelope {
            max_operations: Some(50),
            ..Default::default()
        };

        let result = Pipeline::intersect_envelopes(&a, &b);
        assert_eq!(result.max_operations, Some(50)); // min(100, 50)
    }

    #[test]
    fn test_envelope_intersection_max_duration() {
        let a = Envelope {
            max_duration_ms: Some(1000),
            ..Default::default()
        };
        let b = Envelope {
            max_duration_ms: Some(500),
            ..Default::default()
        };

        let result = Pipeline::intersect_envelopes(&a, &b);
        assert_eq!(result.max_duration_ms, Some(500));
    }

    #[test]
    fn test_envelope_intersection_max_memory() {
        let a = Envelope {
            max_memory_bytes: Some(1024),
            ..Default::default()
        };
        let b = Envelope {
            max_memory_bytes: Some(2048),
            ..Default::default()
        };

        let result = Pipeline::intersect_envelopes(&a, &b);
        assert_eq!(result.max_memory_bytes, Some(1024));
    }

    #[test]
    fn test_envelope_intersection_disallowed_patterns_union() {
        let a = Envelope {
            disallowed_patterns: vec!["password".to_string()],
            ..Default::default()
        };
        let b = Envelope {
            disallowed_patterns: vec!["secret".to_string(), "password".to_string()],
            ..Default::default()
        };

        let result = Pipeline::intersect_envelopes(&a, &b);
        assert_eq!(result.disallowed_patterns.len(), 2);
        assert!(result.disallowed_patterns.contains(&"password".to_string()));
        assert!(result.disallowed_patterns.contains(&"secret".to_string()));
    }

    #[test]
    fn test_envelope_intersection_with_none() {
        let a = Envelope {
            max_operations: Some(100),
            max_duration_ms: None,
            ..Default::default()
        };
        let b = Envelope {
            max_operations: None,
            max_duration_ms: Some(500),
            ..Default::default()
        };

        let result = Pipeline::intersect_envelopes(&a, &b);
        assert_eq!(result.max_operations, Some(100));
        assert_eq!(result.max_duration_ms, Some(500));
    }

    // ==================== Pipeline Build Tests ====================

    #[test]
    fn test_pipeline_build_single_contract() {
        let engine = setup_engine();
        let node = PipelineNode::Contract("validate".to_string());

        let pipeline = Pipeline::build("single", node, &engine).unwrap();

        assert_eq!(pipeline.name(), "single");
        assert_eq!(pipeline.contracts().len(), 1);
        assert!(pipeline.contracts().contains(&"validate"));
    }

    #[test]
    fn test_pipeline_build_sequence() {
        let engine = setup_engine();
        let node = PipelineNode::Sequence(vec![
            PipelineNode::Contract("validate".to_string()),
            PipelineNode::Contract("process".to_string()),
            PipelineNode::Contract("output".to_string()),
        ]);

        let pipeline = Pipeline::build("sequence", node, &engine).unwrap();

        assert_eq!(pipeline.contracts().len(), 3);
        // Effective envelope should be intersection
        assert!(pipeline.envelope().has_capability(Capability::Read));
    }

    #[test]
    fn test_pipeline_build_missing_contract_fails() {
        let engine = setup_engine();
        let node = PipelineNode::Contract("nonexistent".to_string());

        let result = Pipeline::build("bad", node, &engine);

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("missing"));
    }

    #[test]
    fn test_pipeline_build_parallel() {
        let engine = setup_engine();
        let node = PipelineNode::Parallel(vec![
            PipelineNode::Contract("validate".to_string()),
            PipelineNode::Contract("process".to_string()),
        ]);

        let pipeline = Pipeline::build("parallel", node, &engine).unwrap();

        assert_eq!(pipeline.contracts().len(), 2);
    }

    #[test]
    fn test_pipeline_build_conditional() {
        let engine = setup_engine();
        let node = PipelineNode::Conditional {
            condition: "condition".to_string(),
            then_branch: Box::new(PipelineNode::Contract("process".to_string())),
            else_branch: Some(Box::new(PipelineNode::Contract("output".to_string()))),
        };

        let pipeline = Pipeline::build("conditional", node, &engine).unwrap();

        assert_eq!(pipeline.contracts().len(), 3);
    }

    #[test]
    fn test_pipeline_build_loop() {
        let engine = setup_engine();
        let node = PipelineNode::Loop {
            condition: "condition".to_string(),
            body: Box::new(PipelineNode::Contract("loop-body".to_string())),
            max_iterations: 10,
        };

        let pipeline = Pipeline::build("loop", node, &engine).unwrap();

        assert_eq!(pipeline.contracts().len(), 2);
    }

    #[test]
    fn test_pipeline_build_transform() {
        let engine = setup_engine();
        let node = PipelineNode::Transform {
            input: Box::new(PipelineNode::Contract("validate".to_string())),
            transform: "transform".to_string(),
        };

        let pipeline = Pipeline::build("transform", node, &engine).unwrap();

        assert_eq!(pipeline.contracts().len(), 2);
    }

    // ==================== Pipeline Validation Tests ====================

    #[test]
    fn test_pipeline_validate_valid() {
        let engine = setup_engine();
        let node = PipelineNode::Sequence(vec![
            PipelineNode::Contract("validate".to_string()),
            PipelineNode::Contract("process".to_string()),
        ]);

        let pipeline = Pipeline::build("valid", node, &engine).unwrap();
        let result = pipeline.validate(&engine).unwrap();

        assert!(result.valid);
        assert!(result.errors.is_empty());
        assert_eq!(result.contracts_found, 2);
    }

    #[test]
    fn test_pipeline_validate_empty_sequence_warning() {
        let engine = setup_engine();
        let node = PipelineNode::Sequence(vec![
            PipelineNode::Contract("validate".to_string()),
            PipelineNode::Sequence(vec![]), // Empty nested sequence
        ]);

        let pipeline = Pipeline::build("empty-seq", node, &engine).unwrap();
        let result = pipeline.validate(&engine).unwrap();

        assert!(result.valid); // Empty sequence is a warning, not error
        assert!(!result.warnings.is_empty());
    }

    #[test]
    fn test_pipeline_validate_single_parallel_warning() {
        let engine = setup_engine();
        let node = PipelineNode::Parallel(vec![PipelineNode::Contract("validate".to_string())]);

        let pipeline = Pipeline::build("single-parallel", node, &engine).unwrap();
        let result = pipeline.validate(&engine).unwrap();

        assert!(result.valid);
        assert!(result.warnings.iter().any(|w| w.contains("single branch")));
    }

    #[test]
    fn test_pipeline_validate_zero_iterations_error() {
        let engine = setup_engine();
        let node = PipelineNode::Loop {
            condition: "condition".to_string(),
            body: Box::new(PipelineNode::Contract("loop-body".to_string())),
            max_iterations: 0,
        };

        let pipeline = Pipeline::build("zero-loop", node, &engine).unwrap();
        let result = pipeline.validate(&engine).unwrap();

        assert!(!result.valid);
        assert!(result.errors.iter().any(|e| e.contains("max_iterations=0")));
    }

    #[test]
    fn test_pipeline_validate_high_iterations_warning() {
        let engine = setup_engine();
        let node = PipelineNode::Loop {
            condition: "condition".to_string(),
            body: Box::new(PipelineNode::Contract("loop-body".to_string())),
            max_iterations: 5000,
        };

        let pipeline = Pipeline::build("high-loop", node, &engine).unwrap();
        let result = pipeline.validate(&engine).unwrap();

        assert!(result.valid);
        assert!(result.warnings.iter().any(|w| w.contains("high max_iterations")));
    }

    // ==================== Pipeline Execution Tests ====================

    #[test]
    fn test_pipeline_execute_single_contract() {
        let mut engine = setup_engine();
        let node = PipelineNode::Contract("validate".to_string());
        let pipeline = Pipeline::build("single", node, &engine).unwrap();

        let result = pipeline.execute(b"test input", &mut engine).unwrap();

        assert!(result.success);
        assert_eq!(result.stages_executed, 1);
        assert!(!result.final_output_hash.is_empty());
    }

    #[test]
    fn test_pipeline_execute_sequence() {
        let mut engine = setup_engine();
        let node = PipelineNode::Sequence(vec![
            PipelineNode::Contract("validate".to_string()),
            PipelineNode::Contract("process".to_string()),
            PipelineNode::Contract("output".to_string()),
        ]);
        let pipeline = Pipeline::build("sequence", node, &engine).unwrap();

        let result = pipeline.execute(b"test input", &mut engine).unwrap();

        assert!(result.success);
        assert_eq!(result.stages_executed, 3);
    }

    #[test]
    fn test_pipeline_execute_parallel() {
        let mut engine = setup_engine();
        let node = PipelineNode::Parallel(vec![
            PipelineNode::Contract("validate".to_string()),
            PipelineNode::Contract("process".to_string()),
        ]);
        let pipeline = Pipeline::build("parallel", node, &engine).unwrap();

        let result = pipeline.execute(b"test input", &mut engine).unwrap();

        assert!(result.success);
        assert_eq!(result.stages_executed, 2);
    }

    #[test]
    fn test_pipeline_execute_conditional() {
        let mut engine = setup_engine();
        let node = PipelineNode::Conditional {
            condition: "condition".to_string(),
            then_branch: Box::new(PipelineNode::Contract("process".to_string())),
            else_branch: Some(Box::new(PipelineNode::Contract("output".to_string()))),
        };
        let pipeline = Pipeline::build("conditional", node, &engine).unwrap();

        let result = pipeline.execute(b"test input", &mut engine).unwrap();

        assert!(result.success);
        // Should have condition + one branch
        assert!(result.stages_executed >= 2);
    }

    #[test]
    fn test_pipeline_execute_loop() {
        let mut engine = setup_engine();
        let node = PipelineNode::Loop {
            condition: "condition".to_string(),
            body: Box::new(PipelineNode::Contract("loop-body".to_string())),
            max_iterations: 5,
        };
        let pipeline = Pipeline::build("loop", node, &engine).unwrap();

        let result = pipeline.execute(b"test input", &mut engine).unwrap();

        assert!(result.success);
        // Should have at least one condition check
        assert!(result.stages_executed >= 1);
    }

    #[test]
    fn test_pipeline_execute_transform() {
        let mut engine = setup_engine();
        let node = PipelineNode::Transform {
            input: Box::new(PipelineNode::Contract("validate".to_string())),
            transform: "transform".to_string(),
        };
        let pipeline = Pipeline::build("transform", node, &engine).unwrap();

        let result = pipeline.execute(b"test input", &mut engine).unwrap();

        assert!(result.success);
        assert_eq!(result.stages_executed, 2); // input + transform
    }

    #[test]
    fn test_pipeline_execute_records_timing() {
        let mut engine = setup_engine();
        let node = PipelineNode::Contract("validate".to_string());
        let pipeline = Pipeline::build("timing", node, &engine).unwrap();

        let result = pipeline.execute(b"test input", &mut engine).unwrap();

        assert!(result.total_duration_us > 0);
        assert!(result.stage_results[0].duration_us > 0);
    }

    // ==================== PipelineBuilder Tests ====================

    #[test]
    fn test_builder_then() {
        let engine = setup_engine();
        let pipeline = PipelineBuilder::new()
            .then("validate")
            .then("process")
            .build("builder-then", &engine)
            .unwrap();

        assert_eq!(pipeline.contracts().len(), 2);
    }

    #[test]
    fn test_builder_parallel() {
        let engine = setup_engine();
        let pipeline = PipelineBuilder::new()
            .then("validate")
            .parallel(vec!["process", "output"])
            .build("builder-parallel", &engine)
            .unwrap();

        assert_eq!(pipeline.contracts().len(), 3);
    }

    #[test]
    fn test_builder_if_then_else() {
        let engine = setup_engine();
        let pipeline = PipelineBuilder::new()
            .if_then_else("condition", "process", Some("output"))
            .build("builder-if", &engine)
            .unwrap();

        assert_eq!(pipeline.contracts().len(), 3);
    }

    #[test]
    fn test_builder_while_do() {
        let engine = setup_engine();
        let pipeline = PipelineBuilder::new()
            .while_do("condition", "loop-body", 10)
            .build("builder-while", &engine)
            .unwrap();

        assert_eq!(pipeline.contracts().len(), 2);
    }

    #[test]
    fn test_builder_transform() {
        let engine = setup_engine();
        let pipeline = PipelineBuilder::new()
            .then("validate")
            .transform("transform")
            .build("builder-transform", &engine)
            .unwrap();

        assert_eq!(pipeline.contracts().len(), 2);
    }

    #[test]
    fn test_builder_complex_pipeline() {
        let engine = setup_engine();
        let pipeline = PipelineBuilder::new()
            .then("validate")
            .if_then_else("condition", "process", Some("output"))
            .then("transform")
            .build("complex", &engine)
            .unwrap();

        // validate, condition, process, output, transform
        assert_eq!(pipeline.contracts().len(), 5);
    }

    #[test]
    fn test_builder_build_node() {
        let node = PipelineBuilder::new()
            .then("a")
            .then("b")
            .then("c")
            .build_node();

        let mut contracts = Vec::new();
        node.collect_contracts(&mut contracts);
        assert_eq!(contracts.len(), 3);
    }

    // ==================== Visualization Tests ====================

    #[test]
    fn test_pipeline_visualize_sequence() {
        let engine = setup_engine();
        let node = PipelineNode::Sequence(vec![
            PipelineNode::Contract("validate".to_string()),
            PipelineNode::Contract("process".to_string()),
        ]);
        let pipeline = Pipeline::build("viz-seq", node, &engine).unwrap();

        let viz = pipeline.visualize();

        assert!(viz.contains("Pipeline: viz-seq"));
        assert!(viz.contains("SEQUENCE"));
        assert!(viz.contains("validate"));
        assert!(viz.contains("process"));
    }

    #[test]
    fn test_pipeline_visualize_parallel() {
        let engine = setup_engine();
        let node = PipelineNode::Parallel(vec![
            PipelineNode::Contract("validate".to_string()),
            PipelineNode::Contract("process".to_string()),
        ]);
        let pipeline = Pipeline::build("viz-par", node, &engine).unwrap();

        let viz = pipeline.visualize();

        assert!(viz.contains("PARALLEL"));
        assert!(viz.contains("Branch"));
    }

    #[test]
    fn test_pipeline_visualize_conditional() {
        let engine = setup_engine();
        let node = PipelineNode::Conditional {
            condition: "condition".to_string(),
            then_branch: Box::new(PipelineNode::Contract("process".to_string())),
            else_branch: Some(Box::new(PipelineNode::Contract("output".to_string()))),
        };
        let pipeline = Pipeline::build("viz-if", node, &engine).unwrap();

        let viz = pipeline.visualize();

        assert!(viz.contains("IF"));
        assert!(viz.contains("THEN"));
        assert!(viz.contains("ELSE"));
    }

    #[test]
    fn test_pipeline_visualize_loop() {
        let engine = setup_engine();
        let node = PipelineNode::Loop {
            condition: "condition".to_string(),
            body: Box::new(PipelineNode::Contract("loop-body".to_string())),
            max_iterations: 10,
        };
        let pipeline = Pipeline::build("viz-loop", node, &engine).unwrap();

        let viz = pipeline.visualize();

        assert!(viz.contains("WHILE"));
        assert!(viz.contains("max=10"));
        assert!(viz.contains("DO"));
    }

    // ==================== Macro Tests ====================

    #[test]
    fn test_pipeline_macro() {
        let node = pipeline!("test" => "a", "b", "c");

        let mut contracts = Vec::new();
        node.collect_contracts(&mut contracts);
        assert_eq!(contracts.len(), 3);
    }

    #[test]
    fn test_parallel_macro() {
        let node = parallel!("x", "y", "z");

        match node {
            PipelineNode::Parallel(nodes) => {
                assert_eq!(nodes.len(), 3);
            }
            _ => panic!("Expected Parallel node"),
        }
    }

    // ==================== Edge Case Tests ====================

    #[test]
    fn test_deduplicates_contracts() {
        let node = PipelineNode::Sequence(vec![
            PipelineNode::Contract("validate".to_string()),
            PipelineNode::Contract("validate".to_string()), // Duplicate
            PipelineNode::Contract("process".to_string()),
        ]);

        let mut contracts = Vec::new();
        node.collect_contracts(&mut contracts);

        assert_eq!(contracts.len(), 2); // Should deduplicate
    }

    #[test]
    fn test_deeply_nested_pipeline() {
        let engine = setup_engine();
        let node = PipelineNode::Sequence(vec![
            PipelineNode::Conditional {
                condition: "condition".to_string(),
                then_branch: Box::new(PipelineNode::Parallel(vec![
                    PipelineNode::Contract("validate".to_string()),
                    PipelineNode::Transform {
                        input: Box::new(PipelineNode::Contract("process".to_string())),
                        transform: "transform".to_string(),
                    },
                ])),
                else_branch: Some(Box::new(PipelineNode::Loop {
                    condition: "condition".to_string(),
                    body: Box::new(PipelineNode::Contract("loop-body".to_string())),
                    max_iterations: 5,
                })),
            },
            PipelineNode::Contract("output".to_string()),
        ]);

        let pipeline = Pipeline::build("nested", node, &engine).unwrap();

        // All unique contracts should be collected
        let contracts = pipeline.contracts();
        assert!(contracts.contains(&"validate"));
        assert!(contracts.contains(&"process"));
        assert!(contracts.contains(&"output"));
        assert!(contracts.contains(&"condition"));
        assert!(contracts.contains(&"transform"));
        assert!(contracts.contains(&"loop-body"));
    }

    #[test]
    fn test_envelope_intersection_preserves_custom_constraints() {
        let mut a = Envelope::default();
        a.custom_constraints
            .insert("key1".to_string(), serde_json::json!("value1"));

        let mut b = Envelope::default();
        b.custom_constraints
            .insert("key2".to_string(), serde_json::json!("value2"));

        let result = Pipeline::intersect_envelopes(&a, &b);

        assert_eq!(result.custom_constraints.len(), 2);
        assert!(result.custom_constraints.contains_key("key1"));
        assert!(result.custom_constraints.contains_key("key2"));
    }

    #[test]
    fn test_stage_result_contains_contract_id() {
        let mut engine = setup_engine();
        let node = PipelineNode::Contract("validate".to_string());
        let pipeline = Pipeline::build("stage-test", node, &engine).unwrap();

        let result = pipeline.execute(b"test", &mut engine).unwrap();

        assert_eq!(result.stage_results[0].contract_id, "validate");
        assert!(result.stage_results[0].stage_name.contains("validate"));
    }
}
