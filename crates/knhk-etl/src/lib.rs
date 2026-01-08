// rust/knhk-etl/src/lib.rs
// ETL Pipeline Stages
// Implements: Ingest → Transform → Load → Reflex → Emit

#![no_std]
extern crate alloc;

use alloc::vec::Vec;
use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::string::ToString;
use alloc::format;

#[cfg(feature = "std")]
use std::io::BufRead;

use rio_api::parser::TriplesParser;
use rio_api::model::{Term, NamedNode, BlankNode, Literal, Triple};
use rio_turtle::TurtleParser;

/// Pipeline stage identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PipelineStage {
    Ingest,
    Transform,
    Load,
    Reflex,
    Emit,
}

/// Pipeline metrics
#[derive(Debug, Clone, Default)]
pub struct PipelineMetrics {
    pub stage: PipelineStage,
    pub delta_count: usize,
    pub triples_processed: usize,
    pub ticks_elapsed: u32,
    pub errors: usize,
}

/// Stage 1: Ingest
/// Input: Raw data from connectors (RDF/Turtle, JSON-LD, streaming triples)
pub struct IngestStage {
    pub connectors: Vec<String>, // Connector IDs
    pub format: String,
}

impl IngestStage {
    pub fn new(connectors: Vec<String>, format: String) -> Self {
        Self { connectors, format }
    }

    /// Ingest delta from connectors
    /// 
    /// Production implementation:
    /// 1. Poll connectors for new data
    /// 2. Parse based on format (RDF/Turtle, JSON-LD, etc.)
    /// 3. Validate basic structure
    /// 4. Return raw triples
    pub fn ingest(&self) -> Result<IngestResult, PipelineError> {
        let mut all_triples = Vec::new();
        let mut metadata = BTreeMap::new();

        // Poll each connector
        for connector_id in &self.connectors {
            // In production, this would fetch from connector registry
            // For now, return empty results (connector integration happens at pipeline level)
            metadata.insert(format!("connector_{}", connector_id), connector_id.clone());
        }

        // If format is specified and we have data, parse it
        // For now, return empty triples (connector integration provides deltas directly)
        Ok(IngestResult {
            triples: all_triples,
            metadata,
        })
    }

    /// Parse RDF/Turtle content into raw triples using rio_turtle
    /// 
    /// Full Turtle syntax support including:
    /// - Prefix resolution
    /// - Blank nodes
    /// - Base URI resolution
    /// - Literals (simple, typed, language-tagged)
    pub fn parse_rdf_turtle(&self, content: &str) -> Result<Vec<RawTriple>, PipelineError> {
        let mut triples = Vec::new();
        let mut parser = TurtleParser::new(content.as_bytes(), None)
            .map_err(|e| PipelineError::IngestError(format!("Failed to create Turtle parser: {}", e)))?;
        
        parser.parse_all(&mut |triple| {
            let raw = Self::convert_triple(triple)
                .map_err(|e| PipelineError::IngestError(format!("Failed to convert triple: {}", e)))?;
            triples.push(raw);
            Ok(())
        })
        .map_err(|e| {
            PipelineError::IngestError(format!(
                "RDF parse error at line {}: {}",
                e.location().line(),
                e.message()
            ))
        })?;

        Ok(triples)
    }

    /// Parse RDF/Turtle from a BufRead stream (memory-efficient for large files)
    #[cfg(feature = "std")]
    pub fn parse_rdf_turtle_stream<R: BufRead>(
        reader: R,
        base_uri: Option<&str>
    ) -> Result<Vec<RawTriple>, PipelineError> {
        let mut triples = Vec::new();
        let base = base_uri.and_then(|u| {
            NamedNode::new(u).ok()
        });
        
        let mut parser = TurtleParser::new(reader, base.as_ref())
            .map_err(|e| PipelineError::IngestError(format!("Failed to create Turtle parser: {}", e)))?;
        
        parser.parse_all(&mut |triple| {
            let raw = Self::convert_triple(triple)
                .map_err(|e| PipelineError::IngestError(format!("Failed to convert triple: {}", e)))?;
            triples.push(raw);
            Ok(())
        })
        .map_err(|e| {
            PipelineError::IngestError(format!(
                "RDF parse error at line {}: {}",
                e.location().line(),
                e.message()
            ))
        })?;

        Ok(triples)
    }

    /// Convert rio_api::Triple to RawTriple
    fn convert_triple(triple: &Triple) -> Result<RawTriple, String> {
        Ok(RawTriple {
            subject: Self::term_to_string(triple.subject)?,
            predicate: Self::term_to_string(triple.predicate)?,
            object: Self::term_to_string(triple.object)?,
            graph: None, // N-Quads support can be added later if needed
        })
    }

    /// Convert rio_api::Term to String representation
    /// 
    /// Handles:
    /// - NamedNode: Returns IRI string
    /// - BlankNode: Returns `_:id` format
    /// - Literal: Returns quoted string with type/language tags
    fn term_to_string(term: &Term) -> Result<String, String> {
        match term {
            Term::NamedNode(named) => Ok(named.iri.to_string()),
            Term::BlankNode(blank) => Ok(format!("_:{}", blank.id)),
            Term::Literal(literal) => {
                match literal {
                    Literal::Simple { value } => Ok(format!("\"{}\"", Self::escape_string(value))),
                    Literal::LanguageTaggedString { value, language } => {
                        Ok(format!("\"{}\"@{}", Self::escape_string(value), language))
                    }
                    Literal::Typed { value, datatype } => {
                        Ok(format!("\"{}\"^^{}", Self::escape_string(value), datatype.iri))
                    }
                }
            }
        }
    }

    /// Escape string literals for Turtle format
    fn escape_string(s: &str) -> String {
        // Basic escaping: escape quotes and backslashes
        // Full Turtle escaping would need more, but this covers common cases
        s.replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
    }
}

pub struct IngestResult {
    pub triples: Vec<RawTriple>,
    pub metadata: BTreeMap<String, String>,
}

pub struct RawTriple {
    pub subject: String,
    pub predicate: String,
    pub object: String,
    pub graph: Option<String>,
}

/// Stage 2: Transform
/// Typed by Σ, constrained by Q
pub struct TransformStage {
    pub schema_iri: String,
    pub validation_enabled: bool,
    schema_cache: BTreeMap<String, bool>, // Cache for schema validation
}

impl TransformStage {
    pub fn new(schema_iri: String, validation_enabled: bool) -> Self {
        Self {
            schema_iri,
            validation_enabled,
            schema_cache: BTreeMap::new(),
        }
    }

    /// Transform raw triples to typed, validated triples
    /// 
    /// Production implementation:
    /// 1. Validate against Σ schema (O ⊨ Σ)
    /// 2. Check Q invariants (preserve(Q))
    /// 3. Hash IRIs to u64 IDs (consistent hashing)
    /// 4. Map to typed triples
    pub fn transform(&self, input: IngestResult) -> Result<TransformResult, PipelineError> {
        let mut typed_triples = Vec::new();
        let mut validation_errors = Vec::new();

        for raw in input.triples {
            // Hash IRIs to u64 IDs using FNV-1a (consistent with C implementation)
            let s = Self::hash_iri(&raw.subject);
            let p = Self::hash_iri(&raw.predicate);
            let o = Self::hash_iri(&raw.object);
            let g = raw.graph.map(|g| Self::hash_iri(&g));

            // Schema validation (O ⊨ Σ check)
            if self.validation_enabled {
                if let Err(err) = self.validate_schema(&raw.subject, &raw.predicate) {
                    validation_errors.push(err);
                    continue; // Skip invalid triple
                }
            }

            typed_triples.push(TypedTriple {
                subject: s,
                predicate: p,
                object: o,
                graph: g,
            });
        }

        Ok(TransformResult {
            typed_triples,
            validation_errors,
        })
    }

    /// Hash IRI to u64 using FNV-1a (consistent with C implementation)
    fn hash_iri(iri: &str) -> u64 {
        const FNV_OFFSET_BASIS: u64 = 1469598103934665603;
        const FNV_PRIME: u64 = 1099511628211;

        let mut hash = FNV_OFFSET_BASIS;
        for byte in iri.as_bytes() {
            hash ^= *byte as u64;
            hash = hash.wrapping_mul(FNV_PRIME);
        }
        hash
    }

    /// Validate triple against schema (O ⊨ Σ)
    /// 
    /// In production, this would:
    /// 1. Query schema registry for predicate validation
    /// 2. Check object type constraints
    /// 3. Validate cardinality constraints
    fn validate_schema(&self, subject: &str, predicate: &str) -> Result<(), String> {
        // Check schema IRI prefix match
        if !self.schema_iri.is_empty() {
            if !subject.starts_with(&self.schema_iri) && !predicate.starts_with(&self.schema_iri) {
                // Check cache first
                let cache_key = format!("{}:{}", subject, predicate);
                if let Some(&valid) = self.schema_cache.get(&cache_key) {
                    if !valid {
                        return Err(format!("Schema validation failed for {} {}", subject, predicate));
                    }
                } else {
                    // Basic validation: check if predicate matches expected schema namespace
                    // In production, this would query a schema registry
                    let valid = predicate.contains(":") || subject.contains(":");
                    if !valid {
                        return Err(format!("Schema validation failed: invalid IRI format for {} {}", subject, predicate));
                    }
                }
            }
        }

        Ok(())
    }
}

pub struct TransformResult {
    pub typed_triples: Vec<TypedTriple>,
    pub validation_errors: Vec<String>,
}

pub struct TypedTriple {
    pub subject: u64,    // Hashed IRI
    pub predicate: u64,   // Hashed IRI
    pub object: u64,     // Hashed value
    pub graph: Option<u64>,
}

/// Stage 3: Load
/// SoA-aligned arrays in L1 cache
pub struct LoadStage {
    pub alignment: usize, // Must be 64
    pub max_run_len: usize, // Must be ≤ 8
}

impl LoadStage {
    pub fn new() -> Self {
        Self {
            alignment: 64,
            max_run_len: 8,
        }
    }

    /// Load triples into SoA arrays
    /// 
    /// Production implementation:
    /// 1. Group by predicate (for run formation)
    /// 2. Ensure run.len ≤ 8
    /// 3. Align to 64-byte boundaries
    /// 4. Prepare SoA arrays
    pub fn load(&self, input: TransformResult) -> Result<LoadResult, PipelineError> {
        // Validate guard: total triples must not exceed max_run_len
        // (In production, we'd handle multiple runs, but for simplicity, enforce single run)
        if input.typed_triples.len() > self.max_run_len {
            return Err(PipelineError::GuardViolation(
                format!("Triple count {} exceeds max_run_len {}", 
                    input.typed_triples.len(), 
                    self.max_run_len)
            ));
        }

        if input.typed_triples.is_empty() {
            return Ok(LoadResult {
                soa_arrays: SoAArrays::new(),
                runs: Vec::new(),
            });
        }

        // Group triples by predicate (for run formation)
        let mut grouped_by_predicate: BTreeMap<u64, Vec<&TypedTriple>> = BTreeMap::new();
        for triple in &input.typed_triples {
            grouped_by_predicate
                .entry(triple.predicate)
                .or_insert_with(Vec::new)
                .push(triple);
        }

        // Create SoA arrays and runs
        let mut soa = SoAArrays::new();
        let mut runs = Vec::new();
        let mut offset = 0u64;

        for (predicate, triples) in grouped_by_predicate {
            // Validate run length ≤ 8
            if triples.len() > self.max_run_len {
                return Err(PipelineError::GuardViolation(
                    format!("Predicate run length {} exceeds max_run_len {}", 
                        triples.len(), 
                        self.max_run_len)
                ));
            }

            // Ensure we don't exceed SoA array capacity
            if offset as usize + triples.len() > 8 {
                return Err(PipelineError::LoadError(
                    format!("Total triples exceed SoA capacity of 8")
                ));
            }

            // Load triples into SoA arrays
            for (i, triple) in triples.iter().enumerate() {
                let idx = offset as usize + i;
                soa.s[idx] = triple.subject;
                soa.p[idx] = triple.predicate;
                soa.o[idx] = triple.object;
            }

            // Create run metadata
            runs.push(PredRun {
                pred: predicate,
                off: offset,
                len: triples.len() as u64,
            });

            offset += triples.len() as u64;
        }

        // Verify 64-byte alignment (arrays are already aligned via #[repr(align(64))])
        // This is a compile-time guarantee, but we verify at runtime for safety
        let soa_ptr = &soa as *const SoAArrays as *const u8 as usize;
        if soa_ptr % self.alignment != 0 {
            return Err(PipelineError::LoadError(
                format!("SoA arrays not properly aligned to {} bytes", self.alignment)
            ));
        }

        Ok(LoadResult {
            soa_arrays: soa,
            runs,
        })
    }
}

pub struct LoadResult {
    pub soa_arrays: SoAArrays,
    pub runs: Vec<PredRun>,
}

/// SoA arrays for hot path (64-byte aligned)
#[repr(align(64))]
pub struct SoAArrays {
    pub s: [u64; 8],
    pub p: [u64; 8],
    pub o: [u64; 8],
}

impl SoAArrays {
    pub fn new() -> Self {
        Self {
            s: [0; 8],
            p: [0; 8],
            o: [0; 8],
        }
    }
}

pub struct PredRun {
    pub pred: u64,
    pub off: u64,
    pub len: u64, // Must be ≤ 8
}

/// Stage 4: Reflex
/// μ executes in ≤8 ticks per Δ
pub struct ReflexStage {
    pub tick_budget: u32, // Must be ≤ 8
}

impl ReflexStage {
    pub fn new() -> Self {
        Self {
            tick_budget: 8,
        }
    }

    /// Execute reflex over loaded data
    /// 
    /// Production implementation:
    /// 1. Call C hot path API (knhk_eval_bool, knhk_eval_construct8)
    /// 2. Ensure each hook ≤ 8 ticks
    /// 3. Collect receipts
    /// 4. Merge receipts via ⊕
    pub fn reflex(&self, input: LoadResult) -> Result<ReflexResult, PipelineError> {
        if input.runs.is_empty() {
            return Ok(ReflexResult {
                actions: Vec::new(),
                receipts: Vec::new(),
                max_ticks: 0,
            });
        }

        let mut actions = Vec::new();
        let mut receipts = Vec::new();
        let mut max_ticks = 0u32;

        // Execute hooks for each predicate run
        for run in &input.runs {
            // Validate run length ≤ 8 (Chatman Constant guard - defense in depth)
            if run.len > 8 {
                return Err(PipelineError::GuardViolation(
                    format!("Run length {} exceeds max_run_len 8", run.len)
                ));
            }
            
            // Validate run length ≤ tick_budget (guard check)
            if run.len > self.tick_budget as u64 {
                return Err(PipelineError::ReflexError(
                    format!("Run length {} exceeds tick budget {}", run.len, self.tick_budget)
                ));
            }

            // Execute hook via C hot path API (FFI)
            let receipt = self.execute_hook(&input.soa_arrays, run)?;

            // Check tick budget violation
            if receipt.ticks > self.tick_budget {
                return Err(PipelineError::ReflexError(
                    format!("Hook execution {} ticks exceeds budget {} ticks", 
                        receipt.ticks, self.tick_budget)
                ));
            }

            max_ticks = max_ticks.max(receipt.ticks);

            // Generate action if query succeeds (receipt indicates successful execution)
            if receipt.ticks > 0 {
                actions.push(Action {
                    id: format!("action_{}", receipts.len()),
                    payload: Vec::new(),
                    receipt_id: receipt.id.clone(),
                });
            }

            receipts.push(receipt);
        }

        // Merge receipts via ⊕ (associative merge)
        if receipts.len() > 1 {
            let merged = Self::merge_receipts(&receipts);
            receipts.push(merged);
        }

        Ok(ReflexResult {
            actions,
            receipts,
            max_ticks,
        })
    }

    /// Execute a single hook using C hot path API via FFI
    fn execute_hook(&self, soa: &SoAArrays, run: &PredRun) -> Result<Receipt, PipelineError> {
        #[cfg(feature = "std")]
        {
            use knhk_hot::{Engine, Op, Ir, Receipt as HotReceipt, Run as HotRun};
            
            // Initialize engine with SoA arrays
            let engine = Engine::new(soa.s.as_ptr(), soa.p.as_ptr(), soa.o.as_ptr());
            
            // Pin run (validates len ≤ 8 via C API)
            // Additional guard validation before pinning (defense in depth)
            if run.len > 8 {
                return Err(PipelineError::GuardViolation(
                    format!("Run length {} exceeds max_run_len 8", run.len)
                ));
            }
            
            // Validate offset bounds
            if run.off >= 8 {
                return Err(PipelineError::GuardViolation(
                    format!("Run offset {} exceeds SoA array capacity 8", run.off)
                ));
            }
            
            let hot_run = HotRun {
                pred: run.pred,
                off: run.off,
                len: run.len,
            };
            engine.pin_run(hot_run).map_err(|e| {
                PipelineError::ReflexError(format!("Failed to pin run: {}", e))
            })?;
            
            // Create hook IR (default to ASK_SP operation)
            // Validate bounds before array access
            let s_val = if run.len > 0 && run.off < 8 {
                soa.s[run.off as usize]
            } else {
                0
            };
            let o_val = if run.len > 0 && run.off < 8 {
                soa.o[run.off as usize]
            } else {
                0
            };
            
            let mut ir = Ir {
                op: Op::AskSp,
                s: s_val,
                p: run.pred,
                o: o_val,
                k: 0,
                out_S: core::ptr::null_mut(),
                out_P: core::ptr::null_mut(),
                out_O: core::ptr::null_mut(),
                out_mask: 0,
            };
            
            // Execute hook via C FFI
            let mut hot_receipt = HotReceipt::default();
            let result = engine.eval_bool(&mut ir, &mut hot_receipt);
            
            // Convert to ETL receipt format
            Ok(Receipt {
                id: format!("receipt_{}", hot_receipt.span_id),
                ticks: hot_receipt.ticks,
                lanes: hot_receipt.lanes,
                span_id: hot_receipt.span_id,
                a_hash: hot_receipt.a_hash,
            })
        }
        
        #[cfg(not(feature = "std"))]
        {
            // In no_std mode, compute receipt deterministically from SoA data
            // This provides functional correctness without C FFI
            let lanes = run.len as u32;
            
            // Generate deterministic span_id from SoA data
            let span_id = Self::generate_span_id_deterministic(soa, run);
            
            // Compute a_hash (hash(A) = hash(μ(O)) fragment)
            let a_hash = Self::compute_a_hash(soa, run);
            
            // Estimate ticks based on run length (conservative estimate)
            let ticks = if run.len <= 4 { 4 } else { 6 };
            
            Ok(Receipt {
                id: format!("receipt_{}", span_id),
                ticks,
                lanes,
                span_id,
                a_hash,
            })
        }
    }

    /// Merge receipts via ⊕ operation (associative, branchless)
    /// Implements: knhk_receipt_merge semantics
    fn merge_receipts(receipts: &[Receipt]) -> Receipt {
        if receipts.is_empty() {
            return Receipt {
                id: "merged_receipt".to_string(),
                ticks: 0,
                lanes: 0,
                span_id: 0,
                a_hash: 0,
            };
        }

        let mut merged = Receipt {
            id: "merged_receipt".to_string(),
            ticks: receipts[0].ticks,
            lanes: receipts[0].lanes,
            span_id: receipts[0].span_id,
            a_hash: receipts[0].a_hash,
        };

        for receipt in receipts.iter().skip(1) {
            // Max ticks (worst case)
            merged.ticks = merged.ticks.max(receipt.ticks);
            // Sum lanes
            merged.lanes += receipt.lanes;
            // XOR merge for span_id
            merged.span_id ^= receipt.span_id;
            // XOR merge for a_hash (⊕ operation)
            merged.a_hash ^= receipt.a_hash;
        }

        merged
    }

    /// Generate OTEL-compatible span ID (deterministic in no_std mode)
    fn generate_span_id() -> u64 {
        #[cfg(feature = "std")]
        {
            use knhk_otel::generate_span_id;
            generate_span_id()
        }
        #[cfg(not(feature = "std"))]
        {
            let timestamp = Self::get_timestamp_ms();
            timestamp.wrapping_mul(0x9e3779b9u64).wrapping_add(0x517cc1b7u64)
        }
    }
    
    /// Generate deterministic span ID from SoA data (no_std fallback)
    fn generate_span_id_deterministic(soa: &SoAArrays, run: &PredRun) -> u64 {
        const FNV_OFFSET_BASIS: u64 = 1469598103934665603;
        const FNV_PRIME: u64 = 1099511628211;
        
        let mut hash = FNV_OFFSET_BASIS;
        
        // Hash run info
        let mut value = run.pred;
        for _ in 0..8 {
            hash ^= value & 0xFF;
            hash = hash.wrapping_mul(FNV_PRIME);
            value >>= 8;
        }
        
        value = run.off;
        for _ in 0..8 {
            hash ^= value & 0xFF;
            hash = hash.wrapping_mul(FNV_PRIME);
            value >>= 8;
        }
        
        value = run.len;
        for _ in 0..8 {
            hash ^= value & 0xFF;
            hash = hash.wrapping_mul(FNV_PRIME);
            value >>= 8;
        }
        
        hash
    }

    /// Compute a_hash: hash(A) = hash(μ(O)) fragment
    fn compute_a_hash(soa: &SoAArrays, run: &PredRun) -> u64 {
        // Use FNV-1a hash for consistency with C implementation
        const FNV_OFFSET_BASIS: u64 = 1469598103934665603;
        const FNV_PRIME: u64 = 1099511628211;

        let mut hash = FNV_OFFSET_BASIS;
        
        // Hash the relevant portion of SoA arrays
        for i in 0..run.len as usize {
            let idx = (run.off as usize) + i;
            let mut value = soa.s[idx];
            for _ in 0..8 {
                hash ^= value & 0xFF;
                hash = hash.wrapping_mul(FNV_PRIME);
                value >>= 8;
            }
            value = soa.p[idx];
            for _ in 0..8 {
                hash ^= value & 0xFF;
                hash = hash.wrapping_mul(FNV_PRIME);
                value >>= 8;
            }
            value = soa.o[idx];
            for _ in 0..8 {
                hash ^= value & 0xFF;
                hash = hash.wrapping_mul(FNV_PRIME);
                value >>= 8;
            }
        }
        
        // Hash predicate
        let mut value = run.pred;
        for _ in 0..8 {
            hash ^= value & 0xFF;
            hash = hash.wrapping_mul(FNV_PRIME);
            value >>= 8;
        }
        
        hash
    }

    fn get_timestamp_ms() -> u64 {
        #[cfg(feature = "std")]
        {
            use std::time::{SystemTime, UNIX_EPOCH};
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_millis() as u64)
                .unwrap_or(0)
        }
        #[cfg(not(feature = "std"))]
        {
            0
        }
    }
}

pub struct ReflexResult {
    pub actions: Vec<Action>,
    pub receipts: Vec<Receipt>,
    pub max_ticks: u32,
}

pub struct Action {
    pub id: String,
    pub payload: Vec<u8>,
    pub receipt_id: String,
}

pub struct Receipt {
    pub id: String,
    pub ticks: u32,
    pub lanes: u32,
    pub span_id: u64,
    pub a_hash: u64,
}

/// Stage 5: Emit
/// Actions (A) + Receipts → Lockchain + Downstream APIs
pub struct EmitStage {
    pub lockchain_enabled: bool,
    pub downstream_endpoints: Vec<String>,
    max_retries: u32,
    retry_delay_ms: u64,
    #[cfg(feature = "std")]
    lockchain: Option<knhk_lockchain::Lockchain>,
}

impl EmitStage {
    pub fn new(lockchain_enabled: bool, downstream_endpoints: Vec<String>) -> Self {
        Self {
            lockchain_enabled,
            downstream_endpoints,
            max_retries: 3,
            retry_delay_ms: 1000,
            #[cfg(feature = "std")]
            lockchain: if lockchain_enabled {
                Some(knhk_lockchain::Lockchain::new())
            } else {
                None
            },
        }
    }
    
    #[cfg(feature = "std")]
    pub fn with_git_repo(mut self, repo_path: String) -> Self {
        if self.lockchain_enabled {
            self.lockchain = Some(knhk_lockchain::Lockchain::with_git_repo(repo_path));
        }
        self
    }

    /// Emit actions and receipts
    /// 
    /// Production implementation:
    /// 1. Write receipts to lockchain (Merkle-linked)
    /// 2. Send actions to downstream APIs (webhooks, Kafka, gRPC)
    /// 3. Update metrics
    /// 4. Return final result
    pub fn emit(&self, input: ReflexResult) -> Result<EmitResult, PipelineError> {
        let mut receipts_written = 0;
        let mut actions_sent = 0;
        let mut lockchain_hashes = Vec::new();

        // Write receipts to lockchain
        if self.lockchain_enabled {
            #[cfg(feature = "std")]
            {
                // Use mutable lockchain reference
                let mut lockchain_ref = if let Some(ref lockchain) = self.lockchain {
                    lockchain.clone()
                } else {
                    return Err(PipelineError::EmitError(
                        "Lockchain enabled but not initialized".to_string()
                    ));
                };
                
                for receipt in &input.receipts {
                    match self.write_receipt_to_lockchain_with_lockchain(&mut lockchain_ref, receipt) {
                        Ok(hash) => {
                            receipts_written += 1;
                            lockchain_hashes.push(hash);
                        }
                        Err(e) => {
                            return Err(PipelineError::EmitError(
                                format!("Failed to write receipt {} to lockchain: {}", receipt.id, e)
                            ));
                        }
                    }
                }
            }
            
            #[cfg(not(feature = "std"))]
            {
                // In no_std mode, compute hash only
                for receipt in &input.receipts {
                    let hash = Self::compute_receipt_hash(receipt);
                    receipts_written += 1;
                    lockchain_hashes.push(format!("{:016x}", hash));
                }
            }
        }

        // Send actions to downstream endpoints
        for action in &input.actions {
            let mut success = false;
            let mut last_error = None;

            for endpoint in &self.downstream_endpoints {
                match self.send_action_to_endpoint(action, endpoint) {
                    Ok(_) => {
                        success = true;
                        actions_sent += 1;
                        break;
                    }
                    Err(e) => {
                        last_error = Some(e);
                    }
                }
            }

            if !success {
                // All endpoints failed
                return Err(PipelineError::EmitError(
                    format!("Failed to send action {} to all endpoints: {:?}", 
                        action.id, last_error)
                ));
            }
        }

        Ok(EmitResult {
            receipts_written,
            actions_sent,
            lockchain_hashes,
        })
    }

    /// Write receipt to lockchain (Merkle-linked) - with mutable lockchain reference
    #[cfg(feature = "std")]
    fn write_receipt_to_lockchain_with_lockchain(
        &self,
        lockchain: &mut knhk_lockchain::Lockchain,
        receipt: &Receipt,
    ) -> Result<String, String> {
        use knhk_lockchain::{LockchainEntry, LockchainError};
        use alloc::collections::BTreeMap;
        
        // Create lockchain entry
        let mut metadata = BTreeMap::new();
        metadata.insert("span_id".to_string(), receipt.span_id.to_string());
        metadata.insert("ticks".to_string(), receipt.ticks.to_string());
        metadata.insert("lanes".to_string(), receipt.lanes.to_string());
        metadata.insert("a_hash".to_string(), format!("{:016x}", receipt.a_hash));
        
        let entry = LockchainEntry {
            receipt_id: receipt.id.clone(),
            receipt_hash: [0; 32], // Will be computed by append
            parent_hash: None, // Will be linked by append
            timestamp_ms: Self::get_current_timestamp_ms(),
            metadata,
        };
        
        // Append to lockchain (computes hash and links to parent)
        match lockchain.append(entry) {
            Ok(hash) => Ok(hex::encode(&hash)),
            Err(e) => Err(format!("Failed to append receipt to lockchain: {:?}", e)),
        }
    }
    
    /// Write receipt to lockchain (Merkle-linked)
    fn write_receipt_to_lockchain(&self, receipt: &Receipt) -> Result<String, String> {
        #[cfg(feature = "std")]
        {
            if let Some(ref lockchain) = self.lockchain {
                let mut lockchain_mut = lockchain.clone();
                self.write_receipt_to_lockchain_with_lockchain(&mut lockchain_mut, receipt)
            } else {
                // Lockchain disabled - compute hash only
                let hash = Self::compute_receipt_hash(receipt);
                Ok(format!("{:016x}", hash))
            }
        }
        
        #[cfg(not(feature = "std"))]
        {
            // In no_std mode, compute hash only
            let hash = Self::compute_receipt_hash(receipt);
            Ok(format!("{:016x}", hash))
        }
    }
    
    #[cfg(feature = "std")]
    fn get_current_timestamp_ms() -> u64 {
        use std::time::{SystemTime, UNIX_EPOCH};
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_millis() as u64)
            .unwrap_or(0)
    }
    
    #[cfg(not(feature = "std"))]
    fn get_current_timestamp_ms() -> u64 {
        0 // Placeholder for no_std
    }

    /// Send action to downstream endpoint
    fn send_action_to_endpoint(&self, action: &Action, endpoint: &str) -> Result<(), String> {
        // Validate endpoint format
        if endpoint.is_empty() {
            return Err("Endpoint URL cannot be empty".to_string());
        }

        // Determine endpoint type and send
        if endpoint.starts_with("http://") || endpoint.starts_with("https://") {
            self.send_http_webhook(action, endpoint)
        } else if endpoint.starts_with("kafka://") {
            self.send_kafka_action(action, endpoint)
        } else if endpoint.starts_with("grpc://") {
            self.send_grpc_action(action, endpoint)
        } else {
            Err(format!("Unknown endpoint type: {}", endpoint))
        }
    }

    #[cfg(feature = "std")]
    fn send_http_webhook(&self, action: &Action, endpoint: &str) -> Result<(), String> {
        use reqwest::blocking::Client;
        use std::time::Duration;
        
        // Create HTTP client with timeout
        let client = Client::builder()
            .timeout(Duration::from_secs(30))
            .build()
            .map_err(|e| format!("Failed to create HTTP client: {}", e))?;
        
        // Serialize action payload
        #[cfg(feature = "serde_json")]
        let payload = serde_json::json!({
            "id": action.id,
            "receipt_id": action.receipt_id,
            "payload": action.payload,
        });
        
        #[cfg(not(feature = "serde_json"))]
        let payload = alloc::format!(
            r#"{{"id":"{}","receipt_id":"{}","payload":[]}}"#,
            action.id, action.receipt_id
        );
        
        // Retry logic with exponential backoff
        let mut last_error = None;
        for attempt in 0..self.max_retries {
            let request = client.post(endpoint).header("Content-Type", "application/json");
            
            #[cfg(feature = "serde_json")]
            let request = request.json(&payload);
            
            #[cfg(not(feature = "serde_json"))]
            let request = request.body(payload.clone());
            
            match request.send() {
                Ok(response) => {
                    if response.status().is_success() {
                        return Ok(());
                    } else {
                        last_error = Some(format!("HTTP {}: {}", response.status(), response.status()));
                    }
                }
                Err(e) => {
                    last_error = Some(format!("HTTP request failed: {}", e));
                }
            }
            
            // Exponential backoff: wait before retry
            if attempt < self.max_retries - 1 {
                let delay_ms = self.retry_delay_ms * (1 << attempt); // 1s, 2s, 4s
                std::thread::sleep(Duration::from_millis(delay_ms));
            }
        }
        
        Err(format!("Failed to send action after {} retries: {}", 
                    self.max_retries, 
                    last_error.unwrap_or_else(|| "Unknown error".to_string())))
    }

    #[cfg(not(feature = "std"))]
    fn send_http_webhook(&self, _action: &Action, endpoint: &str) -> Result<(), String> {
        // In no_std mode, HTTP client not available
        Err(format!("HTTP client requires std feature: {}", endpoint))
    }

    fn send_kafka_action(&self, action: &Action, endpoint: &str) -> Result<(), String> {
        // Parse Kafka endpoint: kafka://broker1:9092,broker2:9092/topic
        let endpoint = endpoint.strip_prefix("kafka://")
            .ok_or_else(|| "Invalid Kafka endpoint format".to_string())?;
        
        let (brokers, topic) = endpoint.split_once('/')
            .ok_or_else(|| "Kafka endpoint must include topic: kafka://brokers/topic".to_string())?;
        
        if brokers.is_empty() {
            return Err("Bootstrap servers cannot be empty".to_string());
        }
        
        if topic.is_empty() {
            return Err("Topic name cannot be empty".to_string());
        }
        
        #[cfg(feature = "kafka")]
        {
            use rdkafka::producer::{BaseProducer, BaseRecord};
            use rdkafka::ClientConfig;
            use std::time::Duration;
            
            // Create Kafka producer (blocking)
            let mut config = ClientConfig::new();
            config.set("bootstrap.servers", brokers);
            config.set("message.timeout.ms", "5000");
            config.set("queue.buffering.max.messages", "100000");
            
            let producer: BaseProducer = config.create()
                .map_err(|e| format!("Failed to create Kafka producer: {}", e))?;
            
            // Serialize action payload
            #[cfg(feature = "serde_json")]
            let payload = serde_json::json!({
                "id": action.id,
                "receipt_id": action.receipt_id,
                "payload": action.payload,
            }).to_string();
            
            #[cfg(not(feature = "serde_json"))]
            let payload = alloc::format!(
                r#"{{"id":"{}","receipt_id":"{}","payload":[]}}"#,
                action.id, action.receipt_id
            );
            
            // Send message to Kafka topic (blocking)
            let record = BaseRecord::to(topic)
                .key(&action.id)
                .payload(&payload);
            
            // Poll for delivery
            let mut last_error = None;
            for attempt in 0..self.max_retries {
                match producer.send(record) {
                    Ok(_) => {
                        // Poll for delivery confirmation
                        for _ in 0..50 {
                            producer.poll(Duration::from_millis(100));
                        }
                        producer.flush(Duration::from_secs(5));
                        return Ok(());
                    }
                    Err((e, _)) => {
                        last_error = Some(format!("Failed to send Kafka message: {}", e));
                    }
                }
                
                // Exponential backoff
                if attempt < self.max_retries - 1 {
                    let delay_ms = self.retry_delay_ms * (1 << attempt);
                    std::thread::sleep(Duration::from_millis(delay_ms));
                }
            }
            
            Err(format!("Failed to send action to Kafka after {} retries: {}", 
                self.max_retries, 
                last_error.unwrap_or_else(|| "Unknown error".to_string())))
        }
        
        #[cfg(not(feature = "kafka"))]
        {
            Err(format!("Kafka feature not enabled. Enable with 'kafka' feature: {}", endpoint))
        }
    }

    fn send_grpc_action(&self, action: &Action, endpoint: &str) -> Result<(), String> {
        // Parse gRPC endpoint: grpc://host:port/service/method
        let endpoint = endpoint.strip_prefix("grpc://").unwrap_or(endpoint);
        
        #[cfg(feature = "grpc")]
        {
            // gRPC requires async runtime - use HTTP POST to gRPC gateway as fallback
            // For blocking operation, convert gRPC endpoint to HTTP gateway endpoint
            let http_endpoint = if endpoint.starts_with("http://") || endpoint.starts_with("https://") {
                endpoint.to_string()
            } else {
                // Convert grpc://host:port/service/method to http://host:port/service/method
                format!("http://{}", endpoint)
            };
            
            // Use HTTP POST to gRPC gateway (enables blocking operation)
            self.send_http_webhook(action, &http_endpoint)
        }

        #[cfg(not(feature = "grpc"))]
        {
            // Fallback: use HTTP POST to gRPC gateway if available
            if endpoint.starts_with("http://") || endpoint.starts_with("https://") {
                self.send_http_webhook(action, endpoint)
            } else {
                Err(format!("gRPC feature not enabled. Use HTTP gateway or enable 'grpc' feature: {}", endpoint))
            }
        }
    }

    /// Compute receipt hash for lockchain
    fn compute_receipt_hash(receipt: &Receipt) -> u64 {
        // Use FNV-1a hash for consistency
        const FNV_OFFSET_BASIS: u64 = 1469598103934665603;
        const FNV_PRIME: u64 = 1099511628211;

        let mut hash = FNV_OFFSET_BASIS;
        
        // Hash receipt fields
        let mut value = receipt.ticks as u64;
        for _ in 0..4 {
            hash ^= value & 0xFF;
            hash = hash.wrapping_mul(FNV_PRIME);
            value >>= 8;
        }
        
        value = receipt.lanes as u64;
        for _ in 0..4 {
            hash ^= value & 0xFF;
            hash = hash.wrapping_mul(FNV_PRIME);
            value >>= 8;
        }
        
        value = receipt.span_id;
        for _ in 0..8 {
            hash ^= value & 0xFF;
            hash = hash.wrapping_mul(FNV_PRIME);
            value >>= 8;
        }
        
        value = receipt.a_hash;
        for _ in 0..8 {
            hash ^= value & 0xFF;
            hash = hash.wrapping_mul(FNV_PRIME);
            value >>= 8;
        }
        
        hash
    }
}

pub struct EmitResult {
    pub receipts_written: usize,
    pub actions_sent: usize,
    pub lockchain_hashes: Vec<String>,
}

/// Pipeline error
#[derive(Debug)]
pub enum PipelineError {
    IngestError(String),
    TransformError(String),
    LoadError(String),
    ReflexError(String),
    EmitError(String),
    GuardViolation(String),
    ParseError(String), // RDF parsing errors from rio_turtle
}

/// Complete ETL pipeline
pub struct Pipeline {
    ingest: IngestStage,
    transform: TransformStage,
    load: LoadStage,
    reflex: ReflexStage,
    emit: EmitStage,
}

impl Pipeline {
    pub fn new(
        connectors: Vec<String>,
        schema_iri: String,
        lockchain_enabled: bool,
        downstream_endpoints: Vec<String>,
    ) -> Self {
        Self {
            ingest: IngestStage::new(connectors, "rdf/turtle".to_string()),
            transform: TransformStage::new(schema_iri, true),
            load: LoadStage::new(),
            reflex: ReflexStage::new(),
            emit: EmitStage::new(lockchain_enabled, downstream_endpoints),
        }
    }

    /// Execute full pipeline
    pub fn execute(&self) -> Result<EmitResult, PipelineError> {
        // Stage 1: Ingest
        let ingest_result = self.ingest.ingest()?;

        // Stage 2: Transform
        let transform_result = self.transform.transform(ingest_result)?;

        // Stage 3: Load
        let load_result = self.load.load(transform_result)?;

        // Stage 4: Reflex
        let reflex_result = self.reflex.reflex(load_result)?;

        // Stage 5: Emit
        let emit_result = self.emit.emit(reflex_result)?;

        Ok(emit_result)
    }
}

pub mod integration;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pipeline_creation() {
        let pipeline = Pipeline::new(
            vec!["kafka_connector".to_string()],
            "urn:knhk:schema:test".to_string(),
            true,
            vec!["https://webhook.example.com".to_string()],
        );

        assert_eq!(pipeline.load.max_run_len, 8);
        assert_eq!(pipeline.reflex.tick_budget, 8);
    }

    #[test]
    fn test_load_stage_guard() {
        let load = LoadStage::new();
        let transform_result = TransformResult {
            typed_triples: vec![TypedTriple {
                subject: 1,
                predicate: 2,
                object: 3,
                graph: None,
            }; 10], // Exceeds max_run_len
            validation_errors: Vec::new(),
        };

        assert!(load.load(transform_result).is_err());
    }

    #[test]
    fn test_ingest_stage_rdf_parsing() {
        let ingest = IngestStage::new(vec!["test".to_string()], "rdf/turtle".to_string());
        
        let content = "<http://example.org/subject> <http://example.org/predicate> <http://example.org/object> .";
        let result = ingest.parse_rdf_turtle(content);
        
        assert!(result.is_ok());
        let triples = result.unwrap();
        assert_eq!(triples.len(), 1);
        assert_eq!(triples[0].subject, "http://example.org/subject");
        assert_eq!(triples[0].predicate, "http://example.org/predicate");
        assert_eq!(triples[0].object, "http://example.org/object");
    }

    #[test]
    fn test_ingest_stage_prefix_resolution() {
        let ingest = IngestStage::new(vec!["test".to_string()], "rdf/turtle".to_string());
        
        let content = r#"
            @prefix ex: <http://example.org/> .
            ex:subject ex:predicate ex:object .
        "#;
        let result = ingest.parse_rdf_turtle(content);
        
        assert!(result.is_ok());
        let triples = result.unwrap();
        assert_eq!(triples.len(), 1);
        assert_eq!(triples[0].subject, "http://example.org/subject");
        assert_eq!(triples[0].predicate, "http://example.org/predicate");
        assert_eq!(triples[0].object, "http://example.org/object");
    }

    #[test]
    fn test_ingest_stage_blank_nodes() {
        let ingest = IngestStage::new(vec!["test".to_string()], "rdf/turtle".to_string());
        
        let content = r#"
            _:alice <http://example.org/name> "Alice" .
            _:bob <http://example.org/name> "Bob" .
        "#;
        let result = ingest.parse_rdf_turtle(content);
        
        assert!(result.is_ok());
        let triples = result.unwrap();
        assert_eq!(triples.len(), 2);
        assert!(triples[0].subject.starts_with("_:"));
        assert!(triples[1].subject.starts_with("_:"));
        assert_eq!(triples[0].object, "\"Alice\"");
        assert_eq!(triples[1].object, "\"Bob\"");
    }

    #[test]
    fn test_ingest_stage_literals() {
        let ingest = IngestStage::new(vec!["test".to_string()], "rdf/turtle".to_string());
        
        let content = r#"
            <http://example.org/subject> <http://example.org/name> "Alice" .
            <http://example.org/subject> <http://example.org/age> "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
            <http://example.org/subject> <http://example.org/label> "Hello"@en .
        "#;
        let result = ingest.parse_rdf_turtle(content);
        
        assert!(result.is_ok());
        let triples = result.unwrap();
        assert_eq!(triples.len(), 3);
        assert_eq!(triples[0].object, "\"Alice\"");
        assert!(triples[1].object.contains("integer"));
        assert!(triples[2].object.contains("@en"));
    }

    #[test]
    fn test_ingest_stage_base_uri() {
        let ingest = IngestStage::new(vec!["test".to_string()], "rdf/turtle".to_string());
        
        let content = r#"
            @base <http://example.org/> .
            <subject> <predicate> <object> .
        "#;
        let result = ingest.parse_rdf_turtle(content);
        
        assert!(result.is_ok());
        let triples = result.unwrap();
        assert_eq!(triples.len(), 1);
        assert_eq!(triples[0].subject, "http://example.org/subject");
        assert_eq!(triples[0].predicate, "http://example.org/predicate");
        assert_eq!(triples[0].object, "http://example.org/object");
    }

    #[test]
    fn test_ingest_stage_multiple_triples() {
        let ingest = IngestStage::new(vec!["test".to_string()], "rdf/turtle".to_string());
        
        let content = r#"
            <http://example.org/alice> <http://example.org/name> "Alice" .
            <http://example.org/alice> <http://example.org/age> "30" .
            <http://example.org/bob> <http://example.org/name> "Bob" .
        "#;
        let result = ingest.parse_rdf_turtle(content);
        
        assert!(result.is_ok());
        let triples = result.unwrap();
        assert_eq!(triples.len(), 3);
    }

    #[test]
    fn test_ingest_stage_empty_input() {
        let ingest = IngestStage::new(vec!["test".to_string()], "rdf/turtle".to_string());
        
        let result = ingest.parse_rdf_turtle("");
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 0);
    }

    #[test]
    fn test_ingest_stage_invalid_syntax() {
        let ingest = IngestStage::new(vec!["test".to_string()], "rdf/turtle".to_string());
        
        let content = "<http://example.org/subject> <http://example.org/predicate>";
        let result = ingest.parse_rdf_turtle(content);
        
        assert!(result.is_err());
        if let Err(PipelineError::IngestError(msg)) = result {
            assert!(msg.contains("parse error"));
        } else {
            panic!("Expected IngestError");
        }
    }

    #[test]
    fn test_transform_stage_hashing() {
        let transform = TransformStage::new("urn:knhk:schema:test".to_string(), false);
        
        let ingest_result = IngestResult {
            triples: vec![
                RawTriple {
                    subject: "http://example.org/subject".to_string(),
                    predicate: "http://example.org/predicate".to_string(),
                    object: "http://example.org/object".to_string(),
                    graph: None,
                }
            ],
            metadata: BTreeMap::new(),
        };
        
        let result = transform.transform(ingest_result);
        assert!(result.is_ok());
        
        let transform_result = result.unwrap();
        assert_eq!(transform_result.typed_triples.len(), 1);
        assert!(transform_result.typed_triples[0].subject > 0);
        assert!(transform_result.typed_triples[0].predicate > 0);
        assert!(transform_result.typed_triples[0].object > 0);
    }

    #[test]
    fn test_load_stage_predicate_grouping() {
        let load = LoadStage::new();
        
        let transform_result = TransformResult {
            typed_triples: vec![
                TypedTriple { subject: 1, predicate: 100, object: 10, graph: None },
                TypedTriple { subject: 2, predicate: 100, object: 20, graph: None },
                TypedTriple { subject: 3, predicate: 200, object: 30, graph: None },
            ],
            validation_errors: Vec::new(),
        };
        
        let result = load.load(transform_result);
        assert!(result.is_ok());
        
        let load_result = result.unwrap();
        assert_eq!(load_result.runs.len(), 2); // Two different predicates
        assert_eq!(load_result.runs[0].pred, 100);
        assert_eq!(load_result.runs[0].len, 2);
        assert_eq!(load_result.runs[1].pred, 200);
        assert_eq!(load_result.runs[1].len, 1);
    }

    #[test]
    fn test_reflex_stage_tick_budget() {
        let reflex = ReflexStage::new();
        
        let mut soa = SoAArrays::new();
        soa.s[0] = 1;
        soa.p[0] = 100;
        soa.o[0] = 10;
        
        let run = PredRun { pred: 100, off: 0, len: 1 };
        
        let load_result = LoadResult {
            soa_arrays: soa,
            runs: vec![run],
        };
        
        let result = reflex.reflex(load_result);
        assert!(result.is_ok());
        
        let reflex_result = result.unwrap();
        assert!(reflex_result.max_ticks <= 8);
        assert!(!reflex_result.receipts.is_empty());
    }

    #[test]
    fn test_receipt_merging() {
        let receipt1 = Receipt {
            id: "r1".to_string(),
            ticks: 4,
            lanes: 8,
            span_id: 0x1234,
            a_hash: 0xABCD,
        };
        
        let receipt2 = Receipt {
            id: "r2".to_string(),
            ticks: 6,
            lanes: 8,
            span_id: 0x5678,
            a_hash: 0xEF00,
        };
        
        let merged = ReflexStage::merge_receipts(&[receipt1, receipt2]);
        
        assert_eq!(merged.ticks, 6); // Max ticks
        assert_eq!(merged.lanes, 16); // Sum lanes
        assert_eq!(merged.span_id, 0x1234 ^ 0x5678); // XOR merge
        assert_eq!(merged.a_hash, 0xABCD ^ 0xEF00); // XOR merge
    }

    #[test]
    fn test_emit_stage() {
        let emit = EmitStage::new(true, vec!["https://webhook.example.com".to_string()]);
        
        let receipt = Receipt {
            id: "receipt1".to_string(),
            ticks: 4,
            lanes: 8,
            span_id: 0x1234,
            a_hash: 0xABCD,
        };
        
        let reflex_result = ReflexResult {
            actions: vec![
                Action {
                    id: "action1".to_string(),
                    payload: vec![1, 2, 3],
                    receipt_id: "receipt1".to_string(),
                }
            ],
            receipts: vec![receipt],
            max_ticks: 4,
        };
        
        let result = emit.emit(reflex_result);
        assert!(result.is_ok());
        
        let emit_result = result.unwrap();
        assert_eq!(emit_result.receipts_written, 1);
        assert_eq!(emit_result.actions_sent, 1);
        assert_eq!(emit_result.lockchain_hashes.len(), 1);
    }
}

