//! Global channel state: sinks, diagnostic queue, domain prefix, and run-id.
// Mutex poison recovery via `unwrap_or_else(|e| e.into_inner())` is intentional: we
// always recover from a poisoned lock rather than propagating the panic.
#![allow(clippy::unwrap_used)]
#![allow(
    clippy::redundant_closure,
    clippy::redundant_closure_for_method_calls,
    clippy::must_use_candidate,
    clippy::missing_errors_doc,
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation,
    clippy::unnecessary_wraps,
    clippy::significant_drop_tightening,
    clippy::items_after_statements,
    clippy::missing_docs_in_private_items,
    clippy::missing_const_for_fn,
    clippy::use_self,
    clippy::unnecessary_literal_bound
)]
use super::{
    Diagnostic, DiagnosticCategory, DiagnosticSink, MergeStrategy, RunId, SectorStack, Severity,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};
use std::time::Instant;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RunSummary {
    pub run_id: RunId,
    pub evaluated: usize,
    pub admitted: usize,
    pub p_admitted: f64,
    pub andon_count: usize,
    pub warning_count: usize,
    pub dominant_category: Option<DiagnosticCategory>,
    pub total_diagnostics: usize,
    pub category_counts: HashMap<DiagnosticCategory, usize>,
}

impl Default for RunSummary {
    fn default() -> Self {
        Self {
            run_id: String::new(),
            evaluated: 0,
            admitted: 0,
            p_admitted: 0.0,
            andon_count: 0,
            warning_count: 0,
            dominant_category: None,
            total_diagnostics: 0,
            category_counts: HashMap::new(),
        }
    }
}

struct GlobalChannelState {
    sinks: Vec<Box<dyn DiagnosticSink>>,
    queue: Vec<Diagnostic>,
    closed: bool,
    domain: String,
    current_run_id: RunId,
    start_time: Instant,
    sector_stacks: Vec<Box<dyn SectorStack>>,
    merge_strategy: MergeStrategy,
    capacity: Option<usize>,
}

static STATE: OnceLock<Mutex<GlobalChannelState>> = OnceLock::new();

fn get_state() -> &'static Mutex<GlobalChannelState> {
    STATE.get_or_init(|| {
        Mutex::new(GlobalChannelState {
            sinks: Vec::new(),
            queue: Vec::new(),
            closed: false,
            domain: "CORE".to_string(),
            current_run_id: "default-run".to_string(),
            start_time: Instant::now(),
            sector_stacks: Vec::new(),
            merge_strategy: MergeStrategy::Strict,
            capacity: None,
        })
    })
}

pub fn register_sink(sink: Box<dyn DiagnosticSink>) {
    let mut state = get_state().lock().unwrap_or_else(|e| e.into_inner());
    state.sinks.push(sink);
}

pub fn register_domain(prefix: &str) {
    let mut state = get_state().lock().unwrap_or_else(|e| e.into_inner());
    state.domain = prefix.to_string();
}

pub fn set_run_id(run_id: RunId) {
    let mut state = get_state().lock().unwrap_or_else(|e| e.into_inner());
    state.current_run_id = run_id;
}

pub fn get_domain() -> String {
    let state = get_state().lock().unwrap_or_else(|e| e.into_inner());
    state.domain.clone()
}

pub fn get_run_id() -> String {
    let state = get_state().lock().unwrap_or_else(|e| e.into_inner());
    state.current_run_id.clone()
}

pub fn emit_diagnostic(diagnostic: &Diagnostic) {
    let mut state = get_state().lock().unwrap_or_else(|e| e.into_inner());
    if !state.closed {
        state.queue.push(diagnostic.clone());
    }
}

pub fn register_sector_stack(stack: Box<dyn SectorStack>, strategy: MergeStrategy) {
    let mut state = get_state().lock().unwrap_or_else(|e| e.into_inner());
    state.sector_stacks.push(stack);
    state.merge_strategy = strategy;
}

pub fn close_channel() -> Result<RunSummary, String> {
    let (diagnostics, sinks, run_id) = {
        let mut state = get_state().lock().unwrap_or_else(|e| e.into_inner());
        state.closed = true;
        let queue = std::mem::take(&mut state.queue);
        let sinks = std::mem::take(&mut state.sinks);
        let run_id = state.current_run_id.clone();
        state.closed = false;
        (queue, sinks, run_id)
    };

    let mut andon_count = 0;
    let mut warning_count = 0;
    let mut andon_category_counts = HashMap::new();
    let mut category_counts = HashMap::new();

    for diag in &diagnostics {
        *category_counts.entry(diag.code.category).or_insert(0) += 1;
        match diag.severity {
            Severity::Andon => {
                andon_count += 1;
                *andon_category_counts.entry(diag.code.category).or_insert(0) += 1;
            }
            Severity::Warning => warning_count += 1,
            Severity::Info => {}
        }
    }

    let evaluated = diagnostics.len();
    let admitted = evaluated.saturating_sub(andon_count);
    let p_admitted = if evaluated > 0 { admitted as f64 / evaluated as f64 } else { 1.0 };

    let summary = RunSummary {
        run_id,
        evaluated,
        admitted,
        p_admitted,
        andon_count,
        warning_count,
        dominant_category: andon_category_counts
            .into_iter()
            .max_by_key(|&(_, count)| count)
            .map(|(cat, _)| cat),
        total_diagnostics: evaluated,
        category_counts,
    };

    for sink in &sinks {
        for diag in &diagnostics {
            let _ = sink.emit(diag.clone());
        }
        let _ = sink.close(summary.clone());
    }

    Ok(summary)
}

pub fn on_test_started(test_name: &str) {
    let diag = Diagnostic {
        code: crate::core::governance::DiagnosticCode::new(
            get_domain(),
            DiagnosticCategory::Conformance,
            900,
        ),
        category: DiagnosticCategory::Conformance,
        run_id: get_run_id(),
        agent_id: None,
        location: None,
        message: format!("Test started: {test_name}"),
        severity: Severity::Info,
        source_module: "governance::lifecycle",
        context: HashMap::new(),
        elapsed_ns: 0,
    };
    emit_diagnostic(&diag);
}

pub fn on_test_completed(test_name: &str, passed: bool) {
    let mut context = HashMap::new();
    let _ = context.insert("passed", serde_json::json!(passed));
    let diag = Diagnostic {
        code: crate::core::governance::DiagnosticCode::new(
            get_domain(),
            DiagnosticCategory::Conformance,
            901,
        ),
        category: DiagnosticCategory::Conformance,
        run_id: get_run_id(),
        agent_id: None,
        location: None,
        message: format!("Test completed: {test_name} (passed: {passed})"),
        severity: Severity::Info,
        source_module: "governance::lifecycle",
        context,
        elapsed_ns: 0,
    };
    emit_diagnostic(&diag);
}

pub fn set_channel_capacity(capacity: Option<usize>) {
    let mut state = get_state().lock().unwrap_or_else(|e| e.into_inner());
    log::info!(
        "governance::channel: setting capacity to {:?} (previous: {:?})",
        capacity,
        state.capacity
    );
    state.capacity = capacity;
}
