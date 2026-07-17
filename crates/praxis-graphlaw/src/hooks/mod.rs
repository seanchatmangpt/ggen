// Core hook types, constants, and module declarations for v26.7.8
// Organized into 11 focused submodules: parsing, condition, compile, construct, etc.

use crate::encoding::Encoder;
use crate::fastmap::FxHashMap;
use crate::term::{Triple, VarOrTerm};
use crate::TripleStore;
use serde::{Deserialize, Serialize};

pub const KH_NS: &str = "http://seanchatmangpt.github.io/praxis/kh#";
pub const HOOK_ALIAS_NS: &str = "http://seanchatmangpt.github.io/praxis/hook#";

pub const SHACL_LAW_PACK: &str = r#"
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

kh:HookShape a sh:NodeShape ;
    sh:targetClass kh:Hook ;
    sh:closed true ;
    sh:ignoredProperties ( rdf:type ) ;
    sh:property [
        sh:path kh:name ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:on ;
        sh:datatype xsd:string ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:kind ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:var ;
        sh:datatype xsd:string ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:op ;
        sh:datatype xsd:string ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:k ;
        sh:datatype xsd:integer ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:window ;
        sh:datatype xsd:integer ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:program ;
        sh:datatype xsd:string ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:goal ;
        sh:datatype xsd:string ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:query ;
        sh:datatype xsd:string ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:effect ;
        sh:datatype xsd:string ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:action ;
        sh:nodeKind sh:IRI ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:reason ;
        sh:datatype xsd:string ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:priority ;
        sh:datatype xsd:integer ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:after ;
        sh:nodeKind sh:IRI ;
    ] .

kh:ActionShape a sh:NodeShape ;
    sh:targetClass kh:Action ;
    sh:closed true ;
    sh:ignoredProperties ( rdf:type ) ;
    sh:property [
        sh:path kh:handler ;
        sh:nodeKind sh:IRI ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
    ] ;
    sh:property [
        sh:path kh:query ;
        sh:datatype xsd:string ;
        sh:minCount 0 ;
        sh:maxCount 1 ;
    ] .
"#;

pub(crate) const ALLOWED_KH_PREDICATES: &[&str] = &[
    "name", "on", "kind", "var", "op", "k", "window", "program", "goal", "query", "effect",
    "action", "reason", "priority", "after", "handler",
];

/// Maps hook: predicates to kh: equivalents (alias → canonical).
/// Used for rewriting hook:* triples during validation preprocessing.
///
/// # Determinism
/// The mapping is immutable and position-independent; rewriting produces identical output
/// regardless of input order.
pub(crate) const HOOK_ALIAS_MAP: &[(&str, &str)] = &[
    ("name", "name"),
    ("on", "on"),
    ("kind", "kind"),
    ("var", "var"),
    ("op", "op"),
    ("k", "k"),
    ("window", "window"),
    ("program", "program"),
    ("goal", "goal"),
    ("query", "query"),
    ("effect", "effect"),
    ("action", "action"),
    ("reason", "reason"),
    ("priority", "priority"),
    ("after", "after"),
    ("handler", "handler"),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl CmpOp {
    pub fn parse(s: &str) -> Result<Self, String> {
        match s {
            "=" => Ok(Self::Eq),
            "!=" => Ok(Self::Ne),
            "<" => Ok(Self::Lt),
            "<=" => Ok(Self::Le),
            ">" => Ok(Self::Gt),
            ">=" => Ok(Self::Ge),
            other => Err(format!("unknown operator '{}'", other)),
        }
    }

    pub fn holds(self, lhs: u64, rhs: u64) -> bool {
        match self {
            Self::Eq => lhs == rhs,
            Self::Ne => lhs != rhs,
            Self::Lt => lhs < rhs,
            Self::Le => lhs <= rhs,
            Self::Gt => lhs > rhs,
            Self::Ge => lhs >= rhs,
        }
    }
}

// ============================================================================
// PROJ-403: Compiled Hook IR & PROJ-404: Compiled Condition IR
// ============================================================================

/// Hook identifier: unique u32 assigned at compile time.
/// Deterministic: same hook position → same HookId across runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct HookId(pub u32);

/// Event identifier: tracks the event type (on: "assert"/"retract"/"any").
/// Deterministic: same 'on' value → same EventId if seen first in that order.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct EventId(pub u32);

// ============================================================================
// PROJ-404: Compiled Condition IR
// ============================================================================

/// Pre-compiled hook condition with all runtime dispatch replaced by enum variants.
/// No string-based dispatch; all condition evaluation uses direct pattern matching.
///
/// SCOPED: SymbolId references noted in ticket do not exist; using String IRIs instead.
/// When SymbolId interner is available (future ticket), update these fields accordingly.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CompiledCondition {
    Datalog {
        program: String,
        goal: String,
    },
    N3 {
        rules: String,
    },
    Shape {
        target_iri: String,
        shape_iri: String,
    },
    Delta {
        pattern: String,
    },
    Threshold {
        min_count: usize,
    },
    Count {
        op: CmpOp,
        value: usize,
    },
    Window {
        duration_ms: u64,
    },
    Unsupported {
        reason: String,
    },
}

impl CompiledCondition {
    pub fn kind(&self) -> &'static str {
        match self {
            CompiledCondition::Datalog { .. } => "datalog",
            CompiledCondition::N3 { .. } => "n3",
            CompiledCondition::Shape { .. } => "shape",
            CompiledCondition::Delta { .. } => "delta",
            CompiledCondition::Threshold { .. } => "threshold",
            CompiledCondition::Count { .. } => "count",
            CompiledCondition::Window { .. } => "window",
            CompiledCondition::Unsupported { .. } => "unsupported",
        }
    }
}

/// Feature support classification for dialect features.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FeatureDecision {
    Supported,
    Unsupported { reason: &'static str },
    ExternalBoundaryRequired { endpoint: &'static str },
}

/// Profile-level support classification for dialect operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProfileDecision {
    Supported { cost_tier: u8 },
    Unsupported { reason: &'static str },
    ExternalBoundaryRequired { required_endpoint: &'static str },
}

// ============================================================================
// PROJ-408: Compiled Delta Template IR
// ============================================================================

/// A component of a hook effect template (pre-compiled placeholder or literal).
/// Eliminates runtime string scanning for ?0, ?1, etc. placeholders.
///
/// SCOPED: SymbolId noted in ticket does not exist; using String IRIs instead.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TemplatePart {
    Literal { value: String },
    Binding { slot: usize },
}

/// A pre-compiled triple template for hook effects.
/// Each part is either a literal IRI or a binding reference (?N).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompiledTripleTemplate {
    pub subject: TemplatePart,
    pub predicate: TemplatePart,
    pub object: TemplatePart,
}

/// A pre-compiled delta template (collection of triple templates).
/// Contains all triples to be added/retracted when condition fires.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompiledDeltaTemplate {
    pub triples: Vec<CompiledTripleTemplate>,
    pub max_binding_slot: usize,
}

/// Pre-compiled hook representation with ID-based dependency tracking.
/// Uses HookId for all references instead of string IRIs, enabling O(1) lookups.
///
/// SCOPED: PROJ-404 condition compilation deferred; using HookCondition for now.
/// When evaluate_condition is refactored to dispatch on CompiledCondition,
/// change this field to `condition: CompiledCondition`.
///
/// Complexity: all fields are constant-time access; dependency list is bounded by SmallVec<[HookId; 4]>
/// for typical hook hierarchies (most hooks have 0-4 dependencies).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompiledHook {
    pub id: HookId,
    pub iri: String,
    pub name: String,
    pub event: EventId,
    pub on: String,
    pub condition: HookCondition,
    pub effect: EffectKind,
    pub action: Option<String>,
    pub reason: Option<String>,
    pub priority: u8,
    pub after: smallvec::SmallVec<[HookId; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum HookCondition {
    Datalog {
        program: String,
        goal: String,
    },
    Delta {
        var: String,
    },
    Threshold {
        var: String,
        op: CmpOp,
        k: u64,
    },
    Count {
        var: String,
        op: CmpOp,
        k: u64,
    },
    Window {
        var: String,
        op: CmpOp,
        k: u64,
        window: u8,
    },
    Shacl {
        shapes: String,
    },
    Shex {
        schema: String,
        shape_map: String,
    },
    N3 {
        rules: String,
    },
    Sparql {
        query: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum EffectKind {
    EmitDelta,
    GroundAction,
    Refuse,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct KnowledgeHook {
    pub iri: String,
    pub name: String,
    pub on: String,
    pub condition: HookCondition,
    pub effect: EffectKind,
    pub action: Option<String>,
    pub reason: Option<String>,
    pub priority: u8,
    pub after: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HookPack {
    pub name: String,
    pub version: String,
    pub description: String,
    pub required_dialects: Vec<String>,
    pub hooks: Vec<KnowledgeHook>,
}

// Re-export public API from submodules
pub use compile::{compile_hooks, schedule_hooks};
pub use condition::{compile_condition, evaluate_condition};
pub use construct::{evaluate_construct, serialize_delta_quad, HookReceipt};
pub use datalog::translate_datalog_to_n3;
pub use delta_query::parse_shape_map;
pub use evaluate::{evaluate_hooks, ActionOutcome};
pub use parsing::{
    clean_term, contains_forbidden_keyword, parse_rdf_integer, validate_and_extract_hooks,
};
pub use quads::{
    canonicalize_quads, escape_literal, get_where_triple_pattern, parse_construct, serialize_quad,
    strip_comments, tokenize_triple, ConstructQuery,
};
pub use toml::parse_simple_toml;
pub use verdict::{
    hook_hash, DiagnosticDetail, GraphDelta, HookError, HookVerdict, HookVerdictRecord,
    TriggerDiagnostic,
};

// Submodules
mod compile;
mod condition;
mod construct;
mod datalog;
mod delta_query;
mod evaluate;
mod parsing;
mod quads;
mod toml;
mod verdict;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Parser;
    use smallvec::SmallVec;

    #[test]
    fn test_trigger_dialects() {
        let data = "<http://e/s> <http://e/p> <http://e/o> .
<http://e/s> <http://e/item> \"1\"^^<http://www.w3.org/2001/XMLSchema#integer> .
<http://e/s> <http://e/item> \"2\"^^<http://www.w3.org/2001/XMLSchema#integer> .";
        let store = TripleStore::from(data);

        let delta = GraphDelta {
            additions: Parser::parse_triples(
                "<http://e/s> <http://e/item> \"3\"^^<http://www.w3.org/2001/XMLSchema#integer> .",
                crate::parser::Syntax::Turtle,
            )
            .unwrap(),
            removals: Vec::new(),
        };

        let history = vec![GraphDelta {
            additions: Parser::parse_triples(
                "<http://e/s> <http://e/item> \"9\"^^<http://www.w3.org/2001/XMLSchema#integer> .",
                crate::parser::Syntax::Turtle,
            )
            .unwrap(),
            removals: Vec::new(),
        }];

        let cond_delta = HookCondition::Delta {
            var: "http://e/item".to_string(),
        };
        let (fired, _) =
            evaluate_condition(&cond_delta, &store, &delta, &history, "ex:h1").unwrap();
        assert!(fired);

        let cond_threshold = HookCondition::Threshold {
            var: "http://e/item".to_string(),
            op: CmpOp::Gt,
            k: 1,
        };
        let (fired, _) =
            evaluate_condition(&cond_threshold, &store, &delta, &history, "ex:h2").unwrap();
        assert!(fired);

        let cond_count = HookCondition::Count {
            var: "http://e/item".to_string(),
            op: CmpOp::Eq,
            k: 1,
        };
        let (fired, _) =
            evaluate_condition(&cond_count, &store, &delta, &history, "ex:h3").unwrap();
        assert!(fired);

        let cond_window = HookCondition::Window {
            var: "http://e/item".to_string(),
            op: CmpOp::Eq,
            k: 2,
            window: 2,
        };
        let (fired, _) =
            evaluate_condition(&cond_window, &store, &delta, &history, "ex:h4").unwrap();
        assert!(fired);

        let cond_sparql = HookCondition::Sparql {
            query: "SELECT * WHERE { ?s <http://e/p> <http://e/o> }".to_string(),
        };
        let (fired, _) =
            evaluate_condition(&cond_sparql, &store, &delta, &history, "ex:h5").unwrap();
        assert!(fired);

        let cond_sparql_ask = HookCondition::Sparql {
            query: "ASK { ?s <http://e/p> <http://e/o> }".to_string(),
        };
        let (fired, _) =
            evaluate_condition(&cond_sparql_ask, &store, &delta, &history, "ex:h6").unwrap();
        assert!(fired);

        let cond_datalog = HookCondition::Datalog {
            program: "linked(?0) :- t(?1, <http://e/p>, ?0). orphan(?0) :- t(?0, <http://e/item>, ?1), !linked(?0).".to_string(),
            goal: "orphan".to_string(),
        };
        let (fired, _) =
            evaluate_condition(&cond_datalog, &store, &delta, &history, "ex:h7").unwrap();
        assert!(fired);
    }

    #[test]
    fn test_compile_hooks_assigns_unique_hook_ids() {
        let hooks = vec![
            KnowledgeHook {
                iri: "http://ex/h1".to_string(),
                name: "h1".to_string(),
                on: "assert".to_string(),
                condition: HookCondition::Delta {
                    var: "p".to_string(),
                },
                effect: EffectKind::EmitDelta,
                action: None,
                reason: None,
                priority: 0,
                after: vec![],
            },
            KnowledgeHook {
                iri: "http://ex/h2".to_string(),
                name: "h2".to_string(),
                on: "assert".to_string(),
                condition: HookCondition::Delta {
                    var: "p".to_string(),
                },
                effect: EffectKind::EmitDelta,
                action: None,
                reason: None,
                priority: 0,
                after: vec![],
            },
        ];

        let compiled = compile_hooks(hooks).expect("compile should succeed");
        assert_eq!(compiled.len(), 2);
        assert_eq!(compiled[0].id, HookId(0));
        assert_eq!(compiled[1].id, HookId(1));
        assert_ne!(compiled[0].id, compiled[1].id);
    }

    #[test]
    fn test_compile_hooks_resolves_dependencies() {
        let hooks = vec![
            KnowledgeHook {
                iri: "http://ex/h1".to_string(),
                name: "h1".to_string(),
                on: "assert".to_string(),
                condition: HookCondition::Delta {
                    var: "p".to_string(),
                },
                effect: EffectKind::EmitDelta,
                action: None,
                reason: None,
                priority: 0,
                after: vec![],
            },
            KnowledgeHook {
                iri: "http://ex/h2".to_string(),
                name: "h2".to_string(),
                on: "assert".to_string(),
                condition: HookCondition::Delta {
                    var: "p".to_string(),
                },
                effect: EffectKind::EmitDelta,
                action: None,
                reason: None,
                priority: 0,
                after: vec!["http://ex/h1".to_string()],
            },
        ];

        let compiled = compile_hooks(hooks).expect("compile should succeed");
        assert_eq!(compiled[1].after.len(), 1);
        assert_eq!(compiled[1].after[0], HookId(0));
    }

    #[test]
    fn test_compile_hooks_unknown_dependency_error() {
        let hooks = vec![KnowledgeHook {
            iri: "http://ex/h1".to_string(),
            name: "h1".to_string(),
            on: "assert".to_string(),
            condition: HookCondition::Delta {
                var: "p".to_string(),
            },
            effect: EffectKind::EmitDelta,
            action: None,
            reason: None,
            priority: 0,
            after: vec!["http://ex/unknown".to_string()],
        }];

        let result = compile_hooks(hooks);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("unknown after-dependency"));
    }

    #[test]
    fn test_schedule_hooks_tie_break_by_hook_id() {
        let hooks = vec![
            CompiledHook {
                id: HookId(2),
                iri: "http://ex/h2".to_string(),
                name: "h2".to_string(),
                event: EventId(0),
                on: "assert".to_string(),
                condition: HookCondition::Delta {
                    var: "p".to_string(),
                },
                effect: EffectKind::EmitDelta,
                action: None,
                reason: None,
                priority: 0,
                after: SmallVec::new(),
            },
            CompiledHook {
                id: HookId(0),
                iri: "http://ex/h0".to_string(),
                name: "h0".to_string(),
                event: EventId(0),
                on: "assert".to_string(),
                condition: HookCondition::Delta {
                    var: "p".to_string(),
                },
                effect: EffectKind::EmitDelta,
                action: None,
                reason: None,
                priority: 0,
                after: SmallVec::new(),
            },
            CompiledHook {
                id: HookId(1),
                iri: "http://ex/h1".to_string(),
                name: "h1".to_string(),
                event: EventId(0),
                on: "assert".to_string(),
                condition: HookCondition::Delta {
                    var: "p".to_string(),
                },
                effect: EffectKind::EmitDelta,
                action: None,
                reason: None,
                priority: 0,
                after: SmallVec::new(),
            },
        ];

        let scheduled = schedule_hooks(&hooks).expect("schedule should succeed");
        assert_eq!(scheduled.len(), 3);
        // Should be ordered by HookId: 0, 1, 2
        assert_eq!(scheduled[0].id, HookId(0));
        assert_eq!(scheduled[1].id, HookId(1));
        assert_eq!(scheduled[2].id, HookId(2));
    }

    #[test]
    fn test_schedule_hooks_cycle_detection() {
        let hooks = vec![
            CompiledHook {
                id: HookId(0),
                iri: "http://ex/h0".to_string(),
                name: "h0".to_string(),
                event: EventId(0),
                on: "assert".to_string(),
                condition: HookCondition::Delta {
                    var: "p".to_string(),
                },
                effect: EffectKind::EmitDelta,
                action: None,
                reason: None,
                priority: 0,
                after: {
                    let mut sv = SmallVec::new();
                    sv.push(HookId(1));
                    sv
                },
            },
            CompiledHook {
                id: HookId(1),
                iri: "http://ex/h1".to_string(),
                name: "h1".to_string(),
                event: EventId(0),
                on: "assert".to_string(),
                condition: HookCondition::Delta {
                    var: "p".to_string(),
                },
                effect: EffectKind::EmitDelta,
                action: None,
                reason: None,
                priority: 0,
                after: {
                    let mut sv = SmallVec::new();
                    sv.push(HookId(0));
                    sv
                },
            },
        ];

        let result = schedule_hooks(&hooks);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("dependency cycle"));
    }

    #[test]
    fn test_schedule_hooks_respects_dependencies() {
        let hooks = vec![
            CompiledHook {
                id: HookId(0),
                iri: "http://ex/h0".to_string(),
                name: "h0".to_string(),
                event: EventId(0),
                on: "assert".to_string(),
                condition: HookCondition::Delta {
                    var: "p".to_string(),
                },
                effect: EffectKind::EmitDelta,
                action: None,
                reason: None,
                priority: 0,
                after: SmallVec::new(),
            },
            CompiledHook {
                id: HookId(1),
                iri: "http://ex/h1".to_string(),
                name: "h1".to_string(),
                event: EventId(0),
                on: "assert".to_string(),
                condition: HookCondition::Delta {
                    var: "p".to_string(),
                },
                effect: EffectKind::EmitDelta,
                action: None,
                reason: None,
                priority: 0,
                after: {
                    let mut sv = SmallVec::new();
                    sv.push(HookId(0));
                    sv
                },
            },
        ];

        let scheduled = schedule_hooks(&hooks).expect("schedule should succeed");
        assert_eq!(scheduled.len(), 2);
        // h0 should come before h1
        assert_eq!(scheduled[0].id, HookId(0));
        assert_eq!(scheduled[1].id, HookId(1));
    }
}
