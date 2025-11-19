//! Toyota Production System (TPS) integration for ggen
//!
//! This module implements the six core TPS principles for continuous improvement
//! in code generation:
//!
//! 1. **Just-In-Time (JIT)**: Delta-driven regeneration with field-level changes
//! 2. **Jidoka**: Autonomous error detection with semantic validation
//! 3. **Heijunka**: Load leveling with rate limiting and resource quotas
//! 4. **Genchi Genbutsu**: Reality verification through audit trails and diffs
//! 5. **Nemawashi**: Consensus building via annotations and collaboration
//! 6. **Hansei**: Reflection ceremonies through metrics and retrospectives

pub mod jit;
pub mod jidoka;
pub mod heijunka;
pub mod genchi_genbutsu;
pub mod nemawashi;
pub mod hansei;

// Re-export commonly used types
pub use jit::{FieldDelta, TemplateSelector, ChangeType};
pub use jidoka::{SemanticValidator, ValidationIssue};
pub use heijunka::RateLimiter;
pub use genchi_genbutsu::{AuditEntry, AuditLog, AuditAction};
pub use nemawashi::{Annotation, AnnotationStore};
pub use hansei::{MetricsCollector, GenerationEvent, Trends};
