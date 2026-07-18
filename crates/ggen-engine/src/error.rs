//! Error types for ggen.

use once_cell::sync::Lazy;
use thiserror::Error;

/// The top-level error type for this crate.
#[derive(Debug, Error)]
pub enum AppError {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("serialization error: {0}")]
    Serde(#[from] serde_json::Error),

    /// Argument validation failure. Every message MUST include FM identifier and remediation text.
    #[error("validation error: {0}")]
    Validation(String),

    /// RDF graph failure (store, canonicalization, delta). Messages carry an FM-GRAPH-* code.
    #[error("graph error: {0}")]
    Graph(String),

    /// Configuration (ggen.toml) failure. Messages carry an FM-CONFIG-* code.
    #[error("config error: {0}")]
    Config(String),
}

impl AppError {
    /// Construct a validation error. Callers must embed FM/RPN identifier and remediation text.
    pub fn validation(msg: impl Into<String>) -> Self {
        Self::Validation(msg.into())
    }
}

/// Convenience alias used by the poka-yoke trait layer.
pub type Result<T> = std::result::Result<T, AppError>;

// ---------------------------------------------------------------------------
// TemplateFailureCause — typed FM-GEN-008 sub-classification
// ---------------------------------------------------------------------------

/// Typed causal sub-classification for an `[FM-GEN-008]` failure
/// (`crate::generation_rules`'s per-rule Tera render/output-path step).
/// The public `[FM-GEN-008]` string identifier never changes; this enum is
/// the *typed* cause layered alongside it (see [`AppError::fm_gen_render_failure`]).
///
/// `tera::Error`'s own `ErrorKind` is coarse — most failures collapse into
/// a single `Msg(String)` variant (e.g. tera 1.20.1's
/// `renderer/processor.rs::process_path` raises "Variable ... not found in
/// context" as a bare `Error::msg(..)`, not a dedicated `ErrorKind`
/// variant) — so `crate::template::classify_tera_render_error` combines
/// `ErrorKind` matching (where Tera *does* give a structured variant) with
/// message substring matching (the only signal Tera exposes for the rest).
/// That is a genuine constraint of Tera's error model, not a shortcut this
/// crate chose — documented here rather than hidden.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TemplateFailureCause {
    /// The template name itself was never registered (a stale/renamed
    /// per-rule template name, or `Tera::render`'s own top-level lookup
    /// failing for the exact name the caller asked to render).
    TemplateNotFound,
    /// Tera rejected the template body's or output-file pattern's syntax.
    TemplateParseFailed,
    /// `{{ x }}` / `{% if x %}` / etc. referenced a context key that was
    /// never inserted (the Cluster D `sparql_results` failure mode this
    /// migration's context-key fix addresses).
    TemplateVariableMissing,
    /// A filter/function/test name Tera has no registration for at all
    /// (distinct from [`Self::TemplateContextInvalid`], where the name
    /// exists but the call itself failed).
    TemplateFilterUnknown,
    /// Hygen-style frontmatter (`crate::template::Frontmatter`) failed to
    /// parse or validate.
    TemplateFrontmatterInvalid,
    /// The render context was malformed for what the template needed (a
    /// math operation against a non-number, a JSON serialization failure,
    /// or a registered filter/function/test that exists but was called
    /// with the wrong argument shape).
    TemplateContextInvalid,
    /// The rule's `output_file`/`to` pattern failed to render — wrong
    /// variable, bad filter, or a syntax error in the pattern itself.
    TemplateOutputPathInvalid,
    /// `{% include %}` / `{% extends %}` named a template file that isn't
    /// registered, or a circular/missing inheritance chain.
    TemplateIncludeNotFound,
    /// The template source isn't Tera content at all — e.g. a YAML
    /// file-tree/meta-spec (`structure:` + `foreach:`) that
    /// `crate::generation_rules` has no interpreter for (Cluster B,
    /// `examples/clap-noun-verb-demo`).
    TemplateSchemaIncompatible,
    /// Every other Tera failure this classifier has no more specific
    /// bucket for (I/O, UTF-8 conversion, circular extend at the raw
    /// `ErrorKind` level).
    TemplateRenderInternal,
}

impl TemplateFailureCause {
    /// The stable, machine-parseable identifier — exactly the ten names
    /// this migration's Definition of Done specced for FM-GEN-008's typed
    /// sub-classification.
    #[must_use]
    pub fn as_code(self) -> &'static str {
        match self {
            Self::TemplateNotFound => "TEMPLATE_NOT_FOUND",
            Self::TemplateParseFailed => "TEMPLATE_PARSE_FAILED",
            Self::TemplateVariableMissing => "TEMPLATE_VARIABLE_MISSING",
            Self::TemplateFilterUnknown => "TEMPLATE_FILTER_UNKNOWN",
            Self::TemplateFrontmatterInvalid => "TEMPLATE_FRONTMATTER_INVALID",
            Self::TemplateContextInvalid => "TEMPLATE_CONTEXT_INVALID",
            Self::TemplateOutputPathInvalid => "TEMPLATE_OUTPUT_PATH_INVALID",
            Self::TemplateIncludeNotFound => "TEMPLATE_INCLUDE_NOT_FOUND",
            Self::TemplateSchemaIncompatible => "TEMPLATE_SCHEMA_INCOMPATIBLE",
            Self::TemplateRenderInternal => "TEMPLATE_RENDER_INTERNAL",
        }
    }
}

// ---------------------------------------------------------------------------
// Poka-yoke CLI validator trait (clnrm poka_yoke pattern)
// ---------------------------------------------------------------------------

/// Dyn-compatible argument validation surface.
///
/// Every `validate_*` method MUST return an error whose message contains:
/// 1. An FM/RPN identifier (e.g. `[FM-CLI-001]`).
/// 2. A human-readable remediation instruction.
///
/// RPN severity guide: 1 (certain detection) – 10 (undetectable).
pub trait CliValidator: Send + Sync {
    /// FM-CLI-001 | RPN-SEV-8: invalid parallel/job count combination.
    ///
    /// Failure Mode: `--parallel` flag set but `--jobs` is zero → runtime panic or no-op.
    /// Remediation: set `--jobs` to a positive integer or remove `--parallel` flag.
    fn validate_run_args(&self, parallel: bool, jobs: usize) -> Result<()>;
}

/// Default implementation of [`CliValidator`] enforcing all FM/RPN checks.
pub struct DefaultCliValidator;

impl Default for DefaultCliValidator {
    fn default() -> Self {
        Self
    }
}

impl CliValidator for DefaultCliValidator {
    fn validate_run_args(&self, parallel: bool, jobs: usize) -> Result<()> {
        // FM-CLI-001 | RPN-SEV-8
        if parallel && jobs == 0 {
            return Err(AppError::validation(
                "[FM-CLI-001] --parallel requires --jobs > 0. \
                 Remediation: pass --jobs <N> with a positive integer.",
            ));
        }
        Ok(())
    }
}

/// Process-wide singleton validator. Replace via dependency injection in tests.
pub static CLI_VALIDATOR: Lazy<DefaultCliValidator> = Lazy::new(DefaultCliValidator::default);

// ---------------------------------------------------------------------------
// Typed FM-code constructors
// ---------------------------------------------------------------------------

impl AppError {
    /// FM-CLI-* failure: CLI argument or invocation violation.
    ///
    /// Embeds `[FM-CLI-{code}]` prefix so log scrapers can extract failure modes.
    pub fn fm_cli(code: u16, msg: impl Into<String>) -> Self {
        Self::Validation(format!("[FM-CLI-{code:03}] {}", msg.into()))
    }

    /// FM-CHAIN-* failure: receipt chain construction or integrity violation.
    pub fn fm_chain(code: u16, msg: impl Into<String>) -> Self {
        Self::Validation(format!("[FM-CHAIN-{code:03}] {}", msg.into()))
    }

    /// FM-GRAPH-* failure: RDF store, canonicalization, or delta violation.
    pub fn fm_graph(code: u16, msg: impl Into<String>) -> Self {
        Self::Graph(format!("[FM-GRAPH-{code:03}] {}", msg.into()))
    }

    /// FM-TPL-* failure: template frontmatter parse or render violation.
    pub fn fm_tpl(code: u16, msg: impl Into<String>) -> Self {
        Self::Validation(format!("[FM-TPL-{code:03}] {}", msg.into()))
    }

    /// FM-WRITE-* failure: file-write planning or application violation.
    pub fn fm_write(code: u16, msg: impl Into<String>) -> Self {
        Self::Validation(format!("[FM-WRITE-{code:03}] {}", msg.into()))
    }

    /// FM-PACK-* failure: pack resolution, hashing, or lockfile violation.
    pub fn fm_pack(code: u16, msg: impl Into<String>) -> Self {
        Self::Validation(format!("[FM-PACK-{code:03}] {}", msg.into()))
    }

    /// FM-CONFIG-* failure: ggen.toml loading or schema violation.
    pub fn fm_config(code: u16, msg: impl Into<String>) -> Self {
        Self::Config(format!("[FM-CONFIG-{code:03}] {}", msg.into()))
    }

    /// FM-WATCH-* failure: filesystem watch setup or initial-sync violation.
    pub fn fm_watch(code: u16, msg: impl Into<String>) -> Self {
        Self::Validation(format!("[FM-WATCH-{code:03}] {}", msg.into()))
    }

    /// FM-SHELL-* failure: frontmatter `sh_before`/`sh_after` hook refused or failed.
    pub fn fm_shell(code: u16, msg: impl Into<String>) -> Self {
        Self::Validation(format!("[FM-SHELL-{code:03}] {}", msg.into()))
    }

    /// FM-LAW-* failure: law-state engine violation (rule load/materialize,
    /// SHACL/ShEx gate, denial check) or a law operation requested from an
    /// engine that does not support it.
    pub fn fm_law(code: u16, msg: impl Into<String>) -> Self {
        Self::Validation(format!("[FM-LAW-{code:03}] {}", msg.into()))
    }

    /// FM-GEN-* failure: declarative `[[generation.rules]]` sync path violation
    /// (`crate::generation_rules`) -- query/template resolution, an unimplemented
    /// `QuerySource`/`TemplateSource` variant, or a write-mode/merge failure.
    pub fn fm_gen(code: u16, msg: impl Into<String>) -> Self {
        Self::Validation(format!("[FM-GEN-{code:03}] {}", msg.into()))
    }

    /// FM-GEN-008, typed: a Tera render/output-path failure inside
    /// `crate::generation_rules`, sub-classified by [`TemplateFailureCause`].
    /// The public `[FM-GEN-008]` identifier is retained unchanged (nothing
    /// downstream that greps for it breaks); a machine-parseable
    /// `[{CAUSE_CODE}]` tag plus the example project root, the resolved
    /// template source, and the rule name are embedded alongside it, per
    /// this repo's evidence-first discipline (a generic "template render
    /// failed" message with no cause, path, or rule name was exactly the
    /// defect this constructor exists to close — see
    /// `docs/releases/v26.7.17/EXAMPLE_FAILURE_MATRIX.md`'s Cluster D/A
    /// writeup).
    pub fn fm_gen_render_failure(
        cause: TemplateFailureCause,
        example: &str,
        template: &str,
        rule: &str,
        detail: impl Into<String>,
        location: Option<&str>,
    ) -> Self {
        let loc = location
            .map(|l| format!(" (at {l})"))
            .unwrap_or_default();
        Self::Validation(format!(
            "[FM-GEN-008][{}] example=`{example}` template=`{template}` rule=`{rule}`: {}{loc}",
            cause.as_code(),
            detail.into(),
        ))
    }

    /// FM-KEY-* failure: ed25519 signing/verifying-key resolution violation
    /// (`crate::keys`) -- a malformed `GGEN_SIGNING_KEY` env var or
    /// `.ggen/keys/{signing,verifying}.key` file, or an I/O failure
    /// generating/persisting a fresh keypair.
    pub fn fm_key(code: u16, msg: impl Into<String>) -> Self {
        Self::Validation(format!("[FM-KEY-{code:03}] {}", msg.into()))
    }
}

// ---------------------------------------------------------------------------
// ValidationChain — collect multiple checks, report all failures at once
// ---------------------------------------------------------------------------

/// Collect multiple validation results and surface all failures in one error.
///
/// Unlike early-return `?`, `ValidationChain` runs every check and joins all
/// failure messages so operators see the complete picture in one pass.
///
/// # Example
///
/// ```rust
/// use ggen_engine::error::{AppError, ValidationChain};
///
/// let mut chain = ValidationChain::new();
/// chain.check(Err(AppError::fm_cli(1, "--jobs must be positive")));
/// chain.check(Ok(()));
/// chain.check(Err(AppError::fm_cli(2, "--output is required")));
///
/// let result = chain.finish();
/// assert!(result.is_err());
/// let msg = result.unwrap_err().to_string();
/// assert!(msg.contains("FM-CLI-001"));
/// assert!(msg.contains("FM-CLI-002"));
/// ```
pub struct ValidationChain {
    errors: Vec<String>,
}

impl ValidationChain {
    /// Create an empty chain.
    pub fn new() -> Self {
        ValidationChain { errors: Vec::new() }
    }

    /// Add a check result. `Ok(())` is silently accepted; `Err` is recorded.
    pub fn check(&mut self, result: Result<()>) -> &mut Self {
        if let Err(e) = result {
            self.errors.push(e.to_string());
        }
        self
    }

    /// Convenience: add a check only when `condition` is false.
    pub fn require(&mut self, condition: bool, error: AppError) -> &mut Self {
        if !condition {
            self.errors.push(error.to_string());
        }
        self
    }

    /// Return `Ok(())` if no errors were recorded, else `Err` with all messages joined.
    pub fn finish(self) -> Result<()> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(AppError::Validation(self.errors.join("; ")))
        }
    }

    /// Returns `true` if any errors have been recorded.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Returns the count of recorded errors.
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }
}

impl Default for ValidationChain {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod validation_chain_tests {
    use super::*;

    #[test]
    fn empty_chain_is_ok() {
        assert!(ValidationChain::new().finish().is_ok());
    }

    #[test]
    fn single_error_reported() {
        let mut c = ValidationChain::new();
        c.check(Err(AppError::fm_cli(1, "bad arg")));
        let e = c.finish().unwrap_err();
        assert!(e.to_string().contains("FM-CLI-001"));
    }

    #[test]
    fn multiple_errors_joined() {
        let mut c = ValidationChain::new();
        c.check(Err(AppError::fm_chain(1, "empty chain")));
        c.check(Ok(()));
        c.check(Err(AppError::fm_graph(3, "hash mismatch")));
        let e = c.finish().unwrap_err().to_string();
        assert!(e.contains("FM-CHAIN-001"), "missing chain error: {e}");
        assert!(e.contains("FM-GRAPH-003"), "missing graph error: {e}");
    }

    #[test]
    fn require_records_error_on_false() {
        let mut c = ValidationChain::new();
        c.require(false, AppError::fm_cli(99, "must be true"));
        assert!(c.has_errors());
        assert_eq!(c.error_count(), 1);
    }

    #[test]
    fn require_passes_on_true() {
        let mut c = ValidationChain::new();
        c.require(true, AppError::fm_cli(99, "should not appear"));
        assert!(!c.has_errors());
    }

    #[test]
    fn fm_code_constructors_embed_code() {
        assert!(AppError::fm_cli(42, "msg")
            .to_string()
            .contains("FM-CLI-042"));
        assert!(AppError::fm_chain(7, "msg")
            .to_string()
            .contains("FM-CHAIN-007"));
        assert!(AppError::fm_graph(1, "msg")
            .to_string()
            .contains("FM-GRAPH-001"));
    }
}
