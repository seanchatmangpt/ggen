//! Tera template law-surface analyzer (`.tera`).
//!
//! Surfaces template syntax errors (ggen's **E0024**) at author time, plus
//! filter/keyword completion, an outline of `{% set %}`/`{% block %}`/`{% include %}`
//! directives, and folding over `{% for %}`/`{% if %}`/`{% block %}` blocks.

use lsp_max::lsp_types::{
    CallHierarchyItem, CodeLens, CompletionItem, CompletionItemKind, CompletionResponse,
    DiagnosticSeverity, DocumentSymbol, FoldingRange, FoldingRangeKind, Hover, InlayHint, Location,
    Position, Range, SymbolKind, TextEdit, WorkspaceEdit,
};
use std::collections::BTreeSet;

use crate::analyzers::diag;

/// Canonical diagnostic code for the cross-surface unbound-projection law:
/// a template consumes a variable that the rule's SPARQL `SELECT` does not produce.
pub const GGEN_TPL_001: &str = "GGEN-TPL-001";

/// Canonical diagnostic code for the cross-surface unbound-OUTPUT-PATH law.
///
/// A rule's `output_file` pattern (a tiny Tera template, rendered per SPARQL
/// result row) consumes a variable that the rule's SPARQL `SELECT` does not
/// produce. The dual of [`GGEN_TPL_001`] on the `ggen.toml`/SPARQL surfaces.
pub const GGEN_OUT_001: &str = "GGEN-OUT-001";

/// Canonical diagnostic code for the unbound-rule-file law: a ggen.toml
/// [[generation.rules]] query/template `{file=...}` points at a missing file.
///
/// This is the FOUNDATIONAL binding-integrity check the GGEN-TPL-001 /
/// GGEN-OUT-001 detectors presuppose (they skip a rule whose bound file could
/// not be read). It surfaces a previously-silent `RuleIndexEntry::issues` item
/// as a live diagnostic on the `ggen.toml` declaration surface.
pub const GGEN_RULE_001: &str = "GGEN-RULE-001";

/// Canonical diagnostic code for the output-path-escape law.
///
/// A rule's `output_file` (after stripping Tera `{{ }}` interpolations)
/// resolves to a path outside the project root — a pack-root traversal /
/// path-injection risk.
pub const GGEN_YIELD_001: &str = "GGEN-YIELD-001";

/// Canonical diagnostic code for the orphaned-output law: a rule's output_file
/// lacks a static filename base (e.g. is just an extension like `.rs`).
pub const GGEN_YIELD_003: &str = "GGEN-YIELD-003";

/// Canonical diagnostic code for the competing-authority law: multiple rules
/// target the same output file.
pub const GGEN_YIELD_004: &str = "GGEN-YIELD-004";

/// Canonical diagnostic code for the remote-fetch law: output_file is a URL.
pub const GGEN_YIELD_005: &str = "GGEN-YIELD-005";

/// Canonical diagnostic code for the SELECT * blindspot advisory: a rule uses
/// `SELECT *` which disables GGEN-TPL-001 and GGEN-OUT-001 because the
/// projected variable set is unknowable at author time.
pub const GGEN_QUERY_002: &str = "GGEN-QUERY-002";

/// Canonical diagnostic code for the pack-source advisory.
///
/// A rule's query or template source is `{ pack = "...", output = "...",
/// file = "..." }`, which ggen-lsp cannot resolve at author time (resolution
/// happens at `ggen sync`). GGEN-TPL-001 and GGEN-OUT-001 are therefore
/// disabled for this rule.
pub const GGEN_PACK_001: &str = "GGEN-PACK-001";

const FILTERS: &[&str] = &[
    "upper",
    "lower",
    "capitalize",
    "trim",
    "truncate",
    "length",
    "reverse",
    "first",
    "last",
    "join",
    "default",
    "replace",
    "title",
    "wordcount",
    "slugify",
    "json_encode",
    "abs",
    "round",
];
const KEYWORDS: &[&str] = &[
    "if",
    "elif",
    "else",
    "endif",
    "for",
    "endfor",
    "block",
    "endblock",
    "set",
    "include",
    "extends",
    "macro",
    "endmacro",
    "filter",
    "endfilter",
    "raw",
    "endraw",
    "in",
    "and",
    "or",
    "not",
];

#[derive(Clone)]
pub struct TeraAnalyzer {
    source: String,
    /// Variables the rule's SPARQL `SELECT` produces (no `?`). Empty when the
    /// analyzer is built in single-file isolation (`new_from_content(_, "")`).
    available_vars: BTreeSet<String>,
}

impl TeraAnalyzer {
    pub fn new_from_content(
        content: &str, sparql_bindings: &str,
    ) -> Result<Self, crate::error::LspError> {
        Ok(Self {
            source: content.to_string(),
            available_vars: super::select_projection_vars(sparql_bindings, None),
        })
    }

    /// The variables this rule's SPARQL `SELECT` produces (no `?`).
    #[must_use]
    pub fn available_vars(&self) -> &BTreeSet<String> {
        &self.available_vars
    }

    /// Template syntax diagnostics, carrying ggen's E0024 code.
    ///
    /// When this analyzer was built with the rule's SPARQL bindings (i.e.
    /// `available_vars` is non-empty), it *also* surfaces GGEN-TPL-001
    /// (unbound projection) so the interactive server and the headless gate
    /// enforce the cross-surface law identically. In single-file isolation
    /// (`available_vars` empty) only the E0024 syntax law is checked, since
    /// there is no rule context to compare against.
    #[must_use]
    pub fn diagnostics(&self) -> Vec<lsp_max_protocol::MaxDiagnostic> {
        let mut tera = tera::Tera::default();
        let mut diags = match tera.add_raw_template("__ggen_lsp_doc__", &self.source) {
            Ok(()) => Vec::new(),
            Err(err) => {
                let mut message = err.to_string();
                let mut source = std::error::Error::source(&err);
                while let Some(cause) = source {
                    message.push_str(&format!(": {cause}"));
                    source = cause.source();
                }
                vec![diag::max_whole_line(
                    0,
                    DiagnosticSeverity::ERROR,
                    Some("E0024"),
                    format!("Tera template error: {message}"),
                    lsp_max_protocol::LawAxis::Autopoiesis,
                )]
            }
        };
        if !self.available_vars.is_empty() {
            diags.extend(unbound_projection_diagnostics(
                &self.source,
                &self.available_vars,
            ));
        }
        diags
    }

    pub fn completion_at(&self, _line: u32, _character: u32) -> Option<CompletionResponse> {
        let mut items: Vec<CompletionItem> = FILTERS
            .iter()
            .map(|f| CompletionItem {
                label: (*f).to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("Tera filter".to_string()),
                ..Default::default()
            })
            .collect();
        for kw in KEYWORDS {
            items.push(CompletionItem {
                label: (*kw).to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            });
        }
        // Project variables: bare `VAR` for `{{ VAR }}` interpolation (the canonical
        // path). Also offer `row.VAR` as a legacy alias recognised by consumed_vars().
        for var in &self.available_vars {
            items.push(CompletionItem {
                label: var.clone(),
                kind: Some(CompletionItemKind::VARIABLE),
                detail: Some("SPARQL projection variable".to_string()),
                ..Default::default()
            });
            items.push(CompletionItem {
                label: format!("row.{var}"),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(format!(
                    "SPARQL projection variable `{var}` (via row.{var} — legacy alias)"
                )),
                ..Default::default()
            });
        }
        Some(CompletionResponse::Array(items))
    }

    pub fn hover_at(&self, line: u32, character: u32) -> Option<Hover> {
        let token = word_at(&self.source, line, character)?;
        // Strip `row.` prefix if present for variable lookup.
        let lookup = token.strip_prefix("row.").unwrap_or(&token);

        if FILTERS.iter().any(|f| *f == token) {
            return Some(Hover {
                contents: lsp_max::lsp_types::HoverContents::Markup(lsp_max::lsp_types::MarkupContent {
                    kind: lsp_max::lsp_types::MarkupKind::Markdown,
                    value: format!("**Tera filter** `{token}`\n\nSee [Tera documentation](https://keats.github.io/tera/docs/#built-in-filters) for usage."),
                }),
                range: None,
            });
        }
        if KEYWORDS.iter().any(|kw| *kw == token) {
            return Some(Hover {
                contents: lsp_max::lsp_types::HoverContents::Markup(
                    lsp_max::lsp_types::MarkupContent {
                        kind: lsp_max::lsp_types::MarkupKind::Markdown,
                        value: format!("**Tera keyword** `{token}`"),
                    },
                ),
                range: None,
            });
        }
        if self.available_vars.contains(lookup) {
            return Some(Hover {
                contents: lsp_max::lsp_types::HoverContents::Markup(lsp_max::lsp_types::MarkupContent {
                    kind: lsp_max::lsp_types::MarkupKind::Markdown,
                    value: format!("**SPARQL variable** `{lookup}`\n\nProjected from the bound rule query."),
                }),
                range: None,
            });
        }
        None
    }

    pub fn definition_at(&self, _line: u32, _character: u32) -> Option<Location> {
        None
    }

    pub fn references_at(&self, _line: u32, _character: u32) -> Option<Vec<Location>> {
        None
    }

    pub fn semantic_tokens(&self) -> Option<lsp_max::lsp_types::SemanticTokens> {
        None
    }

    pub fn document_symbols(&self, _range: Option<Range>) -> Vec<DocumentSymbol> {
        let mut symbols = Vec::new();
        for (idx, text) in self.source.lines().enumerate() {
            let line = u32::try_from(idx).unwrap_or(0);
            for (marker, kind) in [
                ("{% set ", SymbolKind::VARIABLE),
                ("{% block ", SymbolKind::NAMESPACE),
                ("{% macro ", SymbolKind::FUNCTION),
                ("{% include ", SymbolKind::FILE),
            ] {
                if let Some(pos) = text.find(marker) {
                    let rest = &text[pos + marker.len()..];
                    let name: String = rest
                        .chars()
                        .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '"' || *c == '.')
                        .collect();
                    if !name.trim().is_empty() {
                        symbols.push(make_symbol(name.trim_matches('"'), kind, line));
                    }
                }
            }
        }
        symbols
    }

    pub fn code_lenses(&self) -> Option<Vec<CodeLens>> {
        None
    }

    pub fn folding_ranges(&self) -> Option<Vec<FoldingRange>> {
        let mut ranges = Vec::new();
        let mut stack: Vec<u32> = Vec::new();
        for (idx, text) in self.source.lines().enumerate() {
            let line = u32::try_from(idx).unwrap_or(0);
            if ["{% for", "{% if", "{% block", "{% macro"]
                .iter()
                .any(|m| text.contains(m))
            {
                stack.push(line);
            }
            if ["{% endfor", "{% endif", "{% endblock", "{% endmacro"]
                .iter()
                .any(|m| text.contains(m))
            {
                if let Some(start) = stack.pop() {
                    if line > start {
                        ranges.push(FoldingRange {
                            start_line: start,
                            end_line: line,
                            start_character: None,
                            end_character: None,
                            kind: Some(FoldingRangeKind::Region),
                            collapsed_text: None,
                        });
                    }
                }
            }
        }
        if ranges.is_empty() {
            None
        } else {
            Some(ranges)
        }
    }

    pub fn format_document(&self) -> Option<Vec<TextEdit>> {
        None
    }

    pub fn inlay_hints(&self, _range: Option<Range>) -> Vec<InlayHint> {
        Vec::new()
    }

    pub fn rename_symbol(&self, _position: Position, _new_name: &str) -> Option<WorkspaceEdit> {
        None
    }

    pub fn call_hierarchy_items(&self, _position: Position) -> Option<Vec<CallHierarchyItem>> {
        None
    }
}

fn make_symbol(name: &str, kind: SymbolKind, line: u32) -> DocumentSymbol {
    let range = Range {
        start: Position { line, character: 0 },
        end: Position {
            line,
            character: u32::MAX,
        },
    };
    #[allow(deprecated)]
    DocumentSymbol {
        name: name.to_string(),
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children: None,
    }
}

/// Extract the token under the cursor — a contiguous run of Tera identifier characters.
fn word_at(source: &str, line: u32, character: u32) -> Option<String> {
    let text = source.lines().nth(line as usize)?;
    let chars: Vec<char> = text.chars().collect();
    let is_word = |c: char| c.is_alphanumeric() || matches!(c, '_' | '.' | '[' | ']' | '"' | '\'');
    let idx = character as usize;
    if idx >= chars.len() {
        return None;
    }
    let mut start = idx;
    while start > 0 && is_word(chars[start - 1]) {
        start -= 1;
    }
    let mut end = idx;
    while end < chars.len() && is_word(chars[end]) {
        end += 1;
    }
    if start == end {
        return None;
    }
    let token: String = chars[start..end].iter().collect();
    // Clean up surrounding syntax if needed.
    let token = token
        .trim_matches(|c| c == '[' || c == ']' || c == '"' || c == '\'')
        .to_string();
    if token.is_empty() {
        None
    } else {
        Some(token)
    }
}

/// Variables introduced *locally* by the template itself — loop bindings
/// (`{% for X in … %}`, including `{% for K, V in … %}`) and `{% set X = … %}`.
/// These must never count as consumed-from-query, so the detector subtracts them.
fn local_vars(template: &str) -> BTreeSet<String> {
    let mut locals = BTreeSet::new();
    for (start, _) in template.match_indices("{%") {
        let Some(rel_end) = template[start..].find("%}") else {
            continue;
        };
        let inner = template[start + 2..start + rel_end].trim();
        if let Some(rest) = inner.strip_prefix("for ") {
            // `for X in …` or `for K, V in …`
            if let Some(in_idx) = rest.find(" in ") {
                for name in rest[..in_idx].split(',') {
                    let ident = name.trim();
                    if is_identifier(ident) {
                        locals.insert(ident.to_string());
                    }
                }
            }
        } else if let Some(rest) = inner.strip_prefix("set ") {
            // `set X = …`
            let name = rest.split('=').next().unwrap_or("").trim();
            if is_identifier(name) {
                locals.insert(name.to_string());
            }
        }
    }
    locals
}

/// Variables a template *consumes* from the query result set.
///
/// Detects, at minimum:
/// - `row["name"]` / `row['name']` → `name`
/// - `{{ name }}` → `name`
/// - `{{ row.name }}` → `name`
///
/// Variables introduced locally by `{% for %}`/`{% set %}` are subtracted, so
/// loop/set bindings are never reported as unbound projections.
#[must_use]
pub fn consumed_vars(template: &str) -> BTreeSet<String> {
    let mut consumed = BTreeSet::new();

    // 1) `row["key"]` / `row['key']` bracket access.
    consumed.extend(bracket_keys(template));

    // 2) `{{ … }}` interpolation expressions.
    for (start, _) in template.match_indices("{{") {
        let Some(rel_end) = template[start..].find("}}") else {
            continue;
        };
        let expr = template[start + 2..start + rel_end].trim();
        // Strip any filter chain (`name | upper`) — only the head matters.
        let head = expr.split('|').next().unwrap_or("").trim();
        if let Some(name) = leading_var(head) {
            consumed.insert(name);
        }
    }

    // Subtract locally-introduced variables.
    for local in local_vars(template) {
        consumed.remove(&local);
    }
    consumed
}

/// Extract keys from `row["key"]` / `row['key']` access patterns.
fn bracket_keys(template: &str) -> BTreeSet<String> {
    let mut keys = BTreeSet::new();
    let bytes = template.as_bytes();
    let mut i = 0;
    while let Some(rel) = template[i..].find('[') {
        let open = i + rel;
        let after = open + 1;
        if after < bytes.len() && (bytes[after] == b'"' || bytes[after] == b'\'') {
            let quote = bytes[after];
            let key_start = after + 1;
            if let Some(close_rel) = template[key_start..].find(quote as char) {
                let key = &template[key_start..key_start + close_rel];
                if is_identifier(key) {
                    keys.insert(key.to_string());
                }
            }
        }
        i = open + 1;
    }
    keys
}

/// Given the head of a `{{ … }}` expression, return the consumed query var:
/// `name` → `name`; `row.name` → `name`; `row["name"]` handled separately;
/// literals / non-identifiers → `None`.
fn leading_var(head: &str) -> Option<String> {
    let head = head.trim();
    if head.is_empty() {
        return None;
    }
    // `row.name` (or deeper) → take the segment after the first dot.
    if let Some(dot) = head.find('.') {
        let prefix = head[..dot].trim();
        if prefix == "loop" {
            return None;
        }
        let key = head[dot + 1..]
            .split(|c: char| !(c.is_alphanumeric() || c == '_'))
            .next()
            .unwrap_or("");
        return is_identifier(key).then(|| key.to_string());
    }
    // Bare identifier `name`. Skip bracket/quote/digit-led forms.
    let ident: String = head
        .chars()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect();
    if ident == head && is_identifier(&ident) {
        if ident == "loop" {
            return None;
        }
        Some(ident)
    } else {
        None
    }
}

/// A non-empty identifier: starts with a letter/underscore, all chars
/// alphanumeric/underscore (rejects numeric literals, quoted strings, etc.).
fn is_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_alphanumeric() || c == '_')
}

/// Pure cross-surface detector.
///
/// For each variable the `template` consumes that
/// the rule's SPARQL `SELECT` does **not** produce (`available_vars`), emit a
/// GGEN-TPL-001 (`unbound_projection`) error diagnostic.
///
/// Reads no files and writes no files — a pure function over its inputs.
/// Anchored at the whole first line (line 0) for the MVP.
#[must_use]
pub fn unbound_projection_diagnostics(
    template: &str, available_vars: &BTreeSet<String>,
) -> Vec<lsp_max_protocol::MaxDiagnostic> {
    consumed_vars(template)
        .into_iter()
        .filter(|var| !available_vars.contains(var))
        .map(|var| {
            diag::max_whole_line(
                0,
                DiagnosticSeverity::ERROR,
                Some(GGEN_TPL_001),
                format!(
                    "{GGEN_TPL_001} unbound_projection: template consumes `{var}` which the \
                     rule's SPARQL SELECT does not produce"
                ),
                lsp_max_protocol::LawAxis::Domain,
            )
        })
        .collect()
}

/// Pure cross-surface detector for unbound OUTPUT-PATH projections.
///
/// For each variable the rule's `output_file` pattern consumes that the rule's
/// SPARQL `SELECT` does **not** produce (`available_vars`), emit a GGEN-OUT-001
/// (`unbound_output_path`) error diagnostic. `output_file` is a tiny Tera
/// template rendered once per SPARQL result row against the row's bindings
/// (`ggen_core::codegen::pipeline` renders it via `tera.render_str`), so the
/// SAME [`consumed_vars`] extractor that powers GGEN-TPL-001 applies verbatim.
///
/// Returns empty when `output_file` is a STATIC path (no `{{`) — `consumed_vars`
/// finds no references, so the static-path case is silent by construction (the
/// `pipeline` engine itself treats `!output_file.contains("{{")` as a no-op
/// single render). The caller ([`crate::analyzers::detect_out_001`]) additionally
/// skips rules whose `available_vars` is empty (`SELECT *` / missing query) to
/// avoid false positives where there is no introspectable producer to compare.
///
/// Reads no files and writes no files — a pure function over its inputs.
/// Anchored at the whole first line (line 0) for the MVP, on the `ggen.toml`
/// declaration surface where `output_file` lives (NOT the template, NEVER an
/// emitted output file).
#[must_use]
pub fn unbound_output_path_diagnostics(
    output_file: &str, available_vars: &BTreeSet<String>,
) -> Vec<lsp_max_protocol::MaxDiagnostic> {
    consumed_vars(output_file)
        .into_iter()
        .filter(|var| !available_vars.contains(var))
        .map(|var| {
            diag::max_whole_line(
                0,
                DiagnosticSeverity::ERROR,
                Some(GGEN_OUT_001),
                format!(
                    "{GGEN_OUT_001} unbound_output_path: output_file consumes `{var}` which the \
                     rule's SPARQL SELECT does not produce"
                ),
                lsp_max_protocol::LawAxis::Domain,
            )
        })
        .collect()
}

/// Pure detector for unbound RULE FILES (GGEN-RULE-001).
///
/// Given a rule's `issues` strings (from [`crate::rule_index::RuleIndexEntry`]),
/// emit a GGEN-RULE-001 ERROR for each MISSING-FILE issue — identified by the
/// stable prefixes `"query file missing:"` / `"template file missing:"` that the
/// index produces when a rule's `{file=...}` cannot be read (overlay-aware: an
/// open buffer for the path counts as present, so it is not flagged). Ignores
/// `"unsupported template source"` (Git/Package — a separate concern) and the
/// `"SELECT *"` info issue: neither is a missing-file defect.
///
/// Diagnostics anchor on the whole first line (line 0) of the rule's `ggen.toml`
/// declaration surface — NEVER an emitted output. Reads/writes no files: this is
/// a pure function over its inputs (the index already did the overlay-aware I/O).
#[must_use]
pub fn unbound_rule_file_diagnostics(issues: &[String]) -> Vec<lsp_max_protocol::MaxDiagnostic> {
    issues
        .iter()
        .filter(|i| i.starts_with("query file missing:") || i.starts_with("template file missing:"))
        .map(|i| {
            diag::max_whole_line(
                0,
                DiagnosticSeverity::ERROR,
                Some(GGEN_RULE_001),
                format!("{GGEN_RULE_001} unbound_rule_file: {i}"),
                lsp_max_protocol::LawAxis::Domain,
            )
        })
        .collect()
}

/// Pack-source advisory diagnostics (GGEN-PACK-001).
///
/// When a rule's query or template is supplied via `{ pack = "...", output = "...",
/// file = "..." }`, ggen-lsp cannot resolve the content at author time. GGEN-TPL-001
/// and GGEN-OUT-001 checks are therefore vacuous for the rule. This function emits
/// a WARNING for each pack-source issue recorded in `entry.issues` so the author is
/// informed that author-time provision checks are disabled.
///
/// Pure function over its inputs (no I/O).
#[must_use]
pub fn pack_001_diagnostics(issues: &[String]) -> Vec<lsp_max_protocol::MaxDiagnostic> {
    issues
        .iter()
        .filter(|i| i.starts_with("pack query (") || i.starts_with("pack template ("))
        .map(|i| {
            diag::max_whole_line(
                0,
                DiagnosticSeverity::WARNING,
                Some(GGEN_PACK_001),
                format!(
                    "{GGEN_PACK_001}: pack source resolved at generation time — \
                     GGEN-TPL-001 and GGEN-OUT-001 are disabled for this rule. \
                     Use a direct file path for author-time checks. ({i})"
                ),
                lsp_max_protocol::LawAxis::Domain,
            )
        })
        .collect()
}

/// Pure output-path-escape detector (GGEN-YIELD-001).
///
/// Strips `{{ }}` Tera interpolations from `output_file` to get the static
/// skeleton, resolves it against the manifest directory, and checks whether the
/// result escapes `project_root`. A conservatively lexical check: `..` in the
/// skeleton path always escapes regardless of variable values at render time.
///
/// Returns an ERROR diagnostic when the skeleton escapes, or when
/// `output_file` is an absolute path that starts outside `project_root`.
///
/// Reads no files, writes no files — pure over its inputs.
#[must_use]
pub fn yield_001_diagnostics(
    output_file: &str, manifest_path: &std::path::Path, project_root: &std::path::Path,
) -> Vec<lsp_max_protocol::MaxDiagnostic> {
    let skeleton = strip_tera_vars(output_file);
    let manifest_dir = manifest_path.parent().unwrap_or(std::path::Path::new("."));
    let candidate = manifest_dir.join(&skeleton);
    let cleaned = lexical_clean(&candidate);
    let root_clean = lexical_clean(project_root);
    if !cleaned.starts_with(&root_clean) {
        return vec![diag::max_whole_line(
            0,
            DiagnosticSeverity::ERROR,
            Some(GGEN_YIELD_001),
            format!(
                "{GGEN_YIELD_001} LAYER_VIOLATION: output_file `{output_file}` resolves outside \
                 the project root. Stripped skeleton: `{}`. \
                 = help: Use a path within the project (e.g. `src/{{{{ name }}}}.rs`)",
                cleaned.display()
            ),
            lsp_max_protocol::LawAxis::Domain,
        )];
    }
    Vec::new()
}

/// Pure orphaned-output detector (GGEN-YIELD-003).
///
/// Detects output paths that lack a static filename base (e.g. just `.rs` or empty
/// after stripping Tera variables).
#[must_use]
pub fn yield_003_diagnostics(output_file: &str) -> Vec<lsp_max_protocol::MaxDiagnostic> {
    let skeleton = strip_tera_vars(output_file);
    if skeleton.is_empty() || (skeleton.starts_with('.') && !skeleton.contains('/')) {
        return vec![diag::max_whole_line(
            0,
            DiagnosticSeverity::ERROR,
            Some(GGEN_YIELD_003),
            format!(
                "{GGEN_YIELD_003} ORPHANED_OUTPUT: output_file `{output_file}` lacks a static \
                 filename base. Orphaned output paths are not permitted."
            ),
            lsp_max_protocol::LawAxis::Domain,
        )];
    }
    Vec::new()
}

/// Pure remote-fetch detector (GGEN-YIELD-005).
///
/// Detects output paths that look like remote URLs (http:// or https://).
#[must_use]
pub fn yield_005_diagnostics(output_file: &str) -> Vec<lsp_max_protocol::MaxDiagnostic> {
    if output_file.starts_with("http://") || output_file.starts_with("https://") {
        return vec![diag::max_whole_line(
            0,
            DiagnosticSeverity::ERROR,
            Some(GGEN_YIELD_005),
            format!(
                "{GGEN_YIELD_005} REMOTE_FETCH: output_file `{output_file}` is a remote URL. \
                 Only local filesystem paths are permitted."
            ),
            lsp_max_protocol::LawAxis::Domain,
        )];
    }
    Vec::new()
}

/// Pure competing-authority detector (GGEN-YIELD-004).
///
/// Detects multiple rules targeting the same output file.
#[must_use]
pub fn yield_004_diagnostics(
    rule_id: &str, output_file: &str, competing_rules: &[String],
) -> Vec<lsp_max_protocol::MaxDiagnostic> {
    if competing_rules.is_empty() {
        return Vec::new();
    }
    vec![diag::max_whole_line(
        0,
        DiagnosticSeverity::ERROR,
        Some(GGEN_YIELD_004),
        format!(
            "{GGEN_YIELD_004} COMPETING_AUTHORITY: rule `{rule_id}` targets output_file \
             `{output_file}`, which is also targeted by rule(s): {}. \
             Only one rule may target a given output path.",
            competing_rules.join(", ")
        ),
        lsp_max_protocol::LawAxis::Domain,
    )]
}

/// Strip `{{ … }}` Tera interpolation blocks from `s`, returning the static skeleton.
#[must_use]
pub fn strip_tera_vars(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut rest = s;
    while let Some(open) = rest.find("{{") {
        result.push_str(&rest[..open]);
        if let Some(close_rel) = rest[open..].find("}}") {
            rest = &rest[open + close_rel + 2..];
        } else {
            result.push_str(&rest[open..]);
            return result;
        }
    }
    result.push_str(rest);
    result
}

/// Lexically collapse `.` and `..` segments in `path` without touching the
/// filesystem (no `canonicalize`). Safe to call on paths that do not exist yet.
#[must_use]
pub fn lexical_clean(path: &std::path::Path) -> std::path::PathBuf {
    let mut components: Vec<std::path::Component> = Vec::new();
    for c in path.components() {
        match c {
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                if matches!(components.last(), Some(std::path::Component::Normal(_))) {
                    components.pop();
                } else {
                    components.push(c);
                }
            }
            other => components.push(other),
        }
    }
    components.iter().collect()
}

/// Pure SELECT * blindspot advisory (GGEN-QUERY-002).
///
/// When a generation rule's query uses `SELECT *` the variable projection set
/// is unknowable at author time, silently disabling GGEN-TPL-001 / GGEN-OUT-001.
#[must_use]
pub fn select_star_diagnostics(
    rule_id: &str, query_text: &str,
) -> Vec<lsp_max_protocol::MaxDiagnostic> {
    if !is_select_star(query_text) {
        return Vec::new();
    }
    vec![diag::max_whole_line(
        0,
        DiagnosticSeverity::WARNING,
        Some(GGEN_QUERY_002),
        format!(
            "{GGEN_QUERY_002} rule `{rule_id}` uses SELECT * — explicit projections required. \
             This disables GGEN-TPL-001 and GGEN-OUT-001 for this rule."
        ),
        lsp_max_protocol::LawAxis::Domain,
    )]
}

/// Returns `true` if `query` is a `SELECT *` query (case-insensitive).
#[must_use]
pub fn is_select_star(query: &str) -> bool {
    let stripped: String = query
        .lines()
        .filter(|l| !l.trim_start().starts_with('#'))
        .collect::<Vec<_>>()
        .join(" ");
    let upper = stripped.to_uppercase();
    let after_select = match upper.find("SELECT") {
        Some(pos) => &upper[pos + 6..],
        None => return false,
    };
    let projection = after_select
        .trim_start()
        .trim_start_matches("DISTINCT")
        .trim_start();
    projection.starts_with('*')
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_max::lsp_types::NumberOrString;
    use lsp_max_protocol::MaxDiagnostic;

    fn vars(names: &[&str]) -> BTreeSet<String> {
        names.iter().map(|s| (*s).to_string()).collect()
    }

    fn has_tpl_001(diags: &[MaxDiagnostic]) -> bool {
        diags
            .iter()
            .any(|d| d.lsp.code == Some(NumberOrString::String(GGEN_TPL_001.to_string())))
    }

    fn has_out_001(diags: &[MaxDiagnostic]) -> bool {
        diags
            .iter()
            .any(|d| d.lsp.code == Some(NumberOrString::String(GGEN_OUT_001.to_string())))
    }

    #[test]
    fn available_name_consumed_name_bracket_no_diagnostic() {
        let t = r#"{{ row["name"] }}"#;
        let diags = unbound_projection_diagnostics(t, &vars(&["name"]));
        assert!(diags.is_empty(), "expected no diagnostic, got {diags:?}");
    }

    #[test]
    fn available_name_consumed_title_one_tpl_001() {
        let t = r#"{{ row["title"] }}"#;
        let diags = unbound_projection_diagnostics(t, &vars(&["name"]));
        assert_eq!(diags.len(), 1, "expected exactly one diagnostic: {diags:?}");
        assert!(has_tpl_001(&diags));
        assert!(diags[0].lsp.message.contains("title"));
    }

    #[test]
    fn both_vars_available_no_diagnostic() {
        let t = "{{ name }} - {{ row.title }}";
        let diags = unbound_projection_diagnostics(t, &vars(&["name", "title"]));
        assert!(diags.is_empty(), "expected no diagnostic, got {diags:?}");
    }

    #[test]
    fn empty_available_with_consumed_var_triggers_tpl_001() {
        let t = "{{ x }}";
        let diags = unbound_projection_diagnostics(t, &BTreeSet::new());
        assert_eq!(diags.len(), 1, "expected one diagnostic: {diags:?}");
        assert!(has_tpl_001(&diags));
        assert!(diags[0].lsp.message.contains('x'));
    }

    #[test]
    fn loop_local_var_is_not_consumed() {
        let t = "{% for x in items %}{{ x }}{% endfor %}";
        let diags = unbound_projection_diagnostics(t, &vars(&["items"]));
        assert!(
            diags.is_empty(),
            "loop-local `x` must not be an unbound projection: {diags:?}"
        );
    }

    #[test]
    fn set_local_var_is_not_consumed() {
        let t = "{% set y = name %}{{ y }}";
        // `name` IS consumed (rhs of set), `y` is local.
        let diags = unbound_projection_diagnostics(t, &vars(&["name"]));
        assert!(diags.is_empty(), "expected no diagnostic, got {diags:?}");
    }

    #[test]
    fn live_diagnostics_surface_tpl_001_with_bindings() {
        // When built with real SPARQL bindings, diagnostics() includes GGEN-TPL-001.
        let analyzer =
            TeraAnalyzer::new_from_content(r#"{{ row["missing"] }}"#, "SELECT ?name WHERE")
                .expect("analyzer");
        assert!(has_tpl_001(&analyzer.diagnostics()));
    }

    #[test]
    fn valid_template_has_no_diagnostics() {
        let t = "Hello {{ name }}\n{% for x in items %}{{ x }}{% endfor %}\n";
        let analyzer = TeraAnalyzer::new_from_content(t, "").expect("analyzer");
        assert!(analyzer.diagnostics().is_empty());
    }

    #[test]
    fn unclosed_block_reports_e0024() {
        let t = "{% for x in items %}{{ x }}\n"; // missing endfor
        let analyzer = TeraAnalyzer::new_from_content(t, "").expect("analyzer");
        let diags = analyzer.diagnostics();
        assert!(!diags.is_empty());
        assert_eq!(
            diags[0].lsp.code,
            Some(lsp_max::lsp_types::NumberOrString::String("E0024".into()))
        );
    }

    // ───────────── GGEN-OUT-001: unbound output-path projection ──────────────

    #[test]
    fn output_path_bound_var_no_diagnostic() {
        // The canonical fixture pattern: `output_file = "src/{{name}}.rs"` where
        // `name` IS projected by the SELECT → no OUT-001.
        let diags = unbound_output_path_diagnostics("src/models/{{name}}.rs", &vars(&["name"]));
        assert!(diags.is_empty(), "expected no diagnostic, got {diags:?}");
    }

    #[test]
    fn output_path_unbound_var_one_out_001() {
        // `missing` is consumed by the output path but not projected → 1 OUT-001.
        let diags = unbound_output_path_diagnostics("src/{{missing}}.rs", &vars(&["name"]));
        assert_eq!(diags.len(), 1, "expected exactly one diagnostic: {diags:?}");
        assert!(has_out_001(&diags));
        assert!(diags[0].lsp.message.contains("missing"));
        assert_eq!(diags[0].lsp.severity, Some(DiagnosticSeverity::ERROR));
    }

    #[test]
    fn static_output_path_is_silent_by_construction() {
        // No `{{ }}` → `consumed_vars` finds nothing → zero diagnostics, even
        // when `available_vars` is non-empty.
        let diags = unbound_output_path_diagnostics("src/lib.rs", &vars(&["name"]));
        assert!(
            diags.is_empty(),
            "a static output_file must never raise OUT-001: {diags:?}"
        );
    }

    #[test]
    fn output_path_partial_binding_reports_only_unbound() {
        // `{{a}}` is bound, `{{b}}` is not → exactly one OUT-001 (for `b`).
        let diags = unbound_output_path_diagnostics("{{a}}/{{b}}.rs", &vars(&["a"]));
        assert_eq!(diags.len(), 1, "only the unbound `b`: {diags:?}");
        assert!(diags[0].lsp.message.contains('b'));
        assert!(!diags[0].lsp.message.contains("`a`"));
    }

    #[test]
    fn output_path_empty_vars_dynamic_reports_per_consumed_var() {
        // A direct call with empty vars + a dynamic output path reports each
        // consumed var. (The index-level detector skips empty-vars rules; this
        // documents the pure function's own contract.)
        let diags = unbound_output_path_diagnostics("out/{{slug}}.txt", &BTreeSet::new());
        assert_eq!(diags.len(), 1, "expected one diagnostic: {diags:?}");
        assert!(has_out_001(&diags));
        assert!(diags[0].lsp.message.contains("slug"));
    }

    #[test]
    fn out_001_and_tpl_001_codes_are_distinct() {
        // The two detectors emit DISTINCT codes — no cross-surface confusion.
        let out = unbound_output_path_diagnostics("{{x}}.rs", &BTreeSet::new());
        let tpl = unbound_projection_diagnostics("{{x}}", &BTreeSet::new());
        assert!(has_out_001(&out) && !has_tpl_001(&out));
        assert!(has_tpl_001(&tpl) && !has_out_001(&tpl));
    }

    // ---- GGEN-RULE-001: unbound rule file ----

    fn has_rule_001(diags: &[MaxDiagnostic]) -> bool {
        diags
            .iter()
            .any(|d| d.lsp.code == Some(NumberOrString::String(GGEN_RULE_001.to_string())))
    }

    #[test]
    fn missing_query_issue_raises_one_rule_001() {
        let issues = vec!["query file missing: /p/queries/items.rq (No such file)".to_string()];
        let diags = unbound_rule_file_diagnostics(&issues);
        assert_eq!(diags.len(), 1, "expected one RULE-001: {diags:?}");
        assert!(has_rule_001(&diags));
        assert_eq!(diags[0].lsp.severity, Some(DiagnosticSeverity::ERROR));
        assert!(diags[0].lsp.message.contains("query file missing"));
    }

    #[test]
    fn missing_template_issue_raises_one_rule_001() {
        let issues = vec!["template file missing: /p/templates/missing.tera".to_string()];
        let diags = unbound_rule_file_diagnostics(&issues);
        assert_eq!(diags.len(), 1, "expected one RULE-001: {diags:?}");
        assert!(has_rule_001(&diags));
        assert!(diags[0].lsp.message.contains("template file missing"));
    }

    #[test]
    fn unsupported_git_source_issue_raises_zero_rule_001() {
        // Git/Package sources are a separate "unsupported MVP" concern, NOT a
        // missing-file defect — RULE-001 must NOT fire on them.
        let issues =
            vec!["unsupported template source for MVP: git (https://x#a.tera)".to_string()];
        let diags = unbound_rule_file_diagnostics(&issues);
        assert!(diags.is_empty(), "git source is not RULE-001: {diags:?}");
    }

    #[test]
    fn select_star_issue_raises_zero_rule_001() {
        // The `SELECT *` info issue is not a missing-file defect.
        let issues =
            vec!["SELECT * is not introspectable: no explicit projection variables".to_string()];
        let diags = unbound_rule_file_diagnostics(&issues);
        assert!(diags.is_empty(), "SELECT * is not RULE-001: {diags:?}");
    }

    #[test]
    fn empty_issues_raise_zero_rule_001() {
        assert!(unbound_rule_file_diagnostics(&[]).is_empty());
    }

    #[test]
    fn both_missing_files_raise_two_rule_001() {
        let issues = vec![
            "query file missing: /p/q.rq (nope)".to_string(),
            "template file missing: /p/t.tera".to_string(),
        ];
        let diags = unbound_rule_file_diagnostics(&issues);
        assert_eq!(diags.len(), 2, "both bindings dangling: {diags:?}");
        assert!(diags.iter().all(|d| has_rule_001(std::slice::from_ref(d))));
    }

    // ───────────── GGEN-YIELD-001: output_file path escape ───────────────────

    #[test]
    fn yield_001_escaping_path_emits_error() {
        use std::path::Path;
        let diags = yield_001_diagnostics(
            "../../escaped/{{ name }}.rs",
            Path::new("/project/ggen.toml"),
            Path::new("/project"),
        );
        assert!(!diags.is_empty(), "expected YIELD-001 diagnostic, got none");
        assert!(
            diags[0].lsp.message.contains("GGEN-YIELD-001"),
            "message must contain GGEN-YIELD-001: {}",
            diags[0].lsp.message
        );
    }

    #[test]
    fn yield_001_in_project_path_is_clean() {
        use std::path::Path;
        let diags = yield_001_diagnostics(
            "src/{{ name }}.rs",
            Path::new("/project/ggen.toml"),
            Path::new("/project"),
        );
        assert!(
            diags.is_empty(),
            "in-project path must not raise YIELD-001: {diags:?}"
        );
    }

    #[test]
    fn strip_tera_vars_removes_interpolations() {
        assert_eq!(strip_tera_vars("src/{{ name }}.rs"), "src/.rs");
        assert_eq!(strip_tera_vars("static.rs"), "static.rs");
        assert_eq!(strip_tera_vars("{{ a }}/{{ b }}.rs"), "/.rs");
    }

    #[test]
    fn lexical_clean_collapses_dotdot() {
        use std::path::PathBuf;
        let p = lexical_clean(std::path::Path::new("/project/src/../escaped"));
        assert_eq!(p, PathBuf::from("/project/escaped"));
    }

    // ───────────── GGEN-QUERY-002: SELECT * blindspot ────────────────────────

    #[test]
    fn select_star_emits_query_002() {
        let diags = select_star_diagnostics("my-rule", "SELECT * WHERE { ?s ?p ?o }");
        assert!(!diags.is_empty(), "expected QUERY-002 diagnostic, got none");
        assert!(
            diags[0].lsp.message.contains("GGEN-QUERY-002"),
            "message must contain GGEN-QUERY-002: {}",
            diags[0].lsp.message
        );
        assert!(
            diags[0].lsp.message.contains("my-rule"),
            "message must contain the rule id: {}",
            diags[0].lsp.message
        );
    }

    #[test]
    fn select_distinct_star_emits_query_002() {
        let diags = select_star_diagnostics("r", "SELECT DISTINCT * WHERE { ?s ?p ?o }");
        assert!(
            !diags.is_empty(),
            "SELECT DISTINCT * must also raise QUERY-002: {diags:?}"
        );
    }

    #[test]
    fn explicit_select_does_not_emit_query_002() {
        let diags = select_star_diagnostics("r", "SELECT ?name ?label WHERE { ?s :name ?name }");
        assert!(
            diags.is_empty(),
            "explicit projections must not raise QUERY-002: {diags:?}"
        );
    }

    #[test]
    fn empty_query_does_not_emit_query_002() {
        let diags = select_star_diagnostics("r", "");
        assert!(
            diags.is_empty(),
            "empty query must not raise QUERY-002: {diags:?}"
        );
    }
}
