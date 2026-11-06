//! Ontology-driven semantic tokens for ggen-generated Rust surfaces
//! (reverse-80/20 §4: `textDocument/semanticTokens` full + delta).
//!
//! Self-contained module (no `crate::` references) so integration lanes can
//! wire it into `lib.rs`/handlers without merge conflict with this file's
//! owner. Sources of truth:
//!
//! * Legend: derived from `.specify/repo-facts.ttl` `rf:` individuals
//!   (`rf:Crate` individuals -> `crate` token type; `rf:DiagnosticCode`
//!   individuals -> `diagnostic-code` token type), plus the tree-sitter-rust
//!   grammar kinds (`module`/`struct`/`fn`/`const`) the parser can actually
//!   classify. A legend entry exists only when its referent exists.
//! * Classification: a real `tree-sitter-rust` parse of the document text.
//! * `ggen.toml` generation rules: every rule `output_file` ending in `.rs`
//!   is a generated Rust surface; tokens inside such a document carry the
//!   `generated` modifier (bit 2).
//! * Delta: resultId + relative-encoded edits per LSP 3.16. The tests
//!   round-trip full -> delta -> apply -> decode against the real generated
//!   fixture (`crates/ggen-cli/src/generated_commands.rs`).

use std::collections::{BTreeMap, BTreeSet};

use lsp_max_protocol::lsp_3_18::{SemanticTokens, SemanticTokensDelta, SemanticTokensEdit};
use oxigraph::sparql::QueryResults;
use oxigraph::store::Store;
use tree_sitter::{Node, Parser};

// ---------------------------------------------------------------------------
// Legend
// ---------------------------------------------------------------------------

/// Ontology-driven legend. The numeric index of each entry in
/// [`Legend::token_types`] is the bit-packed `token_type` value emitted in
/// `data`. Order is deterministic: fixed candidate order, filtered by
/// ontology presence, so identical ontology state yields identical indices.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Legend {
    pub token_types: Vec<String>,
    pub modifiers: Vec<String>,
}

impl Legend {
    /// Index of a token type in the legend, if present.
    #[must_use]
    pub fn index_of(&self, token_type: &str) -> Option<u32> {
        self.token_types
            .iter()
            .position(|t| t == token_type)
            .map(|p| p as u32)
    }

    /// Modifier bit position, by name.
    #[must_use]
    pub fn modifier_bit(&self, name: &str) -> Option<u32> {
        self.modifiers
            .iter()
            .position(|m| m == name)
            .map(|p| p as u32)
    }
}

/// Derive the legend from real ontology facts.
///
/// `has_crate_individuals` / `has_diagnostic_codes` come from an oxigraph
/// parse of `repo-facts.ttl`. `module`/`struct`/`fn`/`const` are present
/// because a real tree-sitter-rust parser backs them.
#[must_use]
pub fn legend_from_facts(has_crate_individuals: bool, has_diagnostic_codes: bool) -> Legend {
    let mut token_types = Vec::new();
    if has_crate_individuals {
        token_types.push("crate".to_owned());
    }
    // Grammar kinds, fixed order.
    for name in ["module", "struct", "fn", "const"] {
        token_types.push(name.to_owned());
    }
    if has_diagnostic_codes {
        token_types.push("diagnostic-code".to_owned());
    }
    Legend {
        token_types,
        modifiers: vec![
            "declaration".to_owned(),
            "definition".to_owned(),
            "generated".to_owned(),
        ],
    }
}

// ---------------------------------------------------------------------------
// Ontology facts (real oxigraph parse of repo-facts.ttl + ggen.toml rules)
// ---------------------------------------------------------------------------

const RF: &str = "http://ggen.org/repo-facts#";

/// Facts extracted from the workspace ontology and generation rules.
#[derive(Debug, Clone, Default)]
pub struct OntologyFacts {
    /// `rf:package` literals of `rf:Crate` individuals (e.g. "ggen-engine").
    pub crate_packages: BTreeSet<String>,
    /// Diagnostic code strings (E0010, GGEN-TPL-001, ...) mined from
    /// `rf:DiagnosticCode` individuals' `rf:code` literals, with `**`
    /// decoration stripped and multi-code literals ("E0011 / E0013") split.
    pub diagnostic_codes: BTreeSet<String>,
    /// `output_file` values (ending `.rs`) of `[[generation.rules]]` in
    /// `ggen.toml` — the generated Rust surfaces.
    pub generated_rust_files: BTreeSet<String>,
}

fn extract_code_tokens(literal: &str) -> Vec<String> {
    let mut codes = Vec::new();
    for piece in literal.split(|c: char| !(c.is_ascii_alphanumeric() || c == '-')) {
        if piece.is_empty() {
            continue;
        }
        let digits = &piece[1..];
        let is_ecode = piece.len() == 5
            && piece.starts_with('E')
            && digits.bytes().all(|b| b.is_ascii_digit());
        let is_gcode = piece.starts_with("GGEN-") && piece.len() > 5;
        if is_ecode || is_gcode {
            codes.push(piece.to_owned());
        }
    }
    codes
}

/// Parse the repo-facts graph via oxigraph + SPARQL (real graph parse, not
/// regex over the TTL text).
///
/// `ggen_toml` (optional) supplies `[[generation.rules]]` output files.
#[must_use]
pub fn parse_ontology_facts(ttl: &str, ggen_toml: Option<&str>) -> OntologyFacts {
    let mut facts = OntologyFacts::default();

    if let Ok(store) = Store::new() {
        match store.load_from_reader(
            oxigraph::io::RdfParser::from_format(oxigraph::io::RdfFormat::Turtle),
            ttl.as_bytes(),
        ) {
            Ok(()) => {}
            Err(e) => eprintln!("tokens: repo-facts.ttl parse failed: {e}"),
        }

        // Crate packages.
        let crate_query =
            "SELECT ?pkg WHERE { ?c a <{RF}Crate> . ?c <{RF}package> ?pkg }".replace("{RF}", RF);
        let crate_eval = oxigraph::sparql::SparqlEvaluator::new().parse_query(&crate_query);
        match crate_eval.map(|q| q.on_store(&store).execute()) {
            Err(e) => eprintln!("tokens: crate query failed: {e}"),
            Ok(results) => {
                if let Ok(QueryResults::Solutions(mut sols)) = results {
                    while let Some(Ok(sol)) = sols.next() {
                        if let Some(oxigraph::model::Term::Literal(lit)) = sol.get("pkg") {
                            facts
                                .crate_packages
                                .insert(lit.value().trim_matches('"').to_owned());
                        }
                    }
                }
            }
        }

        // Diagnostic codes.
        let code_query = "SELECT ?code WHERE { ?c a <{RF}DiagnosticCode> . ?c <{RF}code> ?code }"
            .replace("{RF}", RF);
        let code_eval = oxigraph::sparql::SparqlEvaluator::new().parse_query(&code_query);
        match code_eval.map(|q| q.on_store(&store).execute()) {
            Err(e) => eprintln!("tokens: code query failed: {e}"),
            Ok(results) => {
                if let Ok(QueryResults::Solutions(mut sols)) = results {
                    while let Some(Ok(sol)) = sols.next() {
                        if let Some(oxigraph::model::Term::Literal(lit)) = sol.get("code") {
                            for code in extract_code_tokens(lit.value()) {
                                facts.diagnostic_codes.insert(code);
                            }
                        }
                    }
                }
            }
        }
    }

    // Generation rules whose output is Rust (toml crate — same parser
    // ggen-config uses).
    if let Some(toml_text) = ggen_toml {
        if let Ok(value) = toml::from_str::<toml::Value>(toml_text) {
            if let Some(rules) = value
                .get("generation")
                .and_then(|g| g.get("rules"))
                .and_then(|r| r.as_array())
            {
                for rule in rules {
                    if let Some(out) = rule.get("output_file").and_then(|o| o.as_str()) {
                        if out.ends_with(".rs") {
                            facts.generated_rust_files.insert(out.to_owned());
                        }
                    }
                }
            }
        }
    }

    facts
}

// ---------------------------------------------------------------------------
// Rust classification (real tree-sitter-rust parse)
// ---------------------------------------------------------------------------

/// An absolute token before delta encoding: UTF-16 coordinates (LSP default
/// position encoding) plus the source byte range for text extraction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct AbsToken {
    pub line: u32,
    pub start: u32,
    pub length: u32,
    pub token_type: u32,
    pub modifiers: u32,
    pub byte_start: usize,
    pub byte_end: usize,
}

fn utf16_col(source: &str, line_start_byte: usize, byte_col: usize) -> u32 {
    let line_text = &source[line_start_byte..line_start_byte + byte_col];
    u32::try_from(line_text.encode_utf16().count()).unwrap_or(u32::MAX)
}

/// Classify a real tree-sitter-rust parse into absolute tokens.
///
/// Emits:
/// * `fn`/`struct`/`const`/`module` for the name child of the corresponding
///   declaration item nodes (`definition` modifier; `declaration` for bare
///   `mod x;` items).
/// * `crate` for identifiers naming an `rf:Crate` package from the ontology.
/// * `diagnostic-code` for occurrences of an `rf:DiagnosticCode` string
///   inside identifier / string / comment leaves.
///
/// Returns an empty vector when the parser cannot be constructed or the
/// document is `None`-parseable (parse errors are skipped, not fatal).
#[must_use]
pub fn classify_rust(
    source: &str, legend: &Legend, facts: &OntologyFacts, is_generated: bool,
) -> Vec<AbsToken> {
    let mut out: Vec<AbsToken> = Bitpack::new(legend, facts, is_generated).run(source);
    out.sort_unstable_by_key(|t| (t.line, t.start));
    out
}

/// Walker over a real tree-sitter-rust tree.
struct Bitpack<'a> {
    legend: &'a Legend,
    facts: &'a OntologyFacts,
    generated_bit: u32,
    out: Vec<AbsToken>,
}

impl<'a> Bitpack<'a> {
    fn new(legend: &'a Legend, facts: &'a OntologyFacts, is_generated: bool) -> Self {
        Self {
            legend,
            facts,
            generated_bit: if is_generated {
                legend
                    .modifier_bit("generated")
                    .map(|b| 1 << b)
                    .unwrap_or(0)
            } else {
                0
            },
            out: Vec::new(),
        }
    }

    fn run(mut self, source: &str) -> Vec<AbsToken> {
        let mut parser = Parser::new();
        if parser
            .set_language(&tree_sitter_rust::LANGUAGE.into())
            .is_err()
        {
            return Vec::new();
        }
        let Some(tree) = parser.parse(source, None) else {
            return Vec::new();
        };
        self.visit(tree.root_node(), source);
        self.out
    }

    fn visit(&mut self, node: Node, source: &str) {
        self.visit_one(node, source);
        let mut child = node.child(0);
        while let Some(c) = child {
            self.visit(c, source);
            child = c.next_sibling();
        }
    }

    fn visit_one(&mut self, node: Node, source: &str) {
        let kind = node.kind();
        let idx = |name: &str| self.legend.index_of(name);
        let def_bit = self
            .legend
            .modifier_bit("definition")
            .map(|b| 1 << b)
            .unwrap_or(0);
        let decl_bit = self
            .legend
            .modifier_bit("declaration")
            .map(|b| 1 << b)
            .unwrap_or(0);
        let gen = self.generated_bit;

        // Declaration name child.
        let name_node = node.child_by_field_name("name");
        let token_type = match kind {
            "function_item" => idx("fn"),
            "struct_item" | "enum_item" | "union_item" => idx("struct"),
            "const_item" => idx("const"),
            "mod_item" => idx("module"),
            _ => None,
        };
        if let (Some(tt), Some(name)) = (token_type, name_node) {
            let mods = (if kind == "mod_item" {
                decl_bit
            } else {
                def_bit
            }) | gen;
            self.push(name, tt, mods, source);
        }

        // crate references: identifiers naming a known rf:Crate package.
        if kind == "identifier" {
            let text = &source[node.start_byte()..node.end_byte()];
            // rf:package literals are crate names ("ggen-engine"); Rust
            // identifiers use underscores ("ggen_engine").
            let rust_ident = text.replace('_', "-");
            if self.facts.crate_packages.contains(&rust_ident) {
                if let Some(tt) = idx("crate") {
                    self.push(node, tt, gen, source);
                    return;
                }
            }
        }

        // diagnostic-code occurrences inside identifier/string/comment leaves.
        if matches!(
            kind,
            "identifier" | "string_literal" | "line_comment" | "doc_comment" | "string_content"
        ) {
            let text = &source[node.start_byte()..node.end_byte()];
            for code in &self.facts.diagnostic_codes {
                if text.contains(code.as_str()) {
                    if let Some(tt) = idx("diagnostic-code") {
                        self.push(node, tt, gen, source);
                    }
                    break;
                }
            }
        }
    }

    fn push(&mut self, node: Node, token_type: u32, modifiers: u32, source: &str) {
        let line = node.start_position().row;
        let line_start = source[..node.start_byte()]
            .rfind('\n')
            .map(|i| i + 1)
            .unwrap_or(0);
        let start = utf16_col(source, line_start, node.start_byte() - line_start);
        let length = utf16_len_of(&source[node.start_byte()..node.end_byte()]);
        self.out.push(AbsToken {
            line: u32::try_from(line).unwrap_or(u32::MAX),
            start,
            length,
            token_type,
            modifiers,
            byte_start: 0,
            byte_end: 0,
        });
    }
}

fn utf16_len_of(s: &str) -> u32 {
    u32::try_from(s.encode_utf16().count()).unwrap_or(u32::MAX)
}

// ---------------------------------------------------------------------------
// Encoding / decoding (LSP relative delta encoding)
// ---------------------------------------------------------------------------

/// Encode absolute tokens into the LSP bit-packed relative `data` array.
#[must_use]
pub fn encode(tokens: &[AbsToken]) -> Vec<u32> {
    let mut data = Vec::with_capacity(tokens.len() * 5);
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;
    for t in tokens {
        let dl = t.line - prev_line;
        let ds = if dl == 0 {
            t.start - prev_start
        } else {
            t.start
        };
        data.push(dl);
        data.push(ds);
        data.push(t.length);
        data.push(t.token_type);
        data.push(t.modifiers);
        prev_line = t.line;
        prev_start = t.start;
    }
    data
}

/// Decode a relative `data` array back into absolute tokens.
///
/// `byte_start`/`byte_end` are left 0 (decoded tokens carry no byte range).
#[must_use]
pub fn decode(data: &[u32]) -> Vec<AbsToken> {
    let mut out = Vec::with_capacity(data.len() / 5);
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;
    for chunk in data.chunks_exact(5) {
        let line = prev_line + chunk[0];
        let start = if chunk[0] == 0 {
            prev_start + chunk[1]
        } else {
            chunk[1]
        };
        prev_line = line;
        prev_start = start;
        out.push(AbsToken {
            line,
            start,
            length: chunk[2],
            token_type: chunk[3],
            modifiers: chunk[4],
            byte_start: 0,
            byte_end: 0,
        });
    }
    out
}

// ---------------------------------------------------------------------------
// Delta (resultId + relative-encoded edits, LSP 3.16)
// ---------------------------------------------------------------------------

/// One generation of tokens for a document.
#[derive(Debug, Clone)]
struct Generation {
    result_id: String,
    data: Vec<u32>,
}

/// Token engine: full + delta for ggen-generated Rust surfaces, with
/// per-document resultId state.
#[derive(Debug)]
pub struct TokensEngine {
    legend: Legend,
    facts: OntologyFacts,
    generations: BTreeMap<String, Generation>,
    counter: u64,
}

impl TokensEngine {
    /// Build from a real ontology parse.
    #[must_use]
    pub fn from_ontology(ttl: &str, ggen_toml: Option<&str>) -> Self {
        let facts = parse_ontology_facts(ttl, ggen_toml);
        let legend = legend_from_facts(
            !facts.crate_packages.is_empty(),
            !facts.diagnostic_codes.is_empty(),
        );
        Self {
            legend,
            facts,
            generations: BTreeMap::new(),
            counter: 0,
        }
    }

    /// The ontology-derived legend.
    #[must_use]
    pub fn legend(&self) -> &Legend {
        &self.legend
    }

    /// The set of `.rs` outputs declared by `ggen.toml` `[[generation.rules]]`
    /// (paths as written in the manifest).
    #[must_use]
    pub fn declared_generated(&self) -> BTreeSet<String> {
        self.facts.generated_rust_files.clone()
    }

    /// Whether `uri` names an ontology-declared generated `.rs` surface.
    #[must_use]
    pub fn is_generated_uri(&self, uri: &str) -> bool {
        self.is_generated(uri)
    }

    fn is_generated(&self, uri: &str) -> bool {
        self.facts
            .generated_rust_files
            .iter()
            .any(|out| uri.ends_with(out))
    }

    fn next_result_id(&mut self) -> String {
        self.counter += 1;
        format!("tok-{}", self.counter)
    }

    fn mint(&mut self, uri: &str, text: &str) -> Generation {
        let abs = classify_rust(text, &self.legend, &self.facts, self.is_generated(uri));
        Generation {
            result_id: self.next_result_id(),
            data: encode(&abs),
        }
    }

    /// `textDocument/semanticTokens` (full): classify + encode + store a
    /// generation for `uri`.
    pub fn full(&mut self, uri: &str, text: &str) -> SemanticTokens {
        let gen = self.mint(uri, text);
        let result_id = gen.result_id.clone();
        let data = gen.data.clone();
        self.generations.insert(uri.to_owned(), gen);
        SemanticTokens {
            result_id: Some(result_id),
            data,
        }
    }

    /// `textDocument/semanticTokens/full/delta`.
    ///
    /// When `previous_result_id` matches the stored generation for `uri`,
    /// emits a minimal common-prefix/suffix diff; otherwise emits a single
    /// full-replacement edit. Always records the new generation.
    pub fn delta(
        &mut self, uri: &str, previous_result_id: &str, new_text: &str,
    ) -> SemanticTokensDelta {
        let gen = self.mint(uri, new_text);
        let new_result_id = gen.result_id.clone();
        let edits = match self.generations.get(uri) {
            Some(old) if old.result_id == previous_result_id => diff_edits(&old.data, &gen.data),
            Some(old) => vec![replace_all_edit(&old.data, &gen.data)],
            None => vec![SemanticTokensEdit {
                start: 0,
                delete_count: 0,
                data: Some(gen.data.clone()),
            }],
        };
        let stored = Generation {
            result_id: new_result_id.clone(),
            data: gen.data.clone(),
        };
        self.generations.insert(uri.to_owned(), stored);
        SemanticTokensDelta {
            result_id: Some(new_result_id),
            edits,
        }
    }
}

/// Diff two encoded arrays: keep the common token prefix and suffix, replace
/// the middle with one edit. Coordinates are array-element offsets (5 per
/// token), exactly what `SemanticTokensEdit` speaks.
#[must_use]
pub fn diff_edits(old: &[u32], new: &[u32]) -> Vec<SemanticTokensEdit> {
    if old == new {
        return Vec::new();
    }
    // Token granularity (chunks of 5).
    let old_tokens = old.len() / 5;
    let new_tokens = new.len() / 5;
    // Common prefix in tokens.
    let mut prefix = 0usize;
    while prefix < old_tokens
        && prefix < new_tokens
        && old[prefix * 5..prefix * 5 + 5] == new[prefix * 5..prefix * 5 + 5]
    {
        prefix += 1;
    }
    // Common suffix in tokens (not crossing the prefix).
    let mut suffix = 0usize;
    while suffix < old_tokens - prefix
        && suffix < new_tokens - prefix
        && old[(old_tokens - 1 - suffix) * 5..(old_tokens - suffix) * 5]
            == new[(new_tokens - 1 - suffix) * 5..(new_tokens - suffix) * 5]
    {
        suffix += 1;
    }
    let delete_count = (old_tokens - prefix - suffix) * 5;
    let start = prefix * 5;
    let data = new[(prefix * 5)..(new_tokens - suffix) * 5].to_vec();
    vec![SemanticTokensEdit {
        start: start as u32,
        delete_count: delete_count as u32,
        data: Some(data),
    }]
}

fn replace_all_edit(old: &[u32], new: &[u32]) -> SemanticTokensEdit {
    SemanticTokensEdit {
        start: 0,
        delete_count: old.len() as u32,
        data: Some(new.to_vec()),
    }
}

/// Spec-shaped client-side application of `SemanticTokensEdit`s to a
/// previous encoded array. Edits are applied in the order given, each
/// against the array as it stands after the previous edits (per LSP).
///
/// Returns `None` when an edit reaches past the end of the current array.
#[must_use]
pub fn apply_edits(old: &[u32], edits: &[SemanticTokensEdit]) -> Option<Vec<u32>> {
    let mut out: Vec<u32> = old.to_vec();
    for edit in edits {
        let start = edit.start as usize;
        let end = start + edit.delete_count as usize;
        if end > out.len() {
            return None;
        }
        let mut next = out[..start].to_vec();
        if let Some(data) = &edit.data {
            next.extend_from_slice(data);
        }
        next.extend_from_slice(&out[end..]);
        out = next;
    }
    Some(out)
}
