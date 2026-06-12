//! Code lenses — actionable, count-bearing annotations above ggen law surfaces.
//!
//! A code lens is a `{ range, command, data }` triple the editor renders as a
//! clickable line above the code it annotates. Here every lens carries a REAL
//! computed value (a property count, a variable count, a key count) derived by
//! actually parsing the document text — never a hardcoded label. Where an action
//! is meaningful (re-checking a class shape, running the headless gate over a
//! query) the lens carries the `ggen.lsp.check` command with the document URI as
//! its argument; pure count lenses carry `command: None`.
//!
//! The command id `"ggen.lsp.check"` is the editor-facing handle for ggen's
//! `ggen lsp check` gate — the same law that diagnostics surface, offered as a
//! one-click re-verification.
//!
//! This module is a pure function over `(FileType, &str)`, mirroring the analyzer
//! contract, so the interactive server and any headless caller see identical
//! lenses.

use lsp_max::lsp_types::{CodeLens, Command, Position, Range};
use serde_json::Value;

use crate::state::FileType;

/// The editor command id wired to `ggen lsp check`.
pub const CHECK_COMMAND: &str = "ggen.lsp.check";

/// Compute the code lenses for a document of `file_type` with the given `content`.
///
/// Returns `None` when the surface carries no lensable structure (so the server
/// can answer `null` cleanly). At least the Rdf, Sparql, and Toml surfaces return
/// real, computed lenses.
#[must_use]
pub fn code_lenses(file_type: FileType, content: &str) -> Option<Vec<CodeLens>> {
    let lenses = match file_type {
        FileType::Rdf => rdf_lenses(content),
        FileType::Sparql => sparql_lenses(content),
        FileType::Toml => toml_lenses(content),
        // Tera/Unknown carry no class/query/section structure to lens.
        FileType::Tera | FileType::Unknown => Vec::new(),
    };
    if lenses.is_empty() {
        None
    } else {
        Some(lenses)
    }
}

// --- RDF -------------------------------------------------------------------

/// Above each class declaration, a `"{N} properties · check shape"` lens.
///
/// A class declaration is a subject typed as a class:
/// `Subject a owl:Class` / `a rdfs:Class` / `Subject rdf:type owl:Class` (or
/// `rdfs:Class`). The property count is the number of properties whose
/// `rdfs:domain` is that subject — the real shape of the class as declared in the
/// document. The lens action re-checks the shape via `ggen lsp check`.
fn rdf_lenses(content: &str) -> Vec<CodeLens> {
    let classes = declared_classes(content);
    if classes.is_empty() {
        return Vec::new();
    }
    let domains = domain_counts(content);
    classes
        .into_iter()
        .map(|(line, subject)| {
            let count = domains.get(&subject).copied().unwrap_or(0);
            let noun = if count == 1 { "property" } else { "properties" };
            CodeLens {
                range: whole_line(line),
                command: Some(Command {
                    title: format!("{count} {noun} · check shape"),
                    command: CHECK_COMMAND.to_string(),
                    arguments: Some(vec![Value::String(subject)]),
                }),
                data: None,
            }
        })
        .collect()
}

/// Find `(line, subject)` for every class declaration. `subject` is the verbatim
/// token that opens the statement; `_:b0`-style blank nodes are skipped (no stable
/// shape to check).
fn declared_classes(content: &str) -> Vec<(u32, String)> {
    let mut out = Vec::new();
    for (idx, raw) in content.lines().enumerate() {
        let text = strip_comment(raw);
        let trimmed = text.trim_start();
        if trimmed.is_empty() {
            continue;
        }
        // Only statement-opening lines carry a subject; continuation lines
        // (predicate-object pairs after `;`) are indented/typeless here.
        let Some(subject) = leading_subject(trimmed) else {
            continue;
        };
        if subject.starts_with("_:") {
            continue;
        }
        // The remainder after the subject must type it as a class.
        let rest = trimmed[subject.len()..].trim_start();
        if declares_class(rest) {
            out.push((u32::try_from(idx).unwrap_or(0), subject));
        }
    }
    out
}

/// True if the predicate-object remainder of a statement types the subject as a
/// class: `a owl:Class`, `a rdfs:Class`, `rdf:type owl:Class`, `rdf:type
/// rdfs:Class` (case-insensitive on the keyword `a` only by exact match).
fn declares_class(rest: &str) -> bool {
    let mut tokens = rest.split_whitespace();
    let Some(pred) = tokens.next() else {
        return false;
    };
    let is_type_pred = pred == "a" || pred == "rdf:type";
    if !is_type_pred {
        return false;
    }
    let Some(obj) = tokens.next() else {
        return false;
    };
    let obj = obj.trim_end_matches([';', ',', '.']);
    matches!(
        obj,
        "owl:Class"
            | "rdfs:Class"
            | "<http://www.w3.org/2002/07/owl#Class>"
            | "<http://www.w3.org/2000/01/rdf-schema#Class>"
    )
}

/// Count, per class subject, the number of properties that declare it as their
/// `rdfs:domain`. Handles both expanded form (`ex:prop rdfs:domain ex:Class`) and
/// predicate lists where `rdfs:domain ex:Class` appears as a `;`-clause under a
/// property subject.
fn domain_counts(content: &str) -> std::collections::HashMap<String, usize> {
    let mut counts: std::collections::HashMap<String, usize> = std::collections::HashMap::new();
    for raw in content.lines() {
        let text = strip_comment(raw);
        // Find `rdfs:domain <object>` anywhere on the line.
        if let Some(pos) = text.find("rdfs:domain") {
            let after = &text[pos + "rdfs:domain".len()..];
            if let Some(obj) = after.split_whitespace().next() {
                let obj = obj.trim_end_matches([';', ',', '.']);
                if !obj.is_empty() {
                    *counts.entry(obj.to_string()).or_insert(0) += 1;
                }
            }
        }
    }
    counts
}

/// The first whitespace-delimited token of a statement — its subject — or `None`
/// if the line opens with a predicate keyword (continuation) or directive.
fn leading_subject(trimmed: &str) -> Option<String> {
    let first = trimmed.split_whitespace().next()?;
    // Directives and predicate-only continuations are not subjects.
    if first.starts_with('@') || first == "a" || first.starts_with(';') || first.starts_with(',') {
        return None;
    }
    Some(first.to_string())
}

// --- SPARQL ----------------------------------------------------------------

/// One lens per query form. The query-form line gets an actionable
/// `"ggen lsp check"` lens (runs the headless gate over this file); a companion
/// `"{N} variables"` count lens reports the distinct `?`/`$` variables in scope.
fn sparql_lenses(content: &str) -> Vec<CodeLens> {
    let mut out = Vec::new();
    let var_count = distinct_variables(content);
    for (idx, raw) in content.lines().enumerate() {
        let text = strip_comment(raw);
        let upper = text.trim_start().to_ascii_uppercase();
        let is_query_head = upper.starts_with("SELECT")
            || upper.starts_with("CONSTRUCT")
            || upper.starts_with("ASK")
            || upper.starts_with("DESCRIBE");
        if !is_query_head {
            continue;
        }
        let line = u32::try_from(idx).unwrap_or(0);
        // Actionable: re-run the gate over this query file.
        out.push(CodeLens {
            range: whole_line(line),
            command: Some(Command {
                title: "ggen lsp check".to_string(),
                command: CHECK_COMMAND.to_string(),
                arguments: Some(vec![Value::String("${file}".to_string())]),
            }),
            data: None,
        });
        // Count-only: distinct variables in the query.
        let noun = if var_count == 1 {
            "variable"
        } else {
            "variables"
        };
        out.push(CodeLens {
            range: whole_line(line),
            command: Some(Command {
                title: format!("{var_count} {noun}"),
                command: String::new(),
                arguments: None,
            }),
            data: None,
        });
        // Only the first query form per file is lensed (one query per .rq file).
        break;
    }
    out
}

/// Distinct `?var` / `$var` names in the query.
fn distinct_variables(content: &str) -> usize {
    let mut set = std::collections::BTreeSet::new();
    for token in content.split(|c: char| !(c.is_alphanumeric() || c == '_' || c == '?' || c == '$'))
    {
        if (token.starts_with('?') || token.starts_with('$')) && token.len() > 1 {
            set.insert(token.to_string());
        }
    }
    set.len()
}

// --- TOML ------------------------------------------------------------------

/// One `"{N} keys"` lens above each `[section]` header, where N is the real
/// number of `key = value` lines belonging to that section (until the next
/// header). Count-only (no action).
fn toml_lenses(content: &str) -> Vec<CodeLens> {
    let lines: Vec<&str> = content.lines().collect();
    let mut out = Vec::new();
    let mut idx = 0usize;
    while idx < lines.len() {
        let trimmed = lines[idx].trim();
        if is_section_header(trimmed) {
            let header_line = u32::try_from(idx).unwrap_or(0);
            let mut keys = 0usize;
            let mut j = idx + 1;
            while j < lines.len() {
                let t = lines[j].trim();
                if is_section_header(t) {
                    break;
                }
                if is_key_line(t) {
                    keys += 1;
                }
                j += 1;
            }
            let noun = if keys == 1 { "key" } else { "keys" };
            out.push(CodeLens {
                range: whole_line(header_line),
                command: Some(Command {
                    title: format!("{keys} {noun}"),
                    command: String::new(),
                    arguments: None,
                }),
                data: None,
            });
        }
        idx += 1;
    }
    out
}

fn is_section_header(trimmed: &str) -> bool {
    trimmed.starts_with('[') && trimmed.ends_with(']')
}

/// A TOML key line: `key = value`, not a comment, not a header.
fn is_key_line(trimmed: &str) -> bool {
    if trimmed.is_empty() || trimmed.starts_with('#') || is_section_header(trimmed) {
        return false;
    }
    match trimmed.split_once('=') {
        Some((key, _)) => !key.trim().is_empty(),
        None => false,
    }
}

// --- shared ----------------------------------------------------------------

/// Strip a trailing `#` comment, respecting nothing fancy — sufficient for the
/// line-oriented lensing here (we only read tokens before the comment).
fn strip_comment(line: &str) -> &str {
    match line.find('#') {
        // Keep `#`-bearing IRIs (e.g. `rdf:type` expands fine; full IRIs use `<>`).
        // A comment `#` is one preceded by whitespace or at line start.
        Some(0) => "",
        Some(pos) if line.as_bytes()[pos - 1].is_ascii_whitespace() => &line[..pos],
        _ => line,
    }
}

fn whole_line(line: u32) -> Range {
    Range {
        start: Position { line, character: 0 },
        end: Position { line, character: 0 },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn titles(lenses: &[CodeLens]) -> Vec<String> {
        lenses
            .iter()
            .filter_map(|l| l.command.as_ref().map(|c| c.title.clone()))
            .collect()
    }

    #[test]
    fn rdf_two_classes_yield_lenses_with_real_property_counts() {
        // Person has 2 properties (name, age) by rdfs:domain; Org has 1 (label).
        let ttl = "\
@prefix ex: <http://example.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a owl:Class .
ex:Org a owl:Class .

ex:name rdfs:domain ex:Person .
ex:age rdfs:domain ex:Person .
ex:label rdfs:domain ex:Org .
";
        let lenses = code_lenses(FileType::Rdf, ttl).expect("rdf lenses");
        // Two class declarations → two lenses.
        assert_eq!(lenses.len(), 2, "one lens per class declaration");

        // ex:Person is declared on line index 4 (0-based).
        let person = &lenses[0];
        assert_eq!(person.range.start.line, 4, "lens sits above ex:Person");
        let person_cmd = person.command.as_ref().expect("actionable lens");
        assert_eq!(person_cmd.title, "2 properties · check shape");
        assert_eq!(person_cmd.command, CHECK_COMMAND);
        assert_eq!(
            person_cmd.arguments.as_ref().unwrap()[0],
            Value::String("ex:Person".to_string())
        );

        // ex:Org is declared on line index 5; has exactly 1 property.
        let org = &lenses[1];
        assert_eq!(org.range.start.line, 5, "lens sits above ex:Org");
        let org_cmd = org.command.as_ref().expect("actionable lens");
        assert_eq!(org_cmd.title, "1 property · check shape");
        assert_eq!(org_cmd.command, CHECK_COMMAND);
    }

    #[test]
    fn rdf_rdf_type_and_rdfs_class_forms_are_recognized() {
        let ttl = "\
@prefix ex: <http://example.org/> .
ex:Thing rdf:type rdfs:Class .
ex:p rdfs:domain ex:Thing .
";
        let lenses = code_lenses(FileType::Rdf, ttl).expect("rdf lenses");
        assert_eq!(lenses.len(), 1);
        assert_eq!(
            lenses[0].command.as_ref().unwrap().title,
            "1 property · check shape"
        );
    }

    #[test]
    fn rdf_with_no_classes_yields_none() {
        let ttl = "@prefix ex: <http://example.org/> .\nex:s ex:p ex:o .\n";
        assert!(code_lenses(FileType::Rdf, ttl).is_none());
    }

    #[test]
    fn toml_per_section_lenses_count_real_keys() {
        let toml = "\
[project]
name = \"demo\"
version = \"1.0.0\"

[logging]
level = \"info\"
";
        let lenses = code_lenses(FileType::Toml, toml).expect("toml lenses");
        assert_eq!(lenses.len(), 2, "one lens per [section]");

        // [project] on line 0, 2 keys.
        assert_eq!(lenses[0].range.start.line, 0);
        assert_eq!(lenses[0].command.as_ref().unwrap().title, "2 keys");

        // [logging] on line 4, 1 key.
        assert_eq!(lenses[1].range.start.line, 4);
        assert_eq!(lenses[1].command.as_ref().unwrap().title, "1 key");
    }

    #[test]
    fn toml_ignores_comments_when_counting_keys() {
        let toml = "[a]\n# a comment\nk = 1\n";
        let lenses = code_lenses(FileType::Toml, toml).expect("toml lenses");
        assert_eq!(lenses[0].command.as_ref().unwrap().title, "1 key");
    }

    #[test]
    fn sparql_select_gets_check_and_variable_count_lenses() {
        let q = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
        let lenses = code_lenses(FileType::Sparql, q).expect("sparql lenses");
        let ts = titles(&lenses);
        assert!(
            ts.iter().any(|t| t == "ggen lsp check"),
            "actionable check lens"
        );
        assert!(
            ts.iter().any(|t| t == "3 variables"),
            "real variable count: {ts:?}"
        );
        // The check lens carries the ggen.lsp.check command.
        let check = lenses
            .iter()
            .find(|l| {
                l.command
                    .as_ref()
                    .map(|c| c.title == "ggen lsp check")
                    .unwrap_or(false)
            })
            .expect("check lens present");
        assert_eq!(check.command.as_ref().unwrap().command, CHECK_COMMAND);
    }

    #[test]
    fn sparql_construct_is_lensed_with_check() {
        let q = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o } ORDER BY ?s";
        let lenses = code_lenses(FileType::Sparql, q).expect("sparql lenses");
        assert!(titles(&lenses).iter().any(|t| t == "ggen lsp check"));
        assert!(titles(&lenses).iter().any(|t| t == "3 variables"));
    }

    #[test]
    fn sparql_singular_variable_uses_singular_noun() {
        let q = "ASK { ?s a <http://x> }";
        let lenses = code_lenses(FileType::Sparql, q).expect("sparql lenses");
        assert!(titles(&lenses).iter().any(|t| t == "1 variable"));
    }

    #[test]
    fn non_lensable_surfaces_return_none() {
        assert!(code_lenses(FileType::Tera, "{{ x }}").is_none());
        assert!(code_lenses(FileType::Unknown, "anything").is_none());
    }
}
