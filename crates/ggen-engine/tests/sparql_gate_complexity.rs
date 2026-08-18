//! Permanent regression court for the real SPARQL evaluator stack-overflow
//! boundary discovered while expanding `dspy-pack` on 2026-08-10.
//!
//! praxis-graphlaw's current evaluator overflowed on a very large UNION
//! algebra tree (observed around the 140–160 branch region). Pack gates are
//! admission law, so a query shape that can crash the evaluator is not an
//! acceptable source artifact even if its semantics are otherwise valid.
//!
//! The court stays deliberately conservative and engine-independent: any one
//! checked-in `packs/**/gates/*.rq` query with more than 120 real UNION tokens
//! is refused and should be refactored to VALUES / bounded table-driven
//! patterns. Comments and string literals do not count.

#![allow(clippy::expect_used, clippy::panic)]

use std::{fs, path::{Path, PathBuf}};

const MAX_UNION_TOKENS: usize = 120;

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

fn sparql_without_comments_or_strings(source: &str) -> String {
    let mut out = String::with_capacity(source.len());
    let mut chars = source.chars().peekable();
    let mut quote: Option<char> = None;
    let mut escaped = false;

    while let Some(ch) = chars.next() {
        if let Some(q) = quote {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == q {
                quote = None;
            }
            out.push(' ');
            continue;
        }

        match ch {
            '#' => {
                for next in chars.by_ref() {
                    if next == '\n' {
                        out.push('\n');
                        break;
                    }
                }
            }
            '"' | '\'' => {
                quote = Some(ch);
                out.push(' ');
            }
            _ => out.push(ch),
        }
    }
    out
}

fn union_count(source: &str) -> usize {
    sparql_without_comments_or_strings(source)
        .split(|ch: char| !(ch.is_ascii_alphanumeric() || ch == '_'))
        .filter(|token| token.eq_ignore_ascii_case("UNION"))
        .count()
}

fn collect_gate_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = fs::read_dir(dir)
        .unwrap_or_else(|error| panic!("read {}: {error}", dir.display()));
    for entry in entries {
        let path = entry.expect("directory entry").path();
        if path.is_dir() {
            collect_gate_files(&path, out);
        } else if path.extension().and_then(|value| value.to_str()) == Some("rq")
            && path.components().any(|part| part.as_os_str() == "gates")
        {
            out.push(path);
        }
    }
}

#[test]
fn union_counter_ignores_comments_strings_and_substrings() {
    let query = r#"
        # UNION UNION UNION
        SELECT ?s WHERE {
          { ?s ?p "UNION" } UNION { ?s ?p 'union' }
          BIND("NOTUNION" AS ?label)
          FILTER(?label != "reUNION")
        }
    "#;
    assert_eq!(union_count(query), 1);
}

#[test]
fn checked_in_pack_gates_stay_below_observed_union_crash_boundary() {
    let mut gates = Vec::new();
    collect_gate_files(&packs_dir(), &mut gates);
    gates.sort();
    assert!(!gates.is_empty(), "expected checked-in pack SPARQL gates");

    let offenders: Vec<String> = gates
        .into_iter()
        .filter_map(|path| {
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
            let count = union_count(&source);
            (count > MAX_UNION_TOKENS).then(|| {
                format!(
                    "{}: UNION tokens={count} > {MAX_UNION_TOKENS}",
                    path.strip_prefix(packs_dir())
                        .unwrap_or(&path)
                        .display()
                )
            })
        })
        .collect();

    assert!(
        offenders.is_empty(),
        "REFUSED:SPARQL_UNION_COMPLEXITY: current praxis-graphlaw evaluator has a real observed stack-overflow boundary for large UNION algebra; refactor to VALUES/table-driven gates:\n{}",
        offenders.join("\n")
    );
}
