//! Real syntax + semantic evaluator for the `.specify/gates/*.rq` SPARQL law
//! gates. Backs `scripts/ci/guard-sparql-gates.sh`.
//!
//! Two passes, run over every `*.rq` file found directly under the given
//! gates directory:
//!
//! 1. **Syntax** — each file is parsed with [`ggen_graph::check_sparql_syntax`],
//!    the exact same oxigraph `SparqlEvaluator` parser `ggen-lsp`'s
//!    `sparql_analyzer` already uses for author-time diagnostics. This is not
//!    a second parser: it is the one real parser this repo already trusts,
//!    reused. Every gate file gets this pass, regardless of whether it also
//!    gets pass 2.
//!
//! 2. **Semantics** — a named subset of gates (`GATES_WITH_KNOWN_DATA` below)
//!    whose required ontology data is fully declared in this repo's own
//!    `ggen.toml` `[ontology].imports` list are additionally *executed*
//!    against a `Store` loaded from the given Turtle files:
//!      - `ASK` result: `true` = pass, `false` = the invariant is violated.
//!      - `SELECT`/`CONSTRUCT` result: zero rows/triples = pass, any row is
//!        one violation (the convention already used by
//!        `cross-pack-contamination.rq` and the `l5-*.rq` gates).
//!
//! Gates *not* in `GATES_WITH_KNOWN_DATA` (`cross-pack-contamination.rq` and
//! the three `l5-*.rq` gates) get syntax-check only here: their real data is
//! a full pack-composed graph assembled by `ggen-engine`'s `[law]` pipeline
//! at sync time (see `crates/ggen-engine/tests/pack_e2e.rs` for their real
//! Chicago-TDD execution proof against that pipeline). Reconstructing that
//! composition inside this standalone guard would risk a second, divergent
//! implementation of graph assembly for a check that already has a real test
//! elsewhere — so this binary deliberately does not attempt it.
#![allow(clippy::print_stdout)]

use ggen_graph::check_sparql_syntax;
use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::sparql::{QueryResults, QuerySolution};
use oxigraph::store::Store;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

/// Gate files (by stem, no `.rq`) whose full ASK/SELECT semantics are
/// checked against the real ontology graph passed on the command line, not
/// merely parsed. Every name here must correspond to a real `*.rq` file in
/// the gates directory at run time, or this program refuses to run (a
/// stale/drifted list is a bug in this file, not a silent no-op).
const GATES_WITH_KNOWN_DATA: &[&str] = &[
    "every-action-has-binding",
    "every-binding-has-output-pattern",
    "every-binding-has-template",
    "every-command-has-handler",
    "every-generator-has-action",
    "no-orphan-actions",
];

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 3 {
        eprintln!(
            "usage: {} <gates-dir> <ttl-file> [<ttl-file> ...]",
            args.first().map_or("sparql_gate_check", String::as_str)
        );
        return ExitCode::FAILURE;
    }
    let gates_dir = PathBuf::from(&args[1]);
    let ttl_files: Vec<PathBuf> = args[2..].iter().map(PathBuf::from).collect();

    match run(&gates_dir, &ttl_files) {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => ExitCode::FAILURE,
        Err(e) => {
            eprintln!("BUILD_BROKEN: {e}");
            ExitCode::FAILURE
        }
    }
}

fn describe_row(row: &QuerySolution) -> String {
    row.iter()
        .map(|(var, term)| format!("{var}={term}"))
        .collect::<Vec<_>>()
        .join(", ")
}

fn run(gates_dir: &Path, ttl_files: &[PathBuf]) -> Result<bool, String> {
    let mut rq_files: Vec<PathBuf> = fs::read_dir(gates_dir)
        .map_err(|e| format!("cannot read gates dir {}: {e}", gates_dir.display()))?
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some("rq"))
        .collect();
    rq_files.sort();

    if rq_files.is_empty() {
        return Err(format!(
            "no *.rq files found under {} -- refusing to report a vacuous pass",
            gates_dir.display()
        ));
    }

    // Fail loudly up front if GATES_WITH_KNOWN_DATA has drifted from the real
    // files on disk (renamed/removed gate), rather than silently skipping it.
    let stems_on_disk: Vec<String> = rq_files
        .iter()
        .filter_map(|p| p.file_stem().and_then(|s| s.to_str()).map(String::from))
        .collect();
    for known in GATES_WITH_KNOWN_DATA {
        if !stems_on_disk.iter().any(|s| s == known) {
            return Err(format!(
                "GATES_WITH_KNOWN_DATA names '{known}' but no {known}.rq exists under {} \
                 -- this list has drifted from reality",
                gates_dir.display()
            ));
        }
    }

    // Build the real project graph once, reused for every semantic check.
    let store = Store::new().map_err(|e| format!("failed to create store: {e}"))?;
    for ttl in ttl_files {
        let content = fs::read_to_string(ttl)
            .map_err(|e| format!("cannot read ontology file {}: {e}", ttl.display()))?;
        store
            .load_from_reader(
                RdfParser::from_format(RdfFormat::Turtle),
                content.as_bytes(),
            )
            .map_err(|e| format!("cannot parse ontology file {}: {e}", ttl.display()))?;
    }

    let mut all_ok = true;

    for rq_path in &rq_files {
        let name = rq_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("<unknown>")
            .to_string();
        let query = fs::read_to_string(rq_path)
            .map_err(|e| format!("cannot read {}: {e}", rq_path.display()))?;

        if let Err(msg) = check_sparql_syntax(&query) {
            println!("FAIL {name}: syntax_error -- {msg}");
            all_ok = false;
            continue;
        }

        if !GATES_WITH_KNOWN_DATA.contains(&name.as_str()) {
            println!(
                "SYNTAX_OK {name}: parses; semantic execution not attempted here \
                 (needs the full pack-composed graph -- see module doc)"
            );
            continue;
        }

        #[allow(deprecated)]
        let exec = store.query(&query);
        match exec {
            Ok(QueryResults::Boolean(b)) => {
                if b {
                    println!("PASS {name}: ASK -> true");
                } else {
                    println!("FAIL {name}: ASK -> false (invariant violated)");
                    all_ok = false;
                }
            }
            Ok(QueryResults::Solutions(solutions)) => {
                let rows: Vec<QuerySolution> = solutions
                    .collect::<Result<Vec<_>, _>>()
                    .map_err(|e| format!("{name}: error reading solutions: {e}"))?;
                if rows.is_empty() {
                    println!("PASS {name}: SELECT -> 0 violation rows");
                } else {
                    println!(
                        "FAIL {name}: SELECT -> {} violation row(s), first: {}",
                        rows.len(),
                        describe_row(&rows[0])
                    );
                    all_ok = false;
                }
            }
            Ok(QueryResults::Graph(triples)) => {
                let count = triples.count();
                if count == 0 {
                    println!("PASS {name}: CONSTRUCT -> 0 violation triples");
                } else {
                    println!("FAIL {name}: CONSTRUCT -> {count} violation triple(s)");
                    all_ok = false;
                }
            }
            Err(e) => {
                println!("FAIL {name}: execution_error -- {e}");
                all_ok = false;
            }
        }
    }

    Ok(all_ok)
}
