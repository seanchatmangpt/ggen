//! Chicago tests: real generated files, real emitted index, real queries.

use ggen_scip::emit::{build_index, discover, is_generated, package_name};
use ggen_scip::{parse_symbol, ROLE_DEFINITION};
use std::path::Path;

const ROOT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../..");
const GEN_CMDS: &str = "crates/ggen-cli/src/generated_commands.rs";

fn rels(root: &Path) -> Vec<String> {
    discover(root)
        .iter()
        .filter(|f| is_generated(f))
        .map(|f| f.strip_prefix(root).unwrap().to_string_lossy().into_owned())
        .collect()
}

#[test]
fn discovery_finds_real_generated_files() {
    let root = Path::new(ROOT);
    let rs = rels(root);
    assert!(
        rs.iter().any(|r| r == GEN_CMDS),
        "generated_commands.rs missing from {rs:?}"
    );
    assert!(
        rs.iter().any(|r| r.ends_with("generated/refusals.rs")),
        "generated/refusals.rs missing from {rs:?}"
    );
    let placeholder = root.join("crates/ggen-sjira/src/generated.rs");
    if placeholder.exists() {
        assert!(
            !is_generated(&placeholder),
            "placeholder counted as generated"
        );
    }
}

#[test]
fn package_name_resolves_crate_manifest() {
    let p = Path::new(ROOT).join(GEN_CMDS);
    assert_eq!(package_name(&p), "ggen-cli-lib");
}

#[test]
fn emit_builds_nonempty_index_over_real_surface() {
    let root = Path::new(ROOT);
    let index = build_index(root).expect("build_index");
    assert!(!index.documents.is_empty(), "no documents indexed");
    let cmds = index
        .documents
        .iter()
        .find(|d| d.path == GEN_CMDS)
        .expect("generated_commands document");
    assert!(!cmds.occurrences.is_empty());
    // COMMANDS_REFERENCE has a definition occurrence.
    let def = cmds
        .occurrences
        .iter()
        .find(|o| o.roles & ROLE_DEFINITION != 0 && o.symbol.ends_with("COMMANDS_REFERENCE"))
        .expect("COMMANDS_REFERENCE definition");
    assert_eq!(def.def_info.as_ref().unwrap().kind, "const");
    // Exact range check: the [start_col, end_col) slice of line start_row
    // must be exactly the defined name.
    let src = std::fs::read_to_string(root.join(GEN_CMDS)).unwrap();
    let line = src.lines().nth(def.range[0] as usize).expect("row exists");
    let cols = &line.as_bytes()[def.range[1] as usize..def.range[2] as usize];
    assert_eq!(std::str::from_utf8(cols).unwrap(), "COMMANDS_REFERENCE");
    assert_eq!(
        def.symbol,
        "ggen ggen-cli-lib generated_commands.COMMANDS_REFERENCE"
    );
}

#[test]
fn query_def_and_refs_no_reparse() {
    let root = Path::new(ROOT);
    let index = build_index(root).expect("build_index");
    let q = ggen_scip::query::Query::from_index(index);
    let wire = "ggen ggen-cli-lib generated_commands.COMMANDS_REFERENCE";
    let d = q.def(wire).expect("definition indexed");
    assert!(d.roles & ROLE_DEFINITION != 0);
    // references exist: COMMANDS_REFERENCE is used in describe_command
    assert!(
        !q.refs(wire).is_empty(),
        "expected >=1 reference occurrence"
    );
    // parse_symbol round-trips
    let s = parse_symbol(wire).unwrap();
    assert_eq!(s.to_wire(), wire);
}
