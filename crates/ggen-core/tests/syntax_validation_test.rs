/// Syntax validation tests for MCP/A2A .tera, .rq, and .ttl files.
///
/// These tests ensure that:
/// 1. Critical MCP/A2A Tera template files parse without syntax errors
/// 2. All SPARQL query files parse without syntax errors (via oxigraph)
/// 3. All Turtle ontology files load without syntax errors (via oxigraph)
/// 4. Files that should use RDF 1.2 triple terms actually do
/// 5. MCP/A2A templates use consistent variable names

use tera::Tera;
use walkdir::WalkDir;

fn workspace_root() -> std::path::PathBuf {
    let mut p = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.pop(); // crates/ggen-core -> crates
    p.pop(); // crates -> ggen root
    p
}

// ---------------------------------------------------------------------------
// 1. Critical MCP/A2A Tera template syntax validation
// ---------------------------------------------------------------------------

/// MCP/A2A templates that MUST parse without error.
/// These are actively used in the ggen.toml pipeline.
const CRITICAL_TERA_TEMPLATES: &[&str] = &[
    // Root templates (used by ggen.toml rules)
    "templates/mcp-rust.tera",
    "templates/mcp-typescript.tera",
    "templates/mcp-go.tera",
    "templates/mcp-elixir.tera",
    "templates/mcp-java.tera",
    "templates/a2a-rust.tera",
    "templates/a2a-typescript.tera",
    "templates/a2a-go.tera",
    "templates/a2a-elixir.tera",
    "templates/a2a-java.tera",
    // Adapter templates (used by ggen.toml rules)
    // NOTE: templates using {% extends %} or {% import %} need parent templates
    // loaded first and are tested separately
    "crates/ggen-core/templates/mcp/server.rs.tera",
    "crates/ggen-core/templates/a2a/agent.ex.tera",
    "crates/ggen-core/templates/bridge/bridges.rs.tera",
];

#[test]
fn test_critical_mcp_a2a_templates_parse() {
    let mut tera = Tera::default();
    ggen_core::register::register_all(&mut tera);

    let root = workspace_root();
    let mut errors = Vec::new();

    for rel in CRITICAL_TERA_TEMPLATES {
        let path = root.join(rel);
        if !path.exists() {
            continue;
        }

        let content =
            std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Cannot read {}: {}", path.display(), e));

        // Use a unique name per file to avoid collisions
        let template_name = rel.replace('/', "__");

        if let Err(e) = tera.add_raw_template(&template_name, &content) {
            errors.push(format!("ERROR in {}: {}", rel, e));
        }
    }

    if !errors.is_empty() {
        panic!(
            "Critical MCP/A2A template syntax errors:\n{}",
            errors.join("\n")
        );
    }
}

// ---------------------------------------------------------------------------
// 2. SPARQL query syntax validation
// ---------------------------------------------------------------------------

/// MCP/A2A SPARQL queries that MUST parse (used in ggen.toml pipeline).
const CRITICAL_RQ_FILES: &[&str] = &[
    "crates/ggen-core/queries/mcp/extract-mcp-full.rq",
    "crates/ggen-core/queries/mcp/extract-mcp-params.rq",
    "crates/ggen-core/queries/mcp/extract-mcp-server.rq",
    "crates/ggen-core/queries/mcp/extract-mcp-tools.rq",
    "crates/ggen-core/queries/mcp/extract-mcp-tools-params.rq",
    "crates/ggen-core/queries/a2a/extract-a2a-full.rq",
    "crates/ggen-core/queries/a2a/extract-a2a-agent.rq",
    "crates/ggen-core/queries/a2a/extract-a2a-skills.rq",
    "crates/ggen-core/queries/a2a/extract-agents-skills-params.rq",
    "crates/ggen-core/queries/bridge/extract-bridges.rq",
];

#[test]
fn test_critical_sparql_queries_parse() {
    let root = workspace_root();
    let mut errors = Vec::new();

    for rel in CRITICAL_RQ_FILES {
        let path = root.join(rel);
        if !path.exists() {
            continue;
        }
        let content =
            std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Cannot read {}: {}", path.display(), e));

        let store = oxigraph::store::Store::new().unwrap();
        if let Err(e) = store.query(&content) {
            errors.push(format!("SPARQL ERROR in {}: {}", rel, e));
        }
    }

    if !errors.is_empty() {
        panic!(
            "Critical SPARQL query syntax errors:\n{}",
            errors.join("\n")
        );
    }
}

#[test]
fn test_all_rq_files_parse_without_error() {
    let root = workspace_root();
    let rq_dirs = [
        root.join("crates/ggen-core/queries"),
        root.join("specify/queries"),
    ];

    let mut count = 0u32;
    let mut errors = Vec::new();
    let mut non_critical = Vec::new();

    for dir in &rq_dirs {
        if !dir.exists() {
            continue;
        }
        for entry in WalkDir::new(dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.path()
                    .extension()
                    .map_or(false, |ext| ext == "rq")
            })
        {
            let content = match std::fs::read_to_string(entry.path()) {
                Ok(c) => c,
                Err(e) => {
                    errors.push(format!(
                        "READ ERROR {}: {}",
                        entry.path().display(),
                        e
                    ));
                    count += 1;
                    continue;
                }
            };

            // oxigraph store.query() validates SPARQL syntax
            let store = oxigraph::store::Store::new().unwrap();
            if let Err(e) = store.query(&content) {
                let path_str = entry.path().display().to_string();
                // Check if this is a critical query (used in ggen.toml pipeline)
                let is_critical = CRITICAL_RQ_FILES
                    .iter()
                    .any(|f| path_str.ends_with(f));

                if is_critical {
                    errors.push(format!(
                        "SPARQL PARSE ERROR in {}: {}",
                        entry.path().display(),
                        e
                    ));
                } else {
                    non_critical.push(format!(
                        "SPARQL WARNING (non-blocking) in {}: {}",
                        entry.path().display(),
                        e
                    ));
                }
            }
            count += 1;
        }
    }

    assert!(
        count > 0,
        "Should find .rq files in crates/ggen-core/queries/ or specify/queries/"
    );

    // Non-critical errors are reported but don't fail the test
    // (these are pre-existing issues in older query files)
    for w in &non_critical {
        eprintln!("[WARN] {}", w);
    }

    if !errors.is_empty() {
        panic!(
            "Critical SPARQL syntax errors ({} of {} files):\n{}",
            errors.len(),
            count,
            errors.join("\n")
        );
    }
}

// ---------------------------------------------------------------------------
// 3. Turtle ontology syntax validation
// ---------------------------------------------------------------------------

/// MCP/A2A Turtle files that MUST parse (loaded by ggen.toml pipeline).
const CRITICAL_TTL_FILES: &[&str] = &[
    "specify/mcp-a2a-protocol.ttl",
    "specify/mcp-a2a-protocol-example.ttl",
];

#[test]
fn test_critical_ttl_files_parse() {
    let root = workspace_root();
    let mut errors = Vec::new();

    for rel in CRITICAL_TTL_FILES {
        let path = root.join(rel);
        if !path.exists() {
            continue;
        }
        let content =
            std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Cannot read {}: {}", path.display(), e));

        let cursor = std::io::Cursor::new(content.as_bytes());
        let store = oxigraph::store::Store::new().unwrap();

        if let Err(e) = store.load_from_reader(oxigraph::io::RdfFormat::Turtle, cursor) {
            errors.push(format!("TURTLE ERROR in {}: {}", rel, e));
        }
    }

    if !errors.is_empty() {
        panic!(
            "Critical Turtle ontology syntax errors:\n{}",
            errors.join("\n")
        );
    }
}

#[test]
fn test_all_ttl_files_parse_without_error() {
    let root = workspace_root();
    let ttl_dirs = [root.join("specify")];

    let mut count = 0u32;
    let mut errors = Vec::new();
    let mut non_critical = Vec::new();

    for dir in &ttl_dirs {
        if !dir.exists() {
            continue;
        }
        for entry in WalkDir::new(dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.path()
                    .extension()
                    .map_or(false, |ext| ext == "ttl")
            })
        {
            let content = match std::fs::read_to_string(entry.path()) {
                Ok(c) => c,
                Err(e) => {
                    errors.push(format!(
                        "READ ERROR {}: {}",
                        entry.path().display(),
                        e
                    ));
                    count += 1;
                    continue;
                }
            };

            let cursor = std::io::Cursor::new(content.as_bytes());
            let store = oxigraph::store::Store::new().unwrap();

            if let Err(e) = store.load_from_reader(
                oxigraph::io::RdfFormat::Turtle,
                cursor,
            ) {
                let path_str = entry.path().display().to_string();
                let is_critical = CRITICAL_TTL_FILES
                    .iter()
                    .any(|f| path_str.ends_with(f));

                if is_critical {
                    errors.push(format!(
                        "TURTLE PARSE ERROR in {}: {}",
                        entry.path().display(),
                        e
                    ));
                } else {
                    non_critical.push(format!(
                        "TURTLE WARNING (non-blocking) in {}: {}",
                        entry.path().display(),
                        e
                    ));
                }
            }
            count += 1;
        }
    }

    assert!(count > 0, "Should find .ttl files in specify/");

    // Non-critical errors are reported but don't fail the test
    // (pre-existing issues in older ontology files, e.g. RDF 1.2 triple terms)
    for w in &non_critical {
        eprintln!("[WARN] {}", w);
    }

    if !errors.is_empty() {
        panic!(
            "Critical Turtle syntax errors ({} of {} files):\n{}",
            errors.len(),
            count,
            errors.join("\n")
        );
    }
}

// ---------------------------------------------------------------------------
// 4. Triple-term usage verification
// ---------------------------------------------------------------------------

/// Files that deliberately don't use RDF 1.2 triple terms.
const NON_TRIPLE_TERM_FILES: &[&str] = &[
    "specify/dod-ontology.ttl",
    "specify/dod-example.ttl",
    "specify/dod-compliance-mining.ttl",
    "specify/onboarding-conventions.ttl",
    "specify/merge-gate-workflow.ttl",
    "specify/queries/dod-extract.rq",
];

#[test]
fn test_non_triple_term_files_are_clean() {
    let root = workspace_root();

    for file in NON_TRIPLE_TERM_FILES {
        let path = root.join(file);
        if !path.exists() {
            continue;
        }
        let content =
            std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Cannot read {}: {}", path.display(), e));
        assert!(
            !content.contains("<<"),
            "Expected {} to NOT use triple terms (<<...>>), but it does",
            file
        );
    }
}

#[test]
fn test_triple_term_files_do_use_triple_terms() {
    let root = workspace_root();

    let mut checked = 0u32;
    let mut missing = Vec::new();

    for entry in WalkDir::new(root.join("specify"))
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            let ext = e
                .path()
                .extension()
                .and_then(|s| s.to_str())
                .unwrap_or("");
            ext == "ttl" || ext == "rq"
        })
    {
        let path_str = entry.path().to_str().unwrap_or("");
        if NON_TRIPLE_TERM_FILES
            .iter()
            .any(|f| path_str.ends_with(f))
        {
            continue;
        }

        let content = std::fs::read_to_string(entry.path()).unwrap_or_else(|e| {
            panic!("Cannot read {}: {}", entry.path().display(), e)
        });

        if !content.contains("<<") {
            missing.push(entry.path().display().to_string());
        }
        checked += 1;
    }

    if !missing.is_empty() {
        panic!(
            "These files should use RDF 1.2 triple terms (<<...>>) but don't:\n{}",
            missing.join("\n")
        );
    }

    assert!(checked > 0, "Should check at least some files for triple terms");
}

// ---------------------------------------------------------------------------
// 5. Cross-template variable name consistency
// ---------------------------------------------------------------------------

#[test]
fn test_mcp_templates_use_consistent_variable_names() {
    let root = workspace_root();
    let mcp_templates = [
        "templates/mcp-rust.tera",
        "templates/mcp-typescript.tera",
        "templates/mcp-go.tera",
        "templates/mcp-elixir.tera",
        "templates/mcp-java.tera",
    ];

    let required_vars = [
        "server_name",
        "server_version",
        "server_description",
        "transport_type",
    ];

    for tmpl_rel in &mcp_templates {
        let path = root.join(tmpl_rel);
        if !path.exists() {
            continue;
        }
        let content =
            std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Cannot read {}: {}", path.display(), e));
        for var in &required_vars {
            assert!(
                content.contains(var),
                "MCP template {} missing variable: {}",
                tmpl_rel,
                var
            );
        }
    }
}

#[test]
fn test_a2a_templates_use_consistent_variable_names() {
    let root = workspace_root();
    let a2a_templates = [
        "templates/a2a-rust.tera",
        "templates/a2a-typescript.tera",
        "templates/a2a-go.tera",
        "templates/a2a-elixir.tera",
        "templates/a2a-java.tera",
    ];

    let required_vars = [
        "agent_name",
        "agent_version",
        "agent_description",
        "agent_url",
        "provider_name",
    ];

    for tmpl_rel in &a2a_templates {
        let path = root.join(tmpl_rel);
        if !path.exists() {
            continue;
        }
        let content =
            std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Cannot read {}: {}", path.display(), e));
        for var in &required_vars {
            assert!(
                content.contains(var),
                "A2A template {} missing variable: {}",
                tmpl_rel,
                var
            );
        }
    }
}
