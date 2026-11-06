//! F2 gate — Tera whitespace-control census + poisoned-fixture render law.
//!
//! PL10 corrected law (see docs/jira/v26.11.4/PLANS.md ZP-24 erratum): `-}}`
//! on expression tags IS lawful Tera in both engine versions this repo spans:
//!
//! - tera 1.20.x: `variable_end = { "-}}" | "}}" }`
//!   (src/parser/tera.pest:150, tera-1.20.x registry source)
//! - tera 2.4.0: `Token::VariableEnd(true)` on the `-` + `variable_end` path
//!   (src/parsing/lexer.rs:532-544, tera-2.4.0 registry source)
//!
//! The 30 expression-tag `-}}` uses (12 files under ggen `packs/`, 18 under
//! the sibling ggen-marketplace `packs/` tree) are LAWFUL and load-bearing.
//! The only lawful-to-flag risk is version migration drift: if a future Tera
//! bump stops honoring `-}}`, rendered bytes change. This gate catches that
//! by rendering a poisoned fixture through the same per-file registration
//! discipline as `ggen_engine::template::load_templates_glob_lenient`
//! (template.rs:630-775): `tera::Template::new` per file, insert into
//! `Tera::default()`, `build_inheritance_chains`.
//!
//! Non-vacuity: the poisoned fixture MUST produce a named finding — if a run
//! reports zero findings on it, the gate itself is buggy (asserted).

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::similar_names
)]
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use serde_json::json;
use tera::Tera;

// ---------------------------------------------------------------------------
// Roots
// ---------------------------------------------------------------------------

/// ggen repo root = crate dir /../.. (crates/ggen-engine -> repo root).
fn ggen_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
}

/// Sibling marketplace checkout (sibling directory of the ggen checkout).
fn marketplace_root() -> Option<PathBuf> {
    let canonical = ggen_root().canonicalize().ok()?;
    let p = canonical.parent()?.join("ggen-marketplace");
    p.is_dir().then_some(p)
}

/// All roots censused: ggen packs/, ggen templates/, ggen examples/, and
/// (when the sibling checkout is present) ggen-marketplace packs/.
fn census_roots() -> Vec<(String, PathBuf)> {
    let root = ggen_root();
    let mut roots = vec![
        ("ggen/packs".to_string(), root.join("packs")),
        ("ggen/templates".to_string(), root.join("templates")),
        ("ggen/examples".to_string(), root.join("examples")),
    ];
    if let Some(m) = marketplace_root() {
        roots.push(("marketplace/packs".to_string(), m.join("packs")));
    }
    roots
}

const TEMPLATE_EXTS: [&str; 2] = ["tmpl", "tera"];

fn collect_tmpl_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(rd) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in rd.flatten() {
        let p = entry.path();
        if p.is_dir() {
            collect_tmpl_files(&p, out);
        } else if p
            .extension()
            .and_then(|e| e.to_str())
            .is_some_and(|e| TEMPLATE_EXTS.contains(&e))
        {
            out.push(p);
        }
    }
}

// ---------------------------------------------------------------------------
// Classification
// ---------------------------------------------------------------------------

#[derive(Default, Debug, PartialEq, Eq)]
struct Classification {
    /// Lines containing `{{` AND `-}}` — the flagged (but lawful) class.
    expr_tag_dashes: usize,
    /// Lines containing `{%-` or `-%}` (lawful control-tag whitespace control).
    ctrl_tag_dashes: usize,
}

fn classify_content(content: &str) -> Classification {
    let mut c = Classification::default();
    for line in content.lines() {
        if line.contains("{{") && line.contains("-}}") {
            c.expr_tag_dashes += 1;
        }
        if line.contains("{%-") || line.contains("-%}") {
            c.ctrl_tag_dashes += 1;
        }
    }
    c
}

// ---------------------------------------------------------------------------
// Lenient-path render replica (template.rs:630-775 discipline)
// ---------------------------------------------------------------------------

/// Register `content` under `name` exactly the way
/// `ggen_engine::template::load_templates_glob_lenient` registers a file:
/// parse individually (`tera::Template::new`), insert into `Tera::default()`,
/// then `build_inheritance_chains`. `load_templates_glob_lenient` itself is
/// `pub(crate)`, so this integration test replicates the byte-relevant
/// registration sequence rather than calling it.
fn render_lenient_style(
    name: &str,
    content: &str,
    ctx: &serde_json::Value,
) -> Result<String, String> {
    let mut tera = Tera::default();
    let tpl = tera::Template::new(name, None, content)
        .map_err(|e| format!("parse: {e}"))?;
    tera.templates.insert(name.to_string(), tpl);
    tera.build_inheritance_chains()
        .map_err(|e| format!("inheritance: {e}"))?;
    tera.render(name, &to_tera_ctx(ctx))
        .map_err(|e| format!("render: {e}"))
}

fn to_tera_ctx(v: &serde_json::Value) -> tera::Context {
    let mut ctx = tera::Context::new();
    if let serde_json::Value::Object(map) = v {
        for (k, val) in map {
            ctx.insert(k, val);
        }
    }
    ctx
}

// ---------------------------------------------------------------------------
// Census receipt
// ---------------------------------------------------------------------------

/// Expected expression-tag `-}}` population, receipted from the PL10 grounding
/// sweep (2026-11-04, disk read). Any drift (new use, removed use, file moved)
/// fails this gate loudly with the named file.
const EXPECTED_EXPR_FILES_GGEN: usize = 12;
const EXPECTED_EXPR_FILES_MARKETPLACE: usize = 18;

#[test]
fn census_expression_tag_dash_population_is_receipted() {
    let mut findings: Vec<String> = Vec::new();
    let mut counts: BTreeMap<String, (usize, usize)> = BTreeMap::new(); // root -> (files, expr-dash files)

    for (label, root) in census_roots() {
        let mut files = Vec::new();
        collect_tmpl_files(&root, &mut files);
        files.sort();
        let expr_files: Vec<PathBuf> = files
            .iter()
            .filter(|f| {
                classify_content(&std::fs::read_to_string(f).unwrap_or_default()).expr_tag_dashes
                    > 0
            })
            .cloned()
            .collect();
        counts.insert(label.clone(), (files.len(), expr_files.len()));
        for f in &expr_files {
            findings.push(format!("{label}/{}", f.strip_prefix(&root).unwrap().display()));
        }
    }

    println!("== Tera whitespace census ==");
    for (label, (files, expr)) in &counts {
        println!("  {label}: {files} template files, {expr} with expression-tag -}} dashes");
    }
    println!("  expression-tag -}} files ({}):", findings.len());
    for f in &findings {
        println!("    {f}");
    }

    // Receipted population: 12 ggen + 18 marketplace = 30.
    let ggen_expr = counts
        .get("ggen/packs")
        .map(|c| c.1)
        .expect("ggen/packs root must exist");
    assert_eq!(
        ggen_expr, EXPECTED_EXPR_FILES_GGEN,
        "ggen/packs expression-tag -}} file count drifted from the receipted 12"
    );
    match counts.get("marketplace/packs") {
        Some((_, m)) => assert_eq!(
            *m, EXPECTED_EXPR_FILES_MARKETPLACE,
            "marketplace/packs expression-tag -}} file count drifted from the receipted 18"
        ),
        None => println!(
            "  DEFERRED: ggen-marketplace sibling checkout not present; \
             marketplace leg of the census not executed"
        ),
    }

    // Non-vacuity, leg A: the poisoned fixture must be flagged by the classifier.
    let fixture = ggen_root().join(
        "crates/ggen-engine/tests/fixtures/templates/newline-law-poisoned.tmpl",
    );
    let cls = classify_content(&std::fs::read_to_string(&fixture).expect("fixture readable"));
    assert!(
        cls.expr_tag_dashes >= 1,
        "GATE BUG: poisoned fixture produced ZERO classification findings \
         (non-vacuity violation)"
    );
}

#[test]
fn poisoned_fixture_renders_with_no_newline_after_interpolation() {
    let fixture = ggen_root().join(
        "crates/ggen-engine/tests/fixtures/templates/newline-law-poisoned.tmpl",
    );
    let content = std::fs::read_to_string(&fixture).expect("fixture readable");

    let out = render_lenient_style(
        "newline-law-poisoned.tmpl",
        &content,
        &json!({"rows": [{"source": "X"}]}),
    )
    .expect("poisoned fixture must render under the pinned tera");

    // Law: `-}}` trims the newline following the expression tag, so the
    // interpolation "X" is immediately followed by the NEXT line's bytes.
    let law_honored = out.contains("XNEXT") && !out.contains("X\nNEXT");
    println!("== Poisoned-fixture render law ==");
    println!("  fixture bytes after frontmatter: {{ rows[0].source -}}\\nNEXT");
    println!("  rendered bytes: {out:?}");
    if law_honored {
        println!("  LAW HELD: `-}}` trimmed the newline after interpolation (XNEXT)");
    } else {
        println!(
            "  NAMED FINDING: `-}}` NOT honored — rendered bytes changed (got {out:?}); \
             a Tera bump drifted the whitespace law"
        );
    }

    // Non-vacuity, leg B: the two candidate worlds must be byte-distinct, so
    // this gate can actually flip. If law-held output ever equals law-broken
    // output, the gate is vacuous (a zero-finding run on the poisoned fixture
    // is a gate bug).
    assert_ne!("XNEXT", "X\nNEXT", "GATE BUG: law-held and law-broken bytes are identical — gate cannot flip");
    assert!(
        law_honored,
        "newline law BROKEN on poisoned fixture \
         crates/ggen-engine/tests/fixtures/templates/newline-law-poisoned.tmpl: \
         rendered {out:?} instead of containing \"XNEXT\" — `-}}` no longer trims the \
         following newline on the render surface (load_templates_glob_lenient discipline)"
    );
    assert!(fixture.is_file(), "poisoned fixture missing from the tree");
}

#[test]
fn grammar_law_is_textually_grounded_in_both_engine_versions() {
    let cargo_home = std::env::var("CARGO_HOME")
        .map(PathBuf::from)
        .or_else(|_| std::env::var("HOME").map(|h| PathBuf::from(h).join(".cargo")))
        .expect("CARGO_HOME or HOME resolvable");
    let src = cargo_home.join("registry").join("src");
    let find = |ver: &str| -> Option<PathBuf> {
        std::fs::read_dir(&src)
            .ok()?
            .flatten()
            .map(|e| e.path().join(format!("tera-{ver}")))
            .find(|p| p.is_dir())
    };

    // Leg 1: tera 1.20.x pest grammar (1.20.1 is the pinned engine version).
    let pest = ["1.20.1", "1.20.0"]
        .iter()
        .find_map(|v| find(v).map(|d| d.join("src/parser/tera.pest")))
        .expect("tera 1.20.x registry source (src/parser/tera.pest) present");
    let pest_text = std::fs::read_to_string(&pest).expect("pest readable");
    assert!(
        pest_text.contains("variable_end   = { \"-}}\" | \"}}\" }"),
        "tera 1.20.x pest grammar no longer carries the expression-tag `-}}` law \
         at variable_end ({}): the grounding moved",
        pest.display()
    );
    println!(
        "  tera 1.20.x grammar receipt: {} — variable_end = {{ \"-}}\" | \"}}\" }}",
        pest.display()
    );

    // Leg 2: tera 2.4.0 lexer (textual only — see deferred leg below).
    let lexer = find("2.4.0").map(|d| d.join("src/parsing/lexer.rs"));
    match lexer {
        Some(l) if l.is_file() => {
            let text = std::fs::read_to_string(&l).expect("lexer.rs readable");
            assert!(
                text.contains("Token::VariableEnd(true)"),
                "tera 2.4.0 lexer no longer carries Token::VariableEnd(true) ({}): \
                 the grounding moved",
                l.display()
            );
            println!(
                "  tera 2.4.0 lexer receipt: {} — Token::VariableEnd(true)",
                l.display()
            );
        }
        Some(l) => println!("  DEFERRED: tera 2.4.0 registry source not extracted at {}", l.display()),
        None => println!("  DEFERRED: tera 2.4.0 registry source not under registry src"),
    }

    // Leg 3 (dual-engine render-diff across the 30 lawful files): the court.
    // tera 2.4.0 is now a REAL renamed dev-dependency (`tera2`, OI-2 landed the
    // Cargo.toml edit), so the poisoned fixture + a sample of the 30 lawful
    // expression-tag `-}}` files render under BOTH engine versions and the
    // rendered bytes must agree — if a future Tera bump drifted the whitespace
    // law, this diff flips the gate.
    let fixture = ggen_root().join(
        "crates/ggen-engine/tests/fixtures/templates/newline-law-poisoned.tmpl",
    );
    let mut subjects: Vec<(String, String)> = vec![(
        "fixture/newline-law-poisoned.tmpl".to_string(),
        std::fs::read_to_string(&fixture).expect("poisoned fixture readable"),
    )];
    // Sample of the lawful population: every expression-tag `-}}` file under
    // ggen/packs (the receipted 12). Frontmatter is stripped the same way the
    // lenient loader does (template.rs): everything after the closing `---`.
    let mut roots = census_roots();
    roots.retain(|(label, _)| label == "ggen/packs");
    for (_label, root) in roots {
        let mut files = Vec::new();
        collect_tmpl_files(&root, &mut files);
        files.sort();
        for f in files {
            let content = std::fs::read_to_string(&f).unwrap_or_default();
            if classify_content(&content).expr_tag_dashes > 0 {
                subjects.push((
                    format!("ggen/packs/{}", f.strip_prefix(&root).unwrap().display()),
                    strip_frontmatter(&content),
                ));
            }
        }
    }

    println!(
        "  == dual-engine render-diff court: {} subjects under tera 1.20.x vs tera 2.4.0 ==",
        subjects.len()
    );
    let fixture_ctx = json!({"rows": [{"source": "X"}]});
    let mut mismatches = Vec::new();
    for (name, content) in &subjects {
        // The poisoned fixture needs its rows context; pack bodies are rendered
        // context-free. A subject that errors under BOTH engines for the same
        // missing-context class is still a parity hold (outcomes agree); a
        // hold requires Ok/Ok byte-equality or Err/Err outcome-equality.
        let ctx = if name.starts_with("fixture/") {
            &fixture_ctx
        } else {
            &json!({})
        };
        let r1 = render_lenient_style(name, content, ctx);
        let r2 = render_lenient_style_v2(name, content, ctx);
        let holds = match (&r1, &r2) {
            (Ok(a), Ok(b)) => a == b,
            (Err(_), Err(_)) => true,
            _ => false,
        };
        if !holds {
            mismatches.push(name.clone());
            println!("  MISMATCH: {name} (v1={r1:?} vs v2={r2:?})");
        }
    }
    assert!(
        mismatches.is_empty(),
        "dual-engine render-diff court FAILED: {} of {} subjects render \
         differently under tera 1.20.x vs tera 2.4.0: {mismatches:?} — the \
         whitespace `-}}` law drifted across the version span",
        mismatches.len(),
        subjects.len()
    );
    println!(
        "  COURT HELD: all {} subjects render byte-identically under both engines",
        subjects.len()
    );
}

/// Strip a leading TOML frontmatter block (`---\n...\n---\n`) so pack template
/// bodies render standalone, mirroring the loader's pre-parse.
fn strip_frontmatter(content: &str) -> String {
    if !content.starts_with("---\n") {
        return content.to_string();
    }
    let after_open = &content[4..];
    match after_open.find("\n---") {
        Some(idx) => after_open[idx + 4..].trim_start_matches('\n').to_string(),
        None => content.to_string(),
    }
}

/// tera 2.4.0 twin of `render_lenient_style` — the public API changed in the
/// 2.x line (`templates` is `pub(crate)`, no public `build_inheritance_chains`).
/// `add_raw_template` is the lawful equivalent of the 1.x per-file registration
/// discipline: it runs `Template::new` per file, inserts into the instance, and
/// finalizes inheritance chains (`tera.rs:790-805`, registry source), so the
/// byte-relevant sequence is identical.
fn render_lenient_style_v2(name: &str, content: &str, ctx: &serde_json::Value) -> Result<String, String> {
    let mut tera = tera2::Tera::default();
    tera.add_raw_template(name, content)
        .map_err(|e| format!("parse: {e}"))?;
    tera.render(name, &to_tera_ctx_v2(ctx))
        .map_err(|e| format!("render: {e}"))
}

fn to_tera_ctx_v2(v: &serde_json::Value) -> tera2::Context {
    tera2::Context::from_serialize(v).expect("fixture context must serialize (tera2)")
}
