from pathlib import Path
import re


def replace_once(text: str, old: str, new: str, label: str) -> str:
    count = text.count(old)
    if count != 1:
        raise SystemExit(f"{label}: expected exactly one match, found {count}")
    return text.replace(old, new, 1)


def sub_once(text: str, pattern: str, replacement: str, label: str) -> str:
    updated, count = re.subn(pattern, lambda _: replacement, text, count=1, flags=re.S)
    if count != 1:
        raise SystemExit(f"{label}: expected exactly one regex match, found {count}")
    return updated


sync_path = Path("crates/ggen-engine/src/sync.rs")
sync = sync_path.read_text()

sync = replace_once(
    sync,
    """struct PendingWrite<'a> {
    to: String,
    body: String,
    tpl: &'a Template,
}
""",
    """struct PendingWrite {
    to: String,
    body: String,
    frontmatter: Frontmatter,
}
""",
    "PendingWrite ownership",
)

sync = replace_once(
    sync,
    """    // Storage slot for the synthetic module-aggregator template (declared
    // before `pending` so its borrow can live as long as `pending`'s
    // entries do; only populated when `[templates] aggregate_modules` is
    // set — see below, after the render loop).
    let mut aggregator_slot: Option<Template> = None;
    let mut pending: Vec<PendingWrite<'_>> = Vec::new();
""",
    """    // Every pending write owns the output-phase frontmatter rendered from
    // the same semantic context as its path and body.
    let mut pending: Vec<PendingWrite> = Vec::new();
""",
    "pending storage",
)

sync = replace_once(
    sync,
    "        check_shape_files_exist(root, tpl_path, &tpl.frontmatter.shape)?;\n\n",
    "",
    "pre-render shape check",
)

sync = sub_once(
    sync,
    r'''                let to = render_str\(active_tera, &tpl\.frontmatter\.to, &ctx, tpl_path\)\?;
                let body = render_str\(active_tera, &tpl\.body, &ctx, tpl_path\)\?;
                check_determinism\(
                    active_tera,
                    &tpl\.body,
                    tpl_path,
                    &tpl\.frontmatter,
                    determinism_recheck\.as_ref\(\),
                    Some\(row_index\),
                    &body,
                    &to,
                \)\?;
                pending\.push\(PendingWrite \{ to, body, tpl \}\);''',
    '''                let to = render_str(active_tera, &tpl.frontmatter.to, &ctx, tpl_path)?;
                let body = render_str(active_tera, &tpl.body, &ctx, tpl_path)?;
                let frontmatter = render_output_frontmatter(
                    active_tera,
                    &tpl.frontmatter,
                    &ctx,
                    tpl_path,
                    &to,
                )?;
                check_shape_files_exist(root, tpl_path, &frontmatter.shape)?;
                check_determinism(
                    active_tera,
                    &tpl.body,
                    tpl_path,
                    &tpl.frontmatter,
                    determinism_recheck.as_ref(),
                    Some(row_index),
                    &body,
                    &frontmatter,
                )?;
                pending.push(PendingWrite {
                    to,
                    body,
                    frontmatter,
                });''',
    "per-row output rendering",
)

sync = sub_once(
    sync,
    r'''        \} else \{
            let ctx = base_context\(&named, &results\);
            let body = render_str\(active_tera, &tpl\.body, &ctx, tpl_path\)\?;
            check_determinism\(
                active_tera,
                &tpl\.body,
                tpl_path,
                &tpl\.frontmatter,
                determinism_recheck\.as_ref\(\),
                None,
                &body,
                &tpl\.frontmatter\.to,
            \)\?;
            pending\.push\(PendingWrite \{
                to: tpl\.frontmatter\.to\.clone\(\),
                body,
                tpl,
            \}\);
        \}''',
    '''        } else {
            let ctx = base_context(&named, &results);
            let to = tpl.frontmatter.to.clone();
            let body = render_str(active_tera, &tpl.body, &ctx, tpl_path)?;
            let frontmatter = render_output_frontmatter(
                active_tera,
                &tpl.frontmatter,
                &ctx,
                tpl_path,
                &to,
            )?;
            check_shape_files_exist(root, tpl_path, &frontmatter.shape)?;
            check_determinism(
                active_tera,
                &tpl.body,
                tpl_path,
                &tpl.frontmatter,
                determinism_recheck.as_ref(),
                None,
                &body,
                &frontmatter,
            )?;
            pending.push(PendingWrite {
                to,
                body,
                frontmatter,
            });
        }''',
    "single-output rendering",
)

aggregator_start = sync.index("            aggregator_slot = Some(Template {\n")
aggregator_end_marker = """            pending.push(PendingWrite {
                to: AGGREGATOR_REL_PATH.to_string(),
                body,
                tpl: tpl_ref,
            });
"""
aggregator_end = sync.index(aggregator_end_marker, aggregator_start) + len(aggregator_end_marker)
aggregator_replacement = """            let frontmatter = Frontmatter {
                to: AGGREGATOR_REL_PATH.to_string(),
                sparql: BTreeMap::new(),
                construct: None,
                inject: false,
                before: None,
                after: None,
                at_line: None,
                skip_if: None,
                unless_exists: false,
                force: true,
                when: None,
                skip_empty: false,
                from: None,
                sh_before: None,
                sh_after: None,
                backup: false,
                shape: Vec::new(),
                determinism: None,
                freeze_policy: None,
                freeze_slots_dir: None,
                rdf: Vec::new(),
                rdf_inline: Vec::new(),
                prefixes: BTreeMap::new(),
                base: None,
            };
            pending.push(PendingWrite {
                to: AGGREGATOR_REL_PATH.to_string(),
                body,
                frontmatter,
            });
"""
sync = sync[:aggregator_start] + aggregator_replacement + sync[aggregator_end:]

sync = replace_once(
    sync,
    "            pw.tpl,\n",
    "            &pw.frontmatter,\n",
    "apply rendered frontmatter",
)

render_marker = """fn render_str(
    tera: &mut tera::Tera, template: &str, ctx: &tera::Context, tpl_path: &Path,
) -> Result<String> {
    tera.render_str(template, ctx).map_err(|e| {
        AppError::fm_tpl(
            17,
            format!(
                "render failed for {}: {e}. Available top-level context keys: {}.",
                tpl_path.display(),
                context_key_summary(ctx)
            ),
        )
    })
}
"""
render_helpers = render_marker + """

/// Render one optional output-phase string through the same Tera context
/// as the output path and body.
fn render_optional_output_field(
    tera: &mut tera::Tera,
    value: Option<&str>,
    ctx: &tera::Context,
    tpl_path: &Path,
) -> Result<Option<String>> {
    value
        .map(|template| render_str(tera, template, ctx, tpl_path))
        .transpose()
}

/// Compile the output-phase frontmatter for one materialized projection.
///
/// Resolve/enrich/extract fields remain static because they determine the
/// semantic context itself. Structural markers, idempotence needles,
/// hooks, shape paths, and checksum-slot paths are rendered after query
/// extraction so one ontology row can lawfully specialize the complete
/// write lifecycle rather than only `to:` and the body.
fn render_output_frontmatter(
    tera: &mut tera::Tera,
    source: &Frontmatter,
    ctx: &tera::Context,
    tpl_path: &Path,
    rendered_to: &str,
) -> Result<Frontmatter> {
    let mut rendered = source.clone();
    rendered.to = rendered_to.to_string();
    rendered.before =
        render_optional_output_field(tera, source.before.as_deref(), ctx, tpl_path)?;
    rendered.after =
        render_optional_output_field(tera, source.after.as_deref(), ctx, tpl_path)?;
    rendered.skip_if =
        render_optional_output_field(tera, source.skip_if.as_deref(), ctx, tpl_path)?;
    rendered.sh_before =
        render_optional_output_field(tera, source.sh_before.as_deref(), ctx, tpl_path)?;
    rendered.sh_after =
        render_optional_output_field(tera, source.sh_after.as_deref(), ctx, tpl_path)?;
    rendered.freeze_slots_dir = render_optional_output_field(
        tera,
        source.freeze_slots_dir.as_deref(),
        ctx,
        tpl_path,
    )?;
    rendered.shape = source
        .shape
        .iter()
        .map(|shape| render_str(tera, shape, ctx, tpl_path))
        .collect::<Result<Vec<_>>>()?;
    Ok(rendered)
}
"""
sync = replace_once(sync, render_marker, render_helpers, "output frontmatter helper")

apply_start = sync.index("fn apply(\n")
apply_end = sync.index("\n/// Refuse `cmd` against the shell-command denylist", apply_start)
apply_chunk = sync[apply_start:apply_end]
apply_chunk = replace_once(
    apply_chunk,
    "    root: &Path, rel_to: &str, body: &str, tpl: &Template, opts: SyncOptions,\n",
    "    root: &Path, rel_to: &str, body: &str, frontmatter: &Frontmatter, opts: SyncOptions,\n",
    "apply signature",
)
apply_chunk = apply_chunk.replace("tpl.frontmatter", "frontmatter")
apply_chunk = apply_chunk.replace(
    "plan_write(root, rel_to, body, &frontmatter)",
    "plan_write(root, rel_to, body, frontmatter)",
)
sync = sync[:apply_start] + apply_chunk + sync[apply_end:]

sync = replace_once(
    sync,
    """fn check_determinism(
    tera: &mut tera::Tera, body_template: &str, tpl_path: &Path,
    frontmatter: &crate::template::Frontmatter, determinism_recheck: Option<&ExtractedRows>,
    row_index: Option<usize>, first_render: &str, first_to: &str,
) -> Result<()> {
""",
    """fn check_determinism(
    tera: &mut tera::Tera, body_template: &str, tpl_path: &Path,
    frontmatter: &crate::template::Frontmatter, determinism_recheck: Option<&ExtractedRows>,
    row_index: Option<usize>, first_render: &str, first_frontmatter: &Frontmatter,
) -> Result<()> {
""",
    "determinism signature",
)

sync = sub_once(
    sync,
    r'''    // The templated `to:` path is part of the output — a non-deterministic
    // path escapes a body-only check\.
    let second_to = render_str\(tera, &frontmatter\.to, &ctx2, tpl_path\)\?;
    if second_to != first_to \{
        return Err\(AppError::fm_tpl\(
            9,
            format!\(
                "\{\}: `determinism: true` violated — re-rendering the `to:` path from a \\
                 second, independent query execution produced `\{second_to\}` after \\
                 `\{first_to\}`\. \\
                 Remediation: remove non-deterministic terms from the query or from `to:`\.",
                tpl_path\.display\(\)
            \),
        \)\);
    \}''',
    '''    // The complete output-phase frontmatter is part of the projection.
    // Recheck path, injection markers, idempotence needle, hook commands,
    // shape paths, and checksum-slot path against independently extracted
    // bindings; body-only determinism would leave those consequences open.
    let second_to = render_str(tera, &frontmatter.to, &ctx2, tpl_path)?;
    let second_frontmatter =
        render_output_frontmatter(tera, frontmatter, &ctx2, tpl_path, &second_to)?;
    if &second_frontmatter != first_frontmatter {
        return Err(AppError::fm_tpl(
            9,
            format!(
                "{}: `determinism: true` violated — re-rendering output-phase \
                 frontmatter from a second, independent query execution produced \
                 different path, composition, hook, shape, or freeze-slot semantics. \
                 Remediation: remove non-deterministic terms from the query or \
                 output-phase frontmatter.",
                tpl_path.display()
            ),
        ));
    }''',
    "determinism output frontmatter",
)

sync_path.write_text(sync)

schema_path = Path("crates/ggen-engine/schema/frontmatter-schema.ttl")
schema = schema_path.read_text()
schema_replacements = {
    'rdfs:comment    "Closed frontmatter key set for a ggen template, Hygen semantics (crate::template::Frontmatter)." ;':
        'rdfs:comment    "Closed frontmatter key set for a ggen template: Hygen-derived write semantics plus ontology-driven output-phase Tera projection (crate::template::Frontmatter)." ;',
    'rdfs:comment "Inject before the first line containing this marker." .':
        'rdfs:comment "Inject before the first line containing this marker. Tera-rendered per output from the same query context as to: and the body." .',
    'rdfs:comment "Inject after the first line containing this marker." .':
        'rdfs:comment "Inject after the first line containing this marker. Tera-rendered per output from the same query context as to: and the body." .',
    'rdfs:comment "Skip the write when the existing file already contains this substring." .':
        'rdfs:comment "Skip the write when the existing file already contains this substring. Tera-rendered per output, enabling row-specific idempotence." .',
    'rdfs:comment "Shell command run before the write decision, cwd = project root. Refused (not executed) if it matches shell_safety::check_shell_command_safe\'s denylist — a bounded denylist, not a sandbox." .':
        'rdfs:comment "Shell command Tera-rendered per output and run after all templates render but before this output\'s write decision, cwd = project root. It still runs when the later write decision skips. Refused if it matches the bounded shell denylist; this is not a sandbox." .',
    'rdfs:comment "Shell command run after a successful Written/Injected outcome (never after Skipped). Same denylist and cwd as sh_before." .':
        'rdfs:comment "Shell command Tera-rendered per output and run after a successful Written/Injected outcome (never after Skipped). Same bounded denylist and cwd as sh_before." .',
    'rdfs:comment "SHACL shape file paths (relative to the project root) declared as governing this output. LIMITATION: existence-checked only — no SHACL engine runs in this crate yet, so listed shapes are not evaluated against rendered output." .':
        'rdfs:comment "Tera-renderable SHACL shape file paths (relative to the project root) declared as governing each output. LIMITATION: existence-checked only — no SHACL engine runs in this crate yet, so listed shapes are not evaluated against rendered output." .',
    'rdfs:comment "Directory (relative to the project root) storing per-output BLAKE3 checksums for freeze_policy: checksum. Required when that policy is set; ignored otherwise." .':
        'rdfs:comment "Tera-renderable directory (relative to the project root) storing per-output BLAKE3 checksums for freeze_policy: checksum. Required when that policy is set; ignored otherwise." .',
}
for old, new in schema_replacements.items():
    schema = replace_once(schema, old, new, f"schema comment: {old[:30]}")
schema_path.write_text(schema)

test_path = Path("crates/ggen-engine/tests/frontmatter_maximalism_e2e.rs")
test_path.write_text(r'''//! Real-boundary coverage for ontology-driven output-phase frontmatter.
//!
//! These tests cross the filesystem and subprocess boundaries. They prove
//! that Tera specialization applies to structural slots, idempotence,
//! lifecycle hooks, and shape paths—not only to the output path and body.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "frontmatter-maximalism"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alpha ex:name "alpha" .
"#;

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::create_dir_all(root.join("targets")).expect("mkdir targets");
    std::fs::create_dir_all(root.join("shapes")).expect("mkdir shapes");
}

fn write_template(root: &Path, content: &str) {
    std::fs::write(root.join("templates/maximal.tmpl"), content).expect("write template");
}

#[test]
fn row_context_projects_structural_hooks_and_shape_paths() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("targets/alpha.txt"),
        "header\n// SLOT:alpha\nfooter\n",
    )
    .expect("seed target");
    std::fs::write(dir.path().join("shapes/alpha.ttl"), "# governing shape\n")
        .expect("write shape");

    write_template(
        dir.path(),
        r#"---
to: "targets/{{ row.name }}.txt"
sparql:
  entities: |
    SELECT ?name WHERE {
      ?entity <http://example.org/name> ?name .
    }
    ORDER BY ?name
inject: true
before: "// SLOT:{{ row.name }}"
skip_if: "generated {{ row.name }}"
sh_before: "echo before-{{ row.name }} >> hooks.log"
sh_after: "echo after-{{ row.name }} >> hooks.log"
shape:
  - "shapes/{{ row.name }}.ttl"
determinism: true
---
generated {{ row.name }}
"#,
    );

    let first = sync(dir.path(), SyncOptions::default()).expect("first sync");
    assert_eq!(
        first.written,
        vec![std::path::PathBuf::from("targets/alpha.txt")]
    );

    let target = std::fs::read_to_string(dir.path().join("targets/alpha.txt"))
        .expect("read injected target");
    let generated = target.find("generated alpha").expect("generated text");
    let slot = target.find("// SLOT:alpha").expect("slot marker");
    assert!(
        generated < slot,
        "content must be injected before the rendered slot"
    );

    let second = sync(dir.path(), SyncOptions::default()).expect("second sync");
    assert!(
        second.written.is_empty(),
        "skip_if must make the second sync a no-op"
    );
    assert_eq!(second.skipped.len(), 1);

    let hooks =
        std::fs::read_to_string(dir.path().join("hooks.log")).expect("read real hook evidence");
    assert_eq!(
        hooks.lines().collect::<Vec<_>>(),
        vec!["before-alpha", "after-alpha", "before-alpha"],
        "sh_before runs before the later skip decision; sh_after runs only after mutation"
    );
}

#[test]
fn rendered_shape_path_refuses_before_any_hook_or_write() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("targets/alpha.txt"),
        "header\n// SLOT:alpha\nfooter\n",
    )
    .expect("seed target");

    write_template(
        dir.path(),
        r#"---
to: "targets/{{ row.name }}.txt"
sparql:
  entities: |
    SELECT ?name WHERE {
      ?entity <http://example.org/name> ?name .
    }
    ORDER BY ?name
inject: true
before: "// SLOT:{{ row.name }}"
sh_before: "echo should-not-run >> hooks.log"
shape:
  - "shapes/{{ row.name }}.ttl"
---
generated {{ row.name }}
"#,
    );

    let error =
        sync(dir.path(), SyncOptions::default()).expect_err("missing rendered shape must refuse");
    assert!(error.to_string().contains("FM-TPL-014"), "{error}");
    assert!(!dir.path().join("hooks.log").exists());
    assert_eq!(
        std::fs::read_to_string(dir.path().join("targets/alpha.txt"))
            .expect("target remains readable"),
        "header\n// SLOT:alpha\nfooter\n"
    );
}
''')

docs_path = Path("docs/reference/frontmatter-maximalism.md")
docs_path.write_text(r'''# Frontmatter maximalism

ggen frontmatter is a closed ontology-to-infrastructure projection program. Its Hygen lineage remains visible in `to`, injection, preservation, and shell lifecycle semantics. ggen extends that local generator grammar with admitted RDF, SPARQL extraction, semantic derivation, deterministic rendering, freeze ownership, and chained receipts.

## Phase law

Properties are evaluated in the phase where their information is available:

| Phase | Properties | Law |
|---|---|---|
| Resolve | `from`, `rdf`, `rdf_inline`, `prefixes`, `base` | Establish the template body and semantic overlay before query extraction. |
| Derive and extract | `construct`, `when`, `sparql` | Manufacture and select the admitted knowledge view. |
| Output projection | `to`, body, `before`, `after`, `skip_if`, `sh_before`, `sh_after`, `shape`, `freeze_slots_dir` | Tera-render from the same bounded query context for each materialized output. |
| Write law | `inject`, `at_line`, `unless_exists`, `force`, `skip_empty`, `backup`, `freeze_policy` | Decide ownership, composition, preservation, and mutation. |

A dynamic `to:` creates one output context per driving SPARQL row. All output-projection properties receive that same `row` plus its top-level bindings. This permits one ontology fact to specialize the path, structural slot, duplicate-prevention needle, native lifecycle command, governing shape, and checksum namespace together.

## Structural ports

`before` and `after` are Tera-renderable structural ports. They let a host artifact export stable composition slots while a pack projects row-specific content into the appropriate slot. `skip_if` is rendered from the same context, so idempotence can name the exact consequence being injected.

```yaml
to: "src/{{ row.module }}/mod.rs"
inject: true
before: "// GGEN:SLOT:{{ row.capability }}:END"
skip_if: "pub mod {{ row.generated_module }};"
```

Marker matching remains first-line substring matching. Missing markers fail closed. Two outputs resolving to the same target in one sync are still refused rather than ordered implicitly.

## Shell lifecycle boundary

`sh_before` and `sh_after` are Tera-rendered per output, then checked by the existing bounded denylist before `sh -c` execution in the project root.

The phases are intentionally asymmetric:

1. Every template and every output-phase property renders before any filesystem mutation.
2. `sh_before` runs immediately before that output's write decision. It therefore still runs when `unless_exists`, `skip_if`, freeze policy, or unchanged-content logic later skips the write.
3. `sh_after` runs only after `Written` or `Injected`, never after `Skipped`.
4. Dry-run executes neither hook.

Shell hooks are not a sandbox and do not receive authority merely because a pack declares them. External packs containing hooks must be treated as executable supply-chain inputs and admitted accordingly.

## Determinism

`determinism: true` now re-executes query extraction and compares the complete output projection: path, body, structural markers, idempotence needle, hook commands, shape paths, and checksum-slot path. A stable body with an unstable actuator or composition target is a determinism violation.

## Hygen lineage and extension

Hygen established the project-local action grammar:

```text
add → preserve → inject → execute
```

ggen preserves that fence and adds semantic standing:

```text
admit → derive → project → compose → actuate → verify → receipt → replay
```

Frontmatter maximalism does not mean every property executes in every phase. It means every property is used to its maximum lawful consequence under explicit phase, ownership, authority, and receipt boundaries.
''')
