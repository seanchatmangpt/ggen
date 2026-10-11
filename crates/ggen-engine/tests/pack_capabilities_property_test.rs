//! Property-style coverage of the FM-PACK-018 two-tier capability rule via
//! the REAL resolve path (`GgenConfig::load` + `pack::resolve`) over a
//! fixed-seed deterministic scenario generator (hand-rolled LCG, no proptest
//! dep — same pattern as `receipt_chain_differential_test`).
//!
//! Scenario dimensions varied per scenario:
//! - URN-form requires (`urn:ggen:pack:<name>`) naming declared vs undeclared packs
//! - non-URN requires satisfied by the declared transitive dependency closure
//!   (a→b→c chain) vs provided only by a declared out-of-closure pack vs
//!   provided by nothing
//! - mutual URN requires between two declared packs (H2: must NOT fabricate a
//!   dependency cycle, i.e. never FM-PACK-016)
//! - empty requires
//!
//! Invariants asserted per scenario:
//! 1. Every URN require resolves iff the stripped name is in the consumer's
//!    declared pack universe (Tier 1).
//! 2. Every non-URN require resolves only via the requiring pack's declared
//!    transitive closure provides — a declared-but-undeclaring-closure
//!    provider never satisfies (Tier 2, fail-closed).
//! 3. Mutual URN requires between declared packs compose without a false
//!    cycle: FM-PACK-016 never fires.
//! 4. FM-PACK-018 fires exactly when a URN require's name is undeclared or a
//!    non-URN require has no closure provider — and never otherwise.
//!
//! Real filesystem (`tempfile::TempDir`), real TOML parsing, real pack
//! resolution — no mocks (Chicago).

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::struct_excessive_bools,
    clippy::trivially_copy_pass_by_ref,
    clippy::cast_possible_truncation
)]

use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use ggen_engine::config::GgenConfig;
use ggen_engine::pack::resolve;
use tempfile::TempDir;

/// Fixed seed: every run generates the identical 50-scenario matrix.
const SEED: u64 = 0xFACE_B00C;
const SCENARIOS: u64 = 50;

struct Lcg(u64);

impl Lcg {
    fn next(&mut self) -> u64 {
        self.0 = self
            .0
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1_442_695_040_888_963_407);
        self.0
    }

    fn below(&mut self, n: u64) -> bool {
        (self.next() >> 32) % n == 0
    }
}

fn write(root: &Path, rel: &str, content: &str) {
    let path = root.join(rel);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("mkdir parent");
    }
    std::fs::write(path, content).expect("write file");
}

/// Real on-disk pack: pack.toml + ontology.ttl + one template (FM-PACK-005).
fn write_pack(
    root: &Path, name: &str, dependencies: &[&str], provides: &[&str], requires: &[&str],
) {
    fn toml_array(values: &[&str]) -> String {
        let body = values
            .iter()
            .map(|v| format!("\"{v}\""))
            .collect::<Vec<_>>()
            .join(", ");
        format!("[{body}]")
    }

    let local = name.replace('-', "_");
    let mut manifest = format!(
        "[pack]\nname = \"{name}\"\nversion = \"1.0.0\"\n\
         description = \"property fixture\"\n"
    );
    if !dependencies.is_empty() {
        manifest.push_str("\n[dependencies]\n");
        for d in dependencies {
            let _ = writeln!(manifest, "{d} = \"1.0.0\"");
        }
    }
    if !provides.is_empty() || !requires.is_empty() {
        manifest.push_str("\n[capabilities]\n");
        let _ = writeln!(manifest, "provides = {}", toml_array(provides));
        let _ = writeln!(manifest, "requires = {}", toml_array(requires));
    }

    write(root, &format!("packs/{name}/pack.toml"), &manifest);
    write(
        root,
        &format!("packs/{name}/ontology.ttl"),
        &format!("@prefix ex: <http://example.com/prop#> .\nex:{local} a ex:Pack .\n"),
    );
    write(
        root,
        &format!("packs/{name}/templates/{local}.md.tmpl"),
        &format!("---\nto: docs/{local}.md\n---\n# {name}\n"),
    );
}

/// Deterministic scenario description.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Scenario {
    /// `a` requires `urn:ggen:pack:c-leaf` — declared: Tier-1 must pass
    /// (satisfied by consumer [packs] declaration alone; see
    /// `pack_two_tier_satisfaction_test` and FM-PACK-018).
    urn_ok: bool,
    /// `a` requires `urn:ggen:pack:ghost-p` — undeclared: Tier-1 must fail.
    urn_bad: bool,
    /// `a` requires `cap.c` — provided inside its declared closure (a→b→c).
    nonurn_ok: bool,
    /// `a` requires `cap.out` — provided only by declared out-of-closure `outlier`.
    nonurn_outlier: bool,
    /// `a` requires `cap.nope` — provided by nothing: Tier-2 must fail.
    nonurn_bad: bool,
    /// Declared pair `mut1`/`mut2` with mutual URN requires (H2 invariant).
    mutual: bool,
}

impl Scenario {
    fn generate(rng: &mut Lcg) -> Self {
        Scenario {
            urn_ok: rng.below(2),
            urn_bad: rng.below(2),
            nonurn_ok: rng.below(2),
            nonurn_outlier: rng.below(2),
            nonurn_bad: rng.below(2),
            mutual: rng.below(2),
        }
    }

    /// Pack `a`'s requires list.
    fn a_requires(&self) -> Vec<String> {
        let mut r = Vec::new();
        if self.urn_ok {
            r.push("urn:ggen:pack:c-leaf".to_string());
        }
        if self.urn_bad {
            r.push("urn:ggen:pack:ghost-p".to_string());
        }
        if self.nonurn_ok {
            r.push("cap.c".to_string());
        }
        if self.nonurn_outlier {
            r.push("cap.out".to_string());
        }
        if self.nonurn_bad {
            r.push("cap.nope".to_string());
        }
        r
    }

    /// The scenario must resolve green iff nothing violates the two tiers.
    fn expect_ok(&self) -> bool {
        !(self.urn_bad || self.nonurn_outlier || self.nonurn_bad)
    }

    /// Names a requires entry must be blamed for when the scenario fails.
    fn expected_blamed(&self) -> Vec<&'static str> {
        let mut blamed = Vec::new();
        if self.urn_bad {
            blamed.push("ghost-p");
        }
        if self.nonurn_outlier {
            blamed.push("cap.out");
        }
        if self.nonurn_bad {
            blamed.push("cap.nope");
        }
        blamed
    }
}

fn build(s: Scenario) -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path().to_path_buf();

    // Declared transitive chain a→b→c (a requires go on `a`; b, c clean).
    write_pack(
        &root,
        "a-chain",
        &["b-mid"],
        &["cap.a"],
        &s.a_requires()
            .iter()
            .map(String::as_str)
            .collect::<Vec<_>>(),
    );
    write_pack(&root, "b-mid", &["c-leaf"], &["cap.b"], &[]);
    write_pack(&root, "c-leaf", &[], &["cap.c"], &[]);
    // Declared provider OUTSIDE a's dependency closure (no edges to it).
    write_pack(&root, "outlier", &[], &["cap.out"], &[]);
    // Mutual URN-require pair (both declared — Tier 1 satisfied by universe).
    let (m1_req, m2_req): (&[&str], &[&str]) = if s.mutual {
        (&["urn:ggen:pack:mut2"], &["urn:ggen:pack:mut1"])
    } else {
        (&[], &[])
    };
    write_pack(&root, "mut1", &[], &["cap.m1"], m1_req);
    write_pack(&root, "mut2", &[], &["cap.m2"], m2_req);

    // Consumer declares the whole universe.
    let mut manifest = String::from(
        "[project]\nname = \"prop-fixture\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [templates]\ndir = \"templates\"\n\n",
    );
    for name in ["a-chain", "b-mid", "c-leaf", "outlier", "mut1", "mut2"] {
        let _ = writeln!(manifest, "[packs.{name}]\npath = \"packs/{name}\"\n");
    }
    write(&root, "ggen.toml", &manifest);
    write(&root, "ontology.ttl", "");

    (dir, root.clone())
}

#[test]
fn two_tier_rule_holds_across_generated_scenarios() {
    let mut rng = Lcg(SEED);
    let mut ok_count = 0u32;
    let mut refused_count = 0u32;

    for i in 0..SCENARIOS {
        // Guarantee coverage regardless of the draw: scenario 0 is the
        // fully-green corner, scenario 1 the fully-refused corner, and the
        // mutual-pair corner is forced green too.
        let mut s = Scenario::generate(&mut rng);
        if i == 0 {
            s = Scenario {
                urn_ok: true,
                urn_bad: false,
                nonurn_ok: true,
                nonurn_outlier: false,
                nonurn_bad: false,
                mutual: true,
            };
        } else if i == 1 {
            s = Scenario {
                urn_ok: true,
                urn_bad: true,
                nonurn_ok: true,
                nonurn_outlier: true,
                nonurn_bad: true,
                mutual: true,
            };
        }
        let (_dir, root) = build(s);

        let config = GgenConfig::load(&root.join("ggen.toml")).expect("load config");
        let result = resolve(&config, &root);

        // Invariant 3: FM-PACK-016 (false cycle from mutual URN requires)
        // must NEVER fire — mutual requires are consumer advice, not edges.
        if let Err(ref e) = result {
            let msg = e.to_string();
            assert!(
                !msg.contains("[FM-PACK-016]"),
                "scenario {i} ({s:?}): mutual URN requires must not fabricate a \
                 dependency cycle, got: {msg}"
            );
        }

        if s.expect_ok() {
            ok_count += 1;
            let packs = result
                .unwrap_or_else(|e| panic!("scenario {i} ({s:?}): must resolve green, got: {e}"));
            // Invariant 1: resolved requires survive verbatim; the declared
            // universe contains every URN target.
            let declared: std::collections::BTreeSet<&str> =
                packs.iter().map(|p| p.name.as_str()).collect();
            for pack in &packs {
                for req in &pack.requires {
                    if let Some(target) = req.strip_prefix("urn:ggen:pack:") {
                        assert!(
                            declared.contains(target),
                            "scenario {i}: resolved URN require {req} names undeclared \
                             pack under a green scenario"
                        );
                    }
                }
            }
            // Invariant 3 (positive): mutual pair resolved together.
            if s.mutual {
                let names: Vec<&str> = packs.iter().map(|p| p.name.as_str()).collect();
                assert!(
                    names.contains(&"mut1") && names.contains(&"mut2"),
                    "scenario {i}: mutual pair must both resolve"
                );
            }
        } else {
            refused_count += 1;
            let err = result.expect_err("scenario must refuse").to_string();
            // Invariant 4: FM-PACK-018 fires exactly when expected.
            assert!(
                err.contains("[FM-PACK-018]"),
                "scenario {i} ({s:?}): expected FM-PACK-018, got: {err}"
            );
            // The refusal names at least one actually-unsatisfied require.
            assert!(
                s.expected_blamed().iter().any(|b| err.contains(b)),
                "scenario {i}: refusal must name an unsatisfied require \
                 {:?}, got: {err}",
                s.expected_blamed()
            );
            // Tier-1 failure must blame the URN target as a pack reference.
            if s.urn_bad && !s.nonurn_outlier && !s.nonurn_bad {
                assert!(
                    err.contains("does not declare the referenced pack"),
                    "scenario {i}: pure Tier-1 failure must use the undeclared-pack \
                     refusal, got: {err}"
                );
            }
            // Tier-2 failure must use the closure refusal.
            if (s.nonurn_outlier || s.nonurn_bad) && !s.urn_bad {
                assert!(
                    err.contains("no provider exists in its declared dependency closure"),
                    "scenario {i}: pure Tier-2 failure must use the closure refusal, \
                     got: {err}"
                );
            }
        }
    }

    assert_eq!(
        ok_count + refused_count,
        SCENARIOS as u32,
        "every scenario must classify"
    );
    assert!(
        ok_count > 0 && refused_count > 0,
        "matrix must cover both outcomes (ok={ok_count}, refused={refused_count})"
    );
}

/// Targeted corner: empty requires resolve green on the same universe — the
/// generator hits it only sometimes; this pins it deterministically.
#[test]
fn empty_requires_resolve_green() {
    let s = Scenario {
        urn_ok: false,
        urn_bad: false,
        nonurn_ok: false,
        nonurn_outlier: false,
        nonurn_bad: false,
        mutual: true,
    };
    let (_dir, root) = build(s);
    let config = GgenConfig::load(&root.join("ggen.toml")).expect("load config");
    let packs = resolve(&config, &root).expect("empty-requires universe must resolve");
    assert_eq!(packs.len(), 6);
}

/// Targeted corner: pure Tier-1 (URN names a pack that exists in the
/// universe but provides nothing relevant) is satisfied by DECLARATION alone
/// — Tier 1 never consults provides.
#[test]
fn tier1_satisfied_by_declaration_not_provides() {
    let s = Scenario {
        urn_ok: false,
        urn_bad: false,
        nonurn_ok: false,
        nonurn_outlier: false,
        nonurn_bad: false,
        mutual: false,
    };
    let (_dir, root) = build(s);
    // Add a requires on `a` for urn:ggen:pack:mut1 — mut1 provides cap.m1
    // (irrelevant); declaration alone must satisfy Tier 1.
    let toml_path = root.join("packs/a-chain/pack.toml");
    let manifest = std::fs::read_to_string(&toml_path).expect("read pack.toml");
    std::fs::write(
        &toml_path,
        manifest.replace(
            "requires = [\"cap.a\"]",
            "requires = [\"cap.a\", \"urn:ggen:pack:mut1\"]",
        ),
    )
    .expect("rewrite pack.toml");
    let config = GgenConfig::load(&root.join("ggen.toml")).expect("load config");
    resolve(&config, &root).expect("URN require satisfied by declaration alone");
}
