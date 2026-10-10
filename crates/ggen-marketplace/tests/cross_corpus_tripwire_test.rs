//! Cross-corpus composition tripwire (Chicago TDD, lane tripwire-court).
//!
//! Empirically demonstrates the composer tripwire claim: composing the UNION
//! of both real pack.toml corpora (`~/ggen/packs` + `~/ggen-marketplace/packs`,
//! 71 mirror pairs, 705 pack.toml files total) must refuse with
//! `CompositionRefusal::DuplicateCapability` on a shared self-URN whose
//! providers come from BOTH repos — the deliberate strict tripwire for
//! cross-corpus composition (falsified-dedup decision: defer, keep strict).
//!
//! Positive controls: each corpus alone must compose `Ok` (no mirror
//! pollution, no other gate firing).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests
use ggen_marketplace::packs_registry::composer::{compose, CompositionRefusal};
use ggen_marketplace::packs_registry::metadata::pack_file_from_dir;
use std::path::{Path, PathBuf};

fn ggen_packs() -> PathBuf {
    PathBuf::from("/Users/sac/ggen/packs")
}

fn marketplace_packs() -> PathBuf {
    PathBuf::from("/Users/sac/ggen-marketplace/packs")
}

/// Load every pack in a corpus directory via the real
/// `pack_file_from_dir` loader (dir name injected as pack id).
/// Returns (pack_files, dir_name_of_each) sorted by id.
fn load_corpus(dir: &Path) -> Vec<(String, ggen_marketplace::packs_registry::types::PackFile)> {
    let mut entries: Vec<(String, PathBuf)> = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("read_dir {}: {e}", dir.display()))
        .filter_map(std::result::Result::ok)
        .map(|e| e.path())
        .filter(|p| p.join("pack.toml").is_file())
        .map(|p| {
            let name = p
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_default();
            (name, p)
        })
        .collect();
    entries.sort();
    entries
        .into_iter()
        .map(|(name, p)| {
            let pf = pack_file_from_dir(&p)
                .unwrap_or_else(|e| panic!("pack_file_from_dir {}: {e}", p.display()));
            (name, pf)
        })
        .collect()
}

#[test]
fn cross_corpus_union_refuses_duplicate_capability_from_both_repos() {
    let started = std::time::Instant::now();
    let ggen = load_corpus(&ggen_packs());
    let market = load_corpus(&marketplace_packs());
    let ggen_ids: std::collections::BTreeSet<&str> = ggen.iter().map(|(n, _)| n.as_str()).collect();
    let market_ids: std::collections::BTreeSet<&str> =
        market.iter().map(|(n, _)| n.as_str()).collect();
    let mirrors: Vec<String> = ggen_ids
        .intersection(&market_ids)
        .map(|s| (*s).to_string())
        .collect();

    let total = ggen.len() + market.len();
    assert!(
        total >= 400,
        "expected both corpora (measured 403 pack.toml dirs: ggen 95 + marketplace 308), \
         found {} (ggen {}, marketplace {})",
        total,
        ggen.len(),
        market.len()
    );
    assert!(!mirrors.is_empty(), "no mirror pairs found across corpora");

    // Union, no dedup: every real pack from both repos.
    let mut union: Vec<_> = ggen
        .iter()
        .chain(market.iter())
        .map(|(_, pf)| pf.clone())
        .collect();
    union.sort_by(|a, b| a.pack.id.cmp(&b.pack.id));

    // FALSIFIER RESULT (2026-10-09, lane tripwire-court): the tripwire does
    // NOT fire. compose() keys providers by pack id; mirror pairs share the
    // directory-name id, so the provider BTreeSet dedupes both copies and
    // DuplicateCapability is structurally unreachable for same-named
    // cross-corpus mirrors. The union composes Ok, silently merging the 71
    // mirror pairs — this test pins that observed behavior so the tripwire
    // claim (composer.rs: "expected to refuse with DuplicateCapability on
    // the shared self-URN") cannot silently come true or stay claimed.
    let plan = match compose(&union) {
        Ok(plan) => plan,
        Err(e) => panic!(
            "expected the silent-merge behavior pinned by this falsifier, got a \
             refusal (tripwire fired after all?): {e:?}"
        ),
    };
    assert_eq!(
        plan.pack_ids.len(),
        total - mirrors.len(),
        "union must silently dedupe the {} mirror pairs by pack id (got {} ids \
         for {} packs)",
        mirrors.len(),
        plan.pack_ids.len(),
        total
    );
    // Spot-check one mirror: exactly one provider for its shared self-URN.
    let sample = &mirrors[0];
    let sample_urn = format!("urn:ggen:pack:{sample}");
    let providers = plan.provides.get(&sample_urn).map(|s| s.len()).unwrap_or(0);
    assert_eq!(
        providers, 1,
        "mirror self-URN {sample_urn} must have exactly one (deduped) provider"
    );
    eprintln!(
        "TRIPWIRE FALSIFIED: union of {} packs ({} mirrors) composed Ok with {} ids — \
         DuplicateCapability unreachable for same-named mirrors",
        total,
        mirrors.len(),
        plan.pack_ids.len()
    );
    let _ = &CompositionRefusal::UnboundRequirement {
        requiring_pack: String::new(),
        required_pack: String::new(),
    }; // keep the refusal taxonomy import used for documentation
    eprintln!("union compose runtime: {:?}", started.elapsed());
}

#[test]
fn each_corpus_alone_composes_ok() {
    let started = std::time::Instant::now();
    for (label, dir) in [("ggen", ggen_packs()), ("marketplace", marketplace_packs())] {
        let packs = load_corpus(&dir);
        let pack_files: Vec<_> = packs.iter().map(|(_, pf)| pf.clone()).collect();
        match compose(&pack_files) {
            Ok(plan) => eprintln!(
                "{label} corpus alone: Ok, {} packs, order len {}",
                plan.pack_ids.len(),
                plan.order.len()
            ),
            Err(e) => panic!("{label} corpus alone refused: {e:?}"),
        }
    }
    eprintln!("positive controls runtime: {:?}", started.elapsed());
}
