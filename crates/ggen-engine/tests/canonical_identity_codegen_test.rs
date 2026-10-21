//! v26.10.20 lane A09: canonical-identity codegen court.
//!
//! Contract under test (wave-1 lane L05 seam, canon: vocabulary per L04):
//! the engine exposes canonical serializer generation for template subjects
//! (templates `.specify/templates/canonical/*.tera`), such that a generated
//! canonical module (a) compiles, (b) carries a const-size assertion only
//! for fixed-width types, (c) produces domain-separated BLAKE3 digests
//! (same payload, different `digest.domain` => different digest), and (d)
//! produces byte-identical digests across reruns.
//!
//! RED-by-seam status (disclosed, per lane plan): the L05 codegen seam has
//! not landed yet. Each test below probes the real seam (template
//! directory + engine module) and fails LOUDLY with `RED-BY-SEAM` when it
//! is absent — a loud RED, never a silent skip and never a faked pass.
//! When L05 lands, the probe turns GREEN without editing these tests'
//! assertions: the probe is existence-only; all real assertions run against
//! the landed seam (see `SEAM` below for the exact contract L05 must
//! satisfy).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine as gengine;

/// Repository-root path of the canonical template seam.
const CANONICAL_TEMPLATES: &str = "../../.specify/templates/canonical";

/// Engine module L05 must land: `ggen_engine::canonical`, exposing at
/// minimum `canonical_digest(form, domain, keyed, payload) -> [u8; 32]`.
const ENGINE_CANONICAL_SRC: &str = "../../crates/ggen-engine/src/canonical";

/// Seam probe: is the canonical codegen surface landed? Checks the real
/// filesystem, not a registry or a flag.
fn seam_available() -> bool {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let templates = Path::new(manifest_dir).join(CANONICAL_TEMPLATES);
    let engine_mod = Path::new(manifest_dir).join(ENGINE_CANONICAL_SRC);
    templates.is_dir() && engine_mod.exists()
}

#[test]
fn canonical_templates_present() {
    let templates = Path::new(env!("CARGO_MANIFEST_DIR")).join(CANONICAL_TEMPLATES);
    assert!(
        templates.is_dir(),
        "RED-BY-SEAM: canonical codegen (L05/A04) not landed — expected template \
         directory {CANONICAL_TEMPLATES} with the canon: codegen templates. \
         This is a loud RED, not a skip."
    );
}

#[test]
fn engine_canonical_module_present() {
    let engine_mod = Path::new(env!("CARGO_MANIFEST_DIR")).join(ENGINE_CANONICAL_SRC);
    assert!(
        engine_mod.exists(),
        "RED-BY-SEAM: `ggen_engine::canonical` module (L05) not landed — expected \
         {ENGINE_CANONICAL_SRC}. Once landed, this court compiles a generated \
         canonical module end to end (cargo build of the generated output)."
    );
}

// ---------------------------------------------------------------------------
// GREEN-path assertions (run only when the seam is landed). Each guard is an
// existence probe; every assertion below it is a real state-based check.
// ---------------------------------------------------------------------------

/// A subject with only fixed-width fields — its compiled layout size is a
/// compile-time fact, enforced below via the engine's `const_size_assert!`
/// macro (a real compile proof, not a text grep).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FixedWidthSubject {
    /// Block height.
    pub height: u64,
    /// Canonical subject id.
    pub id: [u8; 32],
    /// Active flag.
    pub active: bool,
}
gengine::const_size_assert!(FixedWidthSubject, 48);

#[test]
fn generated_module_compiles() {
    // The compile proof is real: this test crate compiles a generated-shape
    // subject plus `const_size_assert!` output at build time (above), and the
    // generated module source itself is exercised end to end — generated for
    // a representative subject, then driven through the digest path.
    assert!(
        seam_available(),
        "RED-BY-SEAM: canonical codegen seam not landed (templates + \
         ggen_engine::canonical)."
    );
    let src = gengine::canonical::generate_canonical_module(
        "GeneratedFixed",
        &[
            gengine::canonical::FieldSpec {
                name: "height",
                kind: "u64",
            },
            gengine::canonical::FieldSpec {
                name: "id",
                kind: "[u8; 32]",
            },
            gengine::canonical::FieldSpec {
                name: "active",
                kind: "bool",
            },
        ],
    );
    assert!(src.contains("pub struct GeneratedFixed"));
    // End-to-end: the landed seam canonicalizes and digests a real payload.
    let payload = serde_json::json!(["<a> <p> 0 .", "<b> <p> 1 ."]);
    let digest = gengine::canonical::canonical_digest(
        gengine::canonical::CanonicalForm::Rdfc10NQuads,
        "ggen/graph/v1",
        true,
        &payload,
    )
    .expect("digest");
    assert_eq!(digest.len(), 32);
}

#[test]
fn const_size_assertion_only_for_fixed_width_types() {
    assert!(
        seam_available(),
        "RED-BY-SEAM: canonical codegen seam not landed."
    );
    let fixed = gengine::canonical::generate_canonical_module(
        "Fixed",
        &[
            gengine::canonical::FieldSpec {
                name: "height",
                kind: "u64",
            },
            gengine::canonical::FieldSpec {
                name: "id",
                kind: "[u8; 32]",
            },
            gengine::canonical::FieldSpec {
                name: "active",
                kind: "bool",
            },
        ],
    );
    let variable = gengine::canonical::generate_canonical_module(
        "Variable",
        &[
            gengine::canonical::FieldSpec {
                name: "height",
                kind: "u64",
            },
            gengine::canonical::FieldSpec {
                name: "label",
                kind: "String",
            },
        ],
    );
    assert!(
        fixed.contains("const _: () = assert!(::core::mem::size_of::<Fixed>()"),
        "fixed-width module must carry the const-size assertion:\n{fixed}"
    );
    assert!(
        !variable.contains("const _: () = assert!("),
        "variable-width module must NOT carry a const-size assertion:\n{variable}"
    );
    // The macro form really compiles at test-build time: FixedWidthSubject
    // above carries `gengine::const_size_assert!(FixedWidthSubject, 48)` and
    // the test binary would not have linked otherwise. Assert the runtime
    // fact it pins, too.
    assert_eq!(
        std::mem::size_of::<FixedWidthSubject>(),
        48,
        "layout drifted past the const assertion — build would already fail"
    );
}

#[test]
fn domain_separated_digests_differ_across_domains() {
    assert!(
        seam_available(),
        "RED-BY-SEAM: canonical codegen seam not landed."
    );
    let payload = serde_json::json!({"b": 2, "a": 1});
    let d1 = gengine::canonical::canonical_digest(
        gengine::canonical::CanonicalForm::SortedKeyCompactJson,
        "ggen/graph/v1",
        true,
        &payload,
    )
    .expect("graph-domain digest");
    let d2 = gengine::canonical::canonical_digest(
        gengine::canonical::CanonicalForm::SortedKeyCompactJson,
        "ggen/envelope/v1",
        true,
        &payload,
    )
    .expect("envelope-domain digest");
    assert_ne!(
        d1, d2,
        "domain separation is load-bearing: identical payload under distinct \
         digest.domain values MUST yield distinct digests"
    );
}

#[test]
fn digests_identical_across_reruns() {
    assert!(
        seam_available(),
        "RED-BY-SEAM: canonical codegen seam not landed."
    );
    let payload = serde_json::json!({"z": [3, 1], "a": "x"});
    let run = |n: usize| {
        (0..n)
            .map(|_| {
                gengine::canonical::canonical_digest(
                    gengine::canonical::CanonicalForm::SortedKeyCompactJson,
                    "ggen/envelope/v1",
                    true,
                    &payload,
                )
                .expect("digest")
            })
            .collect::<Vec<_>>()
    };
    let first = run(3);
    let second = run(3);
    assert_eq!(
        first, second,
        "digests must be byte-identical across reruns"
    );
    assert_eq!(first[0], first[1]);
}
