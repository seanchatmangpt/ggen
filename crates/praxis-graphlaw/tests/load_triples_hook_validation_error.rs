//! Regression coverage for finding #7: `TripleStore::load_triples` used to
//! silently discard the `Err` returned by `hooks::validate_and_extract_hooks`
//! / `hooks::compile_hooks` when the newly-loaded document's own triples
//! included a knowledge-hook declaration the hook compiler rejects (e.g. a
//! `kh:on` value other than assert/retract/any, or more than 12 declared
//! hooks). The old code discarded both inner `Err`s via `if let Ok(...)` and
//! always returned `Ok(())`, leaving `self.hooks` holding whatever it held
//! before the call while the caller believed the load -- hooks included --
//! had succeeded.
//!
//! `load_triples` now propagates those inner `Err`s as its own `Err` (see
//! `crates/praxis-graphlaw/src/lib.rs`'s `load_triples`), so a document that
//! adds a hook the compiler rejects fails the load instead of silently
//! leaving the prior hook set in place with a false `Ok`.

mod common;

use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

const KH_PREFIX: &str = "@prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .\n\
                          @prefix ex: <http://example.org/> .\n";

/// A single valid hook, used to seed `self.hooks` with a known-good baseline
/// before we attempt to load a document that will fail hook validation.
fn valid_hook_ttl(name: &str) -> String {
    format!(
        "{KH_PREFIX}\nex:{name} a kh:Hook ;\n    kh:name \"{name}\" ;\n    kh:kind \"delta\" ;\n    kh:var \"x\" ;\n    kh:on \"assert\" ;\n    kh:effect \"emit-delta\" .\n"
    )
}

/// `kh:on` set to a value outside the closed assert/retract/any vocabulary
/// -- the exact failure mode named in finding #7 (a hook declaration whose
/// `kh:on` is a typo like "sometimes").
fn bad_on_hook_ttl(name: &str) -> String {
    format!(
        "{KH_PREFIX}\nex:{name} a kh:Hook ;\n    kh:name \"{name}\" ;\n    kh:kind \"delta\" ;\n    kh:var \"y\" ;\n    kh:on \"sometimes\" ;\n    kh:effect \"emit-delta\" .\n"
    )
}

/// Loading a document whose own hook declaration has an invalid `kh:on`
/// value must return `Err`, not the pre-fix `Ok(())`.
#[test]
fn load_triples_fails_when_new_hook_declaration_is_invalid() {
    let mut store = TripleStore::new();
    let res = store.load_triples(&bad_on_hook_ttl("bad_hook"), Syntax::Turtle);

    assert!(
        res.is_err(),
        "load_triples must fail when the loaded document declares a hook \
         with an invalid kh:on value, got Ok"
    );
    let err = res.unwrap_err();
    assert!(
        err.contains("hook:on must be assert, retract, or any"),
        "expected the real hook-validation error message, got: {err}"
    );
}

/// The triples ARE still added to the store even though the hook set failed
/// to validate/compile -- `load_triples` adds triples to `triple_index`
/// before attempting hook extraction. This documents the actual (not
/// aspirational) behavior: the caller learns the load failed via `Err` and
/// can decide what to do, rather than being told `Ok` while hooks silently
/// went stale.
#[test]
fn load_triples_still_adds_triples_when_hook_validation_fails() {
    let mut store = TripleStore::new();
    let res = store.load_triples(&bad_on_hook_ttl("bad_hook"), Syntax::Turtle);
    assert!(res.is_err());

    common::assert_contains_triple(
        &store,
        "http://example.org/bad_hook",
        "http://seanchatmangpt.github.io/praxis/kh#name",
        "bad_hook",
    );
}

/// Core regression for finding #7: seed `self.hooks` with one valid,
/// already-installed hook via `load_hook_pack`, then attempt to load a
/// second document whose hook declaration the compiler rejects via
/// `load_triples`. Before the fix, this returned `Ok(())` while silently
/// leaving `self.hooks` unchanged -- a caller had no way to distinguish
/// "the new hook installed" from "the new hook was silently dropped". Now
/// the caller gets `Err`, so it is not fooled into believing installation
/// succeeded, and can inspect/reload/refuse to proceed. `self.hooks` itself
/// is still left holding the prior, known-good hook (not wiped to empty,
/// not silently replaced by a partially-invalid set).
#[test]
fn load_triples_returns_err_and_keeps_prior_hooks_when_new_hook_is_invalid() {
    let mut store = TripleStore::new();
    store
        .load_hook_pack(valid_hook_ttl("keep_me").as_str())
        .expect("seed hook must install cleanly");
    assert_eq!(store.hooks.len(), 1, "seed hook must be installed");
    assert_eq!(store.hooks[0].name, "keep_me");

    let res = store.load_triples(&bad_on_hook_ttl("bad_hook"), Syntax::Turtle);

    assert!(
        res.is_err(),
        "load_triples must surface the hook-compilation failure instead of \
         returning Ok(()) while leaving self.hooks stale"
    );
    assert_eq!(
        store.hooks.len(),
        1,
        "self.hooks must still hold the prior, valid hook set -- not wiped, \
         not silently overwritten with a partially-invalid one"
    );
    assert_eq!(store.hooks[0].name, "keep_me");
}

/// The other realistic `validate_and_extract_hooks` failure named in finding
/// #7: more than 12 hooks declared in one document.
#[test]
fn load_triples_fails_when_more_than_twelve_hooks_declared() {
    let mut ttl = KH_PREFIX.to_string();
    for i in 0..13 {
        ttl.push_str(&format!(
            "ex:hook{i} a kh:Hook ;\n    kh:name \"hook{i}\" ;\n    kh:kind \"delta\" ;\n    kh:var \"x\" ;\n    kh:on \"assert\" ;\n    kh:effect \"emit-delta\" .\n"
        ));
    }

    let mut store = TripleStore::new();
    let res = store.load_triples(&ttl, Syntax::Turtle);

    assert!(
        res.is_err(),
        "load_triples must fail when the document declares more than 12 hooks, got Ok"
    );
    assert!(
        res.unwrap_err().contains("too many hooks declared"),
        "expected the real too-many-hooks error message"
    );
}

/// Sanity control: a document with a *valid* hook declaration continues to
/// install into `self.hooks` via `load_triples` exactly as before -- this
/// fix must not turn a previously-successful load into a failure.
#[test]
fn load_triples_still_installs_a_valid_hook() {
    let mut store = TripleStore::new();
    store
        .load_triples(&valid_hook_ttl("good_hook"), Syntax::Turtle)
        .expect("a document with a valid hook declaration must still load via load_triples");
    assert_eq!(store.hooks.len(), 1);
    assert_eq!(store.hooks[0].name, "good_hook");
}
