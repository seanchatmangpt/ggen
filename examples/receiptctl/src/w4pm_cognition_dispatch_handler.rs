//! Hand-completable wasm4pm-cognition dispatch handler (generated ONCE by
//! `wasm4pm-cognition-pack`; `unless_exists: true` in this template's
//! frontmatter means `ggen sync` never overwrites this file again after
//! the first sync -- this is the freeze slot for the one hand-written seam
//! this pack requires).
//!
//! The regenerated per-breed wrapper functions in
//! `w4pm_cognition_dispatch.rs` all delegate to [`dispatch_cognition_run`]
//! below by name -- edit this file's body to wire it to a real
//! wasm4pm-backed runtime (e.g. bridging to `cognition_run(input_json)` in
//! a project that depends on wasm4pm). Regenerating the catalog/wrapper
//! files (e.g. after adding a breed to `ontology.ttl`) never touches or
//! reverts this file's body.

/// Single hand-completable dispatch helper: route `breed_id` + `input_json`
/// to the platform's `cognition_run` verb and return its JSON result.
///
/// Default body fails closed until the consumer wires it to a real
/// wasm4pm-backed runtime. Edit this function's body; it will not be
/// regenerated or reverted by a later `ggen sync`.
pub fn dispatch_cognition_run(breed_id: &str, input_json: &str) -> Result<String, String> {
    let _ = input_json;
    Err(format!(
        "dispatch_cognition_run not wired to a wasm4pm runtime yet (breed: {breed_id})"
    ))
}
