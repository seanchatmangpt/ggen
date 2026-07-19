# Target verification (mfact-pack)

Dated hand-verification note, closing the Target-API-fidelity L1->L2 gap
named in `docs/packs/L5_VALIDATION_REPORT.md`'s mfact-pack section ("there
is not even an L2-level 'verified by hand against one target version,
once'").

- **Verified on:** 2026-07-18
- **Target repo:** `~/mfact` (real, in-the-wild checkout on this machine)
- **Target commit:** `801abf7933dabf5c95f9fb18ff21a7a8a1f6a564` (`git -C ~/mfact rev-parse HEAD`, committed 2026-07-16)
- **What was checked:** `~/mfact/README.md`'s "Quickstart" and "What's here"
  sections against every fact this pack's `ontology.ttl` asserts:
  - The 3-stage pipeline (`ggen` projects, `Lean` admits, `mfact`
    certifies) matches the README's opening paragraph verbatim.
  - The 5 top-level authority directories (`packs/`, `procint/`, `paper/`,
    `release/`, `.mfact/artifacts.toml`) and their one-line descriptions
    match the README's "What's here" bullet list.
  - `~/mfact/packs/` on disk at this commit contains exactly
    `lean-math-pack`, `post-release-pack`, `quadrature-pack` — consistent
    with `mfa:Project`'s `rdfs:comment` ("mfact's own packs/ (lean-math-pack,
    quadrature-pack, post-release-pack)").

## What this does NOT close

This is the L2 bar only ("verified by hand against one target version,
once, dated note"), not L3+. mfact/procint does not expose its own
machine-readable ontology this pack could directly track (no `.ttl`/RDF
export of its pipeline semantics under `~/mfact`) — reaching L3 ("generated
output compiled against the pinned target crate in the pack's own CI
proof") is not attempted here since there is no Rust crate surface of
mfact's own to compile against (mfact is a Lean 4 project; this pack
generates a Rust *reference* catalog of its README, not bindings to any
mfact API). Reaching L5 ("Pack tracks the target's ontology, not its API")
requires mfact to publish such an ontology — an external, upstream change
this repo does not control, tracked as the honest blocker for this
dimension rather than worked around.
