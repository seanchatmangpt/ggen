# Pack URN Namespace Proposal — Duplicate Capability Collision

Measured 2026-10-09. Companion work orders:
`ggen_igniter/docs/sjira/v26.10.10/SJIRA-261010-12-pack-urn-dedup-design.md`,
`SJIRA-261010-13-pack-corpus-dedup-execution.md`.

## Measured collision

`ls` intersection of `/Users/sac/ggen/packs/` (97) and
`/Users/sac/ggen-marketplace/packs/` (308): **71 colliding names**, not 54.
All emit the same `urn:ggen:pack:<name>` provides URN. Classification
(`diff -rq` per pair; version from `[pack]` in `pack.toml`):

| class | count | note |
|---|---|---|
| identical | 9 | byte-identical trees (fortune5-architecture, fortune5-deployment-blocks, gall-core, ggen-constitution, gh-enterprise-architecture, gh-terraform, mermaid, temporary-works, wasm4pm-compat) |
| marketplace newer | 22 | version bump in marketplace copy; includes repo-as-found/repo-intervention/repo-load-path/repo-reconciliation (26.7.20 → 26.9.30) |
| diverged | 40 | same or absent version, differing content — untracked drift |

Zero "ggen newer" pairs. Every version-differentiated pair favors marketplace.

## Live-consumed corpus (decisive measurement)

Grepped every consumer `ggen.toml` on disk (`/Users/sac/*/ggen.toml`, 75 files):

- `ggen-marketplace/packs` path references: **45** (27 relative + 18 absolute)
- `/Users/sac/mfact/packs` (own local packs): 3
- `ggen/packs` (the ggen repo's copy): **0**

`/Users/sac/ggen/ggen.toml` itself contains no `ggen/packs` reference.
`ggen/packs` is not live-consumed by any project's sync; the marketplace
corpus is the only one real projects resolve packs from.

## Options considered

**a. Repo-scoped URNs** (`urn:ggen:pack:ggen:<name>` vs `...:marketplace:<name>`).
Breaks every existing `requires` URN across ~90 packs and all consumers; two
identity URNs for one logical capability; drift becomes legal instead of
refused. Rejected.

**b. Precedence rule** (composer prefers marketplace; ggen/packs deprecated
as mirrors). Zero-code option available immediately as a documented composer
policy; matches the live-consumed evidence. But leaves the duplicated corpus
as standing drift: the 40 diverged pairs remain silently non-identical.

**c. Deduplicate the corpora** (delete the 71 copies from `ggen/packs`,
redirect any in-repo references to the marketplace corpus). Non-destructive
at the consumer level (zero `ggen/packs` consumers measured); destructive
inside the ggen repo, so gated on user approval — see
SJIRA-261010-13.

**d. Version-qualified URNs** (`urn:ggen:pack:<name>:<version>`). Solves the
wrong problem: the collision is same-URN/different-content, so qualifying by
version makes the divergence resolvable-but-permanent and breaks requires
semantics (a requires URN must keep resolving across minor bumps). Rejected.

## Recommendation: c, with b as the interim gate behavior

Evidence: marketplace is the sole live corpus (45:0 path references); every
versioned pair favors marketplace (22:0); the 9 identical pairs prove the
mirror was once in sync and has since drifted. Keep the composer's
DuplicateCapability refusal strict (no precedence softening — b alone would
legalize the drift); land the dedup so there is exactly one corpus per name.
If approval is delayed, adopt b as a temporary, explicitly-expiring composer
precedence so cross-repo composition is not blocked.

## FALSIFIED (2026-10-09, census re-run) — premise reversed, option c dead

The live-consumed census was re-run and the direction **reversed**:
consumer `ggen.toml` references resolve **64 → `/Users/sac/ggen/packs`** vs
**1 → `ggen-marketplace/packs`** (previous measurement: 45 marketplace vs 0
ggen). Falsifying lines:

- `examples/cs2-projections/ggen.toml:10` — 18 refs
  `../../packs/multi-projection-pack/*` → ggen's copy, which exists on disk
  with `ontology/`, `queries/`, `templates/`.
- `examples/interview-assist/ggen.toml:8` — absolute path into
  `/Users/sac/ggen/packs`.
- `rust-dialect-pack`: zero refs (the one name with no live consumer either
  side).

**Consequence**: the ggen repo's own examples live-consume `ggen/packs`;
deleting the 71 copies (option c / SJIRA-261010-13) would break them.
Option (c) is DEAD. Both corpora are live in different contexts — ggen/packs:
repo self-hosting and examples; marketplace: distribution. The falsifier in
SJIRA-261010-12 FIRED; that is the system working.

### Options re-adjudicated

- **a. Repo-scoped URNs** — premise-independent objection stands (breaks all
  existing `requires` URNs), and cost is now measurable: ~297 edges would
  need splitting. Still rejected.
- **b. Precedence rule** — meaningless now: both corpora are live, so
  "prefer one" would break the other context. Rejected.
- **c. Dedup to marketplace** — dead, per falsification above.
- **d. Version-qualified URNs** — original rejection stands (wrong problem).
- **e. Mirror-collapse (NEW)**: composer treats cross-repo same-name pairs as
  ONE logical pack, keyed by name, preferring the consuming project's repo.
  No data changes, no URN changes. The `capability_corpus_test`'s
  332-unique-name injection already models this collapse. Evaluated
  seriously: the mirrors genuinely ARE the same pack logically — the 9
  byte-identical pairs prove shared identity; the 40 diverged pairs are
  drift within one logical pack, not two capabilities.

## Revised recommendation

**Defer.** Within-repo composition is correct today (each consumer resolves
inside its own repo; no DuplicateCapability refusal fires in any live
consumer). Cross-corpus composition — the only case where the duplicate URN
bites — has zero observed consumers. Mirror-collapse (e) is the right design
when needed, and is cheap to adopt later since it requires no data or URN
migration. Revisit when a real consumer composes across repos; until then
keep the composer's DuplicateCapability refusal strict as the tripwire that
would surface the first real cross-corpus collision.

## Closing consistency pass (verifier V18, 2026-10-09)

- "71 colliding names" (line 10) CONFIRMED: fresh count under both [pack].name-keyed and
  directory-keyed methods gives 332 unique / 71 mirror pairs (142 entries). The phase1 receipt's
  331/72 census note is the outlier, corrected there.
- Requires edge count for cost framing: live tree = 104 entries across 49 packs (74 distinct
  URNs), lower than any figure quoted in this proposal; 0 dangling, 0 bare — so the option-c
  breakage surface ("~297 edges", line 91) is stale and should be re-derived from 104 before any
  future decision on option c.
