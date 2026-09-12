# `fortune5-deployment-blocks-pack` is not `ggen sync run`-portable; `ggen bblock` ignores `[packs]` wiring entirely

Filed per ticket 04's Plan step 5 / Acceptance: a hidden portability defect found during the
04 live-run re-verification, not fixed inline here.

## Finding

`fortune5-deployment-blocks-pack` cannot be exercised end-to-end from an unrelated scratch
consumer project by any real command, despite the sibling pack
(`fortune5-architecture-pack`) working correctly under the identical procedure. Two
independent, compounding defects:

1. **Ships zero `templates/*.tmpl`, so `ggen sync run` refuses it outright.**
   `~/ggen-marketplace/packs/fortune5-deployment-blocks-pack/templates/` does not exist.
   Wiring the pack into a scratch project's `ggen.toml` `[packs]` table (by `path`, pointing
   at the real `~/ggen-marketplace/packs/fortune5-deployment-blocks-pack`, exactly as done
   successfully for `fortune5-architecture-pack` in ticket 04) and running `ggen sync run
   --dry-run` fails with exit code 1:

   ```
   $ cd ~/f5deploy-scratch-home && docker run --rm --user "$(id -u):$(id -g)" \
       -v "$PWD:/workspace" -w /workspace \
       -v "/Users/sac/ggen-marketplace:/Users/sac/ggen-marketplace:ro" \
       ghcr.io/seanchatmangpt/ggen-ecosystem:v26.8.28 ggen sync run --dry-run
   ...
   ERROR: CLI execution failed: Command execution failed: validation error: [FM-PACK-005]
   pack `fortune5-deployment-blocks`: zero templates under
   `/Users/sac/ggen-marketplace/packs/fortune5-deployment-blocks-pack/templates`.
   Remediation: a pack must ship at least one templates/*.tmpl.
   $ echo $?
   1
   ```

2. **`ggen bblock <verb>` — the pack's own documented consumption path (its `pack.toml`
   description says "The retained catalog consumed by `ggen bblock <verb>`") — does not
   read the `[packs]`-wired pack at all.** It returns the identical JSON catalog
   (`catalog_digest: "299e75f39c8cb74c941f5a185a315ab131e2bae328a2c063df552cc98bb17c5f"`,
   same 15 atomic + 4 composite groups, same descriptions) whether or not any pack is wired
   in `[packs]`:

   ```
   # scratch project WITH fortune5-deployment-blocks-pack wired by path in [packs]:
   $ ggen bblock list   # (same docker invocation as above)
   ... "catalog_digest": "299e75f39c8cb74c941f5a185a315ab131e2bae328a2c063df552cc98bb17c5f"

   # separate scratch project with a bare ggen.toml and NO [packs] table at all:
   $ cat ~/nopacks-check-home/ggen.toml
   [project]
   name = "nopacks-check"
   [ontology]
   source = "ontology.ttl"
   [templates]
   dir = "templates"
   $ ggen bblock list
   ... "catalog_digest": "299e75f39c8cb74c941f5a185a315ab131e2bae328a2c063df552cc98bb17c5f"
   ```

   Identical digest, identical content, with the pack completely absent from the second
   project's manifest. `ggen bblock list/group/validate/inspect/providers` (per `--help`)
   accept no `--pack`, `--path`, or project-selector flag that could explain this any other
   way. The only explanation consistent with the evidence is that `ggen bblock` reads a
   catalog baked into the `ggen` binary itself (a self-pack / embedded fixture), not
   anything resolved through a consuming project's `ggen.toml` `[packs]` wiring.

## Why this matters

The pack's only two possible consumption paths are `ggen sync run` (refused, defect 1) and
`ggen bblock <verb>` (silently ignores the wiring and serves embedded data instead of the
wired pack's `catalog/fortune5-bblocks.json`, defect 2). There is no real command an
unrelated consumer project can run that exercises this pack's own on-disk content at all.
This is a stronger finding than "unverified" — it is verified non-portable: the pack's
documented consumption mechanism does not consume `[packs]`-wired packs in the first place,
so no amount of consumer-side `ggen.toml`/ontology authoring can make it work today.

This differs in kind from the `fortune5-required-capabilities-pack` /
`fortune5-testing-bblock-pack` defects (hardcoded paths/repo assumptions inside otherwise
sync-portable packs) — here the pack's entire delivery mechanism (`ggen bblock`) has no
project-wiring hook at all, and its `ggen sync run` path is blocked before that question
even arises.

## Evidence trail (commands + exit codes)

- `~/ggen-marketplace/packs/fortune5-deployment-blocks-pack/templates/` — confirmed absent
  (`ls: ... No such file or directory`).
- `ggen sync run --dry-run` against `~/f5deploy-scratch-home` (fresh scratch project, own
  minimal `ontology.ttl`, `[packs] fortune5-deployment-blocks = { path =
  "/Users/sac/ggen-marketplace/packs/fortune5-deployment-blocks-pack" }`) → exit 1,
  `[FM-PACK-005]`.
- `ggen bblock list` against that same scratch project → exit 0, full catalog JSON,
  `catalog_digest: 299e75f39c8cb74c941f5a185a315ab131e2bae328a2c063df552cc98bb17c5f`.
- `ggen bblock list` against a second scratch project (`~/nopacks-check-home`) with an empty
  `ggen.toml` `[packs]` table (i.e. no pack wired at all) → exit 0, byte-identical catalog
  JSON, identical digest.
- Contrast: `fortune5-architecture-pack`, run through the identical scratch-project
  procedure in ticket 04, generated correctly (exit 0, 4/4 named artifacts, real content
  traceable to the scratch ontology's own admitted facts) — proving the scratch-project
  procedure itself is sound and the deployment-blocks-pack failure is the pack's own defect,
  not a setup error.

## Scope

Not fixed here, per ticket 04's own instruction (file, don't fix inline). Candidate fixes for
whoever picks this up (not prescriptive, needs its own Define/Measure pass):

- Give the pack at least one real `templates/*.tmpl` (e.g. a `deployment_catalog.md.tmpl`
  projecting `bb:Catalog`/`bb:Provider`/`bb:BlockGroup` the same way
  `fortune5-architecture-pack`'s templates project its classes) so `ggen sync run` stops
  refusing it — this alone does not fix defect 2.
- Separately, `ggen bblock <verb>`'s catalog-resolution code path needs to actually consult
  the current project's `[packs]`-resolved `fortune5-deployment-blocks-pack` (via its
  `catalog/fortune5-bblocks.json` and/or `ontology.ttl` instance data) instead of an
  embedded fixture — this is a `ggen-cli`/`ggen-engine` behavior question, likely out of
  this marketplace repo's own scope (candidate for a `~/ggen` core ticket, not a
  `ggen-marketplace` pack-content fix).

## See Also

- `04-ARCHITECTURE-DEPLOYMENT-BLOCKS-REVERIFICATION.md` — the parent re-verification ticket
  this sub-ticket was filed under; see its closing notes for the full run log of the
  companion `fortune5-architecture-pack` verification (portable, live-verified) alongside
  this pack's finding (not portable, live-verified as broken).
- `02-FORTUNE5-REQUIRED-CAPABILITIES-PORTABILITY.md`,
  `03-FORTUNE5-TESTING-BBLOCK-PORTABILITY.md` — the sibling non-portable-pack tickets this
  finding is adjacent to but distinct in failure class from (see "Why this matters" above).
