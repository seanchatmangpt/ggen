# github-actions-combinatorial-pack

**Status:** v0.2 cloud calculus
**Canonical source:** `ontology.ttl`
**Projection rule:** graph → SPARQL admission → bounded generated workflow/docs/lock
**Invariant:** maximize lawful reversible possibilities; actuate only the admitted dependency-closed slice.

This pack is the GitHub CI/Actions counterpart to Design for Combinatorial Maximalism. It does
**not** attempt to hard-code a finite list of every Marketplace action. That would become stale
immediately and would confuse discovery with execution authority. Instead it defines an
open-world capability registry: arbitrary Marketplace listings, reusable workflows, events,
runners, permissions, connector operations, artifacts, and execution routes can be added as RDF
individuals without changing the calculus.

An action is executable only after the selected individual closes:

`source identity → publisher policy → exact 40-hex commit SHA → capability → permission → network → side effect → authority → receipt`

## GitHub cloud topology

The canonical ChatGPT-cloud precedence is:

`GitHub Connector → GitHub Actions → Artifacts → local verification capsule → CLI fallback`

The connector is the control plane over GitHub's remote object graph. Actions is the remote
execution plane. Artifacts are first-class transport. The local runtime is the replay/verification
capsule. `git`, `gh`, SSH, Docker, package managers, and direct internet are optional accelerators.

A missing CLI is therefore a failed route, not a failed task.

## Named operators

`gh-fix-ci` is modeled as an evidence-first state machine:

`exact PR head → checks → jobs → logs → first causal error → diff relevance → minimum repair → verify → publish → exact-head re-observation → repeat`

`yeet` is modeled as a scope-first publication state machine:

`scope freeze → idempotency → purpose branch → intentional commit → verify → remote publication → draft PR → re-observe identities`

Transport substitution is lawful. Connector-native blob/tree/commit/ref manufacture can satisfy
the publication semantics directly. Neither operator implies merge.

## What the pack preserves

The canonical graph includes:

- current GitHub workflow event families, with an open extension mechanism;
- GitHub-hosted, larger, self-hosted, ARC, architecture, GPU, networking, and autoscaling runner capabilities;
- current `GITHUB_TOKEN` permission-scope vocabulary, including artifact metadata, attestations, code quality, models, and vulnerability alerts;
- workflow DAG, matrices, concurrency, reusable workflows, environments, caches, artifacts, OIDC, attestations, release/package/security composition;
- an open Marketplace/action capability vocabulary spanning checkout, toolchains, build/test/lint, artifacts, containers, publishing, cloud auth, deployment, security, IaC, PR/issue automation, benchmarking, fuzzing, chaos, receipts, and model inference;
- connector control-plane reads and Git-object/PR/Actions mutations;
- source-materialization, cross-private dependency, artifact, and execution-route ladders;
- independent authority principals for ChatGPT connector, Actions `GITHUB_TOKEN`, GitHub App installation tokens, OIDC, and human escalation;
- exact-head standing and typed CI failure classes;
- BRCE-shaped zero-unreceipted-actuation constraints.

## What it generates

A clean consumer sync generates three bounded products:

1. `.github/workflows/crown-ci.yml` — one exact-head repository-native verifier, not a fan-out of every preserved capability.
2. `.github/ggen/marketplace-lock.yml` — machine-readable selected action identities/capabilities pinned to exact SHAs.
3. `docs/github-actions/cloud-operating-doctrine.md` — a query-derived cloud operating map for GPT-5.7-class engineering sessions and humans.

The graph may contain thousands of lawful possibilities while the selected workflow remains small.
That is the point: **maximum optionality before selection, minimum WIP after admission**.

## Refusal gates

- `010_admission.rq` — workflow/job plans must be identified, bounded, dependency-closed, and renderable.
- `020_supply_chain.rq` — selected Marketplace/reusable dependencies must close immutable identity, provenance, capability, permissions, and network requirements.
- `030_authority_receipt.rq` — privileged triggers and consequential steps require explicit actuation authority and receipt closure.
- `040_topology.rq` — refuses false `BLOCKED`, untyped failed routes, premature low-precedence route selection, and cross-private authority conflation.

## Cross-private dependency law

A repository-scoped Actions `GITHUB_TOKEN` failing to clone another private repository proves only
that one principal/route failed. The dependency graph remains open through immutable packages,
retained artifacts, dependency-side manufacture, existing GitHub App strategies, same-repository
materialization, reusable build products, lawful exact snapshots, and finally minimal external
authority.

Prefer:

`source → build once → verify once → immutable artifact + digest + receipt → many consumers`

over repeated credential-coupled private clones.

## Verification

The Chicago-TDD e2e test uses the real filesystem, RDF/SPARQL gate execution, Tera rendering, and
`ggen_engine::sync`. It proves deterministic generation, immutable selected action refs, exact-head
receipt emission, machine-readable Marketplace locking, cloud-doctrine projection, and sabotage
refusals for supply-chain, authority, receipt, and false-blocker defects.

The generated workflow definition itself is **not** runtime standing. `ALIVE` requires execution on
the exact admitted published head and exact-head Actions evidence.
