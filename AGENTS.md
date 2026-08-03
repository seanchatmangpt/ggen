# ggen Repository Operating Contract

This file governs the repository unless a deeper `AGENTS.md` narrows a subtree. A nested contract may add constraints, but it may not silently weaken repository safety, evidence, or publication rules.

## 1. Preserve before changing

1. Resolve the requested repository and base to an exact commit SHA.
2. Read this file, every applicable nested `AGENTS.md`, `CLAUDE.md`, relevant architecture documents, manifests, task runners, and verifier entrypoints.
3. Preserve public interfaces, generated/manual boundaries, receipts, replay semantics, typed refusals, and compatibility unless the task explicitly requires a change.
4. Prefer the smallest coherent repair of the existing path over a parallel implementation.
5. Do not hand-edit generated projections when an authoritative graph, template, schema, or generator owns them.

When a rule appears obsolete or contradictory, inspect why it existed before removing it. Replace stale doctrine with a narrower executable rule and record the falsifier.

## 2. Evidence vocabulary

Use these standings precisely:

- `UNKNOWN`: the relevant subject was not observed, or the evidence is stale or contradictory.
- `PARTIAL_ALIVE`: a bounded checkpoint executed successfully; the crown claim remains open.
- `ALIVE`: the exact admitted subject executed and produced the claimed consequence.
- `BLOCKED`: an admitted dependency or authority boundary prevents execution.
- `BUILD_BROKEN`: the requested verifier cannot be reached because the build path is broken.
- `UNSUPPORTED`: the capability is outside the admitted boundary.
- `REFUSED_<TYPE>`: a typed policy, safety, authority, or admission refusal.

Never promote inspection, source presence, a workflow definition, a connector object, or historical output to `ALIVE`. Track observed, admitted, executed, changed, verified, inferred, blocked, unsupported, and refused claims separately.

## 3. Manufacture and authority

The repository follows:

```text
A = μ(O*)
R = receipt(A)
```

`O*` is admitted, aligned, grounded, and bounded observation. `μ` is lawful manufacture. A receipt binds subject identity, authority, consequence, replay, and standing.

Separate operations into:

- `SELECT`: choose an admitted subject or route.
- `CONSTRUCT`: build reversible artifacts, plans, edits, graphs, or intents.
- `DO`: actuate machine state through the authorized boundary.

BRCE is the exclusive `DO` path where a BRCE boundary exists. Raw input, model output, generated code, proof text, hooks, and semantic derivations have no ambient execution authority. Hooks manufacture intents; they do not directly actuate.

## 4. Repository orientation

Before implementation, establish:

- repository, ref, exact base SHA, tree identity, branch, and PR state;
- available transports, credentials, mounts, Git implementation, network, archives, runtimes, compilers, package caches, and test tools;
- root and nested doctrine;
- workspace members, feature flags, generated surfaces, dependency policy, and release policy;
- the exact acceptance command or the narrowest documented equivalent.

Use this materialization ladder until one path succeeds or each failure is typed:

1. verified local checkout;
2. exact-SHA source archive;
3. clone or fetch;
4. bundle or workflow artifact supplied by the user;
5. connector-backed tree/blob reconstruction;
6. dependency-closed sparse tree;
7. classified remote execution explicitly permitted by the user.

A connector-visible repository is not a mounted tree. Record transport failures without collapsing the whole task into failure.

## 5. Implementation law

Follow the live path:

```text
parse → route → admit/refuse → diagnose/repair → construct → actuate → receipt → replay → standing
```

Requirements:

- Keep diffs bounded and cohesive. Default to no more than 12 files unless closure requires more.
- Preserve deterministic behavior, portability, failure transparency, receipt/replay identity, and existing authority boundaries.
- Do not fabricate evidence, weaken tests, replace requested integration proof with unit proof, or make unrelated refactors.
- Test doubles may isolate deterministic internal logic, but they cannot prove an external process, filesystem, network, compiler, editor, protocol, database, or service boundary.
- Unit tests are valid for pure functions and local invariants. Integration and end-to-end claims require the real boundary named by the claim.
- Do not add unresolved placeholders to a changed production path. Existing unrelated debt is not automatically in scope.
- Prefer typed errors or refusals over silent fallback.
- Treat one failed edge as topology information, not proof that every route failed.

## 6. Verification ladder

Run the cheapest high-information verifier first, then expand only after success:

1. format or syntax check;
2. narrow package check;
3. focused unit tests;
4. package test suite;
5. integration or protocol tests;
6. end-to-end execution;
7. workspace or release gates when materially affected.

On failure:

1. preserve the exact command, exit code, and diagnostic;
2. locate the failed transition;
3. form a new hypothesis;
4. repair the narrowest cause;
5. encode a permanent guard, test, refusal, fixture, schema, or theorem;
6. rerun the failed boundary before expanding.

Do not rerun an unchanged failure without a new hypothesis. Do not use hosted CI as a substitute for available local validation.

## 7. Generated artifacts

A generated artifact is a projection, not automatically the editing authority.

- Find the owning ontology, query, template, generator, or schema.
- Regenerate through the documented command when the generator is in scope and executable.
- Preserve merge markers and generated/manual regions.
- Report generated artifacts as changed, unchanged, skipped, blocked, or unsupported.
- Never claim source/generated equivalence without executing the relevant generator or byte-identity verifier.

## 8. GitHub publication

Unless the user explicitly asks otherwise:

- branch from the exact admitted base;
- use a purpose-specific branch name;
- create intentional commits without force-pushing;
- open a draft pull request;
- do not merge;
- do not silently move the base;
- inspect the exact published head and compare it with the admitted base.

GitHub metadata supplements execution evidence. A green status label is not a log, and a workflow definition is not a successful run.

## 9. Final receipt

Every implementation result must expose:

- repository, exact base, branch, commit, and PR identity;
- admitted scope and exclusions;
- materialization transports attempted and typed failures;
- files and behavior changed;
- generated-artifact status;
- commands executed with exits;
- verification ladder reached;
- standing for each material claim;
- replay command or next exact verifier;
- unresolved falsifiers or blockers.

The receipt must make it possible for another operator to reproduce the standing without trusting prose.
