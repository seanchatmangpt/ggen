# TICKET-028: Precondition/policy-target/authority-check projection

## Status

ALIVE

## Parent

EPIC: InterviewAssist v26.7.23

## Source

- RDF resources:
  - `packs/wasm4pm-interview-assist-pack/ontology/30-capabilities.ttl (dcterms:requires chains)`
  - `packs/wasm4pm-interview-assist-pack/ontology/50-policy.ttl (6 odrl:Set policies, 8 authority-action/*)`
- ARD components:
  - `ARD §3.15 Authority Broker`
- PRD requirements: (none)
- Acceptance-test steps: (none)
- Policies:
  - `policy/authority-broker-default`
  - `policy/practice-mode`
  - `policy/mock-interview-mode`
  - `policy/live-assistance-mode`
  - `policy/assessment-mode`
  - `policy/prohibited-mode`
- SHACL shapes: (none)

## Objective

Generate the precondition-check function (walking each capability's dcterms:requires chain) and the policy-check function (matching odrl:Permission/Prohibition against the active operating-mode policy set) that gate every capability dispatch before execution.

## Current state

UNKNOWN — no implementation exists yet.

## Target state

Exact scaffold/artifact described in Outputs exists, verified, and committed.

## Projection classification

- Template: 85%
- Domain data: admitted RDF input under packs/wasm4pm-interview-assist-pack/ontology/ — not counted as template or custom code
- Custom code: 15%
- Expected ratio: 85/15
- Custom-code justification: graph-walk (transitive dcterms:requires closure) and ODRL permission/prohibition matching are generic algorithms but require careful design of the traversal/matching logic itself, same classification basis as TICKET-023/025.

## Inputs

- TICKET-026 capability.ts
- TICKET-019 authority-state.ts
- queries/policy-permissions.rq, queries/capability-preconditions.rq (new)

## Outputs

- examples/interview-assist/lib/domain/preconditions.ts (checkPreconditions(capabilityId, state))
- examples/interview-assist/lib/domain/policy-check.ts (checkPolicy(capabilityId, activeMode))

Projected files are canonical source and must not be placed under `generated/`.

## Template responsibility

Generic transitive-closure precondition walker + generic ODRL permission/prohibition matcher, both reusable structural algorithms parameterized entirely by RDF-selected data.

## Domain-data responsibility

The actual precondition chains and policy permission/prohibition sets live in 30-capabilities.ttl and 50-policy.ttl.

## Custom-code boundary

NONE.

## Exclusions

- no capability-specific precondition literal (e.g. 'execute requires compile') hardcoded outside the dcterms:requires-driven traversal — this is the exact case flagged UNSUPPORTED-as-a-stretch in the prior TTL report, now consumed here honestly, not silently treated as certain

## Implementation steps

1. Query dcterms:requires chains per capability, compute transitive closure.
2. Query odrl:Permission/Prohibition sets per policy/* resource.
3. Generate checkPreconditions walking the closure against current state.
4. Generate checkPolicy matching capability against the active mode's odrl:Set.
5. Test against policy/prohibited-mode's 6 prohibited-action/* resources explicitly.

## Admission gates

- TICKET-026.
- TICKET-019.

## Acceptance criteria

- Given policy/prohibited-mode active, when checkPolicy is called for prohibited-action/hidden-overlay, then it returns denied.
- Given capability/runtime/execute's precondition (requires compile), when checkPreconditions is called without a prior compile, then it returns unmet.

## Negative tests

- Call checkPolicy for a permitted action under policy/practice-mode and confirm it returns allowed — the positive-path test proving the checker isn't fail-closed-always (a different, equally wrong failure mode).

## Verification ladder

- Unit: precondition-closure and policy-match unit tests against real RDF-derived data, both positive and negative cases
- Integration: generated file loads correctly alongside sibling generated files
- End-to-end: exercised once the full shell exists (workstream C completion), via `next build`
- Chaos: N/A with reason — deterministic generation, no runtime concurrency at build time
- Stress: N/A with reason — small, fixed template set, no stress profile
- Benchmark: N/A with reason — no perf target for build-time generation
- Verifier report: generated-file diff against expected shape + `next build`/`tsc` pass

## Receipts

- preconditions.ts/policy-check.ts hashes

## Dependencies

- TICKET-026
- TICKET-019

## Falsifier

If checkPolicy allows a prohibited-action/* capability under policy/prohibited-mode, this ticket is not complete — this is a safety-critical negative test, not optional.

## Handoff

TICKET-029 (timeout/result/refusal handling) and TICKET-035 (sandbox executor) call these checks before every dispatch.

## Definition of done

- both checkers generated and RDF-driven
- prohibited-mode denial test passes as a hard safety gate
- practice-mode allowance test passes

## Implementation notes (real evidence) — closes as ALIVE

- New queries `packs/wasm4pm-interview-assist-pack/queries/capability-preconditions.rq` (42 rows,
  direct `dcterms:requires` edges among `capability/*`) and
  `packs/wasm4pm-interview-assist-pack/queries/policy-permissions.rq` (odrl:Permission/Prohibition
  statements across all 6 `policy/*` `odrl:Set` resources) — both run via rdflib against
  `ontology/30-capabilities.ttl` and `ontology/50-policy.ttl`.
- Wrote `examples/interview-assist/lib/domain/preconditions.ts` (`DIRECT_REQUIRES` map +
  `transitiveRequires` graph-walk + `checkPreconditions`) and
  `examples/interview-assist/lib/domain/policy-check.ts` (`POLICY_STATEMENTS` + `checkPolicy`,
  prohibition-wins-over-permission, unspecified treated as fail-closed by callers), plus reusable
  templates `templates/028a_preconditions_ts.tmpl` and `templates/028b_policy_check_ts.tmpl`
  (Tera `group_by` over the query rows).
- **SAFETY-CRITICAL TEST — real output** (`node --experimental-strip-types
  __tests__/policy-check.test.mjs`), the ticket's non-negotiable falsifier:
  ```
  === SAFETY-CRITICAL TEST: policy/prohibited-mode denial ===
  checkPolicy("prohibited-action/hidden-overlay", "policy/prohibited-mode") -> denied
  checkPolicy("prohibited-action/screen-capture-evasion", "policy/prohibited-mode") -> denied
  checkPolicy("prohibited-action/monitoring-bypass", "policy/prohibited-mode") -> denied
  checkPolicy("prohibited-action/disguised-process", "policy/prohibited-mode") -> denied
  checkPolicy("prohibited-action/covert-audio-capture", "policy/prohibited-mode") -> denied
  checkPolicy("prohibited-action/misrepresent-unaided-work", "policy/prohibited-mode") -> denied
  PASS: all 6 prohibited-action/* resources denied under policy/prohibited-mode
  ```
- Positive-path test (proving not fail-closed-always), real output:
  `checkPolicy("authority-action/execute-code", "policy/practice-mode") -> allowed`.
- Precondition-closure test against real data (`run-complete-test-suite` requires both
  `run-visible-test` and `run-hidden-test`, transitively also `runtime/execute`,
  `runtime/compile`, `runtime/select-language`) — real output confirmed the exact chain named in
  the ticket's Objective, plus unmet/met cases for `runtime/execute` with and without `compile`
  satisfied (`__tests__/preconditions.test.mjs`, all PASS).
- No mocks; all data is the real admitted RDF, loaded via rdflib and consumed by the generated
  TS at real runtime through `node --experimental-strip-types` (Node v25.9.0 built-in TS type
  stripping, no compilation step, no test doubles).

## Update (real ggen sync run now exercised, evidence added)

Previously this ticket's TS output was produced by a hand-run rdflib script matching the
template's intended shape, not the real engine -- the corresponding `.tmpl` file was missing its
required leading `---` frontmatter delimiter (`FM-TPL-001`) and had never actually been rendered
by `ggen sync run`. Fixed (see TICKET-017's implementation notes for the full fix list). The real
engine now generates this file directly; re-verified via a real (non-dry) sync + a full-tree
byte-identical double-sync idempotency check.

## Update (cross-workstream wiring gap closed: workstream H adapters now call the real checkPolicy)

**The gap.** Workstream H (`examples/interview-assist/lib/adapters/*.ts`) was built before this
ticket's real `lib/domain/policy-check.ts` existed, against `lib/adapters/policy-check-stub.ts`'s
placeholder shape `checkPolicy({capability}) -> {allowed, reason}` -- a default-allow stub, by
design, documented as PENDING(TICKET-028). The real generated checker that landed here has a
different, RDF-authentic shape: `checkPolicy(action: string, activeMode: PolicyId):
PolicyDecision`, where `action` must be one of the 8 `authority-action/*` / 6
`prohibited-action/*` resources in 50-policy.ttl, not the ad hoc local capability strings
(`"compile_python"`, `"checksum_hash"`, ...) every adapter actually passes. Swapping the import
was never going to be the one-line change the stub's own docstring predicted; this update closes
that disclosed mismatch honestly rather than force-fitting it.

**What changed.** New file `examples/interview-assist/lib/adapters/policy-check-adapter.ts`: a
real, documented mapping from each adapter-local capability id to the `authority-action/*`
resource it represents (ontology-grounded for execute-code via 50-policy.ttl's own
`dcterms:requires <capability/runtime/execute>` annotation; categorical best-fit for `project`,
grounded in the odrl:Permission `rdfs:comment` text under `policy/live-assistance-mode` and
`policy/assessment-mode`), delegating the actual permit/deny decision to the real
`lib/domain/policy-check.ts::checkPolicy`. Four adapters now import from it instead of the stub:
`sandbox-executor.ts`, `monaco-adapter.ts` (both -> `authority-action/execute-code`),
`ollama-adapter.ts`, `accessibility-platform-adapter.ts` (both -> `authority-action/project`).
Each gained an optional `activeMode?: PolicyId` field (default `policy/practice-mode`, grounded
in that mode's own `rdfs:comment`: "All InterviewAssist capabilities may be enabled.").

**Two adapters deliberately NOT swapped.** `checksum-adapter.ts` (raw BLAKE3 hashing) and
`persistence-adapter.ts` (local event-log save/load) stay on the stub. Verified
(`grep -n dcterms:requires 30-capabilities.ttl`) neither adapter's capability is linked to any
`authority-action/*` resource, and separately verified
(`grep -c 'authority-action/retain\|authority-action/export' 50-policy.ttl` -> 2, both bare
`a schema:Action` declarations) that even the closest-sounding authority class (`retain`) never
appears in any of the 6 `odrl:permission`/`odrl:prohibition` statements -- it is ungoverned by
every admitted policy today, not merely by this wiring pass. Forcing a mapping would be Epistemic
Bypass (coding-agent-mistakes.md); both files carry an explicit code comment stating this and
pointing at `policy-check-adapter.ts`'s module doc.

**Real proof the wiring is load-bearing, not decorative.** Verified directly against the real
generated checker before wiring anything:
```
$ node --experimental-strip-types -e '
import { checkPolicy } from "./lib/domain/policy-check.ts";
console.log(checkPolicy("authority-action/execute-code", "policy/practice-mode"));
console.log(checkPolicy("authority-action/execute-code", "policy/authority-broker-default"));
'
allowed
denied
```
`policy/authority-broker-default` carries BOTH a permission and a prohibition on
`authority-action/execute-code` in the real admitted RDF (permission: "within the declared
sandbox"; prohibition: "code submission ... outside the declared sandbox") -- the real
`checkPolicy` resolves that conflict prohibition-wins, so it denies. Added a new test to
`tests/adapters/sandbox-executor.test.ts` asserting exactly this: calling `execute()` with
`activeMode: "policy/authority-broker-default"` returns `{kind: "policy_denied"}` and spawns no
subprocess (verified via a real `ps ax` scan for a unique marker string). Real output:
```
$ npx vitest run tests/adapters/
 Test Files  6 passed (6)
      Tests  25 passed (25)   # was 24 before this change; +1 is the new denial test
```
All 6 pre-existing adapter test files stayed green with zero behavior changes to their assertions
-- `policy/practice-mode` (the new default) permits `execute-code` unconditionally, so
`execute_python`/`execute_rust` subprocess tests, `buildMonacoConfig`, `buildAnnouncement`, and
the ollama self-play tests all still pass through the real checker exactly as before.

**Groundwork surfaced by this pass (not this ticket's scope, noted for the record):** building
`app/page.tsx` (workstream I groundwork, not tied to a numbered ticket) required a real end-to-end
POST to a sandbox-executor-backed API route and surfaced that
`app/api/sandbox/[capability]/route.ts` (pre-existing, TICKET-013) only validates a capability id
and echoes `{status:"accepted"}` -- it never calls a real executor despite its name implying
dispatch. A new `app/api/run/route.ts` was added alongside it (not a replacement) that does call
the real `sandbox-executor.ts`, now policy-checked as described above; reconciling or removing the
decorative `[capability]` route is out of this task's scope and is flagged here rather than
silently left implying it already does real dispatch.
