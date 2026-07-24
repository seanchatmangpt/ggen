/**
 * TICKET-028 wiring closure: real adapter-facing wrapper around the real,
 * RDF-driven `checkPolicy` (examples/interview-assist/lib/domain/policy-check.ts,
 * generated from packs/wasm4pm-interview-assist-pack/ontology/50-policy.ttl).
 *
 * Workstream H (this directory) was built against `policy-check-stub.ts`'s
 * `checkPolicy({capability}) -> {allowed, reason}` shape. The real generated
 * checker that landed from workstream F has a different, RDF-authentic
 * shape: `checkPolicy(action: string, activeMode: PolicyId): PolicyDecision`,
 * where `action` must be one of the 8 `authority-action/*` (or 6
 * `prohibited-action/*`) resources declared in 50-policy.ttl -- NOT the ad
 * hoc local capability strings adapters pass today (`"compile_python"`,
 * `"checksum_hash"`, etc). This file is the disclosed integration glue that
 * closes that gap: a real mapping from each adapter-local capability id to
 * the actual `authority-action/*` resource it represents (verified against
 * the ontology below, not invented), plus a normalization of
 * `PolicyDecision` back to the boolean shape every adapter call site
 * already expects.
 *
 * Mapping provenance (per coding-agent-mistakes.md's Epistemic Bypass rule
 * -- no capability-specific policy literal invented outside real RDF data):
 *
 * - sandbox capabilities (compile_python/execute_python/run_pytest/
 *   compile_rust/execute_rust/run_cargo_test) -> `authority-action/execute-code`.
 *   Ontology-grounded: 50-policy.ttl declares
 *   `<authority-action/execute-code> ... dcterms:requires <capability/runtime/execute>`
 *   and every one of these six local capabilities is a runtime compile/execute
 *   operation gated behind that exact capability.
 * - self-play roles (ollama-adapter) and accessibility announcements
 *   (accessibility-platform-adapter) -> `authority-action/project`. Weaker
 *   than the execute-code link above (no dcterms:requires chain connects
 *   them) -- grounded instead in 50-policy.ttl's own odrl:Permission
 *   `rdfs:comment` text for `project` under `policy/live-assistance-mode`
 *   ("An employer or interviewer explicitly permits InterviewAssist ... to
 *   project [assistance]") and `policy/assessment-mode` ("permitted
 *   references" the tool may surface): both self-play-generated content and
 *   accessibility announcements are the system presenting/surfacing
 *   assistance output to the candidate, which is what `project` denotes.
 *   This is a categorical best-fit, not an ontology-declared edge -- stated
 *   here plainly rather than overclaimed as equally certain to the
 *   execute-code mapping.
 * - checksum hashing (checksum-adapter) and local persistence
 *   (persistence-adapter) -> UNMAPPED, deliberately. Verified
 *   (`grep -n 'dcterms:requires' 30-capabilities.ttl`) that no capability
 *   these two adapters touch is linked to any `authority-action/*` resource,
 *   and separately verified
 *   (`grep -c 'authority-action/retain\|authority-action/export' 50-policy.ttl`
 *   -> 2, both bare `a schema:Action` declarations, zero appearances in any
 *   `odrl:permission`/`odrl:prohibition` statement across all 6 policy/*
 *   sets) that even the closest-sounding authority class (`retain`, for
 *   session-log retention) is currently ungoverned by every admitted
 *   policy. Routing these through a fabricated action id would make
 *   `checkPolicy` return "unspecified" unconditionally regardless of mode,
 *   which -- if fail-closed per policy-check.ts's own doc-comment
 *   recommendation -- would deny local persistence and hashing under every
 *   mode including the ones meant to allow everything. That is not a real
 *   safety property; it is an artifact of the ontology not yet declaring
 *   retention/hashing permissions. These two adapters remain on
 *   `policy-check-stub.ts`'s explicit default-allow placeholder untouched
 *   (see those two files) rather than force-fitting a mapping this file
 *   cannot back with real RDF data. Extending 50-policy.ttl with real
 *   retain/export permission statements is a follow-up ontology task, not
 *   integration wiring.
 */
import { checkPolicy as domainCheckPolicy, type PolicyId } from "../domain/policy-check";

export interface PolicyCheckResult {
  allowed: boolean;
  reason?: string;
}

/**
 * Default operating mode used when a call site does not yet thread a real
 * session-level `activeMode` through (no ticket in this backlog wires
 * session-mode selection into these adapters yet). Grounded in
 * 50-policy.ttl's own `rdfs:comment`/prose for `policy/practice-mode`:
 * "All InterviewAssist capabilities may be enabled." -- the one named mode
 * explicitly documented as least-restrictive, not an arbitrary pick.
 */
export const DEFAULT_ACTIVE_MODE: PolicyId = "policy/practice-mode";

const ACTION_FOR_CAPABILITY: Readonly<Record<string, string>> = {
  compile_python: "authority-action/execute-code",
  execute_python: "authority-action/execute-code",
  run_pytest: "authority-action/execute-code",
  compile_rust: "authority-action/execute-code",
  execute_rust: "authority-action/execute-code",
  run_cargo_test: "authority-action/execute-code",
  self_play_interviewer: "authority-action/project",
  self_play_candidate: "authority-action/project",
  "self_play_test-generator": "authority-action/project",
  self_play_critic: "authority-action/project",
  accessibility_announce_info: "authority-action/project",
  accessibility_announce_warning: "authority-action/project",
  accessibility_announce_refusal: "authority-action/project",
};

/**
 * Real, RDF-backed policy check for adapters whose local capability id maps
 * onto a real `authority-action/*` resource (see the module doc for the
 * exact provenance of each mapping). Delegates the actual permit/deny
 * decision to the generated `checkPolicy` -- this function performs no
 * allow/deny reasoning of its own beyond the capability->action lookup and
 * the disclosed "unspecified -> allowed" normalization below.
 *
 * `"unspecified"` (the active policy set never mentions the mapped action)
 * is normalized to `allowed`, not denied. An explicit `odrl:prohibition`
 * always wins and is normalized to `allowed: false` unconditionally --
 * fail-closed behavior on real prohibitions is preserved exactly as
 * `domainCheckPolicy` implements it (see TICKET-028's prohibited-mode
 * safety test). Only the *absence* of any statement is treated
 * permissively here, because most named modes in the current admitted RDF
 * only enumerate a single action (e.g. `policy/practice-mode` mentions only
 * `execute-code`; `policy/live-assistance-mode` mentions only `project`) --
 * fail-closing on every unmentioned action would deny nearly everything
 * under nearly every mode, which is not what any of the 6 policy/* sets'
 * own prose describes them as doing.
 */
export function checkPolicy(capability: string, activeMode: PolicyId = DEFAULT_ACTIVE_MODE): PolicyCheckResult {
  const action = ACTION_FOR_CAPABILITY[capability];
  if (!action) {
    return {
      allowed: true,
      reason: `no ontology-defined authority-action for capability "${capability}" -- ungoverned by 50-policy.ttl (see policy-check-adapter.ts module doc)`,
    };
  }
  const decision = domainCheckPolicy(action, activeMode);
  if (decision === "denied") {
    return { allowed: false, reason: `${action} denied under ${activeMode}` };
  }
  return {
    allowed: true,
    reason:
      decision === "unspecified"
        ? `${action} unspecified under ${activeMode} (no odrl statement either way; not prohibited)`
        : `${action} allowed under ${activeMode}`,
  };
}

export type { PolicyId };
