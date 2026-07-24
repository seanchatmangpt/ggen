/**
 * TICKET-040: Vertical scenario -- Bootstrap.
 *
 * Playwright-vs-vitest substitution (documented per this workflow's explicit
 * fallback clause, not silently applied): a real `next build` against
 * examples/interview-assist/ currently FAILS --
 *
 *   Turbopack build failed with 1 errors:
 *   ./app/page.tsx
 *   Code generation for chunk item errored
 *   ...the chunking context (unknown) does not support external modules
 *   (request: node:module)
 *
 * Real-command evidence: `npx next build` (from examples/interview-assist/),
 * 2026-07-23, exit non-zero. Root cause traced by reading the import chain:
 * app/page.tsx ("use client") imports lib/domain/reducer.ts, which
 * unconditionally imports lib/domain/receipt-emitter.ts (TICKET-055), which
 * imports lib/adapters/checksum-adapter.ts, whose top-level
 * `import { createRequire } from "node:module"` is a real Node builtin that
 * cannot be bundled into a browser chunk. This is a real, reproduced
 * regression (checksum-adapter.ts's own module doc only ever audited this
 * import for SERVER route bundling, e.g. app/api/receipt/route.ts --
 * TICKET-055 wiring reducer.ts -> receipt-emitter.ts -> checksum-adapter.ts
 * is what newly drags it into the CLIENT bundle via page.tsx), not an
 * environment-installation gap: Playwright itself IS installed and working
 * (`npx playwright --version` -> 1.61.1; chromium-1181 present under
 * ~/Library/Caches/ms-playwright). Fixing this production bundling bug is
 * out of this ticket's own stated Custom-code boundary ("no new production
 * custom code introduced by this ticket, only test code exercising existing
 * adapters"), so it is reported here, unfixed, rather than silently patched
 * or silently ignored.
 *
 * Per the assignment's explicit fallback, this scenario is authored as a
 * real vitest test against the real reducer / real RDF-generated
 * capability-dispatch and precondition tables -- no mocked core
 * collaborator, just no browser.
 */
import { describe, it, expect } from "vitest";
import { sessionReducer, type SessionState } from "../../lib/domain/reducer";
import { CAPABILITY_DISPATCH, HTTP_CAPABILITY_COUNT } from "../../lib/domain/capability-dispatch";
import { checkPreconditions, type CapabilityState } from "../../lib/domain/preconditions";
import type { CapabilityId } from "../../lib/domain/capability";

describe("TICKET-040 bootstrap (real reducer + real RDF-generated dispatch/precondition tables, no mocks)", () => {
  it("capability/session/create-session is a real admitted hydra:Operation dispatch slot (RDF-sourced, acceptance-step/2)", () => {
    expect(Object.prototype.hasOwnProperty.call(CAPABILITY_DISPATCH, "capability/session/create-session")).toBe(
      true,
    );
    expect(HTTP_CAPABILITY_COUNT).toBe(9);
  });

  it("a fresh session dispatching the create-session-equivalent event through the real reducer reaches phase/created", () => {
    const freshState: SessionState = { phase: "CREATED" };
    const result = sessionReducer(freshState, { family: "SessionEvent", type: "create-session" });
    expect(result.status).toBe("admitted");
    if (result.status === "admitted") {
      // Sourced from the real reducer's own return value, not a hand-set
      // fixture: sessionReducer only ever returns state.phase unchanged
      // (no targetPhase supplied) or updated -- this assertion proves the
      // create-session-shaped event does not corrupt or drop it.
      expect(result.value.phase).toBe("CREATED");
    }
  });

  it("negative: start-interview is refused before create-session is satisfied -- no session exists yet, no implicit state", () => {
    const noSessionYet: CapabilityState = new Set<CapabilityId>();
    const result = checkPreconditions("capability/session/start-interview", noSessionYet);
    expect(result.met).toBe(false);
    expect(result.missing).toContain("capability/session/create-session");
  });

  it("negative: join-session is likewise refused before create-session (a second real dcterms:requires edge, not a one-off)", () => {
    const noSessionYet: CapabilityState = new Set<CapabilityId>();
    const result = checkPreconditions("capability/session/join-session", noSessionYet);
    expect(result.met).toBe(false);
    expect(result.missing).toContain("capability/session/create-session");
  });

  it("positive control: once create-session is marked satisfied, start-interview's precondition IS met (proves the check is a real gate, not always-false)", () => {
    const withSession: CapabilityState = new Set<CapabilityId>(["capability/session/create-session"]);
    const result = checkPreconditions("capability/session/start-interview", withSession);
    expect(result.met).toBe(true);
    expect(result.missing).toEqual([]);
  });
});
