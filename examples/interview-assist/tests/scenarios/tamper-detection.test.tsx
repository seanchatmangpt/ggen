/**
 * TICKET-049: Vertical scenario -- Tamper detection.
 *
 * Playwright-vs-vitest substitution: see tests/scenarios/bootstrap.test.ts's
 * module doc for the full real evidence, re-verified fresh for this ticket
 * (see TICKET-048's module doc). No browser needed: this scenario needs
 * real fs I/O (TICKET-036), real replay (TICKET-025), real BLAKE3 hashing
 * (TICKET-038), and the real GENERATED presentation component
 * ReplayFailurePresentation (TICKET-032) -- exercised here via real React
 * SSR (`renderToStaticMarkup`, the actual production rendering function,
 * not mocked), which requires no browser at all since the component is a
 * pure function of its typed props. No mocked core collaborator.
 */
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { mkdtemp, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { renderToStaticMarkup } from "react-dom/server";
import { FilesystemEventLogStore, type EventLogEntry } from "../../lib/adapters/persistence-adapter";
import { replaySession } from "../../lib/domain/replay";
import { getChecksum } from "../../lib/adapters/checksum-adapter";
import { ReplayFailurePresentation } from "../../components/replay-failure-presentation";
import { buildRealSessionEventLog, assertFixtureLegalAgainstRealTable, toEventLogEntries, fromEventLogEntries } from "./fixtures/session-log";

function finalStateChecksum(result: ReturnType<typeof replaySession>): string {
  return getChecksum().hashHex(JSON.stringify(result));
}

describe("TICKET-049 tamper detection (real BLAKE3 hashing + real fs tamper + real SSR presentation, no mocks)", () => {
  let dir: string;
  let store: FilesystemEventLogStore;

  beforeEach(async () => {
    dir = await mkdtemp(join(tmpdir(), "interview-assist-tamper-"));
    store = new FilesystemEventLogStore(dir);
  });

  afterEach(async () => {
    await store.clear();
  });

  it("positive companion: an UNtampered persisted log's replay hash matches the original exactly, and ReplayFailurePresentation renders the verified state", async () => {
    const events = buildRealSessionEventLog();
    assertFixtureLegalAgainstRealTable(events);
    const original = replaySession(events);
    expect(original.status).toBe("admitted");
    const originalHash = finalStateChecksum(original);

    const sessionId = "tamper-detection-untampered";
    await store.save(sessionId, toEventLogEntries(events));
    const loaded = await store.load(sessionId);
    const replayed = replaySession(fromEventLogEntries(loaded!));
    const replayedHash = finalStateChecksum(replayed);

    expect(replayedHash).toBe(originalHash);

    const html = renderToStaticMarkup(
      <ReplayFailurePresentation originalHash={originalHash} replayedHash={replayedHash} />,
    );
    expect(html).toContain('data-matches="true"');
    expect(html).toContain("Replay verified: hashes match.");
    expect(html).not.toContain("Tamper detected");
  });

  it("a real single-field tamper (bypassing the app, mutating the persisted file directly) is detected: the recomputed hash diverges and ReplayFailurePresentation shows an explicit tamper-detected state (acceptance-step/10, Architecture Decision 12)", async () => {
    const events = buildRealSessionEventLog();
    assertFixtureLegalAgainstRealTable(events);
    const original = replaySession(events);
    expect(original.status).toBe("admitted");
    const originalHash = finalStateChecksum(original);

    const sessionId = "tamper-detection-tampered";
    await store.save(sessionId, toEventLogEntries(events));

    // Real tamper: read the persisted file directly off disk (bypassing
    // FilesystemEventLogStore's own save/load API entirely, simulating an
    // attacker or storage-layer corruption), parse it, and mutate exactly
    // ONE field on exactly ONE entry -- the targetPhase of the
    // WorkflowEvent that legally advances CLARIFICATION -> PLANNING is
    // flipped to DEBUGGING (a target the real transition-plan table does
    // NOT admit from CLARIFICATION), then write the mutated JSON back.
    const filePath = join(dir, `${sessionId}.json`);
    const raw = await readFile(filePath, "utf8");
    const entries = JSON.parse(raw) as EventLogEntry[];
    const targetIndex = entries.findIndex(
      (e) => (e.payload as { family?: string; targetPhase?: string }).targetPhase === "PLANNING",
    );
    expect(targetIndex).toBeGreaterThanOrEqual(0); // sanity: the field we intend to tamper really exists
    const before = JSON.stringify(entries[targetIndex]);
    (entries[targetIndex]!.payload as { targetPhase: string }).targetPhase = "DEBUGGING";
    expect(JSON.stringify(entries[targetIndex])).not.toBe(before); // real, verified single-field mutation
    await writeFile(filePath, JSON.stringify(entries, null, 2), "utf8");

    // Reload (real disk read of the tampered file) and independently
    // re-derive via the real reducer -- replay.ts never trusts the
    // persisted final state, it re-runs isLegalTransition for every hop.
    const loaded = await store.load(sessionId);
    const tamperedResult = replaySession(fromEventLogEntries(loaded!));
    const tamperedHash = finalStateChecksum(tamperedResult);

    expect(tamperedHash).not.toBe(originalHash);
    // The tamper is a genuinely illegal transition (CLARIFICATION has no
    // transition-plan edge to DEBUGGING), so real re-derivation refuses at
    // that hop rather than silently accepting a different-but-plausible
    // final state -- this IS the real tamper-detection mechanism, not an
    // incidental side effect.
    expect(tamperedResult.status).toBe("refused");

    const html = renderToStaticMarkup(
      <ReplayFailurePresentation originalHash={originalHash} replayedHash={tamperedHash} />,
    );
    expect(html).toContain('data-matches="false"');
    expect(html).toContain("Tamper detected: replay hash mismatch.");
    expect(html).toContain(originalHash);
    expect(html).toContain(tamperedHash);
  });
});
