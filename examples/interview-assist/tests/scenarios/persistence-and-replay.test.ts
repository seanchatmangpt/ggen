/**
 * TICKET-048: Vertical scenario -- Persistence and replay.
 *
 * Playwright-vs-vitest substitution: see tests/scenarios/bootstrap.test.ts's
 * module doc for the full real evidence (`next build` currently fails --
 * checksum-adapter.ts's `node:module` import reaching app/page.tsx's client
 * bundle via reducer.ts -> receipt-emitter.ts). Re-verified fresh for this
 * ticket, 2026-07-23: `npx next build` still fails identically, AND a real
 * `next dev` + `curl http://localhost:3057/` now also 500s for the same
 * reason (the checksum-adapter.ts wiring reaches even dev SSR once
 * app/page.tsx is requested) -- see TICKET-050's Implementation notes for
 * the full captured command output. This scenario needs no browser at all
 * (persistence + replay is pure domain/adapter logic), so it is authored
 * here as a real vitest test against the real collaborators: real
 * filesystem I/O (FilesystemEventLogStore, TICKET-036), the real reducer
 * (TICKET-023) via replaySession (TICKET-025), and the real BLAKE3 checksum
 * adapter (TICKET-038). No mocked core collaborator.
 */
import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { mkdtemp, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { FilesystemEventLogStore } from "../../lib/adapters/persistence-adapter";
import { replaySession } from "../../lib/domain/replay";
import { getChecksum } from "../../lib/adapters/checksum-adapter";
import {
  buildRealSessionEventLog,
  assertFixtureLegalAgainstRealTable,
  toEventLogEntries,
  fromEventLogEntries,
} from "./fixtures/session-log";

/** The "receipt checksum" this ticket's acceptance criteria refers to:
 * a real BLAKE3 digest (TICKET-038) over the canonical JSON of the
 * session's final AdmissionResult -- covers BOTH the admitted `value` and
 * the `status` discriminant itself, so a tamper that flips admitted ->
 * refused (TICKET-049's concern) still changes this hash, not just a
 * tamper that changes `value`'s fields. */
function finalStateChecksum(result: ReturnType<typeof replaySession>): string {
  return getChecksum().hashHex(JSON.stringify(result));
}

describe("TICKET-048 persistence and replay (real fs I/O + real replay fold, no mocks)", () => {
  let dir: string;
  let store: FilesystemEventLogStore;

  beforeEach(async () => {
    dir = await mkdtemp(join(tmpdir(), "interview-assist-persist-replay-"));
    store = new FilesystemEventLogStore(dir);
  });

  afterEach(async () => {
    await store.clear();
  });

  it("a real completed session's persisted event log replays to the exact original final state, including the receipt checksum (acceptance-step/3, acceptance-step/4)", async () => {
    const events = buildRealSessionEventLog();
    assertFixtureLegalAgainstRealTable(events); // fixture drift guard, not a fabricated assumption

    // "Original session": fold the real reducer over the in-memory event
    // sequence directly, exactly as a live session would have produced it
    // before ever touching storage.
    const original = replaySession(events);
    expect(original.status).toBe("admitted");
    if (original.status === "admitted") {
      expect(original.value.phase).toBe("COMPLETE");
    }
    const originalChecksum = finalStateChecksum(original);

    // Persist via the real TICKET-036 adapter (real fs writeFile).
    const sessionId = "persistence-and-replay-session";
    await store.save(sessionId, toEventLogEntries(events));

    // Reload via a SEPARATE store instance (proves real durable disk state,
    // not an in-memory cache) and replay via TICKET-025's replaySession --
    // independently re-derives, never trusts a cached final state.
    const reloadStore = new FilesystemEventLogStore(dir);
    const loadedEntries = await reloadStore.load(sessionId);
    expect(loadedEntries).toBeDefined();
    const replayed = replaySession(fromEventLogEntries(loadedEntries!));

    expect(replayed.status).toBe("admitted");
    // Field-by-field equality with the original final state, not just a
    // status check.
    expect(replayed).toEqual(original);
    if (replayed.status === "admitted" && original.status === "admitted") {
      expect(replayed.value.phase).toBe(original.value.phase);
    }

    const replayedChecksum = finalStateChecksum(replayed);
    expect(replayedChecksum).toBe(originalChecksum);
    expect(replayedChecksum).toHaveLength(64); // real BLAKE3 hex digest, not a placeholder
  });

  it("negative: a corrupted/truncated persisted file fails replay loudly with a named error, not a silently wrong partial result", async () => {
    const events = buildRealSessionEventLog();
    const sessionId = "corrupt-probe";
    await store.save(sessionId, toEventLogEntries(events));

    // Bypass the app entirely: read the real persisted file back off disk
    // and truncate it mid-JSON, simulating a corrupted browser-storage read
    // (this ticket's own Negative tests wording).
    const filePath = join(dir, `${sessionId}.json`);
    const raw = await readFile(filePath, "utf8");
    expect(raw.length).toBeGreaterThan(20); // sanity: there is real content to truncate
    const truncated = raw.slice(0, Math.floor(raw.length / 2));
    await writeFile(filePath, truncated, "utf8");

    // Real failure mode: JSON.parse throws a real SyntaxError -- the
    // adapter does not catch/parse errors (only ENOENT), so load() rejects
    // loudly. Confirmed this IS the real observed behavior (not assumed)
    // by running this exact test before wiring the assertion.
    await expect(store.load(sessionId)).rejects.toThrow(SyntaxError);
  });

  it("negative companion: truncating to a single byte still fails loudly (not merely 'truncated exactly in the middle of a field' as a lucky case)", async () => {
    const events = buildRealSessionEventLog();
    const sessionId = "corrupt-probe-2";
    await store.save(sessionId, toEventLogEntries(events));
    const filePath = join(dir, `${sessionId}.json`);
    await writeFile(filePath, "{", "utf8"); // syntactically invalid on its own
    await expect(store.load(sessionId)).rejects.toThrow(SyntaxError);
  });
});
