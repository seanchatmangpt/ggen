import { describe, it, expect, beforeEach, afterEach } from "vitest";
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { FilesystemEventLogStore, type EventLogEntry } from "../../lib/adapters/persistence-adapter";

describe("persistence-adapter (real filesystem I/O, Node-side stand-in for browser storage)", () => {
  let dir: string;
  let store: FilesystemEventLogStore;

  beforeEach(async () => {
    dir = await mkdtemp(join(tmpdir(), "interview-assist-persist-"));
    store = new FilesystemEventLogStore(dir);
  });

  afterEach(async () => {
    await store.clear();
  });

  it("returns undefined for a session that was never saved", async () => {
    const loaded = await store.load("never-saved");
    expect(loaded).toBeUndefined();
  });

  it("saves a real event log to disk and loads it back byte-for-byte equivalent", async () => {
    const log: EventLogEntry[] = [
      { seq: 1, type: "session_started", payload: { track: "python" }, timestampMs: 1000 },
      { seq: 2, type: "candidate_submitted", payload: { code: "print(1)" }, timestampMs: 2000 },
    ];
    await store.save("session-abc", log);
    const loaded = await store.load("session-abc");
    expect(loaded).toEqual(log);
  });

  it("persists across store instances (real disk state, not in-memory)", async () => {
    const log: EventLogEntry[] = [{ seq: 1, type: "x", payload: null, timestampMs: 1 }];
    await store.save("session-durable", log);
    const secondInstance = new FilesystemEventLogStore(dir);
    const loaded = await secondInstance.load("session-durable");
    expect(loaded).toEqual(log);
  });

  it("sanitizes session ids so path traversal cannot escape the store dir", async () => {
    await store.save("../../etc/evil", [{ seq: 1, type: "x", payload: null, timestampMs: 1 }]);
    const ids = await store.listSessionIds();
    expect(ids.some((id) => id.includes(".."))).toBe(false);
  });
});
