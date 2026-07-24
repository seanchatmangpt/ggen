/**
 * TICKET-036: Local persistence adapter (custom).
 *
 * Real event-log persistence behind a generated PersistenceAdapter port.
 * TICKET-020 (which is meant to define this interface as part of its
 * receipt/state type projection scope) has not generated yet, so the
 * `PersistenceAdapter` interface below is authored by hand and marked
 * PENDING(TICKET-020) — it must be reconciled against the real generated
 * shape once that ticket lands, not treated as final.
 *
 * HONEST SUBSTITUTION: the objective text says "real localStorage/
 * IndexedDB persistence", but this adapter runs in a Node/Vitest test
 * context, not a browser — there is no real `window.indexedDB` available
 * here. Per the task's own instruction, this is implemented against the
 * real filesystem (a temp-directory-based JSON store) as an explicitly
 * documented Node-side stand-in for "browser storage", not a claim of real
 * IndexedDB. A browser build would swap this file's internals for real
 * `indexedDB.open(...)` calls behind the exact same `PersistenceAdapter`
 * interface -- the port stays identical either way.
 *
 * TICKET-028 wiring closure note: this adapter deliberately stays on
 * `policy-check-stub.ts`'s default-allow placeholder, not the real
 * `policy-check-adapter.ts`. The closest-sounding authority class,
 * `authority-action/retain`, is declared in 50-policy.ttl but never once
 * appears in any `odrl:permission`/`odrl:prohibition` statement across all
 * 6 `policy/*` sets (verified:
 * `grep -c 'authority-action/retain\|authority-action/export' 50-policy.ttl`
 * -> 2, both bare declarations) -- it is currently ungoverned by every
 * admitted policy, not merely by this wiring. Mapping onto it would make
 * every save/load call return "unspecified" regardless of mode; treating
 * that as fail-closed would deny local persistence under every mode,
 * including the ones documented as least-restrictive. See
 * lib/adapters/policy-check-adapter.ts's module doc for the full reasoning.
 */
import { mkdir, readFile, writeFile, readdir, rm } from "node:fs/promises";
import { join } from "node:path";
import { checkPolicy } from "./policy-check-stub";

/** PENDING(TICKET-020): expected shape of the generated PersistenceAdapter
 * port. Replace with the real generated import once it exists. */
export interface EventLogEntry {
  seq: number;
  type: string;
  payload: unknown;
  timestampMs: number;
}

export interface PersistenceAdapter {
  save(sessionId: string, eventLog: EventLogEntry[]): Promise<void>;
  load(sessionId: string): Promise<EventLogEntry[] | undefined>;
}

/**
 * A real filesystem-backed store: one JSON file per session id under
 * `storeDir`. This is real I/O (readFile/writeFile against a real temp
 * directory), not an in-memory fake -- satisfies this repo's Chicago-TDD
 * "real collaborators" requirement for the Node-side substitution.
 */
export class FilesystemEventLogStore implements PersistenceAdapter {
  constructor(private readonly storeDir: string) {}

  private pathFor(sessionId: string): string {
    const safeId = sessionId.replace(/[^a-zA-Z0-9_-]/g, "_");
    return join(this.storeDir, `${safeId}.json`);
  }

  async save(sessionId: string, eventLog: EventLogEntry[]): Promise<void> {
    const decision = checkPolicy({ capability: "persistence_save" });
    if (!decision.allowed) {
      throw new Error(`persistence-adapter refused save: ${decision.reason ?? "policy denied"}`);
    }
    await mkdir(this.storeDir, { recursive: true });
    await writeFile(this.pathFor(sessionId), JSON.stringify(eventLog, null, 2), "utf8");
  }

  async load(sessionId: string): Promise<EventLogEntry[] | undefined> {
    const decision = checkPolicy({ capability: "persistence_load" });
    if (!decision.allowed) {
      throw new Error(`persistence-adapter refused load: ${decision.reason ?? "policy denied"}`);
    }
    try {
      const raw = await readFile(this.pathFor(sessionId), "utf8");
      return JSON.parse(raw) as EventLogEntry[];
    } catch (err: unknown) {
      if (err && typeof err === "object" && "code" in err && (err as { code: string }).code === "ENOENT") {
        return undefined;
      }
      throw err;
    }
  }

  /** Test/ops helper -- not part of the PersistenceAdapter port. */
  async listSessionIds(): Promise<string[]> {
    try {
      const files = await readdir(this.storeDir);
      return files.filter((f) => f.endsWith(".json")).map((f) => f.slice(0, -".json".length));
    } catch {
      return [];
    }
  }

  /** Test/ops helper -- not part of the PersistenceAdapter port. */
  async clear(): Promise<void> {
    await rm(this.storeDir, { recursive: true, force: true }).catch(() => undefined);
  }
}

/**
 * Reduction path: if session persistence moves server-side to a real
 * database, only this file's internals swap (SQL/HTTP calls instead of
 * fs calls) -- the `PersistenceAdapter` interface (once reconciled against
 * TICKET-020's real generated shape) stays the same, and callers (e.g.
 * TICKET-025's replaySession) never need to change.
 */
export const REDUCTION_PATH_NOTE =
  "PersistenceAdapter port is stable across storage-backend swaps " +
  "(fs today, real IndexedDB in-browser, or a server DB later); only " +
  "FilesystemEventLogStore's internals are backend-specific.";
