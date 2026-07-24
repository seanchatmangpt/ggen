import { randomUUID } from "node:crypto";
import { eq, sql } from "drizzle-orm";
import { afterAll, beforeEach, describe, expect, it } from "vitest";
import { getDatabase, getPool } from "@/lib/db";
import { notes, toolReceipts, user } from "@/lib/db/schema";
import { createToolBroker } from "@/lib/ai/tool-broker";
import { blake3Hex } from "@/lib/receipts/blake3";
import { canonicalJson } from "@/lib/receipts/canonical-json";

/**
 * Chicago TDD: real local Postgres (started for this session), real
 * @neondatabase/serverless driver (the exact one that ships to production),
 * real drizzle schema and migration SQL. No mocked client, no fake receipts.
 */

const db = getDatabase();

async function insertUser(overrides: Partial<{ id: string; name: string; email: string }> = {}) {
  const id = overrides.id ?? randomUUID();
  await db.insert(user).values({
    id,
    name: overrides.name ?? "Test User",
    email: overrides.email ?? `${id}@example.invalid`,
  });
  return id;
}

beforeEach(async () => {
  // Real cascading delete exercised here too -- clearing `user` cascades to
  // every dependent table via the actual FK constraints, not app code.
  await db.execute(sql`TRUNCATE TABLE "user" CASCADE`);
});

afterAll(async () => {
  await getPool().end();
});

describe("schema matches the real, checked-in migration", () => {
  it("notes table has exactly the columns the migration created", async () => {
    const rows = await db.execute<{ column_name: string }>(
      sql`SELECT column_name FROM information_schema.columns WHERE table_name = 'notes' ORDER BY column_name`,
    );
    const columns = rows.rows.map((r) => r.column_name).sort();
    expect(columns).toEqual(["body", "created_at", "id", "title", "updated_at", "user_id"].sort());
  });

  it("tool_receipts.digest carries a real UNIQUE constraint", async () => {
    const rows = await db.execute<{ conname: string }>(
      sql`SELECT conname FROM pg_constraint WHERE conrelid = 'tool_receipts'::regclass AND contype = 'u'`,
    );
    expect(rows.rows.some((r) => r.conname === "tool_receipts_digest_key")).toBe(true);
  });
});

describe("notes.create tool handler against a real database", () => {
  it("persists a real row and returns a receipt whose digest independently recomputes", async () => {
    const userId = await insertUser();
    const broker = createToolBroker();
    const requestId = randomUUID();

    const result = await broker.invoke("notes.create", { title: "Real DB note", body: "Written by a real transaction." }, { requestId, userId });

    expect(result.receipt.status).toBe("ALIVE");
    expect(result.receipt.algorithm).toBe("blake3");
    expect(result.receipt.digest).toMatch(/^[0-9a-f]{64}$/);

    const rows = await db.select().from(notes).where(eq(notes.userId, userId));
    expect(rows).toHaveLength(1);
    expect(rows[0]?.title).toBe("Real DB note");
    expect(rows[0]?.body).toBe("Written by a real transaction.");

    const receiptRows = await db.select().from(toolReceipts).where(eq(toolReceipts.requestId, requestId));
    expect(receiptRows).toHaveLength(1);
    expect(receiptRows[0]?.digest).toBe(result.receipt.digest);

    // Recompute the digest independently from the persisted row -- proves
    // the receipt is a real function of the real output, not a fabricated
    // constant.
    const persisted = receiptRows[0]!;
    const recomputed = blake3Hex({
      algorithm: "blake3",
      status: "ALIVE",
      requestId: persisted.requestId,
      userId: persisted.userId,
      handlerKey: persisted.handlerKey,
      input: persisted.input,
      output: persisted.output,
      occurredAt: persisted.createdAt.toISOString(),
    });
    expect(recomputed).toBe(persisted.digest);
  });

  it("refuses input that fails real zod validation before touching the database", async () => {
    const userId = await insertUser();
    const broker = createToolBroker();
    await expect(broker.invoke("notes.create", { title: "", body: "" }, { requestId: randomUUID(), userId })).rejects.toThrow();

    const rows = await db.select().from(notes).where(eq(notes.userId, userId));
    expect(rows).toHaveLength(0);
  });

  it("does not leak notes across two real users", async () => {
    const alice = await insertUser({ name: "Alice" });
    const bob = await insertUser({ name: "Bob" });
    const broker = createToolBroker();

    await broker.invoke("notes.create", { title: "Alice's note", body: "private" }, { requestId: randomUUID(), userId: alice });
    await broker.invoke("notes.create", { title: "Bob's note", body: "also private" }, { requestId: randomUUID(), userId: bob });

    const listed = await broker.invoke("notes.list", { limit: 10 }, { requestId: randomUUID(), userId: alice });
    const titles = (listed.output as Array<{ title: string }>).map((n) => n.title);
    expect(titles).toEqual(["Alice's note"]);
  });

  it("lists notes newest-first, matching a real ORDER BY on the actual table", async () => {
    const userId = await insertUser();
    const broker = createToolBroker();
    await broker.invoke("notes.create", { title: "first", body: "b" }, { requestId: randomUUID(), userId });
    await new Promise((resolve) => setTimeout(resolve, 10));
    await broker.invoke("notes.create", { title: "second", body: "b" }, { requestId: randomUUID(), userId });

    const listed = await broker.invoke("notes.list", { limit: 10 }, { requestId: randomUUID(), userId });
    const titles = (listed.output as Array<{ title: string }>).map((n) => n.title);
    expect(titles).toEqual(["second", "first"]);
  });
});

describe("cascade delete is a real database constraint, not app logic", () => {
  it("deleting a user really removes their notes and receipts via ON DELETE CASCADE", async () => {
    const userId = await insertUser();
    const broker = createToolBroker();
    await broker.invoke("notes.create", { title: "will be cascaded", body: "b" }, { requestId: randomUUID(), userId });

    expect(await db.select().from(notes).where(eq(notes.userId, userId))).toHaveLength(1);
    expect(await db.select().from(toolReceipts).where(eq(toolReceipts.userId, userId))).toHaveLength(1);

    await db.delete(user).where(eq(user.id, userId));

    expect(await db.select().from(notes).where(eq(notes.userId, userId))).toHaveLength(0);
    expect(await db.select().from(toolReceipts).where(eq(toolReceipts.userId, userId))).toHaveLength(0);
  });
});

describe("ToolBroker refuses unknown handler keys", () => {
  it("throws ToolRefusedError rather than silently no-op'ing against the database", async () => {
    const userId = await insertUser();
    const broker = createToolBroker();
    await expect(broker.invoke("notes.delete_everything", {}, { requestId: randomUUID(), userId })).rejects.toThrow(/No admitted tool handler/);
  });
});

describe("canonical JSON + BLAKE3 receipts are tamper-evident", () => {
  it("changing one byte of the persisted output changes the recomputed digest", () => {
    const subject = { algorithm: "blake3" as const, status: "ALIVE" as const, requestId: "r1", userId: "u1", handlerKey: "notes.create", input: { title: "x" }, output: { id: "1" }, occurredAt: "2026-01-01T00:00:00.000Z" };
    const original = blake3Hex(subject);
    const tampered = blake3Hex({ ...subject, output: { id: "2" } });
    expect(tampered).not.toBe(original);
  });

  it("canonicalJson is key-order independent, matching how Postgres round-trips jsonb", () => {
    const a = canonicalJson({ b: 1, a: 2 });
    const b = canonicalJson({ a: 2, b: 1 });
    expect(a).toBe(b);
  });
});
