import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { eq } from "drizzle-orm";
import { createToolBroker } from "@/lib/ai/tool-broker";
import { getDatabase, getPool } from "@/lib/db";
import { notes, toolReceipts, user } from "@/lib/db/schema";
import { blake3Hex } from "@/lib/receipts/blake3";

const userId = `boundary-${crypto.randomUUID()}`;
const requestId = crypto.randomUUID();

describe("brokered database actuation", () => {
  beforeAll(async () => {
    if (!process.env.DATABASE_URL) throw new Error("DATABASE_URL is required: this test refuses to replace PostgreSQL with an in-memory substitute");
    await getDatabase().insert(user).values({ id: userId, name: "Boundary Test", email: `${userId}@example.invalid` });
  });
  afterAll(async () => { await getDatabase().delete(user).where(eq(user.id, userId)); await getPool().end(); });

  it("creates state and a recomputable BLAKE3 receipt in one real transaction", async () => {
    const input = { title: "Receipt boundary", body: "Persisted through the admitted tool broker" };
    const result = await createToolBroker().invoke("notes.create", input, { requestId, userId });
    const [note] = await getDatabase().select().from(notes).where(eq(notes.userId, userId));
    const [stored] = await getDatabase().select().from(toolReceipts).where(eq(toolReceipts.digest, result.receipt.digest));
    expect(note?.title).toBe(input.title);
    expect(stored?.handlerKey).toBe("notes.create");
    expect(result.receipt.digest).toBe(blake3Hex({ algorithm: "blake3", status: "ALIVE", requestId, userId, handlerKey: "notes.create", input, output: result.output, occurredAt: result.receipt.occurredAt }));
  });
});
