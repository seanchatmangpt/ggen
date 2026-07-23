import "server-only";
import { desc, eq } from "drizzle-orm";
import { z } from "zod";
import { getDatabase } from "@/lib/db";
import { notes, toolReceipts } from "@/lib/db/schema";
import { blake3Hex } from "@/lib/receipts/blake3";
import type { ToolExecutionResult, ToolHandler } from "./tool-contracts";

const createNoteInput = z.object({ title: z.string().min(1).max(160), body: z.string().min(1).max(10_000) }).strict();
const listNotesInput = z.object({ limit: z.number().int().min(1).max(50).default(20) }).strict();

type DatabaseTransaction = Parameters<Parameters<ReturnType<typeof getDatabase>["transaction"]>[0]>[0];

async function transactWithReceipt<T>(handlerKey: string, input: unknown, context: Parameters<ToolHandler>[1], operation: (tx: DatabaseTransaction) => Promise<T>): Promise<ToolExecutionResult<T>> {
  const db = getDatabase();
  return db.transaction(async (tx) => {
    const output = await operation(tx);
    const occurredAt = new Date().toISOString();
    const receiptSubject = { algorithm: "blake3" as const, status: "ALIVE" as const, requestId: context.requestId, userId: context.userId, handlerKey, input, output, occurredAt };
    const digest = blake3Hex(receiptSubject);
    const receipt = { algorithm: receiptSubject.algorithm, digest, requestId: context.requestId, userId: context.userId, handlerKey, status: "ALIVE" as const, occurredAt };
    await tx.insert(toolReceipts).values({ requestId: context.requestId, userId: context.userId, toolName: handlerKey.replace(".", "_"), handlerKey, input, output, status: receipt.status, algorithm: receipt.algorithm, digest });
    return { output, receipt };
  });
}

const createNote: ToolHandler = async (raw, context) => {
  const input = createNoteInput.parse(raw);
  return transactWithReceipt("notes.create", input, context, async (tx) => {
    const [note] = await tx.insert(notes).values({ userId: context.userId, title: input.title, body: input.body }).returning();
    if (!note) throw new Error("note insert returned no row");
    return { ...note, createdAt: note.createdAt.toISOString(), updatedAt: note.updatedAt.toISOString() };
  });
};

const listNotes: ToolHandler = async (raw, context) => {
  const input = listNotesInput.parse(raw ?? {});
  return transactWithReceipt("notes.list", input, context, async (tx) => {
    const rows = await tx.select().from(notes).where(eq(notes.userId, context.userId)).orderBy(desc(notes.createdAt)).limit(input.limit);
    return rows.map((note) => ({ ...note, createdAt: note.createdAt.toISOString(), updatedAt: note.updatedAt.toISOString() }));
  });
};

export const admittedToolHandlers: Readonly<Record<string, ToolHandler>> = Object.freeze({
  "notes.create": createNote,
  "notes.list": listNotes,
});
