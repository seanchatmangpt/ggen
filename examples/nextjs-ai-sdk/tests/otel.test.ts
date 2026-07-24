import { randomUUID } from "node:crypto";
import { trace } from "@opentelemetry/api";
import { BasicTracerProvider, InMemorySpanExporter, SimpleSpanProcessor } from "@opentelemetry/sdk-trace-base";
import { sql } from "drizzle-orm";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";
import { getDatabase, getPool } from "@/lib/db";
import { user } from "@/lib/db/schema";
import { createToolBroker } from "@/lib/ai/tool-broker";

/**
 * Chicago TDD: a real @opentelemetry/sdk-trace-base tracer provider,
 * registered as the real global provider @opentelemetry/api's trace API
 * dispatches to -- not a mocked tracer/span. Asserts on spans the real
 * ToolBroker actually emitted while hitting the real database.
 */

const exporter = new InMemorySpanExporter();
let provider: BasicTracerProvider;

beforeAll(() => {
  provider = new BasicTracerProvider({ spanProcessors: [new SimpleSpanProcessor(exporter)] });
  trace.setGlobalTracerProvider(provider);
});

afterAll(async () => {
  await provider.shutdown();
  await getPool().end();
});

beforeEach(async () => {
  exporter.reset();
  await getDatabase().execute(sql`TRUNCATE TABLE "user" CASCADE`);
});

async function insertUser() {
  const id = randomUUID();
  await getDatabase().insert(user).values({ id, name: "Otel User", email: `${id}@example.invalid` });
  return id;
}

describe("ToolBroker emits real OpenTelemetry spans", () => {
  it("records a real span with real receipt attributes on success", async () => {
    const userId = await insertUser();
    const requestId = randomUUID();
    const broker = createToolBroker();

    const result = await broker.invoke("notes.create", { title: "spanned", body: "b" }, { requestId, userId });

    const spans = exporter.getFinishedSpans();
    const toolSpan = spans.find((s) => s.name === "tool.notes.create");
    expect(toolSpan).toBeDefined();
    expect(toolSpan!.attributes["tool.handler_key"]).toBe("notes.create");
    expect(toolSpan!.attributes["tool.request_id"]).toBe(requestId);
    expect(toolSpan!.attributes["enduser.id"]).toBe(userId);
    // Real receipt digest attribute, matching the real receipt returned to the caller.
    expect(toolSpan!.attributes["tool.receipt.digest"]).toBe(result.receipt.digest);
    expect(toolSpan!.attributes["tool.receipt.status"]).toBe("ALIVE");
    expect(toolSpan!.status.code).toBe(1); // SpanStatusCode.OK
  });

  it("records an ERROR-status span with the real exception when the handler throws", async () => {
    const userId = await insertUser();
    const broker = createToolBroker();

    await expect(broker.invoke("notes.create", { title: "", body: "" }, { requestId: randomUUID(), userId })).rejects.toThrow();

    const spans = exporter.getFinishedSpans();
    const toolSpan = spans.find((s) => s.name === "tool.notes.create");
    expect(toolSpan).toBeDefined();
    expect(toolSpan!.status.code).toBe(2); // SpanStatusCode.ERROR
    expect(toolSpan!.events.some((e) => e.name === "exception")).toBe(true);
  });

  it("does not emit a span at all for a handler key that was never invoked (no fabricated telemetry)", async () => {
    const userId = await insertUser();
    const broker = createToolBroker();
    await expect(broker.invoke("notes.delete_everything", {}, { requestId: randomUUID(), userId })).rejects.toThrow();

    const spans = exporter.getFinishedSpans();
    expect(spans.find((s) => s.name === "tool.notes.delete_everything")).toBeUndefined();
  });
});
