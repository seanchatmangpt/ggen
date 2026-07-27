import "server-only";
import { jsonSchema, tool } from "ai";
import { createToolBroker, type ToolExecutionContext } from "./tool-broker";

export function createTools(context: ToolExecutionContext) {
  const broker = createToolBroker();
  return {
    notes_create: tool({
      title: "Create a note",
      description: "Creates an authenticated user's durable note and receipt in one database transaction.",
      inputSchema: jsonSchema({"type":"object","properties":{"title":{"type":"string","minLength":1,"maxLength":160},"body":{"type":"string","minLength":1,"maxLength":10000}},"required":["title","body"],"additionalProperties":false}),
      needsApproval: true,
      execute: (input) => broker.invoke("notes.create", input, context),
    }),
    notes_list: tool({
      title: "List notes",
      description: "Reads the authenticated user's durable notes and emits a BLAKE3 receipt.",
      inputSchema: jsonSchema({"type":"object","properties":{"limit":{"type":"integer","minimum":1,"maximum":50}},"additionalProperties":false}),
      needsApproval: false,
      execute: (input) => broker.invoke("notes.list", input, context),
    }),
  };
}

export type AppTools = ReturnType<typeof createTools>;
