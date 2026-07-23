import "server-only";
import { jsonSchema, tool } from "ai";
import "./tool-handlers";
import { toolBroker, type ToolExecutionContext } from "./tool-broker";

export function createTools(context: ToolExecutionContext) {
  return {
    notes_create: tool({
      title: "Create a note",
      description: "Creates a durable note through the application adapter.",
      inputSchema: jsonSchema({"type":"object","properties":{"title":{"type":"string"},"body":{"type":"string"}},"required":["title","body"],"additionalProperties":false}),
      needsApproval: true,
      execute: async (input) => toolBroker.invoke("notes.create", input, context),
    }),
    system_echo: tool({
      title: "Echo admitted input",
      description: "Returns the admitted input with a request-scoped receipt.",
      inputSchema: jsonSchema({"type":"object","properties":{"message":{"type":"string","description":"Message to echo"}},"required":["message"],"additionalProperties":false}),
      needsApproval: false,
      execute: async (input) => toolBroker.invoke("system.echo", input, context),
    }),
  };
}

export type AppTools = ReturnType<typeof createTools>;
