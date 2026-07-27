import "server-only";
import { toolBroker } from "./tool-broker";

// Replace these fixture handlers with application adapters. The broker remains
// the only execution path so every side effect has a named admission boundary.
toolBroker.register("system.echo", async (input, context) => ({
  input,
  receipt: { requestId: context.requestId, handlerKey: "system.echo", status: "ALIVE" },
}));

toolBroker.register("notes.create", async (input, context) => ({
  input,
  receipt: { requestId: context.requestId, handlerKey: "notes.create", status: "PARTIAL_ALIVE" },
  warning: "Fixture adapter only; replace with a transactional repository implementation.",
}));
