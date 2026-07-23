import "server-only";
import { ToolLoopAgent, stepCountIs } from "ai";
import { model } from "./models";
import { createTools } from "./tools";
import type { ToolExecutionContext } from "./tool-broker";

export function createAppAgent(context: ToolExecutionContext) {
  return new ToolLoopAgent({
    model,
    providerOptions: { gateway: { models: ["openai/gpt-5.6-luna"] } },
    instructions: `You are a precise application assistant. Use tools only when they materially advance the request. Never retry a denied tool. State the receipt status of every actuation.`,
    tools: createTools(context),
    stopWhen: stepCountIs(12),
    experimental_telemetry: {
      isEnabled: true,
      functionId: "nextjs-ai-verify-agent",
      metadata: { requestId: context.requestId },
    },
  });
}

export type AppAgent = ReturnType<typeof createAppAgent>;
