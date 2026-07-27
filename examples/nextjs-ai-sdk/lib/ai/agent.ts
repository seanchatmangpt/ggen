import "server-only";
import { ToolLoopAgent, stepCountIs } from "ai";
import { gatewayProviderOptions, model } from "./models";
import { createTools } from "./tools";
import type { ToolExecutionContext } from "./tool-broker";

export function createAppAgent(context: ToolExecutionContext) {
  return new ToolLoopAgent({
    model,
    providerOptions: gatewayProviderOptions,
    instructions: `You are a precise application assistant. Use notes_list to read notes and notes_create only when the user explicitly asks to persist a note. Never retry a denied tool. After every tool call, report the returned receipt status and digest.`,
    tools: createTools(context),
    stopWhen: stepCountIs(12),
    experimental_telemetry: { isEnabled: true, functionId: "nextjs-ai-sdk-agent", metadata: { requestId: context.requestId, userId: context.userId } },
  });
}

export type AppAgent = ReturnType<typeof createAppAgent>;
