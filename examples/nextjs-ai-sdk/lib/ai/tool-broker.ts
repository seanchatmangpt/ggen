import "server-only";
import { SpanStatusCode, trace } from "@opentelemetry/api";
import { admittedToolHandlers } from "./tool-handlers";
import type { ToolExecutionContext, ToolExecutionResult, ToolHandler } from "./tool-contracts";

export class ToolRefusedError extends Error {
  readonly code = "TOOL_HANDLER_REFUSED";
  constructor(readonly handlerKey: string) { super(`No admitted tool handler is registered for ${handlerKey}`); }
}

export class ToolBroker {
  constructor(private readonly handlers: Readonly<Record<string, ToolHandler>>) {}

  invoke(handlerKey: string, input: unknown, context: ToolExecutionContext): Promise<ToolExecutionResult> {
    const handler = this.handlers[handlerKey];
    if (!handler) throw new ToolRefusedError(handlerKey);
    return trace.getTracer("nextjs-ai-sdk.tool-broker").startActiveSpan(`tool.${handlerKey}`, async (span) => {
      span.setAttributes({ "tool.handler_key": handlerKey, "tool.request_id": context.requestId, "enduser.id": context.userId });
      try {
        const result = await handler(input, context);
        span.setAttributes({ "tool.receipt.digest": result.receipt.digest, "tool.receipt.status": result.receipt.status });
        span.setStatus({ code: SpanStatusCode.OK });
        return result;
      } catch (error) {
        span.recordException(error as Error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error instanceof Error ? error.message : String(error) });
        throw error;
      } finally { span.end(); }
    });
  }
}

export function createToolBroker(): ToolBroker { return new ToolBroker(admittedToolHandlers); }
export type { ToolExecutionContext } from "./tool-contracts";
