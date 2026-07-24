/**
 * TICKET-037: Ollama client adapter (custom).
 *
 * Real OpenAI-compatible HTTP transport to a local Ollama server, reusing
 * the proven local-model pattern already established in this repo at
 * examples/nextjs-ai-sdk (endpoint shape: http://localhost:11434/v1,
 * OpenAI-compatible chat-completions body) rather than reinventing the
 * transport. `examples/nextjs-ai-sdk/lib/ai/models.ts` uses the Vercel AI
 * SDK's `gateway()` provider against a hosted gateway; this adapter targets
 * the OpenAI-compatible surface of a LOCAL Ollama instance directly via
 * `fetch`, since `@ai-sdk/openai-compatible` is not a dependency of this
 * package (adding the AI SDK is out of scope for a thin adapter -- the
 * wire protocol it wraps is the actual proven part being reused here, not
 * the SDK object itself).
 *
 * TICKET-018's generated SelfPlayWorker port has not landed yet in this
 * session; the `SelfPlayWorker` interface below is authored by hand and
 * marked PENDING(TICKET-018).
 *
 * Per ARD's own architecture decision, this adapter runs OUTSIDE the live
 * runtime's critical path (self-play/offline generation only) -- it is
 * never on the candidate-facing request path.
 */
import { checkPolicy, DEFAULT_ACTIVE_MODE, type PolicyId } from "./policy-check-adapter";

export type SelfPlayRole = "interviewer" | "candidate" | "test-generator" | "critic";

/** PENDING(TICKET-018): expected shape of the generated SelfPlayWorker port. */
export interface SelfPlayRequest {
  role: SelfPlayRole;
  prompt: string;
  model?: string;
  /** Optional: see policy-check-adapter.ts's DEFAULT_ACTIVE_MODE doc. */
  activeMode?: PolicyId;
}

export interface SelfPlayResponse {
  role: SelfPlayRole;
  content: string;
  model: string;
}

export interface SelfPlayWorker {
  run(request: SelfPlayRequest): Promise<SelfPlayResponse>;
}

export interface OllamaAdapterConfig {
  baseUrl: string;
  defaultModel: string;
  timeoutMs: number;
}

export const DEFAULT_OLLAMA_CONFIG: OllamaAdapterConfig = {
  baseUrl: "http://localhost:11434/v1",
  defaultModel: "qwen3.5:0.8b",
  timeoutMs: 30_000,
};

export class OllamaUnreachableError extends Error {}

class OllamaSelfPlayWorker implements SelfPlayWorker {
  constructor(private readonly config: OllamaAdapterConfig) {}

  async run(request: SelfPlayRequest): Promise<SelfPlayResponse> {
    const decision = checkPolicy(`self_play_${request.role}`, request.activeMode ?? DEFAULT_ACTIVE_MODE);
    if (!decision.allowed) {
      throw new Error(`ollama-adapter refused: ${decision.reason ?? "policy denied"}`);
    }

    const model = request.model ?? this.config.defaultModel;
    const controller = new AbortController();
    const timer = setTimeout(() => controller.abort(), this.config.timeoutMs);
    try {
      const res = await fetch(`${this.config.baseUrl}/chat/completions`, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({
          model,
          messages: [{ role: "user", content: request.prompt }],
          stream: false,
        }),
        signal: controller.signal,
      });
      if (!res.ok) {
        throw new OllamaUnreachableError(`ollama returned HTTP ${res.status}`);
      }
      const json = (await res.json()) as {
        choices?: Array<{ message?: { content?: string } }>;
        model?: string;
      };
      const content = json.choices?.[0]?.message?.content ?? "";
      return { role: request.role, content, model: json.model ?? model };
    } catch (err) {
      if (err instanceof OllamaUnreachableError) throw err;
      throw new OllamaUnreachableError(err instanceof Error ? err.message : String(err));
    } finally {
      clearTimeout(timer);
    }
  }
}

export function getOllamaWorker(config: OllamaAdapterConfig = DEFAULT_OLLAMA_CONFIG): SelfPlayWorker {
  return new OllamaSelfPlayWorker(config);
}

/**
 * Real reachability probe (mirrors `curl -s http://localhost:11434/api/tags`)
 * -- used by tests to decide whether to attempt a live call or report
 * BLOCKED honestly instead of failing the whole suite when no local
 * Ollama server happens to be running.
 */
export async function isOllamaReachable(baseUrl = "http://localhost:11434", timeoutMs = 2000): Promise<boolean> {
  try {
    const controller = new AbortController();
    const timer = setTimeout(() => controller.abort(), timeoutMs);
    const res = await fetch(`${baseUrl}/api/tags`, { signal: controller.signal });
    clearTimeout(timer);
    return res.ok;
  } catch {
    return false;
  }
}

/**
 * Reduction path: if self-play migrates to an in-process embedding model,
 * the network-transport half of this file (the fetch call + AbortController
 * timeout handling) shrinks or disappears; the worker-role prompt-building
 * logic stays the same regardless of transport.
 */
export const REDUCTION_PATH_NOTE =
  "fetch-based OpenAI-compatible transport is the reusable proven half; " +
  "would shrink if self-play moves in-process, role logic stays.";
