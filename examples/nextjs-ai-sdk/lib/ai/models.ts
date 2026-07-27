import "server-only";
import { gateway } from "@ai-sdk/gateway";

export const admittedModels = [
  { id: "openai/gpt-5.4-mini", label: "GPT-5.4 Mini", role: "fallback" },
  { id: "openai/gpt-5.4", label: "GPT-5.4", role: "primary" },
] as const;

export type AdmittedModelId = (typeof admittedModels)[number]["id"];
const admittedModelIds = new Set<string>(admittedModels.map(({ id }) => id));

export class ModelRefusedError extends Error {
  readonly code = "MODEL_NOT_ADMITTED";
}

function resolvePrimaryModel(): AdmittedModelId {
  const requested = process.env.AI_MODEL ?? "openai/gpt-5.4";
  if (!admittedModelIds.has(requested)) {
    throw new ModelRefusedError(`AI_MODEL ${requested} is not admitted by schema/domain.ttl`);
  }
  return requested as AdmittedModelId;
}

export const PRIMARY_MODEL_ID = resolvePrimaryModel();
export const FALLBACK_MODEL_ID: AdmittedModelId = "openai/gpt-5.4-mini";
export const model = gateway(PRIMARY_MODEL_ID);
export const gatewayProviderOptions = { gateway: { models: [FALLBACK_MODEL_ID] } } as const;
