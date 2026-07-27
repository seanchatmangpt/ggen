import { gateway } from "ai";

export const admittedModels = [
  { id: "openai/gpt-5.6-luna", label: "GPT-5.6 Luna", role: "fallback" },
  { id: "openai/gpt-5.6-terra", label: "GPT-5.6 Terra", role: "primary" },
] as const;

export type AdmittedModelId = (typeof admittedModels)[number]["id"];

export const DEFAULT_MODEL_ID: AdmittedModelId =
  (process.env.AI_MODEL as AdmittedModelId | undefined) ?? "openai/gpt-5.6-terra";

export const model = gateway(DEFAULT_MODEL_ID);
