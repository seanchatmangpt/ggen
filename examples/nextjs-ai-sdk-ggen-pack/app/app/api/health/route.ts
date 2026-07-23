import { admittedModels, DEFAULT_MODEL_ID } from "@/lib/ai/models";

export function GET(): Response {
  return Response.json({
    status: "ALIVE",
    model: DEFAULT_MODEL_ID,
    admittedModels,
    timestamp: new Date().toISOString(),
  });
}
