import { sql } from "drizzle-orm";
import { getDatabase } from "@/lib/db";
import { PRIMARY_MODEL_ID } from "@/lib/ai/models";

export async function GET(): Promise<Response> {
  const missing = ["DATABASE_URL", "BETTER_AUTH_SECRET", "AI_GATEWAY_API_KEY"].filter((name) => !process.env[name]);
  if (missing.length) return Response.json({ status: "BLOCKED", missing }, { status: 503 });
  try {
    await getDatabase().execute(sql`select 1 as ready`);
    return Response.json({ status: "ALIVE", database: "reachable", model: PRIMARY_MODEL_ID });
  } catch {
    return Response.json({ status: "BLOCKED", database: "unreachable", code: "DATABASE_UNREACHABLE" }, { status: 503 });
  }
}
