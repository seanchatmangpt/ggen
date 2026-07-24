import { randomUUID } from "node:crypto";
import { createOpenAICompatible } from "@ai-sdk/openai-compatible";
import { ToolLoopAgent, stepCountIs } from "ai";
import { it } from "vitest";
import { getDatabase, getPool } from "@/lib/db";
import { user, notes } from "@/lib/db/schema";
import { createTools } from "@/lib/ai/tools";
import { eq } from "drizzle-orm";

it("debug", async () => {
  const userId = randomUUID();
  await getDatabase().insert(user).values({ id: userId, name: "Agent Test", email: `${userId}@example.invalid` });

  const ollama = createOpenAICompatible({ name: "ollama", baseURL: "http://localhost:11434/v1" });
  const model = ollama.chatModel("qwen3.5:0.8b");
  const context = { requestId: randomUUID(), userId };

  const agent = new ToolLoopAgent({
    model,
    instructions: "You are a precise application assistant. Use notes_create to persist a note when asked.",
    tools: createTools(context),
    stopWhen: stepCountIs(4),
  });

  const result = await agent.generate({ prompt: "Create a note titled 'Ollama test' with body 'written by a real local model' using notes_create." });
  console.log("TEXT", result.text);
  console.log("STEPS", JSON.stringify(result.steps?.map((s) => s.content), null, 2));

  const rows = await getDatabase().select().from(notes).where(eq(notes.userId, userId));
  console.log("NOTES IN DB", JSON.stringify(rows));
  await getPool().end();
}, 60000);
