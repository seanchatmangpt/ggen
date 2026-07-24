import type { InferAgentUIMessage } from "ai";
import type { AppAgent } from "./agent";

export type AppUIMessage = InferAgentUIMessage<AppAgent>;
