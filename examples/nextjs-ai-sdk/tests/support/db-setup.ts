import { neonConfig } from "@neondatabase/serverless";
import ws from "ws";

process.env.DATABASE_URL ??= "postgresql://sac@localhost:5432/nextjs_ai_sdk_test";
process.env.BETTER_AUTH_SECRET ??= "test-secret-not-for-production-0123456789abcdef";
process.env.AI_GATEWAY_API_KEY ??= "test-gateway-key";

neonConfig.webSocketConstructor = ws;
neonConfig.wsProxy = () => `localhost:${process.env.NEON_WS_PROXY_PORT ?? "5434"}`;
neonConfig.useSecureWebSocket = false;
neonConfig.pipelineTLS = false;
neonConfig.pipelineConnect = false;
