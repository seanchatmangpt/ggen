import { registerOTel } from "@vercel/otel";

export function register(): void {
  registerOTel({ serviceName: process.env.OTEL_SERVICE_NAME ?? "nextjs-ai-sdk" });
}
