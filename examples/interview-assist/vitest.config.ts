import { defineConfig } from "vitest/config";
import path from "node:path";
export default defineConfig({ resolve: { alias: { "@": path.resolve(__dirname, ".") } }, test: { environment: "node", include: ["tests/**/*.test.ts"], testTimeout: 30_000, hookTimeout: 30_000 } });
