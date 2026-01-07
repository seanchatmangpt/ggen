import { defineConfig } from 'vitest/config';
import { configureCitty } from 'citty/vitest';

export default defineConfig({
  test: {
    // Citty: Auto-detect and configure test environment
    ...configureCitty({
      // Choose test execution mode
      mode: 'local', // or 'docker' for containerized testing

      // Enable snapshot testing
      snapshots: true,

      // Timeout for CLI commands
      timeout: 30000,

      // Fail test if stderr is not empty
      failOnStderr: false,

      // Show full diff on assertion failure
      diff: true,
    }),

    // Vitest configuration
    globals: true,
    environment: 'node',
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      include: ['**/*.test.js'],
      exclude: ['node_modules'],
    },
    reporters: ['verbose'],
  },
});
