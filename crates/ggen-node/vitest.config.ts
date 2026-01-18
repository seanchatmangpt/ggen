import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    // Environment for testing
    environment: 'node',

    // Enable browser testing for vanilla JS
    browser: {
      enabled: false, // Can be enabled with @vitest/browser
      provider: 'playwright',
      instances: [
        {
          browser: 'chromium',
          launch: {
            headless: true,
          },
        },
      ],
    },

    // Test file patterns
    include: [
      '__tests__/**/*.test.*',
      'tests/**/*.test.*',
      '**/__tests__/**/*.{test,spec}.*',
    ],

    // Test timeout
    testTimeout: 30000,

    // Global test setup
    globals: true,

    // Reporter configuration
    reporters: ['verbose', 'html'],
    outputFile: {
      html: './test-results/index.html',
    },

    // Coverage configuration
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html', 'lcov'],
      exclude: [
        'node_modules/',
        '__tests__/',
        'tests/',
        'dist/',
        'build/',
        '*.config.*',
      ],
    },

    // Watch mode configuration
    watch: false,

    // Seed for deterministic test runs
    seed: 42,

    // Isolate test environment
    isolate: true,

    // Single thread for deterministic results
    threads: false,
    singleThread: true,
  },
});
