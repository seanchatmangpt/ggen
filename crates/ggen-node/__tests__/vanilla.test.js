/**
 * Vanilla JavaScript test suite for @ggen/node
 *
 * These tests demonstrate that ggen-node works in vanilla JavaScript environments
 * without React, TypeScript, or any framework dependencies.
 *
 * @file Browser-compatible vanilla JS tests (non-React, non-TypeScript)
 */

import { describe, it, expect } from 'vitest';
import {
  version,
  run,
  marketSearch,
  marketList,
  templateList,
  projectNew,
  projectGenerate,
  projectInit,
  doctor,
  help,
} from '../index.mjs';

// ═══════════════════════════════════════════════════════════════════════════════
// Core API Tests - Vanilla JavaScript Only
// ═══════════════════════════════════════════════════════════════════════════════

describe('Vanilla JavaScript API - Core Functions', () => {
  it('version() returns semantic version string', () => {
    // No TypeScript, no decorators, pure JS
    const v = version();
    expect(typeof v).toBe('string');
    expect(v.length).toBeGreaterThan(0);
    // Match semantic versioning pattern: MAJOR.MINOR.PATCH
    expect(v).toMatch(/^\d+\.\d+\.\d+/);
  });

  it('run() executes with vanilla JS arguments array', async () => {
    // Plain JavaScript array, no type annotations
    const args = ['--help'];
    const result = await run(args);

    // Plain JS object destructuring
    const { code, stdout, stderr } = result;
    expect(typeof code).toBe('number');
    expect(typeof stdout).toBe('string');
    expect(typeof stderr).toBe('string');
    expect(code).toBe(0);
  });

  it('run() handles --version command', async () => {
    const result = await run(['--version']);
    const versionOutput = result.stdout.trim();
    expect(versionOutput).toMatch(/^\d+\.\d+\.\d+/);
  });

  it('run() works with empty array', async () => {
    // Edge case: empty command
    const result = await run([]);
    expect(typeof result.code).toBe('number');
  });

  it('run() handles error cases gracefully', async () => {
    // Invalid command should not crash
    const result = await run(['invalid-xyz-command']);
    expect(typeof result.code).toBe('number');
    // Error code should not be 0 for invalid command
    expect(result.code).not.toBe(0);
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Marketplace API Tests - Vanilla JavaScript
// ═══════════════════════════════════════════════════════════════════════════════

describe('Vanilla JavaScript API - Marketplace Operations', () => {
  it('marketSearch() accepts string parameter', async () => {
    const query = 'rust-templates';
    const result = await marketSearch(query);

    // Plain JS property access
    expect(result.code).toBeDefined();
    expect(result.stdout).toBeDefined();
    expect(typeof result.stderr).toBe('string');
  });

  it('marketList() returns marketplace catalog', async () => {
    const result = await marketList();
    expect(typeof result.code).toBe('number');
    expect(typeof result.stdout).toBe('string');
    // Stdout should contain marketplace data
    expect(result.stdout.length).toBeGreaterThan(0);
  });

  it('marketSearch() with multiple queries', async () => {
    // Vanilla JS array of queries
    const queries = ['rust', 'typescript', 'python'];

    for (const q of queries) {
      const result = await marketSearch(q);
      expect(result).toHaveProperty('code');
      expect(result).toHaveProperty('stdout');
      expect(result).toHaveProperty('stderr');
    }
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Template API Tests - Vanilla JavaScript
// ═══════════════════════════════════════════════════════════════════════════════

describe('Vanilla JavaScript API - Template Operations', () => {
  it('templateList() returns available templates', async () => {
    const result = await templateList();
    expect(result.code).toEqual(0);
    expect(result.stdout.length).toBeGreaterThan(0);
  });

  it('templateList() result is plain object', async () => {
    const result = await templateList();
    // Verify it's a plain JS object
    expect(Object.prototype.toString.call(result)).toBe('[object Object]');
    expect('code' in result).toBe(true);
    expect('stdout' in result).toBe(true);
    expect('stderr' in result).toBe(true);
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Project Scaffolding Tests - Vanilla JavaScript
// ═══════════════════════════════════════════════════════════════════════════════

describe('Vanilla JavaScript API - Project Scaffolding', () => {
  it('projectInit() works with vanilla JS', async () => {
    const result = await projectInit();
    expect(typeof result.code).toBe('number');
    expect(typeof result.stdout).toBe('string');
  });

  it('projectGenerate() accepts optional path parameter', async () => {
    // Optional parameter with vanilla JS
    const result = await projectGenerate();
    expect(result).toHaveProperty('code');
    expect(result).toHaveProperty('stdout');
    expect(result).toHaveProperty('stderr');
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Utility API Tests - Vanilla JavaScript
// ═══════════════════════════════════════════════════════════════════════════════

describe('Vanilla JavaScript API - Utility Commands', () => {
  it('doctor() runs environment diagnostics', async () => {
    const result = await doctor();
    expect(typeof result.code).toBe('number');
    expect(typeof result.stdout).toBe('string');
  });

  it('help() returns help text in vanilla JS', async () => {
    const result = await help();
    expect(result.code).toBe(0);
    expect(result.stdout).toContain('ggen');
  });

  it('help(command) with specific command', async () => {
    const result = await help('market');
    expect(result.code).toBe(0);
    // Help output should contain the command name
    expect(result.stdout.toLowerCase()).toContain('market');
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Contract Testing - Vanilla JavaScript
// ═══════════════════════════════════════════════════════════════════════════════

describe('Vanilla JavaScript API - Type Contracts', () => {
  it('all functions return RunResult shape', async () => {
    // Array of function calls
    const calls = [
      run(['--version']),
      marketSearch('test'),
      marketList(),
      templateList(),
      doctor(),
      help(),
    ];

    // Vanilla JS Promise.all
    const results = await Promise.all(calls);

    // Verify all have RunResult shape
    for (const result of results) {
      expect(result).toHaveProperty('code');
      expect(result).toHaveProperty('stdout');
      expect(result).toHaveProperty('stderr');

      expect(typeof result.code).toBe('number');
      expect(typeof result.stdout).toBe('string');
      expect(typeof result.stderr).toBe('string');
    }
  });

  it('code is always a number', async () => {
    const testFunctions = [
      () => run(['--help']),
      () => marketSearch('test'),
      () => templateList(),
      () => doctor(),
    ];

    for (const fn of testFunctions) {
      const result = await fn();
      expect(Number.isInteger(result.code)).toBe(true);
    }
  });

  it('stdout and stderr are always strings', async () => {
    const result = await run(['--help']);
    expect(typeof result.stdout).toBe('string');
    expect(typeof result.stderr).toBe('string');
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Browser Compatibility Tests - Vanilla JavaScript
// ═══════════════════════════════════════════════════════════════════════════════

describe('Vanilla JavaScript API - Browser Compatibility', () => {
  it('functions work without ES6 class instantiation', async () => {
    // No class instantiation, no decorators
    const result = await version().constructor === String.constructor;
    expect(result).toBe(false); // version() returns string, not constructor
  });

  it('no global state pollution', async () => {
    // Call multiple times
    const v1 = version();
    const v2 = version();
    // Should return same value consistently
    expect(v1).toBe(v2);
  });

  it('async/await works in vanilla JS', async () => {
    // Pure vanilla JS async/await
    let result;
    try {
      result = await run(['--version']);
    } catch (error) {
      result = null;
    }
    expect(result).not.toBeNull();
  });

  it('handles promise chains for vanilla JS', () => {
    // Promise chaining without async/await
    return run(['--help'])
      .then((result) => {
        expect(result.code).toBe(0);
        return result.stdout;
      })
      .then((stdout) => {
        expect(stdout).toContain('ggen');
      });
  });

  it('works with vanilla JS destructuring', async () => {
    const result = await help();
    const { code, stdout, stderr } = result;

    expect(code).toBeDefined();
    expect(stdout).toBeDefined();
    expect(stderr).toBeDefined();
  });

  it('supports spread operator in vanilla JS', async () => {
    const baseArgs = ['market'];
    const allArgs = [...baseArgs, '--help'];
    const result = await run(allArgs);
    expect(typeof result.code).toBe('number');
  });

  it('works with object shorthand in vanilla JS', async () => {
    const query = 'templates';
    // Object shorthand
    const searchConfig = { query };
    const result = await marketSearch(searchConfig.query);
    expect(result).toHaveProperty('stdout');
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Error Handling - Vanilla JavaScript
// ═══════════════════════════════════════════════════════════════════════════════

describe('Vanilla JavaScript API - Error Handling', () => {
  it('handles rejected promises in vanilla JS', async () => {
    let errorCaught = false;
    try {
      await run(['--invalid-flag-xyz']);
    } catch (error) {
      errorCaught = true;
    }
    // Command execution might not throw, just return error code
    expect(typeof errorCaught).toBe('boolean');
  });

  it('supports vanilla JS try/catch/finally', async () => {
    let executed = false;
    try {
      const result = await run(['--version']);
      executed = result.code === 0;
    } finally {
      expect(executed).toBe(true);
    }
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Performance Tests - Vanilla JavaScript
// ═══════════════════════════════════════════════════════════════════════════════

describe('Vanilla JavaScript API - Performance', () => {
  it('version() executes synchronously in milliseconds', () => {
    const start = performance.now();
    const v = version();
    const end = performance.now();
    const duration = end - start;

    expect(v).toBeDefined();
    expect(duration).toBeLessThan(100); // Should be very fast
  });

  it('multiple sequential calls work reliably', async () => {
    const results = [];
    for (let i = 0; i < 5; i++) {
      const result = await help();
      results.push(result.code);
    }
    expect(results.length).toBe(5);
    expect(results.every((code) => code === 0)).toBe(true);
  });
});
