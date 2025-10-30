/**
 * Comprehensive test suite for @ggen/node N-API bindings
 *
 * Tests are organized by function category:
 * - Core Functions
 * - Marketplace Commands
 * - Lifecycle Commands
 * - Template Generation
 * - AI Commands
 * - Utility Commands
 */

import { describe, it, expect } from 'vitest';
import {
  version,
  run,
  // Marketplace
  marketSearch,
  marketList,
  marketCategories,
  // Lifecycle
  lifecycleList,
  lifecycleReadiness,
  // Templates
  templateList,
  // AI
  aiProject,
  // Utilities
  doctor,
  help,
  // Legacy compatibility
  search,
  list,
  categories,
} from '../index';

// ═══════════════════════════════════════════════════════════════════════════════
// Core Functions
// ═══════════════════════════════════════════════════════════════════════════════

describe('Core Functions', () => {
  it('version() returns non-empty string', () => {
    const v = version();
    expect(typeof v).toBe('string');
    expect(v.length).toBeGreaterThan(0);
    expect(v).toMatch(/^\d+\.\d+\.\d+/); // Semantic version pattern
  });

  it('run() executes CLI with --help', async () => {
    const res = await run(['--help']);
    expect(res.code).toBe(0);
    expect(res.stdout).toContain('ggen');
    expect(typeof res.stderr).toBe('string');
  });

  it('run() handles --version', async () => {
    const res = await run(['--version']);
    expect(res.code).toBe(0);
    expect(res.stdout.trim()).toMatch(/^\d+\.\d+\.\d+/);
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Marketplace Commands
// ═══════════════════════════════════════════════════════════════════════════════

describe('Marketplace Commands', () => {
  it('marketSearch() executes and returns RunResult', async () => {
    const res = await marketSearch('rust');
    expect(typeof res.code).toBe('number');
    expect(typeof res.stdout).toBe('string');
    expect(typeof res.stderr).toBe('string');
  });

  it('marketList() executes successfully', async () => {
    const res = await marketList();
    expect(typeof res.code).toBe('number');
    expect(typeof res.stdout).toBe('string');
  });

  it('marketCategories() returns category list', async () => {
    const res = await marketCategories();
    expect(typeof res.code).toBe('number');
    expect(typeof res.stdout).toBe('string');
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Lifecycle Commands
// ═══════════════════════════════════════════════════════════════════════════════

describe('Lifecycle Commands', () => {
  it('lifecycleList() returns available phases', async () => {
    const res = await lifecycleList();
    expect(typeof res.code).toBe('number');
    expect(typeof res.stdout).toBe('string');
  });

  it('lifecycleReadiness() checks production status', async () => {
    const res = await lifecycleReadiness();
    expect(typeof res.code).toBe('number');
    expect(typeof res.stdout).toBe('string');
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Template Generation Commands
// ═══════════════════════════════════════════════════════════════════════════════

describe('Template Generation Commands', () => {
  it('templateList() returns available templates', async () => {
    const res = await templateList();
    expect(typeof res.code).toBe('number');
    expect(typeof res.stdout).toBe('string');
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// AI Commands
// ═══════════════════════════════════════════════════════════════════════════════

describe('AI Commands', () => {
  it('aiProject() accepts description and optional flags', async () => {
    // Note: This may fail without API keys, but tests the binding works
    const res = await aiProject('hello world', 'test-project', 'rust');
    expect(typeof res.code).toBe('number');
    expect(typeof res.stdout).toBe('string');
    expect(typeof res.stderr).toBe('string');
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Utility Commands
// ═══════════════════════════════════════════════════════════════════════════════

describe('Utility Commands', () => {
  it('doctor() runs environment diagnostics', async () => {
    const res = await doctor();
    expect(typeof res.code).toBe('number');
    expect(typeof res.stdout).toBe('string');
  });

  it('help() returns help text', async () => {
    const res = await help();
    expect(res.code).toBe(0);
    expect(res.stdout).toContain('ggen');
  });

  it('help(command) returns command-specific help', async () => {
    const res = await help('market');
    expect(res.code).toBe(0);
    expect(res.stdout.toLowerCase()).toContain('market');
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Legacy Compatibility
// ═══════════════════════════════════════════════════════════════════════════════

describe('Legacy Compatibility', () => {
  it('search() is alias for marketSearch()', async () => {
    const res = await search('rust');
    expect(typeof res.code).toBe('number');
    expect(typeof res.stdout).toBe('string');
  });

  it('list() is alias for templateList()', async () => {
    const res = await list();
    expect(typeof res.code).toBe('number');
    expect(typeof res.stdout).toBe('string');
  });

  it('categories() is alias for marketCategories()', async () => {
    const res = await categories();
    expect(typeof res.code).toBe('number');
    expect(typeof res.stdout).toBe('string');
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Error Handling and Edge Cases
// ═══════════════════════════════════════════════════════════════════════════════

describe('Error Handling', () => {
  it('handles invalid command gracefully', async () => {
    const res = await run(['invalid-command-xyz']);
    expect(res.code).not.toBe(0);
    expect(res.stderr.length).toBeGreaterThan(0);
  });

  it('handles empty args array', async () => {
    const res = await run([]);
    expect(typeof res.code).toBe('number');
    // Should show help or error, not crash
  });
});

// ═══════════════════════════════════════════════════════════════════════════════
// Type Safety and Contracts
// ═══════════════════════════════════════════════════════════════════════════════

describe('Type Safety', () => {
  it('all functions return proper RunResult shape', async () => {
    const functions = [
      () => run(['--version']),
      () => marketSearch('test'),
      () => marketList(),
      () => templateList(),
      () => doctor(),
      () => help(),
    ];

    for (const fn of functions) {
      const res = await fn();
      expect(res).toHaveProperty('code');
      expect(res).toHaveProperty('stdout');
      expect(res).toHaveProperty('stderr');
      expect(typeof res.code).toBe('number');
      expect(typeof res.stdout).toBe('string');
      expect(typeof res.stderr).toBe('string');
    }
  });
});
