#!/usr/bin/env node

/**
 * @fileoverview Golden Results Comparison Script
 * Compares generated output from ggen sync against golden reference files
 *
 * Usage:
 *   node scripts/compare-golden.mjs                    # Compare all files
 *   node scripts/compare-golden.mjs --update           # Update golden files from generated
 *   node scripts/compare-golden.mjs --file tasks.mjs   # Compare specific file
 *
 * Exit codes:
 *   0 - All files match
 *   1 - Differences found
 *   2 - Error (missing files, etc.)
 */

import { readFileSync, readdirSync, statSync, existsSync, writeFileSync, mkdirSync } from 'node:fs';
import { join, relative, dirname } from 'node:path';
import { diffLines } from 'diff';

// =============================================================================
// Configuration
// =============================================================================

const GOLDEN_DIR = 'golden';
const GENERATED_DIR = '.';  // Generated files go to root of example

const COMPARISON_FILES = [
  // YAWL XML Specifications
  'generated/workflows/cicd-pipeline.yawl',
  'generated/workflows/document-approval.yawl',
  'generated/workflows/hr-onboarding.yawl',
  'generated/workflows/incident-management.yawl',
  'generated/workflows/order-processing.yawl',
  // ESM Definitions
  'lib/definitions/tasks.mjs',
  'lib/definitions/flows.mjs',
  'lib/definitions/index.mjs',
  // Types
  'lib/types/cases.mjs',
  'lib/types/workitems.mjs',
  'lib/types/receipts.mjs',
  // API & Engine
  'lib/api/routes.mjs',
  'lib/engine/WorkflowEngine.mjs',
  // Worklets
  'lib/worklets/index.mjs',
  // Tests
  'lib/tests/workflow.test.mjs',
  // Entry Point
  'lib/main.mjs',
  // Package
  'package.json',
];

// =============================================================================
// Argument Parsing
// =============================================================================

const args = process.argv.slice(2);
const updateMode = args.includes('--update');
const fileFilter = args.find(a => a.startsWith('--file='))?.split('=')[1];
const verbose = args.includes('--verbose') || args.includes('-v');

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Read file contents safely
 * @param {string} path
 * @returns {string|null}
 */
function readFileSafe(path) {
  try {
    return readFileSync(path, 'utf-8');
  } catch (err) {
    if (err.code === 'ENOENT') return null;
    throw err;
  }
}

/**
 * Normalize line endings and trailing whitespace
 * @param {string} content
 * @returns {string}
 */
function normalize(content) {
  return content
    .replace(/\r\n/g, '\n')           // Normalize CRLF to LF
    .replace(/[ \t]+$/gm, '')          // Remove trailing whitespace
    .replace(/\n+$/, '\n');            // Single trailing newline
}

/**
 * Generate colorized diff output
 * @param {string} filename
 * @param {string} golden
 * @param {string} generated
 * @returns {string}
 */
function generateDiff(filename, golden, generated) {
  const diff = diffLines(golden, generated);
  let output = `\n=== ${filename} ===\n`;
  let lineNum = 1;

  for (const part of diff) {
    const lines = part.value.split('\n').filter(l => l !== '');
    const prefix = part.added ? '+' : part.removed ? '-' : ' ';
    const color = part.added ? '\x1b[32m' : part.removed ? '\x1b[31m' : '\x1b[0m';

    for (const line of lines) {
      if (part.added || part.removed) {
        output += `${color}${prefix} ${lineNum.toString().padStart(4)}: ${line}\x1b[0m\n`;
      }
      if (!part.removed) lineNum++;
    }
  }

  return output;
}

// =============================================================================
// Comparison Logic
// =============================================================================

/**
 * Compare a single file
 * @param {string} relativePath
 * @returns {{ match: boolean, golden: string|null, generated: string|null, diff: string|null }}
 */
function compareFile(relativePath) {
  const goldenPath = join(GOLDEN_DIR, relativePath);
  const generatedPath = join(GENERATED_DIR, relativePath);

  const goldenContent = readFileSafe(goldenPath);
  const generatedContent = readFileSafe(generatedPath);

  if (goldenContent === null && generatedContent === null) {
    return { match: true, golden: null, generated: null, diff: null };
  }

  if (goldenContent === null) {
    return {
      match: false,
      golden: null,
      generated: generatedContent,
      diff: `Golden file missing: ${goldenPath}`,
    };
  }

  if (generatedContent === null) {
    return {
      match: false,
      golden: goldenContent,
      generated: null,
      diff: `Generated file missing: ${generatedPath}`,
    };
  }

  const normalizedGolden = normalize(goldenContent);
  const normalizedGenerated = normalize(generatedContent);

  if (normalizedGolden === normalizedGenerated) {
    return { match: true, golden: goldenContent, generated: generatedContent, diff: null };
  }

  return {
    match: false,
    golden: goldenContent,
    generated: generatedContent,
    diff: generateDiff(relativePath, normalizedGolden, normalizedGenerated),
  };
}

/**
 * Update golden file from generated
 * @param {string} relativePath
 */
function updateGoldenFile(relativePath) {
  const goldenPath = join(GOLDEN_DIR, relativePath);
  const generatedPath = join(GENERATED_DIR, relativePath);

  const generatedContent = readFileSafe(generatedPath);
  if (generatedContent === null) {
    console.error(`Cannot update: generated file missing: ${generatedPath}`);
    return false;
  }

  const dir = dirname(goldenPath);
  if (!existsSync(dir)) {
    mkdirSync(dir, { recursive: true });
  }

  writeFileSync(goldenPath, generatedContent);
  console.log(`Updated: ${goldenPath}`);
  return true;
}

// =============================================================================
// Main Execution
// =============================================================================

function main() {
  console.log('YAWL Golden Results Comparison');
  console.log('==============================\n');

  const filesToCompare = fileFilter
    ? COMPARISON_FILES.filter(f => f.includes(fileFilter))
    : COMPARISON_FILES;

  if (filesToCompare.length === 0) {
    console.error(`No files match filter: ${fileFilter}`);
    process.exit(2);
  }

  if (updateMode) {
    console.log('Update Mode: Copying generated files to golden directory\n');
    let updated = 0;
    for (const file of filesToCompare) {
      if (updateGoldenFile(file)) updated++;
    }
    console.log(`\nUpdated ${updated} golden files.`);
    process.exit(0);
  }

  // Comparison mode
  let passed = 0;
  let failed = 0;
  let missing = 0;
  const diffs = [];

  for (const file of filesToCompare) {
    const result = compareFile(file);

    if (result.match) {
      passed++;
      if (verbose) console.log(`✓ ${file}`);
    } else {
      if (result.golden === null || result.generated === null) {
        missing++;
        console.log(`⚠ ${file} - MISSING`);
      } else {
        failed++;
        console.log(`✗ ${file} - DIFFERS`);
      }
      if (result.diff) diffs.push(result.diff);
    }
  }

  // Summary
  console.log('\n==============================');
  console.log(`Results: ${passed} passed, ${failed} failed, ${missing} missing`);
  console.log(`Total: ${filesToCompare.length} files compared`);

  // Show diffs if any
  if (diffs.length > 0 && !args.includes('--no-diff')) {
    console.log('\nDifferences:');
    for (const diff of diffs) {
      console.log(diff);
    }
  }

  // Exit code
  if (failed > 0 || missing > 0) {
    console.log('\n❌ Golden comparison FAILED');
    console.log('Run with --update to update golden files from generated output.');
    process.exit(1);
  }

  console.log('\n✓ All golden comparisons passed!');
  process.exit(0);
}

main();
