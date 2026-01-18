#!/usr/bin/env node
/**
 * Validation script for OpenAPI example
 * Compares generated output in lib/ with golden files in golden/lib/
 * 
 * Usage:
 *   node validate.mjs
 *   ./validate.mjs
 * 
 * Exit codes:
 *   0 - All files match
 *   1 - Mismatches found
 */

import { readdir, readFile, stat } from 'fs/promises';
import { join, relative } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = new URL('.', import.meta.url).pathname;

const SCRIPT_DIR = __dirname.replace(/\/$/, '');
const GENERATED_DIR = join(SCRIPT_DIR, 'lib');
const GOLDEN_DIR = join(SCRIPT_DIR, 'golden', 'lib');

// ANSI color codes
const RED = '\x1b[31m';
const GREEN = '\x1b[32m';
const YELLOW = '\x1b[33m';
const RESET = '\x1b[0m';

/**
 * Recursively get all files in a directory
 */
async function getAllFiles(dir, baseDir = dir) {
  const files = [];
  try {
    const entries = await readdir(dir, { withFileTypes: true });
    
    for (const entry of entries) {
      const fullPath = join(dir, entry.name);
      const relPath = relative(baseDir, fullPath);
      
      if (entry.isDirectory()) {
        const subFiles = await getAllFiles(fullPath, baseDir);
        files.push(...subFiles);
      } else {
        files.push({ fullPath, relPath });
      }
    }
  } catch (error) {
    if (error.code !== 'ENOENT') {
      throw error;
    }
  }
  
  return files.sort((a, b) => a.relPath.localeCompare(b.relPath));
}

/**
 * Compare two files
 */
async function compareFiles(generatedPath, goldenPath, relPath) {
  try {
    const [generatedContent, goldenContent] = await Promise.all([
      readFile(generatedPath, 'utf-8'),
      readFile(goldenPath, 'utf-8')
    ]);
    
    if (generatedContent === goldenContent) {
      console.log(`${GREEN}✅ Match:${RESET} ${relPath}`);
      return { match: true, relPath };
    } else {
      console.log(`${RED}❌ Mismatch:${RESET} ${relPath}`);
      console.log(`   Run: diff ${generatedPath} ${goldenPath}`);
      return { match: false, relPath, generatedPath, goldenPath };
    }
  } catch (error) {
    console.error(`${RED}❌ Error comparing ${relPath}:${RESET} ${error.message}`);
    return { match: false, relPath, error: error.message };
  }
}

/**
 * Main validation function
 */
async function validate() {
  console.log('🔍 Validating generated output against golden files...\n');
  
  // Check if directories exist
  try {
    await stat(GENERATED_DIR);
  } catch (error) {
    console.error(`${RED}❌ Error: Generated directory not found: ${GENERATED_DIR}${RESET}`);
    console.error('   Run "ggen sync" first to generate output');
    process.exit(1);
  }
  
  try {
    await stat(GOLDEN_DIR);
  } catch (error) {
    console.error(`${RED}❌ Error: Golden directory not found: ${GOLDEN_DIR}${RESET}`);
    process.exit(1);
  }
  
  // Get all files from both directories
  const [generatedFiles, goldenFiles] = await Promise.all([
    getAllFiles(GENERATED_DIR),
    getAllFiles(GOLDEN_DIR)
  ]);
  
  const generatedMap = new Map(generatedFiles.map(f => [f.relPath, f]));
  const goldenMap = new Map(goldenFiles.map(f => [f.relPath, f]));
  
  const results = {
    matches: [],
    mismatches: [],
    missing: [],
    extra: []
  };
  
  // Compare files that exist in both
  for (const [relPath, goldenFile] of goldenMap) {
    const generatedFile = generatedMap.get(relPath);
    
    if (!generatedFile) {
      results.missing.push(relPath);
      console.log(`${YELLOW}⚠️  Missing in generated:${RESET} ${relPath}`);
      continue;
    }
    
    const result = await compareFiles(
      generatedFile.fullPath,
      goldenFile.fullPath,
      relPath
    );
    
    if (result.match) {
      results.matches.push(relPath);
    } else {
      results.mismatches.push(result);
    }
  }
  
  // Check for extra files in generated
  for (const [relPath, generatedFile] of generatedMap) {
    if (!goldenMap.has(relPath)) {
      results.extra.push(relPath);
      console.log(`${YELLOW}⚠️  Extra in generated (not in golden):${RESET} ${relPath}`);
    }
  }
  
  // Summary
  console.log('\n' + '='.repeat(60));
  console.log('Summary:');
  console.log(`  ${GREEN}✅ Matches:${RESET} ${results.matches.length}`);
  console.log(`  ${RED}❌ Mismatches:${RESET} ${results.mismatches.length}`);
  console.log(`  ${YELLOW}⚠️  Missing:${RESET} ${results.missing.length}`);
  console.log(`  ${YELLOW}⚠️  Extra:${RESET} ${results.extra.length}`);
  console.log('='.repeat(60));
  
  // Exit with appropriate code
  if (results.mismatches.length > 0 || results.missing.length > 0) {
    console.log(`\n${RED}❌ Validation failed${RESET}`);
    if (results.mismatches.length > 0) {
      console.log('\nTo see differences, run:');
      results.mismatches.forEach(m => {
        if (m.generatedPath && m.goldenPath) {
          console.log(`  diff ${m.generatedPath} ${m.goldenPath}`);
        }
      });
    }
    process.exit(1);
  } else {
    console.log(`\n${GREEN}✅ All files match golden files!${RESET}`);
    process.exit(0);
  }
}

// Run validation
validate().catch(error => {
  console.error(`${RED}❌ Fatal error:${RESET}`, error);
  process.exit(1);
});

