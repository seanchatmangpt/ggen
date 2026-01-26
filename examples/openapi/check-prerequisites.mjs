#!/usr/bin/env node
/**
 * Pre-flight prerequisite check script (Poka-Yoke PY-001)
 * Validates all requirements before running ggen sync
 * 
 * Usage:
 *   node check-prerequisites.mjs
 *   ./check-prerequisites.mjs
 * 
 * Exit codes:
 *   0 - All prerequisites met
 *   1 - Prerequisites not met
 */

import { execSync } from 'child_process';
import { existsSync, statSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// ANSI color codes
const RED = '\x1b[31m';
const GREEN = '\x1b[32m';
const YELLOW = '\x1b[33m';
const BLUE = '\x1b[34m';
const RESET = '\x1b[0m';

let errors = 0;
let warnings = 0;

function error(message) {
  console.error(`${RED}❌ ${message}${RESET}`);
  errors++;
}

function warning(message) {
  console.warn(`${YELLOW}⚠️  ${message}${RESET}`);
  warnings++;
}

function success(message) {
  console.log(`${GREEN}✅ ${message}${RESET}`);
}

function info(message) {
  console.log(`${BLUE}ℹ️  ${message}${RESET}`);
}

console.log(`${BLUE}🔍 Checking prerequisites for OpenAPI example...${RESET}\n`);

// Check 1: ggen CLI installed and in PATH
info('Checking ggen CLI...');
try {
  const ggenPath = execSync('which ggen', { encoding: 'utf-8' }).trim();
  if (!ggenPath) {
    error('ggen CLI not found in PATH');
    console.error('   Install ggen: cargo install --path .');
    console.error('   Or add ggen to your PATH');
  } else {
    success(`ggen found at: ${ggenPath}`);
    
    // Check version
    try {
      const version = execSync('ggen --version', { encoding: 'utf-8' }).trim();
      const versionMatch = version.match(/ggen (\d+\.\d+\.\d+)/);
      if (versionMatch) {
        const [major, minor] = versionMatch[1].split('.').map(Number);
        if (major > 5 || (major === 5 && minor >= 0)) {
          success(`ggen version: ${versionMatch[1]}`);
        } else {
          warning(`ggen version ${versionMatch[1]} may be too old (recommended: 5.0.0+)`);
        }
      } else {
        success(`ggen version: ${version}`);
      }
    } catch (e) {
      warning(`Could not determine ggen version: ${e.message}`);
    }
  }
} catch (e) {
  error('ggen CLI not found in PATH');
  console.error('   Install ggen: cargo install --path .');
  console.error('   Or add ggen to your PATH');
}

// Check 2: Node.js version ≥ 18
info('Checking Node.js version...');
try {
  const nodeVersion = process.version;
  const versionMatch = nodeVersion.match(/v(\d+)\./);
  if (versionMatch) {
    const majorVersion = parseInt(versionMatch[1], 10);
    if (majorVersion >= 18) {
      success(`Node.js version: ${nodeVersion} (required: 18+)`);
    } else {
      error(`Node.js version ${nodeVersion} is too old (required: 18+)`);
      console.error('   Update Node.js: https://nodejs.org/');
    }
  } else {
    warning(`Could not parse Node.js version: ${nodeVersion}`);
  }
} catch (e) {
  error(`Could not determine Node.js version: ${e.message}`);
}

// Check 3: Directory structure
info('Checking directory structure...');
const requiredDirs = [
  'ontology',
  'templates',
  'golden',
];
const requiredFiles = [
  'ggen.toml',
  'ontology/blog-api.ttl',
  'ontology/api-schema.ttl',
];

let dirErrors = 0;
for (const dir of requiredDirs) {
  const dirPath = join(__dirname, dir);
  if (existsSync(dirPath) && statSync(dirPath).isDirectory()) {
    success(`Directory exists: ${dir}/`);
  } else {
    error(`Required directory missing: ${dir}/`);
    dirErrors++;
  }
}

for (const file of requiredFiles) {
  const filePath = join(__dirname, file);
  if (existsSync(filePath) && statSync(filePath).isFile()) {
    success(`File exists: ${file}`);
  } else {
    error(`Required file missing: ${file}`);
    dirErrors++;
  }
}

if (dirErrors === 0) {
  success('Directory structure is correct');
}

// Check 4: Validate ontology files are valid Turtle (basic check)
info('Validating ontology files...');
try {
  const blogApiPath = join(__dirname, 'ontology', 'blog-api.ttl');
  const apiSchemaPath = join(__dirname, 'ontology', 'api-schema.ttl');
  
  if (existsSync(blogApiPath)) {
    const blogApiContent = await import('fs/promises').then(fs => fs.readFile(blogApiPath, 'utf-8'));
    if (blogApiContent.includes('@prefix') || blogApiContent.includes('PREFIX')) {
      success('blog-api.ttl appears to be valid Turtle');
    } else {
      warning('blog-api.ttl may not be valid Turtle (missing @prefix or PREFIX)');
    }
  }
  
  if (existsSync(apiSchemaPath)) {
    const apiSchemaContent = await import('fs/promises').then(fs => fs.readFile(apiSchemaPath, 'utf-8'));
    if (apiSchemaContent.includes('@prefix') || apiSchemaContent.includes('PREFIX')) {
      success('api-schema.ttl appears to be valid Turtle');
    } else {
      warning('api-schema.ttl may not be valid Turtle (missing @prefix or PREFIX)');
    }
  }
} catch (e) {
  warning(`Could not validate ontology files: ${e.message}`);
}

// Check 5: Check if we're in the right directory
info('Checking working directory...');
const expectedFiles = ['ggen.toml', 'README.md', 'ontology', 'templates'];
const allPresent = expectedFiles.every(file => existsSync(join(__dirname, file)));

if (allPresent) {
  success(`Working directory is correct: ${__dirname}`);
} else {
  error('Not in the correct directory (examples/openapi/)');
  console.error('   Run this script from: examples/openapi/');
}

// Check 6: Check if lib/ exists (PY-006: Warning for existing lib/)
if (existsSync(join(__dirname, 'lib'))) {
  warning('lib/ directory already exists');
  console.warn('   Consider deleting it for a fresh generation: rm -rf lib/');
  console.warn('   Existing files may be overwritten by ggen sync');
}

// Summary
console.log('\n' + '='.repeat(60));
if (errors === 0 && warnings === 0) {
  console.log(`${GREEN}✅ All prerequisites met!${RESET}`);
  console.log('   You can now run: ggen sync');
  process.exit(0);
} else if (errors === 0) {
  console.log(`${YELLOW}⚠️  Prerequisites met with warnings${RESET}`);
  console.log(`   ${warnings} warning(s) - review above`);
  console.log('   You can proceed, but review warnings first');
  process.exit(0);
} else {
  console.log(`${RED}❌ Prerequisites not met${RESET}`);
  console.log(`   ${errors} error(s) must be fixed before proceeding`);
  console.log(`   ${warnings} warning(s) - review above`);
  process.exit(1);
}

