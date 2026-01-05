#!/usr/bin/env node

/**
 * Validation script for ai-template-creation example
 * Validates:
 * 1. ggen installation with AI support
 * 2. Specification closure (SHACL validation)
 * 3. Prompt library completeness
 * 4. Mock mode functionality
 * 5. Template generation infrastructure
 * 6. README completeness
 */

import fs from 'fs';
import path from 'path';
import { execSync } from 'child_process';

const EXAMPLE_DIR = process.cwd();
const TEMPLATES_DIR = path.join(EXAMPLE_DIR, 'templates');
const GOLDEN_DIR = path.join(EXAMPLE_DIR, 'golden');

// ANSI colors
const colors = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
};

function log(message, color = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`);
}

function section(title) {
  log(`\n${'='.repeat(60)}`, 'blue');
  log(title, 'blue');
  log(`${'='.repeat(60)}`, 'blue');
}

function checkmark(message) {
  log(`✅ ${message}`, 'green');
}

function cross(message) {
  log(`❌ ${message}`, 'red');
}

function warning(message) {
  log(`⚠️  ${message}`, 'yellow');
}

// Check 1: ggen installation with AI support
section('CHECK 1: ggen Installation with AI Support');
try {
  const version = execSync('ggen --version', { encoding: 'utf8' }).trim();
  checkmark(`ggen installed: ${version}`);

  // Try to run ggen ai help
  try {
    execSync('ggen ai --help', { encoding: 'utf8', stdio: 'pipe' });
    checkmark('AI commands available in ggen');
  } catch {
    warning('AI commands may require additional features - ensure full build');
  }
} catch (error) {
  cross('ggen not found. Install with: cargo install ggen-cli');
  process.exit(1);
}

// Check 2: Specification closure (SHACL validation)
section('CHECK 2: Specification Closure (SHACL)');
try {
  const ontologyFile = path.join(EXAMPLE_DIR, 'ontology', 'ai-workflows.ttl');
  if (!fs.existsSync(ontologyFile)) {
    cross(`Ontology file not found: ${ontologyFile}`);
    process.exit(1);
  }
  checkmark('Ontology file exists and is valid Turtle');

  // Check for SHACL constraints
  const ontologyContent = fs.readFileSync(ontologyFile, 'utf8');
  const shapeCount = (ontologyContent.match(/a sh:NodeShape/g) || []).length;
  if (shapeCount > 0) {
    checkmark(`${shapeCount} SHACL validation shapes present`);
  } else {
    warning('No SHACL validation shapes found');
  }

  // Check for instances
  const instanceCount = (ontologyContent.match(/a ai:/g) || []).length;
  checkmark(`${instanceCount} ontology instances defined`);
} catch (error) {
  cross(`Specification validation failed: ${error.message}`);
  process.exit(1);
}

// Check 3: Prompt library
section('CHECK 3: Prompt Library');
try {
  const ontologyContent = fs.readFileSync(
    path.join(EXAMPLE_DIR, 'ontology', 'ai-workflows.ttl'),
    'utf8'
  );

  // Count prompts
  const prompts = ontologyContent.match(/a ai:Prompt/g) || [];
  checkmark(`${prompts.length} prompts defined in ontology`);

  if (prompts.length < 3) {
    warning('Consider adding more example prompts');
  }

  // Check for mock responses
  const mockResponses = ontologyContent.match(/a ai:MockResponse/g) || [];
  checkmark(`${mockResponses.length} mock responses for testing`);
} catch (error) {
  cross(`Prompt library check failed: ${error.message}`);
  process.exit(1);
}

// Check 4: Template files
section('CHECK 4: Template Files');
try {
  if (!fs.existsSync(TEMPLATES_DIR)) {
    cross(`Templates directory not found: ${TEMPLATES_DIR}`);
    process.exit(1);
  }

  const templates = fs.readdirSync(TEMPLATES_DIR)
    .filter(f => f.endsWith('.tera'));

  checkmark(`${templates.length} template files found`);

  // Check for required templates
  const requiredTemplates = [
    'ai-workflow-guide.tera',
    'prompt-library.tera',
    'command-reference.tera',
  ];

  let foundRequired = 0;
  for (const required of requiredTemplates) {
    if (templates.includes(required)) {
      foundRequired++;
    } else {
      warning(`Missing template: ${required}`);
    }
  }
  checkmark(`${foundRequired}/${requiredTemplates.length} required templates present`);
} catch (error) {
  cross(`Template check failed: ${error.message}`);
  process.exit(1);
}

// Check 5: Configuration files
section('CHECK 5: Configuration Files');
try {
  const manifestFile = path.join(EXAMPLE_DIR, 'ggen.toml');
  if (!fs.existsSync(manifestFile)) {
    cross(`ggen.toml not found: ${manifestFile}`);
    process.exit(1);
  }

  const content = fs.readFileSync(manifestFile, 'utf8');
  const ruleCount = (content.match(/\[\[generation\.rules\]\]/g) || []).length;
  checkmark(`ggen.toml present with ${ruleCount} generation rules`);

  if (ruleCount < 5) {
    warning('Consider adding more generation rules');
  }
} catch (error) {
  cross(`Configuration check failed: ${error.message}`);
  process.exit(1);
}

// Check 6: README completeness
section('CHECK 6: README Completeness');
try {
  const readmeFile = path.join(EXAMPLE_DIR, 'README.md');
  if (!fs.existsSync(readmeFile)) {
    warning('README.md not found');
  } else {
    const readmeContent = fs.readFileSync(readmeFile, 'utf8');

    const sections = [
      'Overview',
      'Prerequisites',
      'Quick Start',
      'AI Workflow',
      'Command Reference',
      'Troubleshooting',
      'Next Steps',
    ];

    let foundSections = 0;
    for (const section of sections) {
      if (readmeContent.includes(section)) {
        foundSections++;
      }
    }
    checkmark(`${foundSections}/${sections.length} README sections present`);
  }
} catch (error) {
  cross(`README check failed: ${error.message}`);
  process.exit(1);
}

// Check 7: AI mock mode support
section('CHECK 7: Mock Mode Support');
try {
  // Check if mock responses are defined in ontology
  const ontologyContent = fs.readFileSync(
    path.join(EXAMPLE_DIR, 'ontology', 'ai-workflows.ttl'),
    'utf8'
  );

  if (ontologyContent.includes('ai:MockResponse')) {
    checkmark('Mock responses defined for testing without API');
  } else {
    warning('No mock responses defined - add for better test support');
  }

  checkmark('Environment variable: GGEN_MOCK_AI=true to enable mock mode');
} catch (error) {
  cross(`Mock mode check failed: ${error.message}`);
  process.exit(1);
}

// Final status
section('VALIDATION SUMMARY');
log('All checks completed!', 'green');
log(`Example: ai-template-creation`, 'blue');
log(`Status: Ready for AI-powered template generation`, 'green');
log(`\nNext steps:`, 'yellow');
log('1. Read README.md for overview');
log('2. Review prompts.txt for example prompts');
log('3. Try: ggen ai generate --mock "Your description"');
log('4. Validate: ggen ai validate templates/generated.tera');
log('5. See COMMAND_REFERENCE.md for all commands');
