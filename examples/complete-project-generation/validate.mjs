#!/usr/bin/env node
import fs from 'fs';

const GREEN = '\x1b[32m';
const RED = '\x1b[31m';
const NC = '\x1b[0m';

let passed = 0, failed = 0;

function check(name, condition) {
    if (condition) {
        console.log(`${GREEN}✓${NC} ${name}`);
        passed++;
    } else {
        console.log(`${RED}✗${NC} ${name}`);
        failed++;
    }
}

console.log('Validating complete-project-generation...\n');

check('ggen.toml exists', fs.existsSync('./ggen.toml'));
check('ontology/workspace.ttl exists', fs.existsSync('./ontology/workspace.ttl'));
check('templates/ directory exists', fs.existsSync('./templates'));
check('golden/ directory exists', fs.existsSync('./golden'));
check('README.md exists', fs.existsSync('./README.md'));

const tomlContent = fs.readFileSync('./ggen.toml', 'utf8');
check('ggen.toml has 10 rules', (tomlContent.match(/\[\[generation\.rules\]\]/g) || []).length >= 10);

const readmeContent = fs.readFileSync('./README.md', 'utf8');
check('README has 9 sections', (readmeContent.match(/^##\s/gm) || []).length >= 8);

console.log(`\nResult: ${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
