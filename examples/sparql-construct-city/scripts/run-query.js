#!/usr/bin/env node

/**
 * Execute a specific SPARQL CONSTRUCT query
 * Usage: node run-query.js <query-number>
 * Example: node run-query.js 1
 */

import { readFileSync } from 'fs';
import { join } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = __filename.substring(0, __filename.lastIndexOf('/'));

const QUERIES_FILE = join(__dirname, '../queries.sparql');
const queryNumber = parseInt(process.argv[2], 10);

if (isNaN(queryNumber) || queryNumber < 1 || queryNumber > 8) {
  console.error('❌ Usage: node run-query.js <1-8>');
  console.error('Example: node run-query.js 1');
  process.exit(1);
}

try {
  const content = readFileSync(QUERIES_FILE, 'utf8');
  const queries = content
    .split(/^---\s*$/m)
    .map(q => q.trim())
    .filter(q => q && q.includes('CONSTRUCT'));

  if (queryNumber > queries.length) {
    console.error(`❌ Query ${queryNumber} not found. Available: 1-${queries.length}`);
    process.exit(1);
  }

  const query = queries[queryNumber - 1];
  const lines = query.split('\n');

  // Extract pattern name from comments
  const commentLine = lines.find(l => l.startsWith('# Query'));
  const patternName = commentLine ? commentLine.replace(/^# Query \d+: /, '') : `Query ${queryNumber}`;

  console.log(`\n📋 Pattern ${queryNumber}: ${patternName}\n`);
  console.log('═'.repeat(80));
  console.log(query);
  console.log('═'.repeat(80));
  console.log(`\n✅ Query extracted successfully`);
  console.log(`\n💡 To execute this query:`);
  console.log(`   1. With Oxigraph: oxigraph query --file data.ttl`);
  console.log(`   2. With RDFox: sparql --file data.ttl --query <query>`);
  console.log(`   3. With SPARQL endpoint: POST to /sparql with query body`);
  console.log();

} catch (error) {
  console.error(`❌ Error: ${error.message}`);
  process.exit(1);
}
