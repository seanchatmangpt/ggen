#!/usr/bin/env node

/**
 * ggen-sparql-cli: SPARQL Query CLI
 *
 * Built with Citty - An elegant CLI builder
 * Execute 8 bleeding edge SPARQL CONSTRUCT patterns
 *
 * Usage:
 *   ggen-sparql query <pattern> [options]
 *   ggen-sparql list
 *   ggen-sparql info
 *   ggen-sparql --help
 */

import { defineCommand, runMain, showUsage } from 'citty';
import { executeQuery, getQueryInfo, listQueries } from './executor.js';
import { colorize } from './colors.js';

// Define subcommand: query <pattern>
const queryCommand = defineCommand({
  meta: {
    name: 'query',
    description: 'Execute a SPARQL CONSTRUCT query',
  },
  args: {
    pattern: {
      type: 'positional',
      description: 'Query pattern (optional|bind|filter|union|concat|values|exists|paths|all)',
      required: false,
    },
    format: {
      type: 'string',
      description: 'Output format (json|turtle|ntriples|compact)',
      default: 'compact',
    },
    limit: {
      type: 'number',
      description: 'Limit results (default: none)',
    },
    verbose: {
      type: 'boolean',
      description: 'Show detailed output',
      alias: 'v',
    },
  },
  async run({ args }) {
    const pattern = args.pattern || 'optional';

    if (pattern === 'all') {
      // Execute all patterns
      console.log(colorize('🚀 Executing all 8 SPARQL patterns...\n', 'cyan', 'bold'));

      const patterns = ['optional', 'bind', 'filter', 'union', 'concat', 'values', 'exists', 'paths'];
      let totalResults = 0;

      for (const p of patterns) {
        try {
          const result = await executeQuery(p, {
            format: args.format,
            verbose: args.verbose,
          });

          if (result.success) {
            totalResults += result.tripleCount || 0;
            console.log(
              colorize(`✅ ${p.padEnd(10)}`, 'green') +
              colorize(` ${result.tripleCount || 0} results`, 'gray')
            );
          } else {
            console.log(colorize(`❌ ${p.padEnd(10)}`, 'red') + ' ' + result.error);
          }
        } catch (err) {
          console.log(colorize(`❌ ${p.padEnd(10)}`, 'red') + ' ' + err.message);
        }
      }

      console.log(
        '\n' + colorize(`Total: ${totalResults} triples generated`, 'cyan', 'bold')
      );
      return;
    }

    // Execute single pattern
    try {
      console.log(
        colorize(`🔍 Executing pattern: ${pattern}\n`, 'cyan', 'bold')
      );

      const result = await executeQuery(pattern, {
        format: args.format,
        verbose: args.verbose,
        limit: args.limit,
      });

      if (result.success) {
        if (args.format === 'json') {
          console.log(JSON.stringify(result.data, null, 2));
        } else {
          console.log(result.data);
        }

        console.log(
          '\n' + colorize(`✅ Success: ${result.tripleCount} triples`, 'green')
        );
      } else {
        console.error(colorize(`❌ Error: ${result.error}`, 'red'));
        process.exit(1);
      }
    } catch (err) {
      console.error(colorize(`❌ Error: ${err.message}`, 'red'));
      process.exit(1);
    }
  },
});

// Define subcommand: list
const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List all available query patterns',
  },
  args: {
    verbose: {
      type: 'boolean',
      description: 'Show detailed descriptions',
      alias: 'v',
    },
  },
  run({ args }) {
    console.log(colorize('\n📋 Available SPARQL CONSTRUCT Patterns\n', 'cyan', 'bold'));

    const queries = listQueries();

    queries.forEach((q, index) => {
      console.log(colorize(`${index + 1}. ${q.name.padEnd(12)}`, 'yellow', 'bold') +
        colorize(q.title, 'white'));

      if (args.verbose) {
        console.log(colorize('   ', 'gray') + q.description);
        console.log(colorize('   Features: ', 'gray') + colorize(q.features.join(', '), 'green'));
        console.log();
      }
    });

    console.log(colorize('\nRun: ggen-sparql query <pattern>', 'gray'));
    console.log(colorize('Example: ggen-sparql query optional\n', 'gray'));
  },
});

// Define subcommand: info
const infoCommand = defineCommand({
  meta: {
    name: 'info',
    description: 'Show information about a query pattern',
  },
  args: {
    pattern: {
      type: 'positional',
      description: 'Query pattern name',
      required: true,
    },
  },
  run({ args }) {
    const info = getQueryInfo(args.pattern);

    if (!info) {
      console.error(colorize(`❌ Pattern not found: ${args.pattern}`, 'red'));
      process.exit(1);
    }

    console.log(colorize(`\n📌 Pattern: ${info.title}\n`, 'cyan', 'bold'));
    console.log(colorize('Description:', 'yellow'));
    console.log(colorize(info.description, 'white'));

    console.log(colorize('\nFeatures:', 'yellow'));
    info.features.forEach(f => {
      console.log(colorize(`  • ${f}`, 'green'));
    });

    console.log(colorize('\nBenefits:', 'yellow'));
    info.benefits.forEach(b => {
      console.log(colorize(`  • ${b}`, 'green'));
    });

    console.log(colorize('\nUse Case:', 'yellow'));
    console.log(colorize(info.useCase, 'white'));

    console.log(colorize('\nExample:', 'yellow'));
    console.log(colorize(info.example, 'white'));

    console.log();
  },
});

// Define main command with subcommands
const main = defineCommand({
  meta: {
    name: 'ggen-sparql',
    version: '1.0.0',
    description: 'SPARQL Query CLI - Execute 8 bleeding edge CONSTRUCT patterns',
  },
  subCommands: {
    query: queryCommand,
    list: listCommand,
    info: infoCommand,
  },
  args: {
    help: {
      type: 'boolean',
      description: 'Show help',
      alias: 'h',
    },
    version: {
      type: 'boolean',
      description: 'Show version',
    },
  },
  run({ args }) {
    if (args.version) {
      console.log('ggen-sparql v1.0.0');
      return;
    }

    // Show default help
    console.log(colorize('\n🚀 ggen-sparql: SPARQL Query CLI\n', 'cyan', 'bold'));
    console.log(colorize('Built with Citty - An elegant CLI builder\n', 'gray'));

    console.log(colorize('Usage:', 'yellow'));
    console.log(colorize('  ggen-sparql query <pattern> [options]', 'white'));
    console.log(colorize('  ggen-sparql list [--verbose]', 'white'));
    console.log(colorize('  ggen-sparql info <pattern>', 'white'));
    console.log();

    console.log(colorize('Patterns (8 bleeding edge CONSTRUCT features):', 'yellow'));
    console.log(colorize('  1. optional        - Safe property enrichment', 'green'));
    console.log(colorize('  2. bind            - Computed values', 'green'));
    console.log(colorize('  3. filter          - Conditional output', 'green'));
    console.log(colorize('  4. union           - Polymorphic matching', 'green'));
    console.log(colorize('  5. concat          - Aggregation', 'green'));
    console.log(colorize('  6. values          - Parameterization', 'green'));
    console.log(colorize('  7. exists          - Graph logic', 'green'));
    console.log(colorize('  8. paths           - Transitive navigation', 'green'));
    console.log();

    console.log(colorize('Options:', 'yellow'));
    console.log(colorize('  --format <type>   Output format (json|turtle|ntriples|compact)', 'white'));
    console.log(colorize('  --limit <n>       Limit results', 'white'));
    console.log(colorize('  --verbose, -v     Show detailed output', 'white'));
    console.log(colorize('  --help, -h        Show help', 'white'));
    console.log(colorize('  --version         Show version', 'white'));
    console.log();

    console.log(colorize('Examples:', 'yellow'));
    console.log(colorize('  ggen-sparql query optional', 'white'));
    console.log(colorize('  ggen-sparql query all --verbose', 'white'));
    console.log(colorize('  ggen-sparql list --verbose', 'white'));
    console.log(colorize('  ggen-sparql info bind', 'white'));
    console.log();
  },
});

// Run the CLI
runMain(main).catch((err) => {
  console.error(colorize(`\n❌ Error: ${err.message}`, 'red'));
  process.exit(1);
});
