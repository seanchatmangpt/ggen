# ggen-sparql-cli: SPARQL Query CLI with Citty

A practical CLI tool demonstrating how to build elegant command-line interfaces with **Citty** - the lightweight CLI framework by unjs.

Execute 8 bleeding edge SPARQL CONSTRUCT patterns for feature innovation, graph enrichment, and data transformation.

## What is Citty?

**Citty** is an elegant CLI builder from unjs featuring:
- ✨ Fast argument parsing (based on mri)
- 🔄 Async/await support
- 📚 Automatic help generation
- 🎯 Subcommand support
- 🔌 Pluggable design

> See [github.com/unjs/citty](https://github.com/unjs/citty)

## Installation

```bash
npm install
# or
pnpm add citty colored
```

## Quick Start

### Run the CLI

```bash
# Show help
node src/cli.js --help

# List all patterns
node src/cli.js list

# Execute a pattern
node src/cli.js query optional

# Get info about a pattern
node src/cli.js info bind

# Execute all patterns
node src/cli.js query all

# Show version
node src/cli.js --version
```

### NPM Scripts

```bash
npm run demo:optional     # Execute OPTIONAL pattern
npm run demo:bind         # Execute BIND pattern
npm run demo:filter       # Execute FILTER pattern
npm run demo:union        # Execute UNION pattern
npm run demo:concat       # Execute GROUP_CONCAT pattern
npm run demo:values       # Execute VALUES pattern
npm run demo:exists       # Execute EXISTS/NOT EXISTS pattern
npm run demo:paths        # Execute Property Paths pattern
npm run demo:all          # Execute all 8 patterns
npm run demo:list         # List all patterns
npm run demo:help         # Show help
```

## Architecture

### File Structure

```
ggen-sparql-cli/
├── src/
│   ├── cli.js           # Main Citty CLI definition
│   ├── executor.js      # Query definitions & execution
│   ├── colors.js        # Terminal colorization
│   └── subcommands/
│       ├── query.js     # Query subcommand logic
│       ├── list.js      # List subcommand logic
│       └── info.js      # Info subcommand logic
├── package.json         # Dependencies & scripts
└── README.md            # This file
```

### How Citty Works

#### 1. Define Command Structure

```javascript
import { defineCommand, runMain } from 'citty';

const queryCommand = defineCommand({
  meta: {
    name: 'query',
    description: 'Execute a SPARQL query',
  },
  args: {
    pattern: {
      type: 'positional',
      description: 'Query pattern',
      required: false,
    },
    format: {
      type: 'string',
      description: 'Output format',
      default: 'compact',
    },
  },
  async run({ args }) {
    // Command logic
  },
});
```

#### 2. Add Subcommands

```javascript
const main = defineCommand({
  meta: {
    name: 'ggen-sparql',
    description: 'SPARQL Query CLI',
  },
  subCommands: {
    query: queryCommand,
    list: listCommand,
    info: infoCommand,
  },
});
```

#### 3. Run the CLI

```javascript
import { runMain } from 'citty';

runMain(main).catch((err) => {
  console.error(`Error: ${err.message}`);
  process.exit(1);
});
```

## The 8 SPARQL CONSTRUCT Patterns

### 1. OPTIONAL - Safe Property Enrichment
```bash
node src/cli.js query optional
```

Add optional properties without NULLs:
```sparql
OPTIONAL {
  ?mayor city:mayorsOf ?city ;
    foaf:name ?mayorName
}
BIND(BOUND(?mayorName) AS ?hasMayorInfo)
```

**Benefits**: Graceful missing data handling, discoverable relationships

---

### 2. BIND - Computed Values
```bash
node src/cli.js query bind
```

Derive new attributes from existing data:
```sparql
BIND(?pop / ?area AS ?density)
BIND(IF(?pop > 1000000, "Major", "Small") AS ?sizeCategory)
```

**Benefits**: Type-safe computation, reduces downstream complexity

---

### 3. FILTER - Conditional Output
```bash
node src/cli.js query filter
```

Focus output on important entities:
```sparql
WHERE {
  ?city a city:City ; city:isMajor true .
  FILTER (?pop > 500000)
}
```

**Benefits**: Smaller output graphs, better performance

---

### 4. UNION - Polymorphic Matching
```bash
node src/cli.js query union
```

Unify heterogeneous entity types:
```sparql
{
  ?poi a city:Landmark ; ...
}
UNION
{
  ?poi a city:Event ; ...
}
```

**Benefits**: Single query for multiple types, schema flexibility

---

### 5. GROUP_CONCAT - Aggregation
```bash
node src/cli.js query concat
```

Collect related values into summaries:
```sparql
SELECT ?city (GROUP_CONCAT(DISTINCT ?neighborName; separator=" | ") AS ?neighborsList)
GROUP BY ?city
```

**Benefits**: Summarization without data loss, human-readable output

---

### 6. VALUES - Parameterization
```bash
node src/cli.js query values
```

Process dynamic lists of entities:
```sparql
VALUES ?city {
  city:sf
  city:oakland
  city:berkeley
}
```

**Benefits**: Parameterized templates, reusable queries

---

### 7. EXISTS / NOT EXISTS - Graph Logic
```bash
node src/cli.js query exists
```

Sophisticated pattern matching:
```sparql
BIND(IF(EXISTS {?landmark city:locatedIn ?city}, true, false) AS ?hasLandmarks)
BIND(IF(?hasLandmarks && ?hasMayors, "Historic", "Unmapped") AS ?characterType)
```

**Benefits**: Sophisticated logical patterns, avoids expensive JOINs

---

### 8. Property Paths - Transitive Navigation
```bash
node src/cli.js query paths
```

Follow relationships at any depth:
```sparql
?startCity city:hasNeighbor+ ?reachableCity .
```

**Benefits**: Graph traversal without recursion, path finding

---

## CLI Options

### Global Options

```
--help, -h              Show help
--version               Show version
```

### Query Subcommand Options

```
ggen-sparql query <pattern> [options]

Arguments:
  pattern               Query pattern (optional|bind|filter|union|concat|values|exists|paths|all)
                        Default: optional

Options:
  --format <type>       Output format (json|turtle|ntriples|compact)
                        Default: compact
  --limit <n>           Limit results
  --verbose, -v         Show detailed output
```

### List Subcommand Options

```
ggen-sparql list [options]

Options:
  --verbose, -v         Show detailed descriptions
```

### Info Subcommand

```
ggen-sparql info <pattern>

Get detailed information about a specific pattern
```

## Examples

### Basic Queries

```bash
# Execute OPTIONAL pattern
npm run demo:optional

# Execute all patterns
npm run demo:all

# Show verbose output
node src/cli.js query bind --verbose

# Get JSON output
node src/cli.js query filter --format json
```

### Exploration

```bash
# List all available patterns
npm run demo:list

# Get detailed list with descriptions
node src/cli.js list --verbose

# Get info about a specific pattern
npm run info bind
```

### Help

```bash
# Show main help
npm run demo:help

# Show help for query subcommand
node src/cli.js query --help
```

## Citty Features Demonstrated

### 1. Subcommands
```javascript
subCommands: {
  query: queryCommand,
  list: listCommand,
  info: infoCommand,
}
```

### 2. Argument Types

```javascript
args: {
  pattern: {
    type: 'positional',      // Positional argument
    required: false,
  },
  format: {
    type: 'string',          // String option
    default: 'compact',
  },
  limit: {
    type: 'number',          // Numeric option
  },
  verbose: {
    type: 'boolean',         // Boolean flag
    alias: 'v',
  },
}
```

### 3. Async Execution

```javascript
async run({ args }) {
  const result = await executeQuery(args.pattern);
  // ...
}
```

### 4. Automatic Help

Citty automatically generates help from metadata:
```
ggen-sparql --help
```

## Integration with Oxigraph

To use real SPARQL execution:

1. Install Oxigraph:
```bash
npm install oxigraph
```

2. Update `src/executor.js`:
```javascript
import { Store } from 'oxigraph';

const store = new Store();
store.add(turtleData);
const results = store.query(sparqlQuery);
```

## Performance

| Pattern | Time | Memory | Use When |
|---------|------|--------|----------|
| OPTIONAL | O(n) | O(n) | Many optionals |
| BIND | O(n) | O(1) | Pre-compute |
| FILTER | O(n log n) | O(1) | Focus output |
| UNION | O(n+m) | O(n+m) | Combine patterns |
| GROUP_CONCAT | O(n log n) | O(m) | Aggregate |
| VALUES | O(1) | O(k) | k parameters |
| EXISTS | O(n) | O(1) | Check presence |
| Property Paths | O(V+E) | O(V) | Graph traversal |

## Production Deployment

### Make Executable

```bash
chmod +x src/cli.js
npm link
ggen-sparql --help
```

### Docker Container

```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY . .
RUN npm install --production
RUN npm link
ENTRYPOINT ["ggen-sparql"]
```

```bash
docker build -t ggen-sparql .
docker run ggen-sparql query optional
```

## Testing

Add Vitest tests:

```bash
npm install --save-dev vitest
```

```javascript
import { test, expect } from 'vitest';
import { executeQuery } from './src/executor.js';

test('executes optional pattern', async () => {
  const result = await executeQuery('optional');
  expect(result.success).toBe(true);
  expect(result.tripleCount).toBeGreaterThan(0);
});
```

## Debugging

### Enable Verbose Output

```bash
node src/cli.js query bind --verbose
```

### Show Query Details

```bash
node src/cli.js info bind
```

### JSON Output for Inspection

```bash
node src/cli.js query filter --format json | jq '.query'
```

## Further Reading

- **Citty Docs**: [github.com/unjs/citty](https://github.com/unjs/citty)
- **SPARQL Spec**: [w3.org/TR/sparql11-query](https://www.w3.org/TR/sparql11-query/)
- **Oxigraph**: [oxigraph.org](https://oxigraph.org/)

## Contributing

1. Add new pattern to `src/executor.js`
2. Add subcommand to `src/cli.js`
3. Update this README with examples
4. Test with `npm run demo:all`

## License

MIT - See LICENSE in parent ggen project
