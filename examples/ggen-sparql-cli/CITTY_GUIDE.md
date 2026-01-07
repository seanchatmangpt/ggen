# Building CLIs with Citty: Complete Guide

Learn how to build elegant command-line interfaces using Citty, the lightweight CLI framework by unjs.

This guide uses `ggen-sparql-cli` as a practical example.

## What is Citty?

**Citty** is a modern CLI builder featuring:
- 🚀 Fast argument parsing (based on mri)
- ✨ Zero dependencies (core)
- 🔄 Async/await support
- 📖 Auto-generated help docs
- 🎯 Nested subcommands
- 🔧 Type-safe arguments

**GitHub**: [unjs/citty](https://github.com/unjs/citty)

---

## Part 1: Getting Started

### Installation

```bash
npm install citty
# Optional: for colorized output
npm install colored
```

### Basic Command

```javascript
#!/usr/bin/env node
import { defineCommand, runMain } from 'citty';

const main = defineCommand({
  meta: {
    name: 'hello',
    version: '1.0.0',
    description: 'Say hello',
  },
  args: {
    name: {
      type: 'positional',
      description: 'Your name',
      required: true,
    },
  },
  run({ args }) {
    console.log(`Hello ${args.name}!`);
  },
});

runMain(main);
```

**Usage**:
```bash
node hello.js Alice
# Output: Hello Alice!
```

---

## Part 2: Argument Types

### Positional Arguments

```javascript
args: {
  name: {
    type: 'positional',
    description: 'User name',
    required: true,  // Must be provided
  },
}
```

**CLI**: `myapp Alice`

### String Options

```javascript
args: {
  format: {
    type: 'string',
    description: 'Output format',
    default: 'json',
  },
}
```

**CLI**: `myapp --format yaml`

### Boolean Flags

```javascript
args: {
  verbose: {
    type: 'boolean',
    description: 'Verbose output',
    alias: 'v',  // Short form
  },
}
```

**CLI**: `myapp --verbose` or `myapp -v`

### Number Arguments

```javascript
args: {
  count: {
    type: 'number',
    description: 'Item count',
    default: 10,
  },
}
```

**CLI**: `myapp --count 50`

---

## Part 3: Subcommands

Citty supports nested command hierarchies:

### Define Subcommands

```javascript
const queryCommand = defineCommand({
  meta: {
    name: 'query',
    description: 'Execute queries',
  },
  args: { /* ... */ },
  run({ args }) {
    // Query logic
  },
});

const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List items',
  },
  args: { /* ... */ },
  run({ args }) {
    // List logic
  },
});
```

### Attach Subcommands

```javascript
const main = defineCommand({
  meta: {
    name: 'myapp',
    description: 'My Application',
  },
  subCommands: {
    query: queryCommand,
    list: listCommand,
  },
});

runMain(main);
```

### Usage

```bash
myapp query --help
myapp list --verbose
```

---

## Part 4: Async/Await Support

Citty supports asynchronous command execution:

```javascript
const fetchCommand = defineCommand({
  meta: {
    name: 'fetch',
    description: 'Fetch data',
  },
  args: {
    url: {
      type: 'positional',
      required: true,
    },
  },
  async run({ args }) {
    try {
      const response = await fetch(args.url);
      const data = await response.json();
      console.log(JSON.stringify(data, null, 2));
    } catch (err) {
      console.error(`Error: ${err.message}`);
      process.exit(1);
    }
  },
});
```

---

## Part 5: Advanced Features

### Default Values

```javascript
args: {
  port: {
    type: 'number',
    default: 3000,
  },
}
```

If `--port` is not provided, defaults to 3000.

### Aliases

```javascript
args: {
  verbose: {
    type: 'boolean',
    alias: 'v',  // Can use -v instead of --verbose
  },
}
```

### Context Access

```javascript
async run({ args, command, rawArgs }) {
  console.log(args);        // Parsed arguments
  console.log(command);     // Current command
  console.log(rawArgs);     // Raw process.argv
}
```

### Error Handling

```javascript
runMain(main).catch((err) => {
  console.error(`Fatal error: ${err.message}`);
  process.exit(1);
});
```

---

## Part 6: Real-World Example

### Complete CLI with 3 Subcommands

```javascript
#!/usr/bin/env node
import { defineCommand, runMain } from 'citty';

// Subcommand 1: query
const queryCmd = defineCommand({
  meta: { name: 'query', description: 'Execute query' },
  args: {
    pattern: {
      type: 'positional',
      description: 'Query pattern',
      required: false,
    },
    format: {
      type: 'string',
      description: 'Output format',
      default: 'json',
    },
  },
  async run({ args }) {
    console.log(`Executing: ${args.pattern} (format: ${args.format})`);
  },
});

// Subcommand 2: list
const listCmd = defineCommand({
  meta: { name: 'list', description: 'List items' },
  args: {
    verbose: {
      type: 'boolean',
      alias: 'v',
    },
  },
  run({ args }) {
    console.log(args.verbose ? 'Detailed list' : 'Short list');
  },
});

// Main command
const main = defineCommand({
  meta: {
    name: 'myapp',
    version: '1.0.0',
    description: 'My CLI App',
  },
  subCommands: {
    query: queryCmd,
    list: listCmd,
  },
  run({ args }) {
    console.log('Use --help for usage information');
  },
});

// Execute
runMain(main);
```

**Usage**:
```bash
node app.js --help
node app.js query --help
node app.js query sparql --format turtle
node app.js list --verbose
```

---

## Part 7: ggen-sparql-cli Implementation Details

### File Structure

```
src/
├── cli.js         # Main Citty definition (267 lines)
├── executor.js    # Query logic (569 lines)
└── colors.js      # Terminal colors (75 lines)
```

### CLI Definition (cli.js)

```javascript
// 1. Define individual commands
const queryCommand = defineCommand({...});
const listCommand = defineCommand({...});
const infoCommand = defineCommand({...});

// 2. Combine into main command
const main = defineCommand({
  meta: {...},
  subCommands: {
    query: queryCommand,
    list: listCommand,
    info: infoCommand,
  },
});

// 3. Execute
runMain(main);
```

### Executor Module (executor.js)

```javascript
// Store all query definitions
const QUERIES = {
  optional: { title, features, description, query },
  bind: { ... },
  filter: { ... },
  // ... 8 patterns total
};

// Execution function
export async function executeQuery(pattern, options) {
  const queryDef = QUERIES[pattern];
  // Execute SPARQL
  // Format results
  return { success, tripleCount, data };
}

// Information function
export function getQueryInfo(pattern) {
  return QUERIES[pattern];
}

// List function
export function listQueries() {
  return Object.values(QUERIES);
}
```

### Terminal Colors (colors.js)

```javascript
export function colorize(text, color, style) {
  const ansiCodes = {
    red: '\x1b[31m',
    green: '\x1b[32m',
    yellow: '\x1b[33m',
    cyan: '\x1b[36m',
    reset: '\x1b[0m',
  };
  return `${ansiCodes[color]}${text}${ansiCodes.reset}`;
}
```

---

## Part 8: Patterns & Best Practices

### 1. Single Responsibility

**❌ Bad**: Mix CLI parsing with business logic
```javascript
async run({ args }) {
  // Parse file
  const data = fs.readFileSync(args.file);
  // Transform data
  // Output results
  // Handle errors
}
```

**✅ Good**: Separate concerns
```javascript
async run({ args }) {
  const result = await executeQuery(args.pattern);
  console.log(result.data);
}
```

### 2. Consistent Error Handling

```javascript
// In each command's run()
try {
  const result = await execute(...);
  console.log(colorize('✅ Success', 'green'));
} catch (err) {
  console.error(colorize(`❌ Error: ${err.message}`, 'red'));
  process.exit(1);
}
```

### 3. Help Text as Documentation

```javascript
meta: {
  name: 'query',
  description: 'Execute a SPARQL query',
  // Citty auto-generates help from descriptions
}
```

### 4. Sensible Defaults

```javascript
args: {
  format: {
    type: 'string',
    default: 'compact',  // User-friendly default
  },
}
```

### 5. Type Safety with Argument Types

```javascript
args: {
  count: {
    type: 'number',     // Ensures numeric validation
    default: 10,
  },
}
```

---

## Part 9: Deployment

### Make Executable

```bash
chmod +x src/cli.js
npm link                # Make globally available
ggen-sparql --help
```

### NPM Binary

```json
{
  "bin": {
    "ggen-sparql": "src/cli.js"
  }
}
```

### Docker

```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY package*.json .
RUN npm ci --only=production
COPY src/ src/
RUN chmod +x src/cli.js
RUN npm link
ENTRYPOINT ["ggen-sparql"]
```

```bash
docker build -t ggen-sparql .
docker run ggen-sparql query optional
```

---

## Part 10: Testing

### Unit Test (Vitest)

```javascript
import { test, expect } from 'vitest';
import { executeQuery, getQueryInfo, listQueries } from './src/executor.js';

test('executes optional pattern', async () => {
  const result = await executeQuery('optional');
  expect(result.success).toBe(true);
  expect(result.tripleCount).toBeGreaterThan(0);
});

test('provides pattern info', () => {
  const info = getQueryInfo('bind');
  expect(info).toBeDefined();
  expect(info.features).toContain('BIND');
});

test('lists all patterns', () => {
  const queries = listQueries();
  expect(queries.length).toBe(8);
});
```

### Integration Test (Citty)

```javascript
import { execSync } from 'child_process';

test('CLI help works', () => {
  const output = execSync('node src/cli.js --help').toString();
  expect(output).toContain('ggen-sparql');
});

test('Query subcommand executes', () => {
  const output = execSync('node src/cli.js query optional').toString();
  expect(output).toContain('CityProfile');
});
```

---

## Part 11: Common Patterns

### Boolean Flags with Default

```javascript
args: {
  production: {
    type: 'boolean',
    description: 'Production mode',
    // Default is false (not provided)
  },
}

// Usage: myapp --production
```

### Required Positional

```javascript
args: {
  filename: {
    type: 'positional',
    required: true,
    description: 'File to process',
  },
}

// Usage: myapp data.json (required)
```

### Optional Positional with Default

```javascript
args: {
  pattern: {
    type: 'positional',
    required: false,
    description: 'Pattern name',
  },
}

// Usage: myapp (no arg) or myapp bind
```

### Multiple Options

```javascript
args: {
  input: {
    type: 'string',
    alias: 'i',
    description: 'Input file',
  },
  output: {
    type: 'string',
    alias: 'o',
    description: 'Output file',
  },
  format: {
    type: 'string',
    alias: 'f',
    default: 'json',
  },
}

// Usage: myapp -i input.ttl -o output.json -f turtle
```

---

## Part 12: Troubleshooting

### Help not showing

**Issue**: `--help` doesn't display
**Solution**: Ensure `runMain(main)` is called

### Arguments not parsing

**Issue**: `args.count` is string, not number
**Solution**: Specify `type: 'number'` in args definition

### Async command not running

**Issue**: `async run()` not being awaited
**Solution**: Citty automatically awaits async runs

### Subcommand not found

**Issue**: `myapp unknown` fails
**Solution**: Define subcommand in `subCommands` object

---

## Learning Resources

1. **Citty GitHub**: https://github.com/unjs/citty
2. **unjs Organization**: https://github.com/unjs
3. **mri (parser)**: https://github.com/lukeed/mri
4. **SPARQL Spec**: https://www.w3.org/TR/sparql11-query/

---

## Summary

Citty provides:

| Feature | Benefit |
|---------|---------|
| **defineCommand** | Structure CLI logically |
| **subCommands** | Organize nested operations |
| **Type-safe args** | Validate inputs automatically |
| **Auto help** | Self-documenting CLIs |
| **Async support** | Modern JavaScript patterns |
| **Pluggable design** | Extend easily |

---

**Next**: Run `npm run demo:all` to see ggen-sparql-cli in action!
