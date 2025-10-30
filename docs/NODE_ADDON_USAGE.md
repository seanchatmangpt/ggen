# Ggen Node.js Addon Usage Guide

## Overview

The `@ggen/node` package provides production-grade Node.js N-API bindings for the ggen CLI. It enables JavaScript/TypeScript applications to use ggen functionality programmatically without spawning external processes.

## Installation

```bash
npm install @ggen/node
# or
yarn add @ggen/node
# or
pnpm add @ggen/node
```

## Core API: `run()` Function

The `run()` function is the low-level API that executes any ggen CLI command and returns structured results.

### Signature

```typescript
async function run(args: string[]): Promise<RunResult>

interface RunResult {
  code: number;      // Exit code (0 = success, non-zero = error)
  stdout: string;    // Captured standard output
  stderr: string;    // Captured standard error
}
```

### Examples

#### Get Version

```typescript
import { run } from '@ggen/node';

const result = await run(['--version']);
console.log(result.stdout); // "ggen 1.2.0"
console.log(result.code);   // 0 or 1
```

#### Search Marketplace

```typescript
import { run } from '@ggen/node';

const result = await run(['market', 'search', 'rust web']);
if (result.code === 0) {
  console.log('Search results:', result.stdout);
} else {
  console.error('Search failed:', result.stderr);
}
```

## License

MIT - See LICENSE file for details
