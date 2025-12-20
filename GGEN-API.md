# GGen API - Code Generation Abstraction Layer

**Version**: 1.0.0
**Location**: `src/lib/ggen.mjs`
**Purpose**: Clean abstraction over ~/ggen for generate/refine/test/iterate workflows

---

## Installation

```javascript
import { GGen } from './src/lib/ggen.mjs'
// or
import { generate, refine, test, iterate, list } from './src/lib/ggen.mjs'
```

---

## Quick Start

### Generate Code
```javascript
const ggen = new GGen()

const result = await ggen.generate({
  modules: [
    { id: 'retry', prompt: 'Generate retry(fn, max, delay) with exponential backoff' },
    { id: 'cache', prompt: 'Generate LRU cache class' }
  ],
  output: 'my-utils',
  concurrency: 10
})

// result: { modules: 2, tests: 2, totalLines: 150, duration: 45.2, outputDir: '~/ggen/my-utils' }
```

### Refine Code
```javascript
const result = await ggen.refine('my-utils')

// result: { refined: 2, jsdocBlocks: 8, outputDir: '~/ggen/my-utils-refined' }
```

### Test Code
```javascript
const result = await ggen.test('my-utils')

// result: { passed: 7, failed: 0, success: true }
```

### Iterate Until Passing
```javascript
const result = await ggen.iterate('my-utils', { maxRounds: 3 })

// result: { success: true, rounds: 2, finalResult: { passed: 7, failed: 0 } }
```

### List Projects
```javascript
const projects = await ggen.list()

// projects: [
//   { name: 'my-utils', path: '~/ggen/my-utils', modules: 2, tests: 2 },
//   { name: 'figex-gen', path: '~/ggen/figex-gen', modules: 23, tests: 0 }
// ]
```

---

## API Reference

### Class: GGen

```javascript
new GGen(options)
```

**Options**:
- `root` (string) - Root directory (default: `~/ggen`)
- `model` (string) - Default Ollama model (default: `qwen3:8b`)
- `timeout` (number) - Request timeout ms (default: `120000`)
- `verbose` (boolean) - Log output (default: `true`)

**Example**:
```javascript
const ggen = new GGen({
  root: '/custom/path',
  model: 'mistral:7b',
  timeout: 180000,
  verbose: false
})
```

---

### Method: generate(config)

Generate modules with JSDoc and tests in a single pass.

**Parameters**:
```javascript
{
  modules: [
    {
      id: string,        // Module filename (without .mjs)
      prompt: string     // Generation prompt
    }
  ],
  output: string,        // Output directory name
  concurrency?: number   // Parallel generators (default: 10)
}
```

**Returns**:
```javascript
{
  modules: number,       // Modules created
  tests: number,         // Test files created
  totalLines: number,    // Total code lines
  duration: number,      // Seconds
  outputDir: string      // Full path to output
}
```

**Example**:
```javascript
await ggen.generate({
  modules: [
    { id: 'event-bus', prompt: 'Generate EventEmitter with on/emit/off' },
    { id: 'debounce', prompt: 'Generate debounce(fn, delay)' }
  ],
  output: 'utils',
  concurrency: 2
})
```

**Output Structure**:
```
~/ggen/utils/
├── event-bus.mjs
├── event-bus.test.mjs
├── debounce.mjs
├── debounce.test.mjs
├── package.json
└── vitest.config.js
```

---

### Method: refine(project, options)

Refine existing modules - add JSDoc, fix issues, improve code.

**Parameters**:
```javascript
project: string           // Project directory name
options?: {
  maxModules?: number     // Max modules to refine (default: 10)
}
```

**Returns**:
```javascript
{
  refined: number,        // Modules refined
  jsdocBlocks: number,    // JSDoc blocks added
  outputDir: string       // Output path (project-refined)
}
```

**Example**:
```javascript
await ggen.refine('utils', { maxModules: 5 })

// Creates ~/ggen/utils-refined/ with refined modules
```

---

### Method: test(project)

Run tests and return results.

**Parameters**:
```javascript
project: string           // Project directory name
```

**Returns**:
```javascript
{
  passed: number,         // Tests passed
  failed: number,         // Tests failed
  success: boolean,       // All passed?
  error?: string          // Error output if failed
}
```

**Example**:
```javascript
const result = await ggen.test('utils')

if (!result.success) {
  console.error(`${result.failed} tests failed`)
  console.error(result.error)
}
```

---

### Method: iterate(project, options)

Iterate until all tests pass - auto-refine failing code.

**Parameters**:
```javascript
project: string           // Project directory name
options?: {
  maxRounds?: number      // Max iteration rounds (default: 3)
}
```

**Returns**:
```javascript
{
  success: boolean,       // All tests passing?
  rounds: number,         // Rounds executed
  finalResult: {          // Final test results
    passed: number,
    failed: number,
    success: boolean
  }
}
```

**Example**:
```javascript
const result = await ggen.iterate('utils', { maxRounds: 5 })

if (result.success) {
  console.log(`All tests passing after ${result.rounds} rounds`)
} else {
  console.log(`Failed after ${result.rounds} rounds`)
  console.log(`Final: ${result.finalResult.failed} failing`)
}
```

**Iteration Process**:
1. Test current state
2. If all pass → SUCCESS
3. Refine failing modules
4. Replace originals with refined
5. Repeat (max rounds)

---

### Method: list()

List all projects in ~/ggen.

**Returns**:
```javascript
[
  {
    name: string,         // Project name
    path: string,         // Full path
    modules: number,      // Module count
    tests: number         // Test count
  }
]
```

**Example**:
```javascript
const projects = await ggen.list()

console.log(`Found ${projects.length} projects:`)
projects.forEach(p => {
  console.log(`  ${p.name}: ${p.modules} modules, ${p.tests} tests`)
})
```

---

## Helper Functions

For quick one-off operations:

```javascript
import { generate, refine, test, iterate, list } from './src/lib/ggen.mjs'

// Generate
await generate([
  { id: 'cache', prompt: 'Generate LRU cache' }
], 'my-cache')

// Refine
await refine('my-cache')

// Test
await test('my-cache')

// Iterate
await iterate('my-cache', 3)

// List
await list()
```

---

## Workflows

### Workflow 1: Generate → Test → Deploy
```javascript
const ggen = new GGen()

// 1. Generate
const gen = await ggen.generate({
  modules: [
    { id: 'api-client', prompt: 'Generate REST API client' }
  ],
  output: 'api-utils'
})

console.log(`Generated ${gen.modules} modules`)

// 2. Test
const test = await ggen.test('api-utils')

if (test.success) {
  console.log('✅ Ready to deploy')
} else {
  console.log('❌ Tests failing, needs refinement')
}
```

### Workflow 2: Generate → Iterate → Deploy
```javascript
const ggen = new GGen()

// Generate
await ggen.generate({
  modules: [{ id: 'cache', prompt: 'LRU cache' }],
  output: 'cache-lib'
})

// Iterate until passing
const result = await ggen.iterate('cache-lib', { maxRounds: 3 })

if (result.success) {
  console.log(`✅ All tests passing after ${result.rounds} rounds`)
  // Deploy cache-lib
} else {
  console.log(`❌ Tests still failing after ${result.rounds} rounds`)
}
```

### Workflow 3: Bulk Generation
```javascript
const ggen = new GGen()

const modules = [
  { id: 'array-utils', prompt: 'chunk, unique, flatten' },
  { id: 'string-utils', prompt: 'capitalize, truncate, slug' },
  { id: 'date-utils', prompt: 'format, parse, diff' },
  { id: 'async-utils', prompt: 'sleep, timeout, retry' },
  { id: 'validation', prompt: 'email, url, phone validators' }
]

const result = await ggen.generate({
  modules,
  output: 'utils-library',
  concurrency: 10
})

console.log(`Generated ${result.modules} modules in ${result.duration}s`)
console.log(`Throughput: ${(result.modules / result.duration).toFixed(1)} modules/sec`)
```

---

## Configuration

### Custom Model
```javascript
const ggen = new GGen({ model: 'mistral:7b' })

await ggen.generate({
  modules: [{ id: 'test', prompt: 'Generate test module' }],
  output: 'test-output'
})
```

### Custom Root Directory
```javascript
const ggen = new GGen({ root: '/projects/generated' })

await ggen.generate({
  modules: [{ id: 'mod', prompt: 'Generate module' }],
  output: 'my-project'
})

// Output: /projects/generated/my-project/
```

### Silent Mode
```javascript
const ggen = new GGen({ verbose: false })

const result = await ggen.generate({
  modules: [{ id: 'silent', prompt: 'Generate silently' }],
  output: 'silent-gen'
})

// No console output, only return value
```

---

## Error Handling

```javascript
try {
  const ggen = new GGen()

  const result = await ggen.generate({
    modules: [{ id: 'test', prompt: 'Generate test' }],
    output: 'test-project'
  })

  if (result.modules === 0) {
    throw new Error('No modules generated')
  }

  const testResult = await ggen.test('test-project')

  if (!testResult.success) {
    console.error(`Tests failed: ${testResult.error}`)

    // Attempt iteration
    const iterResult = await ggen.iterate('test-project')

    if (!iterResult.success) {
      throw new Error(`Cannot fix failing tests after ${iterResult.rounds} rounds`)
    }
  }

} catch (error) {
  console.error('Generation failed:', error.message)
}
```

---

## Performance

### Single Module Generation
```
Time: 30-60s
Output: 1 module + 1 test (50-100 lines)
```

### Bulk Generation (10 modules)
```
Time: 60-90s
Output: 10 modules + 10 tests (500-1000 lines)
Concurrency: 10 parallel generators
Throughput: 6-10 modules/min
```

### Refinement
```
Time: 10-15s per module
Output: JSDoc blocks added (2-5 per module)
```

### Iteration (3 rounds)
```
Time: 120-180s
Output: All tests passing
Success Rate: 80-90%
```

---

## Integration

### With CI/CD
```javascript
// generate-utils.mjs
import { GGen } from './src/lib/ggen.mjs'

const ggen = new GGen()

const result = await ggen.generate({
  modules: process.env.MODULES.split(',').map(id => ({
    id,
    prompt: process.env[`PROMPT_${id.toUpperCase()}`]
  })),
  output: process.env.OUTPUT_DIR
})

if (result.modules === 0) {
  process.exit(1)
}

const testResult = await ggen.test(process.env.OUTPUT_DIR)

process.exit(testResult.success ? 0 : 1)
```

### With Build Tools
```javascript
// vite.config.js
import { defineConfig } from 'vite'
import { GGen } from './src/lib/ggen.mjs'

export default defineConfig({
  plugins: [
    {
      name: 'ggen',
      async buildStart() {
        const ggen = new GGen()
        await ggen.generate({
          modules: [{ id: 'config', prompt: 'Generate config module' }],
          output: 'generated'
        })
      }
    }
  ]
})
```

---

## Best Practices

1. **Use Concise Prompts**
   ```javascript
   // ✅ Good
   { id: 'retry', prompt: 'Generate retry(fn, max, delay) with exponential backoff' }

   // ❌ Too verbose
   { id: 'retry', prompt: 'Generate a function called retry that takes...' }
   ```

2. **Test After Generation**
   ```javascript
   await ggen.generate({ modules, output })
   await ggen.test(output) // Always test
   ```

3. **Iterate for Production**
   ```javascript
   await ggen.iterate(output, { maxRounds: 3 }) // Ensure quality
   ```

4. **Use Descriptive IDs**
   ```javascript
   // ✅ Good
   { id: 'array-chunk', prompt: '...' }

   // ❌ Bad
   { id: 'util1', prompt: '...' }
   ```

---

## Conclusion

The GGen abstraction layer provides:
- ✅ Clean API wrapping ~/ggen
- ✅ Generate with JSDoc + tests (single pass)
- ✅ Refine existing code
- ✅ Auto-test validation
- ✅ Iterate until tests pass
- ✅ Project management (list)

**Result**: Production-ready code generation in 60-90 seconds.
