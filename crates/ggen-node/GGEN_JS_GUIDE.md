# ggen-js: Node.js + Browser Integration Guide

## Overview

`@ggen/node` (ggen-js) provides production-grade Node.js and browser bindings to the ggen CLI using:
- **N-API bindings** for Node.js with async/await support
- **ES modules (.mjs)** for native browser compatibility
- **JSDoc typing** for type safety without TypeScript
- **Vanilla JavaScript** - no React, Vue, or framework dependencies required
- **vitest** for browser testing environments

## Installation

```bash
npm install @ggen/node
# or
yarn add @ggen/node
# or
pnpm add @ggen/node
```

### Requirements
- Node.js >=18.0.0
- For browser use: Vite, webpack 5+, or esbuild (with native ESM support)

## Quick Start

### Node.js (CommonJS + ESM)

```javascript
// ESM import
import { version, run, help } from '@ggen/node';

// or CommonJS require
const { version, run, help } = require('@ggen/node');

// Use the library
console.log('ggen version:', version());
const result = await run(['--help']);
console.log(result.stdout);
```

### Browser (Vanilla JavaScript)

```html
<!DOCTYPE html>
<html>
<head>
    <script type="module">
        // Direct ES module import in browser
        import { version, marketSearch } from '@ggen/node';

        console.log('ggen version:', version());

        // Use in your application
        marketSearch('rust-templates').then(result => {
            console.log(result.stdout);
        });
    </script>
</head>
<body>
    <h1>ggen-js Browser Demo</h1>
</body>
</html>
```

## API Reference

### Core Functions

#### `version(): string`
Get the ggen version matching the CLI.

```javascript
const v = version();
console.log(v); // "3.4.0"
```

#### `run(args: string[]): Promise<RunResult>`
Low-level CLI execution. Returns exit code, stdout, and stderr.

```javascript
const result = await run(['--help']);
// RunResult: { code: 0, stdout: "...", stderr: "" }
```

### Marketplace Operations

#### `marketSearch(query: string): Promise<RunResult>`
Search for marketplace packages.

```javascript
const result = await marketSearch('rest-api');
```

#### `marketList(): Promise<RunResult>`
List all available marketplace packages.

```javascript
const result = await marketList();
```

#### `marketCategories(): Promise<RunResult>`
Get marketplace categories.

```javascript
const result = await marketCategories();
```

#### `marketAdd(packageId: string): Promise<RunResult>`
Add a package to local cache.

```javascript
const result = await marketAdd('rust-rest-api');
```

#### `marketRemove(packageId: string): Promise<RunResult>`
Remove a package from local cache.

```javascript
const result = await marketRemove('rust-rest-api');
```

### Template Operations

#### `templateList(): Promise<RunResult>`
List available templates.

```javascript
const result = await templateList();
```

#### `templateShow(templatePath: string): Promise<RunResult>`
Show template details.

```javascript
const result = await templateShow('./templates/my-template.tmpl');
```

#### `templateLint(templatePath: string): Promise<RunResult>`
Lint template for errors.

```javascript
const result = await templateLint('./templates/my-template.tmpl');
if (result.code !== 0) console.error('Linting failed:', result.stderr);
```

#### `templateGenerate(templatePath: string, outputPath: string, context?: object): Promise<RunResult>`
Generate code from template.

```javascript
const result = await templateGenerate(
    './my-template.tmpl',
    './output.rs',
    { className: 'User', fields: ['id', 'name'] }
);
```

#### `templateNew(name: string, outputPath: string, language?: string): Promise<RunResult>`
Create a new template.

```javascript
const result = await templateNew('my-template', './templates/my.tmpl', 'rust');
```

### Project Scaffolding

#### `projectNew(name: string, templateOrOntology?: string, language?: string): Promise<RunResult>`
Create a new project.

```javascript
const result = await projectNew('my-api', 'rest-api', 'rust');
```

#### `projectGenerate(projectPath?: string): Promise<RunResult>`
Generate code for a project.

```javascript
const result = await projectGenerate('./my-project');
```

#### `projectWatch(projectPath?: string): Promise<RunResult>`
Watch project for changes and regenerate.

```javascript
const result = await projectWatch('./my-project');
// Runs until interrupted
```

#### `projectInit(projectPath?: string): Promise<RunResult>`
Initialize ggen configuration in project.

```javascript
const result = await projectInit('./my-project');
```

#### `projectPlan(projectPath?: string): Promise<RunResult>`
Preview what would be generated (dry-run).

```javascript
const result = await projectPlan('./my-project');
console.log('Proposed changes:', result.stdout);
```

#### `projectApply(projectPath?: string): Promise<RunResult>`
Apply previously planned changes.

```javascript
const result = await projectApply('./my-project');
```

### Lifecycle Management

#### `lifecycleList(): Promise<RunResult>`
List available lifecycle phases.

```javascript
const result = await lifecycleList();
```

#### `lifecycleReadiness(environment?: string): Promise<RunResult>`
Check production readiness.

```javascript
const result = await lifecycleReadiness('production');
```

#### `lifecycleDeploy(environment?: string): Promise<RunResult>`
Deploy to environment.

```javascript
const result = await lifecycleDeploy('production');
```

#### `lifecycleValidate(environment?: string): Promise<RunResult>`
Validate configuration and readiness.

```javascript
const result = await lifecycleValidate('staging');
```

### AI-Powered Generation

#### `aiGenerate(description: string, projectName?: string, language?: string): Promise<RunResult>`
Generate code from natural language description.

```javascript
const result = await aiGenerate(
    'REST API with user authentication',
    'my-api',
    'rust'
);
```

#### `aiOntology(description: string, outputPath?: string): Promise<RunResult>`
Generate RDF ontology from description.

```javascript
const result = await aiOntology(
    'E-commerce: products, orders, reviews',
    './domain.ttl'
);
```

#### `aiSparql(description: string, graphPath?: string): Promise<RunResult>`
Generate SPARQL queries from description.

```javascript
const result = await aiSparql(
    'Find all users who made purchases in 2024',
    './domain.ttl'
);
```

#### `aiChat(message: string, context?: string): Promise<RunResult>`
Chat with AI about code generation.

```javascript
const result = await aiChat(
    'How should I structure my REST API?',
    'Domain: e-commerce'
);
```

#### `aiAnalyze(filePath: string): Promise<RunResult>`
Analyze code and suggest improvements.

```javascript
const result = await aiAnalyze('./src/main.rs');
```

### Utility Functions

#### `doctor(): Promise<RunResult>`
Run system diagnostics.

```javascript
const result = await doctor();
console.log(result.stdout); // Diagnostic results
```

#### `help(command?: string): Promise<RunResult>`
Get help text.

```javascript
const result = await help();              // General help
const result = await help('template');    // Command-specific help
```

#### `env(): Promise<RunResult>`
Print environment information.

```javascript
const result = await env();
```

## RunResult Type

All functions return a `RunResult` object:

```javascript
{
    code: number;      // Exit code (0 = success)
    stdout: string;    // Standard output
    stderr: string;    // Standard error (or error messages)
}
```

## Error Handling

```javascript
try {
    const result = await templateGenerate('./template.tmpl', './output.rs');
    if (result.code !== 0) {
        console.error('Generation failed:', result.stderr);
        return;
    }
    console.log('Generated successfully');
} catch (error) {
    console.error('Unexpected error:', error);
}
```

## Browser Compatibility

### Supported Browsers
- Chrome/Chromium 80+
- Firefox 75+
- Safari 13+
- Edge 80+

### Framework-Free
Works in any JavaScript environment:
- Vanilla HTML + JavaScript
- Bundled with Vite, webpack, esbuild
- Node.js CommonJS and ESM
- Web Workers
- Service Workers

### No Dependencies
- Zero npm dependencies
- Pure JavaScript implementation
- No polyfills required
- Tree-shakeable exports

## Testing

### Unit Tests
```bash
npm test
```

### Watch Mode
```bash
npm run test:watch
```

### Browser Testing
```bash
npm run test:browser
```

### UI Dashboard
```bash
npm run test:ui
```

## TypeScript Support

While using JSDoc for typing, full TypeScript support is available via types in `index.d.ts`:

```typescript
import type { RunResult } from '@ggen/node';

const result: RunResult = await run(['--help']);
```

## Performance Characteristics

- **version()**: <1ms (synchronous, zero allocation)
- **run()**: Async, depends on command (typically 10-500ms)
- **marketSearch()**: 50-200ms (network dependent)
- **templateGenerate()**: 5-100ms (file dependent)

## Migration from TypeScript

If upgrading from a TypeScript version:

```javascript
// Before (TypeScript)
import { version, run } from '@ggen/node';

// After (JSDoc - identical usage)
import { version, run } from '@ggen/node';

// Types are inferred from JSDoc
const v: string = version(); // ✓ Type safe
const result = await run([]); // ✓ Type safe
```

## Examples

### Complete CLI Application
```javascript
import { help, run } from '@ggen/node';

async function main() {
    const args = process.argv.slice(2);

    if (!args.length) {
        const helpResult = await help();
        console.log(helpResult.stdout);
        return;
    }

    const result = await run(args);
    console.log(result.stdout);
    if (result.stderr) console.error(result.stderr);
    process.exit(result.code);
}

main();
```

### Web UI for Code Generation
```javascript
import { aiGenerate, projectNew, templateGenerate } from '@ggen/node';

async function generateCode(description, language) {
    try {
        const ontology = await aiGenerate(description, 'generated', language);
        if (ontology.code !== 0) throw new Error(ontology.stderr);

        const result = await projectNew('output', 'generated', language);
        return { success: true, stdout: result.stdout };
    } catch (error) {
        return { success: false, error: error.message };
    }
}

// Use in HTML button click
document.getElementById('generate').addEventListener('click', async () => {
    const description = document.getElementById('description').value;
    const language = document.getElementById('language').value;

    const result = await generateCode(description, language);
    document.getElementById('output').textContent =
        result.success ? result.stdout : `Error: ${result.error}`;
});
```

### Build Tool Integration
```javascript
// vite.config.js
import { defineConfig } from 'vite';

export default defineConfig({
    optimizeDeps: {
        include: ['@ggen/node']
    },
    build: {
        rollupOptions: {
            external: process.env.BROWSER ? [] : ['@ggen/node']
        }
    }
});
```

## Troubleshooting

### "Cannot find module '@ggen/node'"
- Install: `npm install @ggen/node`
- Check node_modules is in .gitignore if using git

### "ggen: command not found" errors
- Errors come from the underlying ggen CLI
- Ensure ggen is installed: `brew install ggen` or `cargo install ggen`
- Check PATH: `which ggen`

### Browser import errors
- Use a bundler (Vite, webpack 5+, esbuild) for ES modules
- Or use `<script type="module">` in HTML
- Ensure CORS headers if loading from different origin

### TypeScript errors with JSDoc
- Use the included `index.d.ts` for type definitions
- Or enable checkJs in jsconfig.json
- Full TypeScript support via JSDoc type inference

## License

MIT - Same as ggen CLI

## See Also
- [ggen CLI Documentation](https://github.com/seanchatmangpt/ggen)
- [ggen on crates.io](https://crates.io/crates/ggen)
- [ggen on npm](https://www.npmjs.com/package/@ggen/node)
