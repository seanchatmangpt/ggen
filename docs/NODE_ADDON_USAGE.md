## ggen Node.js Addon (N-API)

Prerequisites:

- Node.js >= 18
- One of: npm (with npx), pnpm (via corepack pnpm), or yarn

Install from source (in this repo):

```bash
cargo make node-build-release
```

Use in Node:

```ts
import { version, run } from '@ggen/node'

console.log('ggen version', version())
const res = await run(['--help'])
console.log(res.code, res.stdout)
```

Prebuilds (CI):

```bash
cargo make node-prebuild

Notes:

- The build tasks try `npx @napi-rs/cli@2.18.0`, falling back to `corepack pnpm dlx` or `yarn dlx` automatically.
- Tests use Vitest with deterministic reporters by default.
```

