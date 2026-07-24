# Next.js AI SDK ggen pack example

A self-contained ggen example for Next.js 16, AI SDK 7, AI Elements, shadcn/ui, Better Auth, Drizzle, Neon, Vitest, Playwright, and Vercel. Projected code is canonical source; there is no `generated/` directory.

## Layout

- `pack/` — ontology, six admission gates, and deterministic templates
- `app/` — consumer ontology and checked-in projected application
- `ADVERSARIAL_REVIEW.md` — defects found in the supplied bundle and the bounded standing after repair

## Verification ladder

```bash
cd app
python3 scripts/verify-gates.py
ggen sync run
corepack enable
pnpm install
pnpm ui:sync
pnpm typecheck
pnpm test
pnpm build
pnpm test:e2e
pnpm verify:projection
ggen sync run
git diff --exit-code
```

Current standing is `PARTIAL_ALIVE` until that full ladder executes in a networked environment with `ggen`, Node.js 22, pnpm, browser dependencies, and service credentials.
