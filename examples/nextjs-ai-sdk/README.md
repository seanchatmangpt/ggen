# Next.js AI SDK ggen example

A self-contained ggen pack and consumer for Next.js 16, AI SDK 7, AI Elements, shadcn/ui, Better Auth, Neon/PostgreSQL, Drizzle, OpenTelemetry, tool approval, and BLAKE3 receipts. Projected code is canonical source; there is no `generated/` directory.

## Layout

- `pack/` — reusable ontology, admission gates, and templates
- `schema/domain.ttl` — admitted application/model/agent/tool instance
- `app/`, `components/`, `lib/` — first-class projected application source, including the owned AI Elements/shadcn adaptation
- `ADVERSARIAL_REVIEW.md` — defects found in the incoming artifact and repairs

## Verification

```bash
cp .env.example .env.local
ggen sync run
corepack enable
pnpm install
pnpm verify
ggen sync run
git diff --exit-code
```

`pnpm verify` refuses absent real service configuration rather than substituting in-memory state, a fake model, or fabricated receipts.
