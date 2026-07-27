# nextjs-ai-verify

This is the clean consumer and canonical projection target for `nextjs-ai-sdk-pack`.

```bash
ggen sync run
pnpm install
pnpm ui:sync
pnpm verify
```

The checked-in source files in this fixture are the expected first-class projections. They are not segregated into a `generated/` directory. A second sync must produce no byte changes.

Before a real deployment, replace fixture registrations in `lib/ai/tool-handlers.ts` with transactional adapters and persist tool receipts.
