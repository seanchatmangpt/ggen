# Adversarial review

Status before repair: **BUILD_BROKEN / UNSUPPORTED**. The supplied receipt did not execute ggen, dependency installation, typecheck, unit tests, build, browser tests, migration, or deployment.

## Release-blocking findings repaired

1. Two Vitest regular expressions were over-escaped and could not match their intended subjects.
2. `@ai-sdk/gateway` was unnecessary; AI SDK 7 exports `gateway` directly.
3. The declared fallback model was inert. The agent now supplies the admitted fallback through Gateway provider options.
4. The chat route advertised Better Auth but admitted anonymous requests. It now refuses requests without a session and carries the authenticated user into the tool context.
5. The projection verifier merely printed fresh hashes and therefore proved nothing. It now compares canonical files against a committed manifest.
6. The original bundle mixed a top-level pack and example. This submission is self-contained under `examples/nextjs-ai-sdk-ggen-pack/`.

## Remaining bounded claims

- The note adapter remains a fixture and returns `PARTIAL_ALIVE`; it is not represented as durable storage.
- AI Elements and shadcn source are materialized by pinned CLIs, but registry payload bytes are not yet content-addressed.
- No lockfile is included because package installation could not be executed in the review runtime.
- Promotion to `ALIVE` requires the verifier ladder in `README.md`.
