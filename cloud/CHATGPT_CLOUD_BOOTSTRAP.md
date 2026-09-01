# ChatGPT Cloud Bootstrap

This document defines the cross-instance bootstrap contract for the ggen ecosystem.

## Required property

A ChatGPT cloud instance MUST be able to acquire and execute ggen without relying on any filesystem state, shell history, environment variables, or files created by a previous ChatGPT cloud instance.

The portable boundary is GitHub, not `/mnt/data`.

## Discovery

The canonical rendezvous is the GitHub pull request whose title contains `ChatGPT cloud bootstrap` in `seanchatmangpt/ggen`. Its body carries the current exact-head release tag and bootstrap manifest identity.

A fresh ChatGPT instance should use the connected GitHub app to find that PR. It MUST NOT infer a current artifact from a local cache or from a prior conversation.

## Durable distribution

A crowned environment is published as an immutable GitHub prerelease tagged:

```text
chatgpt-cloud-<exact-40-character-ggen-sha>
```

The prerelease contains:

- `ggen-chatgpt-bootstrap.json`
- `ggen-cloud-runtime-<sha>.zip`
- `ggen-source-ggen-marketplace-<sha>.zip`
- `ggen-source-ggen-legacy-<sha>.zip`
- `ggen-source-ggen-create-<sha>.zip`
- `ggen-source-ggen-mcp-<sha>.zip`
- `ggen-source-ggen-spec-kit-<sha>.zip`

Release assets are durable distribution objects. GitHub Actions artifacts remain CI receipts/caches and are not the cross-instance source of truth.

## Admission and replay

For a fresh instance:

1. Resolve the canonical bootstrap PR and exact release tag.
2. Fetch `ggen-chatgpt-bootstrap.json` from that release.
3. Verify its `schema`, `repository`, `head_sha`, release tag, and asset digests.
4. Download the runtime ZIP into the fresh container.
5. Verify the ZIP digest from the bootstrap manifest.
6. Extract the ZIP and verify `ggen-cloud-runtime.tar.gz.sha256`.
7. Extract `ggen-cloud-runtime.tar.gz`.
8. Execute `ggen-cloud-runtime/verify.sh`.
9. Only after that exact execution may the instance claim `ALIVE` for the ggen/DfCM runtime.
10. Acquire source capsules only when the wider ecosystem closure is needed. Source presence alone remains `PARTIAL_ALIVE` until those repo runtimes are executed.

## Authority

- **SELECT:** exact release tag + manifest + digests.
- **CONSTRUCT:** extraction into the current ephemeral instance.
- **DO:** bounded verifier execution only.
- **RECEIPT:** exact subject SHA, release identity, asset digest, inner transport digest, verifier output.

A prior ChatGPT instance has no ambient authority in a new instance. Its receipt can identify what to reacquire; it cannot substitute for replay.

## Falsifiers

The bootstrap is not `ALIVE` if any of these occur:

- the release/tag does not resolve;
- the release points at a different commit;
- an asset digest differs from the bootstrap manifest;
- an inner SHA-256 receipt fails after transport;
- the runtime verifier does not execute successfully in the current instance;
- the bootstrap requires a path or file inherited from a prior instance.
