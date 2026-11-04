# XD-2: Key File Permission Remediation (XC-5r rec #6)

Date: 2026-11-04
Repo: `/Users/sac/ggen` (lane-owned: `.ggen/keys/*` modes, this receipt)

## Subject

`.ggen/keys/` key files were world-readable (0644). `signing.key` is a raw
ed25519 secret seed (64 hex chars); `private.pem` is the repo's private key
material. World-readability on a multi-user host exposes the seed to any local
account. `public.pem`/`verifying.key` are public halves — 0644 is correct.

## Commands / Exits

```
$ chmod 600 /Users/sac/ggen/.ggen/keys/private.pem /Users/sac/ggen/.ggen/keys/signing.key
$ chmod 644 /Users/sac/ggen/.ggen/keys/public.pem /Users/sac/ggen/.ggen/keys/verifying.key
$ ls -l /Users/sac/ggen/.ggen/keys/
-rw-------  private.pem    (0600)
-rw-r--r--  public.pem     (0644)
-rw-------  signing.key    (0600)
-rw-r--r--  verifying.key  (0644)
exit=0
```

Key file contents unchanged (64 bytes each, mtimes preserved: Aug 18 / Jun 27).

## Verify path run (post-change)

```
$ ggen receipt verify            # exit 0
valid: true, signed: true, signature_valid: true
chain_standing: fully-bound, outputs: 53
chain_hash: 4e0210e4f0e13e4b2e3e06093cb85a0eb351df3118f719a045315e97abf5f7d7
```

The verify path resolved the 0600 `signing.key`/`verifying.key` and validated
the existing receipt signature — the chmod did not disturb the signing chain.

## Fail-closed permission check at signing time — decision: FOLLOW-UP, not landed

Read-first findings:

- `crates/ggen-engine/src/verbs/receipt_impl.rs` (the path named in the task)
  **does not exist**. Receipt verbs live in the GENERATED
  `crates/ggen-engine/src/verbs/receipt.rs` (header: "Do not edit by hand").
- The actual signing/verification key seam is
  `crates/ggen-engine/src/keys.rs`:
  - `resolve_signing_key` (`keys.rs:115`) reads `signing.key` on the signing
    path (called from `sync.rs:4580`).
  - `resolve_verifying_key` (`keys.rs:170`) reads `verifying.key` on the
    verify path (called from `verbs/handlers.rs:531`).
- Generation time ALREADY enforces 0600 fail-closed: `restrict_to_owner`
  (`keys.rs:402`, `[FM-KEY-009]`) + test `signing_key_file_is_written_with_owner_only_permissions`
  (`keys.rs:918`). The gap is only **pre-existing** key files created before
  that enforcement (exactly this repo's state: Aug 18/Jun 27 files at 0644).

The check itself is a small coherent diff (~15 lines: unix-only
`mode & 0o077 != 0` → new `[FM-KEY-013]` hard error in
`resolve_signing_key`, plus one negative test in `keys.rs`'s existing test
module). **Not landed** because the correct insertion point is `keys.rs`,
which is outside this lane's owned paths (and the generated verbs file may
not be hand-edited). Recorded here as the follow-up:

> In `crates/ggen-engine/src/keys.rs::resolve_signing_key`, after the
> `Ok(raw)` arm, add (#[cfg(unix)]): read `metadata(signing_path)`; if
> `permissions().mode() & 0o077 != 0`, return
> `AppError::fm_key(13, ...)` naming the path, observed mode, and remediation
> (`chmod 600`). Add test `overly_permissive_preexisting_signing_key_refuses`
> (write signing.key at 0644, assert `[FM-KEY-013]`, assert file untouched).

Standing: chmod = ALIVE (ls -l output above); verify path = ALIVE (exit 0,
signature_valid true); code check = FOLLOW-UP (UNVERIFIED by design, seam
identified with file:line).
