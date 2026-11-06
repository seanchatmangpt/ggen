# XD-8: Fail-Closed Mode Gate for Pre-Existing Signing Keys

Standing: ALIVE

Date: 2026-11-04
Repo: `/Users/sac/ggen` (lane-owned: `crates/ggen-engine/src/keys.rs`, this receipt)
Spec: `docs/jira/v26.11.3/receipts/xd-2-key-perms.md` (follow-up section)

## Subject

Landed the XD-2 follow-up: `resolve_signing_key`
(`crates/ggen-engine/src/keys.rs:115`) now refuses a PRE-EXISTING
`signing.key` whose Unix mode has any group/other bit set
(`mode & 0o077 != 0`) with a new typed hard error `[FM-KEY-013]` naming the
path, observed mode (octal), and remediation (`chmod 600` + rotate if
exposed). Generation-time enforcement (`restrict_to_owner`, `[FM-KEY-009]`)
cannot retroactively fix pre-existing files; this closes that gap
fail-closed — the file is never silently accepted and never silently
chmod'd (a human decides on rotation).

## Change (keys.rs only)

- New `ensure_owner_only_permissions(&signing_path)` helper, `#[cfg(unix)]`,
  mirrors the `restrict_to_owner` style (PermissionsExt, fm_key error).
- Called at the top of the `Ok(raw)` arm of `resolve_signing_key` (only on
  the pre-existing-file path; generation still writes 0600 itself).
- Module doc + inline comments updated; no non-unix behavior change (the
  call site is `#[cfg(unix)]`-gated; no non-unix stub needed since the
  helper is only referenced under the same cfg).
- New test `overly_permissive_preexisting_signing_key_refuses`: real 0644
  file with valid seed content, asserts `[FM-KEY-013]`, path in message,
  observed mode `644`, `chmod 600` remediation, and content+mode untouched
  (fail-closed, not fail-and-fix). Chicago: real file I/O, no mocks.
- Two pre-existing tests (`never_overwrites_an_existing_signing_key_file`,
  `poisoned_zero_byte_signing_key_self_heals_on_next_resolve`) wrote their
  fixtures at the default 0644 and were correctly caught by the new gate;
  they now chmod 600 their fixture files (a lawful pre-existing key IS
  owner-only), preserving their original intent.

## Commands / Exits

```
$ cargo test -p ggen-engine --lib keys
test result: ok. 19 passed; 0 failed; 0 ignored; 0 measured; 219 filtered out
exit=0
$ cargo test -p ggen-engine --lib keys        # second run
test result: ok. 19 passed; 0 failed
exit=0
$ cargo test -p ggen-engine --lib \
    overly_permissive_preexisting_signing_key_refuses
test keys::tests::overly_permissive_preexisting_signing_key_refuses ... ok
test result: ok. 1 passed; 0 failed
exit=0
$ cargo check --workspace
Finished `dev` profile [unoptimized + debuginfo] target(s) in 1m 25s
exit=0
```

All runs executed 2026-11-04, exit 0. (Note: `cargo test ... keys` initially
failed 2 pre-existing tests at 0644 — that IS the gate working; fixtures
fixed as described above, then green x2.)

## Standing

Code check = ALIVE (`cargo test -p ggen-engine --lib keys` green x2 + the
focused new test + `cargo check --workspace` exit 0, all this session).
Negative-path refusal = ALIVE (the two 0644-fixture failures above are a
witnessed firing of `[FM-KEY-013]` on real files, plus the dedicated typed
assertions). Falsifier: a 0644 `signing.key` that `resolve_signing_key`
still accepts.

## See Also

`docs/jira/v26.11.3/receipts/xd-2-key-perms.md`
