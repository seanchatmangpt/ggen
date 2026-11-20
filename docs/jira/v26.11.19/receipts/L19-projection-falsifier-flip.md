# L19 projection falsifier flip (Skeptic MEDIUM)
Standing: ALIVE

## Identity

- Subject: `crates/ggen-sjira/tests/projection_test.rs`
- Repo: `/Users/sac/ggen`
- Branch: `main` → `origin/preserve/v26.11.16-main`
- Finding: Skeptic L19 MEDIUM — `falsifiers: []` vacated `FalsifierGatesMissing`;
  court no longer flipped on unbound projection regression.

## Consequence

- Restored real order falsifier `urn:semantic-jira:falsifier:EXEC-1:f1`.
- Seeded `projections/c1.unbound.rq` in `mini_pack`.
- Added Chicago court `unbound_projection_regression_flips_falsifier_gates_missing`
  (BASELINE pass → delete unbound → FLIP `FalsifierGatesMissing`).
- Staging-leak differential serialized under private `TMPDIR` + process lock.

## Replay

```sh
cd /Users/sac/ggen && cargo test -p ggen-sjira --test projection_test -- --nocapture
# EXIT=0 — 13 passed; 0 failed (includes unbound_projection_regression_flips_falsifier_gates_missing)
```

| Command | EXIT | Count |
|---|---|---|
| `cargo test -p ggen-sjira --test projection_test` | 0 | 13 passed, 0 failed |

## Standing

ALIVE — flip proven by Chicago cargo test; unbound deletion refuses
`FalsifierGatesMissing` while seeded unbound + real falsifiers pass.
