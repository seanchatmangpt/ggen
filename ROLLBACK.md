# Rollback Plan for v26.7.1

## Triggers for Rollback
- Critical regressions in the `ggen sync` pipeline affecting artifact generation.
- Production failures in observability or evidence validation.
- Unhandled syntactical errors in the default `cli-commands.ttl`.

## Execution Steps
1. Revert to `v26.6.25`:
   ```bash
   git checkout v26.6.25
   ```
2. Re-run `ggen sync` to restore the previous generation artifacts:
   ```bash
   cargo run -p ggen-cli-lib --bin ggen -- sync --audit true
   ```
3. Run the validation gates:
   ```bash
   just check && just test
   ```
4. Verify the system states are stable and artifacts match the `v26.6.25` hashes.
5. Communicate the rollback incident via the incident management channel.
