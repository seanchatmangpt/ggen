# CMD G0 Exact-Tree Admission

This checkpoint extends the existing ggen self-host observer. It does not create a competing repository authority and does not change production behavior.

The observer binds the externally supplied exact revision to the recursive Git tree, stage-zero index, tracked object modes, object types, object identities, blob bytes, symlink target bytes, and gitlink commit identities. Untracked files and working-tree changes are recorded separately and are never admitted as HEAD authority.

The executor emits an atomic, content-addressed observation with linked intent and result receipts while retaining `UNKNOWN` standing. A separate verifier recomputes the Git object set, verifies receipts and replay, executes a copied-evidence omission sabotage, and may report only the checkpoint ceiling `PARTIAL_ALIVE`.

Stable command:

```bash
python3 self-host/scripts/run_checkpoint.py \
  --root . \
  --expected-revision "$(git rev-parse HEAD)"
```

Expected omission refusal:

```text
REFUSED: CMD-G0-EXACT-SET
```
