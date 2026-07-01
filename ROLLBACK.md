# Release Rollback Plan (v26.7.1)

In case of critical failures after release v26.7.1, the following rollback steps must be executed:

1. **Revert the Release Tag**:
   ```bash
   git push --delete origin v26.7.1
   git tag -d v26.7.1
   ```

2. **Revert Main Branch**:
   ```bash
   git checkout main
   git revert -m 1 <merge_commit_sha>
   git push origin main
   ```

3. **Demote Cargo Publish** (if already published):
   ```bash
   cargo yank --vers 26.7.1
   ```

4. **Incident Response**:
   Notify the team in the #releases channel and trigger an immediate incident review.
