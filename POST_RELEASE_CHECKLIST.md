# Post-Release Deployment Checklist for v26.7.1

## 1. Local & CI Verifications
- [ ] Ensure all local tests pass (`cargo test --workspace`).
- [ ] Ensure CI pipelines (e.g., GitHub Actions) are green for the `v26.7.1` tag.
- [ ] Verify `Cargo.toml` version is correctly set to `26.7.1`.
- [ ] Verify `CHANGELOG.md` is updated with `v26.7.1` release notes.

## 2. Publish to crates.io
- [ ] Run `cargo publish --dry-run` to verify packaging without actual publishing.
- [ ] Run `cargo publish` to publish the crate to crates.io (if this is a workspace, publish dependent crates first in topological order).
- [ ] Verify the new version `v26.7.1` is live and visible on [crates.io](https://crates.io/).

## 3. Source Control & GitHub Release
- [ ] Tag the release commit (if not already done) with `git tag v26.7.1`.
- [ ] Push tags to the remote (`git push origin v26.7.1`).
- [ ] Create a new GitHub Release for tag `v26.7.1`.
- [ ] Attach any necessary generated binaries, assets, or artifacts to the GitHub Release.
- [ ] Copy the `CHANGELOG.md` notes for `v26.7.1` into the GitHub Release description.
- [ ] Publish the GitHub Release.

## 4. Documentation & External Trackers
- [ ] Check docs.rs to ensure documentation for `v26.7.1` builds successfully and is up-to-date.
- [ ] Update external issue trackers (e.g., Jira, GitHub Projects, Linear): move resolved tickets to 'Done' and close the `v26.7.1` milestone.
- [ ] Announce the release on relevant communication channels (e.g., Discord, Slack, mailing lists, social media).
- [ ] Notify dependent internal projects or partners to upgrade to the new `v26.7.1` version.
