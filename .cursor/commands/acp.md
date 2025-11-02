Stage all git changes, commit with an auto-generated message, and push to the current branch.

Run these commands:
1. `git add -A` to stage all changes
2. `git commit -m "<message>"` with auto-generated commit message
3. `git push` to push to remote

Generate commit message based on changes:
- Documentation files (README, CHANGELOG, docs/): "docs: update documentation"
- Rust code (.rs): "refactor: code changes" or "feat:" or "fix:" based on context
- Dependencies (Cargo.toml, Cargo.lock): "chore: update dependencies"
- Configuration: "chore: update configuration"
- Default: "chore: update" or descriptive message from file changes

Use conventional commit format. Push to current branch automatically. Skip if no changes.

