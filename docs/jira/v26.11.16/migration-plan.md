# ggen migration plan v26.11.16

Standing: UNVERIFIED

## Identity
- Path: /Users/sac/ggen
- Remote: https://github.com/seanchatmangpt/ggen.git
- Branch: main@4e8cd42ef (upstream origin/main, ahead 122)
- Last commit: 2026-11-08 ("docs(v26.11.8): e2e survey receipt final rows (repair complete)")
- Commits: 4396; size: 1437 MB (.git alone 648 MB)
- Languages/build: Rust workspace, 12-14 crates (cargo.toml, pinned nightly), justfile, book/,
  deny.toml, Dockerfiles; CLAUDE.md + AGENTS.md + CONSTITUTION.md
- Nested repos: none (`crates/ggen-architecture` is an excluded nested workspace, not a repo)

## Observed state
- Dirty: 84 tracked, untracked: 0. Grouped: packs/ (62, mostly ` D packs/gh-terraform-pack/
  gates/*.rq` deletions), vendors/tai-erlang-autonomics (14 modified .tf/.tfvars),
  crates/ggen-engine/tests/gh_*_e2e.rs (3), examples/ (2), templates/, justfile, .gitignore.
- Ahead 122 / behind 0 — 122 local commits on main NOT pushed.
- Stash: 4177 (census) — enormous; stash list not enumerated here.
- Local branches: 293 total, 148 unpushed (agent/*, agent-*-import, archive/wip/*,
  backup/pre-v26.7.3-local, worktree branches).
- Build/dep dirs: no `target/` at root. `vendors/` 13 MB.
- Submodules: none.

## What it is
RDF/ontology-driven code-generation engine: Turtle ontology + Tera templates + SPARQL, driven by
`ggen sync run`, with BLAKE3-chained receipts. The hub repo for most other repos' `ggen.toml`.

## Classification
ACTIVE (census agrees).

## Migration risk
- 122 unpushed commits on main — HIGH-value, would be lost.
- 148 unpushed local branches (agent fan-out history, archives, a `backup/pre-v26.7.3-local`).
- 4177 stashes.
- 84 dirty files incl. terraform vendors edits and pack gate deletions.
- README/CLAUDE.md contain unresolved `<<<<<<< GENERATED` conflict markers (committed?).
Rating: HIGH.

## Recommended action
COMMIT-PUSH

1. `git push origin main` FIRST (122 commits; behind 0 so no conflict expected).
2. Snapshot dirty state on a branch: `git checkout -b wip/pre-migration-2026-11-16 &&
   git add -A && git commit -m "wip: pre-migration snapshot 2026-11-16" && git push -u origin HEAD`.
   DECISION: the 62 `packs/gh-terraform-pack/gates/*.rq` deletions — intentional?
3. Push every local branch: `git push --all origin` (148 branches; DECISION: prune
   `worktree-*`/`agent-*` branches first with `git branch --merged main`).
4. Preserve stashes: `tar czf /Volumes/archive/ggen-dotgit.tgz -C /Users/sac/ggen .git`
   and `git bundle create /Volumes/archive/ggen-full.bundle --all`.
5. Check for secrets in vendors terraform edits before pushing:
   `git diff -- vendors | grep -i -E 'secret|token|key' | head`.
6. New machine: clone; do not rsync 1.4 GB tree (no target/ to clean here).

## First commands on the new machine
`rustup show` (nightly pin) then `cargo check --workspace` and `just --list`. Stated, not run.

## See Also
- ../v26.9.18/
- /Users/sac/docs/jira/v26.11.16/README.md
