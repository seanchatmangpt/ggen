# Claude Code Skills — ggen

Modular, discoverable skill documentation for this repo.

## How discovery actually works

Claude Code's built-in skill loader discovers skills by scanning
`.claude/skills/<name>/SKILL.md` — a flat directory per skill, each containing exactly one
`SKILL.md` file with YAML frontmatter (`name`, `description`) followed by Markdown body content.
That is the entire mechanism. There is no metadata index, no `requires`/dependency resolution, no
staged content loading, and no `.claude/hooks/*.sh` or `.claude/settings.json` wiring that
implements anything beyond this scan — a prior version of this directory (and this README)
described a custom `_metadata/` progressive-disclosure system with token-budget tiers and
dependency graphs; none of that was ever implemented by the harness, and `_metadata/` has been
removed (2026-08-03 audit).

A file directly under `.claude/skills/` (not inside a `<name>/` directory) or nested more than
one level deep (e.g. the former `rdf/ontologies.md`, `rust/cargo-make.md`) is invisible to the
loader. It will never appear in the "available skills" listing and can never be invoked via the
`Skill` tool, no matter how good its content is.

## Directory Structure

```
.claude/skills/
├── andon-stop/SKILL.md              # Stop-the-line protocol on compiler/test/clippy signals
├── cargo-make-runner/SKILL.md       # Historical cargo-make target reference (see note below)
├── cargo-make-protocol/SKILL.md     # Historical cargo-make target reference (see note below)
├── otel-span-verifier/SKILL.md      # OTEL span verification for LLM/MCP/pipeline features
├── rdf-ontologies/SKILL.md          # RDF/SHACL ontology editing patterns for .specify/*.ttl
├── rdf-sparql/SKILL.md              # SPARQL query patterns for the μ₂ extract phase
├── sync-executor/SKILL.md           # ggen sync five-stage pipeline (ggen-engine, current)
├── speckit-git-commit/SKILL.md      # Spec Kit skills (leave alone — externally maintained)
├── speckit-git-feature/SKILL.md
├── speckit-git-initialize/SKILL.md
├── speckit-git-remote/SKILL.md
├── speckit-git-validate/SKILL.md
└── README.md                        # This file
```

## Known content caveat: `cargo make` vs `just`

`cargo-make-runner/SKILL.md` and `cargo-make-protocol/SKILL.md` both instruct routing all Rust
build/test/lint commands through `cargo make <target>`. That contradicts this repo's current,
actual entry point: `just <task>` (see `.claude/rules/_core/absolute.md` rule 4 — "ALWAYS
`just <task>`. NEVER call `cargo make` or bare `cargo` directly" — and `CLAUDE.md`'s command
table). `Makefile.toml` is historical reference only. Both skills predate the `just`-based
workflow and were relocated as part of the 2026-08-03 structural cleanup without a content
rewrite (out of scope for that pass — it only had to make files discoverable, not correct their
internal advice). Treat their command examples as outdated; use `just <task>` instead, per
`.claude/rules/_core/workflow.md` and the root `justfile`.

## Adding a New Skill

1. Create `.claude/skills/<kebab-case-name>/SKILL.md`.
2. Give it YAML frontmatter with `name` (human-readable) and `description` (what it does and
   when to trigger it — this is what appears in the "available skills" listing shown to Claude).
3. Write the skill body in Markdown below the frontmatter.

No other registration step exists or is needed — the loader picks it up automatically.

## Skills Deliberately Left Alone

The five `speckit-git-*/SKILL.md` directories are externally maintained by the Spec Kit
integration (`compatibility`/`metadata.source` frontmatter pointing at
`git:commands/speckit.git.*.md`) and were already correctly shaped before this cleanup. They are
out of scope for this repo's own skill-authoring conventions above.
