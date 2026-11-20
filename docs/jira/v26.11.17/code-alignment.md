# code-alignment — ggen v26.11.17

Standing: PASS

## Identity
- Path: `/Users/sac/ggen`
- Branch at generation: main
- HEAD: 4e8cd42ef1e5a20dae254c9510da4b97e9e27c96
- Tracked files: 18957
- Tracked docs markdown: 1485

## Build manifests (tracked at the commit above)
- `Cargo.toml`
- `justfile`
- `Dockerfile`
- `ggen.toml`
- `.tool-versions`
- `CLAUDE.md`
- `AGENTS.md`
- `README.md`

## Top-level layout (tracked file counts)
- `.`: 73
- `.agent`: 8
- `.agent-admissibility`: 29
- `.cargo`: 2
- `.cargo-cicd`: 3
- `.chatmangpt`: 1
- `.claude`: 73
- `.claude-plugin`: 1
- `.gemini`: 8
- `.ggen`: 3
- `.ggen-v2`: 5
- `.githooks`: 2
- `.github`: 106
- `.helix`: 1
- `.sa2a`: 1
- `.sjira`: 1
- `.specify`: 535
- `.tcps`: 11
- `.venv_shacl`: 21
- `analysis`: 30
- `architecture`: 7
- `artifacts`: 34
- `authority`: 1
- `benches`: 22
- `boilerplate`: 67
- `book`: 793
- `cgen`: 1
- `config`: 1
- `crates`: 1345
- `dd-ui`: 2
- `debian`: 5
- `deploy`: 15
- `docker`: 2
- `docs`: 1717
- `dod-projection`: 16
- `ecosystem`: 1
- `enterprise-architecture-as-strategy`: 60
- `evidence`: 11
- `examples`: 3289
- `figures`: 4
- `fusion-thesis`: 11
- `generations`: 3
- `ggen-skills`: 3
- `graphs`: 1
- `marketplace`: 859
- `mutations`: 3
- `my-ontology-project`: 6
- `ocel`: 3
- `ontologies`: 1174
- `ontology`: 29
- `ontology_catalogue`: 4096
- `packs`: 2142
- `phd-thesis`: 62
- `playground`: 60
- `plugins`: 2
- `queries`: 8
- `receipts`: 9
- `registry`: 1
- `research-paper`: 2
- `schema`: 2
- `scripts`: 452
- `self-host`: 43
- `semantic`: 1
- `semconv`: 16
- `specify`: 37
- `specs`: 156
- `src`: 2
- `tai-erlang-autonomics`: 55
- `templates`: 233
- `tests`: 368
- `tickets`: 1
- `tools`: 53
- `vendors`: 756
- `verification`: 1

## Prior doc waves
- `docs/jira/v26.11.5`
- `docs/jira/v26.11.6`
- `docs/jira/v26.11.8`

## Falsifiers
- Any tracked path above absent at HEAD, or a count differing from `git ls-files`.
- HEAD sha not equal to or an ancestor of the live HEAD.

## Acceptance
```bash
python3 ~/zcode-cli/scripts/docs_v261117.py validate /Users/sac/ggen
```
Expected: exit 0 and `PASS`.

## See Also
- `~/zcode-cli/scripts/docs_v261117.py`
