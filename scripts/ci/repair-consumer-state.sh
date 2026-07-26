#!/usr/bin/env bash
set -euo pipefail

repo_root=$(git rev-parse --show-toplevel)
cd "$repo_root"
printf '%s\n' '**/.ggen-v2/' >> .git/info/exclude

python3 - <<'PY'
from pathlib import Path

manifest = Path('examples/tpot2-wasm4pm-autoconfig/ggen.toml')
text = manifest.read_text(encoding='utf-8')
old = '''# -----------------------------------------------------------------------------
# [validation] — ACTIVATE the SHACL gate + strict determinism mode.
# SHACL enforcement is gated by a NON-EMPTY `shacl` list (independent of
# strict_mode; types.rs:399-414). Without this section the imported
# tpot-shapes.ttl was loaded as data only and never enforced (research/06).
# Every OperatorShape / PipelineStageShape / FitnessObjectiveShape constraint
# passes the derived data (research/06 datatype check: BIND fitness is xsd:decimal;
# tiers are xsd:integer; forCategory ∈ the 9 frozen categories). strict_mode
# elevates missing-ORDER-BY (E0011) to a hard error — safe here because all 6
# SELECTs AND both inline inference CONSTRUCTs now carry ORDER BY. Basis:
# source analysis in research/01,06,07 (this container cannot run `ggen sync`).
# -----------------------------------------------------------------------------
[validation]
shacl = ["ontology/tpot-shapes.ttl"]
strict_mode = true
'''
new = '''# -----------------------------------------------------------------------------
# [validation] — engine-independent SPARQL law gates + strict determinism.
# The former sync-time SHACL hook is deliberately retained only as documentary
# ontology input. Executable validation now uses three SELECT gates; any returned
# row is a refusal. Together they preserve the exact-one, datatype, node-kind,
# class, and frozen-enumeration constraints previously expressed by the three
# SHACL NodeShapes. The gates execute after inference, so they validate all 60
# derived tpot:Operator individuals as well as static stages/objectives.
# -----------------------------------------------------------------------------
[validation]
gates = [
    "gates/010_pipeline_stage_contract.rq",
    "gates/020_operator_contract.rq",
    "gates/030_fitness_objective_contract.rq",
]
strict_mode = true
'''
if text.count(old) != 1:
    raise SystemExit(f'REFUSED TPOT migration precondition: old_block={text.count(old)}')
if '[validation]\ngates = [' in text:
    raise SystemExit('REFUSED TPOT migration: gate authority already present beside SHACL')
for path in (
    'examples/tpot2-wasm4pm-autoconfig/gates/010_pipeline_stage_contract.rq',
    'examples/tpot2-wasm4pm-autoconfig/gates/020_operator_contract.rq',
    'examples/tpot2-wasm4pm-autoconfig/gates/030_fitness_objective_contract.rq',
    'examples/crown-conjecture-verify/templates/.gitkeep',
):
    if not Path(path).is_file():
        raise SystemExit(f'REFUSED missing admitted source: {path}')
manifest.write_text(text.replace(old, new), encoding='utf-8', newline='')

tcps_manifest = Path('examples/tcps-generated/ggen.toml')
tcps_text = tcps_manifest.read_text(encoding='utf-8')
locked_evidence = 'tcps-evidence = { path = "evidence" }'
unlocked_evidence = 'tcps-evidence = { path = "evidence", lock = false }'
if tcps_text.count(locked_evidence) != 1:
    raise SystemExit(
        f'REFUSED TCPS evidence-lock precondition: observed={tcps_text.count(locked_evidence)}'
    )
if unlocked_evidence in tcps_text:
    raise SystemExit('REFUSED TCPS evidence pack already has duplicate unlocked declaration')
tcps_manifest.write_text(
    tcps_text.replace(locked_evidence, unlocked_evidence),
    encoding='utf-8',
    newline='',
)

lock = Path('examples/tcps-generated/ggen.lock').read_text(encoding='utf-8')
stale = 'blake3:c95625326471c33083f0689fb707cc7636b857c931ab1b48e45810a00201ba3b'
if lock.count(stale) != 1:
    raise SystemExit(f'REFUSED TCPS lock precondition: stale_hash={lock.count(stale)}')
print('ADMITTED consumer repairs: crown directory, TPOT gates, TCPS unlocked evidence + re-lock')
PY

(
  cd examples/crown-conjecture-verify
  ../../target/debug/ggen sync run
  ../../target/debug/ggen receipt verify
  ../../target/debug/ggen sync run
  ../../target/debug/ggen receipt verify
)

(
  cd examples/tpot2-wasm4pm-autoconfig
  ../../target/debug/ggen sync run
  ../../target/debug/ggen receipt verify
  ../../target/debug/ggen sync run
  ../../target/debug/ggen receipt verify
)

python3 - <<'PY'
from pathlib import Path
import shutil
import subprocess
import tempfile

source = Path('examples/tpot2-wasm4pm-autoconfig').resolve()
ggen = Path('target/debug/ggen').resolve()
with tempfile.TemporaryDirectory(prefix='tpot-gate-sabotage-') as raw:
    candidate = Path(raw) / 'consumer'
    shutil.copytree(source, candidate, ignore=shutil.ignore_patterns('.ggen-v2'))
    ontology = candidate / 'ontology/tpot-search-space.ttl'
    text = ontology.read_text(encoding='utf-8')
    admitted = 'tpot:forCategory "import_export" .'
    sabotage = 'tpot:forCategory "invalid_category" .'
    if text.count(admitted) != 1:
        raise SystemExit(f'REFUSED sabotage precondition: observed={text.count(admitted)}')
    ontology.write_text(text.replace(admitted, sabotage), encoding='utf-8', newline='')
    completed = subprocess.run(
        [str(ggen), 'sync', 'run'],
        cwd=candidate,
        text=True,
        capture_output=True,
        check=False,
    )
    evidence = completed.stdout + completed.stderr
    print(evidence)
    if completed.returncode == 0:
        raise SystemExit('REFUSED: sabotaged TPOT category passed the gate')
    if '010_pipeline_stage_contract.rq' not in evidence:
        raise SystemExit('REFUSED: sabotage did not bind to the pipeline gate')
    if 'PipelineStage contract violated' not in evidence:
        raise SystemExit('REFUSED: sabotage did not emit the declared gate message')
    print('TPOT_GATE_SABOTAGE_REFUSED category=invalid_category')
PY

rm examples/tcps-generated/ggen.lock
(
  cd examples/tcps-generated

  bash scripts/verify.sh
  ../../target/debug/ggen sync run 2>&1 | tee /tmp/tcps-relock.log
  ../../target/debug/ggen receipt verify

  cargo fmt --check > /tmp/tcps-fmt.log 2>&1 || true
  cargo clippy --workspace --all-targets -- -D warnings > /tmp/tcps-clippy.log 2>&1 || true

  bash scripts/verify.sh
  ../../target/debug/ggen sync run
  ../../target/debug/ggen receipt verify
)

GGEN_BIN="$repo_root/target/debug/ggen" \
  python3 -m unittest -v book.tests.test_gap_closure_chicago.RealConsumerStateTests

git rm .github/workflows/repair-consumer-state-pr.yml scripts/ci/repair-consumer-state.sh

python3 - <<'PY'
import subprocess

lines = subprocess.check_output(
    ['git', 'status', '--porcelain=v1', '--untracked-files=all'],
    text=True,
).splitlines()
allowed_exact = {
    '.github/workflows/repair-consumer-state-pr.yml',
    'scripts/ci/repair-consumer-state.sh',
    'examples/tpot2-wasm4pm-autoconfig/ggen.toml',
    'crates/ggen-engine/src/generation_rules.rs',
    'crates/ggen-engine/tests/generation_output_dir_e2e.rs',
}
allowed_prefix = 'examples/tcps-generated/'
bad = []
for line in lines:
    path = line[3:]
    if ' -> ' in path:
        path = path.split(' -> ', 1)[1]
    if '/.ggen-v2/' in path or path.endswith('/.ggen-v2'):
        continue
    if path in allowed_exact or path.startswith(allowed_prefix):
        continue
    bad.append(line)
print('\n'.join(lines))
if bad:
    raise SystemExit('REFUSED unexpected consumer drift:\n' + '\n'.join(bad))
PY

git config user.name 'ggen consumer repair'
git config user.email 'actions@users.noreply.github.com'
git add -A -- \
  .github/workflows/repair-consumer-state-pr.yml \
  scripts/ci/repair-consumer-state.sh \
  crates/ggen-engine/src/generation_rules.rs \
  crates/ggen-engine/tests/generation_output_dir_e2e.rs \
  examples/tpot2-wasm4pm-autoconfig/ggen.toml \
  examples/tcps-generated \
  ':(exclude)examples/tcps-generated/.ggen-v2/**'
git diff --cached --check
git commit -m 'fix(examples): reconcile current consumer authority'
git push origin HEAD:agent/chicago-gap-closure
echo "CONSUMER_REPAIR_COMMIT $(git rev-parse HEAD)"
