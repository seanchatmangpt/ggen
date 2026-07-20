from pathlib import Path
import hashlib
import json

root = Path(__file__).resolve().parents[1]
code = root / "code"
# The book lives at <repo>/book/ inside the ggen repo. The TCPS reference
# tree and the pack source manifests are the repo's canonical copies (the
# book's own snapshots were replaced by pointer READMEs — see
# code/packs/tcps-*-pack/README.md and code/case-studies/
# tcps-v26.7.19-reference/README.md).
repo = root.parent
reference = repo / "packs/tcps-core-pack/reference/製品版"
core_manifest = repo / "packs/tcps-core-pack/source-manifest.json"
release_manifest = repo / "packs/tcps-release-pack/source-manifest.json"
required = [
    code / "packs/canonical-level-five-pack/ontology.ttl",
    code / "packs/canonical-level-five-pack/gates/010_required.rq",
    code / "packs/canonical-level-five-pack/gates/020_single_valued.rq",
    code / "packs/canonical-level-five-pack/gates/030_value_constraints.rq",
    code / "packs/canonical-level-five-pack/templates/catalog.rs.tmpl",
    core_manifest,
    release_manifest,
    code / "examples/canonical-level-five-consumer/ggen.toml",
    reference / "Cargo.toml",
]
for path in required:
    if not path.exists():
        raise SystemExit(f"missing: {path}")

reference_files = sorted(p for p in reference.rglob("*") if p.is_file())
core = json.loads(core_manifest.read_text(encoding="utf-8"))
release = json.loads(release_manifest.read_text(encoding="utf-8"))
if len(reference_files) != 132:
    raise SystemExit(f"expected 132 TCPS reference files; found {len(reference_files)}")
if len(core) + len(release) != 132:
    raise SystemExit("TCPS pack manifests do not cover all 132 reference files")
manifest_paths = {row["path"] for row in core + release}
if len(manifest_paths) != 132:
    raise SystemExit("TCPS pack manifests contain duplicate paths")
for row in core + release:
    path = reference / row["path"]
    digest = hashlib.sha256(path.read_bytes()).hexdigest()
    if digest != row["sha256"]:
        raise SystemExit(f"digest mismatch: {row['path']}")
print(f"OK: TCPS reference=132, core={len(core)}, release={len(release)}")
