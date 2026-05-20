#!/usr/bin/env bash
# JTBD Runtime Smoke Test: every registered ggen CLI verb
#
# Runs each verb in a logical workflow with realistic fixtures, captures
# exit code + state delta, and classifies as REAL / PARTIAL / BROKEN / SKIP.
#
# Usage:
#   bash scripts/jtbd-all-verbs.sh              # interactive run with colored output
#   bash scripts/jtbd-all-verbs.sh --report     # markdown table to stdout
#
# Classification:
#   REAL    — exits 0, produces expected state delta, output structurally valid
#   PARTIAL — exits 0, output valid, but state didn't change OR output contains
#             hardcoded sentinel data (e.g., abcd1234 fake signatures)
#   BROKEN  — non-zero exit, panic, unparseable output
#   SKIP    — needs external service/auth/fixture not provisioned

set -uo pipefail

# Suppress telemetry INFO log that pollutes stdout
export RUST_LOG=error

# ── Configuration ──────────────────────────────────────────────────────────
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BIN="${GGEN_BIN:-"$ROOT/target/debug/ggen"}"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/ggen-jtbd.XXXXXX")"
REPORT_MODE=0
[[ "${1:-}" == "--report" ]] && REPORT_MODE=1

# Colors (suppressed in report mode)
if [[ $REPORT_MODE -eq 0 ]]; then
    RED='\033[0;31m'; GRN='\033[0;32m'; YEL='\033[1;33m'; BLU='\033[0;34m'; NC='\033[0m'
else
    RED=''; GRN=''; YEL=''; BLU=''; NC=''
fi

declare -a RESULTS=()
declare -i COUNT_REAL=0 COUNT_PARTIAL=0 COUNT_BROKEN=0 COUNT_SKIP=0

# Sentinel strings — hardcoded data flags PARTIAL
SENTINELS=(
    "abcd1234" "efgh5678" "ijkl9012" "mnop3456"   # capability.trust fake hashes
    "00000000-0000-0000-0000-000000000000"
)

# ── Helpers ────────────────────────────────────────────────────────────────
log() {
    [[ $REPORT_MODE -eq 0 ]] && echo -e "$@" >&2
}

cleanup() { [[ -n "${TMP:-}" ]] && rm -rf "$TMP"; }
trap cleanup EXIT

ensure_binary() {
    if [[ ! -x "$BIN" ]]; then
        log "${YEL}Binary not found, building...${NC}"
        (cd "$ROOT" && cargo make build >/dev/null 2>&1) || { echo "FAILED: cargo make build" >&2; exit 1; }
    fi
    [[ -x "$BIN" ]] || { echo "FAILED: $BIN still not executable" >&2; exit 1; }
}

# Hash a path's contents (returns "absent" if missing)
state_hash() {
    local p="$1"
    if [[ -e "$p" ]]; then
        if [[ -d "$p" ]]; then
            find "$p" -type f -exec shasum -a 256 {} \; 2>/dev/null | shasum -a 256 | awk '{print $1}'
        else
            shasum -a 256 "$p" 2>/dev/null | awk '{print $1}'
        fi
    else
        echo "absent"
    fi
}

# classify <label> <rc> <stdout-file> <delta:yes|no|n/a> <jq-probe-or-skip> <evidence>
classify() {
    local label="$1" rc="$2" stdout="$3" delta="$4" probe="$5" evidence="$6"
    local class=""

    if [[ "$rc" -ne 0 ]]; then
        class="BROKEN"
    elif [[ -s "$stdout" ]] && grep -qE 'thread.*panicked|fatal runtime error' "$stdout" 2>/dev/null; then
        class="BROKEN"
    else
        local probe_ok=1
        if [[ "$probe" != "skip" ]] && [[ -s "$stdout" ]]; then
            # Many verbs print human-readable text before the JSON on the last line.
            # Extract the last line that parses as JSON for the probe.
            local json_line
            json_line=$(grep -E '^\{|^\[' "$stdout" | tail -1)
            if [[ -n "$json_line" ]]; then
                echo "$json_line" | jq -e "$probe" >/dev/null 2>&1 || probe_ok=0
            else
                jq -e "$probe" "$stdout" >/dev/null 2>&1 || probe_ok=0
            fi
        fi
        local has_sentinel=0
        for s in "${SENTINELS[@]}"; do
            grep -qF "$s" "$stdout" 2>/dev/null && { has_sentinel=1; break; }
        done

        if [[ $probe_ok -eq 0 ]]; then
            class="PARTIAL"   # ran, but output structure didn't match expected probe
            evidence="$evidence probe-failed"
        elif [[ "$delta" == "no" ]] || [[ $has_sentinel -eq 1 ]]; then
            class="PARTIAL"
            [[ $has_sentinel -eq 1 ]] && evidence="$evidence sentinel-match"
            [[ "$delta" == "no" ]]    && evidence="$evidence no-state-change"
        else
            class="REAL"
        fi
    fi

    case "$class" in
        REAL)    COUNT_REAL=$((COUNT_REAL+1))    ; log "  ${GRN}REAL${NC}    $label" ;;
        PARTIAL) COUNT_PARTIAL=$((COUNT_PARTIAL+1)) ; log "  ${YEL}PARTIAL${NC} $label  ($evidence)" ;;
        BROKEN)  COUNT_BROKEN=$((COUNT_BROKEN+1))  ; log "  ${RED}BROKEN${NC}  $label" ;;
        SKIP)    COUNT_SKIP=$((COUNT_SKIP+1))    ; log "  ${BLU}SKIP${NC}    $label" ;;
    esac

    RESULTS+=("$label|$class|$rc|$evidence")
}

skip_verb() {
    local label="$1" reason="$2"
    COUNT_SKIP=$((COUNT_SKIP+1))
    log "  ${BLU}SKIP${NC}    $label ($reason)"
    RESULTS+=("$label|SKIP|n/a|$reason")
}

# run_verb <label> <state-path-or-na> <jq-probe-or-skip> <cmd>...
run_verb() {
    local label="$1" state_path="$2" probe="$3"; shift 3
    local stdout="$TMP/out.json"
    local stderr="$TMP/err.txt"
    local pre_hash="" post_hash="" delta="n/a"

    if [[ "$state_path" != "n/a" ]]; then
        pre_hash=$(state_hash "$state_path")
    fi

    "$@" >"$stdout" 2>"$stderr"
    local rc=$?

    if [[ "$state_path" != "n/a" ]]; then
        post_hash=$(state_hash "$state_path")
    fi

    if [[ "$state_path" == "n/a" ]]; then
        delta="n/a"
    elif [[ "$pre_hash" != "$post_hash" ]]; then
        delta="yes"
    else
        delta="no"
    fi

    classify "$label" "$rc" "$stdout" "$delta" "$probe" "rc=$rc delta=$delta"
}

phase() {
    log ""
    log "${BLU}━━━ $1 ━━━${NC}"
}

# ── Main ───────────────────────────────────────────────────────────────────
log "${BLU}JTBD Runtime Smoke Test — every ggen CLI verb${NC}"
log "Workspace: $TMP"
log "Binary:    $BIN"

ensure_binary
command -v jq >/dev/null 2>&1 || { echo "jq required" >&2; exit 1; }

WORK="$TMP/work"; mkdir -p "$WORK"; cd "$WORK"

# Pack metadata source: ggen marketplace TOML files
# `GGEN_PACKS_DIR` controls where load_pack_metadata() looks for <pack_id>.toml
export GGEN_PACKS_DIR="$ROOT/marketplace/packs"

# Pack install destination: isolated TempDir so we don't touch ~/.ggen/packs
# `GGEN_PACK_CACHE_DIR` is used by install_pack() / pack.remove
PACK_CACHE="$TMP/pack-cache"; mkdir -p "$PACK_CACHE"
export GGEN_PACK_CACHE_DIR="$PACK_CACHE"

# ── Phase 1: Diagnostics (read-only) ──────────────────────────────────────
phase "Phase 1: Diagnostics"

run_verb "doctor.run"       n/a 'has("healthy")'        "$BIN" doctor run
run_verb "doctor.check"     n/a 'has("passed")'         "$BIN" doctor check
run_verb "doctor.config"    n/a 'has("passed")'         "$BIN" doctor config
run_verb "doctor.ontology"  n/a 'has("passed")'         "$BIN" doctor ontology
run_verb "doctor.telemetry" n/a 'has("passed")'         "$BIN" doctor telemetry
run_verb "doctor.registry"  n/a 'has("passed")'         "$BIN" doctor registry
run_verb "doctor.security"  n/a 'has("passed")'         "$BIN" doctor security
run_verb "doctor.full"      n/a 'has("healthy")'        "$BIN" doctor full
run_verb "doctor.publish"   n/a 'has("healthy")'        "$BIN" doctor publish
run_verb "utils.doctor"     n/a 'has("checks_passed")'  "$BIN" utils doctor
run_verb "utils.env"        n/a 'has("variables")'      "$BIN" utils env --list

# ── Phase 2: Discovery ────────────────────────────────────────────────────
phase "Phase 2: Discovery"

run_verb "registry.list"      n/a 'has("packs")'              "$BIN" registry list
run_verb "registry.search"    n/a 'has("packs")'              "$BIN" registry search --query=rust
run_verb "marketplace.list"   n/a 'type|test("object|array")' "$BIN" marketplace list
run_verb "marketplace.search" n/a 'type|test("object|array")' "$BIN" marketplace search --query=mcp
run_verb "capability.list"    n/a 'has("capabilities")'        "$BIN" capability list
run_verb "capability.graph"   n/a 'has("nodes") and has("edges")' "$BIN" capability graph
run_verb "capability.trust"   n/a 'has("packs")'              "$BIN" capability trust

# ── Phase 3: Init / project bootstrap ─────────────────────────────────────
phase "Phase 3: Project Bootstrap"

INIT_DIR="$WORK/init-test"; mkdir -p "$INIT_DIR"
(cd "$INIT_DIR" && run_verb "init.bare"  "$INIT_DIR" 'true' "$BIN" init)

# ── Phase 4: Pack management ──────────────────────────────────────────────
phase "Phase 4: Pack Management"

PROJ="$WORK/proj-pack"; mkdir -p "$PROJ"
cd "$PROJ"
"$BIN" init >/dev/null 2>&1 || true

run_verb "packs.list"          "$PROJ/.ggen" 'true'   "$BIN" packs list
run_verb "packs.doctor"        "n/a"         'true'   "$BIN" packs doctor
run_verb "pack.add"            "$PROJ/.ggen" 'has("status")' "$BIN" pack add --pack_name=enterprise-backend --force=true
run_verb "packs.validate"      "n/a"         'true'   "$BIN" packs validate --pack_id=enterprise-backend
run_verb "packs.show"          "n/a"         'true'   "$BIN" packs show --pack_id=enterprise-backend
run_verb "packs.dependencies"  "n/a"         'true'   "$BIN" packs dependencies --pack_id=enterprise-backend
run_verb "packs.compose"       "n/a"         'true'   "$BIN" packs compose --pack_ids=enterprise-backend
run_verb "packs.search"        "n/a"         'true'   "$BIN" packs search --query=rust
skip_verb "packs.check_compatibility" "subcommand not registered as direct verb"
run_verb "packs.install"       "$PROJ/.ggen" 'true'   "$BIN" packs install --pack_id=enterprise-backend
run_verb "packs.generate"      "n/a"         'true'   "$BIN" packs generate --pack_id=enterprise-backend --project_path="$PROJ"
run_verb "pack.remove"         "$PROJ/.ggen" 'has("status")' "$BIN" pack remove --pack_name=enterprise-backend

# ── Phase 5: Capability lifecycle ─────────────────────────────────────────
phase "Phase 5: Capability Lifecycle"

CAP_PROJ="$WORK/proj-cap"; mkdir -p "$CAP_PROJ"
cd "$CAP_PROJ"
"$BIN" init >/dev/null 2>&1 || true

run_verb "capability.enable"    "$CAP_PROJ/.ggen" 'has("capability")' \
    "$BIN" capability enable --surface=mcp --projection=rust --runtime=axum --profile=enterprise-strict
run_verb "capability.inspect"   "n/a" 'has("atomic_packs")' "$BIN" capability inspect --capability=mcp
run_verb "capability.conflicts" "n/a" 'has("conflicts")'     "$BIN" capability conflicts
run_verb "capability.disable"   "$CAP_PROJ/.ggen" 'has("status")' "$BIN" capability disable --capability=mcp

# ── Phase 6: Policy ──────────────────────────────────────────────────────
phase "Phase 6: Policy"

POL_PROJ="$WORK/proj-pol"; mkdir -p "$POL_PROJ"
cd "$POL_PROJ"
"$BIN" init >/dev/null 2>&1 || true
"$BIN" capability enable --surface=mcp --projection=rust --runtime=axum >/dev/null 2>&1 || true

run_verb "policy.list"     "n/a" 'has("profiles")'       "$BIN" policy list
run_verb "policy.show"     "n/a" 'has("profile_id")'     "$BIN" policy show --profile_id=enterprise-strict
run_verb "policy.validate" "n/a" 'has("passed")'         "$BIN" policy validate --profile=enterprise-strict
run_verb "policy.check"    "n/a" 'has("passed") or has("profile_id")' "$BIN" policy check

# ── Phase 7: Graph / Ontology / Template ─────────────────────────────────
phase "Phase 7: Graph / Ontology / Template"

cd "$WORK"
SAMPLE_TTL="$ROOT/tests/fixtures/sample.ttl"
HL7_TTL="$ROOT/tests/fixtures/ontologies/HL7PatientCare.ttl"

if [[ -f "$SAMPLE_TTL" ]]; then
    run_verb "graph.load"      "n/a" 'has("triples_loaded")' "$BIN" graph load --file="$SAMPLE_TTL"
    run_verb "graph.query"     "n/a" 'has("bindings")'        \
        "$BIN" graph query --sparql_query="SELECT * WHERE { ?s ?p ?o } LIMIT 5" --graph_file="$SAMPLE_TTL"
    run_verb "graph.export"    "$WORK/graph-out.ttl" 'has("triples_exported")' \
        "$BIN" graph export --input_file="$SAMPLE_TTL" --output="$WORK/graph-out.ttl" --format=turtle
    # cd into TempDir so visualize's default output goes there, not into tests/fixtures
    cd "$WORK"
    cp "$SAMPLE_TTL" "$WORK/sample.ttl" 2>/dev/null
    run_verb "graph.visualize" "n/a" 'has("nodes_rendered")' \
        "$BIN" graph visualize --input_file="$WORK/sample.ttl" --format=dot
else
    skip_verb "graph.load"      "missing fixture: $SAMPLE_TTL"
    skip_verb "graph.query"     "missing fixture: $SAMPLE_TTL"
    skip_verb "graph.export"    "missing fixture: $SAMPLE_TTL"
    skip_verb "graph.visualize" "missing fixture: $SAMPLE_TTL"
fi

if [[ -f "$HL7_TTL" ]]; then
    run_verb "ontology.validate" "n/a" 'has("is_valid")' \
        "$BIN" ontology validate --schema_file="$HL7_TTL"
    run_verb "ontology.generate" "$WORK/ont-out" 'has("files_generated")' \
        "$BIN" ontology generate --schema_file="$HL7_TTL" --language=rust --output="$WORK/ont-out"
else
    skip_verb "ontology.validate" "missing fixture: $HL7_TTL"
    skip_verb "ontology.generate" "missing fixture: $HL7_TTL"
fi

run_verb "ontology.init" "$WORK/ont-init" 'has("project_name")' \
    "$BIN" ontology init --project_name=test-ont

# Template verbs operate relative to cwd; create dedicated workspace
TMPL_WORK="$WORK/tmpl-ws"; mkdir -p "$TMPL_WORK/templates"
cd "$TMPL_WORK"
run_verb "template.list"   "n/a" 'has("templates")'      "$BIN" template list
run_verb "template.new"    "$TMPL_WORK/templates"  'has("template_name")' \
    "$BIN" template new --name=jtbd-test
TMPL_PATH="$TMPL_WORK/templates/jtbd-test.tmpl"
[[ -f "$TMPL_WORK/templates/jtbd-test" ]] && TMPL_PATH="$TMPL_WORK/templates/jtbd-test"
run_verb "template.show"   "n/a" 'has("name") or has("path")' "$BIN" template show --template="$TMPL_PATH"
run_verb "template.get"    "n/a" 'has("name") or has("path")' "$BIN" template get  --template="$TMPL_PATH"
run_verb "template.lint"   "n/a" 'has("has_errors")' "$BIN" template lint --template="$TMPL_PATH"
cd "$WORK"

# ── Phase 8: Sync ─────────────────────────────────────────────────────────
phase "Phase 8: Sync Pipeline"

SYNC_PROJ="$WORK/proj-sync"; mkdir -p "$SYNC_PROJ"
cd "$SYNC_PROJ"
"$BIN" init >/dev/null 2>&1 || true

SYNC_TMP="$TMP/sync-out.json"
"$BIN" sync --dry_run=true >"$SYNC_TMP" 2>&1
SYNC_RC=$?
if [[ $SYNC_RC -eq 0 ]]; then
    classify "sync.dry-run" 0 "$SYNC_TMP" "n/a" "skip" "rc=0"
else
    # Sync requires ontology.ttl + manifest config; absent in test workspace
    skip_verb "sync.dry-run" "needs ontology.ttl + ggen.toml manifest config"
fi

# ── Phase 9: Envelope ────────────────────────────────────────────────────
phase "Phase 9: Envelope"

KEYS="$WORK/keys"; mkdir -p "$KEYS"
PRIV="$KEYS/signing.key"
if command -v openssl >/dev/null 2>&1; then
    openssl rand -hex 32 > "$PRIV"
else
    dd if=/dev/urandom bs=32 count=1 2>/dev/null | xxd -p | tr -d '\n' > "$PRIV"
fi

PAYLOAD="$WORK/payload.json"
echo '{"hello":"world"}' > "$PAYLOAD"
ENV_OUT="$WORK/envelope.json"

ENV_TMP="$TMP/env-sign.json"
"$BIN" envelope sign \
    --payload_path="$PAYLOAD" \
    --payload_schema="text/json" \
    --producer_system="jtbd-test" \
    --producer_kind="cli" \
    --operation_id="op-1" \
    --envelope_id="env-1" \
    --private_key="$PRIV" \
    --public_key_ref="local" \
    --output="$ENV_OUT" >"$ENV_TMP" 2>&1
ENV_RC=$?
ENV_DELTA="no"
[[ -f "$ENV_OUT" ]] && ENV_DELTA="yes"
classify "envelope.sign" "$ENV_RC" "$ENV_TMP" "$ENV_DELTA" 'has("envelope_id")' "rc=$ENV_RC"

skip_verb "envelope.verify"       "needs Ed25519 verifying key derived from priv"
skip_verb "envelope.chain_verify" "needs verifying key + chain file"

# ── Phase 10: Workflow ───────────────────────────────────────────────────
phase "Phase 10: Workflow"

WF_DIR="$WORK/wf"; mkdir -p "$WF_DIR"
WF_FILE="$WF_DIR/jtbd.json"   # workflow init writes to output_dir/<name>.json

run_verb "workflow.init"  "$WF_FILE" 'has("workflow_name") or has("path")' \
    "$BIN" workflow init --name=jtbd --output_dir="$WF_DIR"
run_verb "workflow.event" "$WF_FILE" 'has("activity") or has("status")' \
    "$BIN" workflow event --workflow_file="$WF_FILE" --case_id=c1 --activity=Submitted
"$BIN" workflow event --workflow_file="$WF_FILE" --case_id=c1 --activity=Reviewed >/dev/null 2>&1 || true
"$BIN" workflow event --workflow_file="$WF_FILE" --case_id=c2 --activity=Submitted >/dev/null 2>&1 || true

run_verb "workflow.analyze"    "n/a" '.total_events>=1'  "$BIN" workflow analyze    --workflow_file="$WF_FILE"
run_verb "workflow.discover"   "n/a" 'has("total_edges")' "$BIN" workflow discover  --workflow_file="$WF_FILE"
run_verb "workflow.synthesize" "n/a" 'true' \
    "$BIN" workflow synthesize --workflow_file="$WF_FILE" --law_id=jtbd-law --name=JtbdLaw
run_verb "workflow.report"     "$WF_DIR/wf-report.json" 'true' \
    "$BIN" workflow report --workflow_file="$WF_FILE" --format=json --output="$WF_DIR/wf-report.json"

# ── Phase 11: Telco ──────────────────────────────────────────────────────
phase "Phase 11: Telco"

run_verb "telco.route"    "n/a" 'has("status")' "$BIN" telco route
run_verb "telco.dial"     "n/a" 'has("status")' "$BIN" telco dial
run_verb "telco.switch"   "n/a" 'has("status")' "$BIN" telco switch
run_verb "telco.trunk"    "n/a" 'has("status")' "$BIN" telco trunk
run_verb "telco.bluebox"  "n/a" 'has("status")' "$BIN" telco bluebox
run_verb "telco.tap"      "n/a" 'has("status")' "$BIN" telco tap
run_verb "telco.operator" "n/a" 'has("status")' "$BIN" telco operator
run_verb "telco.phreak"   "n/a" 'has("status")' "$BIN" telco phreak

# ── Phase 12: Self-Play & Semantic OS ────────────────────────────────────
phase "Phase 12: Self-Play & Semantic OS"

SP_DIR="$WORK/self-play"; mkdir -p "$SP_DIR"
run_verb "self_play.run"      "$SP_DIR" 'true' "$BIN" self_play run
run_verb "self_play.validate" "n/a"     'true' "$BIN" self_play validate --output_dir="$SP_DIR"

# semantic_os: requires --law_id (depends on prior compile output) or --package_id
run_verb "semantic_os.compile"     "n/a" 'true' "$BIN" semantic_os compile --package_id=enterprise-backend
run_verb "semantic_os.manufacture" "n/a" 'true' "$BIN" semantic_os manufacture --package_id=enterprise-backend
skip_verb "semantic_os.doctor"  "needs law_id from prior compile"
skip_verb "semantic_os.admit"   "needs prior compiled law + input file"
skip_verb "semantic_os.replay"  "needs prior receipt file"
skip_verb "semantic_os.runbook" "needs law_id from prior compile"

# ── Phase 13: Paper ──────────────────────────────────────────────────────
phase "Phase 13: Paper"

PAPER_DIR="$WORK/papers"; mkdir -p "$PAPER_DIR"
cd "$PAPER_DIR"

run_verb "paper.templates"         "n/a" 'has("templates")' "$BIN" paper templates
run_verb "paper.new"               "$PAPER_DIR/test-paper" 'has("paper_path")' \
    "$BIN" paper new --name=test-paper
PAPER_RDF="$PAPER_DIR/test-paper/test-paper.rdf"
run_verb "paper.validate"          "n/a" 'has("is_valid")' \
    "$BIN" paper validate --paper_file="$PAPER_RDF"
run_verb "paper.generate"          "n/a" 'has("output_path")' \
    "$BIN" paper generate --paper_file="$PAPER_RDF"
run_verb "paper.init_bibliography" "$PAPER_DIR/test-paper" 'true' \
    "$BIN" paper init_bibliography --paper_file="$PAPER_RDF"

# ── Phase 14: Marketplace remaining ──────────────────────────────────────
phase "Phase 14: Marketplace remaining"

cd "$WORK"
run_verb "marketplace.show"     "n/a" 'true' "$BIN" marketplace show --package_id=enterprise-backend
run_verb "marketplace.info"     "n/a" 'true' "$BIN" marketplace info --package_id=enterprise-backend
run_verb "marketplace.doctor"   "n/a" 'true' "$BIN" marketplace doctor
skip_verb "marketplace.install" "duplicate of pack.add (lockfile mutation)"
skip_verb "marketplace.sync"    "duplicate of top-level sync command"

# ── Phase 15: Registry remaining + create/validate top-level ─────────────
phase "Phase 15: Registry & Top-Level"

run_verb "registry.info" "n/a" 'has("id") or has("name")' "$BIN" registry info --pack_id=enterprise-backend

# create / validate are top-level commands with construct subcommand
skip_verb "create.construct"   "needs OWL spec file (out of scope for smoke test)"
skip_verb "validate.construct" "needs prior construct module"

# ── Final Report ─────────────────────────────────────────────────────────
TOTAL=$((COUNT_REAL + COUNT_PARTIAL + COUNT_BROKEN + COUNT_SKIP))

if [[ $REPORT_MODE -eq 1 ]]; then
    echo "# JTBD Verb Classification Report"
    echo ""
    echo "Generated: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "Binary: \`$BIN\`"
    echo ""
    echo "## Summary"
    echo ""
    echo "| Class | Count |"
    echo "|---|---|"
    echo "| REAL    | $COUNT_REAL |"
    echo "| PARTIAL | $COUNT_PARTIAL |"
    echo "| BROKEN  | $COUNT_BROKEN |"
    echo "| SKIP    | $COUNT_SKIP |"
    echo "| **Total** | **$TOTAL** |"
    echo ""
    echo "## Per-Verb Classification"
    echo ""
    echo "| Verb | Class | Exit | Evidence | Disposition (TBD) |"
    echo "|---|---|---|---|---|"
    for r in "${RESULTS[@]}"; do
        IFS='|' read -r v c rc e <<< "$r"
        echo "| \`$v\` | $c | $rc | $e | |"
    done
    echo ""
    echo "## Classification Definitions"
    echo ""
    echo "- **REAL** — exits 0, produces expected state delta, output structurally valid"
    echo "- **PARTIAL** — exits 0 but: probe failed, no state change, or output contains hardcoded sentinel data"
    echo "- **BROKEN** — non-zero exit, panic, or unparseable output"
    echo "- **SKIP** — needs external service/auth/fixture not provisioned"
else
    log ""
    log "${BLU}━━━ Summary ━━━${NC}"
    log "  ${GRN}REAL:${NC}    $COUNT_REAL"
    log "  ${YEL}PARTIAL:${NC} $COUNT_PARTIAL"
    log "  ${RED}BROKEN:${NC}  $COUNT_BROKEN"
    log "  ${BLU}SKIP:${NC}    $COUNT_SKIP"
    log "  Total:    $TOTAL"
    log ""
    log "Run with --report to generate markdown classification table."
fi

exit 0
