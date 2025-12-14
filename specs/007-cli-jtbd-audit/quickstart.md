# Quickstart: CLI JTBD Audit Execution

**Feature Branch**: `007-cli-jtbd-audit`
**Timeline**: 1 week (~10 commands/day)

## Prerequisites

```bash
# Verify ggen is installed
ggen --version  # Should show 4.0.0

# Verify cargo-make
cargo make --version

# Navigate to feature directory
cd specs/007-cli-jtbd-audit
```

## Directory Setup

```bash
# Create evidence subdirectories
mkdir -p evidence/{workflow,template,project,ontology,graph,marketplace,fmea,ai,case-studies}
mkdir -p reports
```

## Audit Execution Process

### Step 1: Audit a Single Command

```bash
# Example: Audit "ggen template generate"
COMMAND="template generate"
CATEGORY="template"
CMD_SLUG="template-generate"

# 1. Test execution
ggen $COMMAND --help > evidence/$CATEGORY/$CMD_SLUG-help.log 2>&1
echo "Exit code: $?" >> evidence/$CATEGORY/$CMD_SLUG-help.log

# 2. Test happy path (with sample inputs)
ggen $COMMAND --template examples/simple.tmpl --output /tmp/test 2>&1 | \
  tee evidence/$CATEGORY/$CMD_SLUG-happy-path.log
echo "Exit code: $?" >> evidence/$CATEGORY/$CMD_SLUG-happy-path.log

# 3. Test error handling (invalid inputs)
ggen $COMMAND --template nonexistent.tmpl 2>&1 | \
  tee evidence/$CATEGORY/$CMD_SLUG-error.log
echo "Exit code: $?" >> evidence/$CATEGORY/$CMD_SLUG-error.log

# 4. Create audit result YAML
cat > evidence/$CATEGORY/$CMD_SLUG.yaml << 'EOF'
command: ggen template generate
version_tested: 4.0.0
date: 2024-12-14
tester: claude-code

functional_correctness:
  executes: true
  help_works: true
  happy_path: true
  error_handling: true
  exit_codes: true

agent_score: 0  # Calculate after scoring
agent_breakdown:
  parseable_output: 0    # /25
  error_messages: 0      # /20
  idempotency: 0         # /15
  progress_feedback: 0   # /10
  dry_run: 0             # /10
  documentation: 0       # /10
  exit_codes: 0          # /10

avatar_notes:
  claude_code: ""
  cursor_ai: ""
  copilot: ""
  aider: ""
  devin: ""
  openhands: ""
  windsurf: ""

maturity_level: L0  # Update after scoring
maturity_blockers: []

evidence_files:
  - template-generate-help.log
  - template-generate-happy-path.log
  - template-generate-error.log

recommendations: []
EOF
```

### Step 2: Score Agent Accessibility

For each criterion, assign a score:

| Criterion | 0 (L1) | Partial (L3) | Full (L5) |
|-----------|--------|--------------|-----------|
| **Parseable Output** (25) | Text only: 5 | JSON option: 15 | JSON schema: 25 |
| **Error Messages** (20) | Stack trace: 4 | Human-readable: 12 | Machine-parseable: 20 |
| **Idempotency** (15) | Destructive: 3 | Mostly safe: 9 | Fully idempotent: 15 |
| **Progress Feedback** (10) | Silent: 2 | Final status: 6 | Streaming: 10 |
| **Dry-Run Support** (10) | None: 0 | Partial: 5 | Full preview: 10 |
| **Documentation** (10) | --help only: 2 | Examples: 6 | JTBD + examples: 10 |
| **Exit Codes** (10) | 0/1 only: 2 | Differentiated: 6 | Semantic: 10 |

### Step 3: Determine Maturity Level

```
Score 0-19   → L1 (Initial)
Score 20-39  → L2 (Managed)
Score 40-59  → L3 (Defined)
Score 60-79  → L4 (Quantified)  ← Agent-usable threshold
Score 80-100 → L5 (Optimized)
```

**Blockers that cap maturity:**
- Command crashes → L0
- Deprecated → L0-DEP
- Non-deterministic output → L3 max

### Step 4: Add Avatar Notes

For each avatar, note specific observations:

```yaml
avatar_notes:
  claude_code: "JSON output via --output-format json; deterministic"
  cursor_ai: "Help fits in 8K context; single-command usage"
  copilot: "Patterns learnable from examples"
  aider: "Git-trackable output changes"
  devin: "Self-contained operation"
  openhands: "Customizable via config"
  windsurf: "IDE preview possible"
```

### Step 5: Validate Case Studies

```bash
# For each case study, execute required command sequence
# Example: JPMorgan compliance workflow

ggen ontology validate examples/compliance.ttl
ggen template generate --template compliance.tmpl --output /tmp/compliance
ggen graph query --query "SELECT ?entity WHERE { ?entity a :ComplianceRule }"

# Record in evidence/case-studies/jpmorgan.yaml
```

## Daily Execution Schedule

| Day | Category | Commands | Target |
|-----|----------|----------|--------|
| 1 | workflow | analyze, init, report, event, discover | 5 |
| 1 | utils | various | 1-2 |
| 2 | template | new, list, lint, generate, get, show, generate-tree, regenerate | 8 |
| 3 | project | new, plan, apply, generate, init, watch, gen | 7 |
| 3 | graph | query, load, visualize, export | 4 |
| 4 | ontology | generate, extract, validate, init | 4 |
| 4 | ai | generate, chat, analyze | 3 |
| 5 | marketplace | sparql, install, metrics, validate, info, rdf_stats, versions, publish, validate_fmea, search | 10 |
| 5 | fmea | show, pareto, report, export, list | 5 |
| 6-7 | Reports | Generate maturity matrix, avatar compatibility, gap analysis | - |

## Report Generation

After all audits complete:

```bash
# 1. Generate Maturity Matrix Report
cat evidence/*/*.yaml | yq -s '.' > reports/all-audits.json
# Process into reports/maturity-matrix.md

# 2. Generate Avatar Compatibility Matrix
# Cross-reference all avatar_notes into reports/avatar-compatibility.md

# 3. Generate Fortune 500 Gap Analysis
cat evidence/case-studies/*.yaml | yq -s '.' > reports/case-studies.json
# Process into reports/fortune500-gaps.md
```

## Validation Checklist

Before marking audit complete:

- [ ] All 47+ commands have evidence YAML files
- [ ] All functional_correctness fields filled
- [ ] All agent_breakdown scores assigned
- [ ] agent_score = sum of breakdown (verified)
- [ ] maturity_level assigned with blockers documented
- [ ] All 7 avatars have notes
- [ ] Evidence log files exist
- [ ] All 7 case studies validated
- [ ] Reports generated

## Troubleshooting

**Command not found:**
```bash
# Verify ggen binary is in PATH
which ggen
# Rebuild if needed
cargo make build
```

**Permission denied:**
```bash
# Ensure evidence directories exist
mkdir -p evidence/{workflow,template,project,ontology,graph,marketplace,fmea,ai,case-studies}
```

**Invalid YAML:**
```bash
# Validate YAML syntax
yq eval '.' evidence/template/template-generate.yaml
```

## Next Steps

After completing audits:
1. Review maturity distribution
2. Identify P1 improvements for L4+ compliance
3. Create enhancement issues for gaps
4. Plan remediation roadmap
