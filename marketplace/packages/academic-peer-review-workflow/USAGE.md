## Usage Guide — Academic Peer Review Workflow

### 1) Install the package
```bash
ggen marketplace install --package_id academic-peer-review-workflow --install_path ./peer-review-workflow
```

### 2) Provide required inputs
- Place your submission graph at `./peer-review-workflow/paper.rdf` (sample provided).
- Provide reviewer roster at `./peer-review-workflow/reviewers.json` (sample provided: ids, names, roles, COI flags).

### 3) Configure variables
Edit `workflows/peer_review.workflow.json`:
- `paper_id`: stable identifier for the manuscript
- `deadline`: review deadline in `YYYY-MM-DD`
- `managing_editor`: accountable editor
- `num_reviewers`: count of assigned reviewers (default 3)

### 4) Validate inputs
```bash
./peer-review-workflow/hooks/validate.sh ./peer-review-workflow
```

### 5) Run analysis / tracking
```bash
ggen workflow analyze --workflow ./peer-review-workflow/workflows/peer_review.workflow.json
```
Outputs deterministic JSON summary of cases, events, durations, and bottlenecks.

### 6) Generate reviewer/author artifacts
- Reviewer RDF template: `templates/reviewer-template.rdf`
- Author response template: `templates/response-template.md`

Example rendering:
```bash
ggen template render \
  --template ./peer-review-workflow/templates/response-template.md \
  --data ./peer-review-workflow/templates/reviewer-template.rdf \
  --out ./peer-review-workflow/output/author-response.md
```

### 7) Lifecycle stages (expected order)
initial-submission → editorial-screening → reviewer-assignment → review-period → decision-compilation → decision-communication → author-revision → re-review → final-decision

### 8) Validation contract
`paper.rdf` and `reviewers.json` must exist. Stages `reviewer-assignment`, `review-period`, and `decision-compilation` must be present in event logs for the workflow to be considered complete.




