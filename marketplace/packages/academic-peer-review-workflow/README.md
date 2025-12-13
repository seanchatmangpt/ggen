## Academic Peer Review Workflow

Purpose: end-to-end peer review lifecycle with reviewer coordination, author responses, revisions, and decision tracking. Aligned with the registry manifest and ready for installation via `ggen marketplace install`.

### What you get
- Workflow definition (`workflows/peer_review.workflow.json`) with staged lifecycle and required variables.
- Reviewer RDF template (`templates/reviewer-template.rdf`) for structured review data.
- Author response template (`templates/response-template.md`) for standardized replies.
- Review schema (`data/review-schema.ttl`) for validation.
- Hooks (`hooks/validate.sh`) to enforce required inputs before running the workflow.

### Install
```bash
ggen marketplace install --package_id academic-peer-review-workflow --install_path ./peer-review-workflow
```

### Configure required variables
Edit `workflows/peer_review.workflow.json` to set (sample values already present):
- `paper_id` (string, required)
- `deadline` (YYYY-MM-DD, required)
- `managing_editor` (string, required)
- `num_reviewers` (integer, default 3)

### Run the workflow (example)
```bash
# Track review file generated from your submission pipeline
ggen workflow analyze --workflow ./peer-review-workflow/workflows/peer_review.workflow.json

# Generate author response template from reviewer RDF
ggen template render \
  --template ./peer-review-workflow/templates/response-template.md \
  --data ./peer-review-workflow/templates/reviewer-template.rdf \
  --out ./peer-review-workflow/output/author-response.md
```

### Validate required inputs
```bash
./peer-review-workflow/hooks/validate.sh ./peer-review-workflow
```
Checks for `paper.rdf` and `reviewers.json` per the manifest validation block (sample fixtures included).

### Workflow stages
1) initial-submission  
2) editorial-screening  
3) reviewer-assignment  
4) review-period  
5) decision-compilation  
6) decision-communication  
7) author-revision  
8) re-review  
9) final-decision

### Notes
- Registry lists dependency on `academic-paper-lifecycle`; install that first if not present.
- Keep reviewer data in RDF to preserve provenance and deterministic merges.



