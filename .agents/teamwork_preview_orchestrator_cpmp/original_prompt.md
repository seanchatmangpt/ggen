# Original User Request

## Initial Request — 2026-05-27T12:33:31-07:00

Implement the capability-map (cpmp) requirements as described in /Users/sac/ggen/ORIGINAL_REQUEST.md under follow-up header '2026-05-27T19:32:55Z'.
Maintain your planning, architecture, and progress under:
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_cpmp/plan.md
- /Users/sac/ggen/.agents/teamwork_preview_orchestrator_cpmp/progress.md

Dispatch tasks to specialist subagents (explorer, implementer, etc.) as needed under their own separate coordination folders under /Users/sac/ggen/.agents/.
Once you have fully verified the implementation according to the acceptance criteria, write a handoff and notify the Sentinel of completion.

## Follow-up — 2026-05-27T19:33:53Z

The user has supplied an update and correction to the architecture. We are extending CPMP and ggen to include the Enterprise Wrapper Architecture.

Please incorporate these requirements into your implementation and documentation goals:

1. Architecture:
   - cpmp: discovers projects, files, capabilities, tests, docs, runtimes.
   - ggen enterprise membrane: normalizes, projects, validates, receipts, redacts, packages.
   - Open Ontologies: RDF/OWL store, SPARQL, SHACL, lint, reason, diff, version, lineage.
   - Enterprise Control Plane (Wrapper): auth, tenancy, policy, approvals, audit, retention, backups, exports.

2. Create/Write the required enterprise documentation in `docs/enterprise/`:
   - docs/enterprise/ARCHITECTURE.md
   - docs/enterprise/CONTROL_PLANE.md
   - docs/enterprise/TENANCY.md
   - docs/enterprise/AUTHZ.md
   - docs/enterprise/POLICY_PACKS.md
   - docs/enterprise/AUDIT_AND_LINEAGE.md
   - docs/enterprise/RETENTION_AND_BACKUP.md
   - docs/enterprise/OPEN_ONTOLOGIES_ADAPTER.md
   - docs/enterprise/GGEN_PROJECTION_MEMBRANE.md
   - docs/enterprise/PUBLIC_VOCABULARY_FIREWALL.md
   - docs/enterprise/GAP_CLOSURE_MATRIX.md (Be sure to include all specified gaps: invalid Turtle, illegal URI syntax, namespace laundering, private predicate authority, missing SHACL reports, missing PROV lineage, missing checksum evidence, missing canonical graph hash, JSON-only evidence, stale report emission, unversioned graph mutation, unscoped tenant access, missing no-deletion check, missing backup/export path, missing policy refusal artifact).
   - docs/enterprise/ENTERPRISE_DEFINITION_OF_DONE.md

3. Implement or stub the CLI nouns and commands:
   - cpmp computer discover
   - cpmp graph project
   - cpmp graph validate
   - cpmp graph load
   - cpmp graph query
   - cpmp graph version
   - cpmp graph drift
   - cpmp policy check
   - cpmp policy enforce
   - cpmp tenant create
   - cpmp tenant list
   - cpmp audit lineage
   - cpmp receipt emit
   - cpmp receipt verify-no-deletion
   - cpmp enterprise doctor

4. Create stubs for the enterprise modules:
   - cpmp-enterprise-auth
   - cpmp-enterprise-tenancy
   - cpmp-enterprise-policy
   - cpmp-enterprise-audit
   - cpmp-enterprise-retention
   - cpmp-enterprise-backup
   - cpmp-enterprise-redaction
   - cpmp-enterprise-approval
   - cpmp-enterprise-observability
   - cpmp-open-ontologies-adapter
   - cpmp-ggen-projection
   - cpmp-public-vocabulary-firewall

5. Ensure all gates are enforced in the scanning pipeline. A scan must be refused if:
   - RDF does not parse cleanly or contains illegal URI syntax.
   - Private predicates carry public authority where standard predicates exist.
   - Local IDs are laundered into prov:, sh:, dcat:, or other standard namespaces.
   - Validation fails or missing sh:ValidationReport.
   - Output does not link to the scan activity via prov:wasGeneratedBy.
   - Files lack checksums.
   - Reports lack source graph hashes.
   - Open Ontologies load, validation, or versioning fails.

Please update the project plan, build and implement these structures, verify the tests pass, and report back.

