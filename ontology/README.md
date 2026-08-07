# Federated Semantic Registry

This directory preserves the existing ggen Turtle and SHACL ontologies and adds a federated registry for external semantic authorities.

The registry does **not** treat every source as an OWL ontology. Each source retains the inventory classification:

- `O` — ontology or semantic vocabulary
- `S` — machine-readable schema or information model
- `K` — knowledge base, taxonomy, or control catalog
- `P` — protocol or executable contract

## Authority boundary

`registry.toml` and the 16 `pack.toml` files are the admitted inventory. Upstream files are not copied into the repository and allowed to drift. A source remains `UNKNOWN` until pinned retrieval, license review, digesting, parsing, projection, and validation have executed.

The implemented path is:

```text
public authoritative source
→ pinned retrieval
→ license check
→ source digest
→ parser
→ normalized ggen projection
→ SHACL validation
→ cross-provider mappings
→ generated customer ontology
```

The inventory contains **201 external source records** plus **2 local ontology profiles**:

- `00-foundation/public-semantic-source.ttl` defines the registry metamodel.
- `04-identity-authority/autofde-authority.ttl` defines authority grants, decision rights, permitted consequences, scopes, approval populations, validity intervals, revocation, delegation, and separation of duties.

## Packs

| Pack | Scope |
| --- | --- |
| `00-foundation` | Semantic-web foundation |
| `01-cloud-reference` | Cloud reference architecture |
| `02-cloud-resources` | Cloud resources and integration |
| `03-organization` | Organization, business, and legal |
| `04-identity-authority` | Identity, authority, and entitlement |
| `05-security-threat` | Security threat and defensive action |
| `06-controls-compliance` | Controls and compliance |
| `07-events-observability` | Events and observability |
| `08-process-decision` | Process and decision |
| `09-data-governance` | Data governance, lineage, and privacy |
| `10-cost-finops` | Cost and FinOps |
| `11-software-supply-chain` | Software supply chain |
| `12-ai-agent` | AI and agent governance |
| `13-physical-digital-twin` | Physical infrastructure and digital twins |
| `14-sustainability` | Sustainability |
| `15-industry` | Industry overlays |

## Verification

```bash
python3 scripts/validate_ontology_registry.py
python3 scripts/validate_ontology_registry.py --json
```

The validator checks pack closure, unique source identity, exact P0/provider membership, cross-pack references, required evidence fields, standing law, local RDF syntax, and local source digests.

The source inventory is represented directly in the pack records; upstream artifacts are retrieved only after admission.
