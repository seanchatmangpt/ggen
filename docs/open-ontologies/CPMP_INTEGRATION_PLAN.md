# CPMP Integration Plan: OpenOntologies Governance Mesh

This plan outlines the integration of `open-ontologies` as the authoritative governance layer for `cpmp` (Code Manufacturing Pipeline) catalog projections.

## 1. Overview
`cpmp` will utilize `open-ontologies` to validate the semantic integrity of its catalog projections. Every projection will undergo automated SHACL validation against the `open-ontologies` canonical model before being committed or registered.

## 2. Automated Validation Hook (`onto_validate`)
A new hook will be introduced to the `cpmp` projection pipeline:

1. **Pre-projection:** `cpmp` prepares the RDF/N-Quads output of the catalog projection.
2. **Hook Trigger:** `onto_validate` pipeline executes.
3. **Linting:** 
   - `cpmp` outputs catalog data as N-Quads to a temporary location.
   - `open-ontologies` runs `pyshacl` or equivalent against the exported N-Quads using the canonical SHACL shapes.
4. **Result Handling:**
   - **Success:** Projection continues to registration.
   - **Failure:** Pipeline aborts, emitting a `ValidationError` receipt linked to the problematic projection data.

## 3. N-Quads Emission Specification
To maintain compatibility, `cpmp` will project catalog components using the following pattern:

- **Namespace:** `urn:cpmp:catalog:<component_id>`
- **Format:** Canonical N-Quads (UTF-8).
- **Metadata:** Include provenance timestamps and generator hashes as required by the `ostar` observability standards.

## 4. Implementation Roadmap
- [ ] Define SHACL shapes for CPMP catalog components in `open-ontologies`.
- [ ] Implement `cpmp` N-Quad exporter.
- [ ] Develop `onto_validate` hook script in the ggen workflow.
- [ ] Integrate validation into the `ggen sync` pipeline gate.
