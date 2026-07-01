# Agent-edit event log + discovery

`agent-edit-events.ocel.jsonl` accumulates object-centric events (DiagnosticRaised,
RepairSuggested, GatePassed/Failed, ...) captured by `ggen lsp check`. `ggen lsp mine`
projects them to RDF and discovers failure edges via SPARQL, writing
`discovery/error-edge-mining.md`. This is the 80/20 evidence behind repair routes.
