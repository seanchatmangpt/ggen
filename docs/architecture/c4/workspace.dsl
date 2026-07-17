workspace "ggen Living LSP" "Receipted manufacturing: open-ontology source law, Living LSP admissibility, ggen sync actuation boundary, OCEL receipts, and the wasm4pm process-law oracle. Single source of truth — all C4 views derive from this one model." {

    !identifiers hierarchical

    model {
        # ---- People ----
        author = person "Author / Architect" "Edits source-law surfaces and reviews diagnostics."
        agent  = person "Autonomous Workcell / Agent" "Performs bounded repairs under checkpoint law."

        # ---- External systems ----
        editor   = softwareSystem "Editor / IDE" "LSP client surface." "External"
        git      = softwareSystem "Git / GitHub" "Version control, PRs, branch history; provides the stable source graph O*." "External"
        ci       = softwareSystem "CI / Build Gates" "Repository-wide verification and policy gates." "External"
        wasm4pm  = softwareSystem "wasm4pm / wpm" "Process-mining, discovery, conformance, prefix-completability — the external process-law authority. ggen emits; wasm4pm judges." "External"
        ocelCore = softwareSystem "ocel-core" "Lightweight shared OCEL 2.0 types crate (the only linked dependency ggen takes from the wasm4pm side)." "External,Planned"

        # ---- The system ----
        ggen = softwareSystem "ggen CodeManufactory" "Receipted software manufacturing. Open-ontology source law -> Living LSP admissibility -> ggen sync actuation -> OCEL receipts." {

            # ===== Containers =====
            lsp = container "ggen-lsp" "Read-only Living LSP. Project-relation diagnostics, lifecycle observation, route law, residual-preserving clears, headless check." "Rust" {
                server        = component "Language Server Surface" "LSP entry points, did_open/did_change, refresh + republish orchestration." "server.rs"
                state         = component "ServerState / Living Lifecycle Core" "analyze_and_observe seam (Client-free orchestration), observe_diagnostics, keyed subtraction, residual preservation, pending-repair tracking, stale-clear reconciliation." "state.rs"
                check         = component "Headless Check Surface" "Stateless repository validation; invalid fails / repaired passes. Folds detectors into error_count." "check.rs"
                index         = component "ProjectIndex / RuleIndex" "Project-wide relation indexing across rules, queries, templates, output declarations." "project_index.rs, rule_index.rs"
                harnessIndex  = component "HarnessIndex" "Parses Cargo.toml explicit-path [[test]]/[[bench]] vs on-disk reality. Panic-free I/O boundary." "harness_index.rs"
                teraAnalyzer  = component "Tera Analyzer" "Consumer-set extraction from templates (unbound_projection_diagnostics)." "analyzers/tera_analyzer.rs"
                harnessAnalyzer = component "Harness / Proof Analyzer" "Proof-topology / harness_mismatch relation checker. ACTIVE (GGEN-HARNESS-001, shipped 002)." "analyzers/harness_analyzer.rs"
                detectors     = component "Diagnostic Detectors" "detect_tpl_001 + detect_harness_001 over indexed source-law surfaces. ACTIVE." "analyzers/mod.rs"
                detectOut     = component "OUT-001 Detector" "detect_out_001 / unbound_output_path. NOT IMPLEMENTED — unregistered species, BLOCKED-on-ambiguity until a pre-inventory resolves its relation." "PLANNED" "Planned"
                species       = component "Diagnostic Species Registry" "Declares species: GGEN-TPL-001 (active), GGEN-HARNESS-001 (active). detector_active gating; receipt-order barriers." "route/diagnostic_species.rs"
                routes        = component "Route Registry" "Maps diagnostics to source-law repair families/routes: source-law.bind-projection (TPL), proof-topology.repair (HARNESS / AdmissionFailure)." "route/registry.rs"
                lawSurfaces   = component "Law Surface Discovery" "Maps files/URIs to source-law roles (basename routing: ggen.toml->TPL, Cargo/Makefile.toml->HARNESS)." "discover_law_surfaces (check.rs)"
                events        = component "Event Builders" "Builds DiagnosticRaised, RouteSelected, RepairSuggested, RepairApplied, GatePassed, ReceiptEmitted (BLAKE3 ids, episode objects)." "intel/events.rs"
                logWriter     = component "Intel Log Writer" "Append-only OCEL/NDJSON emission; tolerant read." "intel/log.rs"
            }

            sync = container "ggen sync" "The ONLY lawful actuation boundary. Materializes outputs from admitted source law; emits boundary receipts." "Rust CLI"

            core = container "ggen-engine" "Rule loading, manifest parsing, orchestration, sync execution laws (retired: ggen-core; live since the 2026-ggen-core-replacement migration, PR #255)." "Rust"

            graph = container "ggen-graph" "Indexes project relations; holds OCEL/event-related domain structures. (Carries a duplicate PM stack slated to retire onto wasm4pm.)" "Rust"

            ontology = container "Open Ontology / Source-Law Layer" "Public-footed source law: rule declarations, ontology, query/template/output bindings, validation footing." "RDF / SPARQL / SHACL / PROV-O / DCTERMS / SKOS" {
                ggenToml     = component "ggen.toml Rule Surface" "Construction declaration: project, ontology, generation rules; binds query/template/output. 'What should be built.'" "TOML"
                ontologyDocs = component "Ontology Sources" "Public-footed domain and source-law definitions." "TTL / RDF"
                sparql       = component "SPARQL Query Surfaces" "Producer-set declarations (what variables a rule produces)." ".rq"
                templates    = component "Template Surfaces" "Consumer-set declarations (what variables a template consumes)." ".tera"
                outputs      = component "Output Path Declarations" "Declared artifact path law (output_file)." "ggen.toml"
                shacl        = component "Validation Shapes" "Graph constraint and structural validation." "SHACL"
                provenance   = component "Provenance / Public Vocabulary" "Meaning-bearing public footing for provenance, labeling, relation context." "PROV-O / DCTERMS / SKOS"
            }

            intelLog = container "Intel / OCEL Log" "Append-only object-centric event stream — the externalized process evidence and receipt chain." ".ggen/ocel/agent-edit-events.ocel.jsonl"
            receipts = container "Checkpoint Receipts" "Checkpoint verdicts, gate outcomes, scope audit, external-proof references." "docs/receipts/*.md"
        }

        # ===== Relationships: context =====
        author -> editor "Authors ggen.toml, SPARQL, templates, ontology, proof surfaces"
        agent  -> editor "Operates through bounded author-time surfaces"
        author -> git "Commits / reviews / merges"
        agent  -> git "Works through bounded branch / PR flow"
        editor -> ggen.lsp "LSP requests / diagnostics / code actions"
        git -> ggen "Provides stable source graph O*"
        ggen -> ci "Runs verification gates"
        ggen.intelLog -> wasm4pm "Emits OCEL/process evidence for mining + conformance"
        wasm4pm -> ggen.receipts "Returns external process-law judgment used in receipts"

        # ===== Relationships: containers =====
        editor -> ggen.lsp "LSP protocol"
        ggen.lsp -> ggen.ontology "Reads + evaluates source-law relations"
        ggen.lsp -> ggen.graph "Builds relation indexes / diagnostic context"
        ggen.lsp -> ggen.core "Shared project loading + rule discovery"
        ggen.lsp -> ggen.intelLog "Appends process-evidence events"
        ggen.lsp -> ggen.receipts "References proof obligations / prior receipts"
        ggen.sync -> ggen.core "Uses"
        ggen.sync -> ggen.ontology "Consumes admitted source law"
        ggen.sync -> ggen.graph "Reads indexed rules / graph context"
        ggen.sync -> ggen.receipts "Emits / updates boundary receipts"
        ggen.sync -> ggen.intelLog "May append actuation evidence"
        git -> ggen.lsp "Repository source graph O*"
        git -> ggen.sync "Repository source graph O*"
        ci -> ggen.lsp "Runs headless `ggen lsp check`"
        ci -> ggen.sync "Runs verification / integration gates"
        ggen.intelLog -> ocelCore "Serialized via shared OCEL types"

        # ===== Relationships: ggen-lsp components =====
        ggen.lsp.server -> ggen.lsp.state "Delegates live observation to"
        ggen.lsp.server -> ggen.lsp.check "Triggers / parallels"
        ggen.lsp.check -> ggen.lsp.index "Builds relation context from"
        ggen.lsp.check -> ggen.lsp.detectors "Executes (folds into error_count)"
        ggen.lsp.state -> ggen.lsp.index "Builds / refreshes relation context from"
        ggen.lsp.state -> ggen.lsp.detectors "Executes through live orchestration"
        ggen.lsp.state -> ggen.lsp.routes "Matches pending repairs to routes"
        ggen.lsp.state -> ggen.lsp.species "Checks active/dormant status"
        ggen.lsp.state -> ggen.lsp.events "Builds lifecycle events through"
        ggen.lsp.index -> ggen.lsp.teraAnalyzer "Uses"
        ggen.lsp.index -> ggen.lsp.lawSurfaces "Uses"
        ggen.lsp.harnessIndex -> ggen.lsp.harnessAnalyzer "Feeds declared-target vs file reality to"
        ggen.lsp.detectors -> ggen.lsp.teraAnalyzer "Uses (TPL-001)"
        ggen.lsp.detectors -> ggen.lsp.harnessAnalyzer "Uses (HARNESS-001)"
        ggen.lsp.detectors -> ggen.lsp.index "Reads producer/consumer relation state from"
        ggen.lsp.detectors -> ggen.lsp.species "Uses species definitions from"
        ggen.lsp.detectors -> ggen.lsp.routes "Resolves route-at-raise through"
        ggen.lsp.detectOut -> ggen.lsp.index "(planned) would read output-path relation from"
        ggen.lsp.events -> ggen.lsp.logWriter "Writes events to"
        ggen.lsp.logWriter -> ggen.intelLog "Appends NDJSON events to"
        ggen.lsp.state -> ggen.lsp.harnessIndex "Builds harness relation context from"

        # ===== Relationships: ontology components =====
        ggen.ontology.ggenToml -> ggen.ontology.sparql "Binds (query)"
        ggen.ontology.ggenToml -> ggen.ontology.templates "Binds (template)"
        ggen.ontology.ggenToml -> ggen.ontology.outputs "Declares (output_file)"
        ggen.ontology.ggenToml -> ggen.ontology.ontologyDocs "References (source)"
        ggen.ontology.ontologyDocs -> ggen.ontology.shacl "Constrained through"
        ggen.ontology.ontologyDocs -> ggen.ontology.provenance "Framed with"
        ggen.ontology.sparql -> ggen.ontology.provenance "Interpreted in public footing"
        ggen.ontology.templates -> ggen.ontology.provenance "Interpreted in public footing"
        ggen.ontology.outputs -> ggen.ontology.provenance "Interpreted in public footing"

        # detectors read the ontology surfaces (cross-container)
        ggen.lsp.detectors -> ggen.ontology.sparql "Reads producer set"
        ggen.lsp.detectors -> ggen.ontology.templates "Reads consumer set"
        ggen.lsp.harnessAnalyzer -> ggen.ontology.ggenToml "Distinguishes ggen.toml from Cargo/Makefile.toml by basename"
    }

    views {
        systemContext ggen "C1_Context" "C1 — System context: receipted manufacturing with author, agents, editor, git/CI, and the wasm4pm process-law oracle." {
            include *
            autolayout lr
        }

        container ggen "C2_Containers" "C2 — Containers: ggen-lsp (read-only admissibility), ggen sync (only actuation), open ontology (source law), graph, intel/OCEL log, receipts." {
            include *
            autolayout lr
        }

        component ggen.lsp "C3_ggen_lsp" "C3 — ggen-lsp: the living nerve. ServerState.analyze_and_observe -> detectors -> route-at-raise -> observe_diagnostics -> residual-preserving clear -> events -> OCEL log." {
            include *
            autolayout lr
        }

        component ggen.ontology "C3_Open_Ontology" "C3 — Open ontology / source law: ggen.toml binds producer (SPARQL) / consumer (template) / output, framed by public vocabulary." {
            include *
            autolayout lr
        }

        dynamic ggen.lsp "C4_Living_Clear" "C4 / runtime — cross-surface repair & living clear. clear = keyed subtraction + residual preservation; ALIVE only when the full chain reaches ReceiptEmitted with external evidence." {
            editor -> ggen.lsp.server "didChange (template invalid, then query repaired)"
            ggen.lsp.server -> ggen.lsp.state "analyze_and_observe(uri, content)"
            ggen.lsp.state -> ggen.lsp.detectors "recompute project relation"
            ggen.lsp.detectors -> ggen.lsp.routes "resolve route-at-raise (source-law only)"
            ggen.lsp.state -> ggen.lsp.events "raise -> DiagnosticRaised + RouteSelected + RepairSuggested"
            ggen.lsp.state -> ggen.lsp.state "cross-surface repair: old_keys - new_keys, preserve residual"
            ggen.lsp.state -> ggen.lsp.events "clear -> RepairApplied + GatePassed + ReceiptEmitted"
            ggen.lsp.events -> ggen.lsp.logWriter "append"
            ggen.lsp.logWriter -> ggen.intelLog "6-link OCEL chain"
            ggen.intelLog -> wasm4pm "import / mine / conform (process-law judgment)"
            autolayout lr
        }

        styles {
            element "Person" {
                shape person
                background #08427b
                color #ffffff
            }
            element "Software System" {
                background #1168bd
                color #ffffff
            }
            element "External" {
                background #999999
                color #ffffff
            }
            element "Container" {
                background #438dd5
                color #ffffff
            }
            element "Component" {
                background #85bbf0
                color #000000
            }
            element "Planned" {
                background #cccccc
                color #444444
                border dashed
            }
        }
    }
}
