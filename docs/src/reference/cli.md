<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [CLI Reference](#cli-reference)
  - [Global Options](#global-options)
  - [Commands Overview](#commands-overview)
  - [Ontology Commands](#ontology-commands)
    - [ggen ontology extract](#ggen-ontology-extract)
    - [ggen ontology generate](#ggen-ontology-generate)
    - [ggen ontology validate](#ggen-ontology-validate)
    - [ggen ontology init](#ggen-ontology-init)
  - [Packs Commands](#packs-commands)
    - [ggen packs list](#ggen-packs-list)
    - [ggen packs show](#ggen-packs-show)
    - [ggen packs install](#ggen-packs-install)
    - [ggen packs validate](#ggen-packs-validate)
    - [ggen packs score](#ggen-packs-score)
    - [ggen packs compose](#ggen-packs-compose)
    - [ggen packs dependencies](#ggen-packs-dependencies)
    - [ggen packs list_templates](#ggen-packs-list_templates)
    - [ggen packs check_compatibility](#ggen-packs-check_compatibility)
    - [ggen packs sparql](#ggen-packs-sparql)
    - [ggen packs generate](#ggen-packs-generate)
    - [ggen packs publish](#ggen-packs-publish)
    - [ggen packs search_registry](#ggen-packs-search_registry)
    - [ggen packs versions](#ggen-packs-versions)
    - [ggen packs cache](#ggen-packs-cache)
  - [Paper Commands](#paper-commands)
    - [ggen paper new](#ggen-paper-new)
    - [ggen paper generate](#ggen-paper-generate)
    - [ggen paper validate](#ggen-paper-validate)
    - [ggen paper export](#ggen-paper-export)
    - [ggen paper list_templates](#ggen-paper-list_templates)
    - [ggen paper compile](#ggen-paper-compile)
    - [ggen paper init_bibliography](#ggen-paper-init_bibliography)
    - [ggen paper submit](#ggen-paper-submit)
    - [ggen paper track](#ggen-paper-track)
  - [CI Commands](#ci-commands)
    - [ggen ci workflow](#ggen-ci-workflow)
  - [Workflow Commands](#workflow-commands)
    - [ggen workflow init](#ggen-workflow-init)
    - [ggen workflow analyze](#ggen-workflow-analyze)
    - [ggen workflow discover](#ggen-workflow-discover)
    - [ggen workflow event](#ggen-workflow-event)
    - [ggen workflow report](#ggen-workflow-report)
  - [Marketplace Commands](#marketplace-commands)
    - [ggen marketplace search](#ggen-marketplace-search)
    - [ggen marketplace install](#ggen-marketplace-install)
    - [ggen marketplace list](#ggen-marketplace-list)
    - [ggen marketplace publish](#ggen-marketplace-publish)
  - [AI Commands](#ai-commands)
    - [ggen ai generate-ontology](#ggen-ai-generate-ontology)
    - [ggen ai chat](#ggen-ai-chat)
    - [ggen ai analyze](#ggen-ai-analyze)
  - [Template Commands](#template-commands)
    - [ggen template generate-rdf](#ggen-template-generate-rdf)
    - [ggen template list](#ggen-template-list)
    - [ggen template lint](#ggen-template-lint)
  - [Graph Commands](#graph-commands)
    - [ggen graph load](#ggen-graph-load)
    - [ggen graph query](#ggen-graph-query)
    - [ggen graph validate](#ggen-graph-validate)
    - [ggen graph export](#ggen-graph-export)
  - [Hook Commands](#hook-commands)
    - [ggen hook create](#ggen-hook-create)
    - [ggen hook list](#ggen-hook-list)
    - [ggen hook remove](#ggen-hook-remove)
    - [ggen hook monitor](#ggen-hook-monitor)
  - [Project Commands](#project-commands)
    - [ggen project new](#ggen-project-new)
    - [ggen project gen](#ggen-project-gen)
  - [Utils Commands](#utils-commands)
    - [ggen utils doctor](#ggen-utils-doctor)
    - [ggen utils env](#ggen-utils-env)
    - [ggen utils fmea](#ggen-utils-fmea)
  - [Command Examples](#command-examples)
    - [Complete Workflow](#complete-workflow)
    - [Marketplace Workflow](#marketplace-workflow)
  - [Exit Codes](#exit-codes)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# CLI Reference

Complete reference for all `ggen` command-line interface commands.

## Global Options

All commands support these global flags:

```bash
--help, -h          Show help information
--version, -V       Show version information
--json              Output in JSON format
--verbose, -v       Enable verbose output
--quiet, -q         Suppress output
```

## Commands Overview

ggen provides 13 main command categories:

- **ai** - AI-powered code generation and analysis
- **ci** - CI/CD workflow generation
- **graph** - RDF graph operations
- **hook** - Git hooks and automation
- **ontology** - RDF/OWL ontology management
- **packs** - Package management and composition
- **paper** - Academic paper generation and management
- **project** - Create and manage projects
- **template** - Template management and generation
- **utils** - System utilities and diagnostics
- **workflow** - Process mining and workflow analytics
- **marketplace** - Search, install, and publish templates (DISABLED - v2 migration)
- **fmea** - Failure Mode and Effects Analysis (via utils)

## Ontology Commands

RDF/OWL ontology management: extract, generate code, validate, and initialize projects.

### ggen ontology extract

Extract ontology schema from RDF/OWL files.

**Usage:**
```bash
ggen ontology extract <FILE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - Ontology file path (Turtle, RDF/XML, N-Triples)

**Options:**
- `--namespace <URI>` - Namespace URI (default: http://example.org#)
- `--output <FILE>` - Output JSON schema file

**Examples:**
```bash
ggen ontology extract schema.ttl
ggen ontology extract ecommerce.rdf --namespace http://ecommerce.org#
ggen ontology extract domain.ttl --output schema.json
```

### ggen ontology generate

Generate code from ontology schema (TypeScript, Zod, utilities).

**Usage:**
```bash
ggen ontology generate <SCHEMA_FILE> <LANGUAGE> [OPTIONS]
```

**Arguments:**
- `<SCHEMA_FILE>` - Schema JSON file from extract command
- `<LANGUAGE>` - Target language (typescript)

**Options:**
- `--output <DIR>` - Output directory (default: generated)
- `--zod` - Generate Zod validation schemas
- `--utilities` - Generate utility types

**Examples:**
```bash
ggen ontology generate schema.json typescript --output src/types
ggen ontology generate schema.json typescript --zod --utilities
```

### ggen ontology validate

Validate ontology quality and structure.

**Usage:**
```bash
ggen ontology validate <SCHEMA_FILE> [OPTIONS]
```

**Arguments:**
- `<SCHEMA_FILE>` - Schema JSON file to validate

**Options:**
- `--strict` - Enable strict validation (checks circular references)

**Examples:**
```bash
ggen ontology validate schema.json
ggen ontology validate domain.json --strict
```

### ggen ontology init

Initialize new ontology project with templates.

**Usage:**
```bash
ggen ontology init <PROJECT_NAME> [OPTIONS]
```

**Arguments:**
- `<PROJECT_NAME>` - New project name

**Options:**
- `--template <TEMPLATE>` - Template (schema.org, foaf, dublincore)

**Examples:**
```bash
ggen ontology init my-project
ggen ontology init ecommerce-api --template schema.org
ggen ontology init social-network --template foaf
```

---

## Packs Commands

Package management: install, compose, validate, and publish templates.

### ggen packs list

List all available packs.

**Usage:**
```bash
ggen packs list [OPTIONS]
```

**Options:**
- `--category <CAT>` - Filter by category (startup, enterprise, data-science)

**Examples:**
```bash
ggen packs list
ggen packs list --category startup
```

### ggen packs show

Show detailed pack information.

**Usage:**
```bash
ggen packs show --pack_id <ID>
```

**Options:**
- `--pack_id <ID>` - Pack identifier

**Examples:**
```bash
ggen packs show --pack_id startup-essentials
ggen packs show --pack_id enterprise-backend
```

### ggen packs install

Install a pack (all packages, templates, and queries).

**Usage:**
```bash
ggen packs install --pack_id <ID> [OPTIONS]
```

**Options:**
- `--pack_id <ID>` - Pack identifier
- `--target_dir <DIR>` - Installation directory
- `--force` - Force reinstall
- `--dry_run` - Show what would be installed

**Examples:**
```bash
ggen packs install --pack_id startup-essentials
ggen packs install --pack_id data-science --target_dir ./my-project
ggen packs install --pack_id enterprise-backend --dry_run
```

### ggen packs validate

Validate pack structure and completeness.

**Usage:**
```bash
ggen packs validate --pack_id <ID>
```

**Examples:**
```bash
ggen packs validate --pack_id startup-essentials
```

### ggen packs score

Score pack maturity across 4 dimensions.

**Usage:**
```bash
ggen packs score --pack_id <ID>
```

**Dimensions:**
- Documentation
- Completeness
- Quality
- Usability

**Examples:**
```bash
ggen packs score --pack_id enterprise-backend
```

### ggen packs compose

Compose multiple packs into single project.

**Usage:**
```bash
ggen packs compose --pack_ids <ID1,ID2,...> --project_name <NAME> [OPTIONS]
```

**Options:**
- `--output_dir <DIR>` - Output directory

**Examples:**
```bash
ggen packs compose --pack_ids startup-essentials,data-science --project_name my-app
ggen packs compose --pack_ids pack1,pack2,pack3 --project_name enterprise-project
```

### ggen packs dependencies

Show pack dependency graph.

**Usage:**
```bash
ggen packs dependencies --pack_id <ID>
```

**Examples:**
```bash
ggen packs dependencies --pack_id enterprise-backend
```

### ggen packs list_templates

List templates in a pack.

**Usage:**
```bash
ggen packs list_templates --pack_id <ID>
```

**Examples:**
```bash
ggen packs list_templates --pack_id startup-essentials
```

### ggen packs check_compatibility

Check if multiple packs are compatible.

**Usage:**
```bash
ggen packs check_compatibility --pack_ids <ID1,ID2,...>
```

**Examples:**
```bash
ggen packs check_compatibility --pack_ids startup-essentials,enterprise-backend
ggen packs check_compatibility --pack_ids pack1,pack2,pack3
```

### ggen packs sparql

Execute SPARQL query on pack metadata.

**Usage:**
```bash
ggen packs sparql --pack_id <ID> --query <QUERY>
```

**Examples:**
```bash
ggen packs sparql --pack_id enterprise-backend --query "SELECT * WHERE { ?s ?p ?o }"
ggen packs sparql --pack_id startup-essentials --query "SELECT ?package WHERE { ?package rdf:type ggen:Package }"
```

### ggen packs generate

Generate project from pack template.

**Usage:**
```bash
ggen packs generate --pack_id <ID> --project_name <NAME> [OPTIONS]
```

**Options:**
- `--template <NAME>` - Specific template (default: main)

**Examples:**
```bash
ggen packs generate --pack_id startup-essentials --project_name my-app
ggen packs generate --pack_id startup-essentials --project_name my-app --template quick-start
```

### ggen packs publish

Publish pack to registry.

**Usage:**
```bash
ggen packs publish --pack_dir <DIR> --version <VERSION> [OPTIONS]
```

**Options:**
- `--changelog <TEXT>` - Changelog entry

**Examples:**
```bash
ggen packs publish --pack_dir ./my-pack --version 1.0.0
ggen packs publish --pack_dir ./my-pack --version 1.1.0 --changelog "Bug fixes"
```

### ggen packs search_registry

Search packs in registry.

**Usage:**
```bash
ggen packs search_registry [OPTIONS]
```

**Options:**
- `--query <TEXT>` - Search query
- `--category <CAT>` - Filter by category
- `--tags <TAGS>` - Filter by tags (comma-separated)
- `--production_ready` - Only production-ready packs
- `--limit <N>` - Max results (default: 20)

**Examples:**
```bash
ggen packs search_registry --query "web"
ggen packs search_registry --query "api" --category backend --production_ready
```

### ggen packs versions

List all versions of a pack.

**Usage:**
```bash
ggen packs versions --pack_id <ID>
```

**Examples:**
```bash
ggen packs versions --pack_id my-pack
```

### ggen packs cache

Manage CDN cache for packs.

**Usage:**
```bash
ggen packs cache --action <ACTION> [OPTIONS]
```

**Actions:**
- `cache` - Cache a pack
- `stats` - Get cache statistics
- `clear` - Clear all cache

**Examples:**
```bash
ggen packs cache --action cache --pack_id my-pack
ggen packs cache --action stats --pack_id my-pack
ggen packs cache --action clear
```

---

## Paper Commands

Academic paper generation and management with LaTeX, BibTeX, and submission tracking.

### ggen paper new

Create new academic paper project.

**Usage:**
```bash
ggen paper new <NAME> [OPTIONS]
```

**Arguments:**
- `<NAME>` - Paper name

**Options:**
- `--template <TEMPLATE>` - Paper template (ieee, acm, neurips, arxiv, phd, etc.)
- `--output <DIR>` - Output directory

**Available Templates:**
- ieee-conference, acm-journal, neurips-conference, arxiv-preprint
- phd-thesis, masters-thesis, nature-journal, science-journal, pnas-journal
- icml-conference, iclr-conference, cvpr-conference, iccv-conference

**Examples:**
```bash
ggen paper new "Deep Learning for Code Generation" --template ieee
ggen paper new "Semantic Code Projections" --template arxiv --output ./papers
```

### ggen paper generate

Generate LaTeX from paper ontology.

**Usage:**
```bash
ggen paper generate <FILE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - Paper RDF file

**Options:**
- `--style <STYLE>` - Output style (default: arxiv)
- `--output <FILE>` - Output LaTeX file

**Examples:**
```bash
ggen paper generate my-paper.rdf --style ieee --output my-paper.tex
ggen paper generate thesis.rdf --style phd --output thesis.tex
```

### ggen paper validate

Validate paper ontology and structure.

**Usage:**
```bash
ggen paper validate <FILE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - Paper RDF file

**Options:**
- `--check <CHECKS>` - Specific checks (formatting, citations, metadata)
- `--strict` - Strict validation mode

**Examples:**
```bash
ggen paper validate my-paper.rdf
ggen paper validate research.rdf --check formatting,citations --strict
```

### ggen paper export

Export paper to multiple formats.

**Usage:**
```bash
ggen paper export <FILE> --format <FORMAT> [OPTIONS]
```

**Arguments:**
- `<FILE>` - Paper RDF file
- `--format` - Export format (pdf, html, json-ld, docx)

**Options:**
- `--output <DIR>` - Output directory

**Examples:**
```bash
ggen paper export my-paper.rdf --format pdf
ggen paper export research.rdf --format html --output ./public
ggen paper export paper.rdf --format json-ld
```

### ggen paper list_templates

List available paper templates.

**Usage:**
```bash
ggen paper list-templates [OPTIONS]
```

**Options:**
- `--filter <FILTER>` - Filter templates (conference, journal, thesis)

**Examples:**
```bash
ggen paper list-templates
ggen paper list-templates --filter conference
```

### ggen paper compile

Compile LaTeX to PDF.

**Usage:**
```bash
ggen paper compile <FILE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - LaTeX file

**Options:**
- `--engine <ENGINE>` - LaTeX engine (pdflatex, xelatex)
- `--bibtex` - Run BibTeX for bibliography

**Examples:**
```bash
ggen paper compile my-paper.tex
ggen paper compile thesis.tex --engine xelatex --bibtex
```

### ggen paper init_bibliography

Initialize BibTeX bibliography.

**Usage:**
```bash
ggen paper init-bibliography <FILE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - Paper RDF file

**Options:**
- `--output <FILE>` - Output BibTeX file

**Examples:**
```bash
ggen paper init-bibliography my-paper.rdf
ggen paper init-bibliography paper.rdf --output refs.bib
```

### ggen paper submit

Submit paper to venue (arXiv, conference, journal).

**Usage:**
```bash
ggen paper submit <FILE> --venue <VENUE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - Paper PDF file
- `--venue` - Target venue

**Options:**
- `--metadata <FILE>` - Paper metadata RDF file

**Examples:**
```bash
ggen paper submit my-paper.pdf --venue arxiv --category cs.AI
ggen paper submit research.pdf --venue neurips-2024
```

### ggen paper track

Track paper submission and peer review status.

**Usage:**
```bash
ggen paper track <FILE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - Paper RDF file

**Options:**
- `--venue <VENUE>` - Track specific venue

**Examples:**
```bash
ggen paper track my-paper.rdf
ggen paper track research.rdf --venue neurips-2024
```

---

## CI Commands

CI/CD workflow generation and integration.

### ggen ci workflow

Generate CI workflow configuration.

**Usage:**
```bash
ggen ci workflow [OPTIONS]
```

**Examples:**
```bash
ggen ci workflow
ggen ci workflow --help
```

---

## Workflow Commands

Process mining and workflow analytics using event logs.

### ggen workflow init

Initialize workflow for tracking and analysis.

**Usage:**
```bash
ggen workflow init --name <NAME> [OPTIONS]
```

**Options:**
- `--name <NAME>` - Workflow name
- `--type <TYPE>` - Workflow type (research, maturity, revops)
- `--output_dir <DIR>` - Output directory

**Examples:**
```bash
ggen workflow init --name "university-research" --type research
ggen workflow init --name "package-maturity" --type maturity
ggen workflow init --name "revops-pipeline" --type revops
```

### ggen workflow analyze

Analyze workflow events and generate statistics.

**Usage:**
```bash
ggen workflow analyze --workflow-file <FILE> [OPTIONS]
```

**Options:**
- `--summary` - Show summary statistics

**Examples:**
```bash
ggen workflow analyze --workflow-file workflow.json
ggen workflow analyze --workflow-file workflow.json --summary
```

### ggen workflow discover

Discover process patterns and generate visualization.

**Usage:**
```bash
ggen workflow discover --workflow-file <FILE> [OPTIONS]
```

**Options:**
- `--export <FORMAT>` - Export format (mermaid, svg, png)
- `--pareto` - Show 80/20 critical path

**Examples:**
```bash
ggen workflow discover --workflow-file workflow.json
ggen workflow discover --workflow-file workflow.json --export mermaid --pareto
```

### ggen workflow event

Record workflow event in log.

**Usage:**
```bash
ggen workflow event --workflow-file <FILE> --case-id <ID> --activity <ACTIVITY> [OPTIONS]
```

**Options:**
- `--resource <RESOURCE>` - Resource/actor performing activity

**Examples:**
```bash
ggen workflow event --workflow-file workflow.json --case-id paper-123 --activity "CodeGenerated" --resource "researcher-1"
```

### ggen workflow report

Generate workflow report.

**Usage:**
```bash
ggen workflow report --workflow-file <FILE> [OPTIONS]
```

**Options:**
- `--format <FORMAT>` - Report format (html, json)
- `--output <FILE>` - Output file path

**Examples:**
```bash
ggen workflow report --workflow-file workflow.json --format html --output report.html
ggen workflow report --workflow-file workflow.json --format json --output report.json
```

---

## Marketplace Commands

Search, install, and publish templates (DISABLED - v2 migration in progress).

### ggen marketplace search

Search for packages in the marketplace.

**Usage:**
```bash
ggen marketplace search <QUERY> [OPTIONS]
```

**Arguments:**
- `<QUERY>` - Search query string

**Options:**
- `--limit <N>` - Maximum number of results (default: 10)
- `--category <CAT>` - Filter by category

**Examples:**
```bash
ggen marketplace search "rust microservice"
ggen marketplace search "api" --category backend --limit 20
```

### ggen marketplace install

Install a package from the marketplace.

**Usage:**
```bash
ggen marketplace install <PACKAGE_ID> [OPTIONS]
```

**Arguments:**
- `<PACKAGE_ID>` - Package identifier (e.g., `io.ggen.rust.microservice`)

**Options:**
- `--version <VERSION>` - Install specific version
- `--force` - Force reinstall

**Examples:**
```bash
ggen marketplace install io.ggen.rust.microservice
ggen marketplace install io.ggen.rust.microservice --version 1.2.0
```

### ggen marketplace list

List installed packages.

**Usage:**
```bash
ggen marketplace list [OPTIONS]
```

**Options:**
- `--json` - Output as JSON

**Examples:**
```bash
ggen marketplace list
ggen marketplace list --json
```

### ggen marketplace publish

Publish a package to the marketplace.

**Usage:**
```bash
ggen marketplace publish [OPTIONS]
```

**Options:**
- `--name <NAME>` - Package name
- `--version <VERSION>` - Package version
- `--template-dir <DIR>` - Template directory

**Examples:**
```bash
ggen marketplace publish --name my-template --version 1.0.0 --template-dir ./templates
```

## AI Commands

### ggen ai generate-ontology

Generate an RDF ontology using AI.

**Usage:**
```bash
ggen ai generate-ontology --prompt <PROMPT> --output <FILE> [OPTIONS]
```

**Arguments:**
- `--prompt <PROMPT>` - Natural language description of domain
- `--output <FILE>` - Output RDF file path

**Options:**
- `--provider <PROVIDER>` - AI provider (anthropic, openai, ollama)
- `--model <MODEL>` - Model name

**Examples:**
```bash
ggen ai generate-ontology --prompt "E-commerce: Product, Order, Customer" --output domain.ttl
```

### ggen ai chat

Interactive AI chat session.

**Usage:**
```bash
ggen ai chat [OPTIONS]
```

**Options:**
- `--interactive` - Interactive mode
- `--prompt <PROMPT>` - Single prompt
- `--input <FILE>` - Input file

**Examples:**
```bash
ggen ai chat --interactive
ggen ai chat --prompt "Improve this ontology" --input domain.ttl
```

### ggen ai analyze

Analyze codebase with AI.

**Usage:**
```bash
ggen ai analyze <PATH> [OPTIONS]
```

**Arguments:**
- `<PATH>` - Path to analyze

**Options:**
- `--focus <ASPECT>` - Focus area (domain-model, architecture, etc.)
- `--suggest-improvements` - Suggest improvements

**Examples:**
```bash
ggen ai analyze src/ --focus domain-model
```

## Template Commands

### ggen template generate-rdf

Generate code from RDF ontology using template.

**Usage:**
```bash
ggen template generate-rdf --ontology <FILE> --template <TEMPLATE> --output <DIR> [OPTIONS]
```

**Arguments:**
- `--ontology <FILE>` - RDF ontology file
- `--template <TEMPLATE>` - Template name or path
- `--output <DIR>` - Output directory

**Options:**
- `--vars <KEY=VALUE>` - Template variables
- `--dry-run` - Show what would be generated

**Examples:**
```bash
ggen template generate-rdf --ontology domain.ttl --template rust-models --output src/
```

### ggen template list

List available templates.

**Usage:**
```bash
ggen template list [OPTIONS]
```

**Examples:**
```bash
ggen template list
```

### ggen template lint

Validate template syntax.

**Usage:**
```bash
ggen template lint <TEMPLATE> [OPTIONS]
```

**Arguments:**
- `<TEMPLATE>` - Template file or name

**Examples:**
```bash
ggen template lint templates/rust-models/models.rs.tmpl
```

## Graph Commands

### ggen graph load

Load RDF graph from file.

**Usage:**
```bash
ggen graph load <FILE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - RDF file path

**Examples:**
```bash
ggen graph load domain.ttl
```

### ggen graph query

Execute SPARQL query against graph.

**Usage:**
```bash
ggen graph query <FILE> --sparql <QUERY> [OPTIONS]
```

**Arguments:**
- `<FILE>` - RDF file path
- `--sparql <QUERY>` - SPARQL query string

**Options:**
- `--file <FILE>` - SPARQL query file

**Examples:**
```bash
ggen graph query domain.ttl --sparql "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"
```

### ggen graph validate

Validate RDF graph.

**Usage:**
```bash
ggen graph validate <FILE> [OPTIONS]
```

**Arguments:**
- `<FILE>` - RDF file path

**Options:**
- `--shacl <FILE>` - SHACL shapes file
- `--verbose` - Verbose output

**Examples:**
```bash
ggen graph validate domain.ttl
ggen graph validate domain.ttl --shacl shapes.ttl
```

### ggen graph export

Export graph to different format.

**Usage:**
```bash
ggen graph export <FILE> --format <FORMAT> [OPTIONS]
```

**Arguments:**
- `<FILE>` - RDF file path
- `--format <FORMAT>` - Output format (turtle, json-ld, n-triples)

**Examples:**
```bash
ggen graph export domain.ttl --format json-ld
```

## Hook Commands

### ggen hook create

Create a Git hook.

**Usage:**
```bash
ggen hook create <PHASE> --name <NAME> --command <CMD> [OPTIONS]
```

**Arguments:**
- `<PHASE>` - Lifecycle phase (pre-commit, post-merge, etc.)
- `--name <NAME>` - Hook name
- `--command <CMD>` - Command to execute

**Examples:**
```bash
ggen hook create pre-commit --name validate --command "ggen graph validate domain.ttl"
```

### ggen hook list

List registered hooks.

**Usage:**
```bash
ggen hook list [OPTIONS]
```

**Examples:**
```bash
ggen hook list
```

### ggen hook remove

Remove a hook.

**Usage:**
```bash
ggen hook remove <PHASE> <NAME>
```

**Arguments:**
- `<PHASE>` - Lifecycle phase
- `<NAME>` - Hook name

**Examples:**
```bash
ggen hook remove pre-commit validate
```

### ggen hook monitor

Monitor hook execution.

**Usage:**
```bash
ggen hook monitor [OPTIONS]
```

**Examples:**
```bash
ggen hook monitor
```

## Project Commands

### ggen project new

Create a new project.

**Usage:**
```bash
ggen project new <NAME> [OPTIONS]
```

**Arguments:**
- `<NAME>` - Project name

**Options:**
- `--type <TYPE>` - Project type
- `--framework <FRAMEWORK>` - Framework name

**Examples:**
```bash
ggen project new my-app --type rust-web --framework axum
```

### ggen project gen

Generate code from project plan.

**Usage:**
```bash
ggen project gen <NAME> [OPTIONS]
```

**Arguments:**
- `<NAME>` - Project name

**Options:**
- `--template <TEMPLATE>` - Template to use
- `--vars <KEY=VALUE>` - Variables

**Examples:**
```bash
ggen project gen my-service --template io.ggen.rust.microservice
```

## Utils Commands

Utility commands for system diagnostics, environment management, and FMEA analysis.

### ggen utils doctor

System diagnostics and health check.

**Usage:**
```bash
ggen utils doctor [OPTIONS]
```

**Options:**
- `--all` - Run all checks (verbose)
- `--fix` - Attempt to fix issues
- `--format <FORMAT>` - Output format (table, json)

**Examples:**
```bash
ggen utils doctor
ggen utils doctor --all
ggen utils doctor --fix
```

### ggen utils env

Environment variable management.

**Usage:**
```bash
ggen utils env [OPTIONS]
```

**Options:**
- `--list` - List all GGEN_ environment variables
- `--get <KEY>` - Get specific variable value
- `--set <KEY=VALUE>` - Set environment variable
- `--system` - Manage system-level variables

**Examples:**
```bash
ggen utils env --list
ggen utils env --get GGEN_HOME
ggen utils env --set GGEN_HOME=/usr/local/ggen
```

### ggen utils fmea

Failure Mode and Effects Analysis (FMEA) reporting and analysis.

**Usage:**
```bash
ggen utils fmea <SUBCOMMAND> [OPTIONS]
```

**Subcommands:**

#### ggen utils fmea report

Generate FMEA report with failure modes sorted by risk priority.

**Usage:**
```bash
ggen utils fmea report [OPTIONS]
```

**Options:**
- `--format <FORMAT>` - Output format (text, json) - default: text
- `--risk <LEVEL>` - Filter by risk level (LOW, MEDIUM, HIGH, CRITICAL)
- `--top <N>` - Limit to top N modes (default: 20)

**Examples:**
```bash
ggen utils fmea report
ggen utils fmea report --format json
ggen utils fmea report --risk HIGH --top 10
```

#### ggen utils fmea pareto

Generate Pareto analysis showing 80/20 distribution of risk.

**Usage:**
```bash
ggen utils fmea pareto
```

**Examples:**
```bash
ggen utils fmea pareto
```

#### ggen utils fmea list

List failure modes with filtering and sorting.

**Usage:**
```bash
ggen utils fmea list [OPTIONS]
```

**Options:**
- `--category <CAT>` - Filter by category (fileio, networkops, concurrencyrace, etc.)
- `--sort <FIELD>` - Sort by field (rpn, id, severity)

**Categories:**
- fileio, networkops, concurrencyrace, inputvalidation
- templaterendering, dependencyresolution, memoryexhaustion
- deserialization

**Examples:**
```bash
ggen utils fmea list
ggen utils fmea list --category fileio --sort severity
```

#### ggen utils fmea show

Show detailed information about a failure mode.

**Usage:**
```bash
ggen utils fmea show <MODE_ID> [OPTIONS]
```

**Arguments:**
- `<MODE_ID>` - Failure mode identifier

**Options:**
- `--events` - Show recorded events for this failure mode

**Examples:**
```bash
ggen utils fmea show FM-001
ggen utils fmea show FM-003 --events
```

#### ggen utils fmea export

Export FMEA data to JSON for integration or analysis.

**Usage:**
```bash
ggen utils fmea export [OPTIONS]
```

**Options:**
- `--output <FILE>` - Output JSON file (default: fmea-report.json)

**Examples:**
```bash
ggen utils fmea export
ggen utils fmea export --output risk-analysis.json
```

## Command Examples

### Complete Workflow

```bash
# 1. Generate ontology
ggen ai generate-ontology --prompt "Blog system" --output blog.ttl

# 2. Validate ontology
ggen graph validate blog.ttl

# 3. Generate Rust code
ggen template generate-rdf --ontology blog.ttl --template rust-models --output src/

# 4. Set up hook for auto-regeneration
ggen hook create post-merge --name regenerate --command "ggen template generate-rdf --ontology blog.ttl --template rust-models --output src/"
```

### Marketplace Workflow

```bash
# 1. Search for templates
ggen marketplace search "rust api"

# 2. Install template
ggen marketplace install io.ggen.rust.axum-api

# 3. Use template
ggen template generate-rdf --ontology domain.ttl --template io.ggen.rust.axum-api --output src/
```

## Exit Codes

- `0` - Success
- `1` - General error
- `2` - Invalid arguments
- `3` - File not found
- `4` - Validation error

## See Also

- [Getting Started Tutorial](../tutorials/getting-started.md)
- [Installation Guide](../how-to-guides/installation.md)
- [Troubleshooting Guide](../how-to-guides/troubleshoot.md)

