# Data Model: Fix Template Rendering Engine

**Branch**: `005-fix-rendering-engine`
**Date**: 2025-12-13

## Entities

### Template

A file with YAML frontmatter and Tera template body.

| Field | Type | Description |
|-------|------|-------------|
| `raw_frontmatter` | `serde_yaml::Value` | Raw YAML before variable rendering |
| `front` | `Frontmatter` | Parsed frontmatter struct |
| `body` | `String` | Template content after `---` delimiter |

**Location**: `crates/ggen-core/src/template.rs:180-185`

### Frontmatter

Configuration extracted from template YAML header.

| Field | Type | Description |
|-------|------|-------------|
| `to` | `Option<String>` | Output file path |
| `rdf` | `Vec<String>` | External RDF file paths |
| `rdf_inline` | `Vec<String>` | Inline RDF triples |
| `prefixes` | `BTreeMap<String, String>` | PREFIX declarations for SPARQL |
| `base` | `Option<String>` | BASE IRI for relative URIs |
| `sparql` | `BTreeMap<String, String>` | Named SPARQL queries |
| `sparql_results` | `BTreeMap<String, serde_json::Value>` | Query results (populated during processing) |

**Location**: `crates/ggen-core/src/template.rs:45-132`

### RenderWithRdfOptions

Options for RDF-aware template rendering.

| Field | Type | Description |
|-------|------|-------------|
| `template_path` | `PathBuf` | Path to template file |
| `output_path` | `PathBuf` | Path for output file(s) |
| `rdf_files` | `Vec<PathBuf>` | Additional RDF files (CLI-provided) |
| `variables` | `BTreeMap<String, String>` | User-provided template variables |
| `force_overwrite` | `bool` | Overwrite existing files |
| `use_preprocessor` | `bool` | Enable template preprocessor |

**Location**: `crates/ggen-domain/src/template/render_with_rdf.rs:20-50`

### RenderWithRdfResult

Result metrics from generation.

| Field | Type | Description |
|-------|------|-------------|
| `output_path` | `PathBuf` | Primary output file path |
| `bytes_written` | `usize` | Total bytes written |
| `rdf_files_loaded` | `usize` | Number of RDF files processed |
| `sparql_queries_executed` | `usize` | Number of SPARQL queries run |
| `files_created` | `usize` | Number of output files (multi-file) |

**Location**: `crates/ggen-domain/src/template/render_with_rdf.rs:52-70`

## Relationships

```
┌─────────────────────┐
│    CLI Command      │
│  template generate  │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐     ┌─────────────────────┐
│ GenerateFileOptions │ OR  │ RenderWithRdfOptions│
│   (no RDF support)  │     │   (full RDF support)│
└──────────┬──────────┘     └──────────┬──────────┘
           │                           │
           ▼                           ▼
┌─────────────────────┐     ┌─────────────────────┐
│   generate_file()   │     │  render_with_rdf()  │
└─────────────────────┘     └──────────┬──────────┘
                                       │
                                       ▼
                            ┌─────────────────────┐
                            │      Template       │
                            │  (parse + render)   │
                            └──────────┬──────────┘
                                       │
                            ┌──────────┴──────────┐
                            ▼                     ▼
                   ┌─────────────┐       ┌─────────────┐
                   │ Frontmatter │       │    Body     │
                   │  (config)   │       │  (content)  │
                   └──────┬──────┘       └──────┬──────┘
                          │                     │
                          ▼                     ▼
                   ┌─────────────┐       ┌─────────────┐
                   │   Graph     │       │    Tera     │
                   │ (Oxigraph)  │       │  (render)   │
                   └──────┬──────┘       └──────┬──────┘
                          │                     │
                          ▼                     │
                   ┌─────────────┐              │
                   │   SPARQL    │              │
                   │  execution  │              │
                   └──────┬──────┘              │
                          │                     │
                          ▼                     │
                   ┌─────────────┐              │
                   │sparql_results│─────────────┘
                   └─────────────┘
                          │
                          ▼
                   ┌─────────────┐
                   │   Output    │
                   │   File(s)   │
                   └─────────────┘
```

## State Transitions

### Template Processing States

1. **Parsed**: Template parsed from string, raw frontmatter stored
2. **Frontmatter Rendered**: Variables resolved in frontmatter YAML
3. **Graph Loaded**: RDF data loaded into Oxigraph
4. **Queries Executed**: SPARQL queries run, results stored
5. **Body Rendered**: Template body rendered with sparql_results
6. **Output Written**: File(s) written to disk

### Detection Logic (New)

```
Template Read
    │
    ▼
Parse Frontmatter (quick YAML check)
    │
    ├─── Has rdf/rdf_inline/sparql? ───▶ RenderWithRdfOptions
    │                                          │
    │                                          ▼
    │                                   render_with_rdf()
    │
    └─── No RDF fields? ───────────────▶ GenerateFileOptions
                                              │
                                              ▼
                                        generate_file()
```
