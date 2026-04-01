# Pipeline Integration

**Version:** 6.0.1
**Last Updated:** 2026-04-01
**Status:** Production (pack μ₂ wiring landed in v6.1 track)

## Overview

The marketplace integrates with the ggen pipeline through a **μ₀ stage** that resolves packs before μ₁–μ₅. **Implementation** (not the illustrative snippets below): [crates/ggen-core/src/pack_resolver.rs](../../crates/ggen-core/src/pack_resolver.rs), [crates/ggen-core/src/v6/pipeline.rs](../../crates/ggen-core/src/v6/pipeline.rs). Pack cache: `~/.ggen/packs` or `GGEN_PACK_CACHE_DIR`. **Pack SPARQL:** CONSTRUCT-only — [PACK_QUERY_CONTRACT.md](PACK_QUERY_CONTRACT.md).

### Pipeline Stages

```
μ₀: Pack Resolution     → Resolve bundles to atomic packs, merge ontologies
μ₁: Normalization       → Normalize merged ontology
μ₂: Extraction          → Execute SPARQL queries (project + pack queries)
μ₃: Emission            → Render Tera templates (project + pack templates)
μ₄: Canonicalization    → Deterministic hashing
μ₅: Receipt             → Cryptographic provenance (with pack contributions)
```

## μ₀ Stage: Pack Resolution

### PackResolver

The `PackResolver` handles all pack-related operations before the pipeline starts:

```rust
use crate::packs::{AtomicPackId, Bundle, Profile};

pub struct PackResolver {
    registry: Arc<RdfRegistry>,
    lockfile: Lockfile,
}

impl PackResolver {
    /// μ₀: Resolve packs to atomic set, merge ontologies
    pub async fn resolve_packs(&self) -> Result<ResolvedPacks> {
        // 1. Read lockfile
        let requested = self.lockfile.read()?;

        // 2. Expand bundles to atomic packs
        let atomic_packs = self.expand_bundles(&requested)?;

        // 3. Resolve dependencies
        let resolved = self.resolve_dependencies(&atomic_packs).await?;

        // 4. Check compatibility (multi-dimensional)
        self.check_compatibility(&resolved)?;

        // 5. Merge pack ontologies into project graph
        let merged_ontology = self.merge_ontologies(&resolved).await?;

        // 6. Detect conflicts (ownership map)
        let ownership_map = self.build_ownership_map(&resolved)?;

        Ok(ResolvedPacks {
            atomic_packs: resolved,
            merged_ontology,
            ownership_map,
        })
    }

    fn expand_bundles(&self, requested: &[PackRef]) -> Result<Vec<AtomicPackId>> {
        let mut atomic = Vec::new();
        for pack in requested {
            if let Some(bundle) = self.registry.get_bundle(pack)? {
                atomic.extend(bundle.expand());
            } else {
                atomic.push(AtomicPackId::from(pack.clone()));
            }
        }
        Ok(atomic)
    }

    async fn merge_ontologies(&self, packs: &[AtomicPackId]) -> Result<Graph> {
        let mut merged = Graph::new();

        // Load foundation ontology first
        for foundation in FOUNDATION_PACKS {
            let ontology = self.registry.load_pack_ontology(foundation).await?;
            merged.merge(&ontology)?;
        }

        // Load pack ontologies
        for pack in packs {
            let ontology = self.registry.load_pack_ontology(pack).await?;
            merged.merge(&ontology)?;
        }

        Ok(merged)
    }

    fn build_ownership_map(&self, packs: &[AtomicPackId]) -> Result<OwnershipMap> {
        let mut map = HashMap::new();

        for pack in packs {
            let declarations = self.registry.get_ownership_declarations(pack)?;
            for decl in declarations {
                if let Some(existing) = map.get(&decl.target) {
                    // Check ownership class compatibility
                    match (&existing.class, &decl.class) {
                        (OwnershipClass::Exclusive, OwnershipClass::Exclusive) => {
                            bail!("Conflict: {:?} owned by both {} and {}",
                                  decl.target, existing.owner_pack, decl.owner_pack);
                        }
                        _ => { /* mergeable or overlay */ }
                    }
                }
                map.insert(decl.target.clone(), decl);
            }
        }

        Ok(OwnershipMap { declarations: map })
    }
}

pub struct ResolvedPacks {
    pub atomic_packs: Vec<AtomicPackId>,
    pub merged_ontology: Graph,
    pub ownership_map: OwnershipMap,
}
```

### Lockfile Reading

The lockfile (`.ggen/packs.lock`) is read at μ₀:

```json
{
  "version": 1,
  "bundles": [
    {
      "bundle_id": "mcp-rust-axum",
      "expanded_to": ["surface-mcp", "projection-rust", "runtime-axum"]
    }
  ],
  "profile": {
    "name": "regulated-finance",
    "policies": ["policy-no-defaults", "policy-strict"]
  },
  "packs": [
    {
      "pack_id": "surface-mcp",
      "version": "1.2.3",
      "digest": "sha256:...",
      "signature": "ed25519:..."
    }
  ],
  "digest": "sha256:...",
  "signature": "ed25519:..."
}
```

```rust
impl Lockfile {
    pub fn read(&self) -> Result<Vec<PackRef>> {
        let lockfile_path = ".ggen/packs.lock";
        let lockfile_json = std::fs::read_to_string(lockfile_path)?;
        let lockfile: Lockfile = serde_json::from_str(&lockfile_json)?;

        // Verify digest and signature
        lockfile.verify()?;

        Ok(lockfile.packs.into_iter()
            .map(|p| PackRef {
                pack_id: p.pack_id,
                version: p.version,
            })
            .collect())
    }
}
```

## μ₁ Stage: Normalization

The merged ontology from μ₀ is normalized:

```rust
impl StagedPipeline {
    fn mu1_normalize(ontology: Graph) -> Result<NormalizedGraph> {
        // Normalization uses merged ontology from μ₀
        // Foundation ontology is already merged in
        let normalized = normalize_rdf(ontology)?;
        Ok(normalized)
    }
}
```

## μ₂ Stage: Extraction

Pack queries are merged with project queries:

```rust
impl StagedPipeline {
    fn mu2_extract(
        graph: &Graph,
        packs: &ResolvedPacks
    ) -> Result<Bindings> {
        let mut bindings = HashMap::new();

        // Load project queries
        for rule in &config.generation.rules {
            let query_results = graph.query(&rule.query)?;
            bindings.insert(rule.name.clone(), query_results);
        }

        // Load pack queries (NEW)
        for pack in &packs.atomic_packs {
            let pack_queries = pack.registry.get_pack_queries(pack)?;
            for query in pack_queries {
                let query_results = graph.query(&query.sparql)?;
                bindings.insert(query.name.clone(), query_results);
            }
        }

        Ok(bindings)
    }
}
```

### Pack Queries

Packs can define SPARQL queries in `package.toml`:

```toml
# marketplace/packages/surface-mcp/package.toml
[queries]
tools = "queries/sparql/tools.rq"
all_tools = "queries/sparql/all_tools.rq"
```

```sparql
# surface-mcp/queries/sparql/tools.rq
PREFIX mcp: <http://ggen.dev/mcp#>

SELECT ?tool_id ?tool_name ?description
WHERE {
  ?tool a mcp:Tool ;
        mcp:id ?tool_id ;
        mcp:name ?tool_name ;
        mcp:description ?description .
}
```

Query results are available in templates:

```tera
# surface-mcp/templates/tools.rs.tera
{% for tool in tools %}
pub fn {{ tool.tool_id | snake_case }}() -> Tool {
    Tool {
        id: "{{ tool.tool_id }}".to_string(),
        name: "{{ tool.tool_name }}".to_string(),
        description: "{{ tool.description }}".to_string(),
    }
}
{% endfor %}
```

## μ₃ Stage: Emission

Pack templates are registered and rendered:

```rust
impl StagedPipeline {
    fn mu3_emit(
        bindings: &Bindings,
        packs: &ResolvedPacks
    ) -> Result<GeneratedFiles> {
        let mut resolver = TemplateResolver::new();

        // Register pack templates (NEW)
        for pack in &packs.atomic_packs {
            let pack_templates = pack.registry.get_pack_templates(pack)?;
            for template in pack_templates {
                // Register with pack_id:path syntax
                resolver.register_pack_template(
                    &pack.id,
                    &template.path,
                    &template.content,
                )?;
            }
        }

        // Render templates
        for rule in &config.generation.rules {
            let output = resolver.render(&rule.template, bindings)?;
            generated_files.insert(rule.output_path.clone(), output);
        }

        Ok(generated_files)
    }
}
```

### Pack Templates

Packs can define Tera templates in `package.toml`:

```toml
# marketplace/packages/surface-mcp/package.toml
[templates]
"src/mcp/server.rs" = "templates/server.rs.tera"
"src/mcp/tools.rs" = "templates/tools.rs.tera"
"src/mcp/mod.rs" = "templates/mod.rs.tera"
```

Templates are rendered with bindings from μ₂:

```tera
# surface-mcp/templates/server.rs.tera
use tokio::net::Stdio;

pub struct McpServer {
    tools: Vec<Tool>,
}

impl McpServer {
    pub fn new() -> Self {
        Self {
            tools: vec![
                {% for tool in tools %}
                {{ tool.tool_id | snake_case }}(),
                {% endfor %}
            ],
        }
    }

    pub async fn run(&mut self) -> Result<()> {
        // Server implementation
        Ok(())
    }
}
```

### Template Registration

Templates are registered with pack-specific prefixes:

```rust
impl TemplateResolver {
    pub fn register_pack_template(
        &mut self,
        pack_id: &AtomicPackId,
        path: &str,
        content: &str,
    ) -> Result<()> {
        let template_name = format!("{}:{}", pack_id, path);
        self.templates.insert(template_name, content.to_string());
        Ok(())
    }
}
```

Usage in `ggen.toml`:

```toml
# ggen.toml
[generation.rules]
[[generation.rules]]
name = "mcp-server"
template = "surface-mcp:src/mcp/server.rs"
output_path = "src/mcp/server.rs"

[[generation.rules]]
name = "mcp-tools"
template = "surface-mcp:src/mcp/tools.rs"
output_path = "src/mcp/tools.rs"
```

## μ₄ Stage: Canonicalization

Canonicalization is unchanged (works on generated files):

```rust
impl StagedPipeline {
    fn mu4_canonicalize(files: &GeneratedFiles) -> Result<CanonicalFiles> {
        let canonical = canonicalize_files(files)?;
        Ok(canonical)
    }
}
```

## μ₅ Stage: Receipt

Build receipts track pack provenance:

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildReceipt {
    pub epoch_id: String,
    pub toolchain_version: String,
    pub input_hash: String,
    pub output_files: Vec<OutputFile>,

    // NEW: Pack provenance
    pub packs: Vec<PackProvenance>,
    pub bundle_expansions: Vec<BundleExpansion>,
    pub profile: ProfileRef,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackProvenance {
    pub pack_id: AtomicPackId,
    pub version: String,
    pub signature: String,
    pub digest: Sha256,
    pub templates_contributed: Vec<String>,
    pub queries_contributed: Vec<String>,
    pub files_generated: Vec<OutputPath>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundleExpansion {
    pub bundle_id: BundleId,
    pub expanded_to: Vec<AtomicPackId>,
}
```

### Receipt Population

```rust
impl StagedPipeline {
    fn mu5_receipt(
        files: &GeneratedFiles,
        packs: &ResolvedPacks
    ) -> Result<BuildReceipt> {
        let mut pack_provenance = Vec::new();

        for pack in &packs.atomic_packs {
            let templates = packs.get_pack_templates(pack)?;
            let queries = packs.get_pack_queries(pack)?;
            let generated = files.filter_by_pack(pack)?;

            pack_provenance.push(PackProvenance {
                pack_id: pack.clone(),
                version: pack.version.clone(),
                signature: pack.signature.clone(),
                digest: pack.digest,
                templates_contributed: templates,
                queries_contributed: queries,
                files_generated: generated,
            });
        }

        Ok(BuildReceipt {
            epoch_id: Uuid::new_v4().to_string(),
            toolchain_version: env!("CARGO_PKG_VERSION").to_string(),
            input_hash: compute_input_hash()?,
            output_files: files.clone(),
            packs: pack_provenance,
            bundle_expansions: packs.bundle_expansions(),
            profile: packs.profile.clone(),
        })
    }
}
```

### Receipt Output

Example receipt (`.ggen/latest.json`):

```json
{
  "epoch_id": "2024-03-31T12:00:00Z",
  "toolchain_version": "6.0.1",
  "input_hash": "sha256:...",
  "output_files": [
    {
      "path": "src/mcp/server.rs",
      "hash": "sha256:..."
    },
    {
      "path": "src/mcp/tools.rs",
      "hash": "sha256:..."
    }
  ],
  "packs": [
    {
      "pack_id": "surface-mcp",
      "version": "1.2.3",
      "signature": "ed25519:...",
      "digest": "sha256:...",
      "templates_contributed": [
        "surface-mcp:src/mcp/server.rs",
        "surface-mcp:src/mcp/tools.rs",
        "surface-mcp:src/mcp/mod.rs"
      ],
      "queries_contributed": [
        "tools",
        "all_tools"
      ],
      "files_generated": [
        "src/mcp/server.rs",
        "src/mcp/tools.rs",
        "src/mcp/mod.rs"
      ]
    },
    {
      "pack_id": "projection-rust",
      "version": "2.1.0",
      "signature": "ed25519:...",
      "digest": "sha256:...",
      "templates_contributed": [
        "projection-rust:src/main.rs",
        "projection-rust:Cargo.toml"
      ],
      "queries_contributed": [],
      "files_generated": [
        "src/main.rs",
        "Cargo.toml"
      ]
    }
  ],
  "bundle_expansions": [
    {
      "bundle_id": "mcp-rust-axum",
      "expanded_to": [
        "surface-mcp",
        "projection-rust",
        "runtime-axum"
      ]
    }
  ],
  "profile": {
    "name": "regulated-finance",
    "policies": [
      "policy-no-defaults",
      "policy-strict",
      "receipt-enterprise-signed",
      "validator-shacl"
    ]
  }
}
```

## Integration with `ggen sync`

The full `ggen sync` flow with packs:

```bash
$ ggen sync

μ₀: Pack Resolution
  Reading lockfile: .ggen/packs.lock
  Expanding bundles: mcp-rust-axum
    - surface-mcp
    - projection-rust
    - runtime-axum
  Resolving dependencies: 6 foundation packs
  Checking compatibility: ✅
  Merging ontologies: 9 packs
  Building ownership map: ✅

μ₁: Normalization
  Normalizing merged ontology: ✅

μ₂: Extraction
  Executing queries: 12 queries
    - tools (surface-mcp)
    - all_tools (surface-mcp)
    - ... (10 more)

μ₃: Emission
  Rendering templates: 15 templates
    - surface-mcp:src/mcp/server.rs → src/mcp/server.rs
    - surface-mcp:src/mcp/tools.rs → src/mcp/tools.rs
    - projection-rust:src/main.rs → src/main.rs
    - ... (12 more)

μ₄: Canonicalization
  Canonicalizing files: 15 files

μ₅: Receipt
  Generating receipt: .ggen/latest.json
  Pack provenance: 9 packs
  Bundle expansions: 1 bundle
  Profile: regulated-finance

✅ Sync complete
  Files generated: 15
  Packs used: 9
  Receipt: .ggen/latest.json
```

## Key Files

| Component | File | Purpose |
|-----------|------|---------|
| **Pack resolver** | `crates/ggen-core/src/pack_resolver.rs` | μ₀ stage |
| **Lockfile** | `crates/ggen-core/src/lockfile.rs` | Lockfile handling |
| **Pipeline** | `crates/ggen-core/src/v6/pipeline.rs` | μ₁-μ₅ stages |
| **Receipt** | `crates/ggen-core/src/v6/receipt.rs` | Build receipts |
| **Template resolver** | `crates/ggen-core/src/resolver.rs` | Template registration |

## Further Reading

- [ARCHITECTURE.md](ARCHITECTURE.md) — System architecture
- [ATOMIC_PACKS.md](ATOMIC_PACKS.md) — Atomic pack reference
- [BUNDLES_AND_PROFILES.md](BUNDLES_AND_PROFILES.md) — Bundles and profiles
- [SECURITY_MODEL.md](SECURITY_MODEL.md) — Security and trust model
