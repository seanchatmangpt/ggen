# Phase 2 Integration Architecture Diagram

## High-Level Data Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         CUSTOMER INPUT LAYER                                 │
│                                                                               │
│  HIPAA Domain YAML                                                            │
│  ─────────────────────────────────────────────────────────────               │
│  entities:                                                                    │
│    hipaa_privacy_policy:                                                     │
│      name: "HIPAA Privacy Policy"                                            │
│      type: "Policy"                                                          │
│      attributes: {jurisdiction: "US"}                                        │
│                                                                               │
│    phi_classification:                                                       │
│      name: "Protected Health Information"                                    │
│      type: "Classification"                                                  │
│                                                                               │
│    encryption_control:                                                       │
│      name: "Data Encryption at Rest"                                        │
│      type: "Control"                                                         │
│                                                                               │
└────────────────────┬────────────────────────────────────────────────────────┘
                     │
                     ▼
     ┌───────────────────────────────────────┐
     │   STEP 1: DOMAIN PARSER               │
     │   ─────────────────────────────────   │
     │   Input: YAML string                  │
     │   Output: Vec<Entity>                 │
     │                                       │
     │   • Parse YAML structure              │
     │   • Extract entity properties         │
     │   • Validate required fields          │
     │   • Normalize entity naming           │
     │   • Sort deterministically            │
     └───────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────┐
│                    PARSED ENTITIES                           │
│                                                              │
│  Entity {                                                    │
│    id: "hipaa_privacy_policy",                             │
│    name: "HIPAA Privacy Policy",                           │
│    entity_type: Policy,                                    │
│    attributes: {jurisdiction: "US"},                       │
│    tags: ["privacy", "compliance", "healthcare"],          │
│    relationships: [{target_id: "phi_classification", ...}] │
│  },                                                         │
│                                                              │
│  Entity {                                                    │
│    id: "phi_classification",                               │
│    name: "Protected Health Information",                   │
│    entity_type: Classification,                            │
│    ...                                                      │
│  },                                                         │
│                                                              │
│  Entity {                                                    │
│    id: "encryption_control",                               │
│    name: "Data Encryption at Rest",                        │
│    entity_type: Control,                                   │
│    ...                                                      │
│  }                                                          │
└──────────────────────────────────────────────────────────────┘
                     │
                     ▼
     ┌───────────────────────────────────────┐
     │   STEP 2: ENTITY MAPPER               │
     │   ─────────────────────────────────   │
     │   Input: Vec<Entity>                  │
     │   Output: Vec<OntologyMapping>        │
     │                                       │
     │   • Route to matcher by entity_type   │
     │   • Call EntityMapper methods:        │
     │     - match_policy(...)               │
     │     - match_data_classification(...)  │
     │     - match_security_control(...)     │
     │     - match_compute_service(...)      │
     │     - match_service_level(...)        │
     │   • Extract primary match (top score) │
     │   • Calculate confidence score        │
     │   • Generate mapping rationale        │
     │   • Sort deterministically            │
     └───────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│                        ONTOLOGY MAPPINGS                                      │
│                                                                               │
│  OntologyMapping {                                                            │
│    entity_id: "hipaa_privacy_policy",                                        │
│    entity_name: "HIPAA Privacy Policy",                                      │
│    ontology_matches: [                                                       │
│      { class: ":PrivacyPolicy", label: "Privacy Policy", score: 0.95 },     │
│      { class: ":DataProtectionPolicy", label: "...", score: 0.85 },         │
│      ...                                                                     │
│    ],                                                                        │
│    primary_match: { class: ":PrivacyPolicy", score: 0.95 },                │
│    confidence: 0.95,                                                        │
│    rationale: "Mapped via policy matching method..."                        │
│  },                                                                          │
│                                                                               │
│  OntologyMapping {                                                            │
│    entity_id: "phi_classification",                                          │
│    entity_name: "Protected Health Information",                              │
│    primary_match: { class: ":RestrictedData", score: 0.95 },               │
│    confidence: 0.95,                                                        │
│    ...                                                                      │
│  },                                                                          │
│                                                                               │
│  OntologyMapping {                                                            │
│    entity_id: "encryption_control",                                          │
│    entity_name: "Data Encryption at Rest",                                  │
│    primary_match: { class: ":EncryptionControl", score: 0.96 },            │
│    confidence: 0.96,                                                        │
│    ...                                                                      │
│  }                                                                           │
│                                                                               │
│  Aggregate Confidence: (0.95 + 0.95 + 0.96) / 3 = 0.953                    │
└──────────────────────────────────────────────────────────────────────────────┘
                     │
                     ▼
     ┌───────────────────────────────────────┐
     │   STEP 3: SPARQL QUERY GEN            │
     │   ─────────────────────────────────   │
     │   Input: Vec<OntologyMapping>         │
     │   Output: Vec<SparqlQuery>            │
     │                                       │
     │   For each mapping:                   │
     │   1. Generate enumeration query       │
     │   2. Generate domain-specific query   │
     │      (compliance if policy)           │
     │      (verification if control)        │
     │   3. Assign deterministic IDs         │
     │   4. Sort by query ID                 │
     └───────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│                         SPARQL QUERIES                                        │
│                                                                               │
│  SparqlQuery {                                                                │
│    id: "q_000_enum_hipaa_privacy_policy",                                   │
│    query: "SELECT ?instance ?label WHERE { ?instance rdf:type :PrivacyPolicy │
│            OPTIONAL { ?instance rdfs:label ?label . } }",                    │
│    purpose: "Enumerate all instances of Privacy Policy",                    │
│    applicable_entities: ["hipaa_privacy_policy"],                           │
│    expected_result_type: "instance_list"                                    │
│  },                                                                          │
│                                                                               │
│  SparqlQuery {                                                                │
│    id: "q_001_compliance_hipaa_privacy_policy",                             │
│    query: "SELECT ?policy ?jurisdiction ?hasControls WHERE { ?policy rdf: │
│            type :PrivacyPolicy . ?policy :policyId \"hipaa_privacy_policy\" │
│            OPTIONAL { ?policy :hasJurisdiction ?jurisdiction . } }",        │
│    purpose: "Check compliance status for policy HIPAA Privacy Policy",      │
│    applicable_entities: ["hipaa_privacy_policy"],                           │
│    expected_result_type: "compliance_status"                                │
│  },                                                                          │
│                                                                               │
│  SparqlQuery {                                                                │
│    id: "q_002_enum_phi_classification",                                     │
│    query: "SELECT ?instance ?label WHERE { ?instance rdf:type :RestrictedData │
│            OPTIONAL { ?instance rdfs:label ?label . } }",                    │
│    purpose: "Enumerate all instances of Restricted Data",                   │
│    applicable_entities: ["phi_classification"],                             │
│    expected_result_type: "instance_list"                                    │
│  },                                                                          │
│                                                                               │
│  SparqlQuery {                                                                │
│    id: "q_003_verify_encryption_control",                                   │
│    query: "SELECT ?control ?status ?lastVerified WHERE { ?control rdf:type │
│            :EncryptionControl . ?control :controlId \"encryption_control\"  │
│            OPTIONAL { ?control :verificationStatus ?status . } }",          │
│    purpose: "Verify control Data Encryption at Rest",                       │
│    applicable_entities: ["encryption_control"],                             │
│    expected_result_type: "control_status"                                   │
│  }                                                                           │
│                                                                               │
│  ... (more queries for other entity types)                                   │
└──────────────────────────────────────────────────────────────────────────────┘
                     │
                     ▼
     ┌───────────────────────────────────────┐
     │   STEP 4: AGGREGATION                 │
     │   ─────────────────────────────────   │
     │   Combine all results into            │
     │   MappingResult struct                │
     │                                       │
     │   • Collect parsed_entities           │
     │   • Collect mappings                  │
     │   • Collect queries                   │
     │   • Calculate overall_confidence      │
     │   • Add timestamp                     │
     │   • Create provider_mappings (Phase 3)│
     └───────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│                      MAPPING RESULT (JSON OUTPUT)                            │
│                                                                               │
│  {                                                                            │
│    "domain_id": "healthcare-finops-hipaa",                                   │
│    "parsed_entities": [...6 entities...],                                    │
│    "mappings": [...6 mappings...],                                           │
│    "queries": [...12 queries...],                                            │
│    "overall_confidence": 0.953,                                              │
│    "processed_at": "2024-01-19T10:30:45Z",                                   │
│    "provider_mappings": {                                                    │
│      "aws_operations": [                                                     │
│        "aws_hipaa_privacy_policy",                                           │
│        "aws_phi_classification",                                             │
│        "aws_encryption_control",                                             │
│        ...                                                                   │
│      ],                                                                      │
│      "gcp_operations": [...],                                                │
│      "azure_operations": [...]                                               │
│    }                                                                         │
│  }                                                                           │
└──────────────────────────────────────────────────────────────────────────────┘
                     │
                     ▼
     ┌───────────────────────────────────────┐
     │   STEP 5: CLI OUTPUT                  │
     │   ─────────────────────────────────   │
     │   Output: File or stdout              │
     │                                       │
     │   $ ggen ontology map \               │
     │       --input domain-hipaa.yaml \     │
     │       --format json \                 │
     │       --output hipaa-mapping.json     │
     │                                       │
     │   → hipaa-mapping.json written        │
     └───────────────────────────────────────┘
                     │
                     ▼
┌──────────────────────────────────────────────────────────────────────────────┐
│                    PHASE 3 INPUT (Provider Fan-Out)                          │
│                                                                               │
│  Phase 3 receives MappingResult containing:                                  │
│  • parsed_entities: Validated customer domain entities                       │
│  • mappings: Ontology-mapped entities with confidence scores                 │
│  • queries: SPARQL queries for compliance verification                       │
│  • provider_mappings: Routing information for AWS/GCP/Azure                  │
│                                                                               │
│  Phase 3 then:                                                               │
│  1. Implements AWS/GCP/Azure ProviderMapper                                  │
│  2. Executes SPARQL queries against customer RDF store                       │
│  3. Maps results to provider-specific operations                             │
│  4. Generates provider-specific infrastructure proposals                      │
│  5. Creates compliance receipts with cryptographic proof                     │
└──────────────────────────────────────────────────────────────────────────────┘
```

---

## Component Interaction Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         GGEN PHASE 2 ARCHITECTURE                            │
└─────────────────────────────────────────────────────────────────────────────┘

                    ┌──────────────────┐
                    │  Customer YAML   │
                    │  Domain Config   │
                    └────────┬─────────┘
                             │
                             ▼
        ┌────────────────────────────────────────┐
        │   ggen-ontology-core                    │
        │   crate/ggen-ontology-core/src/         │
        │                                         │
        │  ┌──────────────────────────────────┐  │
        │  │   integration.rs                  │  │
        │  │ ─────────────────────────────   │  │
        │  │ • EnterpriseDomainMapper struct  │  │
        │  │ • parse_domain_description()     │  │
        │  │ • map_entities_to_ontology()     │  │
        │  │ • generate_queries_for_mappings()│  │
        │  │ • execute_mapping_pipeline()     │  │
        │  └────────────┬─────────────────────┘  │
        │               │                         │
        │    ┌──────────┴──────────┐              │
        │    │                     │              │
        │    ▼                     ▼              │
        │  ┌──────────────────┐  ┌──────────────────┐
        │  │ entity_mapper.rs │  │sparql_generator.rs│
        │  │ ──────────────── │  │──────────────────│
        │  │ • match_policy() │  │ • find_policies()│
        │  │ • match_*()      │  │ • generate_*()   │
        │  │ (5 methods)      │  │ (6 methods)      │
        │  └────────┬─────────┘  └─────────┬────────┘
        │           │                      │
        │           └──────────┬───────────┘
        │                      │
        │    ┌─────────────────┼─────────────────┐
        │    │                 │                 │
        │    ▼                 ▼                 ▼
        │  ┌──────────┐  ┌──────────┐  ┌──────────────────┐
        │  │triple_   │  │validators│  │provider_mapper.rs│
        │  │store.rs  │  │.rs       │  │(Phase 3 prep)    │
        │  └──────────┘  └──────────┘  └──────────────────┘
        │
        │  ┌──────────────────────────────────┐
        │  │ receipt_generator.rs              │
        │  │ (Phase 3 prep)                    │
        │  │ • ComplianceReceipt struct        │
        │  │ • generate() & verify()           │
        │  └──────────────────────────────────┘
        │
        └──────────────┬───────────────────────┘
                       │
                       ▼
        ┌────────────────────────────────────────┐
        │   ggen-cli                              │
        │   crates/ggen-cli/src/cmds/             │
        │                                         │
        │  ┌──────────────────────────────────┐  │
        │  │   ontology.rs                     │  │
        │  │ ─────────────────────────────   │  │
        │  │ • OntologyCommand enum            │  │
        │  │   - Map subcommand                │  │
        │  │   - Validate subcommand           │  │
        │  │                                   │  │
        │  │ • execute() method                │  │
        │  │   - Load domain YAML              │  │
        │  │   - Call integration pipeline     │  │
        │  │   - Output JSON/YAML              │  │
        │  └──────────────────────────────────┘  │
        │                                         │
        └──────────────┬──────────────────────────┘
                       │
                       ▼
              ┌─────────────────┐
              │  JSON/YAML Out  │
              │  (MappingResult)│
              └────────┬────────┘
                       │
                       ▼
              ┌─────────────────┐
              │   Phase 3 Input │
              │ (Provider Fan-  │
              │   Out)          │
              └─────────────────┘
```

---

## Determinism Flow

```
                    Same Domain YAML
                           │
                           ▼
        ┌───────────────────────────────────┐
        │  parse_domain_description()        │
        │ ────────────────────────────────  │
        │ • Deterministic YAML parsing       │
        │ • Alphabetically sorted keys       │
        │ • Same parse order always          │
        │ • Consistent entity sorting        │
        └─────────────┬───────────────────────┘
                      │
        ┌─────────────▼────────────────────┐
        │ map_entities_to_ontology()        │
        │ ────────────────────────────────  │
        │ • Deterministic entity_type match  │
        │ • EntityMapper methods deterministic
        │ • Confidence score always same    │
        │ • Sorted by entity_id             │
        └─────────────┬───────────────────────┘
                      │
        ┌─────────────▼────────────────────┐
        │ generate_queries_for_mappings()   │
        │ ────────────────────────────────  │
        │ • Query ID generation consistent  │
        │ • SPARQL string building same     │
        │ • Sorted by query ID              │
        └─────────────┬───────────────────────┘
                      │
                      ▼
         Identical MappingResult
        (byte-for-byte same JSON)
```

---

## Error Handling Path

```
                    Domain YAML Input
                           │
                           ▼
        ┌───────────────────────────────────┐
        │ Invalid YAML?                      │
        │ (malformed syntax)                 │
        └────────┬──────────────┬────────────┘
                 │              │
                 YES            NO
                 │              │
                 ▼              ▼
         ┌──────────────┐   Next check
         │ ParseError   │
         │ with line #  │
         └──────────────┘

                    Valid YAML
                           │
                           ▼
        ┌───────────────────────────────────┐
        │ Required fields present?           │
        │ (name, type for entities)          │
        └────────┬──────────────┬────────────┘
                 │              │
                 NO             YES
                 │              │
                 ▼              ▼
        ┌──────────────────┐   Parsing success
        │ ValidationError  │   Continue to mapping
        │ {missing fields} │
        └──────────────────┘

            During Entity Mapping
                   │
                   ▼
        ┌─────────────────────────────────┐
        │ No ontology matches found?       │
        │ (entity has no matching class)   │
        └────┬──────────────┬──────────────┘
             │              │
             YES            NO
             │              │
             ▼              ▼
        ┌─────────────┐   Mapping success
        │ MapperError │   Continue to query gen
        │ {entity_id} │
        └─────────────┘
```

---

## Performance Optimization Points

```
                    Phase 2 Pipeline
                           │
        ┌──────────────────┼──────────────────┐
        │                  │                  │
        ▼                  ▼                  ▼
    ┌─────────┐     ┌─────────────┐     ┌──────────┐
    │ Parsing │     │   Mapping   │     │ Queries  │
    │ <500ms  │     │   <1s       │     │ <1s      │
    └────┬────┘     └──────┬──────┘     └────┬─────┘
         │                 │                 │
    Optimization:      Optimization:     Optimization:
    • YAML cache      • Match cache    • Query template
    • String pool     • Score reuse    • ID generation
    • Sort once       • Batch matches  • Sorted output

        │                 │                 │
        └──────────────────┼──────────────────┘
                           │
                    Total Pipeline: <3s
                           │
                           ▼
                   MappingResult JSON
                  (ready for Phase 3)
```

---

## Data Structure Relationships

```
Entity (from YAML)
  ├─ id: String
  ├─ name: String
  ├─ entity_type: EntityType (Policy|Control|Service|...)
  ├─ attributes: HashMap<String, String>
  ├─ tags: Vec<String>
  └─ relationships: Vec<EntityRelationship>
       └─ entity_id: String
       └─ relationship_type: String

                    ↓ (map_entities_to_ontology)

OntologyMapping
  ├─ entity_id: String (FK to Entity.id)
  ├─ entity_name: String
  ├─ ontology_matches: Vec<OntologyMatch>
  │  ├─ class: String (:PrivacyPolicy, :EncryptionControl, ...)
  │  ├─ label: String
  │  ├─ score: f32 (0.0-1.0)
  │  └─ reason: String
  ├─ primary_match: OntologyMatch (highest score)
  ├─ confidence: f32
  ├─ rationale: String
  └─ metadata: HashMap<String, String>

                    ↓ (generate_queries_for_mappings)

SparqlQuery
  ├─ id: String (deterministic: "q_{idx}_{type}_{entity_id}")
  ├─ query: String (SPARQL SELECT statement)
  ├─ purpose: String
  ├─ applicable_entities: Vec<String> (FK to Entity.id)
  └─ expected_result_type: String

                    ↓ (execute_mapping_pipeline)

MappingResult
  ├─ domain_id: String
  ├─ parsed_entities: Vec<Entity>
  ├─ mappings: Vec<OntologyMapping>
  ├─ queries: Vec<SparqlQuery>
  ├─ overall_confidence: f32 (average of mapping confidences)
  ├─ processed_at: String (ISO 8601 timestamp)
  └─ provider_mappings: ProviderMappings
     ├─ aws_operations: Vec<String>
     ├─ gcp_operations: Vec<String>
     └─ azure_operations: Vec<String> (Phase 3 targets)
```

