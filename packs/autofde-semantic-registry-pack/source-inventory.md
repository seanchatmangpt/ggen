# The right framing

A Fortune-5 AutoFDE deployment does **not** want one giant “cloud ontology.”

It wants a **federated semantic registry** combining:

* **Ontologies** — OWL/RDF vocabularies with explicit semantics.
* **Authoritative schemas** — provider resource and API type systems.
* **Knowledge bases and control catalogs** — security, compliance, risk, and operations.
* **Protocols and event models** — the contracts through which the cloud changes.

The provider catalogs are too large and change too frequently to model by hand. AutoFDE should generate provider-specific ontology projections from the official AWS, Azure, Google, OCI, IBM, Alibaba, Kubernetes, and Terraform schemas. AWS publishes machine-readable CloudFormation resource specifications, Azure exposes provider namespaces and resource types, Google Cloud Asset Inventory publishes resource, policy, and relationship types, and Terraform exposes provider schemas as JSON. ([AWS Documentation][1])

## Legend

| Mark  | Meaning                                      |
| ----- | -------------------------------------------- |
| **O** | Actual ontology or semantic vocabulary       |
| **S** | Machine-readable schema or information model |
| **K** | Knowledge base, taxonomy, or control catalog |
| **P** | Protocol or executable contract              |

---

# 1. Cloud architecture and resource foundation

These define **what a cloud is**, its actors, resources, services, topology, and deployment relationships.

| Public source                                                | Type | AutoFDE use                                                                |
| ------------------------------------------------------------ | ---: | -------------------------------------------------------------------------- |
| **NIST Cloud Computing Definition, SP 800-145**              |    K | Service models, deployment models, cloud characteristics                   |
| **NIST Cloud Computing Reference Architecture, SP 500-292**  |    K | Consumer, provider, broker, auditor, carrier                               |
| **NIST Cloud Federation Reference Architecture, SP 500-332** |    K | Multicloud trust, identity, resource sharing                               |
| **NIST Cloud Forensic Reference Architecture, SP 800-201**   |  K/S | Forensic readiness, evidence sources, incident investigation               |
| **OASIS TOSCA 2.0**                                          |    S | Portable application topology, node types, relationships and orchestration |
| **DMTF Common Information Model — CIM**                      |  O/S | Compute, network, storage, systems management                              |
| **DMTF Redfish Schema**                                      |    S | Physical and virtual infrastructure management                             |
| **DMTF Open Virtualization Format — OVF**                    |    S | Portable virtual workloads                                                 |
| **OASIS CAMP**                                               |    S | Cloud application platform management                                      |
| **Open Grid Forum OCCI**                                     |  O/S | Vendor-neutral cloud resource management                                   |

NIST provides the neutral taxonomy and actor model; TOSCA 2.0 is the current OASIS topology and orchestration standard. ([NIST][2])

---

# 2. Provider resource ontologies

These are **not generally OWL ontologies**. They are the authoritative provider type systems from which AutoFDE should generate semantic projections.

## Tier 1 providers

| Provider                        | Canonical public source                                                                             |
| ------------------------------- | --------------------------------------------------------------------------------------------------- |
| **AWS**                         | CloudFormation resource specification, resource-provider schemas, AWS APIs, ARN grammar             |
| **Microsoft Azure**             | ARM resource providers and types, ARM/Bicep schemas, REST API specifications, Azure Resource Graph  |
| **Google Cloud**                | Cloud Asset Inventory asset types, resource names, relationships, IAM and organization-policy types |
| **Oracle Cloud Infrastructure** | OCI Resource Search types, searchable attributes, API models, OCID grammar                          |

AWS provides strongly typed, machine-readable specifications for supported resources and properties. Azure resource types follow the `{provider}/{resource-type}` model. Google CAI covers resources, policies, runtime information, and relationships. OCI exposes searchable resource types and their attributes. ([AWS Documentation][1])

## Tier 2 providers

| Provider          | Canonical public source                                                               |
| ----------------- | ------------------------------------------------------------------------------------- |
| **IBM Cloud**     | Global Catalog, Resource Controller, CRN grammar, IAM service definitions             |
| **Alibaba Cloud** | Resource Directory, Resource Groups, Resource Center resource types and relationships |
| **Tencent Cloud** | Cloud APIs, resource tag and account schemas                                          |
| **Huawei Cloud**  | Resource Management and Governance schemas                                            |
| **OVHcloud**      | Public API resource schemas                                                           |
| **SAP BTP**       | Entitlements, subaccounts, services, CloudEvents and service-manager models           |
| **Salesforce**    | Metadata API, object metadata, event and permission schemas                           |
| **ServiceNow**    | Common Service Data Model and CMDB classes                                            |
| **Snowflake**     | Account, object, role, warehouse and governance models                                |
| **Databricks**    | Unity Catalog, workspace, account and compute schemas                                 |

IBM uses globally structured CRNs and Resource Controller models. Alibaba Resource Center provides enterprise-wide resource views across accounts, products, regions, types, tags, configurations, and relationships. ([IBM Cloud][3])

## Cross-provider generated sources

* **Terraform provider schemas**
* **Pulumi provider schemas**
* **Crossplane Composite Resource Definitions**
* **Kubernetes OpenAPI and CRDs**
* **Open Application Model**
* **Helm values schemas**
* **Cloud Custodian resource registries**
* **Steampipe provider schemas**

These should be treated as **derived implementation vocabularies**, not semantic authority. Terraform exposes provider, resource, and data-source schemas through a machine-readable command. ([HashiCorp Developer][4])

---

# 3. Semantic-web foundation

These are the common ontologies every AutoFDE graph should build upon.

| Ontology                               | Purpose                                                      |
| -------------------------------------- | ------------------------------------------------------------ |
| **RDF / RDFS / OWL 2**                 | Graph and ontology semantics                                 |
| **SHACL**                              | Executable graph constraints                                 |
| **SKOS**                               | Taxonomies and controlled vocabularies                       |
| **Dublin Core Terms**                  | Common metadata                                              |
| **PROV-O**                             | Entities, activities, agents, derivation and provenance      |
| **DCAT 3**                             | Datasets, data services, catalogs and versions               |
| **DQV**                                | Data-quality measurements and annotations                    |
| **ODRL 2.2**                           | Permissions, prohibitions, duties and constraints            |
| **W3C ORG**                            | Organizations, units, roles, membership and reporting        |
| **Registered Organization Vocabulary** | Legal entities and registrations                             |
| **OWL-Time**                           | Instants, intervals, durations and temporal ordering         |
| **GeoSPARQL**                          | Places, geometries and spatial relations                     |
| **SOSA/SSN**                           | Observations, sensors, actuators, procedures and deployments |
| **QUDT**                               | Quantities, units, dimensions and datatypes                  |
| **FOAF**                               | People and agents                                            |
| **vCard RDF**                          | Contact and address information                              |
| **schema.org**                         | Broad public entities, services, products and actions        |

PROV-O is the core public provenance ontology; DCAT 3 is the current W3C catalog vocabulary; ODRL represents permission, prohibition, duty, and constraint semantics; SOSA/SSN explicitly models both observation and actuation. ([W3C][5])

---

# 4. Identity, authority and entitlement

A Fortune-5 AutoFDE must understand not merely **who an identity is**, but **which consequence it may authorize**.

| Standard/model                        | Type | Purpose                                                |
| ------------------------------------- | ---: | ------------------------------------------------------ |
| **SCIM 2.0**                          |  P/S | Users, groups and identity lifecycle                   |
| **OAuth 2.x**                         |    P | Delegated authorization                                |
| **OpenID Connect**                    |    P | Authentication and identity claims                     |
| **SAML 2.0**                          |    P | Enterprise federation                                  |
| **WebAuthn / FIDO2**                  |    P | Strong authentication                                  |
| **W3C Verifiable Credentials**        |  O/P | Cryptographically verifiable claims                    |
| **W3C DID**                           |  O/P | Decentralized identity identifiers                     |
| **XACML 3.0**                         |  S/P | Attribute-based access-control policies                |
| **ODRL**                              |    O | Permissions, prohibitions, obligations and constraints |
| **NGAC**                              |  S/K | Graph-based access-control relations                   |
| **NIST RBAC model**                   |    K | Roles and permission assignment                        |
| **AWS IAM policy grammar**            |    S | AWS permissions                                        |
| **Azure RBAC and Entra role schemas** |    S | Azure permissions and administrative roles             |
| **Google IAM policy model**           |    S | GCP bindings and conditions                            |
| **SPIFFE/SPIRE**                      |  P/S | Workload identity                                      |
| **Kubernetes RBAC**                   |    S | Cluster-scoped authority                               |
| **Open Policy Agent/Rego model**      |  P/S | Executable policy decisions                            |

AutoFDE should extend these with its own precise concepts:

```text
AuthorityGrant
DecisionRight
PermittedConsequence
AuthorizedScope
ApprovalPopulation
ValidityInterval
Revocation
Delegation
SeparationOfDuties
```

---

# 5. Security, threat and defensive-action semantics

| Public model                  | Type | AutoFDE role                                                  |
| ----------------------------- | ---: | ------------------------------------------------------------- |
| **NIST OSCAL**                |    S | Controls, profiles, implementations, assessments and findings |
| **NIST CSF 2.0**              |    K | Cybersecurity outcomes                                        |
| **NIST SP 800-53**            |  K/S | Security and privacy controls                                 |
| **CSA Cloud Controls Matrix** |    K | Cloud-specific control framework                              |
| **CSA CAIQ**                  |    K | Cloud-provider assurance questions                            |
| **STIX 2.1**                  |    S | Threat intelligence and cyber observables                     |
| **TAXII 2.x**                 |    P | Threat-intelligence exchange                                  |
| **MITRE ATT&CK**              |    K | Adversary tactics and techniques                              |
| **MITRE D3FEND**              |  O/K | Defensive techniques and technical artifacts                  |
| **MITRE CAPEC**               |    K | Attack patterns                                               |
| **CWE**                       |    K | Software weaknesses                                           |
| **CVE**                       |    K | Vulnerability identities                                      |
| **CVSS**                      |    S | Vulnerability severity                                        |
| **CPE**                       |    S | Product identities                                            |
| **CACAO**                     |    S | Cybersecurity playbooks                                       |
| **OpenC2**                    |    P | Command-and-control actions for defense                       |
| **VERIS**                     |    S | Security-incident classification                              |
| **FIRST EPSS**                |    S | Exploitation probability                                      |
| **Sigma**                     |    S | Detection rules                                               |
| **YARA**                      |    S | Malware matching rules                                        |
| **OSSEM**                     |  O/S | Security-event semantics                                      |

OSCAL provides machine-readable controls, system implementations, assessment plans, and results. STIX provides the cyber-threat language; ATT&CK provides the offensive knowledge base; D3FEND is an actual RDF/OWL defense ontology with downloadable TTL, OWL, and JSON-LD distributions. ([NIST Pages][6])

For Azure Breach Clock, this stack becomes:

```text
STIX observation
→ ATT&CK hypothesis
→ D3FEND countermeasure candidate
→ OSCAL control relationship
→ AutoFDE authority check
→ POWL response commitment
→ OCEL occurrence evidence
```

---

# 6. Events, observations and telemetry

| Standard                                | Type | Purpose                                            |
| --------------------------------------- | ---: | -------------------------------------------------- |
| **CloudEvents**                         |  P/S | Portable event envelopes                           |
| **OpenTelemetry Semantic Conventions**  |    S | Common traces, metrics, logs, resources and events |
| **OpenMetrics**                         |    S | Metric exposition                                  |
| **W3C SOSA/SSN**                        |    O | Observations, actuations and procedures            |
| **OCEL 2.0**                            |    S | Object-centric execution history                   |
| **XES**                                 |    S | Process event logs                                 |
| **OpenLineage**                         |  S/P | Data-job and dataset lineage events                |
| **OpenSLO**                             |    S | Service-level objectives                           |
| **OpenFeature**                         |  P/S | Feature-state changes                              |
| **OpenTracing baggage mappings**        |    S | Distributed causal context                         |
| **ECS — Elastic Common Schema**         |    S | Security and operational events                    |
| **Open Cybersecurity Schema Framework** |    S | Normalized cybersecurity events                    |
| **OpenTelemetry GenAI conventions**     |    S | AI model, agent and tool telemetry                 |

CloudEvents standardizes event context across services and platforms. OpenTelemetry semantic conventions establish common meanings for traces, metrics, logs, cloud resources, databases, messaging, CI/CD, and other operational surfaces. ([GitHub][7])

---

# 7. Cost, billing and technology-value semantics

| Public model                     | Type | Purpose                                        |
| -------------------------------- | ---: | ---------------------------------------------- |
| **FOCUS 1.4**                    |    S | Multicloud, SaaS, AI and data-platform billing |
| **FinOps Framework**             |    K | Cost-management capabilities and personas      |
| **OpenCost**                     |    S | Kubernetes and cloud-native allocation         |
| **Cloud Carbon Footprint model** |    S | Estimated cloud emissions                      |
| **QUDT**                         |    O | Currency, quantities, rates and units          |
| **GoodRelations**                |    O | Products, prices and commercial offers         |
| **schema.org financial terms**   |    O | Commercial metadata                            |
| **ISO 4217 currency vocabulary** |    K | Currency identifiers                           |

FOCUS defines a vendor-neutral billing schema and currently publishes data generators for AWS, Azure, Google Cloud, Oracle, Alibaba, Tencent, Huawei, OVHcloud, Databricks, and other technology providers. ([Focus FinOps][8])

AutoFDE needs to relate:

```text
Resource
→ Usage
→ Charge
→ Invoice
→ Owner
→ BusinessCapability
→ CustomerOutcome
```

This is how it reasons about **business value**, not merely cloud spend.

---

# 8. Data governance, lineage and privacy

| Public ontology/model                    | Type | Purpose                                                    |
| ---------------------------------------- | ---: | ---------------------------------------------------------- |
| **DCAT 3**                               |    O | Data and service catalogs                                  |
| **PROV-O**                               |    O | Lineage and derivation                                     |
| **OpenLineage**                          |    S | Runtime data-job lineage                                   |
| **W3C Data Privacy Vocabulary — DPV**    |    O | Personal data, processing, purposes, legal bases and risks |
| **DPV-GDPR / Legal / Risk / Technology** |    O | Regulatory and technology extensions                       |
| **ODRL**                                 |    O | Data-use policies                                          |
| **Data Quality Vocabulary — DQV**        |    O | Quality metrics and annotations                            |
| **R2RML / RML**                          |    S | Relational and heterogeneous data-to-RDF mapping           |
| **CSVW**                                 |  S/O | Tabular metadata                                           |
| **Data Cube Vocabulary**                 |    O | Multidimensional observations                              |
| **VoID**                                 |    O | Linked-data dataset descriptions                           |
| **FAIR Digital Object models**           |  O/S | Research and regulated-data objects                        |
| **Data Contract Specification**          |    S | Producer-consumer data contracts                           |
| **Apache Atlas model**                   |  S/O | Data assets, classifications and lineage                   |
| **Egeria Open Metadata types**           |  O/S | Enterprise metadata federation                             |

DPV is explicitly designed as a vocabulary and ontology for personal-data processing, roles, purposes, legal justifications, technologies, risks, and controls. OpenLineage supplies an extensible operational lineage-event model. ([W3C][9])

---

# 9. APIs, integration and messaging

| Standard                         | Type | Purpose                        |
| -------------------------------- | ---: | ------------------------------ |
| **OpenAPI 3.2**                  |    S | HTTP API capabilities          |
| **AsyncAPI**                     |    S | Event-driven APIs              |
| **JSON Schema**                  |    S | JSON constraints               |
| **JSON-LD**                      |  O/S | Linked-data JSON               |
| **GraphQL introspection schema** |    S | Graph API capabilities         |
| **Protocol Buffers descriptors** |    S | Typed RPC and event messages   |
| **Apache Avro schemas**          |    S | Event and data serialization   |
| **gRPC reflection**              |  P/S | RPC service discovery          |
| **CloudEvents**                  |  P/S | Event portability              |
| **AMQP**                         |    P | Enterprise messaging           |
| **MQTT**                         |    P | IoT messaging                  |
| **OData CSDL**                   |    S | Enterprise data APIs           |
| **OASIS OSLC**                   |  O/P | Lifecycle-resource integration |
| **TM Forum Open APIs**           |    S | Telecom service operations     |
| **Open Service Broker API**      |    P | Service provisioning           |
| **Backstage Catalog model**      |    S | Software and service ownership |

OpenAPI allows machines and humans to discover service capabilities without source access or traffic inspection. ([OpenAPI Initiative Publications][10])

---

# 10. Software, AI and supply-chain semantics

| Standard/model                   | Type | Purpose                                                      |
| -------------------------------- | ---: | ------------------------------------------------------------ |
| **SPDX 3.x**                     |  O/S | Software, packages, vulnerabilities, builds, datasets and AI |
| **CycloneDX**                    |    S | SBOM, SaaSBOM, ML-BOM, CBOM and VEX                          |
| **SLSA**                         |  K/S | Build integrity and provenance levels                        |
| **in-toto**                      |  S/P | Supply-chain attestations                                    |
| **Sigstore bundle model**        |    S | Signing and transparency evidence                            |
| **VEX**                          |    S | Vulnerability exploitability status                          |
| **PURL**                         |    S | Package identities                                           |
| **CPE**                          |    S | Product identities                                           |
| **SWID**                         |    S | Software identities                                          |
| **GUAC graph model**             |  K/S | Software supply-chain knowledge graph                        |
| **OpenSSF Scorecard**            |  K/S | Project-security signals                                     |
| **Model Cards**                  |    S | Model limitations and evaluation                             |
| **Datasheets for Datasets**      |    S | Dataset documentation                                        |
| **NIST AI RMF**                  |    K | AI governance and risk                                       |
| **SPDX AI and Dataset profiles** |  O/S | AI model, dataset and package provenance                     |
| **DPV AI extensions**            |    O | Privacy and AI governance concepts                           |

SPDX 3 is particularly attractive because the current model is available in SHACL, while prior versions also publish RDF and OWL representations; SPDX now covers software, build, security, data, and AI use cases. ([SPDX][11])

---

# 11. Organization, business and legal semantics

| Ontology/model                            | Purpose                                                                    |
| ----------------------------------------- | -------------------------------------------------------------------------- |
| **W3C ORG**                               | Business units, roles, memberships, reporting and sites                    |
| **Registered Organization Vocabulary**    | Legal-entity identity                                                      |
| **FIBO**                                  | Financial contracts, instruments, legal entities, markets and transactions |
| **GoodRelations**                         | Products, services, prices and offers                                      |
| **schema.org**                            | Customers, suppliers, services, products and actions                       |
| **LegalRuleML**                           | Machine-readable legal norms and rules                                     |
| **Akoma Ntoso**                           | Legislation and legal documents                                            |
| **ODRL**                                  | Contractual permissions, duties and prohibitions                           |
| **Business Process Model and Notation**   | Business-process structure                                                 |
| **Decision Model and Notation**           | Business decisions                                                         |
| **CMMN**                                  | Case management                                                            |
| **BMM**                                   | Business motivation                                                        |
| **ArchiMate exchange model**              | Enterprise-architecture relationships                                      |
| **TOGAF content metamodel**               | Architecture artifacts and relationships                                   |
| **APQC Process Classification Framework** | Enterprise process taxonomy                                                |
| **ValueFlows**                            | Economic events, commitments and resources                                 |
| **REA ontology**                          | Resources, events and agents                                               |

FIBO is a public OWL ontology standardized through the EDM Council and OMG and intended for unambiguous financial-business semantics. ([EDM Council][12])

---

# 12. Physical infrastructure, IoT and digital twins

| Ontology/model                          | Purpose                                          |
| --------------------------------------- | ------------------------------------------------ |
| **SOSA/SSN**                            | Sensors, observations, actuators and deployments |
| **W3C Web of Things Thing Description** | Device capabilities and interactions             |
| **ETSI SAREF**                          | Smart appliances, energy and IoT                 |
| **Brick Schema**                        | Buildings, equipment and telemetry               |
| **Project Haystack**                    | Building-system tags                             |
| **OPC UA information models**           | Industrial equipment and operations              |
| **Asset Administration Shell**          | Industrial digital twins                         |
| **ISA-95 / B2MML**                      | Enterprise-control system integration            |
| **IEC Common Information Model**        | Electric-grid systems                            |
| **NGSI-LD**                             | Context information and digital twins            |
| **Digital Twin Definition Language**    | Twin entities and relationships                  |
| **QUDT**                                | Units and measurable values                      |
| **GeoSPARQL**                           | Spatial relationships                            |
| **OWL-Time**                            | Temporal relationships                           |

---

# 13. Sustainability and environmental semantics

A top-five enterprise will need:

* **Green Software Foundation Software Carbon Intensity**
* **Greenhouse Gas Protocol concepts**
* **QUDT units**
* **FOCUS cost and usage**
* **Cloud Carbon Footprint resource mappings**
* **SOSA observations**
* **GeoSPARQL regions**
* **OWL-Time reporting periods**
* **ISO 14064 and ISO 14067 concept mappings**
* **EU sustainability reporting taxonomy**
* **electricityMap/WattTime grid-intensity vocabularies**

These should become one generated relationship:

```text
CloudResource
→ consumed QuantityOfEnergy
→ in Region
→ during TimeInterval
→ produced CarbonEquivalent
→ attributedTo BusinessCapability
```

---

# 14. Industry overlays Fortune-5 buyers will expect

## Financial services

* FIBO
* ISO 20022
* FIX Orchestra
* ACORD
* LEI and GLEIF data model
* BIAN service landscape
* XBRL taxonomies

## Healthcare and life sciences

* HL7 FHIR and FHIR RDF
* SNOMED CT
* LOINC
* RxNorm
* CDISC
* OMOP
* IDMP
* BioPortal ontologies

## Retail and supply chain

* GS1 Web Vocabulary
* EPCIS 2.0
* UN/CEFACT Core Component Library
* Schema.org Product and Offer
* Open Supply Hub models

## Manufacturing and automotive

* Asset Administration Shell
* OPC UA Companion Specifications
* ISA-95/B2MML
* SAREF
* SOSA/SSN
* QUDT
* Catena-X semantic models

## Telecommunications

* TM Forum SID
* TM Forum Open APIs
* ETSI NFV information models
* 3GPP management models
* YANG/OpenConfig

## Energy and utilities

* IEC CIM
* SAREF4ENER
* OpenADR
* Green Button
* SOSA/SSN
* QUDT

---

# The canonical AutoFDE ontology inventory

I would create these **16 ontology packs**:

```text
ontology/
  00-foundation/
  01-cloud-reference/
  02-cloud-resources/
  03-organization/
  04-identity-authority/
  05-security-threat/
  06-controls-compliance/
  07-events-observability/
  08-process-decision/
  09-data-governance/
  10-cost-finops/
  11-software-supply-chain/
  12-ai-agent/
  13-physical-digital-twin/
  14-sustainability/
  15-industry/
```

## Mandatory P0 imports

The first AutoFDE version should directly profile or import:

1. RDF/RDFS/OWL
2. SHACL
3. SKOS
4. DCTERMS
5. PROV-O
6. DCAT 3
7. ODRL
8. ORG
9. OWL-Time
10. SOSA/SSN
11. QUDT
12. DPV
13. NIST cloud taxonomy
14. TOSCA
15. OSCAL
16. STIX
17. ATT&CK
18. D3FEND
19. CloudEvents
20. OpenTelemetry semantic conventions
21. FOCUS
22. OpenLineage
23. SPDX
24. OCEL
25. POWL

Then generate provider packs from:

```text
AWS CloudFormation schemas
Azure ARM schemas
Google Cloud Asset Inventory
OCI Resource Search
IBM Resource Controller
Alibaba Resource Center
Terraform provider schemas
Kubernetes OpenAPI
```

---

# Critical design rule

Do **not** copy these into AutoFDE and then let them drift.

Each public source needs a registry record:

```text
PublicSemanticSource {
    source_identity
    source_kind
    canonical_location
    version
    retrieval_timestamp
    license
    source_digest
    transformation
    generated_projection_digest
    validation_result
    supersedes
    standing
}
```

The pipeline should be:

```text
public authoritative source
→ pinned retrieval
→ license check
→ source digest
→ parser
→ normalized AutoFDE projection
→ SHACL validation
→ cross-provider mappings
→ generated customer ontology
```

# The strategic conclusion

The core value will not be possessing these vocabularies.

The category-defining value is:

> **AutoFDE compiles the public semantics of cloud infrastructure, identity, security, finance, data, process, and organizational authority into one customer-specific executable operating model.**

That is the foundation from which ggen can manufacture 80% of each AutoFDE deployment without customer-specific imperative code.

[1]: https://docs.aws.amazon.com/AWSCloudFormation/latest/TemplateReference/cfn-resource-specification.html?utm_source=chatgpt.com "CloudFormation resource specification - AWS CloudFormation"
[2]: https://www.nist.gov/publications/nist-cloud-computing-reference-architecture?utm_source=chatgpt.com "NIST Cloud Computing Reference Architecture | NIST"
[3]: https://cloud.ibm.com/docs/account?locale=en&topic=account-crn&utm_source=chatgpt.com "Cloud Resource Names | IBM Cloud Docs"
[4]: https://developer.hashicorp.com/terraform/cli/commands/providers/schema?utm_source=chatgpt.com "terraform providers schema command | Terraform | HashiCorp Developer"
[5]: https://www.w3.org/TR/prov-o/?utm_source=chatgpt.com "PROV-O: The PROV Ontology"
[6]: https://pages.nist.gov/OSCAL/?utm_source=chatgpt.com "OSCAL - Open Security Controls Assessment Language"
[7]: https://github.com/cloudevents/spec?utm_source=chatgpt.com "GitHub - cloudevents/spec: CloudEvents Specification · GitHub"
[8]: https://focus.finops.org/focus-specification/v1-4/?utm_source=chatgpt.com "FOCUS Specification v1.4"
[9]: https://www.w3.org/community/reports/dpvcg/CG-FINAL-dpv-20240801/?utm_source=chatgpt.com "Data Privacy Vocabulary (DPV)"
[10]: https://spec.openapis.org/oas/v3.2.0.html?utm_source=chatgpt.com "OpenAPI Specification v3.2.0"
[11]: https://spdx.dev/use/specifications/?utm_source=chatgpt.com "Specifications – SPDX"
[12]: https://spec.edmcouncil.org/fibo/index.html?utm_source=chatgpt.com "FIBO"
