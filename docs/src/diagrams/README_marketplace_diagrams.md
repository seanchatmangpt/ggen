# Ggen Marketplace C4 Diagrams

This directory contains comprehensive C4 architecture diagrams for the Ggen Marketplace system, documenting the full end-to-end lifecycles and system interactions.

## Diagram Overview

### 1. System Context (`C4_marketplace_context.puml`)
**Purpose**: High-level view of the marketplace system and its external interactions
**Key Elements**:
- Developer and Publisher personas
- Ggen CLI system
- Marketplace registry
- GitHub hosting platform

### 2. Container Diagram (`C4_marketplace_container.puml`)
**Purpose**: Shows the major containers and their responsibilities
**Key Elements**:
- CLI and Core Engine containers
- Local Cache and Lockfile
- Registry Index and CI/CD Pipeline
- Gpack Repositories

### 3. Consumer Lifecycle (`C4_marketplace_consumer_lifecycle.puml`)
**Purpose**: Detailed workflow for developers using gpacks
**Key Elements**:
- Search, Add, List, Generate, Update, Remove commands
- Registry Client, Cache Manager, Lockfile Manager
- Template Resolver and Generation Pipeline
- External systems (Registry, Repos, Cache, Lockfile)

### 4. Publisher Lifecycle (`C4_marketplace_publisher_lifecycle.puml`)
**Purpose**: Detailed workflow for publishers creating gpacks
**Key Elements**:
- Pack Init, Lint, Test, Publish commands
- Validation System (Schema, Semver, Compatibility, Path, License, Size, Security)
- Registry System (Repository, Index Generator, Pages)
- Gpack Repository structure

### 5. Data Flow (`C4_marketplace_data_flow.puml`)
**Purpose**: Shows how data flows through the system
**Key Elements**:
- Search, Add, Generate, Publish, Update data flows
- Local System (CLI, Cache, Lockfile, Config)
- Registry System (Index, Pages)
- Gpack Repositories (Manifest, Templates, RDF, Queries)

### 6. Sequence Diagram (`C4_marketplace_sequence.puml`)
**Purpose**: Detailed sequence of interactions for key workflows
**Key Elements**:
- Search Workflow
- Add Workflow
- Generate Workflow
- Update Workflow
- Remove Workflow

### 7. Deployment Diagram (`C4_marketplace_deployment.puml`)
**Purpose**: Shows how the system is deployed across different environments
**Key Elements**:
- Developer Machine (Local installation)
- GitHub Platform (Registry repo, Gpack repos, Pages)
- Network (HTTPS, Git protocol)
- Security considerations

### 8. Security Model (`C4_marketplace_security.puml`)
**Purpose**: Documents the security architecture and threat model
**Key Elements**:
- Trust boundaries and relationships
- Security controls (SHA256, License, Path, Sandbox, Network, Static Analysis)
- Security threats and mitigations
- Trust levels (Trusted, Semi-trusted, Untrusted)

### 9. Error Handling (`C4_marketplace_error_handling.puml`)
**Purpose**: Documents error scenarios and recovery strategies
**Key Elements**:
- Network errors, Pack not found, Version resolution errors
- Download errors, Integrity verification errors
- Lockfile errors, Template resolution errors
- Cache corruption, Compatibility errors
- Recovery strategies and user guidance

### 10. Performance & Scalability (`C4_marketplace_performance.puml`)
**Purpose**: Documents performance characteristics and scalability considerations
**Key Elements**:
- Performance optimizations (Local caching, Index caching, Parallel downloads)
- Incremental updates, Compression, CDN distribution
- Performance metrics and monitoring
- Scalability limits and considerations

## Usage

These diagrams can be rendered using PlantUML:

```bash
# Install PlantUML
npm install -g plantuml

# Render all diagrams
plantuml docs/diagrams/C4_marketplace_*.puml

# Render specific diagram
plantuml docs/diagrams/C4_marketplace_context.puml
```

## Key Lifecycle Flows

### Consumer Lifecycle
1. **Search** → Find gpacks in registry
2. **Add** → Download and cache gpacks
3. **List** → Show installed gpacks
4. **Generate** → Use gpack templates
5. **Update** → Update to latest versions
6. **Remove** → Clean up gpacks

### Publisher Lifecycle
1. **Init** → Create new gpack structure
2. **Lint** → Validate gpack manifest
3. **Test** → Test template rendering
4. **Publish** → Submit to registry via PR
5. **Validation** → Automated CI/CD checks
6. **Deployment** → Registry index update

### Error Recovery
- **Network errors** → Retry with exponential backoff
- **Integrity errors** → Re-download and verify
- **Cache corruption** → Clear and re-download
- **Compatibility errors** → Suggest version updates
- **Template errors** → Provide helpful diagnostics

## Security Considerations

- **Trust boundaries** clearly defined
- **Sandboxed execution** for templates
- **SHA256 verification** for integrity
- **License validation** for compliance
- **Path sanitization** for security
- **Network controls** for access restriction

## Performance Characteristics

- **Local caching** for fast access
- **CDN distribution** for global performance
- **Parallel downloads** for efficiency
- **Incremental updates** for minimal transfers
- **Compression** for bandwidth optimization

These diagrams provide comprehensive documentation of the marketplace system architecture, covering all aspects from high-level context to detailed implementation, security, and performance considerations.
