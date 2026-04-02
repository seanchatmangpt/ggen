# GGEN PACKS ARCHITECTURE & DETAILED DESIGN
## Analyze Phase - Lean Six Sigma

**Document Purpose**: Comprehensive architecture and detailed specifications for ggen packs implementation
**Phase**: ANALYZE (Step 3 of DMAIC)
**Status**: DESIGN SPECIFICATION

---

# 1. System Architecture

## 1.1 High-Level Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                    CLI Layer (clap-noun-verb)                │
│                 Auto-discovery of ggen packs                 │
│                    commands at startup                       │
└────────────────────────┬─────────────────────────────────────┘
                         │
         ┌───────────────┼───────────────┐
         │               │               │
    ┌────▼───┐    ┌─────▼────┐    ┌────▼────┐
    │Discovery│    │Management│    │Compliance│
    │ Commands│    │ Commands │    │ Commands │
    └────┬────┘    └─────┬────┘    └────┬─────┘
         │               │               │
         └───────────────┼───────────────┘
                         │
         ┌───────────────▼───────────────┐
         │  Package Management Domain    │
         │  (ggen-domain/marketplace)    │
         │                               │
         │ ├─ Package Discovery          │
         │ ├─ Dependency Analysis        │
         │ ├─ Manifest Management        │
         │ ├─ Security Scanning          │
         │ ├─ Audit Logging              │
         │ ├─ Statistics & Analytics     │
         │ └─ Compliance Checking        │
         └───────────────┬───────────────┘
                         │
         ┌───────────────┼───────────────┐
         │               │               │
    ┌────▼────┐    ┌────▼────┐    ┌────▼────┐
    │ ggen-   │    │ Local   │    │ CVE/   │
    │marketplace    │File Sys │    │Security│
    │ (v2/v3) │    │         │    │Database│
    └─────────┘    └─────────┘    └────────┘
```

## 1.2 Layered Architecture

### Layer 1: CLI Presentation Layer
- **Responsibility**: Accept user input, format output
- **Components**:
  - Command routing (clap-noun-verb auto-discovery)
  - Input validation and parsing
  - Output formatting (human, JSON, YAML)
  - Error handling and user messages
  - Progress indicators and interactive features

### Layer 2: Domain Logic Layer
- **Responsibility**: Business logic for package management
- **Components**:
  - Package discovery service
  - Dependency resolver
  - Manifest manager
  - Compliance checker
  - Security scanner
  - Audit logger
  - Statistics calculator

### Layer 3: Data & Integration Layer
- **Responsibility**: Access to packages, files, external data
- **Components**:
  - MarketplaceRegistry adapter (v2/v3)
  - File system integration (gpack.toml scanning)
  - Local cache management
  - CVE database access
  - Audit log persistence

---

# 2. Core Components & Services

## 2.1 Package Discovery Service

**Purpose**: Find and catalog all packages on a system

**Responsibilities**:
- Scan all projects for gpack.toml files
- Query marketplace for installed packages
- Index packages in memory/cache
- Filter and sort results
- Measure package sizes

**Inputs**:
- System paths to search
- Filter criteria (quality, production-ready, author, etc.)
- Sort preferences

**Outputs**:
- List of PackageInfo objects
- Metadata and statistics
- Performance metrics

**Performance Target**: <1 second for 1000 packages

**Implementation Strategy**:
- Parallel directory scanning (rayon)
- Lazy loading of package metadata
- In-memory cache with file-based fallback
- Incremental updates (delta scanning)

---

## 2.2 Dependency Resolver

**Purpose**: Build complete dependency tree and detect conflicts

**Responsibilities**:
- Parse dependency specifications (from gpack.toml)
- Resolve transitive dependencies
- Detect circular dependencies
- Identify version conflicts
- Calculate dependency trees
- Show impact of changes

**Inputs**:
- Root package
- Dependency specifications
- Version constraints

**Outputs**:
- Complete dependency tree
- Conflict warnings
- Impact analysis

**Algorithm**:
```
1. Start with root package
2. For each dependency:
   a. Resolve version constraint
   b. Load that package's dependencies
   c. Recursively resolve (track visited to avoid cycles)
3. Aggregate results
4. Detect conflicts:
   - Same package, different versions
   - Circular dependencies
5. Return tree and conflicts
```

**Performance Target**: <100ms for complex graphs (100+ packages)

---

## 2.3 Manifest Manager

**Purpose**: Generate, apply, and maintain dependency manifests

**Responsibilities**:
- Generate manifests from current state
- Lock versions (exact versions)
- Apply manifests (install exact versions)
- Compare manifests
- Store and retrieve manifests
- Validate manifest format

**Manifest Operations**:

### Generate Manifest
```
Input: Project/environment identifier
Output: Manifest file with all packages and versions

1. Discover all packages in project
2. Resolve all dependencies (transitive)
3. Lock versions (exact semver)
4. Create manifest.json/yaml
5. Store with metadata (created_by, timestamp, environment)
```

### Apply Manifest
```
Input: Manifest file
Output: Project with exact versions installed

1. Parse and validate manifest
2. For each package in manifest:
   a. Check if already installed
   b. If different version, install correct version
   c. If not installed, install
3. Verify all dependencies satisfied
4. Log operation (audit)
5. Return status
```

### Compare Manifests
```
Input: Two manifest files
Output: Differences between them

1. Parse both manifests
2. Find packages in A but not B
3. Find packages in B but not A
4. Find packages in both with version differences
5. Format output
```

---

## 2.4 Security Scanner

**Purpose**: Scan packages for known vulnerabilities

**Responsibilities**:
- Download/update CVE database
- Check packages against CVEs
- Calculate risk scores
- Generate vulnerability reports
- Track scan history

**Scanner Workflow**:
```
1. Update CVE database (if needed)
2. For each package:
   a. Look up package name and version
   b. Find matching CVEs
   c. Assess severity (critical/high/medium/low)
   d. Recommend fix (update to version X)
3. Aggregate results
4. Generate report
5. Return exit code based on severity
```

**CVE Data Integration**:
- Source: National Vulnerability Database (NVD)
- Format: JSON feed
- Update frequency: Daily (configurable)
- Caching: Local database (SQLite or similar)
- Offline mode: Works with cached data

---

## 2.5 Audit Logger

**Purpose**: Track all package operations for compliance

**Responsibilities**:
- Log all package operations (install, update, delete, etc.)
- Store audit entries
- Query audit logs
- Export audit logs
- Rotate logs (optional)

**Audit Entry Content**:
```
{
  timestamp: DateTime,
  user: String,
  action: String (install, update, uninstall, etc.),
  package_id: String,
  version: String,
  from_version: Option<String>,  // For updates
  status: String (success, failure),
  details: String (error message if failed),
  environment: {
    machine: String,
    os: String,
    project: Option<String>,
  }
}
```

**Storage**:
- Location: `~/.ggen/audit.log` (per user)
- Format: JSONL (one JSON object per line)
- Rotation: Based on size/time
- Retention: 1 year (configurable)

---

## 2.6 Compliance Checker

**Purpose**: Enforce team standards and policies

**Responsibilities**:
- Validate packages against bundles
- Check quality scores
- Check production-ready flags
- Verify required licenses
- Enforce version constraints

**Compliance Rules**:
```
1. Only approved packages (bundle members)
2. Minimum quality score (e.g., 70)
3. Must be production-ready (for prod environment)
4. License must be compatible
5. Version must be in allowed range
```

**Implementation**:
- Soft enforcement (warn): Advisory mode
- Hard enforcement (block): Prevent installation
- Configurable per team/environment

---

## 2.7 Statistics & Analytics

**Purpose**: Provide insights on package usage

**Responsibilities**:
- Track package usage patterns
- Calculate adoption metrics
- Identify unused packages
- Show version migration trends
- Generate reports

**Key Metrics**:
- **Adoption**: % of projects using package
- **Version Distribution**: Which versions are popular
- **Trend**: Adoption over time
- **Health**: Unused packages, outdated versions
- **Security**: Packages with vulnerabilities
- **Compliance**: Unapproved packages

---

# 3. Detailed Command Specifications

## 3.1 Discovery Commands

### `ggen packs list [OPTIONS]`

**Purpose**: List all packages installed on system

**Options**:
```
--format <FORMAT>             # Output format: human (default), json, yaml, csv
--filter <FILTER>             # Filter expression (see filtering section)
--sort <FIELD>                # Sort by: name, version, size, quality, last-used
--limit <N>                   # Show first N results
--projects                    # Show which projects use each
--quality-score <MIN>         # Only packages with score >= MIN
--production-only             # Only production-ready packages
--include-deprecated          # Include deprecated packages
--cache <STRATEGY>            # Cache strategy: update, reuse, clear
```

**Output Examples**:

Human format:
```
Package              Version  Quality  Size      Projects
─────────────────────────────────────────────────────────
react                18.2.0   95       2.3 MB    web, dashboard, mobile (3)
lodash               4.17.21  88       1.2 MB    web, api, lib (3)
axios                1.6.2    82       0.8 MB    api, mobile (2)
typescript           5.0.2    91       50 MB     all (5)
```

JSON format:
```json
{
  "packages": [
    {
      "id": "react",
      "name": "React",
      "version": "18.2.0",
      "quality_score": 95,
      "size_bytes": 2400000,
      "projects": ["web", "dashboard", "mobile"],
      "is_production_ready": true
    }
  ],
  "total": 42,
  "timestamp": "2025-11-17T10:30:00Z"
}
```

**Performance Target**: <1s for 1000 packages

**Error Handling**:
- No packages found → Show helpful message
- Permission denied → Show which paths require access
- Corrupted manifest → Show which file is problematic

---

### `ggen packs search <QUERY> [OPTIONS]`

**Purpose**: Search for packages by name, description, author

**Options**:
```
--all-fields              # Search in name, description, author
--name-only              # Search only in package names
--description            # Search only in descriptions
--author <NAME>          # Filter by author
--limit <N>              # Show first N results
--quality-score <MIN>    # Minimum quality score
--format <FORMAT>        # Output format
--sort <FIELD>           # Sort results
```

**Examples**:
```bash
ggen packs search "database"
ggen packs search "lodash" --name-only
ggen packs search "UI" --quality-score 80 --author "React Core"
ggen packs search "test" --limit 5 --json
```

**Algorithm**:
1. Tokenize query
2. Search name/description/author for matches
3. Score by relevance (exact match > fuzzy > partial)
4. Filter by criteria
5. Sort and limit
6. Return results

---

### `ggen packs info <PACKAGE> [OPTIONS]`

**Purpose**: Show detailed information about a package

**Options**:
```
--include-versions       # Show all available versions
--include-dependents     # Show packages that depend on this
--include-security       # Show security vulnerabilities
--format <FORMAT>        # Output format
```

**Output**:
```
Package: react
────────────────────
ID: react
Name: React
Version: 18.2.0
Author: React Core Team
License: MIT
Repository: https://github.com/facebook/react
Homepage: https://react.dev

Quality Score: 95/100
Production Ready: Yes

Description:
A JavaScript library for building user interfaces with
reactive components and virtual DOM.

Installed In:
  • web/gpack.toml (18.2.0)
  • dashboard/gpack.toml (18.2.0)
  • mobile/gpack.toml (18.1.0)

Security Status:
  • CVEs Found: 0
  • Last Scanned: 2025-11-17T10:00:00Z
  • Status: SAFE

Dependencies (12):
  • @babel/runtime ^7.0
  • loose-envify ^1.1.0
  • scheduler ^0.23.0
  • ... (9 more)

Dependents (8):
  • @next/react (26 projects)
  • react-dom (42 projects)
  • react-redux (3 projects)
  • ... (5 more)
```

---

### `ggen packs stats [OPTIONS]`

**Purpose**: Show package statistics and trends

**Options**:
```
--timeline <DAYS>        # Look back N days
--package <NAME>         # Stats for specific package
--top <N>                # Top N packages
--sort <METRIC>          # Sort by: adoption, size, popularity
--format <FORMAT>        # Output format
--export <FILE>          # Export to file
```

**Output**:
```
Overall Statistics
──────────────────
Total Packages: 42
Total Size: 2.3 GB
Total Projects: 5
Avg Package Age: 180 days

Top Packages by Adoption
────────────────────────
1. typescript       85% of projects (5/5)
2. @types/node     80% of projects (4/5)
3. react           60% of projects (3/5)
4. webpack         60% of projects (3/5)
5. lodash          60% of projects (3/5)

Unused Packages (not in any active project)
──────────────────────────────────────────
• old-logger (1.0.0) - 2.1 MB
• deprecated-api (0.5.0) - 0.8 MB

Security Status
──────────────
Safe: 38 packages
Warnings: 2 packages (minor CVEs)
Critical: 0 packages
Last Scan: 2025-11-17T10:00:00Z
```

---

## 3.2 Management Commands

### `ggen packs install <PACKAGE> [OPTIONS]`

**Purpose**: Install a package globally or in a project

**Options**:
```
--version <VERSION>      # Specific version (default: latest)
--project <PATH>         # Project path (default: all projects)
--update-if-exists       # Update if already installed
--save-to-manifest       # Add to manifest after install
--validate               # Validate after install
--skip-deps              # Don't install dependencies
```

**Workflow**:
```
1. Parse package and version
2. Check if already installed
3. If already installed and not --update-if-exists, skip
4. Resolve dependencies
5. Check for conflicts
6. If conflicts and not auto-resolve, ask user
7. Download/copy package
8. Verify integrity
9. Update project manifests
10. Log audit entry
11. Show result
```

---

### `ggen packs uninstall <PACKAGE> [OPTIONS]`

**Purpose**: Remove a package

**Options**:
```
--version <VERSION>      # Specific version (default: all)
--project <PATH>         # Project to uninstall from
--force                  # Force even if dependents exist
--save                   # Save operation to manifest
```

**Safety Checks**:
1. Check for dependents (other packages that require this)
2. If dependents exist and not --force, ask user
3. Show what will be removed
4. Ask for confirmation

---

### `ggen packs update [PACKAGE] [OPTIONS]`

**Purpose**: Update packages to newer versions

**Options**:
```
--all                    # Update all packages
--minor                  # Update to latest minor version
--patch                  # Update to latest patch version
--version <VERSION>      # Update to specific version
--project <PATH>         # Only in specific project
--dry-run                # Show what would be updated
--manifest <FILE>        # Update to versions in manifest
--check-only             # Just check for updates
```

**Update Strategies**:
- `--patch`: 1.2.3 → 1.2.5 (backward compatible)
- `--minor`: 1.2.3 → 1.5.0 (likely backward compatible)
- `--version 2.0`: 1.2.3 → 2.0.0 (may have breaking changes)

**Output**:
```
Checking for updates...

Updates Available:
  • react: 18.2.0 → 18.3.0 (minor)
  • webpack: 5.88.0 → 5.89.0 (patch)
  • typescript: 5.0.2 → 5.1.0 (minor)

Impact Analysis:
  • 3 projects affected
  • 0 breaking changes detected
  • 1 package with security update

Proceed? [y/n]
```

---

## 3.3 Manifest Commands

### `ggen packs manifest generate [OPTIONS]`

**Purpose**: Create a dependency manifest for project/environment

**Options**:
```
--environment <ENV>      # Environment: dev, staging, prod
--output <FILE>          # Output file (default: gpack.lock.json)
--lock                   # Lock exact versions
--include-transitive     # Include transitive dependencies
--format <FORMAT>        # yaml or json
--all-projects           # Generate for all projects
```

**Manifest Content**:
```json
{
  "version": "1.0",
  "environment": "prod",
  "generated_at": "2025-11-17T10:30:00Z",
  "generated_by": "claude",
  "locked": true,

  "packages": [
    {
      "id": "react",
      "version": "18.2.0",
      "required": true,
      "tier": "required",
      "transitive": false
    },
    {
      "id": "react-dom",
      "version": "18.2.0",
      "required": true,
      "transitive": true,
      "parent": "react"
    }
  ],

  "metadata": {
    "total_packages": 42,
    "total_size_bytes": 2300000000,
    "quality_threshold": 70,
    "security_status": "all_safe"
  }
}
```

---

### `ggen packs manifest apply <FILE> [OPTIONS]`

**Purpose**: Apply a manifest (install exact versions)

**Options**:
```
--project <PATH>         # Project to apply to
--backup                 # Backup current state first
--dry-run                # Show what would happen
--force                  # Force even if conflicts
```

**Workflow**:
```
1. Parse manifest
2. Validate format
3. Backup current state (if --backup)
4. For each package:
   a. Check current version
   b. If different, install correct version
   c. Check for conflicts
5. Verify all dependencies satisfied
6. Log operation
7. Show result
8. Offer rollback if failures
```

**Safety Features**:
- Backup before applying
- Atomic operation (all succeed or all rollback)
- Show preview before applying
- Easy rollback to previous manifest

---

### `ggen packs manifest compare <FILE1> <FILE2> [OPTIONS]`

**Purpose**: Compare two manifests

**Options**:
```
--format <FORMAT>        # Output format: human, json, diff
--only-differences       # Show only different packages
--ignore-order           # Ignore package order
```

**Output**:
```
Comparing prod.json vs staging.json
───────────────────────────────────

Only in prod (3):
  • webpack-cli (5.1.0)
  • terser (5.15.0)
  • mini-css-extract-plugin (2.7.0)

Only in staging (0):

Different versions (2):
  • react: 18.2.0 → 18.3.0
  • typescript: 5.0.2 → 5.1.0

Same (39):
  • axios 1.6.2
  • lodash 4.17.21
  • ... (37 more)
```

---

## 3.4 Bundle Commands

### `ggen packs bundle create <NAME> [OPTIONS]`

**Purpose**: Create a bundle of approved packages

**Options**:
```
--description <TEXT>     # Bundle description
--enforcement <LEVEL>    # soft (warn) or hard (block)
--quality-min <SCORE>    # Minimum quality score
--production-only        # Only include production-ready packages
--from-manifest <FILE>   # Create from manifest
```

**Bundle Structure**:
```json
{
  "id": "team-approved",
  "name": "Team Approved Packages",
  "description": "Official package bundle for all team projects",
  "created_at": "2025-11-17T10:30:00Z",
  "created_by": "team-lead",
  "enforcement": "soft",
  "quality_threshold": 70,
  "production_ready_required": false,

  "packages": [
    {
      "id": "react",
      "version_constraint": "^18.0",
      "tier": "required"
    },
    {
      "id": "lodash",
      "version_constraint": "^4.17",
      "tier": "recommended"
    }
  ]
}
```

---

### `ggen packs bundle apply <NAME> [OPTIONS]`

**Purpose**: Apply a bundle to project(s)

**Options**:
```
--project <PATH>         # Project to apply to
--all-projects           # Apply to all projects
--validate               # Validate before applying
--report                 # Show compliance report after
```

**Compliance Check**:
```
Applying bundle: team-approved

Checking all projects...

✓ web/gpack.toml:
  - All packages in bundle
  - All versions within constraints
  - Status: COMPLIANT

✗ legacy-api/gpack.toml:
  - Package 'moment' not in bundle (WARN)
  - Package 'underscore' not in bundle (WARN)
  - Version 'lodash@3.10' outside constraint ^4.17 (ERROR)
  - Status: NON-COMPLIANT

Summary: 4/5 projects compliant
```

---

## 3.5 Compliance Commands

### `ggen packs audit [OPTIONS]`

**Purpose**: View audit log of all package operations

**Options**:
```
--user <NAME>            # Filter by user
--action <ACTION>        # Filter by action type
--package <NAME>         # Filter by package
--from <DATE>            # From date (format: YYYY-MM-DD)
--to <DATE>              # To date
--limit <N>              # Show last N entries
--export <FILE>          # Export to file
--format <FORMAT>        # Output format
```

**Output**:
```
Audit Log (Last 10 operations)
──────────────────────────────────────────────────────────

2025-11-17 10:30:45  claude  install    react@18.2.0        SUCCESS  web/
2025-11-17 10:28:12  claude  manifest   apply staging.json   SUCCESS  -
2025-11-17 09:15:00  claude  update     typescript 5.0 → 5.1 SUCCESS  -
2025-11-17 08:45:30  sarah   security   scan all             SUCCESS  -
2025-11-16 16:20:00  john    uninstall  moment@2.29.0        SUCCESS  lib/
...

Filters:
 ├─ User: all
 ├─ Action: all
 ├─ Package: all
 └─ Time range: Last 24 hours
```

---

### `ggen packs verify [OPTIONS]`

**Purpose**: Verify system consistency and compliance

**Options**:
```
--manifest <FILE>        # Check against specific manifest
--bundle <NAME>          # Check against bundle
--security               # Check for vulnerabilities
--dependencies           # Check dependency completeness
--all                    # Run all checks
--fix                    # Auto-fix issues (if possible)
--report                 # Generate detailed report
```

**Verification Output**:
```
Running Verification...

✓ All packages found (42/42)
✓ No corrupted manifests
✗ Version conflicts detected (2):
  - Package 'react': web/ uses 18.2.0, mobile/ uses 18.1.0
  - Package 'lodash': api/ requires ^4.17, lib/ uses 3.10.0

✗ Security issues (1):
  - axios: CVE-2023-12345 (medium)

✓ All dependencies satisfied
✓ Compliance: 4/5 projects in bundle

Summary: 4 issues found
Recommendation: Run 'ggen packs fix-conflicts' or manual resolution
```

---

### `ggen packs deprecate <PACKAGE> [OPTIONS]`

**Purpose**: Mark a package as deprecated

**Options**:
```
--version <VERSION>      # Specific version to deprecate
--message <TEXT>         # Deprecation message
--replacement <PKG>      # Suggested replacement
--from <DATE>            # Deprecation start date
--to <DATE>              # Final removal date
--notify                 # Notify package dependents
```

**Deprecation Notice**:
```
Package Deprecation Notice
──────────────────────────

Package: moment
Version: 2.x (all versions)
Status: DEPRECATED

Deprecation Date: 2025-11-17
Final Removal: 2026-11-17
Reason: Moment is in maintenance mode. Consider date-fns or day.js.

Replacement: date-fns@^2.30
Migration Guide: https://date-fns.org/v2.30.0/docs/Getting-Started

Affected Projects (12):
 • api/
 • web/
 • batch-jobs/
 • ...

Recommended Action: Plan migration in next sprint
```

---

## 3.6 Security Commands

### `ggen packs security-scan [OPTIONS]`

**Purpose**: Scan packages for security vulnerabilities

**Options**:
```
--severity <LEVEL>       # Filter: critical, high, medium, low, all
--update-db              # Update CVE database
--offline                # Use cached database
--exit-code-if-vulns     # Exit with code 1 if vulnerabilities found
--format <FORMAT>        # Output format
--export <FILE>          # Export scan results
```

**Scan Results**:
```
Security Vulnerability Scan
──────────────────────────────────────────

Scanning 42 packages...

CRITICAL (0):
  (none)

HIGH (1):
  • axios@1.6.1
    CVE-2023-45678: Prototype Pollution in Header Processing
    Affected Versions: <=1.6.1
    Fixed Version: 1.6.2
    CVSS: 8.1
    Action: UPDATE REQUIRED

MEDIUM (2):
  • lodash@4.17.20
    CVE-2023-12345: ReDoS in template parsing
    Affected: 4.17.0 - 4.17.20
    Fixed: 4.17.21
    Action: Update recommended

  • moment@2.29.0
    Multiple CVEs (deprecated package)
    Action: REPLACE with date-fns

LOW (0):

Summary:
 • Total packages scanned: 42
 • Safe: 39
 • With warnings: 2
 • Critical: 0
 • Status: NEEDS ATTENTION

Last Scan: 2025-11-17T10:00:00Z
CVE Database: Updated 2025-11-17
```

---

# 4. Data Model Specifications

## 4.1 Core Data Structures

### PackageInfo
```
PackageInfo {
  id: String                    # Unique identifier
  name: String                  # Display name
  version: SemVer              # Current version
  author: String               # Package author/creator
  quality_score: u32           # 0-100
  is_production_ready: bool    # Maturity flag
  description: String          # Short description
  repository: Option<URL>      # Source repository
  license: Option<String>      # License name
  homepage: Option<URL>        # Project homepage

  # Storage/location
  location: PathBuf            # Where installed
  size_bytes: u64              # Disk usage

  # Metadata
  published_at: DateTime       # Release date
  last_used: DateTime          # Last access time
  created_at: DateTime         # Installation date

  # Status
  is_deprecated: bool          # Deprecation flag
  deprecation_notice: Option<String>
  security_status: SecurityStatus
}

SecurityStatus {
  has_vulnerabilities: bool
  cves: Vec<CVE>
  severity_level: String       # critical, high, medium, low
  last_scanned: DateTime
}

CVE {
  id: String                   # CVE-YYYY-NNNNN
  description: String
  severity: String
  cvss_score: f64              # 0.0-10.0
  affected_versions: Vec<String>
  fixed_version: Option<String>
  link: URL
}
```

### ProjectInfo
```
ProjectInfo {
  path: PathBuf
  name: String                 # Project name
  type: ProjectType            # web, api, lib, etc.
  gpack_manifest: PathBuf      # Path to gpack.toml
  packages: Vec<PackageSpec>   # Direct dependencies

  # Metadata
  team: Option<String>
  owner: Option<String>
  last_updated: DateTime
}

ProjectType = Web | API | Library | CLI | Other
```

### ManifestFile
```
ManifestFile {
  version: String              # Manifest format version
  environment: String          # dev, staging, prod

  packages: Vec<PackageSpec> {
    id: String
    version: String            # Exact or constraint
    required: bool
    tier: String              # required, recommended, optional
    transitive: bool          # Indirect dependency
    parent: Option<String>    # Direct parent if transitive
  }

  # Metadata
  generated_at: DateTime
  generated_by: String
  locked: bool                # Version-locked?

  metadata: {
    total_packages: u32
    total_size_bytes: u64
    quality_threshold: u32
    security_status: String
  }
}
```

---

# 5. Interface Specifications

## 5.1 Output Formatting

### Human Format (Default)
- Table layout with aligned columns
- Color coding (for terminal support)
- Progress bars for long operations
- Clear headers and summaries

### JSON Format
- Valid JSON objects
- Consistent structure across all commands
- Suitable for scripting and integration

### YAML Format
- YAML 1.2 compatible
- Human-readable
- Suitable for configuration/manifests

### CSV Format
- RFC 4180 compliant
- Headers in first row
- Suitable for spreadsheet/analysis

---

## 5.2 Error Handling

### Error Categories
1. **User Errors**: Invalid input, file not found
2. **Validation Errors**: Version constraints, conflicts
3. **System Errors**: Permission denied, disk full
4. **External Errors**: Network issues, CVE database unavailable

### Error Output Pattern
```
Error: <error code>: <short message>
<longer explanation>

Possible fixes:
  • Fix 1
  • Fix 2

For more help: ggen packs <command> --help
```

---

# 6. Performance Requirements & Targets

| Operation | Data Size | Target Time | Rationale |
|-----------|-----------|-------------|-----------|
| List all packages | 1,000 | <1s | Interactive use |
| Search packages | 1,000 | <500ms | Frequent operation |
| Dependency resolve | 100 packages | <100ms | Quick feedback |
| Manifest generate | 100 packages | <500ms | Part of workflow |
| Manifest apply | 100 packages | <2s | Installation time |
| Security scan | 1,000 | <5s | Reasonable wait |
| Audit query | 10,000 entries | <200ms | Fast lookups |
| Compare manifests | 100 packages | <100ms | Quick diff |

---

# 7. Integration Points

## 7.1 Integration with ggen-marketplace

- Use `MarketplaceRegistry` adapter trait
- Query v2 RDF backend for package metadata
- Leverage existing search and quality scoring
- Use existing dependency resolution logic

## 7.2 Integration with ggen-domain

- Use existing audit logging infrastructure
- Use existing validation frameworks
- Leverage security scanning frameworks

## 7.3 External Integrations (Future)

- **CVE Databases**: NVD (National Vulnerability Database)
- **Marketplace Notifications**: For deprecation alerts
- **CI/CD**: GitHub Actions, GitLab CI integration
- **Team Communication**: Slack, Teams notifications

---

# 8. Implementation Roadmap (High-Level)

### Week 1: Setup & Discovery
- [ ] Create domain module structure
- [ ] Implement PackageDiscovery service
- [ ] Implement `ggen packs list` command
- [ ] Implement `ggen packs search` command

### Week 2: Analysis & Management
- [ ] Implement DependencyResolver
- [ ] Implement ManifestManager
- [ ] Implement `ggen packs manifest` commands
- [ ] Implement `ggen packs info` command

### Week 3: Safety & Security
- [ ] Implement AuditLogger
- [ ] Implement SecurityScanner
- [ ] Implement `ggen packs audit` command
- [ ] Implement `ggen packs security-scan` command

### Week 4: Compliance & Polish
- [ ] Implement ComplianceChecker
- [ ] Implement `ggen packs verify` command
- [ ] Implement `ggen packs deprecate` command
- [ ] Testing, documentation, release

---

# 9. Testing Strategy Overview

(Detailed testing strategy in separate document)

### Unit Tests
- PackageInfo serialization/deserialization
- Dependency resolution algorithms
- Version constraint parsing
- Manifest parsing and validation

### Integration Tests
- End-to-end command workflows
- Real project scanning
- Reference dataset validation

### Performance Tests
- Benchmark 1000, 5000, 10000 package scenarios
- Latency profiling
- Memory usage analysis

### Compatibility Tests
- Different OS (Linux, macOS, Windows)
- Different gpack project types
- Different manifest formats (YAML, JSON)

---

# 10. Success Criteria

## MVP Success (Week 4)
- ✓ All discovery and manifest commands working
- ✓ <1s response time for 1000 packages
- ✓ 90%+ test coverage
- ✓ Documentation complete
- ✓ 3 pilot teams using system

## V1 Success (Week 8)
- [ ] All commands implemented
- [ ] <1s response time for 10,000 packages
- [ ] 95%+ test coverage
- [ ] Full documentation
- [ ] 50%+ team adoption

