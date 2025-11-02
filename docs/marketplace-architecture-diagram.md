# Marketplace Architecture - Visual Diagrams

## System Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                          User CLI Commands                           │
│  ggen marketplace search | install | update | list | publish        │
└─────────────────────┬───────────────────────────────────────────────┘
                      │
                      v
┌─────────────────────────────────────────────────────────────────────┐
│                     CLI Command Layer (cmds/)                        │
│  - marketplace/search.rs   - marketplace/install.rs                 │
│  - marketplace/update.rs   - marketplace/list.rs                    │
└─────────────────────┬───────────────────────────────────────────────┘
                      │
                      v
┌─────────────────────────────────────────────────────────────────────┐
│                   Domain Layer (domain/marketplace/)                 │
│                                                                      │
│  ┌──────────────────────┐         ┌───────────────────────────┐   │
│  │     Registry         │◄────────┤    CacheManager           │   │
│  │  (Metadata & Index)  │         │  (Local Storage + LRU)    │   │
│  │                      │         │                           │   │
│  │ • search()           │         │ • write()                 │   │
│  │ • resolve()          │         │ • read()                  │   │
│  │ • resolve_deps()     │         │ • evict_lru()             │   │
│  │ • list_all()         │         │ • verify()                │   │
│  └──────────┬───────────┘         └───────────┬───────────────┘   │
│             │                                  │                   │
└─────────────┼──────────────────────────────────┼───────────────────┘
              │                                  │
              v                                  v
┌──────────────────────────┐      ┌──────────────────────────────────┐
│  ggen-core Registry      │      │    Local Cache Directory         │
│  (RegistryClient)        │      │  ~/.cache/ggen/marketplace/      │
│                          │      │                                  │
│ • fetch_index()          │      │  package_id/                     │
│ • search()               │      │    ├── version1/                 │
│ • resolve()              │      │    │   ├── manifest.json         │
│                          │      │    │   └── ...                   │
│ Remote:                  │      │    └── version2/                 │
│ registry.ggen.dev        │      │        └── ...                   │
│   └── index.json         │      │                                  │
└──────────────────────────┘      └──────────────────────────────────┘
```

## Registry Architecture Details

```
┌────────────────────────────────────────────────────────────────┐
│                          Registry                               │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Fields:                                                        │
│  ┌──────────────────────────────────────────────────────┐     │
│  │ client: RegistryClient                               │     │
│  │ index_cache: Option<RegistryIndex>                   │     │
│  │ cache_ttl: Duration (default: 5min)                  │     │
│  │ last_fetch: Option<Instant>                          │     │
│  └──────────────────────────────────────────────────────┘     │
│                                                                 │
│  Core Operations:                                              │
│  ┌──────────────────────────────────────────────────────┐     │
│  │ Index Management:                                    │     │
│  │  • fetch_index() ──┬─► is_cache_valid()?           │     │
│  │                    │     ├─ Yes ─► Return cached    │     │
│  │                    │     └─ No  ─► Fetch remote     │     │
│  │  • refresh_index()                                   │     │
│  │  • clear_cache()                                     │     │
│  │                                                       │     │
│  │ Discovery:                                           │     │
│  │  • search(query) ──► Filter by name/desc/tags       │     │
│  │  • list_all() ──────► Return all packages           │     │
│  │  • get_package()                                     │     │
│  │                                                       │     │
│  │ Version Resolution:                                  │     │
│  │  • resolve(pkg, ver) ─┬─► Exact: "1.2.3"           │     │
│  │                       ├─► Range: "^1.2.0"           │     │
│  │                       ├─► Latest: "latest"          │     │
│  │                       └─► Constraint: ">=1.0.0"     │     │
│  │                                                       │     │
│  │ Dependencies:                                        │     │
│  │  • resolve_dependencies() ──► DependencyGraph       │     │
│  │  • flatten_dependencies() ──► Install order         │     │
│  │  • check_conflicts() ────────► Conflict list        │     │
│  └──────────────────────────────────────────────────────┘     │
└────────────────────────────────────────────────────────────────┘
```

## CacheManager Architecture Details

```
┌────────────────────────────────────────────────────────────────┐
│                      CacheManager                               │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Fields:                                                        │
│  ┌──────────────────────────────────────────────────────┐     │
│  │ cache_dir: PathBuf (~/.cache/ggen/marketplace)       │     │
│  │ max_size: u64 (default: 5GB)                         │     │
│  │ lru: Arc<Mutex<LruCache<String, CacheEntry>>>        │     │
│  │ current_size: Arc<AtomicU64>                         │     │
│  └──────────────────────────────────────────────────────┘     │
│                                                                 │
│  Cache Operations:                                             │
│  ┌──────────────────────────────────────────────────────┐     │
│  │ write(pkg, ver, content)                             │     │
│  │   └─► Create: cache_dir/pkg_id/version/             │     │
│  │        ├─ manifest.json                              │     │
│  │        └─ package contents                           │     │
│  │                                                       │     │
│  │ read(pkg, ver)                                       │     │
│  │   ├─► Check exists                                   │     │
│  │   ├─► Update LRU (touch)                             │     │
│  │   └─► Return CachedPackage                           │     │
│  │                                                       │     │
│  │ is_cached(pkg, ver)                                  │     │
│  │   └─► Fast filesystem check                          │     │
│  └──────────────────────────────────────────────────────┘     │
│                                                                 │
│  LRU Eviction:                                                 │
│  ┌──────────────────────────────────────────────────────┐     │
│  │ evict_lru()                                           │     │
│  │   ├─► current_size > max_size?                       │     │
│  │   │     ├─ Yes ─► Pop LRU entries                    │     │
│  │   │     │         └─► Remove from disk               │     │
│  │   │     └─ No  ─► Done                               │     │
│  │   └─► Target: 80% of max_size                        │     │
│  │                                                       │     │
│  │ evict_older_than(duration)                           │     │
│  │   └─► Remove entries older than N days               │     │
│  │                                                       │     │
│  │ cleanup_old_versions(pkg)                            │     │
│  │   └─► Keep only latest version per package           │     │
│  └──────────────────────────────────────────────────────┘     │
│                                                                 │
│  Integrity:                                                    │
│  ┌──────────────────────────────────────────────────────┐     │
│  │ verify(pkg, ver, sha256)                             │     │
│  │   ├─► Calculate actual hash                          │     │
│  │   └─► Compare with expected                          │     │
│  │                                                       │     │
│  │ calculate_hash(pkg, ver)                             │     │
│  │   └─► Streaming SHA-256 calculation                  │     │
│  └──────────────────────────────────────────────────────┘     │
└────────────────────────────────────────────────────────────────┘
```

## Dependency Resolution Flow

```
┌─────────────────────────────────────────────────────────────┐
│           resolve_dependencies(pkg_id, version)             │
└────────────────────┬────────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 1: Resolve Root Package                              │
│  resolve(pkg_id, version) ──► ResolvedPackage              │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 2: Initialize DependencyGraph                        │
│  graph = DependencyGraph::new(pkg_id, version)             │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 3: BFS Traversal                                     │
│  queue = [(pkg_id, version, depth=0)]                      │
│                                                             │
│  while queue not empty:                                    │
│    (pkg, ver, depth) = queue.pop()                         │
│    │                                                        │
│    ├─► Get PackageMetadata                                 │
│    │                                                        │
│    ├─► For each dependency:                                │
│    │     ├─► Resolve version constraint                    │
│    │     │     └─► resolve_version_constraint()            │
│    │     │                                                  │
│    │     ├─► Add to graph.resolved                         │
│    │     │                                                  │
│    │     └─► Queue for processing                          │
│    │           (if not optional)                           │
│    │                                                        │
│    └─► Add node to graph                                   │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 4: Check for Conflicts                               │
│  conflicts = graph.detect_conflicts()                      │
│                                                             │
│  For each package in graph:                                │
│    ├─► Collect all version constraints                     │
│    ├─► Check if resolved version satisfies all            │
│    └─► If not ─► Add to conflicts list                    │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 5: Return DependencyGraph                            │
│  graph {                                                    │
│    root: DependencyNode,                                   │
│    nodes: HashMap<String, DependencyNode>,                 │
│    resolved: HashMap<String, String>                       │
│  }                                                          │
└────────────────────────────────────────────────────────────┘
```

## Version Resolution Algorithm

```
┌─────────────────────────────────────────────────────────────┐
│      resolve_version_constraint(pkg_id, constraint)        │
└────────────────────┬────────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 1: Get Package Metadata                              │
│  package = get_package(pkg_id)                             │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 2: Handle Special Cases                              │
│  if constraint == "latest" or "*"                          │
│    └─► return package.latest_version                       │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 3: Parse Semver Constraint                           │
│  requirement = VersionReq::parse(constraint)               │
│                                                             │
│  Examples:                                                 │
│  • "1.2.3"    ─► Exact version                             │
│  • "^1.2.0"   ─► Compatible (caret)                        │
│  • "~1.2.0"   ─► Tilde (patch updates)                     │
│  • ">=1.0.0"  ─► Greater than or equal                     │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 4: Collect Candidate Versions                        │
│  versions = package.versions                               │
│    .filter(|v| !v.yanked)  // Exclude yanked              │
│    .map(|v| Version::parse(v))                             │
│    .sort_desc()  // Newest first                           │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 5: Find First Match                                  │
│  for version in versions:                                  │
│    if requirement.matches(version):                        │
│      return version.to_string()                            │
│                                                             │
│  // No match found                                         │
│  return Error("No matching version")                       │
└────────────────────────────────────────────────────────────┘
```

## Topological Sort (Install Order)

```
┌─────────────────────────────────────────────────────────────┐
│            graph.topological_sort()                         │
│            (DFS with cycle detection)                       │
└────────────────────┬────────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Initialize:                                                │
│  • sorted = []                                              │
│  • visited = Set()                                          │
│  • temp_mark = Set() (for cycle detection)                 │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  For each node in graph:                                   │
│    if not visited:                                          │
│      visit(node)                                            │
│                                                             │
│  visit(node):                                               │
│    if node in temp_mark:                                   │
│      └─► Circular dependency! Error                        │
│                                                             │
│    if node in visited:                                     │
│      └─► return (already processed)                        │
│                                                             │
│    temp_mark.add(node)                                     │
│                                                             │
│    for each dependency of node:                            │
│      visit(dependency)  // Recursive                       │
│                                                             │
│    temp_mark.remove(node)                                  │
│    visited.add(node)                                       │
│    sorted.append(node)                                     │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  sorted.reverse()  // Dependencies first                   │
│  return sorted                                              │
│                                                             │
│  Example result:                                           │
│  [base-utils, web-framework, my-app]                       │
│   ^install first    ^deps        ^root (install last)      │
└────────────────────────────────────────────────────────────┘
```

## LRU Eviction Strategy

```
┌─────────────────────────────────────────────────────────────┐
│                    evict_lru()                              │
└────────────────────┬────────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Check current size:                                        │
│  current_size = cache.current_size.load()                  │
│  max_size = cache.max_size                                 │
│                                                             │
│  if current_size <= max_size:                              │
│    return []  // No eviction needed                        │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Calculate target size:                                    │
│  target_size = max_size * 0.8  // 80% of max              │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Evict LRU entries:                                        │
│  evicted = []                                              │
│                                                             │
│  while current_size > target_size:                         │
│    ├─► entry = lru.pop_lru()  // O(1) operation           │
│    │                                                        │
│    ├─► Remove from filesystem:                             │
│    │     fs::remove_dir_all(entry.path)                   │
│    │                                                        │
│    ├─► Update current_size:                                │
│    │     current_size -= entry.size                        │
│    │                                                        │
│    └─► evicted.push(entry.key)                            │
│                                                             │
│  return evicted                                            │
└────────────────────────────────────────────────────────────┘

LRU Cache Structure (using lru crate):
┌──────────────────────────────────────┐
│  Most Recent                         │
│  ┌────────┐   ┌────────┐   ┌──────┐ │
│  │ Entry1 │◄─►│ Entry2 │◄─►│ ... │ │
│  └────────┘   └────────┘   └──────┘ │
│                                      │
│  Least Recent (evict first)         │
│  ┌────────┐                         │
│  │ EntryN │ ◄── pop_lru() here      │
│  └────────┘                         │
└──────────────────────────────────────┘
```

## Data Flow: Install Command

```
User: ggen marketplace install my-package@^1.0.0
│
v
┌────────────────────────────────────────────────────────────┐
│  CLI Command: marketplace/install.rs                       │
│  • Parse args: package="my-package", version="^1.0.0"      │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Domain: install_package(options)                          │
│  Step 1: Check cache                                       │
│    cache.read("my-package", "1.2.3")                       │
│    ├─ Hit  ─► Skip download                                │
│    └─ Miss ─► Continue                                     │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 2: Resolve version                                   │
│    registry.resolve("my-package", "^1.0.0")                │
│    └─► Resolves to: "1.2.3"                                │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 3: Resolve dependencies (if --no-deps not set)      │
│    graph = registry.resolve_dependencies("my-package")     │
│    order = graph.topological_sort()                        │
│    └─► ["dep1@1.0.0", "dep2@2.1.0", "my-package@1.2.3"]   │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 4: Download & cache (in order)                      │
│    for pkg in order:                                       │
│      ├─► Download from git_url at git_rev                 │
│      ├─► Verify SHA-256                                    │
│      └─► cache.write(pkg_id, version, content)            │
└────────────────────┬───────────────────────────────────────┘
                     │
                     v
┌────────────────────────────────────────────────────────────┐
│  Step 5: Install to target directory                       │
│    Copy from cache to project directory                    │
│    ├─► Merge templates                                     │
│    ├─► Update lockfile                                     │
│    └─► Success!                                            │
└────────────────────────────────────────────────────────────┘
```

---

**Legend**:
- `─►` : Data flow
- `◄─►`: Bidirectional relationship
- `├─` : Branch point
- `└─` : Terminal branch
