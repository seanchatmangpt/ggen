# Template Reference Guide

Quick lookup for template variables, filters, and common patterns.

## C4 Level 1 (System Context)

### Input Variables
```tera
systems: Array of {
  actor_label: String,
  system_label: String,
  rel_type: String (uses|calls|manages),
  target_system: String
}
system_name: String
mermaid_direction: String (TB|LR)
```

### Key Patterns
```tera
{% comment %}Iterate and deduplicate actors{% endcomment %}
{% set seen_actors = [] %}
{% for row in systems %}
  {% if row.actor_label and row.actor_label not in seen_actors %}
    {% set _ = seen_actors | push(value=row.actor_label) %}
    ACT_{{ loop.index }}["{{ row.actor_label }}"]:::actor
  {% endif %}
{% endfor %}
```

### Output Format
- Mermaid flowchart (graph TB/LR)
- Actor subgraph
- System subgraph
- External subgraph
- Relationship arrows with labels

---

## C4 Level 2 (Containers)

### Input Variables
```tera
containers: Array of {
  system_label: String,
  container_label: String,
  container_type: String (Service|Database|Cache|Queue|Storage),
  dataflow_label: String,
  target_container: String
}
system_label: String
```

### Container Type Classification
```tera
{% if "Service" in container_row.container_type %}
  :::service
{% elif "Database" in container_row.container_type %}
  :::database
{% elif "Cache" in container_row.container_type %}
  :::cache
{% elif "Queue" in container_row.container_type %}
  :::queue
{% elif "Storage" in container_row.container_type %}
  :::storage
{% endif %}
```

### Style Classes
```css
service    fill:#438dd5,stroke:#3c7fc0,color:#fff
database   fill:#c25c2c,stroke:#b73e1a,color:#fff
cache      fill:#ff9900,stroke:#ff6600,color:#fff
queue      fill:#9370db,stroke:#6a4c93,color:#fff
storage    fill:#4a90e2,stroke:#2e5c8a,color:#fff
```

---

## C4 Level 3 (Components & FSM)

### Input Variables
```tera
components: Array of {
  container_label: String,
  component_label: String,
  component_type: String,
  state_from: String,
  state_to: String,
  transition_label: String
}
container_label: String
```

### FSM States
```
IDLE ──[signal_received]──> SENSE
SENSE ──[metrics_ready]──> ANALYZE
ANALYZE ──[analysis_complete]──> DECIDE
DECIDE ──[policy_match]──> EXECUTE
EXECUTE ──[action_done]──> OBSERVE
OBSERVE ──[success]──> IDLE
OBSERVE ──[failure]──> ERROR
ERROR ──[recovery_complete]──> IDLE
```

### State Color Classes
```
idle       #4a90e2  (Blue)   - Waiting state
analyzing  #ff9900  (Orange) - Active processing
deciding   #f5a623  (Gold)   - Decision logic
executing  #7ed321  (Green)  - Action execution
observing  #9013fe  (Purple) - Monitoring
error      #d0021b  (Red)    - Error handling
```

---

## C4 Level 4 (Infrastructure)

### Input Variables
```tera
infrastructure: Array of {
  region: String,
  zone: String,
  service_label: String,
  service_type: String,
  protocol: String (HTTP|TCP|UDP),
  port: Integer,
  scaling_policy: String
}
gcp_project_id: String
regions: Array<String>
```

### Regional Topology
```tera
{% for region in regions %}
  subgraph region_{{ loop.index0}}["{{ region }}"]
    {service definitions}
  end
{% endfor %}
```

### GCP Service Mapping
```
Cloud Run      ☁️  compute
GKE            ☸️  compute
Cloud SQL      🗄️  database
Firestore      📄  database
Memorystore    ⚡  cache
Cloud Storage  💾  storage
Pub/Sub        📬  messaging
Cloud Monitor  ☁️  monitoring
```

---

## SKU Catalog

### Input Variables
```tera
skus: Array of {
  sku_name: String,
  sku_tier: String (Basic|Standard|Premium),
  signal_type: String (CPU|Memory|Latency|ErrorRate|...),
  action_type: String (ScaleUp|ScaleDown|Restart|...),
  max_instances: Integer,
  cost_per_hour: Float
}
currency: String (USD|EUR|GBP)
tier_order: Array<String>
```

### Tier Organization
```tera
{% for tier in tier_order %}
  {% set tier_skus = skus | filter(attribute="sku_tier", value=tier) %}
  {% if tier_skus | length > 0 %}
    {tier markdown content}
  {% endif %}
{% endfor %}
```

### Signal-Action Mapping
```tera
{% for signal_action in tier_signals %}
  | {{ signal_action.signal }} | {{ signal_action.action }} |
{% endfor %}
```

### Pricing Calculation
```tera
{% set base_cost = sku.cost_per_hour %}
Monthly Bill = (730 hours × ${{ base_cost }})
             + (Requests × $0.0000001)
             + (Data Transfer × $0.12/GB)
             + (Storage × $0.020/GB-month)
```

---

## Kubernetes Deployment

### Input Variables
```toml
[kubernetes]
namespace = "autonomic-system"
image_registry = "gcr.io/ggen-project"
image_pull_policy = "IfNotPresent"
cpu_request = "250m"
cpu_limit = "500m"
memory_request = "256Mi"
memory_limit = "512Mi"
replicas = 2
min_replicas = 2
max_replicas = 10

[gcp]
project_id = "ggen-autonomics"
region = "us-central1"
zones = ["us-central1-a", "us-central1-b", "us-central1-c"]

[observability]
log_level = "info"
json_logging = true
```

### ConfigMap Keys
```yaml
# Policy pack with signal/action rules
policy.yaml:
  policies:
    - name: "cpu-autoscale-policy"
      signals: [{type, metric, threshold, window}]
      actions: [{type, target, increment}]

# Scaling policy thresholds
scaling-rules.yaml:
  scaling_policies:
    conservative: {cpu_threshold: 0.80, ...}
    standard: {cpu_threshold: 0.75, ...}
    aggressive: {cpu_threshold: 0.70, ...}
```

### Probe Configuration
```yaml
livenessProbe:        # Is pod alive?
  path: /health/live
  period: 10s
  failure_threshold: 3

readinessProbe:       # Ready for traffic?
  path: /health/ready
  period: 5s
  failure_threshold: 2

startupProbe:         # Finished initializing?
  path: /health/startup
  period: 5s
  failure_threshold: 30
```

### HPA Scaling Rules
```yaml
metrics:
  - type: Resource
    resource:
      name: cpu
      target: 70%
  - type: Resource
    resource:
      name: memory
      target: 75%

behavior:
  scaleUp:
    stabilization: 60s
    policies:
      - type: Percent     # Scale up by 100%
        value: 100
      - type: Pods        # Add 2 pods
        value: 2
  scaleDown:
    stabilization: 300s
    policies:
      - type: Percent
        value: 50         # Scale down by 50%
```

---

## Common Tera Filters

### String Filters
```tera
{{ value | upper }}              # UPPERCASE
{{ value | lower }}              # lowercase
{{ value | title }}              # Title Case
{{ value | replace(from="a", to="b") }}
{{ value | truncate(length=30) }}
{{ value | slugify }}            # my-slug-format
{{ value | trim }}               # Remove whitespace
```

### Numeric Filters
```tera
{{ value | round(precision=2) }}
{{ value | abs }}
{{ value | default(value=0) }}
```

### Array/Collection Filters
```tera
{% set filtered = items | filter(attribute="type", value="Service") %}
{{ items | length }}
{% for item in items | reverse %}...{% endfor %}
{% for item in items | sort(attribute="name") %}...{% endfor %}
```

### Conditional & Looping
```tera
{% if condition %}...{% endif %}
{% for item in collection %}
  {% if loop.first %}First!{% endif %}
  {{ loop.index }}           # 1-based index
  {{ loop.index0 }}          # 0-based index
  {{ loop.last }}
  Item: {{ item }}
{% endfor %}
```

---

## Escape & Safe Rendering

### HTML Escaping
```tera
{{ value | escape }}            # &lt;, &gt;, &quot;, &amp;
{{ value | safe }}              # Raw (no escaping)
```

### Markdown Safe Characters
```tera
{{ label | replace(from="|", to="\\|") }}  # Escape pipe
{{ label | replace(from="[", to="\\[") }}  # Escape brackets
{{ value | replace(from="#", to="\\#") }}  # Escape heading
```

### YAML Safe Rendering
```tera
{% if value | contains(s="\"") or value | contains(s="\n") %}
  "{{ value | replace(from="\"", to="\\\"") }}"
{% else %}
  {{ value }}
{% endif %}
```

---

## Template Comments

### Multi-line Comments (Hidden)
```tera
{% comment %}
This is a comment block.
It explains what the template does.
It won't appear in the output.
{% endcomment %}
```

### Single-line Comments (Hidden)
```tera
{# This is a single-line comment #}
```

### Documentation Comments
```tera
{% comment %}
Input Variables:
  - items: Array of {id, name, type}
  - title: String (diagram title)

Output Format:
  Mermaid flowchart with nodes and edges

Example Usage:
  {% for item in items %}
    {{ item.id }}["{{ item.name }}"]
  {% endfor %}
{% endcomment %}
```

---

## Special Variables

### Built-in Variables
```tera
now()               # Current timestamp function
loop                # Loop metadata object:
  - loop.index      # 1-based iteration number
  - loop.index0     # 0-based iteration number
  - loop.first      # true on first iteration
  - loop.last       # true on last iteration
  - loop.length     # Total items in loop
```

### Custom Variables (Pass from ggen.toml)
```toml
[template_context]
organization = "Acme Corp"
environment = "production"
version = "1.0.0"
```

Access in template:
```tera
{{ organization }}
{{ environment }}
{{ version }}
```

---

## Deterministic Output Patterns

### Always Use ORDER BY
```sparql
SELECT ?name ?id WHERE {
  ...
}
ORDER BY ?name ?id    # Ensure consistent order
```

### Consistent Naming
```tera
{{ loop.index }}        # Use for IDs (1, 2, 3...)
{{ loop.index0 }}       # For 0-based indexing
{{ item_name | slugify }} # For safe identifiers
```

### Avoid Non-Deterministic Functions
```tera
{% comment %}DON'T: These produce different results each run{% endcomment %}
{{ random() }}          # ❌ Don't use
{{ shuffle(array) }}    # ❌ Don't use

{% comment %}DO: Use explicit sorting/indexing{% endcomment %}
{% for item in items | sort(attribute="name") %}
  {{ loop.index }}
{% endfor %}
```

### Timestamp Pattern
```tera
{% comment %}Add generation metadata but keep logic deterministic{% endcomment %}
Generated: {{ now() | date(format="%Y-%m-%d %H:%M:%S UTC") }}

{% comment %}Keep the actual logic deterministic{% endcomment %}
{% for item in items | sort(attribute="name") %}
  {deterministic output}
{% endfor %}
```

---

## Debugging & Troubleshooting

### Check Variable Availability
```tera
{% if systems is defined %}
  Systems data available: {{ systems | length }} items
{% else %}
  ERROR: systems variable not defined!
{% endif %}
```

### Debug Output
```tera
{# Include diagnostic comments in output #}
<!-- DEBUG: Processing {{ items | length }} items -->
<!-- DEBUG: Tier filter value: {{ selected_tier }} -->
```

### Empty Fallbacks
```tera
{{ value | default(value="(no value)") }}
{{ items | default(value=[]) | length }}
```

### Type Checking
```tera
{% if value is string %}
  Is string: {{ value }}
{% elif value is number %}
  Is number: {{ value }}
{% elif value is iterable %}
  Is array/list: {{ value | length }} items
{% endif %}
```

---

## Performance Tips

### 1. Pre-compute in Loops
```tera
{% comment %}Bad: Compute inside loop{% endcomment %}
{% for item in items %}
  {% set total = items | length %}
{% endfor %}

{% comment %}Good: Compute once before loop{% endcomment %}
{% set total = items | length %}
{% for item in items %}
  Item {{ loop.index }} of {{ total }}
{% endfor %}
```

### 2. Filter Early
```tera
{% comment %}Bad: Filter inside inner loop{% endcomment %}
{% for system in systems %}
  {% for container in system.containers | filter(...) %}
  {% endfor %}
{% endfor %}

{% comment %}Good: Pre-filter{% endcomment %}
{% set filtered_containers = containers | filter(...) %}
{% for system in systems %}
  {% for container in filtered_containers %}
  {% endfor %}
{% endfor %}
```

### 3. Use Set for Deduplication
```tera
{% set seen = [] %}
{% for item in items %}
  {% if item.name not in seen %}
    {% set _ = seen | push(value=item.name) %}
    {process item}
  {% endif %}
{% endfor %}
```

---

## Cross-Template Links

### Linking Diagrams in Markdown
```markdown
# Architecture Overview

1. [System Context](c4-level1-context.mmd)
2. [Container Diagram](c4-level2-containers.mmd)
3. [Component Diagram](c4-level3-components.mmd)
4. [Deployment Diagram](c4-level4-deployment.mmd)

## Catalog & Pricing

See [SKU Catalog](sku-catalog.md) for tier details.

## Deployment

Deploy using [Kubernetes manifests](deployment-gke.yaml).
```

### Referencing in Kubernetes
```yaml
metadata:
  annotations:
    architecture-diagram: "https://github.com/org/repo/blob/main/generated/c4-level3-components.mmd"
    catalog-url: "https://github.com/org/repo/blob/main/generated/sku-catalog.md"
    specification: "https://github.com/org/repo/tree/main/.specify/specs"
```

---

**Quick Reference Version:** 1.0.0
**Last Updated:** 2026-01-25
