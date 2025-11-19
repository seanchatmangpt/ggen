# Tutorial: Building Your Own Ontology Pack

**Goal:** Create, test, and package a custom ontology pack.

**What you'll learn:**
- Pack structure and metadata format
- Writing RDF ontology definitions
- Creating custom templates
- Testing pack generation
- Packaging for distribution

**Prerequisites:**
- Completed [Getting Started](01-getting-started.md)
- Basic understanding of RDF/OWL
- Familiarity with template languages

**Time:** 45 minutes

---

## Use Case: Company Internal Ontology

We'll create a pack for a fictional company's internal data model:
- **Domain:** Project management
- **Types:** Project, Task, Team, Milestone
- **Output:** TypeScript and Python code

---

## Step 1: Create Pack Structure

```bash
mkdir -p my-company-ontology/{ontology,templates,tests}
cd my-company-ontology
```

Create the basic structure:

```
my-company-ontology/
├── pack.yaml                 # Pack metadata
├── ontology/
│   └── company.ttl          # RDF ontology definition
├── templates/
│   ├── typescript/
│   │   ├── template.hbs
│   │   └── config.yaml
│   └── python/
│       ├── template.hbs
│       └── config.yaml
└── tests/
    ├── typescript/
    └── python/
```

---

## Step 2: Define Pack Metadata

Create `pack.yaml`:

```yaml
name: "my-company-ontology"
version: "1.0.0"
description: "Internal project management ontology for MyCompany"
author: "MyCompany Engineering"
license: "MIT"
homepage: "https://github.com/mycompany/ontology-pack"

ontology:
  format: "turtle"
  file: "ontology/company.ttl"
  namespace: "http://mycompany.com/ontology#"
  prefix: "company"

templates:
  - name: "typescript"
    description: "TypeScript interfaces and types"
    output_extension: ".ts"
    config_file: "templates/typescript/config.yaml"

  - name: "python"
    description: "Python dataclasses"
    output_extension: ".py"
    config_file: "templates/python/config.yaml"

dependencies: []

metadata:
  tags:
    - "project-management"
    - "internal"
    - "typescript"
    - "python"

  statistics:
    classes: 4
    properties: 15

  compatibility:
    ggen_version: ">=1.0.0"
```

---

## Step 3: Define the Ontology

Create `ontology/company.ttl` in Turtle format:

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix company: <http://mycompany.com/ontology#> .

# Ontology metadata
company: a owl:Ontology ;
    rdfs:label "MyCompany Project Management Ontology" ;
    rdfs:comment "Ontology for internal project management system" ;
    owl:versionInfo "1.0.0" .

# Classes

company:Project a owl:Class ;
    rdfs:label "Project" ;
    rdfs:comment "A project with tasks, team members, and milestones" .

company:Task a owl:Class ;
    rdfs:label "Task" ;
    rdfs:comment "A unit of work within a project" .

company:Team a owl:Class ;
    rdfs:label "Team" ;
    rdfs:comment "A group of people working together" .

company:Milestone a owl:Class ;
    rdfs:label "Milestone" ;
    rdfs:comment "A significant checkpoint in a project" .

# Properties for Project

company:projectId a owl:DatatypeProperty ;
    rdfs:label "projectId" ;
    rdfs:domain company:Project ;
    rdfs:range xsd:string ;
    rdfs:comment "Unique identifier for the project" .

company:projectName a owl:DatatypeProperty ;
    rdfs:label "projectName" ;
    rdfs:domain company:Project ;
    rdfs:range xsd:string ;
    rdfs:comment "Human-readable name of the project" .

company:startDate a owl:DatatypeProperty ;
    rdfs:label "startDate" ;
    rdfs:domain company:Project ;
    rdfs:range xsd:dateTime ;
    rdfs:comment "Project start date" .

company:endDate a owl:DatatypeProperty ;
    rdfs:label "endDate" ;
    rdfs:domain company:Project ;
    rdfs:range xsd:dateTime ;
    rdfs:comment "Project end date" .

company:hasTasks a owl:ObjectProperty ;
    rdfs:label "hasTasks" ;
    rdfs:domain company:Project ;
    rdfs:range company:Task ;
    rdfs:comment "Tasks belonging to this project" .

company:hasTeam a owl:ObjectProperty ;
    rdfs:label "hasTeam" ;
    rdfs:domain company:Project ;
    rdfs:range company:Team ;
    rdfs:comment "Team assigned to this project" .

# Properties for Task

company:taskId a owl:DatatypeProperty ;
    rdfs:label "taskId" ;
    rdfs:domain company:Task ;
    rdfs:range xsd:string .

company:taskTitle a owl:DatatypeProperty ;
    rdfs:label "taskTitle" ;
    rdfs:domain company:Task ;
    rdfs:range xsd:string .

company:status a owl:DatatypeProperty ;
    rdfs:label "status" ;
    rdfs:domain company:Task ;
    rdfs:range xsd:string ;
    rdfs:comment "Task status: TODO, IN_PROGRESS, DONE" .

company:assignedTo a owl:DatatypeProperty ;
    rdfs:label "assignedTo" ;
    rdfs:domain company:Task ;
    rdfs:range xsd:string ;
    rdfs:comment "Person assigned to this task" .

# Properties for Team

company:teamName a owl:DatatypeProperty ;
    rdfs:label "teamName" ;
    rdfs:domain company:Team ;
    rdfs:range xsd:string .

company:members a owl:DatatypeProperty ;
    rdfs:label "members" ;
    rdfs:domain company:Team ;
    rdfs:range xsd:string ;
    rdfs:comment "Team member names (array)" .

# Properties for Milestone

company:milestoneName a owl:DatatypeProperty ;
    rdfs:label "milestoneName" ;
    rdfs:domain company:Milestone ;
    rdfs:range xsd:string .

company:dueDate a owl:DatatypeProperty ;
    rdfs:label "dueDate" ;
    rdfs:domain company:Milestone ;
    rdfs:range xsd:dateTime .

company:isCompleted a owl:DatatypeProperty ;
    rdfs:label "isCompleted" ;
    rdfs:domain company:Milestone ;
    rdfs:range xsd:boolean .
```

---

## Step 4: Create TypeScript Template

Create `templates/typescript/template.hbs`:

```handlebars
/**
 * Generated from {{ontology.name}} ontology
 * Version: {{ontology.version}}
 *
 * @generated
 * @see {{ontology.namespace}}
 */

{{#each classes}}
/**
 * {{comment}}
 * @see {{uri}}
 */
export interface {{name}} {
  "@type": "{{name}}";

  {{#each properties}}
  /**
   * {{comment}}
   * @type {{#if isArray}}{{dataType}}[]{{else}}{{dataType}}{{/if}}
   */
  {{propertyName}}{{#if optional}}?{{/if}}: {{#if isArray}}{{tsType}}[]{{else}}{{tsType}}{{/if}};

  {{/each}}
}

{{/each}}

// Type guards
{{#each classes}}
export function is{{name}}(obj: any): obj is {{name}} {
  return obj && obj["@type"] === "{{name}}";
}

{{/each}}

// Export all types
export type AnyEntity = {{#each classes}}{{name}}{{#unless @last}} | {{/unless}}{{/each}};
```

Create `templates/typescript/config.yaml`:

```yaml
template_engine: "handlebars"

# Map RDF types to TypeScript types
type_mappings:
  "http://www.w3.org/2001/XMLSchema#string": "string"
  "http://www.w3.org/2001/XMLSchema#integer": "number"
  "http://www.w3.org/2001/XMLSchema#boolean": "boolean"
  "http://www.w3.org/2001/XMLSchema#dateTime": "string"  # ISO8601
  "http://www.w3.org/2001/XMLSchema#float": "number"
  "http://www.w3.org/2001/XMLSchema#double": "number"

# Generation options
options:
  include_type_guards: true
  include_validators: false
  strict_null_checks: true

# SPARQL queries to extract data
queries:
  classes: |
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?class ?label ?comment WHERE {
      ?class a owl:Class .
      OPTIONAL { ?class rdfs:label ?label }
      OPTIONAL { ?class rdfs:comment ?comment }
    }

  properties: |
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?property ?domain ?range ?label ?comment WHERE {
      {?property a owl:DatatypeProperty} UNION {?property a owl:ObjectProperty}
      OPTIONAL { ?property rdfs:domain ?domain }
      OPTIONAL { ?property rdfs:range ?range }
      OPTIONAL { ?property rdfs:label ?label }
      OPTIONAL { ?property rdfs:comment ?comment }
    }
```

---

## Step 5: Create Python Template

Create `templates/python/template.hbs`:

```handlebars
"""
Generated from {{ontology.name}} ontology
Version: {{ontology.version}}

@generated
@see {{ontology.namespace}}
"""

from dataclasses import dataclass, field
from typing import Optional, List
from datetime import datetime

{{#each classes}}
@dataclass
class {{name}}:
    """
    {{comment}}

    See: {{uri}}
    """
    type_: str = field(default="{{name}}", init=False)

    {{#each properties}}
    {{propertyName}}: {{#if optional}}Optional[{{/if}}{{#if isArray}}List[{{pythonType}}]{{else}}{{pythonType}}{{/if}}{{#if optional}}]{{/if}} = {{#if isArray}}field(default_factory=list){{else}}None{{/if}}
    {{/each}}

    def to_dict(self) -> dict:
        """Convert to dictionary for JSON serialization"""
        return {
            "@type": self.type_,
            {{#each properties}}
            "{{propertyName}}": self.{{propertyName}},
            {{/each}}
        }

{{/each}}
```

Create `templates/python/config.yaml`:

```yaml
template_engine: "handlebars"

type_mappings:
  "http://www.w3.org/2001/XMLSchema#string": "str"
  "http://www.w3.org/2001/XMLSchema#integer": "int"
  "http://www.w3.org/2001/XMLSchema#boolean": "bool"
  "http://www.w3.org/2001/XMLSchema#dateTime": "datetime"
  "http://www.w3.org/2001/XMLSchema#float": "float"
  "http://www.w3.org/2001/XMLSchema#double": "float"

options:
  use_dataclasses: true
  include_validators: false

queries:
  classes: |
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?class ?label ?comment WHERE {
      ?class a owl:Class .
      OPTIONAL { ?class rdfs:label ?label }
      OPTIONAL { ?class rdfs:comment ?comment }
    }

  properties: |
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?property ?domain ?range ?label ?comment WHERE {
      {?property a owl:DatatypeProperty} UNION {?property a owl:ObjectProperty}
      OPTIONAL { ?property rdfs:domain ?domain }
      OPTIONAL { ?property rdfs:range ?range }
      OPTIONAL { ?property rdfs:label ?label }
      OPTIONAL { ?property rdfs:comment ?comment }
    }
```

---

## Step 6: Test Pack Locally

Test TypeScript generation:

```bash
ggen ontology generate ./my-company-ontology \
  --template typescript \
  --output ./test-output/typescript \
  --verbose
```

Verify generated output in `./test-output/typescript/`:

```typescript
export interface Project {
  "@type": "Project";
  projectId?: string;
  projectName?: string;
  startDate?: string;
  endDate?: string;
  hasTasks?: Task[];
  hasTeam?: Team;
}

export interface Task {
  "@type": "Task";
  taskId?: string;
  taskTitle?: string;
  status?: string;
  assignedTo?: string;
}

// ... etc
```

Test Python generation:

```bash
ggen ontology generate ./my-company-ontology \
  --template python \
  --output ./test-output/python
```

---

## Step 7: Create Unit Tests

Create `tests/typescript/test.ts`:

```typescript
import { Project, Task, Team, isProject } from '../../test-output/typescript';

const project: Project = {
  "@type": "Project",
  projectId: "PROJ-001",
  projectName: "New Website",
  startDate: "2025-01-01T00:00:00Z",
  hasTasks: [
    {
      "@type": "Task",
      taskId: "TASK-001",
      taskTitle: "Design homepage",
      status: "IN_PROGRESS",
      assignedTo: "alice@example.com"
    }
  ]
};

console.assert(isProject(project), "Should be a valid Project");
console.log("✓ TypeScript generation test passed");
```

Run test:

```bash
npx ts-node tests/typescript/test.ts
```

---

## Step 8: Package for Distribution

Create `.ggenignore`:

```
tests/
test-output/
*.log
.DS_Store
```

Package the pack:

```bash
ggen ontology pack ./my-company-ontology
```

**Expected output:**
```
Packaging ontology pack...
✓ Validated pack.yaml
✓ Validated ontology file
✓ Validated templates
✓ Created: my-company-ontology-1.0.0.gpack

Package contents:
  - pack.yaml
  - ontology/company.ttl
  - templates/typescript/
  - templates/python/
  - README.md

Size: 15.2 KB
```

---

## Step 9: Install and Test Packed Version

Install from local `.gpack`:

```bash
ggen ontology install ./my-company-ontology-1.0.0.gpack
```

Verify:

```bash
ggen ontology list
```

Generate from installed pack:

```bash
ggen ontology generate my-company-ontology \
  --template typescript \
  --output ./final-test
```

---

## What You Learned

- ✅ Pack structure and metadata format
- ✅ Writing ontologies in Turtle format
- ✅ Creating Handlebars templates
- ✅ Configuring SPARQL queries for extraction
- ✅ Testing pack generation locally
- ✅ Packaging for distribution

---

## Next Steps

- **Publish to marketplace:** [Tutorial: Publishing to Marketplace](04-publishing-pack.md)
- **Advanced templates:** [How to: Customize Code Generation](../how-to/customize-generation.md)
- **Pack composition:** [How to: Compose Multiple Ontologies](../how-to/compose-ontologies.md)

---

## Best Practices

1. **Version properly:** Use semantic versioning (major.minor.patch)
2. **Document thoroughly:** Include README.md with examples
3. **Test all templates:** Verify output compiles/runs
4. **Use SPARQL carefully:** Test queries against your ontology
5. **Provide examples:** Include example usage in tests/

---

## Troubleshooting

**Template not rendering?**
- Check Handlebars syntax
- Verify SPARQL queries return data
- Use `--verbose` flag for debugging

**Type mappings wrong?**
- Check `type_mappings` in config.yaml
- Verify RDF range declarations in ontology

**Pack validation fails?**
- Ensure pack.yaml follows schema
- Validate ontology with RDF validator
- Check file paths are relative to pack root
