<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Evidence Graph Mining: JSON Schemas](#evidence-graph-mining-json-schemas)
  - [1. RepoDescriptor](#1-repodescriptor)
  - [2. FileDescriptor](#2-filedescriptor)
  - [3. Excerpt](#3-excerpt)
  - [4. EvidenceNode](#4-evidencenode)
  - [5. ConceptNode](#5-conceptnode)
  - [6. SystemNode](#6-systemnode)
  - [7. Evidence Graph (Main Output)](#7-evidence-graph-main-output)
  - [8. Concept Coverage Report](#8-concept-coverage-report)
  - [9. Concept Gaps Report](#9-concept-gaps-report)
  - [10. Concept Matcher Configuration](#10-concept-matcher-configuration)
  - [Usage](#usage)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Evidence Graph Mining: JSON Schemas

This document defines the machine-readable schemas for the Evidence Graph output artifacts.

## 1. RepoDescriptor

Metadata for discovered repositories.

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "RepoDescriptor",
  "type": "object",
  "properties": {
    "repo_id": {
      "type": "string",
      "description": "Unique repository identifier (e.g., 'knhk', 'mu-kernel')"
    },
    "origin": {
      "type": "string",
      "description": "Git URL or internal identifier"
    },
    "likely_domains": {
      "type": "array",
      "items": { "type": "string" },
      "description": "Inferred problem domains (e.g., 'knowledge_graph', 'timing_kernel')"
    },
    "priority": {
      "type": "number",
      "minimum": 0,
      "maximum": 1,
      "description": "Scanning priority (0.0-1.0)"
    }
  },
  "required": ["repo_id", "origin", "likely_domains", "priority"]
}
```

## 2. FileDescriptor

Metadata for enumerated files per repository.

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "FileDescriptor",
  "type": "object",
  "properties": {
    "repo_id": {
      "type": "string"
    },
    "path": {
      "type": "string",
      "description": "Relative path within repo"
    },
    "kind": {
      "type": "string",
      "enum": ["code", "doc", "example", "test", "config"],
      "description": "File classification"
    },
    "language": {
      "type": "string",
      "description": "Language or format (rust, python, markdown, toml, json, etc.)"
    }
  },
  "required": ["repo_id", "path", "kind", "language"]
}
```

## 3. Excerpt

Extracted region from a file with concept match scoring.

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "Excerpt",
  "type": "object",
  "properties": {
    "repo_id": {
      "type": "string"
    },
    "path": {
      "type": "string"
    },
    "start_line": {
      "type": "integer",
      "minimum": 1
    },
    "end_line": {
      "type": "integer",
      "minimum": 1
    },
    "concept_id": {
      "type": "string",
      "description": "Matched concept (e.g., 'C_GRAPH_UNIVERSE_PRIMARY')"
    },
    "local_score": {
      "type": "number",
      "minimum": 0,
      "maximum": 1,
      "description": "Concept match score for this region"
    },
    "raw_text": {
      "type": "string",
      "description": "Excerpt content for human/LLM review"
    }
  },
  "required": ["repo_id", "path", "start_line", "end_line", "concept_id", "local_score"]
}
```

## 4. EvidenceNode

Synthesized evidence linking a source region to a concept.

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "EvidenceNode",
  "type": "object",
  "properties": {
    "evidence_id": {
      "type": "string",
      "description": "Unique identifier (format: 'repo_id.filename.linerange')"
    },
    "repo_id": {
      "type": "string"
    },
    "path": {
      "type": "string"
    },
    "lines": {
      "type": "string",
      "description": "Line range (format: 'start-end')"
    },
    "concept_id": {
      "type": "string"
    },
    "support_type": {
      "type": "string",
      "enum": ["direct", "indirect", "contextual"],
      "description": "direct: defines/implements concept; indirect: references/uses; contextual: related context"
    },
    "claim_summary": {
      "type": "string",
      "description": "Neutral, concise summary of what this evidence establishes"
    },
    "key_phrases": {
      "type": "array",
      "items": { "type": "string" },
      "description": "Tokens/phrases that triggered the match"
    },
    "strength": {
      "type": "number",
      "minimum": 0,
      "maximum": 1,
      "description": "Confidence that this evidence supports the concept (0.0-1.0)"
    }
  },
  "required": ["evidence_id", "repo_id", "path", "lines", "concept_id", "support_type", "claim_summary", "key_phrases", "strength"]
}
```

## 5. ConceptNode

Represents a core claim about the graph-universe thesis.

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "ConceptNode",
  "type": "object",
  "properties": {
    "type": {
      "type": "string",
      "enum": ["concept"],
      "const": "concept"
    },
    "id": {
      "type": "string",
      "description": "Concept identifier (e.g., 'C_GRAPH_UNIVERSE_PRIMARY')"
    },
    "category": {
      "type": "string",
      "enum": ["universe", "kernel", "knowledge", "verification", "interface"],
      "description": "Concept domain"
    },
    "description": {
      "type": "string"
    }
  },
  "required": ["type", "id", "category", "description"]
}
```

## 6. SystemNode

Represents an organ system (KNHK, μ-kernel, CTT, etc.).

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "SystemNode",
  "type": "object",
  "properties": {
    "type": {
      "type": "string",
      "enum": ["system"],
      "const": "system"
    },
    "id": {
      "type": "string",
      "description": "System identifier (e.g., 'mu-kernel')"
    },
    "role": {
      "type": "string",
      "description": "System's role in the architecture"
    }
  },
  "required": ["type", "id", "role"]
}
```

## 7. Evidence Graph (Main Output)

Complete graph with nodes and edges.

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "EvidenceGraph",
  "type": "object",
  "properties": {
    "version": {
      "type": "string",
      "description": "Schema version (e.g., '1.0')"
    },
    "generated_at": {
      "type": "string",
      "format": "date-time"
    },
    "nodes": {
      "type": "object",
      "properties": {
        "concepts": {
          "type": "array",
          "items": { "$ref": "#/definitions/ConceptNode" }
        },
        "evidence": {
          "type": "array",
          "items": { "$ref": "#/definitions/EvidenceNode" }
        },
        "systems": {
          "type": "array",
          "items": { "$ref": "#/definitions/SystemNode" }
        }
      },
      "required": ["concepts", "evidence", "systems"]
    },
    "edges": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "from": {
            "type": "string",
            "description": "Source node ID"
          },
          "to": {
            "type": "string",
            "description": "Target node ID"
          },
          "kind": {
            "type": "string",
            "enum": ["supports", "implements", "composed_with"],
            "description": "Edge relationship type"
          },
          "weight": {
            "type": "number",
            "minimum": 0,
            "maximum": 1,
            "description": "Edge strength/confidence"
          }
        },
        "required": ["from", "to", "kind", "weight"]
      }
    }
  },
  "required": ["version", "generated_at", "nodes", "edges"],
  "definitions": {
    "ConceptNode": {
      "$ref": "#/definitions/ConceptNode"
    },
    "EvidenceNode": {
      "$ref": "#/definitions/EvidenceNode"
    },
    "SystemNode": {
      "$ref": "#/definitions/SystemNode"
    }
  }
}
```

## 8. Concept Coverage Report

Per-concept evidence summary.

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "ConceptCoverageReport",
  "type": "object",
  "additionalProperties": {
    "type": "object",
    "properties": {
      "evidence_count": {
        "type": "integer",
        "minimum": 0,
        "description": "Total evidence nodes supporting this concept"
      },
      "systems": {
        "type": "array",
        "items": { "type": "string" },
        "description": "Systems that implement/contain evidence for this concept"
      },
      "min_strength": {
        "type": "number",
        "minimum": 0,
        "maximum": 1
      },
      "max_strength": {
        "type": "number",
        "minimum": 0,
        "maximum": 1
      },
      "avg_strength": {
        "type": "number",
        "minimum": 0,
        "maximum": 1
      }
    },
    "required": ["evidence_count", "systems", "min_strength", "max_strength", "avg_strength"]
  }
}
```

## 9. Concept Gaps Report

Missing evidence for concepts.

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "ConceptGapsReport",
  "type": "object",
  "properties": {
    "gaps": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "concept_id": {
            "type": "string"
          },
          "category": {
            "type": "string"
          },
          "description": {
            "type": "string"
          },
          "evidence_count": {
            "type": "integer",
            "minimum": 0
          },
          "max_strength": {
            "type": "number",
            "minimum": 0,
            "maximum": 1
          },
          "status": {
            "type": "string",
            "enum": ["no_evidence", "weak_evidence", "inconsistent"],
            "description": "no_evidence: 0 findings; weak_evidence: max_strength < 0.5; inconsistent: mixed signals"
          },
          "investigation_notes": {
            "type": "string",
            "description": "Optional notes on why evidence may be sparse"
          }
        },
        "required": ["concept_id", "category", "description", "evidence_count", "max_strength", "status"]
      }
    }
  },
  "required": ["gaps"]
}
```

## 10. Concept Matcher Configuration

Pattern rules for concept identification (for agents).

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "ConceptMatcherConfig",
  "type": "array",
  "items": {
    "type": "object",
    "properties": {
      "concept_id": {
        "type": "string"
      },
      "category": {
        "type": "string"
      },
      "must_include_any": {
        "type": "array",
        "items": { "type": "string" },
        "description": "Must find at least one of these tokens"
      },
      "boost_if_present": {
        "type": "array",
        "items": { "type": "string" },
        "description": "Boost score if these tokens present"
      },
      "exclude": {
        "type": "array",
        "items": { "type": "string" },
        "description": "Penalty if these tokens present"
      },
      "base_weight": {
        "type": "number",
        "minimum": 0,
        "maximum": 1,
        "description": "Base score multiplier"
      }
    },
    "required": ["concept_id", "category", "must_include_any", "boost_if_present", "exclude"]
  }
}
```

---

## Usage

Agents should:
1. Use **RepoDescriptor** and **FileDescriptor** to catalog discovery.
2. Generate **Excerpt** objects from file scanning.
3. Synthesize **EvidenceNode** objects from excerpts.
4. Build **EvidenceGraph** with ConceptNode, SystemNode, EvidenceNode, and edge definitions.
5. Output **concept_coverage.json** and **concept_gaps.json** for analysis.

The Evidence Graph is then ready for:
- Validation (does it support the thesis?)
- Visualization (as a graph database)
- Documentation generation (as LaTeX/markdown for academic paper)
- Refinement (identifying gaps and targeted evidence collection)
