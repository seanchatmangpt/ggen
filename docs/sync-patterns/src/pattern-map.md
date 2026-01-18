# Pattern Map

> *"Each pattern sits in a web of connections, where the whole is more than the sum of its parts."*

---

## Visual Map

```
                        ┌─────────────────────────────────┐
                        │  THE SINGLE COMMAND (1)         │
                        │  The unified entry point        │
                        └───────────────┬─────────────────┘
                                        │
                    ┌───────────────────┼───────────────────┐
                    │                   │                   │
                    ▼                   ▼                   ▼
        ┌───────────────────┐  ┌───────────────┐  ┌────────────────────┐
        │ MANIFEST AS       │  │ THREE-LAYER   │  │ Flags modify       │
        │ TRUTH (2)         │  │ ARCHITECTURE  │  │ behavior:          │
        │ Configuration     │  │ (3) Structure │  │ --dry-run, --force │
        └─────────┬─────────┘  └───────────────┘  │ --validate-only    │
                  │                               │ --rule, --timeout  │
                  │                               └────────────────────┘
    ┌─────────────┼─────────────────────────────────────┐
    │             │                                     │
    ▼             ▼                                     ▼
┌─────────┐  ┌─────────────────┐  ┌─────────────────────────────────┐
│ [ontology] │  │ [inference]     │  │ [generation]                    │
│ section  │  │ section         │  │ section                         │
└────┬────┘  └────────┬────────┘  └──────────────┬──────────────────┘
     │               │                          │
     ▼               ▼                          ▼
┌──────────────┐  ┌──────────────────┐  ┌────────────────────────┐
│ ONTOLOGY     │  │ INFERENCE        │  │ GENERATION RULES (6)   │
│ LOADING (4)  │  │ ENRICHMENT (5)   │  │ Query → Template → Code│
│ TTL → Graph  │  │ CONSTRUCT →      │  └───────────┬────────────┘
└──────┬───────┘  │ Materialize      │              │
       │          └────────┬─────────┘              │
       │                   │                        ▼
       └───────────────────┴─────────►  ┌──────────────────────┐
                                        │ TEMPLATE RENDERING   │
                  ENRICHED GRAPH        │ (7) Variables → Code │
                                        └──────────┬───────────┘
                                                   │
                                                   ▼
                                        ┌──────────────────────┐
                                        │ Generated Files       │
                                        └──────────┬───────────┘
                                                   │
                    ┌──────────────────────────────┼──────────────────┐
                    │                              │                  │
                    ▼                              ▼                  ▼
          ┌─────────────────┐          ┌──────────────────┐  ┌───────────────┐
          │ DETERMINISTIC   │          │ AUDIT TRAIL (14) │  │ PIPELINE      │
          │ OUTPUT (13)     │          │ Records what     │  │ STATE (15)    │
          │ Same in → out   │          │ happened         │  │ Tracks all    │
          └─────────────────┘          └──────────────────┘  └───────────────┘
```

---

## Pattern Relationships

### Foundation → Everything

The foundation patterns support all others:

| Foundation Pattern | Patterns It Enables |
|--------------------|---------------------|
| **THE SINGLE COMMAND** | All patterns (via flags) |
| **MANIFEST AS TRUTH** | ONTOLOGY LOADING, INFERENCE, GENERATION |
| **THREE-LAYER ARCHITECTURE** | Implementation of all patterns |

### Knowledge Flow

Knowledge patterns form a pipeline:

```
ONTOLOGY LOADING → INFERENCE ENRICHMENT → GENERATION RULES → TEMPLATE RENDERING
      ↓                    ↓                     ↓                    ↓
   Graph              Enriched Graph         Query Results        Generated Code
```

### Safety Controls

Safety patterns modify behavior:

| Pattern | What It Controls |
|---------|------------------|
| **DRY RUN** | File writing |
| **VALIDATION GATE** | Entire pipeline (stops early) |
| **FORCE OVERWRITE** | File protection modes |
| **TIMEOUT PROTECTION** | Execution duration |
| **ERROR SIGNALS** | Failure communication |

### Integrity Support

Integrity patterns ensure trust:

| Pattern | Trust It Provides |
|---------|-------------------|
| **DETERMINISTIC OUTPUT** | Reproducibility |
| **AUDIT TRAIL** | Provenance |
| **PIPELINE STATE** | Observability |

---

## Navigation by Intent

### "I want to understand the system"

Start with these patterns in order:
1. [THE SINGLE COMMAND](patterns/01-single-command.md) — What is ggen sync?
2. [MANIFEST AS TRUTH](patterns/02-manifest-as-truth.md) — How is it configured?
3. [THREE-LAYER ARCHITECTURE](patterns/03-three-layer-architecture.md) — How is it built?

### "I want to generate code"

Read these patterns:
1. [ONTOLOGY LOADING](patterns/04-ontology-loading.md) — Bringing in knowledge
2. [GENERATION RULES](patterns/06-generation-rules.md) — Defining transformations
3. [TEMPLATE RENDERING](patterns/07-template-rendering.md) — Shaping output

### "I want to ensure quality"

Focus on these patterns:
1. [VALIDATION GATE](patterns/09-validation-gate.md) — Check before running
2. [DRY RUN](patterns/08-dry-run.md) — Preview before committing
3. [DETERMINISTIC OUTPUT](patterns/13-deterministic-output.md) — Ensure reproducibility

### "I want to debug a problem"

Use these patterns:
1. [ERROR SIGNALS](patterns/12-error-signals.md) — Understanding failures
2. [RULE SELECTION](patterns/16-rule-selection.md) — Isolating the problem
3. [AUDIT TRAIL](patterns/14-audit-trail.md) — Seeing what happened

### "I want to optimize performance"

Apply these patterns:
1. [TIMEOUT PROTECTION](patterns/11-timeout-protection.md) — Setting limits
2. [RULE SELECTION](patterns/16-rule-selection.md) — Running only what's needed
3. [AUDIT TRAIL](patterns/14-audit-trail.md) — Finding slow operations

---

## Pattern Dependencies

```
                    ┌─────────────────────┐
                    │ DETERMINISTIC       │
                    │ OUTPUT ***          │
                    │ (fundamental)       │
                    └──────────┬──────────┘
                               │
          ┌────────────────────┼────────────────────┐
          │                    │                    │
          ▼                    ▼                    ▼
    ┌───────────┐       ┌───────────┐        ┌───────────┐
    │ Ordered   │       │ ORDER BY  │        │ BTreeMap  │
    │ rules     │       │ in SPARQL │        │ not Hash  │
    └───────────┘       └───────────┘        └───────────┘
```

**DETERMINISTIC OUTPUT** is the most fundamental pattern—it constrains how all other patterns must be implemented.

---

## Pattern Conflicts and Resolutions

Some patterns seem to conflict. They don't—they're choices:

| Pattern A | vs | Pattern B | Resolution |
|-----------|----|-----------|-----------
| DRY RUN | vs | Normal | Flag choice: `--dry-run` |
| VALIDATION GATE | vs | Normal | Flag choice: `--validate-only` |
| FORCE OVERWRITE | vs | Protection | Flag choice: `--force` |
| RULE SELECTION | vs | Full run | Flag choice: `--rule` |

The patterns exist to give you options. The flags let you choose which option to apply.

---

## Adding New Patterns

If you discover a new pattern in your use of ggen sync:

1. **Name it** — A memorable name that evokes the pattern's essence
2. **Describe the context** — When does this pattern apply?
3. **Identify the forces** — What tensions does it resolve?
4. **State the solution** — What action resolves the forces?
5. **Connect it** — How does it relate to existing patterns?

New patterns should:
- Fill a gap in the existing language
- Resolve a recurring problem
- Connect to at least two existing patterns
- Support DETERMINISTIC OUTPUT (the fundamental pattern)

---

## The Complete List

| # | Pattern | Confidence | Category |
|---|---------|------------|----------|
| 1 | [THE SINGLE COMMAND](patterns/01-single-command.md) | ** | Foundation |
| 2 | [MANIFEST AS TRUTH](patterns/02-manifest-as-truth.md) | ** | Foundation |
| 3 | [THREE-LAYER ARCHITECTURE](patterns/03-three-layer-architecture.md) | ** | Foundation |
| 4 | [ONTOLOGY LOADING](patterns/04-ontology-loading.md) | ** | Knowledge |
| 5 | [INFERENCE ENRICHMENT](patterns/05-inference-enrichment.md) | * | Knowledge |
| 6 | [GENERATION RULES](patterns/06-generation-rules.md) | ** | Knowledge |
| 7 | [TEMPLATE RENDERING](patterns/07-template-rendering.md) | ** | Knowledge |
| 8 | [DRY RUN](patterns/08-dry-run.md) | ** | Safety |
| 9 | [VALIDATION GATE](patterns/09-validation-gate.md) | ** | Safety |
| 10 | [FORCE OVERWRITE](patterns/10-force-overwrite.md) | * | Safety |
| 11 | [TIMEOUT PROTECTION](patterns/11-timeout-protection.md) | ** | Safety |
| 12 | [ERROR SIGNALS](patterns/12-error-signals.md) | ** | Safety |
| 13 | [DETERMINISTIC OUTPUT](patterns/13-deterministic-output.md) | *** | Integrity |
| 14 | [AUDIT TRAIL](patterns/14-audit-trail.md) | ** | Integrity |
| 15 | [PIPELINE STATE](patterns/15-pipeline-state.md) | * | Integrity |
| 16 | [RULE SELECTION](patterns/16-rule-selection.md) | * | Selective |
