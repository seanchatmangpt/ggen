# Summary

[Preface](preface.md)
[Acknowledgments](acknowledgments.md)

---

# Part I: Foundations

Understanding the philosophy, history, and principles that underlie the pattern language.

- [Introduction: The Quality Without a Name](introduction.md)
  - [What is a Pattern Language?](introduction/what-is-pattern-language.md)
  - [Christopher Alexander's Vision](introduction/alexanders-vision.md)
  - [Why Patterns for Code Generation?](introduction/why-patterns-for-codegen.md)
  - [The Central Insight: Synchronization as Truth](introduction/synchronization-as-truth.md)
- [How to Use This Pattern Language](how-to-use.md)
  - [Reading the Patterns](how-to-use/reading-patterns.md)
  - [The Pattern Format Explained](how-to-use/pattern-format.md)
  - [Navigation Strategies](how-to-use/navigation-strategies.md)
  - [Applying Patterns in Practice](how-to-use/applying-patterns.md)
  - [When Patterns Conflict](how-to-use/pattern-conflicts.md)
- [The History of ggen sync](history.md)
  - [From Many Commands to One](history/many-to-one.md)
  - [The Evolution of the Pipeline](history/pipeline-evolution.md)
  - [Lessons Learned](history/lessons-learned.md)

---

# Part II: The Pattern Language

The complete collection of patterns, organized from large-scale to small-scale.

## Foundation Patterns

These patterns establish the fundamental structure and philosophy of code synchronization. They are the largest-scale patterns, setting the context for everything that follows.

- [1. THE SINGLE COMMAND **](patterns/01-single-command.md)
- [2. MANIFEST AS TRUTH **](patterns/02-manifest-as-truth.md)
- [3. THREE-LAYER ARCHITECTURE **](patterns/03-three-layer-architecture.md)

## Knowledge Patterns

These patterns govern how domain knowledge flows through the system and becomes code. They form the heart of the generation pipeline.

- [4. ONTOLOGY LOADING **](patterns/04-ontology-loading.md)
- [5. INFERENCE ENRICHMENT *](patterns/05-inference-enrichment.md)
- [6. GENERATION RULES **](patterns/06-generation-rules.md)
- [7. TEMPLATE RENDERING **](patterns/07-template-rendering.md)

## Safety Patterns

These patterns protect the system, developers, and outputs from harm. They provide safeguards, previews, and escape hatches.

- [8. DRY RUN **](patterns/08-dry-run.md)
- [9. VALIDATION GATE **](patterns/09-validation-gate.md)
- [10. FORCE OVERWRITE *](patterns/10-force-overwrite.md)
- [11. TIMEOUT PROTECTION **](patterns/11-timeout-protection.md)
- [12. ERROR SIGNALS **](patterns/12-error-signals.md)

## Integrity Patterns

These patterns ensure trust, reproducibility, and auditability in generated outputs.

- [13. DETERMINISTIC OUTPUT ***](patterns/13-deterministic-output.md)
- [14. AUDIT TRAIL **](patterns/14-audit-trail.md)
- [15. PIPELINE STATE *](patterns/15-pipeline-state.md)

## Selective Patterns

These patterns allow focused, intentional synchronization for specific needs.

- [16. RULE SELECTION *](patterns/16-rule-selection.md)

---

# Part III: Patterns in Practice

Applying the patterns to real-world scenarios.

- [Anti-Patterns: What Not to Do](anti-patterns.md)
- [Case Studies](case-studies.md)
- [Complete Worked Examples](examples.md)
- [Troubleshooting Guide](troubleshooting.md)

---

# Part IV: Advanced Topics

Deep dives into specialized areas for experts.

- [Advanced Topics](advanced.md)
- [Integration Guides](integration.md)
- [Extension and Customization](extension.md)

---

# Part V: Reference

Complete reference materials.

- [Pattern Map](pattern-map.md)
- [Glossary](glossary.md)
- [Complete Manifest Reference](reference/manifest.md)
- [SPARQL Quick Reference](reference/sparql.md)
- [Tera Template Quick Reference](reference/tera.md)
- [CLI Flag Reference](reference/cli.md)
- [Exit Code Reference](reference/exit-codes.md)

---

# Appendices

- [Appendix A: The Philosophy of Patterns](appendix/philosophy.md)
- [Appendix B: Christopher Alexander's Influence](appendix/alexander.md)
- [Appendix C: RDF and Semantic Web Primer](appendix/rdf-primer.md)
- [Appendix D: SPARQL Language Reference](appendix/sparql-reference.md)
- [Appendix E: Tera Template Language Reference](appendix/tera-reference.md)
- [Appendix F: Comparison with Other Tools](appendix/comparison.md)

---

[Bibliography](bibliography.md)
[Index](index.md)
