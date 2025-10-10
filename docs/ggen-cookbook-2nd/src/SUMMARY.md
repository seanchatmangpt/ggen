<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Summary](#summary)
- [Part I: The Foundation](#part-i-the-foundation)
- [Part II: Core Engine & CLI](#part-ii-core-engine--cli)
- [Part III: Authoring Language](#part-iii-authoring-language)
- [Part IV: Autonomic System](#part-iv-autonomic-system)
- [Part V: The Ecosystem](#part-v-the-ecosystem)
- [Part VI: Advanced & Enterprise](#part-vi-advanced--enterprise)
- [Appendices](#appendices)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Summary

[Preface](./preface.md)
[Introduction](./introduction.md)

---

# Part I: The Foundation

- [Chapter 1: The Pattern Language](./part-1/chapter-1.md)
  - [1.1 What is a Pattern Language?](./part-1/chapter-1-1.md)
  - [1.2 The Alexandrian Approach](./part-1/chapter-1-2.md)
  - [1.3 Why Patterns for Code Generation?](./part-1/chapter-1-3.md)
  - [1.4 Reading This Book](./part-1/chapter-1-4.md)

- [Chapter 2: Philosophy & Vision](./part-1/chapter-2.md)
  - [2.1 The Autonomic Computing Model](./part-1/chapter-2-1.md)
  - [2.2 Self-* Properties](./part-1/chapter-2-2.md)
  - [2.3 The Generate-Validate-Refine Loop](./part-1/chapter-2-3.md)
  - [2.4 Domain-Driven Generation](./part-1/chapter-2-4.md)

---

# Part II: Core Engine & CLI

- [Chapter 3: The GGen Engine](./part-2/chapter-3.md)
  - [3.1 Architecture Overview](./part-2/chapter-3-1.md)
  - [3.2 Template Processing Pipeline](./part-2/chapter-3-2.md)
  - [3.3 The Registry System](./part-2/chapter-3-3.md)
  - [3.4 Extension Points](./part-2/chapter-3-4.md)

- [Chapter 4: Command-Line Interface](./part-2/chapter-4.md)
  - [4.1 Essential Commands](./part-2/chapter-4-1.md)
  - [4.2 Workflow Patterns](./part-2/chapter-4-2.md)
  - [4.3 CI/CD Integration](./part-2/chapter-4-3.md)
  - [4.4 Shell Completion](./part-2/chapter-4-4.md)

---

# Part III: Authoring Language

- [Chapter 5: Template Anatomy](./part-3/chapter-5.md)
  - [5.1 The .tmpl Format](./part-3/chapter-5-1.md)
  - [5.2 Frontmatter & Metadata](./part-3/chapter-5-2.md)
  - [5.3 Body & Templating Syntax](./part-3/chapter-5-3.md)
  - [5.4 Template Composition](./part-3/chapter-5-4.md)

- [Chapter 6: Core Patterns](./part-3/chapter-6.md)
  - [Pattern 1: Single File Generator](./part-3/chapter-6-1.md)
  - [Pattern 2: Multi-File Project](./part-3/chapter-6-2.md)
  - [Pattern 3: Conditional Generation](./part-3/chapter-6-3.md)
  - [Pattern 4: Template Inheritance](./part-3/chapter-6-4.md)
  - [Pattern 5: Dynamic Variables](./part-3/chapter-6-5.md)

- [Chapter 7: Advanced Patterns](./part-3/chapter-7.md)
  - [Pattern 6: Language-Specific Generators](./part-3/chapter-7-1.md)
  - [Pattern 7: Framework Scaffolding](./part-3/chapter-7-2.md)
  - [Pattern 8: Migration Generators](./part-3/chapter-7-3.md)
  - [Pattern 9: Testing Utilities](./part-3/chapter-7-4.md)
  - [Pattern 10: Documentation Automation](./part-3/chapter-7-5.md)

- [Chapter 8: Data-Driven Generation](./part-3/chapter-8.md)
  - [8.1 JSON/YAML Input](./part-3/chapter-8-1.md)
  - [8.2 RDF & Semantic Models](./part-3/chapter-8-2.md)
  - [8.3 Database Schema to Code](./part-3/chapter-8-3.md)
  - [8.4 API Specification Generators](./part-3/chapter-8-4.md)

- [Chapter 9: Best Practices](./part-3/chapter-9.md)
  - [9.1 Template Design Principles](./part-3/chapter-9-1.md)
  - [9.2 Maintainability & Versioning](./part-3/chapter-9-2.md)
  - [9.3 Testing Templates](./part-3/chapter-9-3.md)
  - [9.4 Documentation Standards](./part-3/chapter-9-4.md)

- [Chapter 10: Pattern Catalog](./part-3/chapter-10.md)
  - [10.1 Index by Use Case](./part-3/chapter-10-1.md)
  - [10.2 Index by Language](./part-3/chapter-10-2.md)
  - [10.3 Index by Framework](./part-3/chapter-10-3.md)
  - [10.4 Contributing New Patterns](./part-3/chapter-10-4.md)

---

# Part IV: Autonomic System

- [Chapter 11: Self-Configuration](./part-4/chapter-11.md)
  - [11.1 Template Discovery](./part-4/chapter-11-1.md)
  - [11.2 Auto-Detection of Context](./part-4/chapter-11-2.md)
  - [11.3 Intelligent Defaults](./part-4/chapter-11-3.md)
  - [11.4 Configuration Inheritance](./part-4/chapter-11-4.md)

- [Chapter 12: Self-Optimization](./part-4/chapter-12.md)
  - [12.1 Performance Profiling](./part-4/chapter-12-1.md)
  - [12.2 Caching Strategies](./part-4/chapter-12-2.md)
  - [12.3 Incremental Generation](./part-4/chapter-12-3.md)
  - [12.4 Parallel Processing](./part-4/chapter-12-4.md)

- [Chapter 13: Self-Healing & Validation](./part-4/chapter-13.md)
  - [13.1 Output Validation](./part-4/chapter-13-1.md)
  - [13.2 Error Recovery](./part-4/chapter-13-2.md)
  - [13.3 Deterministic Guarantees](./part-4/chapter-13-3.md)
  - [13.4 Audit Trails](./part-4/chapter-13-4.md)

---

# Part V: The Ecosystem

- [Chapter 14: The Marketplace](./part-5/chapter-14.md)
  - [14.1 Publishing Templates](./part-5/chapter-14-1.md)
  - [14.2 Discovering & Installing](./part-5/chapter-14-2.md)
  - [14.3 Versioning & Updates](./part-5/chapter-14-3.md)
  - [14.4 Community Guidelines](./part-5/chapter-14-4.md)

- [Chapter 15: Integration & Tooling](./part-5/chapter-15.md)
  - [15.1 IDE/Editor Plugins](./part-5/chapter-15-1.md)
  - [15.2 CI/CD Pipelines](./part-5/chapter-15-2.md)
  - [15.3 API & SDKs](./part-5/chapter-15-3.md)
  - [15.4 Monitoring & Analytics](./part-5/chapter-15-4.md)

---

# Part VI: Advanced & Enterprise

- [Chapter 16: Enterprise Patterns](./part-6/chapter-16.md)
  - [16.1 Multi-Tenant Templates](./part-6/chapter-16-1.md)
  - [16.2 Compliance & Governance](./part-6/chapter-16-2.md)
  - [16.3 Secrets Management](./part-6/chapter-16-3.md)
  - [16.4 Scale & Performance](./part-6/chapter-16-4.md)

- [Chapter 17: Extending GGen](./part-6/chapter-17.md)
  - [17.1 Custom Processors](./part-6/chapter-17-1.md)
  - [17.2 Plugin Architecture](./part-6/chapter-17-2.md)
  - [17.3 Foreign Function Interface](./part-6/chapter-17-3.md)
  - [17.4 Contributing to Core](./part-6/chapter-17-4.md)

---

# Appendices

- [Appendix A: Template Reference](./appendices/appendix-a.md)
  - [A.1 Syntax Quick Reference](./appendices/appendix-a-1.md)
  - [A.2 Built-in Functions](./appendices/appendix-a-2.md)
  - [A.3 Helper Libraries](./appendices/appendix-a-3.md)

- [Appendix B: CLI Reference](./appendices/appendix-b.md)
  - [B.1 Command Index](./appendices/appendix-b-1.md)
  - [B.2 Configuration Options](./appendices/appendix-b-2.md)
  - [B.3 Environment Variables](./appendices/appendix-b-3.md)

- [Appendix C: Resources](./appendices/appendix-c.md)
  - [C.1 Community & Support](./appendices/appendix-c-1.md)
  - [C.2 Learning Path](./appendices/appendix-c-2.md)
  - [C.3 Bibliography](./appendices/appendix-c-3.md)
