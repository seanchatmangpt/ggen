<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Bibliography and References](#bibliography-and-references)
  - [Core Research Papers](#core-research-papers)
    - [Toyota Production System & Manufacturing](#toyota-production-system--manufacturing)
    - [Workflow Patterns](#workflow-patterns)
    - [Organizational Structure & Conway's Law](#organizational-structure--conways-law)
    - [Queueing Theory & Little's Law](#queueing-theory--littles-law)
    - [Type Theory & Formal Methods](#type-theory--formal-methods)
    - [Testing & Test-Driven Development](#testing--test-driven-development)
    - [Distributed Systems & Consensus](#distributed-systems--consensus)
    - [Semantic Web & RDF](#semantic-web--rdf)
    - [Software Architecture](#software-architecture)
    - [Performance & Optimization](#performance--optimization)
    - [Security & Cryptography](#security--cryptography)
  - [Industry Reports & Case Studies](#industry-reports--case-studies)
  - [Economic Theory](#economic-theory)
  - [Protocol Specifications](#protocol-specifications)
  - [Software Engineering Practices](#software-engineering-practices)
  - [Rust & Systems Programming](#rust--systems-programming)
  - [Additional Resources](#additional-resources)
    - [Online Resources](#online-resources)
    - [Related ggen Documentation](#related-ggen-documentation)
  - [Citation Format](#citation-format)
  - [Further Reading](#further-reading)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Bibliography and References

**Version**: 1.0.0
**Date**: 2026-02-09
**Status**: Canonical Reference

## Core Research Papers

### Toyota Production System & Manufacturing

1. **Ohno, T. (1988)**. *Toyota Production System: Beyond Large-Scale Production*. Productivity Press.
   - Foundation of TPS principles: JIT, Jidoka, Kaizen
   - Referenced in: `07-tps-manufacturing-os.md`

2. **Liker, J. K. (2004)**. *The Toyota Way: 14 Management Principles from the World's Greatest Manufacturer*. McGraw-Hill.
   - 14 principles of Toyota philosophy
   - Referenced in: `07-tps-manufacturing-os.md`, `13-jidoka-receipts.md`

3. **Womack, J. P., & Jones, D. T. (1996)**. *Lean Thinking: Banish Waste and Create Wealth in Your Corporation*. Simon & Schuster.
   - Five lean principles, waste elimination
   - Referenced in: `07-tps-manufacturing-os.md`

### Workflow Patterns

4. **van der Aalst, W. M., ter Hofstede, A. H., Kiepuszewski, B., & Barros, A. P. (2003)**. "Workflow Patterns". *Distributed and Parallel Databases*, 14(1), 5-51.
   - Complete catalog of 43 workflow control patterns
   - Referenced in: `05-workflow-patterns.md`

5. **Russell, N., ter Hofstede, A. H., Edmond, D., & van der Aalst, W. M. (2004)**. "Workflow Data Patterns". Queensland University of Technology Technical Report.
   - Data interaction patterns for workflows
   - Referenced in: `05-workflow-patterns.md`

### Organizational Structure & Conway's Law

6. **Conway, M. E. (1968)**. "How Do Committees Invent?". *Datamation*, 14(4), 28-31.
   - Original Conway's Law paper
   - Referenced in: `02-physical-constraints.md`

7. **MacCormack, A., Rusnak, J., & Baldwin, C. Y. (2006)**. "Exploring the Structure of Complex Software Designs: An Empirical Study of Open Source and Proprietary Code". *Management Science*, 52(7), 1015-1030.
   - Empirical validation of Conway's Law
   - Referenced in: `02-physical-constraints.md`

8. **Herbsleb, J. D., & Grinter, R. E. (1999)**. "Architectures, coordination, and distance: Conway's law and beyond". *IEEE Software*, 16(5), 63-70.
   - Coordination costs in distributed development
   - Referenced in: `02-physical-constraints.md`

### Queueing Theory & Little's Law

9. **Little, J. D. C. (1961)**. "A Proof for the Queuing Formula: L = λW". *Operations Research*, 9(3), 383-387.
   - Original proof of Little's Law
   - Referenced in: `02-physical-constraints.md`, `11-backpressure.md`

10. **Reinertsen, D. G. (2009)**. *The Principles of Product Development Flow: Second Generation Lean Product Development*. Celeritas Publishing.
    - Application of queueing theory to product development
    - Referenced in: `02-physical-constraints.md`, `11-backpressure.md`

### Type Theory & Formal Methods

11. **Pierce, B. C. (2002)**. *Types and Programming Languages*. MIT Press.
    - Foundation of type systems
    - Referenced in: `03-scm-vs-ccm.md`

12. **Martin-Löf, P. (1984)**. *Intuitionistic Type Theory*. Bibliopolis.
    - Dependent types and constructive mathematics
    - Referenced in: `03-scm-vs-ccm.md`

13. **Wadler, P. (2015)**. "Propositions as types". *Communications of the ACM*, 58(12), 75-84.
    - Curry-Howard correspondence
    - Referenced in: `03-scm-vs-ccm.md`

### Testing & Test-Driven Development

14. **Beck, K. (2002)**. *Test Driven Development: By Example*. Addison-Wesley.
    - TDD principles and red-green-refactor cycle
    - Referenced in: `19-testing-doctrine.md`

15. **Meszaros, G. (2007)**. *xUnit Test Patterns: Refactoring Test Code*. Addison-Wesley.
    - Chicago vs London schools of TDD
    - Referenced in: `19-testing-doctrine.md`

16. **Freeman, S., & Pryce, N. (2009)**. *Growing Object-Oriented Software, Guided by Tests*. Addison-Wesley.
    - Outside-in TDD, mock objects
    - Referenced in: `19-testing-doctrine.md` (contrasted with Chicago TDD)

### Distributed Systems & Consensus

17. **Lamport, L. (1998)**. "The Part-Time Parliament". *ACM Transactions on Computer Systems*, 16(2), 133-169.
    - Paxos consensus algorithm
    - Referenced in: `16-a2a-construct.md`

18. **Ongaro, D., & Ousterhout, J. (2014)**. "In Search of an Understandable Consensus Algorithm". *USENIX ATC*, 305-319.
    - Raft consensus algorithm
    - Referenced in: `16-a2a-construct.md`

### Semantic Web & RDF

19. **Brickley, D., & Guha, R. V. (2014)**. "RDF Schema 1.1". *W3C Recommendation*.
    - RDFS specification
    - Referenced in: `14-ggen-pipeline.md`

20. **Hitzler, P., Krötzsch, M., Parsia, B., Patel-Schneider, P. F., & Rudolph, S. (2012)**. "OWL 2 Web Ontology Language Primer". *W3C Recommendation*.
    - OWL 2 specification
    - Referenced in: `14-ggen-pipeline.md`

21. **Knublauch, H., & Kontokostas, D. (2017)**. "Shapes Constraint Language (SHACL)". *W3C Recommendation*.
    - SHACL validation specification
    - Referenced in: `14-ggen-pipeline.md`

### Software Architecture

22. **Fowler, M. (2002)**. *Patterns of Enterprise Application Architecture*. Addison-Wesley.
    - Domain model patterns
    - Referenced in: `03-scm-vs-ccm.md`

23. **Evans, E. (2003)**. *Domain-Driven Design: Tackling Complexity in the Heart of Software*. Addison-Wesley.
    - Ubiquitous language, bounded contexts
    - Referenced in: `08-osiris-architecture.md`

### Performance & Optimization

24. **Knuth, D. E. (1974)**. "Structured Programming with go to Statements". *ACM Computing Surveys*, 6(4), 261-301.
    - "Premature optimization is the root of all evil"
    - Referenced in: `04-no-moving-parts.md`

25. **Gregg, B. (2013)**. *Systems Performance: Enterprise and the Cloud*. Prentice Hall.
    - Performance analysis methodology
    - Referenced in: `11-backpressure.md`

### Security & Cryptography

26. **Menezes, A. J., van Oorschot, P. C., & Vanstone, S. A. (1996)**. *Handbook of Applied Cryptography*. CRC Press.
    - Cryptographic primitives
    - Referenced in: `13-jidoka-receipts.md`

27. **Bernstein, D. J., Duif, N., Lange, T., Schwabe, P., & Yang, B. Y. (2012)**. "High-speed high-security signatures". *Journal of Cryptographic Engineering*, 2(2), 77-89.
    - Ed25519 signature scheme
    - Referenced in: `13-jidoka-receipts.md`, `20-mcp-a2a-obligations.md`

## Industry Reports & Case Studies

28. **Microsoft Engineering Practices (2016)**. "Agile Development at Microsoft: Measuring Lead Time".
    - Empirical data on Little's Law in software development
    - Referenced in: `02-physical-constraints.md`

29. **Amazon Web Services (2015)**. "Amazon's Microservices Architecture".
    - Conway's Law in practice at scale
    - Referenced in: `02-physical-constraints.md`

30. **Google (2016)**. "Site Reliability Engineering: How Google Runs Production Systems". O'Reilly.
    - SLO/SLI framework, error budgets
    - Referenced in: `11-backpressure.md`

## Economic Theory

31. **Shapiro, C., & Varian, H. R. (1998)**. *Information Rules: A Strategic Guide to the Network Economy*. Harvard Business School Press.
    - Network effects, lock-in
    - Referenced in: `17-dominance-theorem.md`, `18-marketplace-economy.md`

32. **Bass, F. M. (1969)**. "A New Product Growth for Model Consumer Durables". *Management Science*, 15(5), 215-227.
    - Bass diffusion model
    - Referenced in: `17-dominance-theorem.md`

## Protocol Specifications

33. **MCP (Model Context Protocol) Specification (2024)**. Anthropic.
    - MCP transport requirements
    - Referenced in: `20-mcp-a2a-obligations.md`

34. **A2A (Agent-to-Agent) Protocol Draft (2025)**. Community specification.
    - A2A task coordination
    - Referenced in: `16-a2a-construct.md`, `20-mcp-a2a-obligations.md`

## Software Engineering Practices

35. **Kim, G., Debois, P., Willis, J., & Humble, J. (2016)**. *The DevOps Handbook: How to Create World-Class Agility, Reliability, and Security in Technology Organizations*. IT Revolution Press.
    - DevOps practices, deployment pipelines
    - Referenced in: `19-testing-doctrine.md`

36. **Humble, J., & Farley, D. (2010)**. *Continuous Delivery: Reliable Software Releases through Build, Test, and Deployment Automation*. Addison-Wesley.
    - CD principles, deployment pipeline
    - Referenced in: `19-testing-doctrine.md`

## Rust & Systems Programming

37. **Klabnik, S., & Nichols, C. (2019)**. *The Rust Programming Language*. No Starch Press.
    - Ownership, borrowing, type system
    - Referenced throughout (implementation language)

38. **Blandy, J., Orendorff, J., & Tindall, L. (2021)**. *Programming Rust: Fast, Safe Systems Development*. O'Reilly.
    - Advanced Rust patterns
    - Referenced throughout (implementation examples)

## Additional Resources

### Online Resources

- **W3C Semantic Web Standards**: https://www.w3.org/standards/semanticweb/
- **SPARQL 1.1 Query Language**: https://www.w3.org/TR/sparql11-query/
- **Rust Documentation**: https://doc.rust-lang.org/
- **Toyota Production System**: https://www.toyota-global.com/company/vision_philosophy/toyota_production_system/

### Related ggen Documentation

- **ggen CLAUDE.md**: Project rules and development methodology
- **ggen Architecture**: `/home/user/ggen/docs/architecture/`
- **ggen Tutorials**: `/home/user/ggen/docs/tutorials/`

## Citation Format

All citations follow APA 7th edition format. Cross-references within the paradigm-shift documentation use markdown links with document name and section number.

Example:
```markdown
See [02-physical-constraints.md § 2.1] for Conway's Law proof.
```

## Further Reading

For deeper understanding of specific topics, see the "Further Reading" sections at the end of each document in the paradigm-shift series.

---

**Maintained by**: ggen documentation team
**Last Updated**: 2026-02-09
**License**: Same as ggen project (MIT)
