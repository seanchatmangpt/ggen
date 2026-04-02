# Acknowledgments

> *"In the end, it is the pattern languages which are living and which will create more and more life in the world."*
>
> — Christopher Alexander, *The Timeless Way of Building* (1979)

---

## Christopher Alexander (1936–2022)

This book would not exist without the life's work of Christopher Alexander. His three major contributions — *A Pattern Language* (1977), *The Timeless Way of Building* (1979), and the four-volume *The Nature of Order* (2002–2005) — constitute the deepest inquiry into the structural basis of life and quality in human artifacts that this author knows.

Alexander's specific intellectual contribution to this work is threefold. First, the pattern language methodology: the idea that recurring problems in context can be named, that their solutions can be described precisely, and that patterns at different scales connect into a coherent language that generates good designs. Second, the concept of "the quality without a name" — the recognition that there is an objective, if hard-to-articulate, quality that distinguishes living structures from dead ones, and that this quality is worth pursuing systematically. Third, the organizational principle of moving from large-scale to small-scale: the super-patterns that establish the whole precede the leaf patterns that fill in the details, and the whole is more than the sum of its parts.

The format of every pattern in this book — context, forces, solution, connections — follows Alexander's template. The star rating follows his confidence system. The insistence that patterns be grounded in observable, recurring phenomena rather than invented theoretically follows his empirical discipline.

---

## Wil van der Aalst and the Workflow Patterns Initiative

Wil van der Aalst, Arthur ter Hofstede, and colleagues at Eindhoven University of Technology and Queensland University of Technology developed the Workflow Control-Flow Patterns (WCPs) beginning in the early 2000s. Their catalog of 43 control-flow patterns — sequence, parallel split, synchronization, exclusive choice, simple merge, and the more exotic multi-instance and cancellation patterns — was itself an application of Alexander's pattern language methodology to workflow systems.

The connection to this book runs through YAWL (Yet Another Workflow Language), the formalism van der Aalst designed to express all 43 WCPs directly. YAWL workflows map naturally to Java 26 structured concurrency scopes, virtual thread execution models, and event-driven aggregate patterns. The ggen-yawl crate bridges YAWL ontologies to the Java 26 patterns documented here.

Van der Aalst's contribution demonstrates that pattern language thinking is not confined to buildings or software design patterns in the GoF sense. Any domain rich enough in recurring problems and recurring solutions can be documented as a pattern language. Workflows are one such domain. Java 26 code generation is another.

---

## The Semantic Web Community

Tim Berners-Lee, James Hendler, and Ora Lassila proposed the Semantic Web in their landmark 2001 *Scientific American* article, articulating a vision of the web as a medium not just for human reading but for machine reasoning — a web of meaning, not just syntax.

The substrate that makes ontology-first development possible is the product of that community's thirty years of work: RDF as a universal graph model, SPARQL as a query language expressive enough to extract any pattern from any ontology, OWL as a type system capable of modeling closed-world hierarchies (`owl:oneOf`), property restrictions, cardinality constraints, and inheritance chains that correspond exactly to Java 26's type system.

This correspondence is not coincidence. OWL's designers needed to model the kinds of knowledge that software systems manipulate. Java's designers needed to model the kinds of knowledge that business domains contain. Both systems converged on the same underlying structures: named types, closed hierarchies, value identity, relationship cardinality. The bridge between them — which ggen builds — was always latent. Making it explicit is this project's contribution.

---

## The Java 26 Platform Team

Project Loom, Project Amber, and Project Valhalla represent a decade of sustained investment in making the Java language and runtime worth generating.

Project Loom's virtual threads (finalized in Java 21, deepened in Java 26) eliminate the impedance mismatch between structured concurrency and blocking I/O that made thread-per-request models expensive. When a virtual thread can be suspended and resumed at nearly zero cost, the programming model becomes synchronous again — comprehensible, debuggable, naturally mappable to workflow steps.

Project Amber's contributions — records (Java 16), sealed classes (Java 17), pattern matching in `switch` (Java 21), and their refinements through Java 26 — are what make the domain model patterns in this book possible. A `record` is not just a shorter way to write a data class. It is a formal declaration of value identity, immutability, and structural equality — the same invariants that RDF blank nodes carry. A `sealed` hierarchy is not just a compiler restriction. It is a closed-world assumption encoded in the type system.

Project Valhalla's value objects (arriving in Java 26) complete the picture: types that are identity-free, stack-allocated, inline in memory — the JVM-level realization of what OWL's datatype properties have always modeled.

---

## The ggen Contributors

Sean Chatman designed and implemented the ggen system: the five-stage μ₁–μ₅ pipeline, the deterministic canonicalization that makes outputs reproducible, the Tera templating integration, the SPARQL query layer over Oxigraph, the manifest and receipt system. The patterns in this book are executable because of that implementation.

The ggen community — contributors, early adopters, and users who reported issues and proposed improvements — shaped the patterns through use. Patterns that seemed elegant in theory but proved awkward in practice were revised. Patterns that emerged from real projects were named and documented. The language is grounded in use because of their participation.

---

## A Distinction from the Gang of Four

This book differs from Gamma, Helm, Johnson, and Vlissides's *Design Patterns: Elements of Reusable Object-Oriented Software* (1994) in one fundamental way.

GoF patterns are design recipes. You read a pattern — Observer, Strategy, Decorator — and then you *implement* it: you write the abstract classes, the interfaces, the delegation wiring, the registration mechanism. The pattern guides the implementation. You do the work.

Every pattern in this book is directly executable via `ggen sync`. You do not implement the pattern. You declare it in RDF, and the pattern implements itself. The TTL snippet at the end of each pattern is not a starting point for implementation work. It is the *complete* specification. The Java code that emerges is not your implementation of the pattern — it is the pattern's own projection into the Java 26 type system, derived deterministically from the knowledge you declared.

This is the promise: write knowledge, not code. The code follows from the knowledge, necessarily and completely.

---

*With gratitude,*
*Sean Chatman and the ggen Team*
*March 2026*

---
