# Introduction: Code Precipitates from Knowledge

> *"There is a central quality which is the root criterion of life and spirit in a man, a town, a building, or a wilderness. This quality is objective and precise, but it cannot be named."*
>
> — Christopher Alexander, *The Timeless Way of Building* (1979)

---

## The Quality Without a Name

There is a Spring Boot service you have read — perhaps you wrote it, perhaps a colleague wrote it, perhaps you found it in an old repository at two in the morning trying to understand why something was failing. You remember it not because it was complex or clever, but because it was *obvious*. Every class knew exactly what it was. The boundaries were clean. The types told you what the data meant. The methods were short because they didn't need to be long. Reading it felt like reading a sentence you already knew how to parse.

You have also read the other kind. The `@Service` class with 47 methods. The `OrderDTO` that is just a mutable POJO with no invariants, indistinguishable from the `OrderRequest` and the `OrderResponse` and the `OrderView` — all carrying slightly different subsets of the same fields, all defined separately, all drifting apart over time. The JPA entity whose lifecycle callbacks have side effects that trigger other side effects. The `if (status.equals("PENDING") || status.equals("pending") || status.equals("Pending"))` guard that appears in eleven places.

Alexander called the first kind *alive* and the second kind *dead*. He was talking about buildings. He meant it literally — that there is an objective quality of life in a design, a quality you recognize immediately even if you cannot fully articulate it. The quality is not decoration. It is not cleverness. It is something closer to *fittingness* — each element in right relation to every other, each boundary drawn where the system's nature demands a boundary be drawn.

Java 26 makes it possible for the first kind to become the norm. Not because Java 26 is magic, but because its type system has finally grown powerful enough to express the structure that was always *trying* to emerge from good domain models. `sealed interface` hierarchies replace stringly-typed status fields. `record` types replace anemic DTOs. Pattern matching in `switch` replaces chains of `instanceof` checks. Virtual threads replace the anxious ceremony of thread pool tuning. The language, at last, cooperates with what you were trying to say.

But making it possible is not making it easy. The patterns in this book show you how.

---

## The Revelation: Java 26 Types Are RDF Types

Something strange happens when you look at Java 26 carefully alongside RDF and OWL.

A `sealed interface Shape permits Circle, Rectangle, Triangle` is, precisely, an `owl:Class` with `owl:oneOf` — a closed world type hierarchy where the compiler knows the complete enumeration of subtypes. An `owl:Class` is a set of things. `owl:oneOf` closes that set. `sealed` closes the subtype hierarchy. They are the same idea, written in different syntaxes for different audiences.

A `record Point(double x, double y)` is an anonymous RDF blank node with two fixed datatype properties. It is immutable. It is identified entirely by its values, not by an object identity. An RDF blank node *is* a value object — it has no external URI, no mutable state, no identity beyond the sum of its property values. In every domain-modeling sense, they are the same thing.

A virtual thread is a fiber — a coroutine — a suspension point that the runtime manages transparently. In workflow modeling, this is a *deferred execution step*: a unit of work that can yield the thread and resume later without blocking a kernel thread. YAWL's workflow patterns describe exactly this kind of execution model. Project Loom's virtual threads implement it in the JVM.

Once you see these correspondences, you cannot unsee them. The Java type system and the RDF type system are not competing formalisms for expressing domain knowledge — they are the *same formalism*, expressed at different levels of abstraction for different toolchains. The ontology and the Java code are not two things. They are one thing seen from two directions.

This is the revelation that ggen is built on.

---

## The Chatman Equation for Java

**A = μ(O)**

Code precipitates from Ontology. The equation is simple; the insight is not.

When you define your domain in a Turtle file — your entities, their properties, their relationships, their invariants — you are not writing a *description* of your Java types. You are writing your Java types in a notation that is simultaneously more expressive and more machine-readable. The Turtle is the source. The Java is the precipitation. The pipeline is the process by which knowledge crystallizes into running code.

The five-stage pipeline that ggen executes:

1. **Construct** (μ₁) — Load the ontology graph into an in-memory triple store. All entities, properties, hierarchies, and relationships become queryable facts.

2. **Select** (μ₂) — Execute SPARQL queries to extract exactly the triples needed for each generation rule. One query per concern: entities, value objects, repositories, services, controllers.

3. **Render** (μ₃) — Pass the query results to Tera templates. Each template knows one pattern — how to render a JPA entity, or a sealed hierarchy, or a Spring Data repository — and renders it perfectly, every time.

4. **Canonicalize** (μ₄) — Normalize the output: sort imports, apply consistent formatting, ensure deterministic output. The same ontology always produces identical code. Byte-for-byte.

5. **Receipt** (μ₅) — Record a cryptographic audit of what was generated, from which ontology version, with which rule set. The receipt proves provenance.

Applied to Java, the equation becomes concrete: your `domain.ttl` becomes your `@Entity` classes, your `@Repository` interfaces, your `@Service` implementations, your `@RestController` endpoints, your sealed domain event hierarchies, your test scaffolds — all at once, all consistent, all regeneratable from first principles. You do not write these files. You declare the knowledge they encode. The files follow.

The profound consequence: when the domain changes, you change the ontology and re-run the pipeline. You do not hunt down all the places where the old concept appeared and carefully update each one. You update the source of truth, and the code re-precipitates. The code is not the primary artifact. It is the shadow cast by the knowledge.

---

## Why This Book Exists

There is a companion volume to this one: *A Pattern Language for ggen sync* documents the pipeline itself — the 16 patterns of the synchronization process: how ontologies are loaded, how rules are selected, how integrity is verified, how audit receipts are generated. That book teaches you how the pipe works.

This book teaches you what flows through the pipe.

The distinction matters. You can fully understand `ggen sync` — every flag, every pipeline stage, every manifest entry — and still not know what Java 26 patterns to declare in your ontology. The pipe is a mechanism. The patterns are the grammar of what that mechanism can build.

Together, the two books form a complete language: the pipe patterns tell you how to configure the generation system; the Java 26 patterns tell you what to generate. See [*A Pattern Language for ggen sync*](../../sync-patterns/src/README.md) for the pipeline patterns.

This book is the second half of that language.

---

## How to Use This Book

The 35 patterns in this book can be read at two levels.

**The philosophical level** — reading the larger patterns first — gives you the conceptual architecture before the details. Start with Part II (the five three-star super-patterns). These establish the philosophy: why the ontology is the truth, how the pipeline connects ontology to code, what a layered Java 26 architecture looks like, which Java 26 language features the patterns rely on, and how versioning works when code is derived rather than written. Reading these five patterns gives you the grammar; everything else is vocabulary.

**The practical level** — going directly to the leaf patterns — is for readers who learn by doing. If you need a JPA entity today, go to PATTERN 13: JPA ENTITY MAPPING. The pattern will tell you which super-patterns it depends on (follow those links when something doesn't make sense) and which patterns it enables.

Every pattern in this book makes an executable promise. Each one ends with:

- The Turtle snippet you paste into your ontology
- The ggen.toml rule entry that activates it
- The Java 26 code that emerges from `ggen sync`

The three-step invocation ritual is the same everywhere: **copy the TTL → add the ggen.toml rule → run `ggen sync`**. If you have a running ggen installation, every pattern in this book is runnable today. The philosophy and the practice are the same thing.

The star ratings (*, **, ***) follow Alexander's confidence system. Three stars means the pattern is load-bearing — the system loses coherence without it. One star means the pattern is locally useful but optional. Start with the three-star patterns. Let the language emerge.

---

*Code does not have to be written. It only has to be known.*

*When the knowledge is right, the code follows — necessarily, deterministically, completely.*

*That is what this book teaches.*

---
