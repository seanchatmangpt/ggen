<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [A Pattern Language for ggen sync](#a-pattern-language-for-ggen-sync)
  - [What This Book Contains](#what-this-book-contains)
  - [The Central Pattern](#the-central-pattern)
  - [The Quality Without a Name](#the-quality-without-a-name)
  - [How the Patterns Are Organized](#how-the-patterns-are-organized)
    - [Foundation Patterns (1-3)](#foundation-patterns-1-3)
    - [Knowledge Patterns (4-7)](#knowledge-patterns-4-7)
    - [Safety Patterns (8-12)](#safety-patterns-8-12)
    - [Integrity Patterns (13-15)](#integrity-patterns-13-15)
    - [Selective Patterns (16)](#selective-patterns-16)
  - [The Pattern Format](#the-pattern-format)
  - [Begin Here](#begin-here)
  - [The Promise](#the-promise)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# A Pattern Language for ggen sync

> *"Every pattern we define must be formulated in the form of a rule which establishes a relationship between a context, a system of forces which arises in that context, and a configuration which allows these forces to resolve themselves."*
>
> — Christopher Alexander, *A Pattern Language*

---

## What This Book Contains

This book presents **ggen sync** as a pattern language—a connected network of design solutions that, when woven together, create a living system for transforming domain knowledge into code.

Unlike reference documentation that tells you *what* each flag does, this pattern language explains *why* the system is shaped as it is, *when* each pattern applies, and *how* the patterns support one another to create wholeness.

---

## The Central Pattern

At the heart of this language lies a single, powerful idea:

**Code generation should be a synchronization of truth.**

Your domain knowledge—expressed as ontologies, rules, and templates—is the source. The generated code is merely a projection of that truth into a form that machines can execute. When the knowledge changes, the code changes. When the code exists, it reflects exactly what the knowledge declares.

This is what **ggen sync** does. It synchronizes.

---

## The Quality Without a Name

A code generation system possesses the "quality without a name" when:

- **It is alive**: Changes to domain knowledge flow naturally into code changes
- **It is whole**: Every part serves the whole; nothing is extraneous
- **It is comfortable**: Developers trust it, understand it, feel at ease using it
- **It is free**: The system does not constrain; it liberates
- **It is exact**: Outputs are precise, reproducible, deterministic
- **It is egoless**: The system serves the domain, not itself
- **It is eternal**: The patterns remain valid as the system evolves

---

## How the Patterns Are Organized

The patterns in this language form a hierarchy from large to small, from context-setting to detail-implementing:

### Foundation Patterns (1-3)
These establish the overall shape of the system. They answer: *What is ggen sync? How is it structured?*

### Knowledge Patterns (4-7)
These govern how domain truth enters the system and becomes code. They answer: *How does knowledge flow?*

### Safety Patterns (8-12)
These protect developers, systems, and outputs from harm. They answer: *How do we prevent mistakes?*

### Integrity Patterns (13-15)
These ensure trust in the system's outputs. They answer: *How do we know the output is correct?*

### Selective Patterns (16)
These allow focused, intentional operation. They answer: *How do we generate only what we need?*

---

## The Pattern Format

Each pattern follows this structure:

1. **Pattern Name** — A memorable name that evokes the pattern's essence
2. **Confidence** — Stars indicating how well-established the pattern is:
   - `*` — A promising pattern, still evolving
   - `**` — A solid pattern, proven in practice
   - `***` — A fundamental pattern, essential to the language
3. **Context** — The situation in which this pattern applies
4. **Problem** — The forces at play that create tension
5. **Therefore** — The solution that resolves the forces
6. **Connections** — Links to related patterns
7. **Implementation** — How ggen sync realizes this pattern

---

## Begin Here

If you are new to ggen sync, start with these three patterns:

1. **[THE SINGLE COMMAND](patterns/01-single-command.md)** — Understand the unified nature of the system
2. **[MANIFEST AS TRUTH](patterns/02-manifest-as-truth.md)** — See how configuration declares intent
3. **[DETERMINISTIC OUTPUT](patterns/13-deterministic-output.md)** — Grasp why reproducibility matters

Then explore the **[Pattern Map](pattern-map.md)** to find your path through the remaining patterns based on your needs.

---

## The Promise

When you understand and apply these patterns, you will have:

- A generation system you can trust
- Outputs you can reproduce
- A workflow you can explain
- A foundation you can extend

The patterns are the language. The language is the system. The system serves the domain.

Let us begin.
