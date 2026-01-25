<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Preface](#preface)
  - [Why This Book Exists](#why-this-book-exists)
  - [What This Book Is](#what-this-book-is)
  - [What This Book Is Not](#what-this-book-is-not)
  - [Who This Book Is For](#who-this-book-is-for)
  - [How This Book Is Organized](#how-this-book-is-organized)
    - [Part I: Foundations](#part-i-foundations)
    - [Part II: The Pattern Language](#part-ii-the-pattern-language)
    - [Part III: Patterns in Practice](#part-iii-patterns-in-practice)
    - [Part IV: Advanced Topics](#part-iv-advanced-topics)
    - [Part V: Reference](#part-v-reference)
  - [How to Read This Book](#how-to-read-this-book)
    - [The Sequential Path](#the-sequential-path)
    - [The Explorer's Path](#the-explorers-path)
    - [The Problem Solver's Path](#the-problem-solvers-path)
    - [The Practitioner's Path](#the-practitioners-path)
  - [The Confidence Stars](#the-confidence-stars)
  - [A Living Document](#a-living-document)
  - [Acknowledgments](#acknowledgments)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Preface

> *"The elements of this language are entities called patterns. Each pattern describes a problem which occurs over and over again in our environment, and then describes the core of the solution to that problem, in such a way that you can use this solution a million times over, without ever doing it the same way twice."*
>
> — Christopher Alexander, *A Pattern Language* (1977)

---

## Why This Book Exists

In the autumn of 2024, we faced a crisis of complexity. The ggen code generation tool had grown from a simple utility into a sprawling system with 47 distinct commands. Users complained that they couldn't remember which command to use when. Documentation sprawled across dozens of pages. The mental model required to use the tool effectively had become a barrier rather than an aid.

We needed to simplify. But simplification is not merely subtraction—removing features would break existing workflows. True simplification required reconceptualizing the entire system around a unifying principle.

That principle was **synchronization**.

We realized that every command in ggen was, at its core, attempting to synchronize domain knowledge with generated code. Some commands loaded ontologies. Some validated configurations. Some generated files. Some previewed changes. But all were aspects of a single underlying activity: keeping code in sync with the truth expressed in ontologies.

This insight led to the creation of `ggen sync`—a single command that does everything. The 47 commands became one command with thoughtful flags. The sprawling documentation became a pattern language.

This book documents that pattern language.

---

## What This Book Is

This book is a **pattern language** for code synchronization using ggen sync. It is written in the style of Christopher Alexander's seminal work, *A Pattern Language: Towns, Buildings, Construction* (1977).

Alexander's book presented 253 patterns for architecture and urban planning, ranging from large-scale patterns like "Independent Regions" to small-scale patterns like "Window Place." Each pattern described a problem, the forces at play, and a solution. The patterns formed a language—a coherent system where patterns supported and referenced one another.

This book applies Alexander's approach to software: specifically, to the design and use of a code generation system. We present 16 patterns ranging from large-scale architectural decisions (THE SINGLE COMMAND) to small-scale operational details (RULE SELECTION). Together, they form a complete language for understanding and using ggen sync.

---

## What This Book Is Not

This book is **not** a reference manual. If you need to know the exact syntax of a configuration option, consult the reference appendices or the `ggen --help` output.

This book is **not** a tutorial. If you need step-by-step instructions for your first project, see the Quick Start guide in the main ggen documentation.

This book is **not** a Gang of Four design patterns book. We are not documenting software design patterns like Singleton, Factory, or Observer. We are documenting a pattern language in Alexander's sense—a connected network of solutions to recurring problems in a specific domain.

---

## Who This Book Is For

This book is for anyone who wants to deeply understand ggen sync:

**For Users** who want to understand not just *how* to use the tool, but *why* it is designed the way it is. Understanding the patterns will help you use the tool more effectively and troubleshoot problems when they arise.

**For Contributors** who want to extend or modify ggen. Understanding the patterns will help you make changes that align with the system's philosophy rather than fighting against it.

**For Architects** who are designing their own code generation systems. Even if you're not using ggen, the patterns here may inform your design decisions.

**For Learners** who are curious about pattern languages as an approach to software documentation and design. This book serves as an example of how Alexander's ideas can be applied to software systems.

---

## How This Book Is Organized

The book is divided into five parts:

### Part I: Foundations

This section establishes the philosophical and historical context for the pattern language. You'll learn what a pattern language is, why we chose this approach for documenting ggen sync, and the history behind the tool's evolution.

If you're eager to dive into the patterns themselves, you can skip this section and return to it later. But we recommend at least reading the Introduction, which explains the central insight behind ggen sync.

### Part II: The Pattern Language

This is the heart of the book—the 16 patterns that comprise the language. The patterns are organized into five categories:

- **Foundation Patterns (1-3)**: The large-scale patterns that establish the overall structure of the system.
- **Knowledge Patterns (4-7)**: The patterns that govern how domain knowledge flows through the pipeline and becomes code.
- **Safety Patterns (8-12)**: The patterns that protect users and systems from harm.
- **Integrity Patterns (13-15)**: The patterns that ensure trust and reproducibility.
- **Selective Patterns (16)**: The patterns that allow focused operation.

Each pattern follows a consistent format: context, forces, solution, connections, and implementation details.

### Part III: Patterns in Practice

This section applies the patterns to real-world scenarios. You'll find anti-patterns (what not to do), case studies (how others have used the patterns), worked examples (complete from-scratch implementations), and a troubleshooting guide (what to do when things go wrong).

### Part IV: Advanced Topics

This section covers specialized topics for experts: custom extensions, CI/CD integration, and future directions for the system.

### Part V: Reference

Complete reference materials including the pattern map, glossary, manifest reference, and quick reference guides for SPARQL and Tera templates.

---

## How to Read This Book

There is no single correct way to read this book. Here are several approaches:

### The Sequential Path

Read from beginning to end. This approach gives you the full philosophical context before diving into patterns, and presents patterns from large-scale to small-scale. It's the most thorough approach but also the most time-consuming.

### The Explorer's Path

Start with the Pattern Map (Part V), identify a pattern that interests you, read that pattern, then follow its connections to related patterns. This approach is good for getting a quick understanding of the system's shape.

### The Problem Solver's Path

Start with the Troubleshooting Guide (Part III). Find the problem you're experiencing, then follow references to the relevant patterns. This approach is good when you have an immediate need.

### The Practitioner's Path

Start with the Case Studies and Worked Examples (Part III). See how patterns are applied in practice, then read the patterns that appear most relevant to your situation.

---

## The Confidence Stars

Each pattern in this book is marked with one to three stars:

- ★ — An **emerging** pattern. This solution is promising but still evolving. Use it, but expect refinements in future versions.

- ★★ — An **established** pattern. This solution has proven itself in practice. You can rely on it.

- ★★★ — A **fundamental** pattern. This solution is essential to the language. Without it, the system loses coherence.

The confidence level reflects how deeply the pattern is woven into the fabric of ggen sync, not how "important" it is. An emerging pattern may solve a critical problem; a fundamental pattern may seem obvious in retrospect.

---

## A Living Document

Pattern languages are not static. They evolve as the systems they describe evolve, and as practitioners discover new patterns through experience.

This book represents the pattern language as of ggen v5.0. Future versions of ggen may introduce new patterns, elevate emerging patterns to established status, or (rarely) deprecate patterns that no longer apply.

If you discover a pattern that isn't documented here—a recurring solution to a recurring problem that you've found valuable—we encourage you to contribute it. See Appendix H for guidelines on proposing new patterns.

---

## Acknowledgments

This book would not exist without the work of Christopher Alexander, whose *A Pattern Language* and *The Timeless Way of Building* provided the intellectual foundation for this approach. We are also indebted to the software patterns community that has applied Alexander's ideas to software over the past three decades.

We thank the ggen contributors who built the system documented here, the early users who provided feedback that shaped its evolution, and everyone who reviewed drafts of this pattern language.

Most of all, we thank you, the reader, for taking the time to understand not just how to use ggen sync, but why it exists and how it works. May this understanding serve you well.

---

*Let us begin.*
