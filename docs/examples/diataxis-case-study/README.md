<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Diataxis Case Study: Next.js + shadcn/ui + ElectricSQL](#diataxis-case-study-nextjs--shadcnui--electricsql)
  - [🎯 What You'll Learn](#-what-youll-learn)
    - [About Diataxis (Meta-Level)](#about-diataxis-meta-level)
    - [About the Stack (Practical)](#about-the-stack-practical)
  - [📚 The Diataxis Framework](#-the-diataxis-framework)
    - [The 4 Quadrants](#the-4-quadrants)
  - [🗂️ Case Study Structure](#-case-study-structure)
    - [📖 Tutorials - Learning by Doing](#-tutorials---learning-by-doing)
    - [🛠️ How-to Guides - Solve Specific Problems](#-how-to-guides---solve-specific-problems)
    - [💡 Explanations - Understand the Concepts](#-explanations---understand-the-concepts)
    - [📋 Reference - Look Up Details](#-reference---look-up-details)
  - [🎓 Meta-Guide: Learning Diataxis from This Case Study](#-meta-guide-learning-diataxis-from-this-case-study)
    - [How to Use This Case Study](#how-to-use-this-case-study)
      - [Phase 1: Experience All 4 Quadrants (2 hours)](#phase-1-experience-all-4-quadrants-2-hours)
      - [Phase 2: Analyze the Structure (1 hour)](#phase-2-analyze-the-structure-1-hour)
      - [Phase 3: Apply to Your Own Documentation (2+ hours)](#phase-3-apply-to-your-own-documentation-2-hours)
  - [🏗️ The Stack: Why This Example?](#-the-stack-why-this-example)
    - [Complex Concepts Requiring Explanation](#complex-concepts-requiring-explanation)
    - [Practical Tasks for How-tos](#practical-tasks-for-how-tos)
    - [Clear Learning Path for Tutorials](#clear-learning-path-for-tutorials)
    - [Rich Reference Material](#rich-reference-material)
  - [🚀 Quick Start](#-quick-start)
    - [Option 1: Learn Diataxis (Recommended for Doc Writers)](#option-1-learn-diataxis-recommended-for-doc-writers)
    - [Option 2: Build the App (For Developers)](#option-2-build-the-app-for-developers)
    - [Option 3: Validate the Documentation](#option-3-validate-the-documentation)
  - [📊 Documentation Metrics](#-documentation-metrics)
  - [🎯 Learning Outcomes](#-learning-outcomes)
    - [For Documentation Writers](#for-documentation-writers)
    - [For Developers](#for-developers)
  - [🗺️ Navigation Guide](#-navigation-guide)
    - [I want to...](#i-want-to)
  - [🧪 Validation](#-validation)
  - [🔗 Resources](#-resources)
    - [About Diataxis](#about-diataxis)
    - [About the Stack](#about-the-stack)
    - [Related Examples](#related-examples)
  - [🤝 Contributing](#-contributing)
  - [📄 License](#-license)
  - [🎓 Next Steps](#-next-steps)
    - [If you're learning Diataxis:](#if-youre-learning-diataxis)
    - [If you're building with this stack:](#if-youre-building-with-this-stack)
    - [If you're both:](#if-youre-both)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Diataxis Case Study: Next.js + shadcn/ui + ElectricSQL

**Meta-Level Training: Learn Diataxis by Example**

This case study demonstrates how to create comprehensive, user-focused documentation using the Diataxis framework. We use a real-world stack (Next.js + shadcn/ui + ElectricSQL) as the teaching vehicle.

## 🎯 What You'll Learn

### About Diataxis (Meta-Level)
- How to structure documentation into 4 quadrants
- When to write Tutorials vs How-to Guides vs Explanations vs Reference
- How to make documentation discoverable and navigable
- How to validate documentation with executable examples

### About the Stack (Practical)
- Building local-first applications with ElectricSQL
- Creating beautiful UIs with shadcn/ui components
- Implementing offline-first data patterns
- Real-time reactive data synchronization

---

## 📚 The Diataxis Framework

The Diataxis framework organizes documentation into **4 quadrants** based on two axes:

**Learning vs Using** (horizontal axis)
**Practical vs Theoretical** (vertical axis)

```
                    LEARNING ←→ USING

    PRACTICAL ┌──────────────┬──────────────┐
              │              │              │
              │  TUTORIALS   │  HOW-TO      │
              │              │  GUIDES      │
              │              │              │
              ├──────────────┼──────────────┤
              │              │              │
              │ EXPLANATIONS │  REFERENCE   │
              │              │              │
THEORETICAL   └──────────────┴──────────────┘
```

### The 4 Quadrants

1. **Tutorials** (Learning + Practical)
   - Learning-oriented
   - Step-by-step lessons
   - Get the reader started
   - Safe, predictable outcomes

2. **How-to Guides** (Using + Practical)
   - Task-oriented
   - Problem-solving recipes
   - Accomplish specific goals
   - Assumes knowledge

3. **Explanations** (Learning + Theoretical)
   - Understanding-oriented
   - Conceptual discussions
   - Clarify and deepen knowledge
   - Background and context

4. **Reference** (Using + Theoretical)
   - Information-oriented
   - Accurate descriptions
   - Technical specifications
   - Lookup and recall

---

## 🗂️ Case Study Structure

This case study is itself organized using Diataxis:

### 📖 [Tutorials](tutorials/) - Learning by Doing

**Goal**: Get you started building with this stack

- **[Build Your First Local-First Todo App](tutorials/01-first-todo-app.md)** (30 minutes)
  - Set up Next.js project
  - Install shadcn/ui components
  - Connect ElectricSQL
  - Build complete working app
  - **Meta-lesson**: How to write effective tutorials

### 🛠️ [How-to Guides](how-to/) - Solve Specific Problems

**Goal**: Help you accomplish specific tasks

- **[Set Up ElectricSQL Sync](how-to/setup-electric-sync.md)** - Configure Postgres → ElectricSQL sync
- **[Build Forms with shadcn + Zod](how-to/build-forms-zod.md)** - Type-safe form validation
- **[Implement Offline-First Patterns](how-to/offline-first-patterns.md)** - Handle connectivity loss
- **[Optimize Real-Time Performance](how-to/optimize-realtime.md)** - Reduce latency and battery usage
- **Meta-lesson**: How to write task-oriented guides

### 💡 [Explanations](explanations/) - Understand the Concepts

**Goal**: Deepen your understanding of how and why

- **[Local-First Architecture](explanations/local-first-architecture.md)** - Why local-first matters
- **[Reactive Data Sync Patterns](explanations/reactive-sync-patterns.md)** - How ElectricSQL works
- **[Component Composition](explanations/component-composition.md)** - shadcn/ui design philosophy
- **[Offline-First Trade-offs](explanations/offline-tradeoffs.md)** - When to use local-first
- **Meta-lesson**: How to write conceptual explanations

### 📋 [Reference](reference/) - Look Up Details

**Goal**: Provide accurate technical information

- **[ElectricSQL API](reference/electric-api.md)** - Complete API documentation
- **[shadcn Component Catalog](reference/shadcn-components.md)** - All available components
- **[Configuration Options](reference/configuration.md)** - electric.config.js reference
- **[Schema Definitions](reference/schema.md)** - Database schema with migrations
- **Meta-lesson**: How to write reference documentation

---

## 🎓 Meta-Guide: Learning Diataxis from This Case Study

**This case study is designed to teach you Diataxis by showing, not telling.**

### How to Use This Case Study

#### Phase 1: Experience All 4 Quadrants (2 hours)

1. **Start with the Tutorial** (30 min)
   - Follow [Build Your First Todo App](tutorials/01-first-todo-app.md)
   - Notice: Step-by-step, safe, predictable
   - Notice: No explanations of "why", just "do this"
   - **Meta-observation**: Tutorials get users to success quickly

2. **Solve a Problem with How-to** (30 min)
   - Pick a task: [Set Up ElectricSQL Sync](how-to/setup-electric-sync.md)
   - Notice: Assumes you know basics
   - Notice: Focused on one specific goal
   - **Meta-observation**: How-tos are recipes, not lessons

3. **Understand with Explanations** (30 min)
   - Read [Local-First Architecture](explanations/local-first-architecture.md)
   - Notice: No code, just concepts
   - Notice: Comparisons, trade-offs, context
   - **Meta-observation**: Explanations build mental models

4. **Look Up in Reference** (30 min)
   - Browse [ElectricSQL API](reference/electric-api.md)
   - Notice: Dry, complete, accurate
   - Notice: Not trying to teach or explain
   - **Meta-observation**: Reference is for recall, not learning

#### Phase 2: Analyze the Structure (1 hour)

For each document, ask:

**Questions for Tutorials:**
- Does it have a clear learning outcome?
- Is it safe (no destructive operations)?
- Are steps numbered and sequential?
- Does it work start-to-finish?
- Does it avoid explaining concepts?

**Questions for How-tos:**
- Does it solve one specific problem?
- Does it assume prerequisite knowledge?
- Is the solution clearly stated upfront?
- Are there troubleshooting tips?
- Are there related guides linked?

**Questions for Explanations:**
- Does it clarify a concept, not teach a skill?
- Does it provide context and background?
- Are there comparisons to alternatives?
- Does it discuss trade-offs?
- Is it independent of specific tasks?

**Questions for Reference:**
- Is it accurate and complete?
- Is it organized for lookup?
- Does it avoid explanation or instruction?
- Are examples minimal and factual?
- Is it up-to-date with the code?

#### Phase 3: Apply to Your Own Documentation (2+ hours)

**Exercise 1: Convert Existing Docs**

Take one of your existing docs and:
1. Identify what quadrant it belongs in
2. If it mixes quadrants, split it
3. Rewrite to fit quadrant purpose
4. Add cross-links to other quadrants

**Exercise 2: Fill the Gaps**

Audit your documentation:
1. Map existing docs to quadrants
2. Identify empty quadrants
3. Create one document per missing quadrant
4. Link them together

**Exercise 3: Create a Case Study**

Pick your own tech stack and create:
1. One tutorial (30-minute working example)
2. Three how-to guides (common tasks)
3. Two explanations (core concepts)
4. One reference (API or config)

---

## 🏗️ The Stack: Why This Example?

We chose **Next.js + shadcn/ui + ElectricSQL** because it demonstrates:

### Complex Concepts Requiring Explanation
- Local-first architecture
- Reactive data synchronization
- Offline-first patterns
- Conflict resolution

### Practical Tasks for How-tos
- Setting up sync infrastructure
- Building forms with validation
- Handling offline scenarios
- Optimizing performance

### Clear Learning Path for Tutorials
- Can build something useful in 30 minutes
- Progressive complexity
- Immediate visual feedback
- Real-world applicability

### Rich Reference Material
- ElectricSQL API
- shadcn component library
- Configuration options
- Database schemas

This makes it an **ideal teaching vehicle** for Diataxis documentation.

---

## 🚀 Quick Start

### Option 1: Learn Diataxis (Recommended for Doc Writers)

```bash
# Read the meta-guide first
cat docs/examples/diataxis-case-study/META-GUIDE.md

# Experience each quadrant
# 1. Tutorial
cat docs/examples/diataxis-case-study/tutorials/01-first-todo-app.md

# 2. How-to
cat docs/examples/diataxis-case-study/how-to/setup-electric-sync.md

# 3. Explanation
cat docs/examples/diataxis-case-study/explanations/local-first-architecture.md

# 4. Reference
cat docs/examples/diataxis-case-study/reference/electric-api.md
```

### Option 2: Build the App (For Developers)

```bash
# Clone the working example
git clone https://github.com/example/electric-todos
cd electric-todos

# Follow the tutorial
cat TUTORIAL.md

# Build and run
npm install
npm run dev
```

### Option 3: Validate the Documentation

```bash
# Run documentation validation
./scripts/validate-docs/validate-case-study.sh

# This tests:
# - All code examples work
# - Links are valid
# - Examples are complete
# - Database migrations run
```

---

## 📊 Documentation Metrics

This case study includes:

- **1 Tutorial** (30-minute complete project)
- **4 How-to Guides** (specific tasks)
- **4 Explanations** (core concepts)
- **4 Reference Documents** (API, components, config, schema)
- **1 Meta-Guide** (learning Diataxis itself)
- **~150 code examples** (all validated)
- **Working application** (complete Next.js app)

---

## 🎯 Learning Outcomes

### For Documentation Writers

After completing this case study, you will be able to:

✅ **Identify** which quadrant documentation belongs in
✅ **Write** effective tutorials that get users to success
✅ **Create** task-oriented how-to guides
✅ **Explain** concepts without mixing in instructions
✅ **Document** APIs and references accurately
✅ **Structure** documentation for discoverability
✅ **Validate** documentation with executable examples

### For Developers

After completing this case study, you will be able to:

✅ **Build** local-first applications with ElectricSQL
✅ **Create** beautiful UIs with shadcn/ui
✅ **Implement** offline-first data patterns
✅ **Handle** real-time reactive synchronization
✅ **Optimize** performance for local-first apps
✅ **Deploy** ElectricSQL in production

---

## 🗺️ Navigation Guide

### I want to...

**Learn by doing** → Start with [Tutorials](tutorials/)
- Build your first app
- Follow step-by-step
- Get something working

**Solve a specific problem** → Go to [How-to Guides](how-to/)
- Set up sync
- Build forms
- Handle offline

**Understand concepts** → Read [Explanations](explanations/)
- Why local-first?
- How does sync work?
- What are the trade-offs?

**Look up details** → Check [Reference](reference/)
- API documentation
- Component catalog
- Configuration options

**Learn Diataxis itself** → Read [META-GUIDE.md](META-GUIDE.md)
- How this case study was structured
- How to apply Diataxis to your docs
- Exercises and templates

---

## 🧪 Validation

All documentation in this case study is **executable and validated**.

```bash
# Validate entire case study (all quadrants)
./scripts/validate-docs/validate-case-study.sh

# Or run complete documentation validation suite
./scripts/run-validation-suite.sh
```

**Why validate documentation?**

This case study demonstrates **test-driven documentation**:
- Every code example must work
- Every tutorial must complete successfully
- Every how-to must solve the stated problem
- Every reference must match the actual API

This is itself a **meta-lesson**: Good documentation is tested documentation.

---

## 🔗 Resources

### About Diataxis
- [Diataxis.fr](https://diataxis.fr/) - Official Diataxis documentation
- [Divio Blog](https://www.divio.com/blog/) - Creators of Diataxis

### About the Stack
- [Next.js](https://nextjs.org/) - React framework
- [shadcn/ui](https://ui.shadcn.com/) - Component library
- [ElectricSQL](https://electric-sql.com/) - Local-first sync layer
- [Zod](https://zod.dev/) - TypeScript-first schema validation

### Related Examples
- [ggen Quick Start](../getting-started/quick-start.md) - Another Diataxis tutorial
- [ggen How-to Guides](../how-to/) - Task-oriented documentation
- [ggen Explanations](../explanations/) - Conceptual documentation

---

## 🤝 Contributing

Want to improve this case study?

1. **Add examples** to existing documents
2. **Create new how-to guides** for common tasks
3. **Write explanations** for complex concepts
4. **Improve reference** accuracy and completeness
5. **Fix broken examples** (all must validate)

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

---

## 📄 License

MIT License - Use this case study to learn and teach Diataxis.

---

## 🎓 Next Steps

### If you're learning Diataxis:

1. ✅ Read this overview (you're here!)
2. ➡️ Read [META-GUIDE.md](META-GUIDE.md) for detailed learning path
3. → Experience all 4 quadrants
4. → Analyze the structure
5. → Apply to your own docs

### If you're building with this stack:

1. ✅ Read this overview (you're here!)
2. ➡️ Start [Tutorial 01: First Todo App](tutorials/01-first-todo-app.md)
3. → Build the complete application
4. → Explore how-to guides for specific tasks
5. → Read explanations to deepen understanding

### If you're both:

You're in the perfect place! This case study teaches Diataxis **by showing**, not just telling.

---

**Remember**: Good documentation makes the difference between a tool that's used and a tool that's abandoned.

Diataxis helps you create documentation that users actually want to read.
