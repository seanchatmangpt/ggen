<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Meta-Guide: Learning Diataxis from This Case Study](#meta-guide-learning-diataxis-from-this-case-study)
  - [Purpose of This Guide](#purpose-of-this-guide)
  - [Learning Path (4 Hours Total)](#learning-path-4-hours-total)
    - [Phase 1: Experience (2 hours)](#phase-1-experience-2-hours)
      - [1.1 Complete the Tutorial (30 min)](#11-complete-the-tutorial-30-min)
      - [1.2 Solve a Problem with How-to (30 min)](#12-solve-a-problem-with-how-to-30-min)
      - [1.3 Understand with Explanation (30 min)](#13-understand-with-explanation-30-min)
      - [1.4 Look Up in Reference (30 min)](#14-look-up-in-reference-30-min)
    - [Phase 2: Analyze (1 hour)](#phase-2-analyze-1-hour)
      - [2.1 Map Documents to Quadrants (15 min)](#21-map-documents-to-quadrants-15-min)
      - [2.2 Analyze Tutorial Structure (15 min)](#22-analyze-tutorial-structure-15-min)
      - [2.3 Analyze How-to Structure (15 min)](#23-analyze-how-to-structure-15-min)
      - [2.4 Identify Cross-Links (15 min)](#24-identify-cross-links-15-min)
    - [Phase 3: Apply (1 hour)](#phase-3-apply-1-hour)
      - [3.1 Choose Your Stack (5 min)](#31-choose-your-stack-5-min)
      - [3.2 Write One Document Per Quadrant (55 min)](#32-write-one-document-per-quadrant-55-min)
  - [Step 2: &#91;Next Action&#93;](#step-2-next-action)
  - [🎉 Congratulations!](#-congratulations)
    - [What You Learned](#what-you-learned)
    - [Next Steps](#next-steps)
  - [API](#api)
    - [`functionName(params)`](#functionnameparams)
  - [Configuration](#configuration)
    - [Phase 4: Validate (30 min)](#phase-4-validate-30-min)
      - [4.1 Self-Review Checklist](#41-self-review-checklist)
      - [4.2 User Testing](#42-user-testing)
      - [4.3 Revision](#43-revision)
  - [Diataxis Patterns Demonstrated](#diataxis-patterns-demonstrated)
    - [Pattern 1: Tutorial Flow](#pattern-1-tutorial-flow)
    - [Pattern 2: How-to Flow](#pattern-2-how-to-flow)
    - [Pattern 3: Explanation Flow](#pattern-3-explanation-flow)
    - [Pattern 4: Reference Flow](#pattern-4-reference-flow)
  - [Common Mistakes (Anti-Patterns)](#common-mistakes-anti-patterns)
    - [❌ Mixing Quadrants](#-mixing-quadrants)
    - [❌ Explanation With Step-by-Step](#-explanation-with-step-by-step)
    - [❌ Reference That Explains](#-reference-that-explains)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Meta-Guide: Learning Diataxis from This Case Study

**How to use this case study to master the Diataxis documentation framework**

---

## Purpose of This Guide

This case study is a **learning tool** that teaches Diataxis by showing, not telling. You'll experience all four documentation quadrants (Tutorial, How-to, Explanation, Reference) and learn how to create each type yourself.

**By the end of this meta-guide**, you will be able to:

✅ Identify which quadrant documentation belongs in
✅ Write effective tutorials, how-tos, explanations, and reference docs
✅ Structure documentation for maximum user value
✅ Apply Diataxis to your own projects

---

## Learning Path (4 Hours Total)

### Phase 1: Experience (2 hours)

**Goal**: Experience all 4 quadrants as a user

#### 1.1 Complete the Tutorial (30 min)

**Action**: Follow [Tutorial: Build Your First Todo App](tutorials/01-first-todo-app.md)

**As you work, notice:**
- ✅ Steps are numbered and sequential
- ✅ Every step works (no guessing)
- ✅ Explanations are minimal (just enough)
- ✅ Outcome is predictable (working app)
- ✅ You don't need deep knowledge to start

**Questions to ask:**
1. Did I successfully complete the tutorial?
2. Was I ever stuck or confused?
3. Did I learn by doing, or by reading?
4. Could I repeat this without the tutorial?

**Meta-observation**: Tutorials are **learning-oriented** and **practical**. They get users to success quickly.

#### 1.2 Solve a Problem with How-to (30 min)

**Action**: Follow [How-to: Setup ElectricSQL Sync](how-to/setup-electric-sync.md)

**As you work, notice:**
- ✅ Problem stated upfront
- ✅ Assumes you know basics (not from scratch)
- ✅ Focused on one specific goal
- ✅ Troubleshooting included
- ✅ Production considerations mentioned

**Questions to ask:**
1. Did this solve my specific problem?
2. Did it assume I knew the basics?
3. Could I apply this to my own project?
4. Were alternatives or trade-offs discussed?

**Meta-observation**: How-tos are **task-oriented** and **practical**. They assume knowledge and focus on solutions.

#### 1.3 Understand with Explanation (30 min)

**Action**: Read [Explanation: Local-First Architecture](explanations/local-first-architecture.md)

**As you work, notice:**
- ✅ No code examples (or minimal)
- ✅ Discusses "why" not "how"
- ✅ Compares alternatives
- ✅ Explores trade-offs
- ✅ Builds mental models

**Questions to ask:**
1. Do I understand WHY local-first matters?
2. Can I explain the concept to someone else?
3. Did this change how I think about architecture?
4. Was I trying to find instructions (wrong doc type)?

**Meta-observation**: Explanations are **understanding-oriented** and **theoretical**. They clarify concepts.

#### 1.4 Look Up in Reference (30 min)

**Action**: Browse [Reference: ElectricSQL API](reference/electric-api.md)

**As you work, notice:**
- ✅ Dry, factual descriptions
- ✅ Complete API coverage
- ✅ Organized for lookup (not reading)
- ✅ Minimal examples (just syntax)
- ✅ No explanations of concepts

**Questions to ask:**
1. Could I find what I was looking for quickly?
2. Was the information accurate and complete?
3. Did I learn new concepts (if yes, wrong doc type)?
4. Is this organized for recall, not discovery?

**Meta-observation**: Reference is **information-oriented** and **theoretical**. It's for lookup, not learning.

---

### Phase 2: Analyze (1 hour)

**Goal**: Understand the structure and patterns

#### 2.1 Map Documents to Quadrants (15 min)

**Action**: Create this table for all documents in the case study

| Document | Quadrant | Why? |
|----------|----------|------|
| First Todo App | Tutorial | Learning-oriented, step-by-step |
| Setup Sync | How-to | Task-oriented, assumes knowledge |
| Local-First | Explanation | Understanding-oriented, no instructions |
| Electric API | Reference | Information-oriented, lookup |

**Questions to ask:**
1. Does each document clearly fit ONE quadrant?
2. Are there documents that mix quadrants? (Anti-pattern!)
3. Are all 4 quadrants represented?

#### 2.2 Analyze Tutorial Structure (15 min)

**Action**: Re-read [Tutorial 01](tutorials/01-first-todo-app.md) and answer:

| Tutorial Checklist | Yes/No | Evidence |
|-------------------|--------|----------|
| Clear learning outcome? | | |
| Safe, predictable steps? | | |
| Numbered sequence? | | |
| Works start-to-finish? | | |
| Minimal explanations? | | |
| Avoids choices/alternatives? | | |
| Includes "What you learned"? | | |

**Meta-lesson**: Good tutorials follow this pattern consistently.

#### 2.3 Analyze How-to Structure (15 min)

**Action**: Re-read [How-to: Setup Sync](how-to/setup-electric-sync.md) and answer:

| How-to Checklist | Yes/No | Evidence |
|-----------------|--------|----------|
| Problem stated upfront? | | |
| Assumes prerequisites? | | |
| Solution-focused? | | |
| One specific task? | | |
| Troubleshooting section? | | |
| Links to related guides? | | |
| Production considerations? | | |

**Meta-lesson**: How-tos are recipes for people who know the basics.

#### 2.4 Identify Cross-Links (15 min)

**Action**: Find all cross-references between quadrants

**Example**:
- Tutorial → Links to How-to (next steps)
- How-to → Links to Explanation (understand concepts)
- Explanation → Links to Reference (API details)
- Reference → Links to Tutorial (getting started)

**Questions to ask:**
1. Do cross-links flow naturally?
2. Can users navigate between quadrants easily?
3. Are links contextual (relevant to user's goal)?

**Meta-lesson**: Good documentation guides users between quadrants.

---

### Phase 3: Apply (1 hour)

**Goal**: Create your own Diataxis documentation

#### 3.1 Choose Your Stack (5 min)

Pick a technology stack you know well:
- React + Redux + Firebase
- Vue + Pinia + Supabase
- Python + FastAPI + SQLAlchemy
- Rust + Axum + SeaORM
- etc.

**Requirements:**
- You're comfortable with it
- It has at least 3 components
- There are concepts to explain

#### 3.2 Write One Document Per Quadrant (55 min)

**Tutorial (15 min)**

**Template:**

```markdown
# Tutorial: [Outcome]

**Time**: X minutes | **Level**: Beginner

## What You'll Build
[Specific outcome]

## Step 1: [Action]
```bash
[Command]
```

**What just happened?**
[Brief explanation]

## Step 2: [Next Action]
...

## 🎉 Congratulations!

### What You Learned
- ✅ [Skill 1]
- ✅ [Skill 2]

### Next Steps
- [How-to guide]
- [Explanation]
```

**How-to (15 min)**

**Template:**

```markdown
# How-to: [Task]

**Problem**: [What user wants to do]

**Solution**: [High-level approach]

**Time**: X minutes | **Level**: Intermediate

## Prerequisites
- [Prerequisite 1]
- [Prerequisite 2]

## Step 1: [Action]
[Instructions]

## Step 2: [Action]
...

## Troubleshooting
### Issue: [Problem]
**Solution**: [Fix]

## Related Guides
- [Related how-to]
```

**Explanation (15 min)**

**Template:**

```markdown
# Explanation: [Concept]

**Understanding [concept]**

## What is [Concept]?
[Definition]

## Why [Concept] Matters
[Motivation]

## How [Concept] Works
[Architecture/mechanics]

## Trade-offs
### Advantages
- ✅ [Pro 1]

### Challenges
- ⚠️ [Con 1]

## When to Use [Concept]
✅ Great for: [Use case]
❌ Not ideal for: [Anti-use case]

## Summary
[Key takeaways]
```

**Reference (10 min)**

**Template:**

```markdown
# Reference: [API/Config]

Complete reference for [thing].

## Installation
```bash
[Install command]
```

## API

### `functionName(params)`

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `param1` | `string` | Yes | [Description] |

**Returns:** `Type`

**Example:**

```javascript
functionName('value');
```

## Configuration

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `option1` | `string` | `null` | [Description] |
```

---

### Phase 4: Validate (30 min)

**Goal**: Test your documentation like this case study

#### 4.1 Self-Review Checklist

For each document you created:

**Tutorial:**
- [ ] Can a beginner complete it?
- [ ] Does every step work?
- [ ] Are explanations minimal?
- [ ] Is the outcome clear?

**How-to:**
- [ ] Is the problem specific?
- [ ] Does it assume knowledge?
- [ ] Is troubleshooting included?
- [ ] Are related guides linked?

**Explanation:**
- [ ] Does it clarify concepts?
- [ ] Are trade-offs discussed?
- [ ] Is it independent of tasks?
- [ ] Does it build mental models?

**Reference:**
- [ ] Is it accurate and complete?
- [ ] Is it organized for lookup?
- [ ] Are examples minimal?
- [ ] Is it up-to-date?

#### 4.2 User Testing

**Action**: Give your docs to a colleague and observe:

1. **Give them a task**: "Set up [your stack] using these docs"
2. **Watch them work** (don't help!)
3. **Note where they struggle**
4. **Ask**: "Which document did you use first? Why?"

**Common findings:**
- Users skip tutorials (make benefits clearer)
- Users get stuck in how-tos (add prerequisites)
- Users skip explanations (link from how-tos)
- Users can't find reference info (improve organization)

#### 4.3 Revision

**Action**: Fix issues found in user testing

**Common fixes:**
- Split documents that mix quadrants
- Add cross-links between quadrants
- Clarify prerequisites
- Improve navigation

---

## Diataxis Patterns Demonstrated

### Pattern 1: Tutorial Flow

```
Clear outcome → Prerequisites → Step 1 → "What happened?" →
Step 2 → ... → Congratulations → What you learned → Next steps
```

**See**: [Tutorial 01](tutorials/01-first-todo-app.md)

### Pattern 2: How-to Flow

```
Problem → Solution → Prerequisites → Step 1 → Step 2 → ... →
Verification → Troubleshooting → Production tips → Related guides
```

**See**: [How-to: Setup Sync](how-to/setup-electric-sync.md)

### Pattern 3: Explanation Flow

```
What → Why → How (high-level) → Trade-offs →
When to use → Examples → Summary
```

**See**: [Explanation: Local-First](explanations/local-first-architecture.md)

### Pattern 4: Reference Flow

```
Installation → API Overview → Function 1 (params, returns, example) →
Function 2 → ... → Configuration → Types → Errors
```

**See**: [Reference: Electric API](reference/electric-api.md)

---

## Common Mistakes (Anti-Patterns)

### ❌ Mixing Quadrants

**Bad**: Tutorial that explains concepts deeply

```markdown
# Tutorial: Todo App

## Step 1: Install ElectricSQL

ElectricSQL uses CRDTs (Conflict-Free Replicated Data Types)
which are special data structures that allow for automatic
conflict resolution. CRDTs come in two flavors: state-based
and operation-based. State-based CRDTs merge entire states...

[500 words of CRDT explanation]
```

**Good**: Tutorial that refers to explanation

```markdown
# Tutorial: Todo App

## Step 1: Install ElectricSQL

```bash
npm install electric-sql
```

**What just happened?**
You installed Electric, which handles sync automatically.

**Want to understand how sync works?**
See [Explanation: Reactive Sync](../explanations/sync.md)
```

### ❌ How-to That Teaches Basics

**Bad**: How-to assumes no knowledge

```markdown
# How-to: Add Authentication

## What is Authentication?
Authentication is the process of verifying identity...

## What is a JWT?
JSON Web Tokens (JWT) are...

[Teaching from scratch]
```

**Good**: How-to assumes prerequisites

```markdown
# How-to: Add Authentication

**Problem**: Add JWT authentication to Electric app

**Prerequisites:**
- Completed [Tutorial: First App](../tutorials/01.md)
- Understanding of JWT tokens
- Authentication provider account

## Step 1: Install Auth Library
...
```

### ❌ Explanation With Step-by-Step

**Bad**: Explanation with instructions

```markdown
# Explanation: Local-First

## How to Build Local-First Apps

Step 1: Install SQLite
```bash
npm install wa-sqlite
```

Step 2: Create database...
```

**Good**: Explanation with concepts

```markdown
# Explanation: Local-First

## What is Local-First?

Local-first software keeps data on the device...

## Why It Matters

Performance, offline capability, ownership...

[No step-by-step instructions]
```

### ❌ Reference That Explains

**Bad**: Reference with explanations

```markdown
# Reference: API

## syncTables(tables)

This function is really important because it enables
real-time synchronization which is the core feature of
local-first architecture. It works by establishing a
WebSocket connection...

[Explaining instead of documenting]
```

**Good**: Reference that documents

```markdown
# Reference: API

## syncTables(tables)

Start syncing specified tables.

**Parameters:**
- `tables` (string[]): Table names

**Returns:** Promise<SyncShape>

**Example:**
```javascript
await sync.syncTables(['todos']);
```
```

---

## Exercises

### Exercise 1: Classify Documentation

**Classify these snippets** into Tutorial, How-to, Explanation, or Reference:

1. "Step 1: Install Next.js. Step 2: Create app directory..."
2. "The component lifecycle consists of mounting, updating, and unmounting phases..."
3. "`useState(initialValue)` - Returns stateful value and updater function"
4. "Problem: Add dark mode. Solution: Use CSS variables..."

**Answers:**
1. Tutorial (step-by-step, learning)
2. Explanation (conceptual, understanding)
3. Reference (API documentation)
4. How-to (problem-solution)

### Exercise 2: Fix Mixed Quadrant

**This document mixes quadrants. Identify which quadrants and how to split:**

```markdown
# Tutorial: Build REST API

Step 1: Install FastAPI
```bash
pip install fastapi
```

REST APIs use the HTTP protocol, which is a stateless
request-response model. In REST, resources are identified
by URLs and manipulated using HTTP methods like GET, POST...

[300 words of REST explanation]

Step 2: Create endpoint
```python
@app.get("/users")
def get_users():
    return db.query(User).all()
```

Common error: "Connection refused"
Solution: Check database is running
```

**Answer:**

This mixes **Tutorial** (steps) with **Explanation** (REST concepts) and **How-to** (troubleshooting).

**Split into:**
1. **Tutorial**: Just the steps, link to explanation
2. **Explanation**: REST concepts (separate document)
3. **How-to**: Troubleshooting (separate or reference)

### Exercise 3: Write Missing Quadrant

**This project has:**
- ✅ Tutorial: Build first app
- ✅ How-to: Deploy to production
- ❌ Explanation: ???
- ✅ Reference: API docs

**Write the missing Explanation** (100 words):

**Topic**: Why use server-side rendering?

[Your answer here]

**Good answer includes:**
- What SSR is
- Why it matters (SEO, performance)
- Trade-offs (complexity, cost)
- When to use (content sites) vs not (dashboards)

---

## Measuring Success

### Your Documentation is Good When...

**For Tutorials:**
- [ ] 90%+ completion rate
- [ ] Users successfully build working project
- [ ] Average completion time matches estimate
- [ ] Users proceed to how-tos afterward

**For How-tos:**
- [ ] Problem is solved in < 30 minutes
- [ ] Low support requests for this task
- [ ] Users reference it repeatedly
- [ ] Clear when to use vs other guides

**For Explanations:**
- [ ] Users report "aha!" moments
- [ ] Reduces conceptual questions in support
- [ ] Referenced in discussions/forums
- [ ] Helps users make architectural decisions

**For Reference:**
- [ ] Fast lookup (users find info in < 2 minutes)
- [ ] 100% accuracy (matches actual API)
- [ ] Complete (all features documented)
- [ ] Up-to-date (version matches)

---

## Next Steps

### Continue Learning

1. **Read Diataxis.fr** - Official framework documentation
2. **Study real examples** - Stripe, Django, Vue.js docs
3. **Practice regularly** - Write one doc per week
4. **Get feedback** - User test your docs

### Apply to Your Project

1. **Audit existing docs** - Map to quadrants
2. **Identify gaps** - Which quadrants are missing?
3. **Refactor** - Split documents that mix quadrants
4. **Add cross-links** - Connect the quadrants
5. **Validate** - Test with users

### Share Your Experience

1. **Blog about it** - Share what you learned
2. **Create case studies** - Document your process
3. **Help others** - Review their documentation
4. **Contribute** - Improve open source docs

---

## Summary

**Diataxis is a framework, not a rule book.**

The goal is **user-centered documentation** that:
- ✅ Matches user needs (learning vs using)
- ✅ Is easy to navigate (clear quadrants)
- ✅ Provides value (practical vs theoretical)
- ✅ Scales (add docs without confusion)

**This case study taught you by showing:**
- Experience all 4 quadrants
- Analyze the structure
- Apply to your own project
- Validate with users

**Now go create documentation that users actually want to read!**

---

## Resources

### Official Diataxis
- [Diataxis.fr](https://diataxis.fr/) - Framework documentation
- [Divio Blog](https://www.divio.com/blog/) - Case studies

### Examples of Good Diataxis Docs
- [Django](https://docs.djangoproject.com/) - Python web framework
- [Stripe](https://stripe.com/docs) - Payment API
- [Vue.js](https://vuejs.org/) - JavaScript framework
- [Rust Book](https://doc.rust-lang.org/book/) - Programming language

### This Case Study
- [Case Study README](README.md) - Overview
- [Tutorial 01](tutorials/01-first-todo-app.md) - First app
- [How-to: Setup Sync](how-to/setup-electric-sync.md) - Add sync
- [Explanation: Local-First](explanations/local-first-architecture.md) - Architecture
- [Reference: Electric API](reference/electric-api.md) - API docs

---

**Remember**: Good documentation is not about writing. It's about understanding what users need and giving it to them in the right format.
