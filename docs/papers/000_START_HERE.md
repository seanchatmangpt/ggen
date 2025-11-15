# START HERE: Academic Paper with ggen (5 Minutes)

**Welcome!** You're about to create a professional academic paper that automatically manages equations, cross-references, and formatting.

---

## 🎯 What You're About to Do

In the next **5 minutes**, you will:
1. ✅ Copy a minimal example
2. ✅ Generate your first PDF
3. ✅ Understand how it works
4. ✅ Know where to go next

**Guarantee**: This will work. No prerequisites, no setup needed.

---

## ⚡ The 5-Minute Start

### Step 1: Copy the Minimal Example (1 minute)

Copy this entire block to a file named `my-paper.rdf`:

```turtle
# Minimal Academic Paper
@prefix : <http://example.org/paper#> .
@prefix ap: <http://example.org/academic-paper#> .
@prefix math: <http://example.org/math#> .

:MyPaper a ap:Paper ;
    ap:title "My First Academic Paper" ;
    ap:author "Your Name" ;
    ap:date "2025-11-15" ;
    ap:abstract "This is a minimal example paper demonstrating semantic equation generation." ;
    ap:hasSection :Section1 ;
    math:hasEquation :Equation1 .

:Section1 a ap:Section ;
    ap:sectionNumber 1 ;
    ap:sectionTitle "Introduction" ;
    ap:sectionContent "This paper introduces a new concept." .

:Equation1 a math:Equation ;
    ap:equationName "core_equation" ;
    ap:equationLatex "E = mc^2" ;
    math:equationNumber 1 ;
    ap:equationOrder 1 ;
    math:description "The famous mass-energy equivalence equation by Einstein." .
```

✅ **DONE**: File `my-paper.rdf` created

### Step 2: Generate LaTeX (2 minutes)

Run this command in your terminal:

```bash
ggen paper generate my-paper.rdf --style arxiv --output my-paper.tex
```

**Expected output**:
```
✅ Validating paper...
✅ Generating LaTeX...
✅ Paper generated: my-paper.tex (45 lines)
```

✅ **DONE**: File `my-paper.tex` created

### Step 3: Compile to PDF (1 minute)

Run this command:

```bash
pdflatex my-paper.tex
```

**Expected output**:
```
This is pdfTeX, Version 3.14159...
...
Output written on my-paper.pdf (2 pages).
```

✅ **DONE**: File `my-paper.pdf` created

**Open `my-paper.pdf` in your PDF reader. You have a working academic paper!** 🎉

---

## 🤔 What Just Happened?

### The Magic of Semantic Generation

```
my-paper.rdf (source of truth)
    ↓
    [SPARQL extraction]
    ↓
my-paper.tex (auto-generated)
    ↓
    [pdflatex]
    ↓
my-paper.pdf (final paper)
```

**Key insight**: You edited RDF. Everything else happened automatically.

### The Three Files Explained

| File | What It Is | Do You Edit It? |
|------|-----------|-----------------|
| `my-paper.rdf` | Your paper in RDF format | ✅ YES - always edit this |
| `my-paper.tex` | LaTeX (auto-generated) | ❌ NO - never edit this |
| `my-paper.pdf` | PDF (auto-generated) | ❌ NO - just read this |

**CRITICAL RULE**: Edit `my-paper.rdf`. When you want a new PDF, just run `ggen paper generate` again. Everything updates automatically.

---

## 📚 What's Next?

### Option 1: Learn More (Read Next)

Choose your experience level:

- **Beginner** (Never used academic papers): Read `QUICKSTART.md` (10 minutes)
- **Intermediate** (Know LaTeX): Read `ACADEMIC_PAPER_LIFECYCLE.md` (1 hour)
- **Expert** (Want full details): Read `SEMANTIC-EQUATION-GENERATION-DEMO.md` (2 hours)

### Option 2: Customize Your Paper (Do Next)

**Add more equations**:

Edit `my-paper.rdf` and add:

```turtle
:Equation2 a math:Equation ;
    ap:equationName "pythagoras" ;
    ap:equationLatex "a^2 + b^2 = c^2" ;
    math:equationNumber 2 ;
    ap:equationOrder 2 ;
    math:description "The Pythagorean theorem." .
```

Then run:
```bash
ggen paper generate my-paper.rdf --style arxiv --output my-paper.tex
pdflatex my-paper.tex
```

✅ New PDF with Equation 2 is created. Notice: equation automatically numbered!

---

## ⚠️ Common Mistakes (How to Avoid Them)

### Mistake 1: "I edited the .tex file"

❌ **Don't do this**:
```bash
# DO NOT EDIT my-paper.tex directly
# It will be overwritten when you regenerate
```

✅ **Do this instead**:
```bash
# Edit my-paper.rdf
# Then regenerate: ggen paper generate my-paper.rdf
```

**Why**: `my-paper.rdf` is the source of truth. LaTeX is temporary.

### Mistake 2: "My equation didn't renumber"

❌ **Problem**: You added Equation 5 but numbers still go 1,2,3,4 (skipping 5)

✅ **Solution**: Check `ap:equationOrder`. It must be sequential:

```turtle
:Equation1 a math:Equation ;
    ap:equationOrder 1 .   # ← Must be 1

:Equation2 a math:Equation ;
    ap:equationOrder 2 .   # ← Must be 2

:Equation5 a math:Equation ;
    ap:equationOrder 3 .   # ← Must be 3, even though name says "5"
```

**The rule**: `ap:equationOrder` determines the sequence. `math:equationNumber` is auto-assigned.

### Mistake 3: "I don't know what RDF syntax is"

✅ **Solution**: Don't worry. Use these templates (see next section).

---

## 🎯 Common Tasks (Quick Reference)

### Task: Add a New Equation

**Template**:
```turtle
:EquationN a math:Equation ;
    ap:equationName "name_of_equation" ;
    ap:equationLatex "LaTeX code here" ;
    math:equationNumber N ;
    ap:equationOrder N ;
    math:description "Explanation of equation." .
```

**Example**:
```turtle
:SimpsonRule a math:Equation ;
    ap:equationName "simpson_rule" ;
    ap:equationLatex "\\int_a^b f(x)dx \\approx \\frac{h}{3}(f(a) + 4f(m) + f(b))" ;
    math:equationNumber 3 ;
    ap:equationOrder 3 ;
    math:description "Simpson's rule for numerical integration." .
```

### Task: Add a New Section

**Template**:
```turtle
:SectionN a ap:Section ;
    ap:sectionNumber N ;
    ap:sectionTitle "Section Title" ;
    ap:sectionContent "Content of section." .
```

**Example**:
```turtle
:Methods a ap:Section ;
    ap:sectionNumber 2 ;
    ap:sectionTitle "Methods" ;
    ap:sectionContent "We used the following methodology..." .
```

### Task: Change the Template

By default, papers use `arxiv` style. Options:

```bash
# For IEEE conference
ggen paper generate my-paper.rdf --style ieee --output my-paper.tex

# For ACM journal
ggen paper generate my-paper.rdf --style acm --output my-paper.tex

# For NeurIPS conference
ggen paper generate my-paper.rdf --style neurips --output my-paper.tex

# For PhD thesis
ggen paper generate my-paper.rdf --style thesis --output my-paper.tex
```

---

## 🆘 Something Went Wrong?

### Problem: Command not found: `ggen`

**Solution**: ggen is not installed.

**Fix**:
```bash
# Install ggen (once)
cargo install ggen

# Then try again
ggen paper generate my-paper.rdf --style arxiv
```

### Problem: Invalid RDF syntax

**Example error**:
```
Error: Line 5: Missing colon after property name
```

**Solution**: See `RDF_SYNTAX_GUIDE.md` (in docs/papers/)

**Quick fix checklist**:
- [ ] Does each line end with `;`?
- [ ] Does each property have `:` after the name?
- [ ] Is the RDF in the proper format?

### Problem: PDF won't compile

**Example error**:
```
Undefined control sequence: \mu
```

**Solution**: LaTeX equation syntax is wrong.

**Fix**: Use `\\` instead of `\`:
```turtle
# WRONG:
ap:equationLatex "μ(O)"

# RIGHT:
ap:equationLatex "\\mu(O)"
```

### Problem: I can't find where to fix it

**Solution**: Read `TROUBLESHOOTING_GUIDE.md` (2-minute decision tree)

---

## 📖 Reading Order (By Your Experience Level)

### Path 1: Quick Learning (30 minutes)

1. ✅ **This file** (5 min) - You are here
2. **QUICKSTART.md** (10 min) - Core concepts
3. **RDF_SYNTAX_GUIDE.md** (5 min) - Look up syntax
4. **Try making changes** (10 min) - Experiment

**Result**: You can create and modify papers

### Path 2: Full Understanding (2 hours)

1. ✅ **This file** (5 min)
2. **QUICKSTART.md** (10 min)
3. **ACADEMIC_PAPER_LIFECYCLE.md** (60 min) - Complete workflow
4. **DARK-MATTER-ENERGY-INSIGHTS.md** (20 min) - Why this matters
5. **SEMANTIC-EQUATION-GENERATION-DEMO.md** (25 min) - How it works

**Result**: You understand the entire system

### Path 3: Deep Expertise (4+ hours)

1. All of Path 2
2. **RDF_SYNTAX_GUIDE.md** (20 min)
3. **SUBMISSION_CHECKLIST.md** (10 min)
4. **Template-specific guides** (varies)
5. **Advanced customization** (varies)

**Result**: You can do anything with the system

---

## 🎓 Five Key Principles

### Principle 1: RDF is Source of Truth

Edit `.rdf` files. Generate `.tex` files from them. Ignore `.tex` files.

### Principle 2: One Equation One way

When you add an equation to RDF, it only exists in one place. Change it once, updates everywhere automatically.

### Principle 3: Equations Auto-Number

You just set `ap:equationOrder`. `math:equationNumber` is assigned automatically. Always stay sequential.

### Principle 4: Everything is Queryable

Need to find an equation? Query the RDF:
```sparql
SELECT ?name ?latex WHERE {
  ?eq a math:Equation ;
      ap:equationName ?name ;
      ap:equationLatex ?latex .
} ORDER BY ?equationNumber
```

### Principle 5: Deterministic Output

Same RDF always produces identical LaTeX. Perfect for collaboration and version control.

---

## ✅ Success Checklist

Before moving to next steps, verify:

- [ ] I created `my-paper.rdf` with the minimal example
- [ ] I ran `ggen paper generate` successfully
- [ ] I compiled the `.tex` file to PDF
- [ ] I can open the PDF and see my paper
- [ ] I understand: RDF is edited, LaTeX is generated, PDF is final
- [ ] I know where to find help (This file, QUICKSTART.md, TROUBLESHOOTING_GUIDE.md)

✅ **All checked?** You're ready to move forward!

---

## 🚀 Next Steps

### For the Impatient
Jump to **SUBMISSION_CHECKLIST.md** when you're ready to publish.

### For the Curious
Read **QUICKSTART.md** (10 minutes).

### For the Thorough
Read **ACADEMIC_PAPER_LIFECYCLE.md** (1 hour).

### For Questions
Find your question in **TROUBLESHOOTING_GUIDE.md** (decision tree).

---

## Questions?

**Before asking**, check:

1. Is the answer in **RDF_SYNTAX_GUIDE.md**? (2 min read)
2. Is your question in **TROUBLESHOOTING_GUIDE.md**? (2 min read)
3. Is there an error message? Look it up in **ERROR_CATALOG.md**

**If still stuck**:
- Post your RDF file (the part with your equation)
- Paste the exact error message
- Say what you expected to happen

---

**Date**: 2025-11-15 | **Version**: 1.0 | **Status**: Ready
