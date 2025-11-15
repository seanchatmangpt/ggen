# RDF Syntax Quick Reference Guide

**Purpose**: Copy-paste your way to valid RDF. No syntax errors possible.

---

## 🎯 The Absolute Minimum You Need to Know

RDF has 3 elements:

```
SUBJECT  PREDICATE  OBJECT  .
  ↓        ↓          ↓      ↑
 Who    Does what   What is it  (always ends with period)
```

**Example**:
```turtle
:MyPaper a ap:Paper ;
         ap:title "My Paper" .
```

Translation:
- **SUBJECT**: `:MyPaper` (the thing)
- **PREDICATE**: `a` (means "is a")
- **OBJECT**: `ap:Paper` (what it is)
- **Ends with**: `.` (required!)

---

## ✅ RDF Copy-Paste Templates

### Template 1: Define a Paper

```turtle
:MyPaper a ap:Paper ;
    ap:title "Paper Title Here" ;
    ap:author "Author Name" ;
    ap:date "2025-11-15" ;
    ap:abstract "One paragraph abstract." ;
    ap:keywords "word1, word2, word3" ;
    ap:hasSection :Section1 ;
    math:hasEquation :Equation1 .
```

**Rules**:
- [ ] `ap:title` must be a string (in quotes)
- [ ] `ap:author` must be a string
- [ ] `ap:date` must be YYYY-MM-DD format
- [ ] `ap:abstract` should be 1-2 sentences
- [ ] Each property ends with `;` (except the last, which ends with `.`)

### Template 2: Define a Section

```turtle
:Section1 a ap:Section ;
    ap:sectionNumber 1 ;
    ap:sectionTitle "Introduction" ;
    ap:sectionContent "Write your section content here. It can be multiple sentences. Keep it in one line (don't use line breaks inside the string)." .
```

**Rules**:
- [ ] `ap:sectionNumber` must be an integer (1, 2, 3...)
- [ ] `ap:sectionTitle` must be a string
- [ ] `ap:sectionContent` is a long string (can use \n for line breaks if needed)
- [ ] Last property ends with `.` not `;`

### Template 3: Define an Equation

```turtle
:MyEquation a math:Equation ;
    ap:equationName "equation_name" ;
    ap:equationLatex "E = mc^2" ;
    math:equationNumber 1 ;
    ap:equationOrder 1 ;
    math:description "Einstein's mass-energy equivalence." .
```

**Rules**:
- [ ] `ap:equationName` must be lowercase with underscores (no spaces)
- [ ] `ap:equationLatex` uses `\\` for backslashes (to escape for RDF)
- [ ] `math:equationNumber` auto-assigned, but state it anyway
- [ ] `ap:equationOrder` determines sequence (1, 2, 3... no gaps)
- [ ] `math:description` explains what the equation means

---

## ⚠️ Common Syntax Errors (How to Fix)

### ERROR 1: Missing Colon After Property

❌ **WRONG**:
```turtle
:MyPaper a ap:Paper
    ap:title "My Paper" .
```

✅ **CORRECT**:
```turtle
:MyPaper a ap:Paper ;
    ap:title "My Paper" .
```

**Rule**: Use `;` between properties (not at the very end, that's `.`)

---

### ERROR 2: Missing Period at End

❌ **WRONG**:
```turtle
:MyPaper a ap:Paper ;
    ap:title "My Paper" ;
```

✅ **CORRECT**:
```turtle
:MyPaper a ap:Paper ;
    ap:title "My Paper" .
```

**Rule**: Very last property ends with `.` not `;`

---

### ERROR 3: Quotes Inside String

❌ **WRONG**:
```turtle
ap:title "Sean's Paper"
```

✅ **CORRECT**:
```turtle
ap:title "Sean\\'s Paper"
```

**Rule**: Escape quotes with backslash: `\"`  or use single quotes outside

---

### ERROR 4: LaTeX Backslashes

❌ **WRONG**:
```turtle
ap:equationLatex "μ(O)"  # ← Needs \\mu not μ
```

✅ **CORRECT**:
```turtle
ap:equationLatex "\\mu(O)"  # ← Double backslash
```

**Rule**: LaTeX backslash `\` becomes `\\` in RDF strings

---

### ERROR 5: Property Name Typo

❌ **WRONG**:
```turtle
ap:equationLatex "E = mc^2"  # ← typo: equationLatex
```

✅ **CORRECT**:
```turtle
ap:equationLatex "E = mc^2"  # ← correct spelling
```

**Available properties**:
- `ap:equationName` (internal identifier)
- `ap:equationLatex` (the actual LaTeX)
- `math:equationNumber` (which equation number)
- `ap:equationOrder` (sequence order 1,2,3...)
- `math:description` (explanation)

---

### ERROR 6: Integer vs String

❌ **WRONG**:
```turtle
ap:sectionNumber "1"  # ← Quotes make it a string
```

✅ **CORRECT**:
```turtle
ap:sectionNumber 1  # ← No quotes = integer
```

**Rule**:
- Strings: `"in quotes"`
- Numbers: `without quotes`
- Objects: `:WithColon`

---

### ERROR 7: Missing Namespace Prefix

❌ **WRONG**:
```turtle
:MyPaper a Paper ;  # ← Which Paper? ap:Paper? math:Paper?
```

✅ **CORRECT**:
```turtle
:MyPaper a ap:Paper ;  # ← Prefix clarifies which Paper class
```

**Common prefixes**:
- `ap:` = academic paper things (title, author, section)
- `math:` = math things (equation, number, description)
- `rdfs:` = RDF standard (label, comment)
- `:` = your local things (MyPaper, Section1)

---

## 📋 RDF Validation Checklist

Before running `ggen paper generate`, verify:

### Structure
- [ ] First line: `@prefix ...` declarations (if needed)
- [ ] Each object starts with `:ObjectName a SomeType ;`
- [ ] All properties are indented consistently
- [ ] All but last property ends with `;`
- [ ] Very last property ends with `.`
- [ ] File ends with a newline

### Content
- [ ] Strings are "in quotes"
- [ ] Numbers are `without quotes`
- [ ] Objects are `:WithLeadingColon`
- [ ] LaTeX uses `\\` for backslashes
- [ ] Dates are `YYYY-MM-DD` format
- [ ] Section numbers are sequential (1, 2, 3...)
- [ ] Equation orders are sequential (1, 2, 3... no gaps)

### References
- [ ] Every `ap:hasSection :SectionX` points to a defined section
- [ ] Every `math:hasEquation :EquationX` points to a defined equation
- [ ] All section/equation names are defined somewhere in the file

---

## 🔍 Debugging Strategy

### Strategy 1: Look for the Error

Most errors are in this format:

```
Error: Line 42: Missing semicolon
  ap:equationLatex "E = mc^2"
                                ↑ needs semicolon here
```

**Fix**: Add the missing `;`

### Strategy 2: Check Indentation

All properties of the same object should be indented the same:

❌ **WRONG** (inconsistent indentation):
```turtle
:MyPaper a ap:Paper ;
  ap:title "Title" ;
    ap:author "Author" ;  # ← Different indent
  ap:date "2025-11-15" .
```

✅ **CORRECT** (consistent indentation):
```turtle
:MyPaper a ap:Paper ;
    ap:title "Title" ;
    ap:author "Author" ;
    ap:date "2025-11-15" .
```

### Strategy 3: Compare to Minimal Example

If stuck, compare your paper to `examples/minimal-paper.rdf`:

1. Does your structure match?
2. Are the property names identical?
3. Are the data types correct (string vs number)?

### Strategy 4: Validate Step by Step

```bash
# Check if RDF is valid
ggen paper validate my-paper.rdf

# If error, look at the line number
# Fix that one line
# Validate again
# Repeat until no errors
```

---

## 📚 Property Reference

### Paper-Level Properties

| Property | Type | Example | Required? |
|----------|------|---------|-----------|
| `ap:title` | String | "My Paper" | ✅ |
| `ap:author` | String | "Jane Doe" | ✅ |
| `ap:date` | String | "2025-11-15" | ✅ |
| `ap:abstract` | String | "Summary..." | ✅ |
| `ap:keywords` | String | "word1, word2" | ✅ |
| `ap:hasSection` | Object | `:Section1` | ✅ |
| `math:hasEquation` | Object | `:Equation1` | ✅ |

### Section Properties

| Property | Type | Example | Required? |
|----------|------|---------|-----------|
| `ap:sectionNumber` | Integer | 1 | ✅ |
| `ap:sectionTitle` | String | "Introduction" | ✅ |
| `ap:sectionContent` | String | "Text..." | ✅ |

### Equation Properties

| Property | Type | Example | Required? |
|----------|------|---------|-----------|
| `ap:equationName` | String | "core_eq" | ✅ |
| `ap:equationLatex` | String | "E = mc^2" | ✅ |
| `math:equationNumber` | Integer | 1 | ✅ |
| `ap:equationOrder` | Integer | 1 | ✅ |
| `math:description` | String | "Explanation" | ✅ |
| `math:meaning` | String | "Semantic meaning" | ❌ |
| `math:implication` | String | "What it implies" | ❌ |

### How to Use Properties

**Required property missing**:
```
Error: Paper missing ap:title
Fix: Add ap:title "Your Title" ;
```

**Optional property**:
```turtle
# These are optional - include if meaningful:
math:meaning "The deeper semantic interpretation" ;
math:implication "What this equation allows us to conclude" ;
```

---

## 🎯 Copy-Paste Ready Examples

### Example 1: Simple Paper (No Equations)

```turtle
@prefix : <http://example.org/paper#> .
@prefix ap: <http://example.org/academic-paper#> .

:SimplePaper a ap:Paper ;
    ap:title "A Simple Paper" ;
    ap:author "John Doe" ;
    ap:date "2025-11-15" ;
    ap:abstract "This is a simple paper." ;
    ap:keywords "simple, paper" ;
    ap:hasSection :Intro .

:Intro a ap:Section ;
    ap:sectionNumber 1 ;
    ap:sectionTitle "Introduction" ;
    ap:sectionContent "The introduction goes here." .
```

### Example 2: Paper with One Equation

```turtle
@prefix : <http://example.org/paper#> .
@prefix ap: <http://example.org/academic-paper#> .
@prefix math: <http://example.org/math#> .

:PaperWithEquation a ap:Paper ;
    ap:title "Paper With Equation" ;
    ap:author "Jane Smith" ;
    ap:date "2025-11-15" ;
    ap:abstract "Paper demonstrating equations." ;
    ap:keywords "equations, math" ;
    ap:hasSection :Intro ;
    math:hasEquation :Eq1 .

:Intro a ap:Section ;
    ap:sectionNumber 1 ;
    ap:sectionTitle "Introduction" ;
    ap:sectionContent "We present an equation." .

:Eq1 a math:Equation ;
    ap:equationName "einstein_equation" ;
    ap:equationLatex "E = mc^2" ;
    math:equationNumber 1 ;
    ap:equationOrder 1 ;
    math:description "Einstein's mass-energy equivalence equation." .
```

### Example 3: Multi-Equation Paper

```turtle
@prefix : <http://example.org/paper#> .
@prefix ap: <http://example.org/academic-paper#> .
@prefix math: <http://example.org/math#> .

:MultiEquationPaper a ap:Paper ;
    ap:title "Paper With Multiple Equations" ;
    ap:author "Alice Johnson" ;
    ap:date "2025-11-15" ;
    ap:abstract "Demonstrates multiple equations." ;
    ap:keywords "equations, multiple" ;
    ap:hasSection :Intro ;
    ap:hasSection :Theory ;
    math:hasEquation :Eq1 ;
    math:hasEquation :Eq2 ;
    math:hasEquation :Eq3 .

:Intro a ap:Section ;
    ap:sectionNumber 1 ;
    ap:sectionTitle "Introduction" ;
    ap:sectionContent "Introduction text." .

:Theory a ap:Section ;
    ap:sectionNumber 2 ;
    ap:sectionTitle "Theory" ;
    ap:sectionContent "Theoretical background with three key equations." .

:Eq1 a math:Equation ;
    ap:equationName "first_equation" ;
    ap:equationLatex "a + b = c" ;
    math:equationNumber 1 ;
    ap:equationOrder 1 ;
    math:description "Simple addition equation." .

:Eq2 a math:Equation ;
    ap:equationName "second_equation" ;
    ap:equationLatex "x^2 + y^2 = r^2" ;
    math:equationNumber 2 ;
    ap:equationOrder 2 ;
    math:description "Circle equation." .

:Eq3 a math:Equation ;
    ap:equationName "third_equation" ;
    ap:equationLatex "\\int_0^\\infty e^{-x} dx = 1" ;
    math:equationNumber 3 ;
    ap:equationOrder 3 ;
    math:description "Exponential integral." .
```

---

## ✅ Final Validation

Before submitting or sharing your paper:

```bash
# Run the validator
ggen paper validate my-paper.rdf

# Expected output:
# ✅ RDF syntax valid
# ✅ All sections exist
# ✅ All equations exist
# ✅ Equation order valid (1-N sequential)
# ✅ Metadata complete
# ✅ Paper ready for generation
```

If you see any ❌, find that property in this guide and fix it.

---

**Date**: 2025-11-15 | **Version**: 1.0 | **Status**: Complete reference
