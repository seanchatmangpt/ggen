# Troubleshooting Guide: Decision Tree Format

**Purpose**: No matter what goes wrong, follow this tree to find your answer in <5 minutes.

**How to use**: Start at Q1, answer honestly, follow the arrow to Q2 or solution.

---

## 🌳 The Troubleshooting Decision Tree

```
START HERE
    ↓
Q1: Have you created a paper.rdf file yet?
├─ NO  → Read 000_START_HERE.md (5 min)
└─ YES → Q2

Q2: Can you run `ggen paper generate`?
├─ Command not found → SOLUTION A
└─ Command runs → Q3

Q3: Does the command succeed?
├─ YES (generates .tex file) → Q4
└─ NO (error message) → Q5

Q4: Can you compile to PDF?
├─ YES (PDF created) → Q6
└─ NO (pdflatex error) → SOLUTION C

Q5: What's your error message?
├─ "Invalid RDF" → SOLUTION B1
├─ "Equation undefined" → SOLUTION B2
├─ "Missing metadata" → SOLUTION B3
└─ Other → SOLUTION B4

Q6: Does the PDF look correct?
├─ YES (equations numbered 1,2,3...) → 🎉 SUCCESS
├─ NO (equations wrong) → SOLUTION D1
├─ NO (text formatting wrong) → SOLUTION D2
└─ NO (missing content) → SOLUTION D3
```

---

## 🔧 Solutions by Decision Path

### SOLUTION A: "ggen command not found"

**Problem**: You ran `ggen paper generate` but got:
```
command not found: ggen
```

**Cause**: ggen is not installed

**Fix (choose one)**:

#### Option 1: Install ggen (Recommended)
```bash
# Install from cargo
cargo install ggen

# Verify it worked
ggen --version

# Try again
ggen paper generate my-paper.rdf --style arxiv --output my-paper.tex
```

#### Option 2: Use full path
```bash
# If ggen is somewhere on your system
/path/to/ggen paper generate my-paper.rdf --style arxiv

# Find ggen
which ggen
```

#### Option 3: Use docker
```bash
# If you have Docker installed
docker run ggen paper generate my-paper.rdf --style arxiv
```

**Next**: If install succeeds, go back to Q2

---

### SOLUTION B1: "Invalid RDF" Error

**Problem**: You got an error like:
```
Error: Invalid RDF syntax at line 42
Expected ':' after property
```

**Cause**: RDF file has syntax error

**Fix (step by step)**:

1. **Open your paper.rdf file** in a text editor

2. **Go to the line mentioned** (line 42 in this example)

3. **Check for common errors**:
   ```
   Line 42: ap:equationLatexE = mc^2
                             ↑ MISSING SPACE BEFORE =

   FIX: ap:equationLatex "E = mc^2" ;
   ```

4. **Use this checklist for the error line**:
   - [ ] Property name spelled correctly?
   - [ ] Property name ends with space and quotes for string?
   - [ ] String is in quotes: `"..."`?
   - [ ] Line ends with `;` or `.`?
   - [ ] No quotes inside quotes (unless escaped: `\"`)

5. **Specific error patterns**:

   | Error | Example | Fix |
   |-------|---------|-----|
   | Missing colon | `apequationLatex` | `ap:equationLatex` |
   | Missing quotes | `ap:title My Paper` | `ap:title "My Paper"` |
   | Missing semicolon | `ap:title "Title"` | `ap:title "Title" ;` |
   | Wrong end char | `ap:title "T" ;` (last) | `ap:title "T" .` |
   | Quotes inside string | `ap:title "Sean's Paper"` | `ap:title "Sean\\'s Paper"` |
   | LaTeX backslash | `ap:equationLatex "\\mu(O)"` | `ap:equationLatex "\\mu(O)"` ← need `\\` |

6. **Use RDF_SYNTAX_GUIDE.md** to see the correct format

7. **Validate again**:
   ```bash
   ggen paper validate my-paper.rdf
   ```

**If still stuck**:
- Compare your file to `examples/minimal-paper.rdf`
- Copy that structure exactly
- Modify only the content, not the format

**Next**: Try `ggen paper generate` again (go back to Q2)

---

### SOLUTION B2: "Equation undefined" Error

**Problem**: You got an error like:
```
Error: Equation :Equation1 referenced but not defined
```

**Cause**: Your paper says it has an equation, but the equation isn't defined

**Fix (step by step)**:

1. **Find the reference** in your paper.rdf:
   ```turtle
   math:hasEquation :Equation1 ;  # ← This line references Equation1
   ```

2. **Search for the definition**:
   ```bash
   grep ":Equation1 a math:Equation" my-paper.rdf
   ```

3. **If not found**, you need to add it:
   ```turtle
   :Equation1 a math:Equation ;
       ap:equationName "my_equation" ;
       ap:equationLatex "E = mc^2" ;
       math:equationNumber 1 ;
       ap:equationOrder 1 ;
       math:description "Description here." .
   ```

4. **If found but error still occurs**, check:
   - [ ] Is the object name spelled identically? (`:Equation1` vs `:Equation_1`)
   - [ ] Is the definition complete (has `a math:Equation ;`)?
   - [ ] Does the definition have all required properties?

**Checklist for equation definition**:
```turtle
:EquationName a math:Equation ;        # ← Required: declares as equation
    ap:equationName "internal_name" ;  # ← Required: unique identifier
    ap:equationLatex "LaTeX code" ;    # ← Required: actual equation
    math:equationNumber N ;             # ← Required: which number
    ap:equationOrder N ;                # ← Required: sequence order
    math:description "..." .            # ← Required: explanation
```

**Next**: Run validation again (go back to Q2)

---

### SOLUTION B3: "Missing Metadata" Error

**Problem**: You got an error like:
```
Error: Paper missing required metadata: ap:abstract
```

**Cause**: Your paper definition is missing required properties

**Fix (step by step)**:

1. **Find the paper definition** in your file:
   ```turtle
   :MyPaper a ap:Paper ;
       ap:title "..." ;
       ap:author "..." ;
       ...
   ```

2. **Check required properties exist**:
   ```turtle
   :MyPaper a ap:Paper ;
       ap:title "Your Title" ;           # ← Required
       ap:author "Your Name" ;            # ← Required
       ap:date "2025-11-15" ;             # ← Required
       ap:abstract "One sentence..." ;    # ← Required
       ap:keywords "word1, word2" ;       # ← Required
       ap:hasSection :Section1 ;          # ← Required (at least one)
       math:hasEquation :Equation1 .      # ← Required (at least one)
   ```

3. **If a property is missing**, copy-paste the template:
   ```turtle
   # If missing ap:abstract:
   ap:abstract "Brief summary of the paper." ;

   # If missing ap:keywords:
   ap:keywords "keyword1, keyword2, keyword3" ;

   # If missing ap:date:
   ap:date "2025-11-15" ;
   ```

4. **Re-run validation**:
   ```bash
   ggen paper validate my-paper.rdf
   ```

**Next**: Try generation again (go back to Q2)

---

### SOLUTION B4: Other Error Messages

**Problem**: You got an error we didn't cover

**Debug strategy (do all)**:

1. **Read the error message carefully**:
   ```
   Error: Unknown property 'ap:equationLaTeX'
                                          ↑ Capitals wrong

   Should be: ap:equationLatex (lower case 'a')
   ```

2. **Search in RDF_SYNTAX_GUIDE.md** for the property name

3. **If error mentions a line number**:
   ```bash
   # Show line with context
   sed -n '40,45p' my-paper.rdf
   ```

4. **Post your question with**:
   - [ ] Exact error message (copy-paste)
   - [ ] The line from your file that's problematic
   - [ ] What you expected to happen

5. **Common patterns**:
   - Capitalization wrong: `ap:EquationName` → `ap:equationName`
   - Property misspelled: `ap:equationLatest` → `ap:equationLatex`
   - Object name wrong: `:MyEquation` instead of `:Equation1`

**Next**: Try again after fix (go back to Q2)

---

### SOLUTION C: "pdflatex" Compilation Error

**Problem**: `ggen paper generate` succeeded, but `pdflatex` failed:
```
Undefined control sequence: \mu(O)
```

**Cause**: LaTeX equation syntax is wrong or uses single backslash

**Common latex errors and fixes**:

| Error | Cause | Fix |
|-------|-------|-----|
| `Undefined control sequence` | Backslash not escaped | Use `\\` instead of `\` |
| `Missing $` | Equation not in math mode | Wrap in `$...$` |
| `Extra }` | Mismatched braces | Count `{` and `}` equal |
| `File not found` | Missing dependency | Install LaTeX packages |

**Fix (step by step)**:

1. **Check your equation LaTeX**:
   ```turtle
   # WRONG (single backslash):
   ap:equationLatex "\mu(O)"

   # CORRECT (double backslash in RDF):
   ap:equationLatex "\\mu(O)"
   ```

2. **Common LaTeX patterns**:
   ```
   Greek letters: \\alpha \\beta \\gamma \\mu \\pi
   Subscripts: x_{i} or x_i
   Superscripts: x^{2} or x^2
   Fractions: \\frac{a}{b}
   Integrals: \\int_0^1 f(x) dx
   ```

3. **Verify equation in your RDF file** uses `\\` not `\`

4. **Try compiling again**:
   ```bash
   pdflatex my-paper.tex
   ```

5. **If still fails**, simplify equation:
   ```turtle
   # Try simpler version first
   ap:equationLatex "E = mc^2"

   # Then gradually add complexity
   ap:equationLatex "E = mc^{2}"
   ap:equationLatex "\\alpha E = mc^{2}"
   ```

**Next**: Try LaTeX compilation again (go back to Q4)

---

### SOLUTION D1: "Equations aren't numbering right"

**Problem**: PDF shows equations 1, 3, 5 (skipping some numbers)

**Cause**: `ap:equationOrder` has gaps or wrong values

**Fix (step by step)**:

1. **List all your equations** with their `ap:equationOrder`:
   ```bash
   grep "ap:equationOrder" my-paper.rdf
   ```

2. **Check they're sequential** starting at 1:
   ```
   ✅ CORRECT: ap:equationOrder 1 ;
              ap:equationOrder 2 ;
              ap:equationOrder 3 ;

   ❌ WRONG:   ap:equationOrder 1 ;
              ap:equationOrder 3 ;  ← Gap!
              ap:equationOrder 4 ;
   ```

3. **Fix gaps** by renumbering:
   ```turtle
   # If you have: 1, 3, 4 (missing 2)
   # Renumber to: 1, 2, 3

   # Third equation should be:
   ap:equationOrder 2 ;  # ← Change from 3
   ap:equationOrder 3 ;  # ← Change from 4
   ```

4. **Regenerate and recompile**:
   ```bash
   ggen paper generate my-paper.rdf --style arxiv --output my-paper.tex
   pdflatex my-paper.tex
   ```

**Next**: Check PDF again (go back to Q6)

---

### SOLUTION D2: "Text formatting is wrong"

**Problem**: PDF has wrong spacing, line breaks, or fonts

**Cause**: Template issue or RDF content has formatting issues

**Fix (step by step)**:

1. **Check template choice**:
   ```bash
   # See which template was used
   grep "\\\\documentclass" my-paper.tex

   # Regenerate with different template
   ggen paper generate my-paper.rdf --style ieee --output my-paper.tex
   pdflatex my-paper.tex
   ```

2. **Try all templates**:
   ```bash
   ggen paper generate my-paper.rdf --style arxiv --output my-paper.tex
   ggen paper generate my-paper.rdf --style ieee --output my-paper.tex
   ggen paper generate my-paper.rdf --style acm --output my-paper.tex
   ```

3. **Check section content** for hidden characters:
   ```bash
   # Search for newlines or special chars
   grep "sectionContent" my-paper.rdf | cat -v
   ```

4. **Clean up section content**:
   ```turtle
   # WRONG (line breaks):
   ap:sectionContent "Line 1
   Line 2" ;

   # CORRECT (single line):
   ap:sectionContent "Line 1 Line 2" ;
   ```

**Next**: Try different template (go back to Q6)

---

### SOLUTION D3: "Content is missing from PDF"

**Problem**: Some sections or equations don't appear in PDF

**Cause**: Not linked from main paper object or RDF parsing issue

**Fix (step by step)**:

1. **Check main paper definition**:
   ```turtle
   :MyPaper a ap:Paper ;
       ap:title "..." ;
       ap:author "..." ;
       ap:hasSection :Section1 ;     # ← Is it here?
       ap:hasSection :Section2 ;     # ← Is it here?
       math:hasEquation :Equation1 ; # ← Is it here?
   ```

2. **List all sections and equations**:
   ```bash
   grep "a ap:Section" my-paper.rdf   # Show all section definitions
   grep "a math:Equation" my-paper.rdf # Show all equation definitions
   ```

3. **Check linking**:
   ```bash
   grep "ap:hasSection" my-paper.rdf   # Show all section references
   grep "math:hasEquation" my-paper.rdf # Show all equation references
   ```

4. **If section defined but not linked**:
   ```turtle
   # If you have :Section2 defined but not referenced:
   :MyPaper a ap:Paper ;
       ap:hasSection :Section1 ;
       ap:hasSection :Section2 ;  # ← Add this line
       ...
   ```

5. **Regenerate**:
   ```bash
   ggen paper generate my-paper.rdf --style arxiv --output my-paper.tex
   pdflatex my-paper.tex
   ```

**Next**: Check PDF again (go back to Q6)

---

## ✅ "SUCCESS" Checkpoint

You reached here from Q6 with "YES, equations numbered 1,2,3..."

**Verify**:
- [ ] PDF opens and displays
- [ ] Equations numbered sequentially (1, 2, 3...)
- [ ] All text is readable
- [ ] No error messages

**If all yes**: 🎉 You're done!

**Next steps**:
1. Read SUBMISSION_CHECKLIST.md before publishing
2. Read ACADEMIC_PAPER_LIFECYCLE.md to learn more
3. Make changes to my-paper.rdf and regenerate

---

## 🆘 Still Stuck?

If you followed this tree and still have a problem:

1. **Go back to 000_START_HERE.md** - Start over
2. **Check RDF_SYNTAX_GUIDE.md** - Verify your syntax
3. **Copy examples/minimal-paper.rdf** - Start fresh with working example
4. **Post your exact error message** including:
   - Exact command you ran
   - Exact error output (copy-paste)
   - Your RDF file (private-safe version)

---

**Date**: 2025-11-15 | **Version**: 1.0 | **Status**: Complete decision tree
