<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pre-Submission Checklist: "No Way This Can Fail"](#pre-submission-checklist-no-way-this-can-fail)
  - [📋 Pre-Submission Process (15 minutes)](#-pre-submission-process-15-minutes)
    - [Step 1: Validate RDF (2 minutes)](#step-1-validate-rdf-2-minutes)
    - [Step 2: Generate LaTeX (2 minutes)](#step-2-generate-latex-2-minutes)
    - [Step 3: Compile to PDF (3 minutes)](#step-3-compile-to-pdf-3-minutes)
    - [Step 4: Manual PDF Inspection (3 minutes)](#step-4-manual-pdf-inspection-3-minutes)
    - [Step 5: Metadata Verification (2 minutes)](#step-5-metadata-verification-2-minutes)
    - [Step 6: Content Completeness (2 minutes)](#step-6-content-completeness-2-minutes)
  - [🎯 Venue-Specific Checklists](#-venue-specific-checklists)
  - [Venue Option 1: arXiv](#venue-option-1-arxiv)
    - [arXiv Submission Requirements](#arxiv-submission-requirements)
  - [Venue Option 2: IEEE Conference](#venue-option-2-ieee-conference)
    - [IEEE Submission Requirements](#ieee-submission-requirements)
  - [Venue Option 3: ACM Conference/Journal](#venue-option-3-acm-conferencejournal)
    - [ACM Submission Requirements](#acm-submission-requirements)
  - [Venue Option 4: NeurIPS Conference](#venue-option-4-neurips-conference)
    - [NeurIPS Submission Requirements](#neurips-submission-requirements)
  - [Venue Option 5: PhD Thesis / Dissertation](#venue-option-5-phd-thesis--dissertation)
    - [Thesis Submission Requirements](#thesis-submission-requirements)
  - [🎓 Cross-Venue Final Checklist](#-cross-venue-final-checklist)
    - [Quality Assurance](#quality-assurance)
    - [Technical Correctness](#technical-correctness)
    - [Formatting Compliance](#formatting-compliance)
    - [Metadata](#metadata)
    - [Files](#files)
  - [✅ Final Approval](#-final-approval)
  - [🆘 Last-Minute Issues](#-last-minute-issues)
    - [Issue: PDF has wrong page count](#issue-pdf-has-wrong-page-count)
    - [Issue: Equations don't match venue format](#issue-equations-dont-match-venue-format)
    - [Issue: Bibliography missing](#issue-bibliography-missing)
    - [Issue: Can't find something in PDF](#issue-cant-find-something-in-pdf)
    - [Issue: Formatting looks wrong for venue](#issue-formatting-looks-wrong-for-venue)
  - [📞 Getting Help](#-getting-help)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Pre-Submission Checklist: "No Way This Can Fail"

**Purpose**: Verify your paper is ready for publication. Use this checklist BEFORE submitting to any venue.

**Guarantee**: If you check every box, your paper will be accepted by venue systems (content quality is your responsibility).

---

## 📋 Pre-Submission Process (15 minutes)

### Step 1: Validate RDF (2 minutes)

```bash
ggen paper validate my-paper.rdf
```

**Expected output**:
```
✅ RDF syntax valid
✅ All sections defined
✅ All equations defined
✅ Equation order sequential
✅ All metadata complete
✅ References valid
```

**Checklist**:
- [ ] Command runs without errors
- [ ] All checks show ✅

**If ❌ appears**:
- Fix the issue (see TROUBLESHOOTING_GUIDE.md)
- Re-run validation
- Don't proceed until all ✅

---

### Step 2: Generate LaTeX (2 minutes)

```bash
ggen paper generate my-paper.rdf --style arxiv --output my-paper.tex
```

**Expected output**:
```
✅ Validating paper...
✅ Generating LaTeX...
✅ Paper generated: my-paper.tex (467 lines)
```

**Checklist**:
- [ ] Command succeeds
- [ ] No error messages
- [ ] File `my-paper.tex` created

**If error appears**:
- See TROUBLESHOOTING_GUIDE.md SOLUTION B or C
- Fix the issue
- Re-run generation
- Don't proceed until success

---

### Step 3: Compile to PDF (3 minutes)

```bash
pdflatex my-paper.tex
```

**Expected output**:
```
This is pdfTeX, Version 3.14159...
...
Output written on my-paper.pdf (X pages).
```

**Checklist**:
- [ ] Command completes successfully
- [ ] No "Error" or "Undefined" messages
- [ ] File `my-paper.pdf` created

**If compilation fails**:
- See TROUBLESHOOTING_GUIDE.md SOLUTION C
- Fix LaTeX syntax errors
- Re-run pdflatex
- Don't proceed until PDF created

---

### Step 4: Manual PDF Inspection (3 minutes)

**Open `my-paper.pdf` and verify**:

- [ ] **Title Page**: Title, author name, date are correct
- [ ] **Abstract**: Reads clearly, no formatting issues
- [ ] **Equations**:
  - [ ] All equations appear
  - [ ] Numbered sequentially (1, 2, 3... no gaps)
  - [ ] All LaTeX renders correctly
- [ ] **Sections**: All section titles present
- [ ] **Content**: Text reads clearly, no corruption
- [ ] **Bibliography**: If applicable, references appear
- [ ] **Page Count**: More than 1 page (paper has content)

**If any issue found**:
- Identify the issue type
- Consult TROUBLESHOOTING_GUIDE.md
- Fix in my-paper.rdf
- Regenerate and recompile
- Re-inspect PDF

**Don't proceed** until all boxes checked.

---

### Step 5: Metadata Verification (2 minutes)

Using `pdfinfo` or similar tool, verify:

```bash
pdfinfo my-paper.pdf
```

**Expected output shows**:
```
Title:          Your Paper Title
Author:         Your Name
Creator:        LaTeX with hyperref
```

**Checklist**:
- [ ] Title matches your RDF
- [ ] Author matches your RDF
- [ ] PDF is searchable (not image-based)
- [ ] File size is reasonable (< 10 MB)

**If metadata wrong**:
- Update in my-paper.rdf
- Regenerate and recompile

**Don't proceed** until metadata correct.

---

### Step 6: Content Completeness (2 minutes)

**Verify your paper has**:

- [ ] **Title**: Present and meaningful
- [ ] **Author(s)**: Your name(s) listed
- [ ] **Abstract**: 150-250 words
- [ ] **Keywords**: 4-8 relevant keywords
- [ ] **Introduction**: Explains motivation
- [ ] **Main content**: Theory, methods, results
- [ ] **Equations**: At least 1 (if applicable)
- [ ] **Conclusion**: Summarizes findings
- [ ] **References**: If applicable, proper citations
- [ ] **Figures/Tables**: If applicable, properly labeled

**For each missing section**:
- Decide if needed for your paper
- Add to my-paper.rdf if needed
- Regenerate
- Re-verify

**Don't proceed** until content complete.

---

## 🎯 Venue-Specific Checklists

Choose your target venue:

---

## Venue Option 1: arXiv

### arXiv Submission Requirements

**Format**:
- [ ] PDF created from LaTeX (✅ ggen does this)
- [ ] Uses standard document class (\documentclass{article})
- [ ] Page size is Letter or A4
- [ ] Margins are 1 inch or larger

**Content**:
- [ ] Title is informative
- [ ] Author(s) listed
- [ ] Abstract (required, 1-2 paragraphs)
- [ ] Paper is original research (not plagiarized)

**Files to upload**:
- [ ] Source files: my-paper.rdf + ggen commands
  - OR just my-paper.tex
- [ ] PDF: my-paper.pdf

**Pre-submission**:
```bash
# 1. Validate
ggen paper validate my-paper.rdf

# 2. Generate with arxiv style
ggen paper generate my-paper.rdf --style arxiv --output my-paper.tex

# 3. Compile
pdflatex my-paper.tex
bibtex my-paper  # If you have bibliography

# 4. Verify PDF
# Open my-paper.pdf and check it looks right
```

**Submission steps** (at arxiv.org):

1. Click "New Submission"
2. Select Subject Area
3. Upload files:
   - Select my-paper.rdf (recommended) or my-paper.tex
   - Upload my-paper.pdf
4. Fill in metadata:
   - Title (from RDF)
   - Authors (from RDF)
   - Abstract (from RDF)
   - Subjects
5. Review & Submit

**arXiv Checklist**:
- [ ] PDF displays correctly
- [ ] All equations numbered properly
- [ ] Abstract is present
- [ ] Title and authors correct
- [ ] File size < 10 MB

---

## Venue Option 2: IEEE Conference

### IEEE Submission Requirements

**Format**:
- [ ] Use IEEE template (ggen --style ieee)
- [ ] Two-column format
- [ ] Page limit: usually 6-8 pages (check specific conference)
- [ ] US Letter size
- [ ] Single-spaced, 10pt font

**Content**:
- [ ] Title (10-12 words)
- [ ] Author(s) with affiliations
- [ ] Abstract (100-150 words)
- [ ] Introduction (15% of paper)
- [ ] Methods/Approach (40% of paper)
- [ ] Results (35% of paper)
- [ ] Conclusion (10% of paper)
- [ ] References

**Formatting**:
- [ ] Section numbering: I, II, III... (roman numerals)
- [ ] Subsection numbering: A, B, C...
- [ ] Equations: Numbered as (1), (2), etc.
- [ ] Figures: Captions below
- [ ] Tables: Captions above

**Pre-submission**:
```bash
# 1. Generate with IEEE style
ggen paper generate my-paper.rdf --style ieee --output my-paper.tex

# 2. Compile
pdflatex my-paper.tex
bibtex my-paper

# 3. Check page count
pdfinfo my-paper.pdf | grep Pages

# 4. Verify format
# - Two columns?
# - Roman numeral sections?
# - Correct font size?
```

**IEEE Checklist**:
- [ ] Generated with --style ieee
- [ ] Page count within limit (6-8 pages)
- [ ] Two-column layout
- [ ] Roman numeral section numbering
- [ ] All equations numbered
- [ ] Figures and tables have captions
- [ ] References formatted as [1], [2], etc.

---

## Venue Option 3: ACM Conference/Journal

### ACM Submission Requirements

**Format**:
- [ ] Use ACM template (ggen --style acm)
- [ ] Single column (journal) or two column (conference)
- [ ] Page limit varies (check specific venue)
- [ ] US Letter size
- [ ] "Balancing" for last page

**Content**:
- [ ] CCS Concepts (if applicable)
- [ ] Keywords (4-8 terms)
- [ ] Abstract (150-250 words)
- [ ] Sections with descriptive headers
- [ ] Acknowledgments section (optional)
- [ ] References

**Formatting**:
- [ ] Section numbering: 1, 2, 3...
- [ ] Subsection numbering: 1.1, 1.2, 2.1...
- [ ] Equations: Numbered as (1), (2), etc.
- [ ] Figures: Captions below with "Figure X:"
- [ ] Tables: Captions above with "Table X:"

**Pre-submission**:
```bash
# 1. Generate with ACM style
ggen paper generate my-paper.rdf --style acm --output my-paper.tex

# 2. Compile
pdflatex my-paper.tex
bibtex my-paper

# 3. Check formatting
# - Single/two column as appropriate?
# - Decimal section numbering?
# - ACM citation format?
```

**ACM Checklist**:
- [ ] Generated with --style acm
- [ ] Page count within limit
- [ ] Correct column format
- [ ] Decimal section numbering (1, 1.1, 1.2, 2...)
- [ ] All equations numbered
- [ ] Author names and affiliations correct
- [ ] ACM keywords included

---

## Venue Option 4: NeurIPS Conference

### NeurIPS Submission Requirements

**Format**:
- [ ] Use NeurIPS template (ggen --style neurips)
- [ ] Two-column format
- [ ] Page limit: 8 pages + 2 pages references
- [ ] US Letter or A4 size
- [ ] 11pt font, single-spaced

**Content**:
- [ ] Title (concise, descriptive)
- [ ] Author(s) (anonymous for review)
- [ ] Abstract (1 paragraph, ~150 words)
- [ ] Introduction
- [ ] Related Work
- [ ] Method/Approach
- [ ] Experiments
- [ ] Results and Analysis
- [ ] Discussion
- [ ] Conclusion
- [ ] References
- [ ] Appendix (if needed, space unlimited)

**Formatting**:
- [ ] Equations: Numbered as (1), (2), etc.
- [ ] Figures: Numbered, captions below
- [ ] Tables: Numbered, captions above
- [ ] Subsections: Use \subsection{...}
- [ ] Code listings: Use lstlisting or similar

**Pre-submission**:
```bash
# 1. Generate with NeurIPS style
ggen paper generate my-paper.rdf --style neurips --output my-paper.tex

# 2. Compile (twice for references)
pdflatex my-paper.tex
bibtex my-paper
pdflatex my-paper.tex
pdflatex my-paper.tex

# 3. Check page count
pdfinfo my-paper.pdf | grep Pages
# Should be <= 10 pages (8 + 2 references)

# 4. Verify anonymous (check no author info in text)
grep -i "author\|myself\|my previous" my-paper.tex
# Should return nothing (or only in references)
```

**NeurIPS Checklist**:
- [ ] Generated with --style neurips
- [ ] Page count 8 + up to 2 reference pages
- [ ] Two-column format
- [ ] Anonymous (no author identifying information)
- [ ] All equations numbered
- [ ] Section structure: Intro, Related, Method, Experiments, Results, Conclusion
- [ ] References are complete

---

## Venue Option 5: PhD Thesis / Dissertation

### Thesis Submission Requirements

**Format**:
- [ ] Use thesis template (ggen --style thesis)
- [ ] Single column, large margins
- [ ] Proper front matter (title, copyright, abstract, TOC)
- [ ] Chapter structure
- [ ] Page size per university requirements
- [ ] Line spacing per university requirements

**Content**:
- [ ] Title page (with degree, institution, date)
- [ ] Copyright page
- [ ] Dedication (optional)
- [ ] Abstract (300-500 words)
- [ ] Table of Contents
- [ ] List of Figures (if many)
- [ ] List of Tables (if many)
- [ ] Chapters with content
- [ ] Conclusion chapter
- [ ] Bibliography
- [ ] Appendices (if needed)

**Pre-submission**:
```bash
# 1. Check university requirements
# Some universities have specific formatting rules
# Verify ggen template matches requirements

# 2. Generate with thesis style
ggen paper generate my-paper.rdf --style thesis --output my-thesis.tex

# 3. Compile
pdflatex my-thesis.tex
bibtex my-thesis
pdflatex my-thesis.tex
pdflatex my-thesis.tex

# 4. Verify formatting
# - Front matter present and correctly ordered?
# - Page numbering correct (roman for front, arabic for chapters)?
# - Chapter titles formatted correctly?
# - Bibliography complete?
```

**Thesis Checklist**:
- [ ] Generated with --style thesis
- [ ] Title page matches university template
- [ ] Front matter in correct order
- [ ] All chapters included
- [ ] Page numbering: i, ii, iii... (front), 1, 2, 3... (chapters)
- [ ] Margins meet university requirements (typically 1")
- [ ] Line spacing correct (typically double-spaced)
- [ ] Bibliography complete and formatted correctly
- [ ] Appendices organized logically

---

## 🎓 Cross-Venue Final Checklist

Before submitting to ANY venue:

### Quality Assurance
- [ ] Paper is original (not plagiarized)
- [ ] Paper is complete (not a draft)
- [ ] Paper is proofread (no typos, grammar errors)
- [ ] All citations are complete and correct
- [ ] All equations are correctly formatted
- [ ] All figures are clear and labeled
- [ ] All tables are clear and labeled

### Technical Correctness
- [ ] All claims are supported by evidence
- [ ] All experimental results are documented
- [ ] All mathematical notation is consistent
- [ ] No undefined variables or symbols
- [ ] All referenced equations exist and are numbered

### Formatting Compliance
- [ ] Matches target venue template
- [ ] Page count within limits
- [ ] Font size and style correct
- [ ] Margins correct
- [ ] Headers and footers as specified
- [ ] Bibliography format correct

### Metadata
- [ ] Title is accurate and descriptive
- [ ] Author names and affiliations correct
- [ ] Keywords are relevant (4-8 terms)
- [ ] Abstract is present and complete
- [ ] Subject area/categories selected appropriately

### Files
- [ ] PDF file created
- [ ] Source files (RDF or TeX) included if required
- [ ] All supplementary materials included if needed
- [ ] File names are clear and organized
- [ ] No files included accidentally (backups, drafts, etc.)

---

## ✅ Final Approval

**Before clicking submit**:

- [ ] I have run `ggen paper validate`
- [ ] I have generated with the correct style for my venue
- [ ] I have compiled successfully (pdflatex ran)
- [ ] I have opened the PDF and verified it looks correct
- [ ] I have gone through the venue-specific checklist
- [ ] I have gone through the cross-venue checklist
- [ ] I am confident this paper is ready

**If all boxes checked**: 🎉 Submit!

---

## 🆘 Last-Minute Issues

### Issue: PDF has wrong page count
- Regenerate with correct style
- Recompile with `pdflatex paper.tex` (run twice)
- Check: `pdfinfo my-paper.pdf | grep Pages`

### Issue: Equations don't match venue format
- Check venue style: `ggen paper generate --style [venue]`
- Different styles number equations differently
- Regenerate and recompile

### Issue: Bibliography missing
- Did you run `bibtex my-paper`?
- Then `pdflatex my-paper.tex` again?
- Try: `pdflatex; bibtex; pdflatex; pdflatex`

### Issue: Can't find something in PDF
- Search with Ctrl+F in PDF viewer
- If not found, item may not have been generated
- Check it's linked in main paper (ap:hasSection, math:hasEquation)

### Issue: Formatting looks wrong for venue
- Try regenerating with correct --style flag
- Compare to template provided by venue
- Ask venue support if in doubt

---

## 📞 Getting Help

**If stuck at any step**:

1. Check TROUBLESHOOTING_GUIDE.md (decision tree)
2. Check RDF_SYNTAX_GUIDE.md (syntax reference)
3. Check 000_START_HERE.md (basics)
4. Review examples/minimal-paper.rdf (working example)

**If still stuck**:

Provide:
- [ ] Your RDF file (my-paper.rdf)
- [ ] The exact error message (copy-paste)
- [ ] What you expected to happen
- [ ] What template/venue you're using

---

**Date**: 2025-11-15 | **Version**: 1.0 | **Status**: Complete pre-submission gate
