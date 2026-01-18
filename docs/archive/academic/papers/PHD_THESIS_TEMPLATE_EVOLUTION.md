<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [PhD Thesis LaTeX Template Evolution](#phd-thesis-latex-template-evolution)
  - [Overview](#overview)
  - [Key Improvements](#key-improvements)
    - [1. Modern Bibliography System](#1-modern-bibliography-system)
    - [2. Enhanced Typography](#2-enhanced-typography)
    - [3. Smart Cross-References](#3-smart-cross-references)
    - [4. Enhanced Figure and Table Support](#4-enhanced-figure-and-table-support)
    - [5. Improved Code Listings](#5-improved-code-listings)
    - [6. Better Title Page](#6-better-title-page)
    - [7. Enhanced Front Matter](#7-enhanced-front-matter)
    - [8. Algorithm Support](#8-algorithm-support)
    - [9. Enhanced Hyperref Configuration](#9-enhanced-hyperref-configuration)
    - [10. University Compliance](#10-university-compliance)
  - [Template Structure](#template-structure)
    - [Front Matter](#front-matter)
    - [Main Matter](#main-matter)
    - [Back Matter](#back-matter)
  - [Usage](#usage)
    - [Basic Usage](#basic-usage)
    - [RDF Structure](#rdf-structure)
  - [Compilation Requirements](#compilation-requirements)
    - [Required Packages](#required-packages)
    - [Compilation Process](#compilation-process)
    - [Alternative: Use ggen compile command](#alternative-use-ggen-compile-command)
  - [Customization Options](#customization-options)
    - [Line Spacing](#line-spacing)
    - [Margins](#margins)
    - [Bibliography Style](#bibliography-style)
    - [Chapter Formatting](#chapter-formatting)
  - [Migration from v1.0.0](#migration-from-v100)
    - [Breaking Changes](#breaking-changes)
    - [Non-Breaking Enhancements](#non-breaking-enhancements)
  - [Best Practices](#best-practices)
    - [1. Use `\cref` for Cross-References](#1-use-%5Ccref-for-cross-references)
    - [2. Use `\Cref` for Sentence Start](#2-use-%5Ccref-for-sentence-start)
    - [3. Use `biblatex` Citation Commands](#3-use-biblatex-citation-commands)
    - [4. Organize Figures and Tables](#4-organize-figures-and-tables)
    - [5. Use Subfigures for Multiple Panels](#5-use-subfigures-for-multiple-panels)
  - [University Compliance](#university-compliance)
    - [Common Requirements](#common-requirements)
    - [Template Defaults](#template-defaults)
    - [Customization for Specific Universities](#customization-for-specific-universities)
  - [Performance Considerations](#performance-considerations)
    - [Compilation Speed](#compilation-speed)
    - [File Size](#file-size)
  - [Troubleshooting](#troubleshooting)
    - [biblatex Errors](#biblatex-errors)
    - [Missing Packages](#missing-packages)
    - [Compilation Errors](#compilation-errors)
  - [Future Enhancements](#future-enhancements)
    - [Potential Additions](#potential-additions)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# PhD Thesis LaTeX Template Evolution

**Date**: 2025-12-11  
**Version**: 1.0.0 → 2.0.0  
**Status**: ✅ Evolved

---

## Overview

The PhD thesis LaTeX template has been evolved from version 1.0.0 to 2.0.0 with modern LaTeX best practices, enhanced features, and improved university compliance.

---

## Key Improvements

### 1. Modern Bibliography System

**Before (v1.0.0)**:
```latex
\usepackage{cite}
\bibliographystyle{plain}
\bibliography{references}
```

**After (v2.0.0)**:
```latex
\usepackage[style=authoryear-comp,
            backend=biber,
            maxcitenames=2,
            maxbibnames=99,
            sorting=nyt,
            natbib=true,
            hyperref=true,
            backref=true,
            url=true,
            doi=true]{biblatex}
\addbibresource{references.bib}
\printbibliography[heading=bibintoc, title={References}]
```

**Benefits**:
- ✅ Modern `biblatex` with `biber` backend (replaces BibTeX)
- ✅ Better citation styles and formatting
- ✅ Hyperlinked citations
- ✅ Back references to citation locations
- ✅ DOI and URL support
- ✅ More flexible bibliography customization

### 2. Enhanced Typography

**New Packages**:
- `microtype` - Micro-typography improvements (character protrusion, font expansion)
- `titlesec` - Customize chapter/section titles
- `titletoc` - Customize TOC entries
- `csquotes` - Smart quotes

**Benefits**:
- ✅ Better text justification
- ✅ Professional typography
- ✅ Customizable chapter formatting
- ✅ Improved readability

### 3. Smart Cross-References

**New Package**: `cleveref`

**Before**:
```latex
See Figure~\ref{fig:example} and Table~\ref{tbl:data}
```

**After**:
```latex
See \cref{fig:example} and \cref{tbl:data}
% Automatically generates: "See Figure 1.2 and Table 3.4"
```

**Benefits**:
- ✅ Automatic reference type detection
- ✅ Consistent formatting
- ✅ Less manual work
- ✅ Fewer errors

### 4. Enhanced Figure and Table Support

**New Packages**:
- `subcaption` - Subfigures with proper captions
- `float` - Better float control
- `wrapfig` - Wrapped figures
- `longtable` - Multi-page tables
- `tabularx` - Auto-width tables

**Benefits**:
- ✅ Better figure/table layouts
- ✅ Subfigures support
- ✅ Multi-page tables
- ✅ More flexible positioning

### 5. Improved Code Listings

**Enhanced `listings` Configuration**:
- Line numbers
- Syntax highlighting
- Frame around code
- Better spacing
- Caption positioning

**Optional**: `minted` package for advanced syntax highlighting (requires Python pygments)

### 6. Better Title Page

**New Features**:
- University name
- Department
- Degree type
- Customizable year
- Copyright page (optional)

**Template Variables**:
- `paper.title` - Thesis title
- `paper.department` - Department/Faculty
- `paper.university` - University name
- `paper.degree` - Degree type
- `paper.year` - Graduation year
- `paper.copyright` - Copyright notice

### 7. Enhanced Front Matter

**New Sections**:
- Abstract page (with keywords)
- Copyright page (optional)
- Dedication (optional, properly formatted)
- Acknowledgments (with TOC entry)
- List of Algorithms (if algorithms exist)

**Improvements**:
- Proper page numbering (roman for front matter)
- TOC entries for all front matter sections
- Better spacing and formatting

### 8. Algorithm Support

**New Features**:
- List of Algorithms
- Better algorithm formatting
- Algorithm numbering
- Cross-references with `cleveref`

### 9. Enhanced Hyperref Configuration

**Improvements**:
- PDF metadata (title, author, subject, keywords)
- PDF bookmarks (numbered, open by default)
- Better link colors
- Proper PDF view settings

### 10. University Compliance

**Features**:
- Configurable margins (default: 1.5" left, 1" right)
- Configurable line spacing (default: 1.5, can be changed to 2.0)
- Proper page numbering (roman/arabic)
- Chapter formatting
- Bibliography formatting

---

## Template Structure

### Front Matter
1. Title page
2. Copyright page (optional)
3. Abstract (with keywords)
4. Dedication (optional)
5. Acknowledgments
6. Table of Contents
7. List of Figures
8. List of Tables
9. List of Algorithms

### Main Matter
1. Introduction
2. Background/Literature Review
3. Methodology
4. Results
5. Discussion
6. Conclusion

### Back Matter
1. Appendices
2. Bibliography
3. Index (optional)

---

## Usage

### Basic Usage

```bash
# Generate thesis from RDF
ggen paper generate thesis.rdf --template phd-thesis.tmpl --output thesis.tex

# Compile with biblatex (requires biber)
pdflatex thesis.tex
biber thesis
pdflatex thesis.tex
pdflatex thesis.tex
```

### RDF Structure

The template expects RDF data with the following structure:

```turtle
@prefix paper: <http://example.org/paper#> .

paper:thesis a paper:Paper ;
  paper:title "Your Thesis Title" ;
  paper:hasAuthor [ paper:authorName "Your Name" ] ;
  paper:department "Computer Science" ;
  paper:university "Your University" ;
  paper:degree "Doctor of Philosophy" ;
  paper:year 2025 ;
  paper:abstract "Abstract text..." ;
  paper:keywords "keyword1, keyword2, keyword3" ;
  paper:copyright true ;
  paper:dedication "Dedication text..." ;
  paper:hasSection [
    paper:sectionType "introduction" ;
    paper:sectionTitle "Introduction" ;
    paper:sectionContent "Content..."
  ] ;
  paper:hasFigure [
    paper:figureFile "figures/example.pdf" ;
    paper:figureCaption "Figure caption" ;
    paper:figureName "example"
  ] ;
  paper:hasTable [
    paper:tableCaption "Table caption" ;
    paper:tableName "data" ;
    paper:tableContent "\\begin{tabular}..."
  ] ;
  paper:hasAlgorithm [
    paper:algorithmCaption "Algorithm caption" ;
    paper:algorithmName "example" ;
    paper:algorithmContent "\\State ..."
  ] ;
  paper:bibliography "references.bib" .
```

---

## Compilation Requirements

### Required Packages
- `biblatex` (with `biber` backend)
- `microtype`
- `cleveref`
- `hyperref`
- All packages listed in template

### Compilation Process

```bash
# 1. Compile LaTeX
pdflatex thesis.tex

# 2. Run biber (replaces bibtex)
biber thesis

# 3. Compile again (resolve references)
pdflatex thesis.tex

# 4. Final compile (resolve cross-references)
pdflatex thesis.tex
```

### Alternative: Use ggen compile command

```bash
ggen paper compile thesis.tex --engine pdflatex --biber
```

---

## Customization Options

### Line Spacing

Change in template:
```latex
\onehalfspacing  % 1.5 line spacing
% or
\doublespacing   % 2.0 line spacing (double-spaced)
```

### Margins

Modify `\geometry` settings:
```latex
\geometry{
  left=1.5in,    % Binding margin
  right=1in,      % Outer margin
  top=1.25in,     % Top margin
  bottom=1.25in,  % Bottom margin
}
```

### Bibliography Style

Change `biblatex` style:
```latex
\usepackage[style=authoryear-comp, ...]{biblatex}
% Options: authoryear, numeric, alphabetic, etc.
```

### Chapter Formatting

Customize with `titlesec`:
```latex
\titleformat{\chapter}[display]
  {\normalfont\huge\bfseries}
  {\chaptertitlename\ \thechapter}{20pt}{\Huge}
```

---

## Migration from v1.0.0

### Breaking Changes

1. **Bibliography**: Must use `biber` instead of `bibtex`
   - Old: `bibtex thesis`
   - New: `biber thesis`

2. **Bibliography File**: Must use `\addbibresource` instead of `\bibliography`
   - Old: `\bibliography{references}`
   - New: `\addbibresource{references.bib}`

3. **Bibliography Output**: Must use `\printbibliography` instead of `\bibliography`
   - Old: `\bibliography{references}`
   - New: `\printbibliography[heading=bibintoc, title={References}]`

### Non-Breaking Enhancements

- All other features are backward compatible
- Existing RDF data works without changes
- Template variables are optional (backward compatible)

---

## Best Practices

### 1. Use `\cref` for Cross-References

```latex
% Good
See \cref{fig:example} for details.

% Avoid
See Figure~\ref{fig:example} for details.
```

### 2. Use `\Cref` for Sentence Start

```latex
% Good
\Cref{ch:methodology} describes our approach.

% Avoid
Chapter~\ref{ch:methodology} describes our approach.
```

### 3. Use `biblatex` Citation Commands

```latex
% Good
\cite{author2024}              % (Author 2024)
\parencite{author2024}         % (Author 2024)
\footcite{author2024}          % Footnote citation
\textcite{author2024}         % Author (2024)

% Avoid (old natbib style)
\citep{author2024}            % Still works but less flexible
```

### 4. Organize Figures and Tables

```latex
% Use descriptive labels
\label{fig:methodology-overview}
\label{tbl:experimental-results}

% Use \cref for references
As shown in \cref{fig:methodology-overview}...
```

### 5. Use Subfigures for Multiple Panels

```latex
\begin{figure}[htbp]
  \centering
  \begin{subfigure}{0.48\textwidth}
    \includegraphics[width=\textwidth]{fig1.pdf}
    \caption{First panel}
  \end{subfigure}
  \hfill
  \begin{subfigure}{0.48\textwidth}
    \includegraphics[width=\textwidth]{fig2.pdf}
    \caption{Second panel}
  \end{subfigure}
  \caption{Overall caption}
  \label{fig:multi-panel}
\end{figure}
```

---

## University Compliance

### Common Requirements

1. **Page Size**: A4 or Letter (configurable)
2. **Margins**: Typically 1" all around (configurable)
3. **Line Spacing**: Double-spaced (2.0) or 1.5 (configurable)
4. **Font Size**: 12pt (standard)
5. **Page Numbering**: Roman (front matter), Arabic (main matter)
6. **Binding**: Left margin larger for two-sided printing

### Template Defaults

- **Margins**: 1.5" left, 1" right (two-sided binding)
- **Line Spacing**: 1.5 (change to 2.0 for double-spaced)
- **Font Size**: 12pt
- **Page Numbering**: Automatic (roman/arabic)

### Customization for Specific Universities

Modify the template preamble to match university requirements:

```latex
% Example: MIT requirements
\geometry{
  left=1.5in,
  right=1in,
  top=1in,
  bottom=1in,
}
\doublespacing  % MIT requires double-spacing
```

---

## Performance Considerations

### Compilation Speed

- `biber` is faster than `bibtex` for large bibliographies
- `microtype` adds minimal compilation time
- `cleveref` adds minimal overhead

### File Size

- PDF size may increase slightly due to hyperlinks and bookmarks
- Use `draft` mode during writing to speed up compilation

---

## Troubleshooting

### biblatex Errors

**Problem**: `biber` not found  
**Solution**: Install `biber`:
```bash
# macOS
brew install biber

# Linux
sudo apt-get install biber

# Or use TeX Live
tlmgr install biber
```

### Missing Packages

**Problem**: Package not found  
**Solution**: Install missing packages:
```bash
tlmgr install <package-name>
```

### Compilation Errors

**Problem**: Cross-reference errors  
**Solution**: Run compilation multiple times:
```bash
pdflatex thesis.tex
biber thesis
pdflatex thesis.tex
pdflatex thesis.tex
```

---

## Future Enhancements

### Potential Additions

1. **Multi-language Support**: Babel configuration for multiple languages
2. **Custom Fonts**: Fontspec for custom fonts (requires XeLaTeX)
3. **Advanced Graphics**: TikZ/PGFPlots integration
4. **Thesis-specific Packages**: University-specific packages
5. **Automated Formatting**: Scripts to check university compliance

---

## Summary

The evolved PhD thesis template (v2.0.0) provides:

- ✅ Modern bibliography system (biblatex/biber)
- ✅ Enhanced typography (microtype)
- ✅ Smart cross-references (cleveref)
- ✅ Better figure/table support
- ✅ Improved code listings
- ✅ Enhanced title page
- ✅ Better front matter organization
- ✅ Algorithm support
- ✅ University compliance
- ✅ Professional formatting

**Result**: A production-ready, modern PhD thesis template that follows current LaTeX best practices and academic standards.





