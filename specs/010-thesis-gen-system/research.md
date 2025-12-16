# Research: Reusable Ontology-Driven PhD Thesis Generation

**Feature**: 010-thesis-gen-system
**Date**: 2025-12-16
**Status**: Complete

## Research Questions

### 1. How to achieve zero hardcoded strings in Tera templates?

**Decision**: Use `{{ variable | default(value="") }}` pattern for ALL text, including LaTeX commands.

**Rationale**: Tera's default filter ensures templates never have literal strings—even "Chapter" becomes `{{ chapterLabel | default(value="") }}` sourced from ontology.

**Alternatives Considered**:
- Hardcode structural LaTeX (rejected: violates reusability requirement)
- Separate i18n file (rejected: adds complexity, not RDF-native)

**Pattern**:
```tera
{% for row in sparql_results %}
{{ row["?envBegin"] }}[{{ row["?label"] }}]
{{ row["?envLabel"] }}{% raw %}{{% endraw %}{{ row["?refLabel"] }}{% raw %}}{% endraw %}
{{ row["?content"] }}
{{ row["?envEnd"] }}
{% endfor %}
```

### 2. How to model LaTeX environments in RDF?

**Decision**: Create `thesis:LatexEnvironment` class with properties for begin/end commands, enabling environments themselves to be data.

**Rationale**: Allows complete reusability—even the LaTeX syntax is ontology-driven.

**Schema**:
```turtle
:TheoremEnv a :LatexEnvironment ;
    :envBegin "\\begin{theorem}" ;
    :envEnd "\\end{theorem}" ;
    :envLabel "Theorem" ;
    :requiresProof true .
```

### 3. How to ensure 50+ pages from ontology content?

**Decision**: Comprehensive content density requirements:
- 7 chapters × 4 sections × 500 words = 14,000 words body
- 10 theorems × 200 words (statement + proof) = 2,000 words
- 20 equations × 50 words description = 1,000 words
- 5 tables × 100 words = 500 words
- 10 figures × 100 words caption = 1,000 words
- 30 references = ~2 pages
- Front/back matter = ~5 pages
- **Total**: ~18,500 words ≈ 55-60 pages at 12pt, 1" margins

**Rationale**: Calculated page density based on standard LaTeX thesis formatting.

### 4. How to handle LaTeX special character escaping?

**Decision**: Use Tera filter `| latex_escape` (custom filter or pre-process in SPARQL).

**Rationale**: Characters `& % $ # _ { } ~ ^ \` must be escaped in LaTeX.

**Implementation Options**:
1. Custom Tera filter (preferred—ggen could add this)
2. Pre-escape in ontology content (simple but tedious)
3. SPARQL REPLACE function (complex but RDF-native)

**Workaround**: For this example, ensure ontology content is pre-escaped.

### 5. How to model cross-references in RDF?

**Decision**: Every referenceable entity has `:labelId` property generating LaTeX `\label{id}` and `\ref{id}` commands.

**Pattern**:
```turtle
:Theorem1 a :Theorem ;
    :labelId "thm:determinism" ;
    :statement "..." .

:Section2_1 a :Section ;
    :content "As proven in Theorem~\\ref{thm:determinism}..." .
```

### 6. BibTeX entry types and required fields?

**Decision**: Support standard BibTeX types with ontology properties mapping:

| BibTeX Type | Required Fields | Ontology Properties |
|-------------|-----------------|---------------------|
| @article | author, title, journal, year | :author, :title, :journal, :year |
| @inproceedings | author, title, booktitle, year | :author, :title, :booktitle, :year |
| @book | author, title, publisher, year | :author, :title, :publisher, :year |
| @phdthesis | author, title, school, year | :author, :title, :school, :year |
| @misc | author, title, howpublished | :author, :title, :howpublished |

**Rationale**: Cover 95% of academic citation needs with 5 types.

### 7. How to order chapters/sections deterministically?

**Decision**: Use `:orderIndex` property (integer) with SPARQL `ORDER BY ?orderIndex`.

**Rationale**: RDF triples are unordered; explicit ordering property ensures determinism.

**Pattern**:
```turtle
:Chapter1 a :Chapter ; :orderIndex 1 ; :title "Introduction" .
:Chapter2 a :Chapter ; :orderIndex 2 ; :title "Background" .
```

## Technology Decisions

### LaTeX Document Class

**Decision**: Use `report` class with `12pt,a4paper` options.

**Rationale**: Standard PhD thesis class supporting chapters, sections, front/back matter.

### LaTeX Packages (Minimum Required)

| Package | Purpose |
|---------|---------|
| `inputenc` | UTF-8 input |
| `fontenc` | T1 font encoding |
| `geometry` | Page margins |
| `amsmath, amsthm` | Math environments, theorem styles |
| `algorithm, algpseudocode` | Algorithm environments |
| `graphicx` | Figure inclusion |
| `booktabs` | Professional tables |
| `hyperref` | Cross-references, PDF bookmarks |
| `listings` | Code listings |
| `natbib` | Bibliography with author-year |

### Template-Ontology Mapping

| Generated File | SPARQL Query Source | Template |
|---------------|---------------------|----------|
| thesis.tex | Thesis metadata | thesis-main.tera |
| front-matter.tex | Title, abstract, acknowledgments | front-matter.tera |
| chapter-N.tex | Chapter + sections | chapter.tera |
| theorems.tex | All theorems/proofs | theorem.tera |
| equations.tex | All equations | equation.tera |
| algorithms.tex | All algorithms | algorithm.tera |
| figures.tex | All figures | figure.tera |
| tables.tex | All tables | table.tera |
| references.bib | All references | bibliography.tera |
| appendix.tex | Appendix sections | appendix.tera |

## Best Practices Identified

### 1. Ontology Design

- Separate schema (reusable) from content (topic-specific)
- Use `owl:imports` to reference schema from content
- Prefix convention: `thesis:` for schema, `content:` for instances

### 2. Template Design

- One template per document element type (not per chapter)
- Templates loop over SPARQL results
- Never embed topic-specific content
- Use `{% if %}` for optional elements, not different templates

### 3. SPARQL Query Design

- SELECT all fields needed by template in single query
- ORDER BY ensures deterministic output
- OPTIONAL for non-required fields with template defaults

### 4. File Organization

- `ontology/schema.ttl` - Reusable across all theses
- `ontology/content.ttl` - Specific thesis content
- `templates/` - Reusable LaTeX templates
- `output/` - Generated files (gitignored except examples)

## Risks and Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| LaTeX escaping issues | Medium | High | Pre-escape in ontology, validate on generation |
| Page count variance | Low | Medium | Test with actual content, adjust density targets |
| Cross-ref resolution | Low | High | Generate labels deterministically, run LaTeX twice |
| Missing optional data | Medium | Low | Tera defaults handle gracefully |
| BibTeX format errors | Medium | Medium | Validate BibTeX syntax in tests |

## Conclusion

All technical decisions resolved. Ready for Phase 1 (data model and contracts).

**Key Insight**: The breakthrough is modeling LaTeX environments themselves as RDF resources. This achieves true zero-hardcoding—even `\begin{theorem}` comes from the ontology, making templates 100% reusable across document types.
