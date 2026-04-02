#!/usr/bin/env python3
"""
Direct LaTeX generation from RDF thesis content.
Fallback generator when ggen is not available.
"""

from rdflib import Graph, Namespace, RDF, RDFS
from pathlib import Path
import os

# Define namespaces
THESIS = Namespace("https://ggen.io/ontology/thesis#")
CONTENT = Namespace("https://ggen.io/thesis/construct-schema/")
SCHEMA = Namespace("http://schema.org/")

def load_ontology():
    """Load the thesis ontology"""
    g = Graph()
    g.parse("ontology/content.ttl", format="turtle")
    g.parse("ontology/thesis-schema.ttl", format="turtle")
    return g

def get_thesis_metadata(g):
    """Extract thesis metadata"""
    query = """
    PREFIX thesis: <https://ggen.io/ontology/thesis#>
    SELECT ?title ?subtitle ?author ?institution ?department ?date ?abstract ?dedication ?acknowledgments
    WHERE {
      ?t a thesis:Thesis ;
         thesis:title ?title ;
         thesis:author ?author ;
         thesis:institution ?institution ;
         thesis:date ?date ;
         thesis:abstract ?abstract .
      OPTIONAL { ?t thesis:subtitle ?subtitle }
      OPTIONAL { ?t thesis:department ?department }
      OPTIONAL { ?t thesis:dedication ?dedication }
      OPTIONAL { ?t thesis:acknowledgments ?acknowledgments }
    }
    """
    results = list(g.query(query))
    if results:
        return results[0]
    return None

def get_chapters(g):
    """Get all chapters ordered"""
    query = """
    PREFIX thesis: <https://ggen.io/ontology/thesis#>
    SELECT ?chapter ?orderIndex ?title ?abstract ?labelId
    WHERE {
      ?chapter a thesis:Chapter ;
               thesis:orderIndex ?orderIndex ;
               thesis:title ?title ;
               thesis:labelId ?labelId .
      OPTIONAL { ?chapter thesis:abstract ?abstract }
    }
    ORDER BY ?orderIndex
    """
    return list(g.query(query))

def get_sections(g, chapter_uri):
    """Get sections for a chapter"""
    query = f"""
    PREFIX thesis: <https://ggen.io/ontology/thesis#>
    SELECT ?section ?orderIndex ?title ?content ?labelId
    WHERE {{
      <{chapter_uri}> thesis:hasSection ?section .
      ?section thesis:orderIndex ?orderIndex ;
               thesis:title ?title ;
               thesis:content ?content ;
               thesis:labelId ?labelId .
    }}
    ORDER BY ?orderIndex
    """
    return list(g.query(query))

def get_equations(g, section_uri):
    """Get equations for a section"""
    query = f"""
    PREFIX thesis: <https://ggen.io/ontology/thesis#>
    SELECT ?equation ?orderIndex ?latex ?description ?labelId
    WHERE {{
      <{section_uri}> thesis:hasEquation ?equation .
      ?equation thesis:orderIndex ?orderIndex ;
                thesis:latex ?latex ;
                thesis:labelId ?labelId .
      OPTIONAL {{ ?equation thesis:description ?description }}
    }}
    ORDER BY ?orderIndex
    """
    return list(g.query(query))

def get_theorems(g, section_uri):
    """Get theorems for a section"""
    query = f"""
    PREFIX thesis: <https://ggen.io/ontology/thesis#>
    SELECT ?theorem ?orderIndex ?theoremType ?theoremName ?statement ?proof ?labelId
    WHERE {{
      <{section_uri}> thesis:hasTheorem ?theorem .
      ?theorem thesis:orderIndex ?orderIndex ;
               thesis:theoremType ?theoremType ;
               thesis:statement ?statement ;
               thesis:labelId ?labelId .
      OPTIONAL {{ ?theorem thesis:theoremName ?theoremName }}
      OPTIONAL {{ ?theorem thesis:proof ?proof }}
    }}
    ORDER BY ?orderIndex
    """
    return list(g.query(query))

def get_code_listings(g, section_uri):
    """Get code listings for a section"""
    query = f"""
    PREFIX thesis: <https://ggen.io/ontology/thesis#>
    SELECT ?listing ?orderIndex ?language ?caption ?code ?labelId
    WHERE {{
      <{section_uri}> thesis:hasCodeListing ?listing .
      ?listing thesis:orderIndex ?orderIndex ;
               thesis:language ?language ;
               thesis:caption ?caption ;
               thesis:code ?code ;
               thesis:labelId ?labelId .
    }}
    ORDER BY ?orderIndex
    """
    return list(g.query(query))

def get_tables(g, section_uri):
    """Get tables for a section"""
    query = f"""
    PREFIX thesis: <https://ggen.io/ontology/thesis#>
    SELECT ?table ?orderIndex ?caption ?labelId
    WHERE {{
      <{section_uri}> thesis:hasTable ?table .
      ?table thesis:orderIndex ?orderIndex ;
             thesis:caption ?caption ;
             thesis:labelId ?labelId .
    }}
    ORDER BY ?orderIndex
    """
    return list(g.query(query))

def get_table_rows(g, table_uri):
    """Get rows for a table"""
    query = f"""
    PREFIX thesis: <https://ggen.io/ontology/thesis#>
    SELECT ?row ?rowIndex ?cell
    WHERE {{
      <{table_uri}> thesis:hasRow ?row .
      ?row thesis:orderIndex ?rowIndex ;
           thesis:cell ?cell .
    }}
    ORDER BY ?rowIndex
    """
    return list(g.query(query))

def get_references(g):
    """Get all bibliography references"""
    query = """
    PREFIX thesis: <https://ggen.io/ontology/thesis#>
    SELECT ?ref ?citeKey ?bibType ?author ?title ?year ?journal ?booktitle
           ?publisher ?school ?volume ?pages ?doi ?url ?howpublished
    WHERE {
      ?ref a thesis:Reference ;
           thesis:citeKey ?citeKey ;
           thesis:bibType ?bibType ;
           thesis:author ?author ;
           thesis:title ?title ;
           thesis:year ?year .
      OPTIONAL { ?ref thesis:journal ?journal }
      OPTIONAL { ?ref thesis:booktitle ?booktitle }
      OPTIONAL { ?ref thesis:publisher ?publisher }
      OPTIONAL { ?ref thesis:school ?school }
      OPTIONAL { ?ref thesis:volume ?volume }
      OPTIONAL { ?ref thesis:pages ?pages }
      OPTIONAL { ?ref thesis:doi ?doi }
      OPTIONAL { ?ref thesis:url ?url }
      OPTIONAL { ?ref thesis:howpublished ?howpublished }
    }
    ORDER BY ?citeKey
    """
    return list(g.query(query))

def generate_preamble():
    """Generate LaTeX preamble"""
    return r"""\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{amsmath,amsthm,amssymb}
\usepackage{algorithm2e}
\usepackage{listings}
\usepackage{xcolor}
\usepackage{graphicx}
\usepackage{hyperref}
\usepackage{biblatex}
\addbibresource{references.bib}

% Theorem environments
\newtheorem{theorem}{Theorem}[chapter]
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{corollary}[theorem]{Corollary}
\newtheorem{proposition}[theorem]{Proposition}
\newtheorem{definition}[theorem]{Definition}

% Code listing settings
\lstset{
  basicstyle=\ttfamily\small,
  breaklines=true,
  frame=single,
  numbers=left,
  numberstyle=\tiny,
  showstringspaces=false
}
"""

def escape_latex(text):
    """Escape LaTeX special characters (except those in math mode)"""
    if not text:
        return ""
    # Don't escape if already in LaTeX format
    if '\\' in text or '$' in text or '{' in text:
        return str(text)
    text = str(text)
    replacements = {
        '&': r'\&',
        '%': r'\%',
        '#': r'\#',
        '_': r'\_',
    }
    for old, new in replacements.items():
        text = text.replace(old, new)
    return text

def generate_main_document(g, metadata):
    """Generate main thesis document"""
    content = r"""\documentclass[12pt,a4paper]{report}
"""
    content += generate_preamble()

    content += f"\n\\title{{\\textbf{{{escape_latex(metadata.title)}}}"
    if metadata.subtitle:
        content += f"\\\\[1em]\\large {escape_latex(metadata.subtitle)}"
    content += "}\n"

    content += f"\\author{{{escape_latex(metadata.author)} \\\\ {escape_latex(metadata.institution)}"
    if metadata.department:
        content += f" \\\\ {escape_latex(metadata.department)}"
    content += "}\n"

    content += f"\\date{{{escape_latex(metadata.date)}}}\n\n"
    content += r"\begin{document}" + "\n\n"
    content += r"\maketitle" + "\n\n"

    # Front matter
    content += r"\begin{abstract}" + "\n"
    content += str(metadata.abstract) + "\n"
    content += r"\end{abstract}" + "\n\n"

    if metadata.dedication:
        content += r"\chapter*{Dedication}" + "\n"
        content += str(metadata.dedication) + "\n\n"

    if metadata.acknowledgments:
        content += r"\chapter*{Acknowledgments}" + "\n"
        content += str(metadata.acknowledgments) + "\n\n"

    content += r"\tableofcontents" + "\n"
    content += r"\listoftables" + "\n\n"

    # Chapters
    chapters = get_chapters(g)
    for chapter in chapters:
        content += generate_chapter(g, chapter)

    # Bibliography
    content += "\n\\printbibliography\n\n"
    content += r"\end{document}"

    return content

def generate_chapter(g, chapter):
    """Generate a chapter with all its sections"""
    content = f"\n\\chapter{{{escape_latex(chapter.title)}}}\n"
    content += f"\\label{{{chapter.labelId}}}\n\n"

    if chapter.abstract:
        content += str(chapter.abstract) + "\n\n"

    sections = get_sections(g, chapter.chapter)
    for section in sections:
        content += generate_section(g, section)

    return content

def generate_section(g, section):
    """Generate a section with all its content"""
    content = f"\n\\section{{{escape_latex(section.title)}}}\n"
    content += f"\\label{{{section.labelId}}}\n\n"
    content += str(section.content) + "\n\n"

    # Equations
    equations = get_equations(g, section.section)
    for eq in equations:
        content += "\\begin{equation}\n"
        content += f"\\label{{{eq.labelId}}}\n"
        content += str(eq.latex) + "\n"
        content += "\\end{equation}\n"
        if eq.description:
            content += str(eq.description) + "\n\n"

    # Theorems
    theorems = get_theorems(g, section.section)
    for thm in theorems:
        content += f"\\begin{{{thm.theoremType}}}"
        if thm.theoremName:
            content += f"[{escape_latex(thm.theoremName)}]"
        content += "\n"
        content += f"\\label{{{thm.labelId}}}\n"
        content += str(thm.statement) + "\n"
        content += f"\\end{{{thm.theoremType}}}\n\n"
        if thm.proof:
            content += "\\begin{proof}\n"
            content += str(thm.proof) + "\n"
            content += "\\end{proof}\n\n"

    # Code listings
    listings = get_code_listings(g, section.section)
    for lst in listings:
        content += f"\\begin{{lstlisting}}[language={lst.language},caption={{{escape_latex(lst.caption)}}},label={{{lst.labelId}}}]\n"
        content += str(lst.code) + "\n"
        content += "\\end{lstlisting}\n\n"

    # Tables
    tables = get_tables(g, section.section)
    for tbl in tables:
        rows = get_table_rows(g, tbl.table)
        if rows:
            cells_per_row = len(str(rows[0].cell).split(', '))
            content += "\\begin{table}[h]\n"
            content += "\\centering\n"
            content += f"\\begin{{tabular}}{{|{'l|' * cells_per_row}}}\n"
            content += "\\hline\n"
            for row in rows:
                cells = str(row.cell).split(', ')
                content += " & ".join(escape_latex(c) for c in cells) + " \\\\\n"
                content += "\\hline\n"
            content += "\\end{tabular}\n"
            content += f"\\caption{{{escape_latex(tbl.caption)}}}\n"
            content += f"\\label{{{tbl.labelId}}}\n"
            content += "\\end{table}\n\n"

    return content

def generate_bibliography(g):
    """Generate BibTeX bibliography"""
    refs = get_references(g)
    content = ""

    for ref in refs:
        content += f"@{ref.bibType}{{{ref.citeKey},\n"
        content += f"  author = {{{ref.author}}},\n"
        content += f"  title = {{{ref.title}}},\n"
        content += f"  year = {{{ref.year}}},\n"
        if ref.journal:
            content += f"  journal = {{{ref.journal}}},\n"
        if ref.booktitle:
            content += f"  booktitle = {{{ref.booktitle}}},\n"
        if ref.publisher:
            content += f"  publisher = {{{ref.publisher}}},\n"
        if ref.school:
            content += f"  school = {{{ref.school}}},\n"
        if ref.volume:
            content += f"  volume = {{{ref.volume}}},\n"
        if ref.pages:
            content += f"  pages = {{{ref.pages}}},\n"
        if ref.doi:
            content += f"  doi = {{{ref.doi}}},\n"
        if ref.url:
            content += f"  url = {{{ref.url}}},\n"
        if ref.howpublished:
            content += f"  howpublished = {{{ref.howpublished}}},\n"
        content += "}\n\n"

    return content

def main():
    print("Loading ontology...")
    g = load_ontology()

    print("Extracting thesis metadata...")
    metadata = get_thesis_metadata(g)
    if not metadata:
        print("ERROR: No thesis metadata found!")
        return

    # Create output directory
    output_dir = Path("output")
    output_dir.mkdir(exist_ok=True)

    print("Generating main document...")
    main_doc = generate_main_document(g, metadata)
    with open(output_dir / "thesis.tex", "w") as f:
        f.write(main_doc)

    print("Generating bibliography...")
    bib = generate_bibliography(g)
    with open(output_dir / "references.bib", "w") as f:
        f.write(bib)

    print(f"✓ Generated LaTeX files in {output_dir}/")
    print(f"  - thesis.tex")
    print(f"  - references.bib")
    print("\nTo compile:")
    print(f"  cd {output_dir}/")
    print("  pdflatex thesis.tex")
    print("  biber thesis")
    print("  pdflatex thesis.tex")
    print("  pdflatex thesis.tex")

if __name__ == "__main__":
    main()
