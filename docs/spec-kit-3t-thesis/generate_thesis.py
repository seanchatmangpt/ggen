#!/usr/bin/env python3
"""
Spec-Kit-3T Thesis Generator (80/20 Implementation)
Constitutional Equation: thesis.tex = μ(ontology.ttl)

This is a prototype implementation demonstrating the core pipeline:
μ₂ (SPARQL extraction) + μ₃ (Tera template rendering) → LaTeX generation
"""

import sys
from pathlib import Path
from typing import Dict, List, Any
import re

try:
    from rdflib import Graph, Namespace
    from rdflib.plugins.sparql import prepareQuery
    from jinja2 import Environment, FileSystemLoader, select_autoescape
    import toml
except ImportError as e:
    print(f"Missing dependency: {e}")
    print("Install with: pip install rdflib jinja2 toml")
    sys.exit(1)


# Namespaces
THESIS = Namespace("http://github.com/seanchatmangpt/spec-kit-3t/thesis#")
DIATAXIS = Namespace("http://diataxis.fr/")
CONTENT = Namespace("http://github.com/seanchatmangpt/spec-kit-3t/content#")


class ThesisGenerator:
    """Generate LaTeX thesis from RDF ontology using SPARQL + Jinja2"""

    def __init__(self, base_path: Path):
        self.base_path = base_path
        self.ontology_dir = base_path / "ontology"
        self.templates_dir = base_path / "templates"
        self.output_dir = base_path / "generated"
        self.output_dir.mkdir(exist_ok=True)

        # Load RDF graph (μ₁ - Normalization)
        print("μ₁: Loading RDF ontology...")
        self.graph = Graph()
        self.graph.bind("thesis", THESIS)
        self.graph.bind("diataxis", DIATAXIS)
        self.graph.bind("", CONTENT)

        # Load all TTL files
        for ttl_file in self.ontology_dir.glob("*.ttl"):
            print(f"  Loading: {ttl_file.name}")
            self.graph.parse(ttl_file, format="turtle")

        print(f"  Loaded {len(self.graph)} triples ✓")

        # Setup Jinja2 environment (Tera equivalent)
        self.jinja_env = Environment(
            loader=FileSystemLoader(str(self.templates_dir)),
            autoescape=select_autoescape(['html', 'xml']),
            trim_blocks=True,
            lstrip_blocks=True
        )

        # Add Tera-like filters
        self.jinja_env.filters['slugify'] = self.slugify

    @staticmethod
    def slugify(text: str) -> str:
        """Convert text to slug (Tera filter equivalent)"""
        text = text.lower()
        text = re.sub(r'[^\w\s-]', '', text)
        text = re.sub(r'[-\s]+', '-', text)
        return text.strip('-')

    def execute_sparql(self, query_str: str) -> List[Dict[str, Any]]:
        """Execute SPARQL query and return results as list of dicts (μ₂)"""
        try:
            query = prepareQuery(query_str)
            results = self.graph.query(query)

            rows = []
            for row in results:
                row_dict = {}
                for var in results.vars:
                    value = row[var]
                    if value is not None:
                        # Convert to string, handle literals
                        row_dict[str(var)] = str(value)
                rows.append(row_dict)

            return rows
        except Exception as e:
            print(f"  SPARQL Error: {e}")
            return []

    def render_template(self, template_name: str, context: Dict[str, Any]) -> str:
        """Render Jinja2 template with context (μ₃ - Emission)"""
        try:
            template = self.jinja_env.get_template(template_name)
            return template.render(**context)
        except Exception as e:
            print(f"  Template Error ({template_name}): {e}")
            return ""

    def generate_frontmatter(self):
        """Generate frontmatter.tex"""
        print("\nGenerating frontmatter...")

        query = """
        PREFIX thesis: <http://github.com/seanchatmangpt/spec-kit-3t/thesis#>
        PREFIX : <http://github.com/seanchatmangpt/spec-kit-3t/content#>

        SELECT ?title ?subtitle ?author ?year ?abstract ?dedication ?acknowledgments
        WHERE {
            :spec-kit-3t-thesis
                thesis:hasTitle ?title ;
                thesis:hasSubtitle ?subtitle ;
                thesis:hasYear ?year ;
                thesis:hasAbstract ?abstractNode ;
                thesis:dedication ?dedication ;
                thesis:acknowledgments ?acknowledgments .

            ?abstractNode thesis:hasContent ?abstract .

            :author-chatman thesis:hasName ?author .
        }
        """

        rows = self.execute_sparql(query)
        if rows:
            output = self.render_template("frontmatter.tera", {"rows": rows})
            output_file = self.output_dir / "frontmatter.tex"
            output_file.write_text(output)
            print(f"  ✓ {output_file.name}")

    def generate_chapter(self, chapter_num: int, chapter_type: str, template_name: str):
        """Generate a chapter LaTeX file"""
        print(f"\nGenerating Chapter {chapter_num} ({chapter_type})...")

        # Build SPARQL query dynamically
        if chapter_type == "diataxis":
            # Diataxis chapters (3-6) have special properties
            query = f"""
            PREFIX thesis: <http://github.com/seanchatmangpt/spec-kit-3t/thesis#>
            PREFIX diataxis: <http://diataxis.fr/>
            PREFIX : <http://github.com/seanchatmangpt/spec-kit-3t/content#>

            SELECT ?chapterTitle ?sectionTitle ?sectionContent ?learningObjective ?task ?technicalDetail ?concept
            WHERE {{
                ?chapter thesis:chapterNumber {chapter_num} ;
                         thesis:chapterTitle ?chapterTitle ;
                         thesis:hasSection ?section .

                ?section thesis:sectionTitle ?sectionTitle ;
                         thesis:hasContent ?sectionContent .

                OPTIONAL {{ ?chapter diataxis:learningObjective ?learningObjective }}
                OPTIONAL {{ ?chapter diataxis:task ?task }}
                OPTIONAL {{ ?chapter diataxis:technicalDetail ?technicalDetail }}
                OPTIONAL {{ ?chapter diataxis:concept ?concept }}
            }}
            ORDER BY ?sectionTitle
            """
        else:
            # Standard chapters
            query = f"""
            PREFIX thesis: <http://github.com/seanchatmangpt/spec-kit-3t/thesis#>
            PREFIX : <http://github.com/seanchatmangpt/spec-kit-3t/content#>

            SELECT ?chapterTitle ?sectionTitle ?sectionContent
            WHERE {{
                ?chapter thesis:chapterNumber {chapter_num} ;
                         thesis:chapterTitle ?chapterTitle ;
                         thesis:hasSection ?section .

                ?section thesis:sectionTitle ?sectionTitle ;
                         thesis:hasContent ?sectionContent .
            }}
            ORDER BY ?sectionTitle
            """

        rows = self.execute_sparql(query)
        if rows:
            output = self.render_template(template_name, {"rows": rows})
            output_file = self.output_dir / f"chapter-{chapter_num:02d}.tex"
            output_file.write_text(output)
            print(f"  ✓ {output_file.name} ({len(rows)} sections)")
        else:
            print(f"  ⚠ No content found for chapter {chapter_num}")

    def generate_main_thesis(self):
        """Generate main thesis.tex file"""
        print("\nGenerating main thesis.tex...")

        query = """
        PREFIX thesis: <http://github.com/seanchatmangpt/spec-kit-3t/thesis#>
        PREFIX : <http://github.com/seanchatmangpt/spec-kit-3t/content#>

        SELECT ?title ?subtitle ?author ?year ?institution ?degree
        WHERE {
            :spec-kit-3t-thesis
                thesis:hasTitle ?title ;
                thesis:hasSubtitle ?subtitle ;
                thesis:hasYear ?year ;
                thesis:hasInstitution ?institution ;
                thesis:hasDegree ?degree .

            :author-chatman thesis:hasName ?author .
        }
        """

        rows = self.execute_sparql(query)
        if rows:
            from datetime import datetime
            context = {
                "rows": rows,
                "config": {
                    "timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                }
            }
            output = self.render_template("thesis-main.tera", context)
            output_file = self.output_dir / "thesis-main.tex"
            output_file.write_text(output)
            print(f"  ✓ {output_file.name}")

    def generate_all(self):
        """Execute complete pipeline (μ₂ + μ₃)"""
        print("\n" + "="*70)
        print("Spec-Kit-3T Thesis Generator")
        print("Constitutional Equation: thesis.tex = μ(ontology.ttl)")
        print("="*70)

        # Generate frontmatter
        self.generate_frontmatter()

        # Generate standard chapters (1, 2, 7-10)
        standard_chapters = [
            (1, "chapter.tera"),
            (2, "chapter.tera"),
            (7, "chapter.tera"),
            (8, "chapter.tera"),
            (9, "chapter.tera"),
            (10, "chapter.tera"),
        ]

        for chapter_num, template in standard_chapters:
            self.generate_chapter(chapter_num, "standard", template)

        # Generate Diataxis chapters (3-6)
        diataxis_chapters = [
            (3, "diataxis-tutorial.tera"),
            (4, "diataxis-howto.tera"),
            (5, "diataxis-reference.tera"),
            (6, "diataxis-explanation.tera"),
        ]

        for chapter_num, template in diataxis_chapters:
            self.generate_chapter(chapter_num, "diataxis", template)

        # Generate main thesis file
        self.generate_main_thesis()

        print("\n" + "="*70)
        print("✓ Generation Complete!")
        print(f"  Output directory: {self.output_dir}")
        print(f"  Generated {len(list(self.output_dir.glob('*.tex')))} LaTeX files")
        print("="*70)


def main():
    base_path = Path(__file__).parent
    generator = ThesisGenerator(base_path)
    generator.generate_all()

    print("\nNext steps:")
    print("  cd generated")
    print("  pdflatex thesis-main.tex")
    print("  biber thesis-main")
    print("  pdflatex thesis-main.tex")
    print("  pdflatex thesis-main.tex")


if __name__ == "__main__":
    main()
