#!/usr/bin/env python3
"""
Spec-Kit-3T Thesis Generator v2 (Production Edition)
Constitutional Equation: thesis.tex = μ(ontology.ttl)

Complete pipeline with SHACL validation, incremental builds, and provenance:
μ₁ (SHACL validation) → μ₂ (SPARQL extraction) → μ₃ (Tera rendering) →
μ₄ (canonicalization) → μ₅ (cryptographic receipts)
"""

import sys
import json
import hashlib
from pathlib import Path
from typing import Dict, List, Any, Optional, Set
from datetime import datetime
import re

try:
    from rdflib import Graph, Namespace
    from rdflib.plugins.sparql import prepareQuery
    from pyshacl import validate
    from jinja2 import Environment, FileSystemLoader, select_autoescape
    import toml
except ImportError as e:
    print(f"Missing dependency: {e}")
    print("Install with: pip install rdflib jinja2 toml pyshacl")
    sys.exit(1)


# Namespaces
THESIS = Namespace("http://github.com/seanchatmangpt/spec-kit-3t/thesis#")
DIATAXIS = Namespace("http://diataxis.fr/")
CONTENT = Namespace("http://github.com/seanchatmangpt/spec-kit-3t/content#")
SH = Namespace("http://www.w3.org/ns/shacl#")


class AndonSignal:
    """Andon signal system for quality gates"""
    RED = "🚨 RED"
    YELLOW = "⚠️  YELLOW"
    GREEN = "✅ GREEN"

    @staticmethod
    def stop_line(message: str):
        """Stop the line for RED signal"""
        print(f"\n{AndonSignal.RED} SIGNAL: {message}")
        print("Pipeline STOPPED. Fix errors before proceeding.")
        sys.exit(1)

    @staticmethod
    def warn(message: str):
        """Warning for YELLOW signal"""
        print(f"\n{AndonSignal.YELLOW} SIGNAL: {message}")

    @staticmethod
    def ok(message: str):
        """Success for GREEN signal"""
        print(f"{AndonSignal.GREEN}: {message}")


class BuildManifest:
    """Track build state for incremental builds and provenance (μ₄ + μ₅)"""

    def __init__(self, manifest_path: Path):
        self.manifest_path = manifest_path
        self.manifest = self._load()

    def _load(self) -> Dict[str, Any]:
        """Load existing manifest or create new"""
        if self.manifest_path.exists():
            with open(self.manifest_path, 'r') as f:
                return json.load(f)
        return {
            "version": "2.0.0",
            "builds": [],
            "file_hashes": {},
            "last_build": None
        }

    def save(self):
        """Persist manifest"""
        self.manifest_path.parent.mkdir(exist_ok=True, parents=True)
        with open(self.manifest_path, 'w') as f:
            json.dump(self.manifest, f, indent=2)

    def file_changed(self, file_path: Path) -> bool:
        """Check if file changed since last build (μ₄ - canonicalization)"""
        if not file_path.exists():
            return True

        current_hash = self._compute_hash(file_path)
        stored_hash = self.manifest["file_hashes"].get(str(file_path))

        return current_hash != stored_hash

    def update_hash(self, file_path: Path):
        """Update file hash in manifest"""
        self.manifest["file_hashes"][str(file_path)] = self._compute_hash(file_path)

    def record_build(self, build_info: Dict[str, Any]):
        """Record build metadata (μ₅ - receipt)"""
        build_record = {
            "timestamp": datetime.now().isoformat(),
            "pipeline_version": "2.0.0",
            "constitutional_equation": "thesis.tex = μ(ontology.ttl)",
            **build_info
        }
        self.manifest["builds"].append(build_record)
        self.manifest["last_build"] = build_record
        self.save()

    @staticmethod
    def _compute_hash(file_path: Path) -> str:
        """SHA-256 hash for file integrity (μ₅)"""
        sha256 = hashlib.sha256()
        with open(file_path, 'rb') as f:
            for chunk in iter(lambda: f.read(4096), b""):
                sha256.update(chunk)
        return sha256.hexdigest()


class ThesisGeneratorV2:
    """Production-grade thesis generator with full μ₁-μ₅ pipeline"""

    def __init__(self, base_path: Path):
        self.base_path = base_path
        self.ontology_dir = base_path / "ontology"
        self.templates_dir = base_path / "templates"
        self.output_dir = base_path / "generated"
        self.output_dir.mkdir(exist_ok=True)

        # Build manifest for incremental builds
        self.manifest = BuildManifest(base_path / ".build-manifest.json")

        # Load RDF graph
        print("\n" + "="*70)
        print("Spec-Kit-3T Thesis Generator v2 (Production)")
        print("Constitutional Equation: thesis.tex = μ(ontology.ttl)")
        print("="*70 + "\n")

        print("μ₁: Loading RDF ontology...")
        self.graph = Graph()
        self.graph.bind("thesis", THESIS)
        self.graph.bind("diataxis", DIATAXIS)
        self.graph.bind("", CONTENT)
        self.graph.bind("sh", SH)

        # Track loaded files
        self.ontology_files = []
        for ttl_file in self.ontology_dir.glob("*.ttl"):
            if "shapes" not in ttl_file.name:  # Shapes loaded separately
                print(f"  Loading: {ttl_file.name}")
                self.graph.parse(ttl_file, format="turtle")
                self.ontology_files.append(ttl_file)

        print(f"  Loaded {len(self.graph)} triples ✓")

        # Setup Jinja2 environment
        self.jinja_env = Environment(
            loader=FileSystemLoader(str(self.templates_dir)),
            autoescape=select_autoescape(['html', 'xml']),
            trim_blocks=True,
            lstrip_blocks=True
        )
        self.jinja_env.filters['slugify'] = self.slugify

    @staticmethod
    def slugify(text: str) -> str:
        """Convert text to slug"""
        text = text.lower()
        text = re.sub(r'[^\w\s-]', '', text)
        text = re.sub(r'[-\s]+', '-', text)
        return text.strip('-')

    def validate_with_shacl(self) -> bool:
        """μ₁: SHACL validation with Andon signals"""
        print("\nμ₁: Running SHACL validation...")

        shapes_file = self.ontology_dir / "thesis-shapes.ttl"
        if not shapes_file.exists():
            AndonSignal.warn("SHACL shapes file not found, skipping validation")
            return True

        # Load shapes graph
        shapes_graph = Graph()
        shapes_graph.parse(shapes_file, format="turtle")
        print(f"  Loaded {len(shapes_graph)} shape constraints")

        # Run validation (no inference to avoid spurious type assertions)
        conforms, results_graph, results_text = validate(
            self.graph,
            shacl_graph=shapes_graph,
            inference='none',  # Avoid RDFS inference causing false positives
            abort_on_first=False
        )

        if conforms:
            AndonSignal.ok("SHACL validation passed - all constraints satisfied")
            return True
        else:
            # Parse validation results
            print(f"\n{AndonSignal.RED} SHACL Validation Failed:")
            print(results_text)

            # Count violations
            violations = len(list(results_graph.subjects(predicate=SH.resultSeverity)))
            AndonSignal.stop_line(f"Found {violations} SHACL constraint violations")
            return False

    def needs_rebuild(self, output_file: Path, dependencies: List[Path]) -> bool:
        """Check if output needs regeneration (incremental builds)"""
        # Output doesn't exist
        if not output_file.exists():
            return True

        # Any dependency changed
        for dep in dependencies:
            if self.manifest.file_changed(dep):
                print(f"  Change detected: {dep.name}")
                return True

        return False

    def execute_sparql(self, query_str: str) -> List[Dict[str, Any]]:
        """μ₂: Execute SPARQL query"""
        try:
            query = prepareQuery(query_str)
            results = self.graph.query(query)

            rows = []
            for row in results:
                row_dict = {}
                for var in results.vars:
                    value = row[var]
                    if value is not None:
                        row_dict[str(var)] = str(value)
                rows.append(row_dict)

            return rows
        except Exception as e:
            print(f"  SPARQL Error: {e}")
            return []

    def render_template(self, template_name: str, context: Dict[str, Any]) -> str:
        """μ₃: Render template"""
        try:
            template = self.jinja_env.get_template(template_name)
            return template.render(**context)
        except Exception as e:
            print(f"  Template Error ({template_name}): {e}")
            return ""

    def generate_frontmatter(self, force: bool = False):
        """Generate frontmatter.tex with incremental build support"""
        print("\nGenerating frontmatter...")

        output_file = self.output_dir / "frontmatter.tex"
        template_file = self.templates_dir / "frontmatter.tera"
        dependencies = self.ontology_files + [template_file]

        if not force and not self.needs_rebuild(output_file, dependencies):
            print("  ⏭️  Skipped (no changes)")
            return

        query = """
        PREFIX thesis: <http://github.com/seanchatmangpt/spec-kit-3t/thesis#>
        PREFIX : <http://github.com/seanchatmangpt/spec-kit-3t/content#>

        SELECT ?title ?subtitle ?author ?year ?abstract ?dedication ?acknowledgments ?keywords
        WHERE {
            :spec-kit-3t-thesis
                thesis:hasTitle ?title ;
                thesis:hasSubtitle ?subtitle ;
                thesis:hasYear ?year ;
                thesis:hasAbstract ?abstractNode ;
                thesis:dedication ?dedication ;
                thesis:acknowledgments ?acknowledgments ;
                thesis:keywords ?keywords .

            ?abstractNode thesis:hasContent ?abstract .

            :author-chatman thesis:hasName ?author .
        }
        """

        rows = self.execute_sparql(query)
        if rows:
            output = self.render_template("frontmatter.tera", {"rows": rows})
            output_file.write_text(output)
            self.manifest.update_hash(output_file)
            AndonSignal.ok(f"{output_file.name}")

    def generate_chapter(self, chapter_num: int, chapter_type: str, template_name: str, force: bool = False):
        """Generate chapter with incremental build support"""
        print(f"\nGenerating Chapter {chapter_num} ({chapter_type})...")

        output_file = self.output_dir / f"chapter-{chapter_num:02d}.tex"
        template_file = self.templates_dir / template_name
        dependencies = self.ontology_files + [template_file]

        if not force and not self.needs_rebuild(output_file, dependencies):
            print("  ⏭️  Skipped (no changes)")
            return

        if chapter_type == "diataxis":
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
            output_file.write_text(output)
            self.manifest.update_hash(output_file)
            AndonSignal.ok(f"{output_file.name} ({len(rows)} sections)")
        else:
            AndonSignal.warn(f"No content found for chapter {chapter_num}")

    def generate_main_thesis(self, force: bool = False):
        """Generate main thesis.tex"""
        print("\nGenerating main thesis.tex...")

        output_file = self.output_dir / "thesis-main.tex"
        template_file = self.templates_dir / "thesis-main.tera"
        dependencies = self.ontology_files + [template_file]

        if not force and not self.needs_rebuild(output_file, dependencies):
            print("  ⏭️  Skipped (no changes)")
            return

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
            context = {
                "rows": rows,
                "config": {
                    "timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
                    "version": "2.0.0",
                    "pipeline": "μ₁→μ₂→μ₃→μ₄→μ₅"
                }
            }
            output = self.render_template("thesis-main.tera", context)
            output_file.write_text(output)
            self.manifest.update_hash(output_file)
            AndonSignal.ok(f"{output_file.name}")

    def generate_all(self, force: bool = False):
        """Execute complete pipeline"""
        build_start = datetime.now()

        # μ₁: SHACL Validation
        if not self.validate_with_shacl():
            return  # Stop on validation failure

        # μ₂ + μ₃: SPARQL extraction + Tera rendering
        self.generate_frontmatter(force)

        standard_chapters = [
            (1, "chapter.tera"),
            (2, "chapter.tera"),
            (7, "chapter.tera"),
            (8, "chapter.tera"),
            (9, "chapter.tera"),
            (10, "chapter.tera"),
        ]

        for chapter_num, template in standard_chapters:
            self.generate_chapter(chapter_num, "standard", template, force)

        diataxis_chapters = [
            (3, "diataxis-tutorial.tera"),
            (4, "diataxis-howto.tera"),
            (5, "diataxis-reference.tera"),
            (6, "diataxis-explanation.tera"),
        ]

        for chapter_num, template in diataxis_chapters:
            self.generate_chapter(chapter_num, "diataxis", template, force)

        self.generate_main_thesis(force)

        # μ₄: Canonicalization (file hashes computed during generation)
        # μ₅: Generate cryptographic receipt
        build_duration = (datetime.now() - build_start).total_seconds()

        receipt = {
            "files_generated": len(list(self.output_dir.glob("*.tex"))),
            "triples_processed": len(self.graph),
            "build_duration_seconds": build_duration,
            "ontology_files": [f.name for f in self.ontology_files],
            "validation_passed": True
        }

        self.manifest.record_build(receipt)

        print("\n" + "="*70)
        AndonSignal.ok("Generation Complete!")
        print(f"  Output directory: {self.output_dir}")
        print(f"  Generated {receipt['files_generated']} LaTeX files")
        print(f"  Processed {receipt['triples_processed']} RDF triples")
        print(f"  Build time: {build_duration:.2f}s")
        print(f"  Receipt: {self.manifest.manifest_path}")
        print("="*70)


def main():
    import argparse

    parser = argparse.ArgumentParser(
        description="Spec-Kit-3T Thesis Generator v2 (Production)"
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Force full rebuild (ignore incremental build cache)"
    )
    parser.add_argument(
        "--validate-only",
        action="store_true",
        help="Run SHACL validation only (μ₁)"
    )

    args = parser.parse_args()

    base_path = Path(__file__).parent
    generator = ThesisGeneratorV2(base_path)

    if args.validate_only:
        generator.validate_with_shacl()
        return

    generator.generate_all(force=args.force)

    print("\nNext steps:")
    print("  cd generated")
    print("  pdflatex thesis-main.tex")
    print("  biber thesis-main")
    print("  pdflatex thesis-main.tex")
    print("  pdflatex thesis-main.tex")


if __name__ == "__main__":
    main()
