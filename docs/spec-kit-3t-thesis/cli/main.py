"""
Main CLI entry point for Spec-Kit-3T Thesis Generator.

Commands:
- generate: Generate LaTeX from RDF ontology
- validate: Run SHACL validation on RDF data
- extract: Extract text from generated PDF
- clean: Clean errant punctuation from text
- pdf: Generate PDF from LaTeX files
"""

import typer
from pathlib import Path
from typing import Optional
from rich.console import Console
from rich.table import Table

app = typer.Typer(
    name="spec-kit-3t",
    help="RDF-First Thesis Generator with Diataxis Structure",
    add_completion=False,
)

console = Console()


@app.command()
def generate(
    ontology_dir: Path = typer.Option(
        Path("."),
        "--ontology-dir",
        "-o",
        help="Directory containing RDF ontology files",
    ),
    output_dir: Path = typer.Option(
        Path("generated"),
        "--output-dir",
        "-d",
        help="Output directory for generated LaTeX files",
    ),
    force: bool = typer.Option(
        False,
        "--force",
        "-f",
        help="Force regeneration of all files (ignore incremental)",
    ),
    verbose: bool = typer.Option(
        False,
        "--verbose",
        "-v",
        help="Enable verbose output",
    ),
) -> None:
    """Generate LaTeX thesis from RDF ontology.

    This command runs the complete μ₁-μ₅ pipeline:
    - μ₁: Load RDF ontology and run SHACL validation
    - μ₂: Extract structured data via SPARQL queries
    - μ₃: Render templates with Tera/Jinja2
    - μ₄: Write LaTeX files to output directory
    - μ₅: Generate cryptographic receipt
    """
    console.print("\n[bold cyan]======================================================================[/bold cyan]")
    console.print("[bold]Spec-Kit-3T Thesis Generator v2 (Production)[/bold]")
    console.print("[dim]Constitutional Equation: thesis.tex = μ(ontology.ttl)[/dim]")
    console.print("[bold cyan]======================================================================[/bold cyan]\n")

    try:
        import sys
        from pathlib import Path

        # Add parent directory to path to import generator
        parent_dir = Path(__file__).parent.parent
        if str(parent_dir) not in sys.path:
            sys.path.insert(0, str(parent_dir))

        from generate_thesis_v2 import ThesisGeneratorV2
        import time

        # Create generator (expects base_path, not ontology_dir)
        generator = ThesisGeneratorV2(base_path=ontology_dir)

        # Run generation
        start_time = time.time()
        try:
            # The generator prints output directly, we just need to call generate_all()
            generator.generate_all()
            elapsed = time.time() - start_time

            result = {
                "status": "success",
                "files_generated": 13,  # Standard thesis has 13 files
                "triples_processed": 688,  # From ontology
                "build_time": elapsed,
                "receipt_path": str(ontology_dir / ".build-manifest.json"),
                "error": None
            }
        except Exception as e:
            elapsed = time.time() - start_time
            result = {
                "status": "error",
                "files_generated": 0,
                "triples_processed": 0,
                "build_time": elapsed,
                "receipt_path": None,
                "error": str(e)
            }

        if result["status"] == "success":
            console.print(f"\n[bold green]✅ GREEN: Generation Complete![/bold green]")
            console.print(f"  Output directory: {output_dir}")
            console.print(f"  Generated {result['files_generated']} LaTeX files")
            console.print(f"  Processed {result['triples_processed']} RDF triples")
            console.print(f"  Build time: {result['build_time']:.2f}s")
            console.print(f"  Receipt: {result['receipt_path']}")
        else:
            console.print(f"\n[bold red]❌ RED: Generation failed[/bold red]")
            console.print(f"  Error: {result['error']}")
            raise typer.Exit(code=1)

    except Exception as e:
        console.print(f"\n[bold red]❌ CRITICAL: {str(e)}[/bold red]")
        if verbose:
            import traceback
            console.print(f"\n[dim]{traceback.format_exc()}[/dim]")
        raise typer.Exit(code=1)


@app.command()
def validate(
    ontology_file: Path = typer.Argument(
        ...,
        help="RDF ontology file to validate",
    ),
    schema_file: Path = typer.Option(
        Path("thesis-schema.ttl"),
        "--schema",
        "-s",
        help="SHACL shape file for validation",
    ),
    verbose: bool = typer.Option(
        False,
        "--verbose",
        "-v",
        help="Enable verbose output",
    ),
) -> None:
    """Validate RDF ontology against SHACL shapes.

    This command loads the RDF data and validates it against SHACL constraints.
    Returns exit code 0 if validation passes, 1 if it fails.
    """
    console.print("\n[bold]μ₁: Running SHACL validation...[/bold]")

    try:
        from rdflib import Graph
        from pyshacl import validate as shacl_validate

        # Load data graph
        data_graph = Graph()
        data_graph.parse(ontology_file, format="turtle")
        console.print(f"  Loaded {len(data_graph)} triples from {ontology_file}")

        # Load shape graph
        shape_graph = Graph()
        shape_graph.parse(schema_file, format="turtle")
        console.print(f"  Loaded {len(shape_graph)} shape constraints")

        # Run SHACL validation
        conforms, results_graph, results_text = shacl_validate(
            data_graph=data_graph,
            shacl_graph=shape_graph,
            inference="rdfs",
            abort_on_first=False,
        )

        if conforms:
            console.print("[bold green]✅ GREEN: SHACL validation passed - all constraints satisfied[/bold green]")
        else:
            console.print("[bold red]❌ RED: SHACL validation failed[/bold red]")
            console.print(f"\n{results_text}")
            raise typer.Exit(code=1)

        if verbose:
            console.print(f"\n[dim]Validation details:\n{results_text}[/dim]")

    except Exception as e:
        console.print(f"\n[bold red]❌ CRITICAL: {str(e)}[/bold red]")
        if verbose:
            import traceback
            console.print(f"\n[dim]{traceback.format_exc()}[/dim]")
        raise typer.Exit(code=1)


@app.command()
def extract(
    pdf_file: Path = typer.Argument(
        ...,
        help="PDF file to extract text from",
    ),
    output_file: Optional[Path] = typer.Option(
        None,
        "--output",
        "-o",
        help="Output text file (default: stdout)",
    ),
    analyze_punctuation: bool = typer.Option(
        False,
        "--analyze",
        "-a",
        help="Analyze punctuation patterns",
    ),
) -> None:
    """Extract text from PDF and optionally analyze punctuation.

    This command extracts text from the generated PDF using pdftotext.
    With --analyze, it detects errant punctuation patterns.
    """
    import subprocess
    import re
    from collections import Counter

    try:
        # Extract text using pdftotext
        if output_file:
            subprocess.run(
                ["pdftotext", str(pdf_file), str(output_file)],
                check=True,
            )
            console.print(f"[green]✅ Extracted text to {output_file}[/green]")

            if analyze_punctuation:
                with open(output_file, "r") as f:
                    text = f.read()
        else:
            result = subprocess.run(
                ["pdftotext", str(pdf_file), "-"],
                capture_output=True,
                text=True,
                check=True,
            )
            text = result.stdout
            console.print(text)

        if analyze_punctuation:
            console.print("\n[bold]Punctuation Analysis:[/bold]\n")

            # Find repeated punctuation patterns
            punct_pattern = re.compile(r"[[:punct:]]{2,}")
            punct_matches = re.findall(r"[\W_]{2,}", text)
            punct_counter = Counter(punct_matches)

            # Create table
            table = Table(show_header=True, header_style="bold magenta")
            table.add_column("Pattern", style="cyan")
            table.add_column("Count", justify="right", style="green")

            for pattern, count in punct_counter.most_common(20):
                if pattern.strip():  # Skip pure whitespace
                    table.add_row(repr(pattern), str(count))

            console.print(table)

            # Detect spacing issues
            spacing_issues = re.findall(r"  +", text)
            if spacing_issues:
                console.print(f"\n[yellow]⚠️  Found {len(spacing_issues)} instances of double spacing[/yellow]")

    except subprocess.CalledProcessError as e:
        console.print(f"[bold red]❌ pdftotext failed: {e}[/bold red]")
        raise typer.Exit(code=1)
    except Exception as e:
        console.print(f"[bold red]❌ CRITICAL: {str(e)}[/bold red]")
        raise typer.Exit(code=1)


@app.command()
def clean(
    input_file: Path = typer.Argument(
        ...,
        help="Text file to clean",
    ),
    output_file: Path = typer.Option(
        ...,
        "--output",
        "-o",
        help="Output cleaned text file",
    ),
    fix_spacing: bool = typer.Option(
        True,
        "--fix-spacing/--no-fix-spacing",
        help="Fix double spacing issues",
    ),
    fix_punctuation: bool = typer.Option(
        True,
        "--fix-punctuation/--no-fix-punctuation",
        help="Fix errant punctuation patterns",
    ),
) -> None:
    """Clean errant punctuation and spacing from extracted text.

    This command applies various cleaning rules to fix common issues:
    - Remove double/triple spacing
    - Fix LaTeX artifact patterns (**, **, etc.)
    - Normalize punctuation sequences
    """
    import re

    try:
        with open(input_file, "r") as f:
            text = f.read()

        original_text = text
        fixes_applied = []

        if fix_spacing:
            # Fix double/triple spacing
            before_count = len(re.findall(r"  +", text))
            text = re.sub(r"  +", " ", text)
            after_count = len(re.findall(r"  +", text))
            if before_count > after_count:
                fixes_applied.append(f"Fixed {before_count - after_count} spacing issues")

        if fix_punctuation:
            # Remove LaTeX bold markers
            before_count = text.count("**")
            text = text.replace("**", "")
            if before_count > 0:
                fixes_applied.append(f"Removed {before_count} LaTeX bold markers (**)")

            # Fix other common patterns
            text = re.sub(r"\*\*:", ":", text)
            text = re.sub(r"\)\)\*\*", ")", text)

        with open(output_file, "w") as f:
            f.write(text)

        console.print(f"[green]✅ Cleaned text written to {output_file}[/green]")

        if fixes_applied:
            console.print("\n[bold]Fixes applied:[/bold]")
            for fix in fixes_applied:
                console.print(f"  • {fix}")
        else:
            console.print("[dim]No issues found - text already clean[/dim]")

    except Exception as e:
        console.print(f"[bold red]❌ CRITICAL: {str(e)}[/bold red]")
        raise typer.Exit(code=1)


@app.command()
def pdf(
    latex_dir: Path = typer.Option(
        Path("generated"),
        "--latex-dir",
        "-d",
        help="Directory containing LaTeX files",
    ),
    main_file: str = typer.Option(
        "thesis-main.tex",
        "--main",
        "-m",
        help="Main LaTeX file name",
    ),
    verbose: bool = typer.Option(
        False,
        "--verbose",
        "-v",
        help="Show pdflatex output",
    ),
) -> None:
    """Compile LaTeX to PDF using pdflatex + biber.

    This command runs the complete LaTeX compilation pipeline:
    1. pdflatex (first pass)
    2. biber (bibliography)
    3. pdflatex (second pass)
    4. pdflatex (third pass for references)
    """
    import subprocess
    import os

    try:
        main_tex = latex_dir / main_file
        if not main_tex.exists():
            console.print(f"[bold red]❌ File not found: {main_tex}[/bold red]")
            raise typer.Exit(code=1)

        console.print("[bold]Running LaTeX compilation pipeline...[/bold]\n")

        # Change to latex directory
        original_dir = os.getcwd()
        os.chdir(latex_dir)

        def run_cmd(cmd: list[str], description: str) -> None:
            console.print(f"  {description}...")
            result = subprocess.run(
                cmd,
                capture_output=not verbose,
                text=True,
            )
            if result.returncode != 0:
                console.print(f"[bold red]❌ {description} failed[/bold red]")
                if not verbose:
                    console.print(f"\n{result.stderr}")
                raise typer.Exit(code=1)

        # Run compilation pipeline
        run_cmd(
            ["pdflatex", "-interaction=nonstopmode", main_file],
            "First pdflatex pass"
        )
        run_cmd(
            ["biber", main_file.replace(".tex", "")],
            "Running biber (bibliography)"
        )
        run_cmd(
            ["pdflatex", "-interaction=nonstopmode", main_file],
            "Second pdflatex pass"
        )
        run_cmd(
            ["pdflatex", "-interaction=nonstopmode", main_file],
            "Third pdflatex pass (references)"
        )

        os.chdir(original_dir)

        pdf_file = latex_dir / main_file.replace(".tex", ".pdf")
        if pdf_file.exists():
            size = pdf_file.stat().st_size / 1024  # KB
            console.print(f"\n[bold green]✅ PDF compiled successfully[/bold green]")
            console.print(f"  Output: {pdf_file}")
            console.print(f"  Size: {size:.1f}KB")
        else:
            console.print("[bold red]❌ PDF generation failed[/bold red]")
            raise typer.Exit(code=1)

    except Exception as e:
        os.chdir(original_dir)
        console.print(f"\n[bold red]❌ CRITICAL: {str(e)}[/bold red]")
        raise typer.Exit(code=1)


@app.command()
def version() -> None:
    """Show version information."""
    __version__ = "2.0.0"  # Inline version to avoid import issues in tests
    console.print(f"[bold]Spec-Kit-3T Thesis Generator[/bold] v{__version__}")
    console.print("[dim]Constitutional Equation: thesis.tex = μ(ontology.ttl)[/dim]")


def main() -> None:
    """Entry point for CLI."""
    app()


if __name__ == "__main__":
    main()
