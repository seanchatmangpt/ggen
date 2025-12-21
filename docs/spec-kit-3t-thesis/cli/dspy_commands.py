"""DSPy-enhanced CLI commands."""

import typer
from pathlib import Path
from typing import Optional
from rich.console import Console
from rich.table import Table
from rich.panel import Panel
import json

app = typer.Typer(
    name="dspy",
    help="DSPy LLM enhancement commands"
)

console = Console()


@app.command()
def enhance(
    ontology_dir: Path = typer.Option(
        Path("."),
        "--ontology-dir",
        "-o",
        help="Directory containing RDF ontology"
    ),
    output_file: Path = typer.Option(
        Path("llm_suggestions.json"),
        "--output",
        "-out",
        help="Output file for LLM suggestions"
    ),
    modules: Optional[str] = typer.Option(
        "all",
        "--modules",
        "-m",
        help="Comma-separated modules: abstract,sections,diataxis,citations,coherence"
    ),
    use_cache: bool = typer.Option(
        True,
        "--cache/--no-cache",
        help="Use LLM output cache"
    ),
    verbose: bool = typer.Option(
        False,
        "--verbose",
        "-v",
        help="Verbose output"
    ),
) -> None:
    """
    Enhance thesis using DSPy LLM modules.

    This command runs LLM enhancement over the existing RDF ontology,
    generating suggestions without modifying the deterministic pipeline.

    Examples:
        # Enhance entire thesis
        spec-kit-3t dspy enhance

        # Only enhance abstract
        spec-kit-3t dspy enhance --modules abstract

        # Skip cache for fresh suggestions
        spec-kit-3t dspy enhance --no-cache
    """
    console.print("\n[bold cyan]DSPy LLM Enhancement Pipeline[/bold cyan]\n")

    try:
        # Import DSPy modules (lazy load)
        import sys
        parent_dir = Path(__file__).parent.parent
        if str(parent_dir) not in sys.path:
            sys.path.insert(0, str(parent_dir))

        from dspy_modules.llm_integration import ThesisEnhancer

        # Initialize enhancer
        enhancer = ThesisEnhancer(
            ontology_dir=ontology_dir,
            use_cache=use_cache
        )

        # Parse module list
        if modules == "all":
            module_list = ["abstract", "sections", "diataxis", "citations", "coherence"]
        else:
            module_list = [m.strip() for m in modules.split(",")]

        console.print(f"[bold]Active modules:[/bold] {', '.join(module_list)}")
        console.print(f"[bold]Cache enabled:[/bold] {use_cache}\n")

        # Run enhancement
        results = enhancer.enhance_thesis(
            modules=module_list,
            verbose=verbose
        )

        # Save results
        with open(output_file, 'w') as f:
            json.dump(results, f, indent=2)

        # Display summary
        _display_enhancement_summary(results)

        console.print(f"\n[green]✅ Enhancement complete![/green]")
        console.print(f"  Results saved to: {output_file}")
        console.print(f"  Total suggestions: {results['summary']['total_suggestions']}")

    except Exception as e:
        console.print(f"\n[red]❌ Enhancement failed: {str(e)}[/red]")
        if verbose:
            import traceback
            console.print(f"\n[dim]{traceback.format_exc()}[/dim]")
        raise typer.Exit(code=1)


@app.command()
def suggest(
    section: str = typer.Argument(
        ...,
        help="Section path in format: chapter-N/section-title"
    ),
    ontology_dir: Path = typer.Option(
        Path("."),
        "--ontology-dir",
        "-o",
        help="Directory containing RDF ontology"
    ),
) -> None:
    """
    Get content suggestions for specific section.

    Examples:
        spec-kit-3t dspy suggest "chapter-3/Introduction"
    """
    console.print(f"\n[bold]Generating suggestions for:[/bold] {section}\n")

    try:
        import sys
        parent_dir = Path(__file__).parent.parent
        if str(parent_dir) not in sys.path:
            sys.path.insert(0, str(parent_dir))

        from dspy_modules.llm_integration import ThesisEnhancer

        enhancer = ThesisEnhancer(ontology_dir=ontology_dir)
        suggestions = enhancer.suggest_section_content(section)

        # Display suggestions
        panel = Panel(
            f"[bold]Content Suggestions:[/bold]\n\n{suggestions['suggestions']}\n\n"
            f"[bold]Related Concepts:[/bold]\n{suggestions['related_concepts']}",
            title=f"Section: {section}",
            border_style="blue"
        )
        console.print(panel)

    except Exception as e:
        console.print(f"\n[red]❌ Failed: {str(e)}[/red]")
        raise typer.Exit(code=1)


@app.command(name="cache-stats")
def cache_stats() -> None:
    """Show DSPy cache statistics."""
    try:
        import sys
        parent_dir = Path(__file__).parent.parent
        if str(parent_dir) not in sys.path:
            sys.path.insert(0, str(parent_dir))

        from dspy_modules.cache import DSPyCacheManager
        from dspy_modules.config import load_config

        config = load_config()
        cache_dir = Path(config["dspy"]["cache_dir"])
        if not cache_dir.is_absolute():
            cache_dir = Path.cwd() / cache_dir

        cache_manager = DSPyCacheManager(
            cache_dir=cache_dir,
            ttl_hours=config["dspy"]["cache_ttl_hours"]
        )

        stats = cache_manager.stats()

        table = Table(title="DSPy Cache Statistics")
        table.add_column("Metric", style="cyan")
        table.add_column("Value", style="green")

        table.add_row("Total Entries", str(stats["total_entries"]))
        table.add_row("Cache Size (MB)", str(stats["cache_size_mb"]))
        table.add_row("Cache Directory", stats["cache_dir"])
        table.add_row("TTL (hours)", str(stats["ttl_hours"]))

        console.print(table)

    except Exception as e:
        console.print(f"[red]❌ Failed to load cache stats: {str(e)}[/red]")
        raise typer.Exit(code=1)


@app.command(name="cache-clear")
def cache_clear(
    confirm: bool = typer.Option(
        False,
        "--yes",
        "-y",
        help="Skip confirmation prompt"
    ),
) -> None:
    """Clear DSPy LLM output cache."""
    if not confirm:
        confirmed = typer.confirm(
            "Are you sure you want to clear the entire cache?"
        )
        if not confirmed:
            console.print("[yellow]Cache clear cancelled[/yellow]")
            raise typer.Exit(code=0)

    try:
        import sys
        parent_dir = Path(__file__).parent.parent
        if str(parent_dir) not in sys.path:
            sys.path.insert(0, str(parent_dir))

        from dspy_modules.cache import DSPyCacheManager
        from dspy_modules.config import load_config

        config = load_config()
        cache_dir = Path(config["dspy"]["cache_dir"])
        if not cache_dir.is_absolute():
            cache_dir = Path.cwd() / cache_dir

        cache_manager = DSPyCacheManager(cache_dir=cache_dir)

        count = cache_manager.invalidate_all()
        console.print(f"[green]✅ Cleared {count} cache entries[/green]")

    except Exception as e:
        console.print(f"[red]❌ Failed: {str(e)}[/red]")
        raise typer.Exit(code=1)


def _display_enhancement_summary(results: dict) -> None:
    """Display enhancement results summary."""
    table = Table(title="Enhancement Summary")
    table.add_column("Module", style="cyan")
    table.add_column("Suggestions", style="green")
    table.add_column("Status", style="yellow")

    for module, data in results.get("modules", {}).items():
        table.add_row(
            module,
            str(data.get("suggestion_count", 0)),
            data.get("status", "unknown")
        )

    console.print(table)
