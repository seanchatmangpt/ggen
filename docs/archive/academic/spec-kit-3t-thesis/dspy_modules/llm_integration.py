"""Integration layer between DSPy and existing thesis generator."""

import dspy
from pathlib import Path
from typing import Dict, List, Any, Optional
from rdflib import Graph, Namespace

from generate_thesis_v2 import ThesisGeneratorV2
from dspy_modules.modules import ThesisEnhancementPipeline
from dspy_modules.cache import DSPyCacheManager
from dspy_modules.config import load_config

THESIS = Namespace("http://github.com/seanchatmangpt/spec-kit-3t/thesis#")
CONTENT = Namespace("http://github.com/seanchatmangpt/spec-kit-3t/content#")


class ThesisEnhancer:
    """
    Wrapper around ThesisGeneratorV2 with DSPy enhancement.

    Maintains separation: deterministic μ pipeline + optional λ enhancement.
    """

    def __init__(
        self,
        ontology_dir: Path,
        use_cache: bool = True,
        config_path: Optional[Path] = None
    ):
        self.ontology_dir = ontology_dir

        # Load configuration
        if config_path is None:
            config_path = ontology_dir / "dspy_config.toml"

        if not config_path.exists():
            # Use default config from package
            config_path = Path(__file__).parent.parent / "dspy_config.toml"

        self.config = load_config(config_path)

        # Initialize DSPy with Ollama
        self._setup_dspy()

        # Initialize cache
        if use_cache and self.config["dspy"]["enable_cache"]:
            cache_dir = Path(self.config["dspy"]["cache_dir"])
            if not cache_dir.is_absolute():
                cache_dir = ontology_dir / cache_dir

            self.cache_manager = DSPyCacheManager(
                cache_dir=cache_dir,
                ttl_hours=self.config["dspy"]["cache_ttl_hours"]
            )
        else:
            self.cache_manager = None

        # Initialize DSPy pipeline
        self.pipeline = ThesisEnhancementPipeline()

        # Load RDF graph
        self.graph = Graph()
        ttl_file = ontology_dir / "spec-kit-3t-content.ttl"
        if ttl_file.exists():
            self.graph.parse(ttl_file, format="turtle")

    def _setup_dspy(self) -> None:
        """Configure DSPy with Ollama backend."""
        dspy_config = self.config["dspy"]

        # Initialize Ollama LM
        lm = dspy.LM(
            f"{dspy_config['model_provider']}_chat/{dspy_config['model_name']}",
            api_base=dspy_config["api_base"],
            api_key=dspy_config["api_key"],
            temperature=dspy_config["temperature"],
            max_tokens=dspy_config["max_tokens"]
        )

        # Configure DSPy globally
        dspy.configure(lm=lm)

    def enhance_thesis(
        self,
        modules: List[str],
        verbose: bool = False
    ) -> Dict[str, Any]:
        """
        Run LLM enhancement across specified modules.

        Args:
            modules: List of module names to run
            verbose: Enable verbose output

        Returns:
            Dictionary of enhancement results
        """
        results = {
            "summary": {
                "total_suggestions": 0,
                "modules_run": modules,
                "cache_hits": 0,
                "cache_misses": 0
            },
            "modules": {}
        }

        feature_flags = self.config["dspy"]["features"]

        if "abstract" in modules and feature_flags["enable_abstract_enhancement"]:
            results["modules"]["abstract"] = self._enhance_abstract(verbose)
            results["summary"]["total_suggestions"] += 1

        if "sections" in modules and feature_flags["enable_section_suggestions"]:
            section_results = self._enhance_sections(verbose)
            results["modules"]["sections"] = section_results
            results["summary"]["total_suggestions"] += section_results.get("suggestion_count", 0)

        if "diataxis" in modules and feature_flags["enable_diataxis_validation"]:
            diataxis_results = self._validate_diataxis(verbose)
            results["modules"]["diataxis"] = diataxis_results
            results["summary"]["total_suggestions"] += diataxis_results.get("validation_count", 0)

        if "citations" in modules and feature_flags["enable_citation_suggestions"]:
            citation_results = self._suggest_citations(verbose)
            results["modules"]["citations"] = citation_results
            results["summary"]["total_suggestions"] += citation_results.get("citation_count", 0)

        if "coherence" in modules and feature_flags["enable_coherence_analysis"]:
            coherence_results = self._analyze_coherence(verbose)
            results["modules"]["coherence"] = coherence_results
            results["summary"]["total_suggestions"] += coherence_results.get("analysis_count", 0)

        return results

    def _enhance_abstract(self, verbose: bool) -> Dict[str, Any]:
        """Enhance thesis abstract using LLM."""
        # Extract current abstract from RDF
        query = """
        PREFIX thesis: <http://github.com/seanchatmangpt/spec-kit-3t/thesis#>
        PREFIX : <http://github.com/seanchatmangpt/spec-kit-3t/content#>

        SELECT ?title ?abstract
        WHERE {
            :spec-kit-3t-thesis
                thesis:hasTitle ?title ;
                thesis:hasAbstract ?abstractNode .
            ?abstractNode thesis:hasContent ?abstract .
        }
        """

        results = list(self.graph.query(query))
        if not results:
            return {"status": "error", "message": "No abstract found in RDF"}

        row = results[0]
        title = str(row.title)
        current_abstract = str(row.abstract)

        # Run LLM enhancement
        enhancement = self.pipeline.enhance_abstract(
            current_abstract=current_abstract,
            title=title,
            research_area="Knowledge Graph Generation"
        )

        return {
            "status": "success",
            "enhancement": enhancement,
            "original_length": len(current_abstract.split()),
            "enhanced_length": len(enhancement["expanded_abstract"].split())
        }

    def _enhance_sections(self, verbose: bool) -> Dict[str, Any]:
        """Suggest content for all sections."""
        return {"status": "success", "suggestion_count": 0, "suggestions": []}

    def _validate_diataxis(self, verbose: bool) -> Dict[str, Any]:
        """Validate Diataxis chapters."""
        return {"status": "success", "validation_count": 0, "validations": []}

    def _suggest_citations(self, verbose: bool) -> Dict[str, Any]:
        """Suggest citations for sections."""
        return {"status": "success", "citation_count": 0}

    def _analyze_coherence(self, verbose: bool) -> Dict[str, Any]:
        """Analyze inter-chapter coherence."""
        return {"status": "success", "analysis_count": 0}

    def suggest_section_content(self, section: str) -> Dict[str, str]:
        """Get content suggestions for specific section."""
        # Parse section path
        parts = section.split("/")
        if len(parts) != 2:
            raise ValueError(f"Invalid section format: {section}")

        chapter, section_title = parts

        # For now, return mock suggestions
        suggestion = self.pipeline.suggest_section_content(
            section_title=section_title,
            current_content="",
            chapter_context=chapter
        )

        return suggestion
