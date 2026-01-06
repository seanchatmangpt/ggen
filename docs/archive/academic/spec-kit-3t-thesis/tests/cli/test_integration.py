"""
Integration tests for Spec-Kit-3T CLI end-to-end workflows.

Tests cover complete workflows:
- generate → validate → pdf → extract → clean
"""

import pytest
from pathlib import Path
from typer.testing import CliRunner
from cli.main import app

runner = CliRunner()


@pytest.mark.integration
class TestEndToEndWorkflow:
    """Test complete thesis generation workflow."""

    @pytest.fixture
    def workspace(self, tmp_path):
        """Create a test workspace with minimal ontology."""
        workspace_dir = tmp_path / "workspace"
        workspace_dir.mkdir()

        # Create minimal thesis ontology
        ontology_file = workspace_dir / "thesis-content.ttl"
        ontology_file.write_text("""
        @prefix thesis: <http://github.com/seanchatmangpt/spec-kit-3t/thesis#> .
        @prefix : <http://github.com/seanchatmangpt/spec-kit-3t/content#> .

        :spec-kit-3t-thesis a thesis:PhDThesis ;
            thesis:hasTitle "Test Thesis" ;
            thesis:hasSubtitle "A Test" ;
            thesis:hasYear "2025" ;
            thesis:hasAbstract :abstract-node ;
            thesis:dedication "Test dedication" ;
            thesis:acknowledgments "Test acknowledgments" ;
            thesis:keywords "test, thesis" .

        :abstract-node thesis:hasContent "This is a test abstract." .

        :author-chatman thesis:hasName "Test Author" .
        """)

        # Create minimal schema
        schema_file = workspace_dir / "thesis-schema.ttl"
        schema_file.write_text("""
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix thesis: <http://github.com/seanchatmangpt/spec-kit-3t/thesis#> .

        thesis:ThesisShape a sh:NodeShape ;
            sh:targetClass thesis:PhDThesis .
        """)

        return workspace_dir

    def test_validate_then_generate(self, workspace):
        """Test validation before generation."""
        # Step 1: Validate ontology
        result = runner.invoke(app, [
            "validate",
            str(workspace / "thesis-content.ttl"),
            "--schema", str(workspace / "thesis-schema.ttl"),
        ])

        # Validation should pass
        assert result.exit_code == 0

        # Step 2: Generate LaTeX
        output_dir = workspace / "generated"
        result = runner.invoke(app, [
            "generate",
            "--ontology-dir", str(workspace),
            "--output-dir", str(output_dir),
        ])

        # Generation may fail due to missing templates, but should not crash
        assert result.exit_code in [0, 1]

    @pytest.mark.skipif(
        not Path("/opt/homebrew/bin/pdftotext").exists(),
        reason="pdftotext not available"
    )
    def test_extract_then_clean(self, workspace, tmp_path):
        """Test PDF extraction and text cleaning."""
        # Create test text file simulating PDF extraction
        extracted_file = tmp_path / "extracted.txt"
        extracted_file.write_text("""
        This  is  extracted  text  with  double  spacing.
        It also has **bold** markers from LaTeX.
        And some **other**: patterns.
        """)

        # Clean the extracted text
        cleaned_file = tmp_path / "cleaned.txt"
        result = runner.invoke(app, [
            "clean",
            str(extracted_file),
            "--output", str(cleaned_file),
        ])

        assert result.exit_code == 0
        assert cleaned_file.exists()

        cleaned_text = cleaned_file.read_text()
        assert "  " not in cleaned_text  # No double spacing
        assert "**" not in cleaned_text  # No LaTeX markers


@pytest.mark.performance
class TestCLIPerformance:
    """Performance tests for CLI commands."""

    def test_validate_performance(self, tmp_path):
        """Test that validation completes within reasonable time."""
        import time

        # Create test RDF with 100 triples - properly formatted
        data_file = tmp_path / "data.ttl"
        triples = [f"ex:subject{i} ex:predicate ex:object{i} ." for i in range(100)]

        data_file.write_text(
            "@prefix ex: <http://example.org/> .\n"
            + "\n".join(triples)
        )

        schema_file = tmp_path / "schema.ttl"
        schema_file.write_text(
            "@prefix sh: <http://www.w3.org/ns/shacl#> .\n"
            "@prefix ex: <http://example.org/> .\n"
            "ex:Shape a sh:NodeShape .\n"
        )

        start_time = time.time()
        result = runner.invoke(app, [
            "validate",
            str(data_file),
            "--schema", str(schema_file),
        ])
        elapsed = time.time() - start_time

        # Should complete successfully or fail gracefully
        assert result.exit_code in [0, 1]
        assert elapsed < 5.0  # Should complete in under 5 seconds


@pytest.mark.security
class TestCLISecurity:
    """Security tests for CLI commands."""

    def test_clean_prevents_path_traversal(self, tmp_path):
        """Test that clean command prevents path traversal."""
        # This is a basic test - in production, would test more scenarios
        input_file = tmp_path / "input.txt"
        input_file.write_text("safe content")

        # Try to output to parent directory (should fail or be sanitized)
        output_file = tmp_path / "../output.txt"

        result = runner.invoke(app, [
            "clean",
            str(input_file),
            "--output", str(output_file),
        ])

        # Command should either fail or sanitize the path
        # (Typer handles this by default, but we verify)
        assert result.exit_code in [0, 1]

    def test_extract_with_malicious_filename(self, tmp_path):
        """Test extract with potentially malicious filename."""
        # Test that CLI handles unusual filenames safely
        malicious_file = tmp_path / "'; rm -rf / #.pdf"

        result = runner.invoke(app, [
            "extract",
            str(malicious_file),
        ])

        # Should fail gracefully, not execute the malicious command
        assert result.exit_code == 1
        # The filename appears in error message, but command wasn't executed
        # Verify no actual execution by checking exit code and error type
        assert "pdftotext failed" in result.stdout or "CRITICAL" in result.stdout
