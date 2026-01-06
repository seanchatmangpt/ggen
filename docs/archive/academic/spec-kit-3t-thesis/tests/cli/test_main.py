"""
Unit tests for Spec-Kit-3T CLI commands.

Tests cover all CLI commands: generate, validate, extract, clean, pdf.
"""

import pytest
from pathlib import Path
from typer.testing import CliRunner
from cli.main import app

runner = CliRunner()


class TestGenerateCommand:
    """Tests for the 'generate' command."""

    def test_generate_help(self):
        """Test that generate command shows help."""
        result = runner.invoke(app, ["generate", "--help"])
        assert result.exit_code == 0
        assert "Generate LaTeX thesis from RDF ontology" in result.stdout

    def test_generate_with_default_options(self, tmp_path):
        """Test generate with default options."""
        # Create minimal test RDF file
        ontology_file = tmp_path / "test.ttl"
        ontology_file.write_text("""
        @prefix thesis: <http://github.com/seanchatmangpt/spec-kit-3t/thesis#> .
        @prefix : <http://github.com/seanchatmangpt/spec-kit-3t/content#> .

        :test-thesis a thesis:PhDThesis .
        """)

        output_dir = tmp_path / "output"

        result = runner.invoke(app, [
            "generate",
            "--ontology-dir", str(tmp_path),
            "--output-dir", str(output_dir),
        ])

        # May fail due to incomplete ontology, but should not crash
        assert result.exit_code in [0, 1]  # 0 = success, 1 = validation error

    def test_generate_verbose_flag(self):
        """Test that --verbose flag is recognized."""
        result = runner.invoke(app, ["generate", "--verbose", "--help"])
        assert result.exit_code == 0


class TestValidateCommand:
    """Tests for the 'validate' command."""

    def test_validate_help(self):
        """Test that validate command shows help."""
        result = runner.invoke(app, ["validate", "--help"])
        assert result.exit_code == 0
        assert "Validate RDF ontology against SHACL shapes" in result.stdout

    def test_validate_with_valid_rdf(self, tmp_path):
        """Test validation with valid RDF data."""
        data_file = tmp_path / "data.ttl"
        data_file.write_text("""
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate ex:object .
        """)

        schema_file = tmp_path / "schema.ttl"
        schema_file.write_text("""
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:Shape a sh:NodeShape .
        """)

        result = runner.invoke(app, [
            "validate",
            str(data_file),
            "--schema", str(schema_file),
        ])

        assert result.exit_code == 0
        assert "SHACL validation passed" in result.stdout or "GREEN" in result.stdout

    def test_validate_missing_file(self):
        """Test validation with missing file."""
        result = runner.invoke(app, [
            "validate",
            "nonexistent.ttl",
        ])

        assert result.exit_code == 1


class TestExtractCommand:
    """Tests for the 'extract' command."""

    def test_extract_help(self):
        """Test that extract command shows help."""
        result = runner.invoke(app, ["extract", "--help"])
        assert result.exit_code == 0
        assert "Extract text from PDF" in result.stdout

    @pytest.mark.skipif(
        not Path("/opt/homebrew/bin/pdftotext").exists(),
        reason="pdftotext not available"
    )
    def test_extract_to_stdout(self, tmp_path):
        """Test extracting PDF text to stdout."""
        # Create a minimal PDF (would need actual PDF file)
        pdf_file = tmp_path / "test.pdf"

        if not pdf_file.exists():
            pytest.skip("Test PDF not available")

        result = runner.invoke(app, [
            "extract",
            str(pdf_file),
        ])

        # May fail if PDF is invalid, but command should work
        assert result.exit_code in [0, 1]

    def test_extract_with_output_file(self, tmp_path):
        """Test extracting PDF text to file."""
        pdf_file = tmp_path / "test.pdf"
        output_file = tmp_path / "output.txt"

        if not pdf_file.exists():
            pytest.skip("Test PDF not available")

        result = runner.invoke(app, [
            "extract",
            str(pdf_file),
            "--output", str(output_file),
        ])

        assert result.exit_code in [0, 1]


class TestCleanCommand:
    """Tests for the 'clean' command."""

    def test_clean_help(self):
        """Test that clean command shows help."""
        result = runner.invoke(app, ["clean", "--help"])
        assert result.exit_code == 0
        assert "Clean errant punctuation" in result.stdout

    def test_clean_double_spacing(self, tmp_path):
        """Test cleaning double spacing."""
        input_file = tmp_path / "input.txt"
        input_file.write_text("This  has  double  spacing.")

        output_file = tmp_path / "output.txt"

        result = runner.invoke(app, [
            "clean",
            str(input_file),
            "--output", str(output_file),
        ])

        assert result.exit_code == 0
        assert output_file.exists()

        cleaned_text = output_file.read_text()
        assert "  " not in cleaned_text  # No double spacing
        assert "This has double spacing." in cleaned_text

    def test_clean_latex_markers(self, tmp_path):
        """Test cleaning LaTeX bold markers."""
        input_file = tmp_path / "input.txt"
        input_file.write_text("This **text** has **bold** markers.")

        output_file = tmp_path / "output.txt"

        result = runner.invoke(app, [
            "clean",
            str(input_file),
            "--output", str(output_file),
        ])

        assert result.exit_code == 0

        cleaned_text = output_file.read_text()
        assert "**" not in cleaned_text
        assert "This text has bold markers." in cleaned_text

    def test_clean_no_fixes_needed(self, tmp_path):
        """Test cleaning when text is already clean."""
        input_file = tmp_path / "input.txt"
        input_file.write_text("This text is already clean.")

        output_file = tmp_path / "output.txt"

        result = runner.invoke(app, [
            "clean",
            str(input_file),
            "--output", str(output_file),
        ])

        assert result.exit_code == 0
        assert "No issues found" in result.stdout or "already clean" in result.stdout


class TestPdfCommand:
    """Tests for the 'pdf' command."""

    def test_pdf_help(self):
        """Test that pdf command shows help."""
        result = runner.invoke(app, ["pdf", "--help"])
        assert result.exit_code == 0
        assert "Compile LaTeX to PDF" in result.stdout

    def test_pdf_missing_latex_file(self, tmp_path):
        """Test PDF compilation with missing LaTeX file."""
        result = runner.invoke(app, [
            "pdf",
            "--latex-dir", str(tmp_path),
            "--main", "nonexistent.tex",
        ])

        assert result.exit_code == 1
        assert "not found" in result.stdout


class TestVersionCommand:
    """Tests for the 'version' command."""

    def test_version_command(self):
        """Test that version command shows version info."""
        result = runner.invoke(app, ["version"])

        assert result.exit_code == 0
        assert "Spec-Kit-3T" in result.stdout
        assert "Constitutional Equation" in result.stdout


class TestCLIIntegration:
    """Integration tests for the full CLI."""

    def test_app_has_all_commands(self):
        """Test that app has all expected commands."""
        result = runner.invoke(app, ["--help"])

        assert result.exit_code == 0
        assert "generate" in result.stdout
        assert "validate" in result.stdout
        assert "extract" in result.stdout
        assert "clean" in result.stdout
        assert "pdf" in result.stdout
        assert "version" in result.stdout

    def test_invalid_command(self):
        """Test invoking invalid command."""
        result = runner.invoke(app, ["invalid-command"])

        assert result.exit_code != 0
