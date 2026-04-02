"""
Performance benchmarks for Spec-Kit-3T CLI.

SLOs (Service Level Objectives):
- RDF validation: < 5s for 1000+ triples
- LaTeX generation: < 10s for 10 chapters
- PDF compilation: < 30s total
- Text extraction: < 2s for 40-page PDF
- Text cleaning: < 1s for 1000+ lines
"""

import pytest
import time
from pathlib import Path
from typer.testing import CliRunner
from cli.main import app

runner = CliRunner()


@pytest.fixture
def large_ontology(tmp_path):
    """Create large RDF ontology for benchmarking."""
    ontology = tmp_path / "large.ttl"

    # Generate 1000+ triples
    triples = []
    triples.append("@prefix ex: <http://example.org/> .")
    triples.append("@prefix thesis: <http://github.com/seanchatmangpt/spec-kit-3t/thesis#> .")

    for i in range(1000):
        triples.append(f"ex:entity{i} ex:property ex:value{i} .")

    ontology.write_text("\n".join(triples))
    return ontology


@pytest.fixture
def large_text_file(tmp_path):
    """Create large text file for cleaning benchmarks."""
    text_file = tmp_path / "large.txt"

    # 5000 lines with various issues
    lines = []
    for i in range(5000):
        lines.append(f"Line {i} with  double  spacing and **bold** markers.")

    text_file.write_text("\n".join(lines))
    return text_file


class TestValidationPerformance:
    """Benchmark SHACL validation performance."""

    def test_validation_slo(self, large_ontology, tmp_path):
        """Validate that SHACL validation meets SLO (< 5s for 1000+ triples)."""
        schema = tmp_path / "schema.ttl"
        schema.write_text("""
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:EntityShape a sh:NodeShape ;
            sh:targetClass ex:Entity .
        """)

        start_time = time.time()
        result = runner.invoke(app, [
            "validate",
            str(large_ontology),
            "--schema", str(schema),
        ])
        elapsed = time.time() - start_time

        assert elapsed < 5.0, f"Validation took {elapsed:.2f}s (SLO: 5s)"
        assert result.exit_code == 0

    def test_validation_incremental(self, large_ontology, tmp_path):
        """Test incremental validation performance."""
        schema = tmp_path / "schema.ttl"
        schema.write_text("@prefix sh: <http://www.w3.org/ns/shacl#> .")

        # First run
        start1 = time.time()
        runner.invoke(app, ["validate", str(large_ontology), "--schema", str(schema)])
        time1 = time.time() - start1

        # Second run (should be similar, no caching currently)
        start2 = time.time()
        runner.invoke(app, ["validate", str(large_ontology), "--schema", str(schema)])
        time2 = time.time() - start2

        # Both should be under 5s
        assert time1 < 5.0
        assert time2 < 5.0


class TestGenerationPerformance:
    """Benchmark LaTeX generation performance."""

    def test_generation_cold_start(self, tmp_path):
        """Test cold start generation performance."""
        # Create minimal ontology
        ontology = tmp_path / "thesis.ttl"
        ontology.write_text("""
        @prefix thesis: <http://github.com/seanchatmangpt/spec-kit-3t/thesis#> .
        @prefix : <http://github.com/seanchatmangpt/spec-kit-3t/content#> .

        :test a thesis:PhDThesis ;
            thesis:hasTitle "Benchmark Test" ;
            thesis:hasSubtitle "Performance" ;
            thesis:hasYear "2025" .
        """)

        output_dir = tmp_path / "output"

        start_time = time.time()
        result = runner.invoke(app, [
            "generate",
            "--ontology-dir", str(tmp_path),
            "--output-dir", str(output_dir),
            "--force",
        ])
        elapsed = time.time() - start_time

        # Cold start should complete within 15s (SLO)
        assert elapsed < 15.0, f"Generation took {elapsed:.2f}s (SLO: 15s)"


class TestCleaningPerformance:
    """Benchmark text cleaning performance."""

    def test_cleaning_slo(self, large_text_file, tmp_path):
        """Validate that text cleaning meets SLO (< 1s for 1000+ lines)."""
        output_file = tmp_path / "cleaned.txt"

        start_time = time.time()
        result = runner.invoke(app, [
            "clean",
            str(large_text_file),
            "--output", str(output_file),
        ])
        elapsed = time.time() - start_time

        assert elapsed < 1.0, f"Cleaning took {elapsed:.2f}s (SLO: 1s)"
        assert result.exit_code == 0

    def test_cleaning_throughput(self, large_text_file, tmp_path):
        """Measure cleaning throughput (lines/second)."""
        output_file = tmp_path / "cleaned.txt"

        line_count = len(large_text_file.read_text().splitlines())

        start_time = time.time()
        runner.invoke(app, [
            "clean",
            str(large_text_file),
            "--output", str(output_file),
        ])
        elapsed = time.time() - start_time

        throughput = line_count / elapsed

        # Should process at least 5000 lines/second
        assert throughput > 5000, f"Throughput: {throughput:.0f} lines/s (expected: >5000)"


class TestMemoryUsage:
    """Test memory efficiency."""

    def test_large_file_memory(self, large_text_file, tmp_path):
        """Test that large file processing doesn't exhaust memory."""
        import tracemalloc

        tracemalloc.start()

        output_file = tmp_path / "cleaned.txt"
        runner.invoke(app, [
            "clean",
            str(large_text_file),
            "--output", str(output_file),
        ])

        current, peak = tracemalloc.get_traced_memory()
        tracemalloc.stop()

        # Peak memory should be under 100MB for text processing
        peak_mb = peak / (1024 * 1024)
        assert peak_mb < 100, f"Peak memory: {peak_mb:.1f}MB (expected: <100MB)"


class TestConcurrency:
    """Test concurrent operation performance."""

    def test_parallel_validation(self, tmp_path):
        """Test running multiple validations sequentially (CLI is stateful)."""
        # Note: Typer CLI runner is not thread-safe, so we test sequential performance

        # Create 5 different ontologies
        files = []
        for i in range(5):
            f = tmp_path / f"test{i}.ttl"
            f.write_text(
                f"@prefix ex: <http://example.org/> .\n"
                f"ex:subject{i} ex:predicate ex:object{i} .\n"
            )
            files.append(f)

        schema = tmp_path / "schema.ttl"
        schema.write_text(
            "@prefix sh: <http://www.w3.org/ns/shacl#> .\n"
            "@prefix ex: <http://example.org/> .\n"
            "ex:Shape a sh:NodeShape .\n"
        )

        # Run sequentially
        start_time = time.time()
        results = []
        for f in files:
            result = runner.invoke(app, ["validate", str(f), "--schema", str(schema)])
            results.append(result)
        elapsed = time.time() - start_time

        # 5 files should validate in < 5s total
        assert elapsed < 5.0, f"Sequential validation took {elapsed:.2f}s"
        assert all(r.exit_code == 0 for r in results)


class TestRegressionPrevention:
    """Prevent performance regressions."""

    def test_validation_baseline(self, tmp_path):
        """Establish validation performance baseline."""
        # 100 triples baseline
        data = tmp_path / "baseline.ttl"
        triples = ["@prefix ex: <http://example.org/> ."]
        triples.extend([f"ex:s{i} ex:p ex:o{i} ." for i in range(100)])
        data.write_text("\n".join(triples))

        schema = tmp_path / "schema.ttl"
        schema.write_text("@prefix sh: <http://www.w3.org/ns/shacl#> .")

        # Run 10 times and get average
        times = []
        for _ in range(10):
            start = time.time()
            runner.invoke(app, ["validate", str(data), "--schema", str(schema)])
            times.append(time.time() - start)

        avg_time = sum(times) / len(times)

        # Baseline: 100 triples should validate in < 0.5s on average
        assert avg_time < 0.5, f"Baseline validation: {avg_time:.3f}s (expected: <0.5s)"

    def test_cleaning_baseline(self, tmp_path):
        """Establish cleaning performance baseline."""
        # 1000 lines baseline
        data = tmp_path / "baseline.txt"
        lines = [f"Line {i} with  spacing and **bold**." for i in range(1000)]
        data.write_text("\n".join(lines))

        output = tmp_path / "output.txt"

        # Run 10 times and get average
        times = []
        for _ in range(10):
            start = time.time()
            runner.invoke(app, ["clean", str(data), "--output", str(output)])
            times.append(time.time() - start)

        avg_time = sum(times) / len(times)

        # Baseline: 1000 lines should clean in < 0.1s on average
        assert avg_time < 0.1, f"Baseline cleaning: {avg_time:.3f}s (expected: <0.1s)"
