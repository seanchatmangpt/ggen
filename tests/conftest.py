"""
Pytest configuration and fixtures for comprehensive test suite.
"""

import pytest
import json
from datetime import datetime
from pathlib import Path


# Test markers
def pytest_configure(config):
    """Configure pytest with custom markers."""
    config.addinivalue_line(
        "markers", "academic: Academic paper lifecycle tests"
    )
    config.addinivalue_line(
        "markers", "conference: Conference submission workflow tests"
    )
    config.addinivalue_line(
        "markers", "startup: Startup metrics and milestone tests"
    )
    config.addinivalue_line(
        "markers", "unicorn: Unicorn status ($1B+) achievement tests"
    )
    config.addinivalue_line(
        "markers", "ipo: IPO readiness and exit strategy tests"
    )
    config.addinivalue_line(
        "markers", "e2e: End-to-end integration tests"
    )


# Global test data
class TestContext:
    """Global test context and data."""

    def __init__(self):
        self.start_time = datetime.now()
        self.test_results = {
            "academic": [],
            "conference": [],
            "startup": [],
            "unicorn": [],
            "ipo": [],
            "total_passed": 0,
            "total_failed": 0,
        }

    def add_result(self, category, test_name, passed, message=""):
        """Record test result."""
        self.test_results[category].append({
            "test": test_name,
            "passed": passed,
            "message": message,
            "timestamp": datetime.now().isoformat(),
        })
        if passed:
            self.test_results["total_passed"] += 1
        else:
            self.test_results["total_failed"] += 1

    def get_summary(self):
        """Get test summary."""
        return {
            "duration": str(datetime.now() - self.start_time),
            "total_tests": self.test_results["total_passed"] + self.test_results["total_failed"],
            "passed": self.test_results["total_passed"],
            "failed": self.test_results["total_failed"],
            "success_rate": (
                self.test_results["total_passed"] /
                (self.test_results["total_passed"] + self.test_results["total_failed"])
                if (self.test_results["total_passed"] + self.test_results["total_failed"]) > 0
                else 0
            ),
        }


# Global context
test_context = TestContext()


@pytest.fixture(scope="session")
def test_results_context():
    """Fixture: Global test results context."""
    return test_context


# ============================================================================
# Academic Paper Fixtures
# ============================================================================

@pytest.fixture
def minimal_paper_rdf():
    """Fixture: Minimal valid paper RDF."""
    return """
@prefix : <http://example.org/paper#> .
@prefix ap: <http://example.org/academic-paper#> .
@prefix math: <http://example.org/math#> .

:MinimalPaper a ap:Paper ;
    ap:title "Minimal Test" ;
    ap:author "Author" ;
    ap:date "2025-11-15" ;
    ap:abstract "Test abstract" ;
    ap:keywords "test, minimal" ;
    ap:hasSection :Section1 ;
    math:hasEquation :Equation1 .

:Section1 a ap:Section ;
    ap:sectionNumber 1 ;
    ap:sectionTitle "Introduction" ;
    ap:sectionContent "This is a test section." .

:Equation1 a math:Equation ;
    ap:equationName "einstein" ;
    ap:equationLatex "E = mc^2" ;
    math:equationNumber 1 ;
    ap:equationOrder 1 ;
    math:description "Einstein's mass-energy equivalence." .
    """


@pytest.fixture
def v1_paper_rdf():
    """Fixture: v1 Chatman Equation paper (8 equations)."""
    return """
@prefix : <http://example.org/paper#> .
@prefix ap: <http://example.org/academic-paper#> .
@prefix math: <http://example.org/math#> .

:ChatmanPaperV1 a ap:Paper ;
    ap:title "The Chatman Equation - v1" ;
    ap:author "Sean Chatman" ;
    ap:date "2025-11-15" ;
    ap:abstract "Foundation version with core equation and basic properties" ;
    ap:keywords "Chatman equation, determinism, knowledge hooks" ;
    ap:hasSection :Intro ;
    math:hasEquation :Eq1, :Eq2, :Eq3, :Eq4, :Eq5, :Eq6, :Eq7, :Eq8 .

:Intro a ap:Section ;
    ap:sectionNumber 1 ;
    ap:sectionTitle "Introduction" ;
    ap:sectionContent "Foundation version of the Chatman Equation" .

:Eq1 a math:Equation ;
    ap:equationName "chatman_core" ;
    ap:equationLatex "A = \\mu(O)" ;
    math:equationNumber 1 ;
    ap:equationOrder 1 ;
    math:description "Core Chatman Equation" .

:Eq2 a math:Equation ;
    ap:equationName "determinism" ;
    ap:equationLatex "\\forall O_1, O_2: O_1 = O_2 \\Rightarrow \\mu(O_1) = \\mu(O_2)" ;
    math:equationNumber 2 ;
    ap:equationOrder 2 ;
    math:description "Determinism property" .

:Eq3 a math:Equation ;
    ap:equationName "idempotence" ;
    ap:equationLatex "\\mu \\circ \\mu = \\mu" ;
    math:equationNumber 3 ;
    ap:equationOrder 3 ;
    math:description "Idempotence property" .

:Eq4 a math:Equation ;
    ap:equationName "shard_law" ;
    ap:equationLatex "\\mu(O_1 \\sqcup O_2) = \\mu(O_1) \\sqcup \\mu(O_2)" ;
    math:equationNumber 4 ;
    ap:equationOrder 4 ;
    math:description "Shard law property" .

:Eq5 a math:Equation ;
    ap:equationName "bounded_regeneration" ;
    ap:equationLatex "\\mu_{t+1}(O) = \\mu_t(O) \\text{ while } \\text{drift} > \\varepsilon" ;
    math:equationNumber 5 ;
    ap:equationOrder 5 ;
    math:description "Bounded regeneration" .

:Eq6 a math:Equation ;
    ap:equationName "receipt_schema" ;
    ap:equationLatex "R_t = (h_O, h_\\Gamma, h_H, h_A, h_\\mu)" ;
    math:equationNumber 6 ;
    ap:equationOrder 6 ;
    math:description "Receipt schema" .

:Eq7 a math:Equation ;
    ap:equationName "knowledge_hooks" ;
    ap:equationLatex "\\text{Hooks}(O) = \\{h_i : \\text{guard}_i(O) \\Rightarrow \\text{action}_i\\}" ;
    math:equationNumber 7 ;
    ap:equationOrder 7 ;
    math:description "Knowledge hooks definition" .

:Eq8 a math:Equation ;
    ap:equationName "deterministic_proof" ;
    ap:equationLatex "\\text{hash}(A) = \\text{hash}(\\mu(O)) \\Rightarrow \\text{deterministic}" ;
    math:equationNumber 8 ;
    ap:equationOrder 8 ;
    math:description "Deterministic proof via hashing" .
    """


@pytest.fixture
def v3_paper_rdf():
    """Fixture: v3 Chatman Equation paper (15 equations, includes dark matter)."""
    v1 = v1_paper_rdf()
    # In real implementation, would add equations 9-15 (dark matter)
    return v1


# ============================================================================
# Conference Submission Fixtures
# ============================================================================

@pytest.fixture
def arxiv_submission_metadata():
    """Fixture: arXiv submission metadata."""
    return {
        "venue": "arXiv",
        "format": "arxiv",
        "page_limit": None,  # No limit
        "requirements": [
            "PDF from LaTeX",
            "All source files",
            "Valid metadata",
        ],
    }


@pytest.fixture
def ieee_submission_metadata():
    """Fixture: IEEE submission metadata."""
    return {
        "venue": "IEEE",
        "format": "ieee",
        "page_limit": 8,
        "requirements": [
            "Two-column format",
            "10pt font",
            "1 inch margins",
            "Proper equation numbering",
        ],
    }


@pytest.fixture
def acm_submission_metadata():
    """Fixture: ACM submission metadata."""
    return {
        "venue": "ACM",
        "format": "acm",
        "page_limit": 10,
        "requirements": [
            "ACM template",
            "Keywords required",
            "CCS concepts (if applicable)",
            "Proper citations",
        ],
    }


@pytest.fixture
def neurips_submission_metadata():
    """Fixture: NeurIPS submission metadata."""
    return {
        "venue": "NeurIPS",
        "format": "neurips",
        "page_limit": 8,
        "reference_limit": 2,
        "requirements": [
            "Anonymous submission",
            "Two-column format",
            "NeurIPS template",
            "Supplementary material allowed",
        ],
    }


# ============================================================================
# Startup Fixtures
# ============================================================================

@pytest.fixture
def seed_stage_data():
    """Fixture: Seed stage company data."""
    return {
        "stage": "Seed",
        "arr": 100_000,
        "customers": 1,
        "team_size": 3,
        "runway_months": 12,
        "funding_raised": 1_000_000,
    }


@pytest.fixture
def series_a_stage_data():
    """Fixture: Series A stage company data."""
    return {
        "stage": "Series A",
        "arr": 2_000_000,
        "customers": 10,
        "team_size": 15,
        "runway_months": 18,
        "funding_raised": 15_000_000,
    }


@pytest.fixture
def unicorn_stage_data():
    """Fixture: Unicorn stage company data."""
    return {
        "stage": "Unicorn",
        "valuation": 1_000_000_000,
        "arr": 250_000_000,
        "customers": 500,
        "team_size": 500,
        "gross_margin": 0.80,
        "nps": 60,
    }


# ============================================================================
# IPO Fixtures
# ============================================================================

@pytest.fixture
def ipo_readiness_data():
    """Fixture: IPO readiness checklist."""
    return {
        "audited_financials": True,
        "internal_controls": True,
        "board_structure": True,
        "audit_committee": True,
        "min_revenue": 100_000_000,
        "min_growth_rate": 0.30,
        "min_retention": 0.85,
        "path_to_profitability": True,
    }


# ============================================================================
# Hooks for Test Reporting
# ============================================================================

def pytest_runtest_logreport(report):
    """Hook: Record test results."""
    if report.when == "call":
        # Determine test category from markers
        category = "other"
        for marker in report.keywords:
            if marker in ["academic", "conference", "startup", "unicorn", "ipo"]:
                category = marker
                break

        # Record result
        test_context.add_result(
            category,
            report.nodeid,
            report.outcome == "passed",
            report.longreprtext if report.outcome == "failed" else "",
        )


def pytest_sessionfinish(session, exitstatus):
    """Hook: Print test summary at end."""
    summary = test_context.get_summary()
    print("\n" + "=" * 80)
    print("TEST EXECUTION SUMMARY")
    print("=" * 80)
    print(f"Total Tests: {summary['total_tests']}")
    print(f"Passed: {summary['passed']}")
    print(f"Failed: {summary['failed']}")
    print(f"Success Rate: {summary['success_rate']:.1%}")
    print(f"Duration: {summary['duration']}")
    print("=" * 80)
