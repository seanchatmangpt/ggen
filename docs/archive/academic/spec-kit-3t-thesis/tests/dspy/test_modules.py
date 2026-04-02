"""Unit tests for DSPy modules."""

import pytest
from unittest.mock import Mock, patch
from pathlib import Path


# Skip tests if DSPy not installed
pytest.importorskip("dspy")


@pytest.fixture
def mock_dspy_lm():
    """Mock DSPy LM for testing."""
    import dspy
    # Create mock LM
    mock_lm = Mock()
    dspy.configure(lm=mock_lm)
    return mock_lm


@pytest.fixture
def mock_prediction():
    """Create mock DSPy prediction."""
    import dspy
    return dspy.Prediction(
        expanded_abstract="This is an enhanced abstract with more academic context.",
        motivation="The motivation is to improve thesis quality."
    )


class TestAbstractEnhancer:
    """Tests for AbstractEnhancer module."""

    def test_abstract_enhancer_initialization(self, mock_dspy_lm):
        """Test AbstractEnhancer initializes correctly."""
        from dspy_modules.modules import AbstractEnhancer

        enhancer = AbstractEnhancer()
        assert enhancer is not None
        assert hasattr(enhancer, 'enhance')

    @patch('dspy.ChainOfThought')
    def test_abstract_enhancer_forward(self, mock_cot, mock_dspy_lm, mock_prediction):
        """Test AbstractEnhancer.forward accepts correct inputs."""
        from dspy_modules.modules import AbstractEnhancer

        mock_cot.return_value = Mock(return_value=mock_prediction)
        enhancer = AbstractEnhancer()
        enhancer.enhance = mock_cot.return_value

        result = enhancer(
            current_abstract="This is a test abstract.",
            title="Test Thesis",
            research_area="Testing"
        )

        assert hasattr(result, "expanded_abstract")
        assert hasattr(result, "motivation")


class TestSectionSuggester:
    """Tests for SectionSuggester module."""

    def test_section_suggester_initialization(self, mock_dspy_lm):
        """Test SectionSuggester initializes correctly."""
        from dspy_modules.modules import SectionSuggester

        suggester = SectionSuggester()
        assert suggester is not None
        assert hasattr(suggester, 'suggest')

    @patch('dspy.Predict')
    def test_section_suggester_output_format(self, mock_predict, mock_dspy_lm):
        """Test SectionSuggester returns expected fields."""
        from dspy_modules.modules import SectionSuggester
        import dspy

        mock_result = dspy.Prediction(
            content_suggestions="1. Add more details\n2. Include examples",
            related_concepts="testing, validation, quality"
        )
        mock_predict.return_value = Mock(return_value=mock_result)

        suggester = SectionSuggester()
        suggester.suggest = mock_predict.return_value

        result = suggester(
            section_title="Introduction",
            current_content="Brief intro.",
            chapter_context="Chapter 1: Overview"
        )

        assert hasattr(result, "content_suggestions")
        assert hasattr(result, "related_concepts")


class TestDiataxisValidator:
    """Tests for DiataxisValidator module."""

    def test_diataxis_validator_initialization(self, mock_dspy_lm):
        """Test DiataxisValidator initializes correctly."""
        from dspy_modules.modules import DiataxisValidator

        validator = DiataxisValidator()
        assert validator is not None
        assert hasattr(validator, 'validate')

    @patch('dspy.ReAct')
    def test_diataxis_validator_scores(self, mock_react, mock_dspy_lm):
        """Test DiataxisValidator returns score."""
        from dspy_modules.modules import DiataxisValidator
        import dspy

        mock_result = dspy.Prediction(
            structure_score="8/10: Good alignment with tutorial principles",
            improvement_suggestions="Consider adding more hands-on examples"
        )
        mock_react.return_value = Mock(return_value=mock_result)

        validator = DiataxisValidator()
        validator.validate = mock_react.return_value

        result = validator(
            chapter_type="tutorial",
            chapter_title="Getting Started",
            section_titles="Setup\nFirst Steps\nNext Steps"
        )

        assert hasattr(result, "structure_score")
        assert hasattr(result, "improvement_suggestions")


class TestCitationAssistant:
    """Tests for CitationAssistant module."""

    def test_citation_assistant_initialization(self, mock_dspy_lm):
        """Test CitationAssistant initializes correctly."""
        from dspy_modules.modules import CitationAssistant

        assistant = CitationAssistant()
        assert assistant is not None
        assert hasattr(assistant, 'suggest_citations')


class TestCoherenceAnalyzer:
    """Tests for CoherenceAnalyzer module."""

    def test_coherence_analyzer_initialization(self, mock_dspy_lm):
        """Test CoherenceAnalyzer initializes correctly."""
        from dspy_modules.modules import CoherenceAnalyzer

        analyzer = CoherenceAnalyzer()
        assert analyzer is not None
        assert hasattr(analyzer, 'analyze')


class TestThesisEnhancementPipeline:
    """Tests for ThesisEnhancementPipeline."""

    def test_pipeline_initialization(self, mock_dspy_lm):
        """Test pipeline initializes all modules."""
        from dspy_modules.modules import ThesisEnhancementPipeline

        pipeline = ThesisEnhancementPipeline()
        assert pipeline is not None
        assert hasattr(pipeline, 'abstract_enhancer')
        assert hasattr(pipeline, 'section_suggester')
        assert hasattr(pipeline, 'diataxis_validator')
        assert hasattr(pipeline, 'citation_assistant')
        assert hasattr(pipeline, 'coherence_analyzer')

    @patch.object('dspy_modules.modules.AbstractEnhancer', 'forward')
    def test_enhance_abstract(self, mock_forward, mock_dspy_lm):
        """Test pipeline.enhance_abstract method."""
        from dspy_modules.modules import ThesisEnhancementPipeline
        import dspy

        mock_forward.return_value = dspy.Prediction(
            expanded_abstract="Enhanced abstract",
            motivation="Clear motivation"
        )

        pipeline = ThesisEnhancementPipeline()
        result = pipeline.enhance_abstract(
            current_abstract="Test",
            title="Title",
            research_area="Area"
        )

        assert "expanded_abstract" in result
        assert "motivation" in result
