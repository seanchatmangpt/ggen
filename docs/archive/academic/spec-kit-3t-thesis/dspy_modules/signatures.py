"""DSPy signatures for thesis generation enhancement."""

import dspy
from typing import Optional


class ThesisAbstractExpansion(dspy.Signature):
    """Expand thesis abstract with academic context and motivation."""

    # Input fields
    current_abstract: str = dspy.InputField(
        desc="Current thesis abstract from RDF ontology"
    )
    title: str = dspy.InputField(desc="Thesis title")
    research_area: str = dspy.InputField(desc="Research domain/field")

    # Output fields
    expanded_abstract: str = dspy.OutputField(
        desc="Enhanced abstract with research context, motivation, and contributions (200-300 words)",
        prefix="Enhanced Abstract:"
    )
    motivation: str = dspy.OutputField(
        desc="Research motivation and problem statement (2-3 sentences)",
        prefix="Motivation:"
    )


class SectionContentSuggestion(dspy.Signature):
    """Suggest additional content for thesis sections."""

    # Input fields
    section_title: str = dspy.InputField(desc="Current section title")
    current_content: str = dspy.InputField(desc="Existing section content from RDF")
    chapter_context: str = dspy.InputField(desc="Chapter title and overall context")

    # Output fields
    content_suggestions: str = dspy.OutputField(
        desc="Suggested additional content points (3-5 bullet points)",
        prefix="Suggestions:"
    )
    related_concepts: str = dspy.OutputField(
        desc="Related concepts to explore (comma-separated)",
        prefix="Related Concepts:"
    )


class DiataxisStructureValidator(dspy.Signature):
    """Validate and suggest improvements for Diataxis chapter structure."""

    # Input fields
    chapter_type: str = dspy.InputField(
        desc="Diataxis type: tutorial, howto, reference, or explanation"
    )
    chapter_title: str = dspy.InputField(desc="Chapter title")
    section_titles: str = dspy.InputField(
        desc="List of section titles (newline-separated)"
    )

    # Output fields
    structure_score: str = dspy.OutputField(
        desc="Adherence score to Diataxis framework (0-10 with reasoning)",
        prefix="Score:"
    )
    improvement_suggestions: str = dspy.OutputField(
        desc="Specific suggestions to improve Diataxis alignment",
        prefix="Improvements:"
    )


class AcademicCitationSuggestion(dspy.Signature):
    """Suggest relevant academic citations for thesis content."""

    # Input fields
    section_content: str = dspy.InputField(desc="Section content needing citations")
    research_area: str = dspy.InputField(desc="Research domain")
    year_range: str = dspy.InputField(
        desc="Target publication year range (e.g., 2020-2025)"
    )

    # Output fields
    citation_suggestions: str = dspy.OutputField(
        desc="Suggested citation areas with search queries (3-5 suggestions)",
        prefix="Citation Suggestions:"
    )
    key_terms: str = dspy.OutputField(
        desc="Key academic terms for literature search",
        prefix="Key Terms:"
    )


class ChapterCoherenceAnalysis(dspy.Signature):
    """Analyze coherence between adjacent chapters."""

    # Input fields
    chapter1_summary: str = dspy.InputField(desc="Summary of first chapter")
    chapter2_summary: str = dspy.InputField(desc="Summary of second chapter")

    # Output fields
    coherence_score: str = dspy.OutputField(
        desc="Coherence score (0-10) with reasoning",
        prefix="Coherence Score:"
    )
    transition_suggestions: str = dspy.OutputField(
        desc="Suggested transition sentences or paragraphs",
        prefix="Transition Suggestions:"
    )
