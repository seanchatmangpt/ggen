#!/usr/bin/env python3
"""
Test suite for Spec-Kit-3T thesis validation system
Verifies SHACL shapes catch constraint violations
"""

import sys
from pathlib import Path
from rdflib import Graph, Namespace, Literal
from pyshacl import validate

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

THESIS = Namespace("http://github.com/seanchatmangpt/spec-kit-3t/thesis#")
CONTENT = Namespace("http://github.com/seanchatmangpt/spec-kit-3t/content#")


def load_shapes():
    """Load SHACL shapes graph"""
    shapes_graph = Graph()
    shapes_file = Path(__file__).parent.parent / "ontology" / "thesis-shapes.ttl"
    shapes_graph.parse(shapes_file, format="turtle")
    return shapes_graph


def test_missing_title():
    """Test that missing title triggers validation error"""
    print("\n🧪 Test: Missing thesis title")

    # Create invalid ontology (no title)
    data_graph = Graph()
    data_graph.bind("thesis", THESIS)
    data_graph.bind("", CONTENT)

    thesis_uri = CONTENT["test-thesis"]
    data_graph.add((thesis_uri, THESIS.hasYear, Literal(2025)))

    shapes = load_shapes()
    conforms, _, results = validate(data_graph, shacl_graph=shapes)

    assert not conforms, "Should fail validation (missing title)"
    print("✅ Correctly detected missing title")


def test_invalid_year():
    """Test that invalid year range triggers error"""
    print("\n🧪 Test: Invalid year (out of range)")

    data_graph = Graph()
    data_graph.bind("thesis", THESIS)
    data_graph.bind("", CONTENT)

    thesis_uri = CONTENT["test-thesis"]
    data_graph.add((thesis_uri, THESIS.hasTitle, Literal("Test Thesis")))
    data_graph.add((thesis_uri, THESIS.hasYear, Literal(1999)))  # Too old

    shapes = load_shapes()
    conforms, _, results = validate(data_graph, shacl_graph=shapes)

    assert not conforms, "Should fail validation (year out of range)"
    print("✅ Correctly detected invalid year")


def test_short_abstract():
    """Test that abstract length constraint works"""
    print("\n🧪 Test: Abstract too short")

    data_graph = Graph()
    data_graph.bind("thesis", THESIS)
    data_graph.bind("", CONTENT)

    abstract_uri = CONTENT["test-abstract"]
    data_graph.add((abstract_uri, THESIS.hasContent, Literal("Too short")))  # <200 chars

    shapes = load_shapes()
    conforms, _, results = validate(data_graph, shacl_graph=shapes)

    assert not conforms, "Should fail validation (abstract too short)"
    print("✅ Correctly detected short abstract")


def test_diataxis_tutorial_constraints():
    """Test Diataxis tutorial must have learning objective"""
    print("\n🧪 Test: Diataxis Tutorial constraints")

    from rdflib import RDF

    data_graph = Graph()
    data_graph.bind("thesis", THESIS)
    data_graph.bind("diataxis", Namespace("http://diataxis.fr/"))
    data_graph.bind("", CONTENT)

    DIATAXIS = Namespace("http://diataxis.fr/")

    tutorial_uri = CONTENT["test-tutorial"]
    data_graph.add((tutorial_uri, RDF.type, DIATAXIS.Tutorial))
    # Missing learningObjective

    shapes = load_shapes()
    conforms, _, results = validate(data_graph, shacl_graph=shapes)

    assert not conforms, "Should fail validation (missing learning objective)"
    print("✅ Correctly detected missing learning objective")


def test_valid_thesis():
    """Test that a complete valid thesis passes"""
    print("\n🧪 Test: Valid complete thesis")

    # Load actual thesis content
    data_graph = Graph()
    content_file = Path(__file__).parent.parent / "ontology" / "spec-kit-3t-content.ttl"
    schema_file = Path(__file__).parent.parent / "ontology" / "thesis-schema.ttl"

    data_graph.parse(content_file, format="turtle")
    data_graph.parse(schema_file, format="turtle")

    shapes = load_shapes()
    conforms, _, results = validate(data_graph, shacl_graph=shapes)

    if not conforms:
        print("❌ Valid thesis failed validation:")
        print(results)
        assert False, "Valid thesis should pass validation"
    else:
        print("✅ Valid thesis passed all constraints")


def test_chapter_without_sections():
    """Test that chapter without sections fails"""
    print("\n🧪 Test: Chapter without sections")

    from rdflib import RDF

    data_graph = Graph()
    data_graph.bind("thesis", THESIS)
    data_graph.bind("", CONTENT)

    chapter_uri = CONTENT["test-chapter"]
    data_graph.add((chapter_uri, RDF.type, THESIS.Chapter))
    data_graph.add((chapter_uri, THESIS.chapterNumber, Literal(1)))
    data_graph.add((chapter_uri, THESIS.chapterTitle, Literal("Test Chapter")))
    # Missing hasSection

    shapes = load_shapes()
    conforms, _, results = validate(data_graph, shacl_graph=shapes)

    assert not conforms, "Should fail validation (no sections)"
    print("✅ Correctly detected chapter without sections")


def run_all_tests():
    """Run complete test suite"""
    print("="*70)
    print("SHACL Validation Test Suite")
    print("="*70)

    tests = [
        test_missing_title,
        test_invalid_year,
        test_short_abstract,
        test_diataxis_tutorial_constraints,
        test_chapter_without_sections,
        test_valid_thesis,
    ]

    passed = 0
    failed = 0

    for test in tests:
        try:
            test()
            passed += 1
        except AssertionError as e:
            print(f"❌ Test failed: {e}")
            failed += 1
        except Exception as e:
            print(f"❌ Test error: {e}")
            failed += 1

    print("\n" + "="*70)
    print(f"Results: {passed} passed, {failed} failed")
    print("="*70)

    return failed == 0


if __name__ == "__main__":
    success = run_all_tests()
    sys.exit(0 if success else 1)
