#!/usr/bin/env python3
"""
Validate Evidence Graph 3.0 integrity
"""

import json

def validate_graph(graph_path):
    """Validate graph has no orphaned nodes and all edges are valid"""
    with open(graph_path, 'r') as f:
        graph = json.load(f)

    print("=" * 80)
    print("Evidence Graph 3.0 Integrity Validation")
    print("=" * 80)

    # Collect all node IDs
    node_ids = set()

    for concept in graph['nodes']['concepts']:
        node_ids.add(concept['id'])

    for system in graph['nodes']['systems']:
        node_ids.add(system['id'])

    for evidence in graph['nodes']['evidence']:
        node_ids.add(evidence['evidence_id'])

    print(f"\n✓ Total nodes: {len(node_ids)}")
    print(f"  - Concepts: {len(graph['nodes']['concepts'])}")
    print(f"  - Systems: {len(graph['nodes']['systems'])}")
    print(f"  - Evidence: {len(graph['nodes']['evidence'])}")

    # Validate edges
    orphaned_from = []
    orphaned_to = []
    valid_edges = 0

    for edge in graph['edges']:
        from_id = edge['from']
        to_id = edge['to']

        if from_id not in node_ids:
            orphaned_from.append(from_id)

        if to_id not in node_ids:
            orphaned_to.append(to_id)

        if from_id in node_ids and to_id in node_ids:
            valid_edges += 1

    print(f"\n✓ Total edges: {len(graph['edges'])}")
    print(f"  - Valid edges: {valid_edges}")
    print(f"  - Orphaned 'from' nodes: {len(set(orphaned_from))}")
    print(f"  - Orphaned 'to' nodes: {len(set(orphaned_to))}")

    if orphaned_from:
        print(f"\n⚠ Warning: Found orphaned 'from' nodes:")
        for node in set(orphaned_from):
            print(f"  - {node}")

    if orphaned_to:
        print(f"\n⚠ Warning: Found orphaned 'to' nodes:")
        for node in set(orphaned_to):
            print(f"  - {node}")

    # Validate coverage
    print(f"\n✓ Coverage Metrics:")
    coverage = graph['coverage_metrics']
    for concept_id, stats in sorted(coverage.items(), key=lambda x: -x[1]['evidence_count'])[:5]:
        print(f"  - {concept_id}: {stats['evidence_count']} evidence "
              f"(avg strength: {stats.get('avg_strength', 0):.2f})")

    # Edge type distribution
    edge_types = {}
    for edge in graph['edges']:
        kind = edge['kind']
        edge_types[kind] = edge_types.get(kind, 0) + 1

    print(f"\n✓ Edge Type Distribution:")
    for kind, count in sorted(edge_types.items(), key=lambda x: -x[1]):
        print(f"  - {kind}: {count}")

    # Evidence type distribution
    evidence_types = {}
    for ev in graph['nodes']['evidence']:
        ev_type = ev.get('evidence_type', 'unknown')
        evidence_types[ev_type] = evidence_types.get(ev_type, 0) + 1

    print(f"\n✓ Evidence Type Distribution:")
    for ev_type, count in sorted(evidence_types.items(), key=lambda x: -x[1])[:10]:
        print(f"  - {ev_type}: {count}")

    # Sub-concept validation
    parent_concepts = [c for c in graph['nodes']['concepts'] if 'parent_concept' not in c]
    sub_concepts = [c for c in graph['nodes']['concepts'] if 'parent_concept' in c]

    print(f"\n✓ Concept Hierarchy:")
    print(f"  - Parent concepts: {len(parent_concepts)}")
    print(f"  - Sub-concepts: {len(sub_concepts)}")

    # Find concepts with most sub-concepts
    parent_subconcept_count = {}
    for sub in sub_concepts:
        parent = sub['parent_concept']
        parent_subconcept_count[parent] = parent_subconcept_count.get(parent, 0) + 1

    print(f"\n✓ Concepts with Most Refinements:")
    for parent, count in sorted(parent_subconcept_count.items(), key=lambda x: -x[1])[:5]:
        print(f"  - {parent}: {count} sub-concepts")

    # Final validation
    print("\n" + "=" * 80)
    if not orphaned_from and not orphaned_to:
        print("✓ VALIDATION PASSED: Graph is complete with no orphaned nodes")
    else:
        print("⚠ VALIDATION WARNING: Some orphaned nodes detected")
    print("=" * 80)

    return len(orphaned_from) == 0 and len(orphaned_to) == 0


if __name__ == '__main__':
    validate_graph('/home/user/ggen/.ggen/evidence_graph_v2.json')
