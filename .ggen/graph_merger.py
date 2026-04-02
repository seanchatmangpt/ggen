#!/usr/bin/env python3
"""
Graph Merger Agent: Integrates breadth, depth, and relationship expansions
into a comprehensive, unified Evidence Graph 3.0
"""

import json
import hashlib
from typing import Dict, List, Set, Tuple
from collections import defaultdict
from datetime import datetime

def normalize_path(path: str) -> str:
    """Normalize file paths for comparison"""
    path = path.replace('/home/user/ggen/', '')
    path = path.replace('crates/ggen-dod/src/', 'ggen-dod/src/')
    path = path.replace('crates/ggen-domain/src/', 'ggen-domain/src/')
    path = path.replace('crates/ggen-marketplace/src/', 'ggen-marketplace/src/')
    path = path.replace('crates/ggen-core/src/', 'ggen-core/src/')
    return path

def make_evidence_key(repo: str, path: str, lines: str, concept: str) -> str:
    """Create unique key for evidence de-duplication"""
    norm_path = normalize_path(path)
    return f"{repo}|{norm_path}|{lines}|{concept}"

def make_evidence_id(repo: str, path: str, lines: str, version: str = "v2") -> str:
    """Generate evidence ID"""
    norm_path = normalize_path(path)
    filename = norm_path.split('/')[-1].replace('.rs', '').replace('.md', '').replace('.ttl', '')
    return f"{repo}.{filename}.{lines}.{version}"

class GraphMerger:
    def __init__(self):
        self.evidence_nodes = {}  # key -> evidence node
        self.concept_nodes = []
        self.system_nodes = []
        self.edges = []
        self.coverage_stats = defaultdict(lambda: {
            'evidence_count': 0,
            'implicit_evidence': 0,
            'granular_claims': 0,
            'systems': set(),
            'min_strength': 1.0,
            'max_strength': 0.0,
            'strengths': []
        })

    def load_original_graph(self, graph_path: str):
        """Load original evidence graph"""
        with open(graph_path, 'r') as f:
            data = json.load(f)

        # Load concepts
        self.concept_nodes = data['nodes']['concepts']

        # Load systems
        self.system_nodes = data['nodes']['systems']

        # Load evidence with de-duplication
        for ev in data['nodes']['evidence']:
            key = make_evidence_key(
                ev['repo_id'],
                ev['path'],
                ev['lines'],
                ev['concept_id']
            )

            # Add evidence_type if not present
            if 'evidence_type' not in ev:
                ev['evidence_type'] = 'implementation'

            self.evidence_nodes[key] = ev

            # Track coverage
            concept_id = ev['concept_id']
            self.coverage_stats[concept_id]['evidence_count'] += 1
            self.coverage_stats[concept_id]['strengths'].append(ev['strength'])
            self.coverage_stats[concept_id]['min_strength'] = min(
                self.coverage_stats[concept_id]['min_strength'],
                ev['strength']
            )
            self.coverage_stats[concept_id]['max_strength'] = max(
                self.coverage_stats[concept_id]['max_strength'],
                ev['strength']
            )

        # Load edges
        self.edges = data['edges']

        print(f"✓ Loaded original graph: {len(self.concept_nodes)} concepts, "
              f"{len(self.evidence_nodes)} evidence, {len(self.edges)} edges")

    def integrate_breadth_expansion(self, breadth_path: str):
        """Integrate implicit evidence from breadth expansion"""
        with open(breadth_path, 'r') as f:
            data = json.load(f)

        added_count = 0

        # Add code comments as evidence
        for comment in data['implicit_evidence']['code_comments']:
            key = make_evidence_key('ggen', comment['file'], str(comment['line']), comment['concept_id'])
            if key not in self.evidence_nodes:
                ev = {
                    'evidence_id': make_evidence_id('ggen', comment['file'], str(comment['line'])),
                    'repo_id': 'ggen',
                    'path': comment['file'],
                    'lines': str(comment['line']),
                    'concept_id': comment['concept_id'],
                    'support_type': 'direct',
                    'claim_summary': f"Code comment: {comment['text'][:100]}...",
                    'key_phrases': [comment['text'][:50]],
                    'strength': comment['strength'],
                    'evidence_type': 'code_comment'
                }
                self.evidence_nodes[key] = ev
                self.coverage_stats[comment['concept_id']]['implicit_evidence'] += 1
                added_count += 1

        # Add function names as evidence
        for func in data['implicit_evidence']['function_names']:
            key = make_evidence_key('ggen', func['file'], 'func', func['concept_id'])
            if key not in self.evidence_nodes:
                ev = {
                    'evidence_id': make_evidence_id('ggen', func['file'], 'func'),
                    'repo_id': 'ggen',
                    'path': func['file'],
                    'lines': 'function',
                    'concept_id': func['concept_id'],
                    'support_type': 'direct',
                    'claim_summary': f"Function: {func['name']} - {func['signature'][:100]}",
                    'key_phrases': [func['name']],
                    'strength': func['strength'],
                    'evidence_type': 'function_signature',
                    'implementation_detail': {
                        'function': func['name'],
                        'signature': func['signature']
                    }
                }
                self.evidence_nodes[key] = ev
                self.coverage_stats[func['concept_id']]['implicit_evidence'] += 1
                added_count += 1

        # Add test names as evidence
        for test in data['implicit_evidence']['test_names']:
            key = make_evidence_key('ggen', test['file'], str(test['line']), test['concept_id'])
            if key not in self.evidence_nodes:
                ev = {
                    'evidence_id': make_evidence_id('ggen', test['file'], str(test['line'])),
                    'repo_id': 'ggen',
                    'path': test['file'],
                    'lines': str(test['line']),
                    'concept_id': test['concept_id'],
                    'support_type': 'direct',
                    'claim_summary': f"Test: {test['test']}",
                    'key_phrases': [test['test']],
                    'strength': test['strength'],
                    'evidence_type': 'test',
                    'implementation_detail': {
                        'test_name': test['test'],
                        'location': f"{test['file']}:{test['line']}"
                    }
                }
                self.evidence_nodes[key] = ev
                self.coverage_stats[test['concept_id']]['implicit_evidence'] += 1
                added_count += 1

        # Add quantitative evidence
        for quant in data['quantitative_evidence']:
            concept_id = quant['concept_id']
            key = make_evidence_key('ggen', quant['evidence_file'], 'quant', concept_id)
            if key not in self.evidence_nodes:
                ev = {
                    'evidence_id': f"ggen.quantitative.{quant['metric'].lower().replace(' ', '_')}",
                    'repo_id': 'ggen',
                    'path': quant['evidence_file'],
                    'lines': 'quantitative',
                    'concept_id': concept_id,
                    'support_type': 'direct',
                    'claim_summary': f"{quant['metric']} = {quant['value']}",
                    'key_phrases': [quant['metric']],
                    'strength': quant['strength'],
                    'evidence_type': 'quantitative',
                    'quantitative_detail': {
                        'metric': quant['metric'],
                        'value': quant['value'],
                        'source': quant['evidence_file']
                    }
                }
                self.evidence_nodes[key] = ev
                self.coverage_stats[concept_id]['implicit_evidence'] += 1
                added_count += 1

        print(f"✓ Added {added_count} implicit evidence nodes from breadth expansion")

    def integrate_depth_expansion(self, depth_path: str):
        """Integrate granular claims from depth expansion"""
        with open(depth_path, 'r') as f:
            data = json.load(f)

        added_count = 0

        for concept_claims in data['granular_claims']:
            concept_id = concept_claims['concept_id']

            for claim in concept_claims['claims']:
                # Add each granular claim as refined evidence
                for evidence in claim['supporting_evidence']:
                    file_path = evidence['file']
                    lines = evidence['line']

                    key = make_evidence_key('ggen', file_path, str(lines), concept_id)

                    # If exists, upgrade it; otherwise add new
                    if key in self.evidence_nodes:
                        # Enhance existing evidence with implementation details
                        existing = self.evidence_nodes[key]
                        if 'implementation_detail' not in existing:
                            existing['implementation_detail'] = {}
                        existing['implementation_detail']['granular_claim'] = claim['claim_text']
                        existing['implementation_detail']['detail'] = evidence['detail']
                        existing['strength'] = max(existing['strength'], claim['strength'])
                    else:
                        ev = {
                            'evidence_id': make_evidence_id('ggen', file_path, str(lines)),
                            'repo_id': 'ggen',
                            'path': file_path,
                            'lines': str(lines),
                            'concept_id': concept_id,
                            'support_type': 'direct',
                            'claim_summary': claim['claim_text'],
                            'key_phrases': [claim['claim_text'][:50]],
                            'strength': claim['strength'],
                            'evidence_type': evidence['type'],
                            'implementation_detail': {
                                'granular_claim': claim['claim_text'],
                                'detail': evidence['detail'],
                                'specificity': claim['specificity_level']
                            }
                        }
                        self.evidence_nodes[key] = ev
                        self.coverage_stats[concept_id]['granular_claims'] += 1
                        added_count += 1

        print(f"✓ Enhanced/added {added_count} granular claim evidence nodes from depth expansion")

    def find_matching_evidence_id(self, shorthand: str) -> str:
        """Find matching evidence ID from shorthand reference"""
        # Try to find evidence that contains this reference
        for ev_id in self.evidence_nodes.values():
            if shorthand in ev_id['evidence_id']:
                return ev_id['evidence_id']

        # Return shorthand if not found (edge might reference future node)
        return shorthand

    def integrate_relationships(self, relationships_path: str):
        """Integrate relationship edges and sub-concepts"""
        with open(relationships_path, 'r') as f:
            data = json.load(f)

        edge_count = 0
        skipped_count = 0

        # Collect all valid node IDs
        valid_node_ids = set()
        for ev in self.evidence_nodes.values():
            valid_node_ids.add(ev['evidence_id'])
        for concept in self.concept_nodes:
            valid_node_ids.add(concept['id'])
        for system in self.system_nodes:
            valid_node_ids.add(system['id'])

        # Add evidence relationships (skip if nodes don't exist)
        for rel in data['evidence_relationships']:
            from_id = rel['from_evidence_id']
            to_id = rel['to_evidence_id']

            # Only add edge if both nodes exist
            if from_id in valid_node_ids and to_id in valid_node_ids:
                edge = {
                    'from': from_id,
                    'to': to_id,
                    'kind': rel['relationship_type'],
                    'weight': rel['strength'],
                    'description': rel['description']
                }
                self.edges.append(edge)
                edge_count += 1
            else:
                skipped_count += 1

        # Add system dependencies (only if both systems exist)
        for dep in data['system_dependencies']:
            from_sys = dep['from_system']
            to_sys = dep['to_system']

            if from_sys in valid_node_ids and to_sys in valid_node_ids:
                edge = {
                    'from': from_sys,
                    'to': to_sys,
                    'kind': 'depends_on',
                    'weight': dep['strength'],
                    'description': dep['integration_point']
                }
                self.edges.append(edge)
                edge_count += 1
            else:
                skipped_count += 1

        # Add concept dependencies (only if both concepts exist)
        for dep in data['concept_dependencies']:
            from_concept = dep['from_concept']
            to_concept = dep['to_concept']

            if from_concept in valid_node_ids and to_concept in valid_node_ids:
                edge = {
                    'from': from_concept,
                    'to': to_concept,
                    'kind': dep['relationship'],
                    'weight': dep['strength'],
                    'description': dep['explanation']
                }
                self.edges.append(edge)
                edge_count += 1
            else:
                skipped_count += 1

        # Add sub-concepts
        subconcept_count = 0
        for concept_id, refinement in data['concept_refinements'].items():
            for sub in refinement['sub_concepts']:
                subconcept_node = {
                    'type': 'concept',
                    'id': sub['sub_concept_id'],
                    'parent_concept': concept_id,
                    'category': 'refinement',
                    'name': sub['name'],
                    'description': sub['claim'],
                    'refinement_level': 1,
                    'supporting_evidence_count': sub['evidence_count'],
                    'max_strength': sub['max_strength']
                }
                self.concept_nodes.append(subconcept_node)
                subconcept_count += 1

                # Add decomposes edge
                self.edges.append({
                    'from': concept_id,
                    'to': sub['sub_concept_id'],
                    'kind': 'decomposes',
                    'weight': 1.0,
                    'description': f'{concept_id} decomposes into {sub["name"]}'
                })

        print(f"✓ Added {edge_count} relationship edges and {subconcept_count} sub-concepts")
        if skipped_count > 0:
            print(f"  (skipped {skipped_count} edges referencing non-existent nodes)")

    def finalize_coverage_metrics(self):
        """Compute final coverage metrics"""
        for concept_id, stats in self.coverage_stats.items():
            if stats['strengths']:
                stats['avg_strength'] = sum(stats['strengths']) / len(stats['strengths'])
            else:
                stats['avg_strength'] = 0.0

            # Remove internal strengths list
            del stats['strengths']

            # Convert sets to lists
            stats['systems'] = list(stats['systems'])

    def generate_evidence_graph_v2(self, output_path: str):
        """Generate unified evidence graph v2"""
        # Update coverage stats in evidence
        for ev in self.evidence_nodes.values():
            concept_id = ev['concept_id']
            # Track which systems reference this concept
            if 'repo_id' in ev:
                self.coverage_stats[concept_id]['systems'].add(ev['repo_id'])

        self.finalize_coverage_metrics()

        graph = {
            'version': '2.0',
            'generated_at': datetime.utcnow().isoformat() + 'Z',
            'nodes': {
                'concepts': self.concept_nodes,
                'evidence': list(self.evidence_nodes.values()),
                'systems': self.system_nodes
            },
            'edges': self.edges,
            'coverage_metrics': dict(self.coverage_stats)
        }

        with open(output_path, 'w') as f:
            json.dump(graph, f, indent=2)

        print(f"✓ Generated evidence_graph_v2.json: "
              f"{len(self.concept_nodes)} concepts, "
              f"{len(self.evidence_nodes)} evidence, "
              f"{len(self.edges)} edges")

    def generate_evidence_catalog(self, output_path: str):
        """Generate flat catalog for easy lookup"""
        catalog = {
            'by_concept': defaultdict(list),
            'by_system': defaultdict(list),
            'by_type': defaultdict(list)
        }

        for ev in self.evidence_nodes.values():
            catalog['by_concept'][ev['concept_id']].append(ev)
            catalog['by_system'][ev['repo_id']].append(ev)
            catalog['by_type'][ev['evidence_type']].append(ev)

        # Convert defaultdict to regular dict
        catalog_output = {
            'by_concept': dict(catalog['by_concept']),
            'by_system': dict(catalog['by_system']),
            'by_type': dict(catalog['by_type'])
        }

        with open(output_path, 'w') as f:
            json.dump(catalog_output, f, indent=2)

        print(f"✓ Generated evidence_catalog.json")

    def generate_graph_metrics(self, output_path: str):
        """Generate graph statistics"""
        # Count edge types
        edge_types = defaultdict(int)
        for edge in self.edges:
            edge_types[edge['kind']] += 1

        # Count evidence types
        evidence_types = defaultdict(int)
        for ev in self.evidence_nodes.values():
            evidence_types[ev['evidence_type']] += 1

        # Count concepts with evidence
        concepts_with_evidence = len([c for c in self.coverage_stats.keys() if self.coverage_stats[c]['evidence_count'] > 0])

        # Calculate average strength
        all_strengths = [ev['strength'] for ev in self.evidence_nodes.values()]
        avg_strength = sum(all_strengths) / len(all_strengths) if all_strengths else 0

        # Count direct evidence
        direct_evidence = len([ev for ev in self.evidence_nodes.values() if ev['support_type'] == 'direct'])

        metrics = {
            'nodes': {
                'concepts': len(self.concept_nodes),
                'systems': len(self.system_nodes),
                'evidence': len(self.evidence_nodes),
                'total': len(self.concept_nodes) + len(self.system_nodes) + len(self.evidence_nodes)
            },
            'edges': {
                **dict(edge_types),
                'total': len(self.edges)
            },
            'coverage': {
                'concepts_with_evidence': concepts_with_evidence,
                'concepts_total': len(self.concept_nodes),
                'average_concept_strength': round(avg_strength, 3),
                'percent_direct_evidence': round(100 * direct_evidence / len(self.evidence_nodes), 1) if self.evidence_nodes else 0
            },
            'evidence_by_type': dict(evidence_types)
        }

        with open(output_path, 'w') as f:
            json.dump(metrics, f, indent=2)

        print(f"✓ Generated graph_metrics.json")
        return metrics

    def generate_summary_markdown(self, output_path: str, metrics: dict):
        """Generate human-readable summary"""
        md = []
        md.append("# Evidence Graph 3.0 - Summary Report\n")
        md.append(f"Generated: {datetime.utcnow().strftime('%Y-%m-%d %H:%M:%S')} UTC\n")

        md.append("## Overview\n")
        md.append(f"- **Total Nodes**: {metrics['nodes']['total']}")
        md.append(f"  - Concepts: {metrics['nodes']['concepts']}")
        md.append(f"  - Systems: {metrics['nodes']['systems']}")
        md.append(f"  - Evidence: {metrics['nodes']['evidence']}")
        md.append(f"- **Total Edges**: {metrics['edges']['total']}\n")

        md.append("## Edge Types\n")
        for edge_type, count in sorted(metrics['edges'].items()):
            if edge_type != 'total':
                md.append(f"- **{edge_type}**: {count}")
        md.append("")

        md.append("## Evidence Types\n")
        for ev_type, count in sorted(metrics['evidence_by_type'].items(), key=lambda x: -x[1]):
            md.append(f"- **{ev_type}**: {count}")
        md.append("")

        md.append("## Coverage Metrics\n")
        md.append(f"- **Concepts with Evidence**: {metrics['coverage']['concepts_with_evidence']}/{metrics['coverage']['concepts_total']}")
        md.append(f"- **Average Concept Strength**: {metrics['coverage']['average_concept_strength']}")
        md.append(f"- **Percent Direct Evidence**: {metrics['coverage']['percent_direct_evidence']}%\n")

        md.append("## Top Concepts by Evidence Count\n")
        top_concepts = sorted(self.coverage_stats.items(),
                             key=lambda x: x[1]['evidence_count'],
                             reverse=True)[:10]

        for concept_id, stats in top_concepts:
            md.append(f"### {concept_id}")
            md.append(f"- Evidence Count: {stats['evidence_count']}")
            md.append(f"- Implicit Evidence: {stats['implicit_evidence']}")
            md.append(f"- Granular Claims: {stats['granular_claims']}")
            md.append(f"- Avg Strength: {stats.get('avg_strength', 0):.2f}")
            md.append(f"- Min/Max Strength: {stats['min_strength']:.2f} / {stats['max_strength']:.2f}\n")

        md.append("## Key Architectural Insights\n")
        md.append("- **Timing Bounds**: CHATMAN_CONSTANT_MS = 8ms enforced at kernel level with fatal violations")
        md.append("- **Determinism**: SHA256 hash verification ensures A = μ(O) with replay testing")
        md.append("- **Cryptographic Proofs**: HMAC-SHA256 signatures provide tamper-proof receipts")
        md.append("- **Autonomous Governance**: AHI manages ΔΣ via MAPE-K loop without human arbitration")
        md.append("- **Ontology Primacy**: Σ (RDF ontologies) as single source of truth for all code generation")
        md.append("- **Complete Provenance**: Every decision tracked from observations → receipts → evidence chain\n")

        with open(output_path, 'w') as f:
            f.write('\n'.join(md))

        print(f"✓ Generated evidence_summary_v2.md")


def main():
    print("=" * 80)
    print("Graph Merger Agent - Evidence Graph 3.0 Integration")
    print("=" * 80)

    merger = GraphMerger()

    # Step 1: Load original graph
    print("\n[1/7] Loading original evidence graph...")
    merger.load_original_graph('/home/user/ggen/.ggen/evidence_graph.json')

    # Step 2: Integrate breadth expansion
    print("\n[2/7] Integrating breadth expansion (implicit evidence)...")
    merger.integrate_breadth_expansion('/home/user/ggen/.ggen/breadth_expansion_report.json')

    # Step 3: Integrate depth expansion
    print("\n[3/7] Integrating depth expansion (granular claims)...")
    merger.integrate_depth_expansion('/home/user/ggen/.ggen/depth_expansion_report.json')

    # Step 4: Integrate relationships
    print("\n[4/7] Integrating relationships and sub-concepts...")
    merger.integrate_relationships('/home/user/ggen/.ggen/relationships_graph.json')

    # Step 5: Generate unified graph
    print("\n[5/7] Generating evidence_graph_v2.json...")
    merger.generate_evidence_graph_v2('/home/user/ggen/.ggen/evidence_graph_v2.json')

    # Step 6: Generate catalog
    print("\n[6/7] Generating evidence_catalog.json...")
    merger.generate_evidence_catalog('/home/user/ggen/.ggen/evidence_catalog.json')

    # Step 7: Generate metrics and summary
    print("\n[7/7] Generating metrics and summary...")
    metrics = merger.generate_graph_metrics('/home/user/ggen/.ggen/graph_metrics.json')
    merger.generate_summary_markdown('/home/user/ggen/.ggen/evidence_summary_v2.md', metrics)

    print("\n" + "=" * 80)
    print("✓ Graph merger complete!")
    print(f"  - {metrics['nodes']['evidence']} total evidence nodes")
    print(f"  - {metrics['edges']['total']} edges")
    print(f"  - {metrics['nodes']['concepts']} concepts (including sub-concepts)")
    print("=" * 80)


if __name__ == '__main__':
    main()
