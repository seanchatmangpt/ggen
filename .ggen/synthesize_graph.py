#!/usr/bin/env python3
"""
Evidence Synthesis Agent
Converts interim mining report into formal Evidence Graph
"""

import json
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any

def load_json(path: str) -> Any:
    with open(path, 'r') as f:
        return json.load(f)

def save_json(path: str, data: Any):
    with open(path, 'w') as f:
        json.dump(data, f, indent=2)

def extract_line_range(sections: str) -> str:
    """Extract line range from section description"""
    # Look for patterns like "lines 1-50" or "lines 1-100, 244-450"
    if "lines" in sections.lower():
        parts = sections.split(',')[0]  # Take first range if multiple
        if '-' in parts:
            nums = [s for s in parts.split() if '-' in s and s.replace('-', '').isdigit()]
            if nums:
                return nums[0]
    # Default fallback
    if "full file" in sections.lower():
        return "1-end"
    return "1-50"  # default

def generate_evidence_id(repo_id: str, file_path: str, lines: str) -> str:
    """Generate unique evidence ID"""
    filename = Path(file_path).name
    return f"{repo_id}.{filename}.{lines}"

def create_evidence_nodes(interim_data: Dict) -> List[Dict]:
    """Generate EvidenceNodes from concept_evidence section"""
    evidence_nodes = []

    concept_evidence = interim_data.get("concept_evidence", {})

    for concept_id, evidence_list in concept_evidence.items():
        for evidence in evidence_list:
            file_path = evidence["file"]
            sections = evidence.get("sections", "full file")
            lines = extract_line_range(sections)

            evidence_node = {
                "evidence_id": generate_evidence_id("ggen", file_path, lines),
                "repo_id": "ggen",
                "path": file_path,
                "lines": lines,
                "concept_id": concept_id,
                "support_type": determine_support_type(evidence, concept_id),
                "claim_summary": generate_claim_summary(evidence, concept_id, file_path),
                "key_phrases": evidence.get("key_phrases", []),
                "strength": evidence.get("strength", 0.5)
            }
            evidence_nodes.append(evidence_node)

    return evidence_nodes

def determine_support_type(evidence: Dict, concept_id: str) -> str:
    """Determine if evidence is direct, indirect, or contextual"""
    strength = evidence.get("strength", 0)
    file_path = evidence.get("file", "")

    # Direct: implements or defines the concept
    if strength >= 0.85:
        return "direct"
    # Indirect: references or uses the concept
    elif strength >= 0.6:
        return "indirect"
    # Contextual: provides related background
    else:
        return "contextual"

def generate_claim_summary(evidence: Dict, concept_id: str, file_path: str) -> str:
    """Generate a neutral claim summary"""
    key_phrases = evidence.get("key_phrases", [])
    strength = evidence.get("strength", 0)
    filename = Path(file_path).name

    # Map concept to readable name
    concept_map = {
        "C_GRAPH_UNIVERSE_PRIMARY": "ontology-primary architecture",
        "C_CODE_AS_PROJECTION": "code as projection from ontology",
        "C_RECEIPTS_AND_PROOFS": "receipt and proof systems",
        "C_MU_KERNEL_PHYSICS": "μ-kernel timing physics",
        "C_TIMING_BOUNDS_ENFORCED": "enforced timing bounds",
        "C_KNHK_GRAPH_PRIMARY": "knowledge hypergraph primacy",
        "C_DFLSS_FLOW": "DFLSS optimization flow",
        "C_AHI_GOVERNANCE": "AHI autonomic governance",
        "C_CTT_12_PHASE_VERIFICATION": "Chicago TDD 12-phase verification",
        "C_CLNRM_HERMETIC_TESTING": "hermetic cleanroom testing",
        "C_CNV_AGENT_CLI": "agent-grade capability CLI",
        "C_NOMRG_GRAPH_OVERLAY": "no-merge graph overlays",
        "C_GGEN_PROJECTION_ENGINE": "ggen projection engine",
        "C_UNIVERSE_PROJECTION_AXIOM": "A = μ(O) projection axiom",
        "C_RECEIPT_CHAIN_VERIFICATION": "immutable receipt chain verification",
        "C_AGENTS_ONLY_ARCHITECTURE": "agent-only architecture",
        "C_INVARIANT_ENFORCEMENT": "invariant enforcement"
    }

    concept_name = concept_map.get(concept_id, concept_id)

    if strength >= 0.95:
        verb = "Implements"
    elif strength >= 0.85:
        verb = "Defines"
    elif strength >= 0.7:
        verb = "Demonstrates"
    else:
        verb = "References"

    # Build summary from key phrases
    phrase_str = ", ".join(key_phrases[:3])
    return f"{verb} {concept_name} via {phrase_str}"

def create_concept_nodes(concept_config: List[Dict]) -> List[Dict]:
    """Generate ConceptNodes from matcher config"""
    concept_nodes = []

    for concept in concept_config:
        node = {
            "type": "concept",
            "id": concept["concept_id"],
            "category": concept["category"],
            "description": concept["description"]
        }
        concept_nodes.append(node)

    return concept_nodes

def create_system_nodes() -> List[Dict]:
    """Create SystemNodes for all discovered systems"""
    systems = [
        {"type": "system", "id": "ggen", "role": "projection_engine"},
        {"type": "system", "id": "mu-kernel", "role": "timing_kernel"},
        {"type": "system", "id": "KNHK", "role": "knowledge_hypergraph"},
        {"type": "system", "id": "CTT", "role": "verification_pipeline"},
        {"type": "system", "id": "AHI", "role": "autonomic_governance"},
        {"type": "system", "id": "CNV", "role": "agent_cli"},
        {"type": "system", "id": "clnrm", "role": "hermetic_testing"},
        {"type": "system", "id": "DFLSS", "role": "optimization_flow"},
        {"type": "system", "id": "nomrg", "role": "graph_overlay"}
    ]
    return systems

def build_supports_edges(evidence_nodes: List[Dict]) -> List[Dict]:
    """Create supports edges: EvidenceNode → ConceptNode"""
    edges = []

    for evidence in evidence_nodes:
        edge = {
            "from": evidence["evidence_id"],
            "to": evidence["concept_id"],
            "kind": "supports",
            "weight": evidence["strength"]
        }
        edges.append(edge)

    return edges

def build_implements_edges(evidence_nodes: List[Dict], interim_data: Dict) -> List[Dict]:
    """Create implements edges: SystemNode → ConceptNode"""
    edges = []

    # Track which systems support which concepts
    system_concept_evidence = {}

    for evidence in evidence_nodes:
        # Infer system from file path
        path = evidence["path"]
        system = infer_system_from_path(path)
        concept_id = evidence["concept_id"]

        key = (system, concept_id)
        if key not in system_concept_evidence:
            system_concept_evidence[key] = []
        system_concept_evidence[key].append(evidence["strength"])

    # Create implements edge if: ≥2 evidence nodes AND max_strength ≥ 0.7
    for (system, concept_id), strengths in system_concept_evidence.items():
        if len(strengths) >= 2 and max(strengths) >= 0.7:
            edge = {
                "from": system,
                "to": concept_id,
                "kind": "implements",
                "weight": max(strengths)
            }
            edges.append(edge)
        elif len(strengths) == 1 and max(strengths) >= 0.9:
            # Single strong evidence also counts
            edge = {
                "from": system,
                "to": concept_id,
                "kind": "implements",
                "weight": max(strengths)
            }
            edges.append(edge)

    return edges

def infer_system_from_path(path: str) -> str:
    """Infer which system a file belongs to based on path"""
    path_lower = path.lower()

    if "ggen-dod" in path_lower and ("kernel" in path_lower or "timing" in path_lower):
        return "mu-kernel"
    elif "ahi_contract" in path_lower or "mape_k" in path_lower:
        return "AHI"
    elif "chicago_tdd" in path_lower:
        return "CTT"
    elif "cleanroom" in path_lower:
        return "clnrm"
    elif "clap-noun-verb" in path_lower or "cnv" in path_lower:
        return "CNV"
    elif "dflss" in path_lower:
        return "DFLSS"
    elif "nomrg" in path_lower or "no-merge" in path_lower:
        return "nomrg"
    elif path_lower.endswith(".ttl") or "ontology" in path_lower:
        return "KNHK"
    else:
        return "ggen"

def build_composed_with_edges(evidence_nodes: List[Dict], interim_data: Dict) -> List[Dict]:
    """Create composed_with edges: SystemNode → SystemNode"""
    edges = []

    # Track which systems appear together in files
    file_systems = {}

    for file_entry in interim_data.get("file_inventory", []):
        systems = file_entry.get("systems", [])
        if len(systems) >= 2:
            path = file_entry.get("path", "")
            if path not in file_systems:
                file_systems[path] = set()
            for system in systems:
                # Normalize system names
                normalized = normalize_system_name(system)
                file_systems[path].add(normalized)

    # Count co-occurrences
    system_pairs = {}
    for systems in file_systems.values():
        systems_list = sorted(list(systems))
        for i, sys1 in enumerate(systems_list):
            for sys2 in systems_list[i+1:]:
                pair = tuple(sorted([sys1, sys2]))
                system_pairs[pair] = system_pairs.get(pair, 0) + 1

    # Create edges for pairs that appear together ≥2 times
    for (sys1, sys2), count in system_pairs.items():
        if count >= 2:
            edge = {
                "from": sys1,
                "to": sys2,
                "kind": "composed_with",
                "weight": min(1.0, count / 5.0)  # Normalize by max expected co-occurrence
            }
            edges.append(edge)

    return edges

def normalize_system_name(system: str) -> str:
    """Normalize system names to match SystemNode IDs"""
    if system == "μ-kernel":
        return "mu-kernel"
    elif system == "MAPE-K":
        return "AHI"
    return system

def generate_graph_structure_txt(graph: Dict) -> str:
    """Generate human-readable graph structure summary"""
    concepts = graph["nodes"]["concepts"]
    evidence = graph["nodes"]["evidence"]
    systems = graph["nodes"]["systems"]
    edges = graph["edges"]

    # Group concepts by category
    concept_by_category = {}
    for concept in concepts:
        cat = concept["category"]
        if cat not in concept_by_category:
            concept_by_category[cat] = []
        concept_by_category[cat].append(concept["id"])

    # Count evidence per system
    system_evidence_count = {}
    for ev in evidence:
        system = infer_system_from_path(ev["path"])
        system_evidence_count[system] = system_evidence_count.get(system, 0) + 1

    # Count edges by kind
    edge_counts = {}
    for edge in edges:
        kind = edge["kind"]
        edge_counts[kind] = edge_counts.get(kind, 0) + 1

    # Count evidence by strength bucket
    strength_buckets = {
        "0.95-1.0": 0,
        "0.85-0.94": 0,
        "0.70-0.84": 0,
        "0.50-0.69": 0,
        "0.30-0.49": 0,
        "0.0-0.29": 0
    }

    for ev in evidence:
        s = ev["strength"]
        if s >= 0.95:
            strength_buckets["0.95-1.0"] += 1
        elif s >= 0.85:
            strength_buckets["0.85-0.94"] += 1
        elif s >= 0.70:
            strength_buckets["0.70-0.84"] += 1
        elif s >= 0.50:
            strength_buckets["0.50-0.69"] += 1
        elif s >= 0.30:
            strength_buckets["0.30-0.49"] += 1
        else:
            strength_buckets["0.0-0.29"] += 1

    # Build output
    lines = [
        "EVIDENCE GRAPH STRUCTURE",
        "=" * 50,
        "",
        f"Concepts: {len(concepts)} total"
    ]

    for cat, concept_ids in sorted(concept_by_category.items()):
        lines.append(f"  - {cat.capitalize()} ({len(concept_ids)}): {', '.join(concept_ids)}")

    lines.append("")
    lines.append(f"Systems: {len(systems)} total")
    for system in systems:
        sys_id = system["id"]
        role = system["role"]
        count = system_evidence_count.get(sys_id, 0)
        lines.append(f"  - {sys_id} ({role}): {count} evidence nodes")

    lines.append("")
    lines.append("Edges:")
    for kind, count in sorted(edge_counts.items()):
        if kind == "supports":
            lines.append(f"  - {kind}: {count} (EvidenceNode → ConceptNode)")
        elif kind == "implements":
            lines.append(f"  - {kind}: {count} (SystemNode → ConceptNode)")
        elif kind == "composed_with":
            lines.append(f"  - {kind}: {count} (SystemNode → SystemNode)")

    lines.append("")
    lines.append("Evidence Strength Distribution:")
    for bucket, count in strength_buckets.items():
        lines.append(f"  - {bucket}: {count} evidence nodes")

    lines.append("")
    lines.append(f"Total Evidence Nodes: {len(evidence)}")
    lines.append(f"Total Edges: {len(edges)}")

    return "\n".join(lines)

def main():
    # Load inputs
    interim_data = load_json("/home/user/ggen/.ggen/evidence_mining_interim.json")
    concept_config = load_json("/home/user/ggen/concept_matcher_config.json")

    # Generate nodes
    print("Generating EvidenceNodes...")
    evidence_nodes = create_evidence_nodes(interim_data)

    print("Creating ConceptNodes...")
    concept_nodes = create_concept_nodes(concept_config)

    print("Creating SystemNodes...")
    system_nodes = create_system_nodes()

    # Generate edges
    print("Building supports edges...")
    supports_edges = build_supports_edges(evidence_nodes)

    print("Building implements edges...")
    implements_edges = build_implements_edges(evidence_nodes, interim_data)

    print("Building composed_with edges...")
    composed_edges = build_composed_with_edges(evidence_nodes, interim_data)

    all_edges = supports_edges + implements_edges + composed_edges

    # Build complete graph
    evidence_graph = {
        "version": "1.0",
        "generated_at": datetime.utcnow().isoformat() + "Z",
        "nodes": {
            "concepts": concept_nodes,
            "evidence": evidence_nodes,
            "systems": system_nodes
        },
        "edges": all_edges
    }

    # Save outputs
    print("Saving evidence_graph.json...")
    save_json("/home/user/ggen/.ggen/evidence_graph.json", evidence_graph)

    print("Saving evidence_nodes.json...")
    save_json("/home/user/ggen/.ggen/evidence_nodes.json", evidence_nodes)

    print("Generating graph_structure.txt...")
    structure_txt = generate_graph_structure_txt(evidence_graph)
    with open("/home/user/ggen/.ggen/graph_structure.txt", "w") as f:
        f.write(structure_txt)

    # Print summary
    print("\n" + "=" * 50)
    print("Evidence Graph construction complete!")
    print(f"  Concepts: {len(concept_nodes)}")
    print(f"  Systems: {len(system_nodes)}")
    print(f"  Evidence Nodes: {len(evidence_nodes)}")
    print(f"  Edges: {len(all_edges)}")
    print(f"    - supports: {len(supports_edges)}")
    print(f"    - implements: {len(implements_edges)}")
    print(f"    - composed_with: {len(composed_edges)}")
    print("=" * 50)

if __name__ == "__main__":
    main()
