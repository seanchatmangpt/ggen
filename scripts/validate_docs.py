#!/usr/bin/env python3
import os
import sys
import re
import json
import urllib.parse

# Try importing rdflib
try:
    import rdflib
    HAS_RDFLIB = True
except ImportError:
    HAS_RDFLIB = False

# Directories to exclude from scanning
EXCLUDED_DIRS = {
    '.git', 'target', '.agents', '.venv_shacl', '.antigravitycli',
    '.gemini', 'node_modules', 'venv', 'build', '.cargo', '.ggen',
    '.githooks', '.github', '.helix', 'analysis', '.specify', 'scripts',
    '.agent-admissibility', '.claude', '.claude-plugin', '.metrics',
    'artifacts', 'benches', 'boilerplate', 'cgen', 'crates', 'deploy',
    'docker', 'figures', 'fusion-thesis', 'ggen-skills', 'graphs',
    'marketplace', 'my-ontology-project', 'ocel', 'ontologies',
    'ontology_catalogue', 'phd-thesis', 'playground', 'plugins',
    'queries', 'receipts', 'registry', 'research-paper', 'schema',
    'semconv', 'specs', 'src', 'tai-erlang-autonomics', 'tests',
    'tools', 'vendors', 'archive', 'wip', 'preserved', 'metrics',
    'receipts', 'thesis', 'audits', 'crate-audits',
    'adr', 'agent', 'api', 'architecture', 'ark-covenant', 'automation',
    'cli', 'dflss', 'dx', 'features', 'gall', 'getting-started',
    'ggen-v6-thesis', 'interop', 'jira', 'mcp', 'mcp-rdf', 'mcpp',
    'open-ontologies', 'performance', 'performance-dashboard',
    'post-chatman', 'proof-cycles', 'research', 'security',
    'superpowers', 'swarm', 'templates', 'testing', 'textbook',
    'troubleshooting', 'validation'
}

# Files to exclude from scanning (at the root level)
EXCLUDED_FILES = {
    'CHANGELOG.md', 'MARKETPLACE_AUDIT_REPORT.md', 'PATH_A_EVIDENCE_INDEX.md',
    'PATH_A_MERGE_CHECKLIST.md', 'EVIDENCE_SYNTHESIS.md', 'DOCUMENTATION_AUDIT_REPORT.md',
    'FIXTURE_AUDIT_REPORT.md', 'FIXTURE_BLOCKING_ISSUES.md', 'FIXTURE_SETUP_QUICK_REFERENCE.md',
    'FUSION_THESIS.md', 'IMPLEMENTATION_SUMMARY.md', 'ROLLBACK.md',
    'PhD_THESIS_GGEN_ONTOLOGY_SYNTHESIS.md', 'MANIFESTO.md', 'LSP-ARD-PRD.md',
    'DEFINITION_OF_DONE_DELIVERY.md', 'DOCTEST_VALIDATION_PROGRESS.md', 'MERGE_READINESS_AUDIT.md',
    'PHASE4_IMPLEMENTATION.md', 'PHASE5_WAVE2_PLANNING_AUDIT.md', 'POST_RELEASE_CHECKLIST.md',
    'SKILLS.md'
}

# Active documentation files to scan (relative to repo root)
ACTIVE_FILES = {
    'README.md',
    'CONTRIBUTING.md',
    'AGENTS.md',
    'SECURITY.md',
    'CONTRIBUTORS.md',
    'docs/tutorials/tutorials.md',
    'docs/how-to/how_to_guides.md',
    'docs/reference/reference.md',
    'docs/explanation/explanation.md',
    'docs/explanation/control-plane-security.md',
    'docs/explanation/governance-and-proof-gates.md',
    'docs/explanation/ontology-architecture.md',
    'docs/explanation/vision-2030-best-practices.md'
}

def slugify(text):
    # Remove HTML tags
    text = re.sub(r'<[^>]+>', '', text)
    # Lowercase
    text = text.lower()
    # Replace spaces/hyphens with hyphens, remove other non-alphanumeric chars (keep underscores, hyphens)
    text = re.sub(r'[^a-z0-9\s\-_]', '', text)
    text = re.sub(r'[\s\-]+', '-', text)
    return text.strip('-')

def get_header_anchor(line):
    match = re.match(r'^#+\s+(.+)$', line.strip())
    if not match:
        return None
    title = match.group(1).strip()
    title = re.sub(r'\s+#+$', '', title).strip()
    title = re.sub(r'\[([^\]]+)\]\([^)]+\)', r'\1', title)
    title = title.replace('`', '')
    return slugify(title)

def clean_url(url):
    url = url.strip()
    if url.startswith('<') and url.endswith('>'):
        url = url[1:-1].strip()
    return urllib.parse.unquote(url)

def is_relative_link(url):
    url = url.strip()
    if not url:
        return False
    if url.startswith('#'):
        return False
    if re.match(r'^[a-zA-Z][a-zA-Z0-9\+\-\.]*:', url):
        return False
    return True

# Simple Turtle Validation Fallback
def validate_turtle_fallback(content):
    stack = []
    matching = {')': '(', ']': '[', '}': '{'}
    in_single_quote = False
    in_double_quote = False
    in_triple_double_quote = False
    in_triple_single_quote = False
    in_uri = False
    
    i = 0
    length = len(content)
    while i < length:
        char = content[i]
        
        # Handle escaping
        if char == '\\':
            i += 2
            continue
            
        # Handle triple double quote
        if not in_single_quote and not in_triple_single_quote and not in_double_quote and content[i:i+3] == '"""':
            in_triple_double_quote = not in_triple_double_quote
            i += 3
            continue
        # Handle triple single quote
        if not in_double_quote and not in_triple_double_quote and not in_single_quote and content[i:i+3] == "'''":
            in_triple_single_quote = not in_triple_single_quote
            i += 3
            continue
            
        # Handle normal quotes
        if not in_triple_double_quote and not in_triple_single_quote:
            if char == '"' and not in_single_quote:
                in_double_quote = not in_double_quote
                i += 1
                continue
            if char == "'" and not in_double_quote:
                in_single_quote = not in_single_quote
                i += 1
                continue
                
        if in_triple_double_quote or in_triple_single_quote or in_double_quote or in_single_quote:
            i += 1
            continue
            
        # Handle comment
        if char == '#' and not in_uri:
            while i < length and content[i] != '\n':
                i += 1
            continue
            
        # Handle URI
        if char == '<' and not in_uri:
            in_uri = True
            i += 1
            continue
        if char == '>' and in_uri:
            in_uri = False
            i += 1
            continue
            
        if in_uri:
            if char in ' \t\n"{}|^`':
                return False, f"Invalid character '{char}' in URI"
            i += 1
            continue
            
        # Braces matching
        if char in '([{':
            stack.append((char, i))
        elif char in ')]}':
            if not stack:
                return False, f"Unmatched closing character '{char}' at index {i}"
            top, idx = stack.pop()
            if matching[char] != top:
                return False, f"Mismatched characters: '{top}' at index {idx} and '{char}' at index {i}"
                
        i += 1
        
    if in_triple_double_quote:
        return False, "Unclosed triple double quote ('''...''')"
    if in_triple_single_quote:
        return False, "Unclosed triple single quote ('''...''')"
    if in_double_quote:
        return False, 'Unclosed double quote ("...")'
    if in_single_quote:
        return False, "Unclosed single quote ('...')"
    if in_uri:
        return False, "Unclosed URI (<...>)"
    if stack:
        top, idx = stack[-1]
        return False, f"Unclosed opening character '{top}' at index {idx}"
        
    # Basic prefix check
    lines = content.splitlines()
    for line_num, line in enumerate(lines, 1):
        line = line.strip()
        if not line:
            continue
        if line.startswith('@prefix'):
            match = re.match(r'^@prefix\s+[a-zA-Z0-9_\-]*:\s*<[^>]+>\s*\.$', line)
            if not match:
                return False, f"Line {line_num}: Invalid @prefix syntax: '{line}'"
        elif line.startswith('PREFIX'):
            match = re.match(r'^PREFIX\s+[a-zA-Z0-9_\-]*:\s*<[^>]+>$', line)
            if not match:
                return False, f"Line {line_num}: Invalid PREFIX syntax: '{line}'"
                
    return True, None

def validate_code_block(lang, code, line_num, file_path):
    if lang in ('json', 'json-ld'):
        try:
            json.loads(code)
            return True, None
        except Exception as e:
            return False, f"JSON validation failed: {str(e)}"
    elif lang in ('turtle', 'ttl'):
        if HAS_RDFLIB:
            g = rdflib.Graph()
            try:
                g.parse(data=code, format="turtle")
                return True, None
            except Exception as e:
                err_str = str(e)
                if "namespace" in err_str.lower() or "prefix" in err_str.lower() or "not bound" in err_str.lower():
                    # Prefix/Namespace error: check if general syntax is ok using fallback
                    success, fb_err = validate_turtle_fallback(code)
                    if success:
                        return True, None
                return False, f"Turtle validation failed (rdflib): {err_str}"
        else:
            success, err = validate_turtle_fallback(code)
            if not success:
                return False, f"Turtle validation failed (fallback): {err}"
            return True, None
    return True, None

def resolve_link(link_path, current_file_path, repo_root):
    if link_path.startswith('/'):
        # Repo root relative
        resolved = os.path.normpath(os.path.join(repo_root, link_path.lstrip('/')))
    else:
        # Relative to current file
        dir_path = os.path.dirname(current_file_path)
        resolved = os.path.normpath(os.path.join(dir_path, link_path))
    return resolved

def parse_markdown_file(file_path):
    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
        
    lines = content.splitlines()
    
    anchors = set()
    anchor_counts = {}
    code_blocks = [] # (lang, code, start_line)
    links = [] # (link_path, line_num, link_text)
    placeholders = [] # (placeholder_type, match_text, line_num)
    
    placeholder_patterns = [
        (re.compile(r'\btodo\b', re.IGNORECASE), "TODO"),
        (re.compile(r'\bfixme\b', re.IGNORECASE), "FIXME"),
        (re.compile(r'\btbd\b', re.IGNORECASE), "TBD"),
        (re.compile(r'\btktk\b', re.IGNORECASE), "TKTK"),
        (re.compile(r'\?\?\?'), "???"),
        (re.compile(r'<placeholder>', re.IGNORECASE), "<placeholder>"),
    ]
    
    in_code_block = False
    code_block_lang = ""
    code_block_lines = []
    code_block_start = 0
    
    # Collect HTML id/name attributes
    html_anchors = re.findall(r'(?:id|name)=["\']([^"\']+)["\']', content)
    for ha in html_anchors:
        anchors.add(slugify(ha))
        
    for line_num, line in enumerate(lines, 1):
        stripped = line.strip()
        
        # Code block tracking
        if stripped.startswith('```'):
            if in_code_block:
                code_blocks.append((code_block_lang, "\n".join(code_block_lines), code_block_start))
                in_code_block = False
                code_block_lang = ""
                code_block_lines = []
            else:
                in_code_block = True
                code_block_start = line_num
                lang_match = re.match(r'^```\s*(\S+)', stripped)
                if lang_match:
                    code_block_lang = lang_match.group(1).lower()
                else:
                    code_block_lang = ""
            continue
            
        if in_code_block:
            code_block_lines.append(line)
            continue
            
        # Outside code blocks:
        # 1. Header extraction
        header_anchor = get_header_anchor(line)
        if header_anchor:
            if header_anchor in anchor_counts:
                count = anchor_counts[header_anchor]
                anchor_counts[header_anchor] += 1
                header_anchor = f"{header_anchor}-{count}"
            else:
                anchor_counts[header_anchor] = 1
            anchors.add(header_anchor)
            
        # 2. Placeholders search
        for pattern, p_type in placeholder_patterns:
            for match in pattern.finditer(line):
                placeholders.append((p_type, match.group(0), line_num))
                
        # 3. Links search
        line_no_inline_code = re.sub(r'`[^`]+`', '', line)
        
        # Inline links: [text](url)
        for match in re.finditer(r'\[([^\]]*)\]\(([^)]+)\)', line_no_inline_code):
            text = match.group(1)
            url = clean_url(match.group(2))
            if is_relative_link(url) or url.startswith('#'):
                links.append((url, line_num, text))
                
        # Reference links: [ref]: url
        ref_match = re.match(r'^\[([^\]]+)\]:\s*(\S+)', line_no_inline_code.strip())
        if ref_match:
            url = clean_url(ref_match.group(2))
            if is_relative_link(url) or url.startswith('#'):
                links.append((url, line_num, f"Reference definition for [{ref_match.group(1)}]"))
                
    return anchors, code_blocks, links, placeholders

def main():
    repo_root = os.path.abspath(os.getcwd())
    
    # Walk and collect md files
    md_files = []
    for root, dirs, files in os.walk(repo_root):
        if root == repo_root:
            dirs[:] = [d for d in dirs if d not in EXCLUDED_DIRS and d not in ('examples', 'templates')]
        else:
            dirs[:] = [d for d in dirs if d not in EXCLUDED_DIRS]
        for file in files:
            if root == repo_root and file in EXCLUDED_FILES:
                continue
            if file.endswith('.md'):
                rel_path = os.path.relpath(os.path.join(root, file), repo_root)
                if rel_path in ACTIVE_FILES:
                    md_files.append(os.path.join(root, file))
                
    print(f"Scanning {len(md_files)} markdown files in {repo_root}...")
    
    file_anchors = {}
    file_data = {}
    
    # First pass: parse files and cache anchors
    for file_path in md_files:
        try:
            anchors, code_blocks, links, placeholders = parse_markdown_file(file_path)
            file_anchors[file_path] = anchors
            file_data[file_path] = (code_blocks, links, placeholders)
        except Exception as e:
            print(f"Error parsing {file_path}: {e}", file=sys.stderr)
            
    # Second pass: Validate
    errors_found = False
    
    for file_path in md_files:
        rel_path = os.path.relpath(file_path, repo_root)
        if file_path not in file_data:
            continue
        code_blocks, links, placeholders = file_data[file_path]
        
        # A. Forbidden placeholders
        if placeholders:
            print(f"\n[PLACEHOLDERS] in {rel_path}:")
            for p_type, text, line_num in placeholders:
                print(f"  Line {line_num}: Found forbidden placeholder {p_type}: '{text}'")
                errors_found = True
                
        # B. Code block syntax validation
        for lang, code, line_num in code_blocks:
            if lang in ('json', 'json-ld', 'turtle', 'ttl'):
                success, err_msg = validate_code_block(lang, code, line_num, file_path)
                if not success:
                    print(f"\n[SYNTAX ERROR] in {rel_path} (Line {line_num}, lang={lang}):")
                    print(f"  {err_msg}")
                    errors_found = True
                    
        # C. Relative links and anchors
        for link, line_num, link_text in links:
            if link.startswith('#'):
                anchor = slugify(link[1:])
                current_anchors = file_anchors.get(file_path, set())
                if anchor not in current_anchors:
                    print(f"\n[LINK ERROR] in {rel_path} (Line {line_num}):")
                    print(f"  Internal anchor '{link}' not found in this file.")
                    errors_found = True
                continue
                
            path_part, _, anchor_part = link.partition('#')
            path_part, _, _ = path_part.partition('?')
            
            resolved_path = resolve_link(path_part, file_path, repo_root)
            
            if not os.path.exists(resolved_path):
                print(f"\n[LINK ERROR] in {rel_path} (Line {line_num}):")
                print(f"  Link to '{link}' is broken. Resolved path '{os.path.relpath(resolved_path, repo_root)}' does not exist.")
                errors_found = True
            else:
                if resolved_path.endswith('.md') and anchor_part:
                    anchor_slug = slugify(anchor_part)
                    target_anchors = file_anchors.get(resolved_path)
                    if target_anchors is None:
                        try:
                            target_anchors, _, _, _ = parse_markdown_file(resolved_path)
                            file_anchors[resolved_path] = target_anchors
                        except Exception:
                            target_anchors = set()
                            
                    if anchor_slug not in target_anchors:
                        print(f"\n[LINK ERROR] in {rel_path} (Line {line_num}):")
                        print(f"  Anchor '#{anchor_part}' not found in target file '{os.path.relpath(resolved_path, repo_root)}'.")
                        errors_found = True
                        
    if errors_found:
        print("\n❌ Documentation validation failed.", file=sys.stderr)
        sys.exit(1)
    else:
        print("\n✅ All documentation checks passed.")
        sys.exit(0)

if __name__ == '__main__':
    main()
