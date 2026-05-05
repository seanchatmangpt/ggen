import os
import re

# Get all ggen.toml files in examples/ and marketplace/packages/
files = os.popen('find examples marketplace/packages -name "ggen.toml" -not -path "*/.*"').read().splitlines()

def refactor(path):
    with open(path, 'r') as f:
        content = f.read()
    
    # Update version to 26.5.4
    content = re.sub(r'version = "[^"]+"', 'version = "26.5.4"', content)
    
    # Handle [[generation.rules]] and [[rules]]
    if '[[generation.rules]]' in content or '[[rules]]' in content:
        # Find all rule blocks
        # This matches from [[...rules]] until the next [[...rules]], the next [section], or end of file
        rule_pattern = re.compile(r'\[\[(?:generation\.)?rules\]\]\n(.*?)(?=\n\[\[|\n\[|\Z)', re.DOTALL)
        
        rule_matches = list(rule_pattern.finditer(content))
        if rule_matches:
            rules = []
            for match in rule_matches:
                block = match.group(1).strip()
                # Parse block into key-value pairs, taking care of multi-line strings
                # This is a bit naive but should work for most ggen.toml
                lines = block.split('\n')
                rule_dict = {}
                current_key = None
                current_val = ""
                in_multiline = False
                
                for line in lines:
                    stripped = line.strip()
                    if not in_multiline:
                        if not stripped or stripped.startswith('#'):
                            continue
                        if '=' in stripped:
                            k, v = stripped.split('=', 1)
                            k = k.strip()
                            v = v.strip()
                            if v.startswith('"""') and not (v.endswith('"""') and len(v) > 3):
                                in_multiline = True
                                current_key = k
                                current_val = v + '\n'
                            else:
                                # Simple fix for strings to objects for query/template
                                if k in ['query', 'template'] and v.startswith('"') and v.endswith('"'):
                                    v = '{ file = ' + v + ' }'
                                rule_dict[k] = v
                    else:
                        current_val += line + '\n'
                        if '"""' in line:
                            in_multiline = False
                            rule_dict[current_key] = current_val.strip()
                
                # Format as inline TOML object
                items = [f'{k} = {v}' for k, v in rule_dict.items()]
                rules.append('{ ' + ', '.join(items) + ' }')
            
            # Remove old rule blocks
            content = rule_pattern.sub('', content)
            
            # Remove any trailing empty generation.rules headers if any
            content = content.replace('[[generation.rules]]', '')
            content = content.replace('[[rules]]', '')
            
            # Construct rules array
            rules_text = 'rules = [\n    ' + ',\n    '.join(rules) + '\n]'
            
            # Insert into [generation]
            if '[generation]' in content:
                # Replace existing rules = [] if any
                if re.search(r'\[generation\].*?rules = \[\]', content, re.DOTALL):
                    content = re.sub(r'(\[generation\].*?)rules = \[\]', r'\1' + rules_text, content, flags=re.DOTALL)
                else:
                    content = re.sub(r'(\[generation\])', r'\1\n' + rules_text, content)
            else:
                content += f'\n[generation]\n{rules_text}\n'

    # Cleanup extra newlines
    content = re.sub(r'\n{3,}', '\n\n', content)
    
    with open(path, 'w') as f:
        f.write(content)

for f in files:
    try:
        refactor(f)
        print(f"Processed {f}")
    except Exception as e:
        print(f"Error processing {f}: {e}")
