#!/usr/bin/env python3
import os
import re

# Find all Rust files in src
for root, dirs, files in os.walk('/Users/sac/ggen/ggen-mcp/src'):
    for file in files:
        if file.endswith('.rs'):
            filepath = os.path.join(root, file)

            with open(filepath, 'r') as f:
                content = f.read()

            # Check if file uses DateTime<Utc> or Utc::now()
            needs_chrono = ('DateTime<Utc>' in content or 'Utc::now()' in content)
            has_chrono = re.search(r'^use chrono::\{', content, re.MULTILINE)

            if needs_chrono and not has_chrono:
                print(f"Adding chrono import to {filepath}")
                # Add import after any existing use statements or at the top
                use_pattern = r'((?:^use .*;\n)+)'
                match = re.search(use_pattern, content, re.MULTILINE)

                if match:
                    # Add after existing imports
                    content = content[:match.end()] + 'use chrono::{DateTime, Utc};\n' + content[match.end():]
                else:
                    # Add at the very beginning after comments
                    lines = content.split('\n')
                    insert_pos = 0
                    for i, line in enumerate(lines):
                        if not line.strip().startswith('//') and not line.strip().startswith('/*') and line.strip():
                            insert_pos = i
                            break
                    lines.insert(insert_pos, 'use chrono::{DateTime, Utc};')
                    content = '\n'.join(lines)

                with open(filepath, 'w') as f:
                    f.write(content)

print("Done!")
