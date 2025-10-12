#!/bin/bash

# Find panic points in production code (excluding tests)

for file in $(find cli/src ggen-core/src ggen-ai/src -name "*.rs" -type f | grep -v test); do
  # Skip files with #[cfg(test)] module
  if grep -q "^#\[cfg(test)\]" "$file" 2>/dev/null; then
    continue
  fi

  # Count panic points before any #[cfg(test)] line
  count=0
  in_test_section=false

  while IFS= read -r line; do
    if [[ "$line" == "#[cfg(test)]" ]]; then
      in_test_section=true
      break
    fi

    if echo "$line" | grep -q "\.expect(\|\.unwrap()"; then
      ((count++))
    fi
  done < "$file"

  if [ "$count" -gt 0 ]; then
    echo "$count $file"
  fi
done | sort -rn
