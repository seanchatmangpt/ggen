#!/bin/bash
# Fix Error constructor calls in marketplace domain files

FILES=(
  "cli/src/domain/marketplace/search.rs"
  "cli/src/domain/marketplace/install.rs"
  "cli/src/domain/marketplace/list.rs"
  "cli/src/domain/marketplace/update.rs"
  "cli/src/domain/marketplace/publish.rs"
)

for file in "${FILES[@]}"; do
  if [ -f "$file" ]; then
    echo "Fixing $file..."

    # Fix IoError with source and path
    perl -i -p0e 's/ggen_utils::error::Error::IoError\s*\{\s*source:\s*([^,]+),\s*path:\s*([^}]+)\}/ggen_utils::error::Error::new(&format!("IO error: {}", $1))/gs' "$file"

    # Fix ProcessingError with message and context
    perl -i -p0e 's/ggen_utils::error::Error::ProcessingError\s*\{\s*message:\s*([^,]+),\s*context:\s*([^}]+)\}/ggen_utils::error::Error::with_context(\&$1, \&$2)/gs' "$file"

    echo "✓ Fixed $file"
  fi
done

echo "All files fixed!"
