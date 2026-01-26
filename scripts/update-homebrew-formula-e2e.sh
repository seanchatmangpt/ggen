#!/usr/bin/env bash
# End-to-end Homebrew formula update script
# Ensures Homebrew version matches crate version and uses static binaries (no compilation)

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_debug() { echo -e "${BLUE}[DEBUG]${NC} $1"; }

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
HOMEBREW_SUBMODULE_DIR="$REPO_ROOT/vendors/homebrew-ggen"

# Parse arguments
DRY_RUN=false
SKIP_TEST=false
SKIP_PUSH=false
VERSION=""

while [[ $# -gt 0 ]]; do
  case $1 in
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --skip-test)
      SKIP_TEST=true
      shift
      ;;
    --skip-push)
      SKIP_PUSH=true
      shift
      ;;
    --version)
      VERSION="$2"
      shift 2
      ;;
    -h|--help)
      echo "Usage: $0 [OPTIONS]"
      echo ""
      echo "Update Homebrew formula to match crate version using static binaries"
      echo ""
      echo "Options:"
      echo "  --version VERSION    Specify version (default: from Cargo.toml)"
      echo "  --dry-run            Show what would be done without making changes"
      echo "  --skip-test          Skip local formula testing"
      echo "  --skip-push          Skip pushing changes to remote"
      echo "  -h, --help           Show this help message"
      echo ""
      exit 0
      ;;
    *)
      log_error "Unknown option: $1"
      exit 1
      ;;
  esac
done

# Get version from argument or Cargo.toml
if [[ -z "$VERSION" ]]; then
  VERSION=$(grep '^version = ' "$REPO_ROOT/Cargo.toml" | head -1 | sed 's/version = "\(.*\)"/\1/')
  if [[ -z "$VERSION" ]]; then
    log_error "Failed to extract version from Cargo.toml"
    exit 1
  fi
fi

log_info "Updating Homebrew formula for version ${VERSION}"

# Step 1: Verify submodule exists
log_info "Step 1: Checking Homebrew submodule..."
if [[ ! -d "$HOMEBREW_SUBMODULE_DIR" ]]; then
  log_error "Homebrew submodule not found at $HOMEBREW_SUBMODULE_DIR"
  log_info "Initializing submodule..."
  if [[ "$DRY_RUN" == "false" ]]; then
    cd "$REPO_ROOT"
    git submodule update --init --recursive vendors/homebrew-ggen || {
      log_error "Failed to initialize submodule. Run: git submodule add https://github.com/seanchatmangpt/homebrew-ggen.git vendors/homebrew-ggen"
      exit 1
    }
  else
    log_debug "Would initialize submodule"
  fi
fi

# Step 2: Verify crate version matches
log_info "Step 2: Verifying crate version matches..."
CRATE_VERSION=$(grep '^version = ' "$REPO_ROOT/Cargo.toml" | head -1 | sed 's/version = "\(.*\)"/\1/')
if [[ "$VERSION" != "$CRATE_VERSION" ]]; then
  log_error "Version mismatch:"
  log_error "  Cargo.toml version: $CRATE_VERSION"
  log_error "  Requested version: $VERSION"
  log_error "Versions must match!"
  exit 1
fi
log_info "✅ Crate version matches: $VERSION"

# Step 3: Check if release exists and get SHA256 checksums
log_info "Step 3: Verifying release assets exist..."
RELEASE_URL="https://github.com/seanchatmangpt/ggen/releases/download/v${VERSION}"

# Check each binary exists
declare -A TARGETS=(
  ["aarch64-apple-darwin"]="macOS ARM64"
  ["x86_64-apple-darwin"]="macOS x86_64"
  ["aarch64-unknown-linux-gnu"]="Linux ARM64"
  ["x86_64-unknown-linux-gnu"]="Linux x86_64"
)

declare -A SHA256_CHECKSUMS

for target in "${!TARGETS[@]}"; do
  ASSET_URL="${RELEASE_URL}/ggen-${target}.tar.gz"
  SHA256_URL="${RELEASE_URL}/ggen-${target}.tar.gz.sha256"
  
  log_debug "Checking ${TARGETS[$target]}..."
  
  if ! curl -s -f -I "$ASSET_URL" > /dev/null 2>&1; then
    log_error "Release asset not found: $ASSET_URL"
    log_error "Make sure the GitHub release v${VERSION} exists and binaries are uploaded"
    exit 1
  fi
  
  # Download SHA256 checksum
  SHA256=$(curl -sL "$SHA256_URL" | cut -d' ' -f1)
  if [[ -z "$SHA256" ]]; then
    log_error "Failed to download SHA256 checksum from $SHA256_URL"
    exit 1
  fi
  
  SHA256_CHECKSUMS[$target]="$SHA256"
  log_info "  ✅ ${TARGETS[$target]}: ${SHA256:0:16}..."
done

# Step 4: Update submodule
log_info "Step 4: Updating Homebrew submodule..."
if [[ "$DRY_RUN" == "false" ]]; then
  cd "$HOMEBREW_SUBMODULE_DIR"
  git fetch origin
  git checkout main
  git pull origin main || log_warn "Could not pull latest changes (may be on different branch)"
else
  log_debug "Would update submodule"
fi

# Step 5: Determine formula file location
FORMULA_FILE="$HOMEBREW_SUBMODULE_DIR/Formula/ggen.rb"
if [[ ! -d "$(dirname "$FORMULA_FILE")" ]]; then
  log_info "Creating Formula directory..."
  if [[ "$DRY_RUN" == "false" ]]; then
    mkdir -p "$(dirname "$FORMULA_FILE")"
  fi
fi

# Step 6: Check current formula version
log_info "Step 6: Checking current formula version..."
if [[ -f "$FORMULA_FILE" ]]; then
  CURRENT_FORMULA_VERSION=$(grep -E '^\s*version\s+"' "$FORMULA_FILE" | head -1 | sed 's/.*version "\(.*\)".*/\1/' || echo "")
  if [[ -n "$CURRENT_FORMULA_VERSION" ]]; then
    log_info "Current formula version: $CURRENT_FORMULA_VERSION"
    if [[ "$CURRENT_FORMULA_VERSION" == "$VERSION" ]]; then
      log_warn "Formula already at version $VERSION"
      read -p "Continue anyway? (y/N) " -n 1 -r
      echo
      if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        log_info "Aborted by user"
        exit 0
      fi
    fi
  fi
else
  log_info "Formula file does not exist, will create new one"
fi

# Step 7: Generate formula content (static binaries only, no compilation)
log_info "Step 7: Generating Homebrew formula (static binaries only)..."
FORMULA_CONTENT=$(cat <<EOF
class Ggen < Formula
  desc "Language-agnostic, graph-aware generator for reproducible projections"
  homepage "https://github.com/seanchatmangpt/ggen"
  version "${VERSION}"
  license "MIT"
  
  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/seanchatmangpt/ggen/releases/download/v${VERSION}/ggen-aarch64-apple-darwin.tar.gz"
      sha256 "${SHA256_CHECKSUMS[aarch64-apple-darwin]}"
    else
      url "https://github.com/seanchatmangpt/ggen/releases/download/v${VERSION}/ggen-x86_64-apple-darwin.tar.gz"
      sha256 "${SHA256_CHECKSUMS[x86_64-apple-darwin]}"
    end
  end
  
  on_linux do
    if Hardware::CPU.arm?
      url "https://github.com/seanchatmangpt/ggen/releases/download/v${VERSION}/ggen-aarch64-unknown-linux-gnu.tar.gz"
      sha256 "${SHA256_CHECKSUMS[aarch64-unknown-linux-gnu]}"
    else
      url "https://github.com/seanchatmangpt/ggen/releases/download/v${VERSION}/ggen-x86_64-unknown-linux-gnu.tar.gz"
      sha256 "${SHA256_CHECKSUMS[x86_64-unknown-linux-gnu]}"
    end
  end
  
  def install
    bin.install "ggen"
    generate_completions_from_executable(bin/"ggen", "completion")
  end
  
  test do
    assert_match "ggen", shell_output("\#{bin}/ggen --version")
  end
end
EOF
)

if [[ "$DRY_RUN" == "true" ]]; then
  log_info "Formula content (dry-run):"
  echo "$FORMULA_CONTENT"
else
  echo "$FORMULA_CONTENT" > "$FORMULA_FILE"
  log_info "✅ Formula written to $FORMULA_FILE"
fi

# Step 8: Test formula locally (if on macOS and not skipped)
if [[ "$SKIP_TEST" == "false" ]] && [[ "$OSTYPE" == "darwin"* ]]; then
  log_info "Step 8: Testing formula locally..."
  
  if command -v brew >/dev/null 2>&1; then
    # Test formula syntax
    log_info "  Testing formula syntax..."
    if brew audit --strict "$FORMULA_FILE" 2>&1 | grep -q "Error"; then
      log_error "Formula syntax check failed!"
      brew audit --strict "$FORMULA_FILE"
      exit 1
    fi
    log_info "  ✅ Formula syntax is valid"
    
    # Test formula install (dry-run)
    log_info "  Testing formula install (dry-run)..."
    if brew install --build-from-source --dry-run "$FORMULA_FILE" >/dev/null 2>&1; then
      log_info "  ✅ Formula install test passed"
    else
      log_warn "  ⚠️  Formula install test had warnings (may be expected)"
    fi
  else
    log_warn "  Homebrew not found, skipping local tests"
  fi
elif [[ "$SKIP_TEST" == "false" ]]; then
  log_info "Step 8: Skipping local test (not on macOS)"
fi

# Step 9: Commit changes
if [[ "$DRY_RUN" == "false" ]]; then
  log_info "Step 9: Committing changes..."
  cd "$HOMEBREW_SUBMODULE_DIR"
  
  # Check if there are changes
  if git diff --quiet "$FORMULA_FILE" 2>/dev/null && git diff --cached --quiet 2>/dev/null; then
    log_info "No changes to commit"
  else
    git add "$FORMULA_FILE"
    
    COMMIT_MSG="chore: update ggen formula to v${VERSION}

- macOS ARM64:  ${SHA256_CHECKSUMS[aarch64-apple-darwin]}
- macOS x86_64: ${SHA256_CHECKSUMS[x86_64-apple-darwin]}
- Linux ARM64:  ${SHA256_CHECKSUMS[aarch64-unknown-linux-gnu]}
- Linux x86_64: ${SHA256_CHECKSUMS[x86_64-unknown-linux-gnu]}

Crate version: ${VERSION}
Static binaries only (no compilation required)"
    
    git commit -m "$COMMIT_MSG"
    log_info "✅ Changes committed"
    
    # Step 10: Push to remote (if not skipped)
    if [[ "$SKIP_PUSH" == "false" ]]; then
      log_info "Step 10: Pushing to remote..."
      # Check if remote URL needs authentication token
      REMOTE_URL=$(git remote get-url origin 2>/dev/null || echo "")
      
      # Handle authentication - prefer SSH, fallback to HTTPS with token
      if [[ "$REMOTE_URL" == *"x-access-token"* ]] || [[ -n "${GITHUB_TOKEN:-}" ]]; then
        log_debug "Using token-based authentication"
        if [[ -n "${GITHUB_TOKEN:-}" ]] && [[ "$REMOTE_URL" == *"https://"* ]] && [[ "$REMOTE_URL" != *"x-access-token"* ]]; then
          # Update remote URL to use token
          git remote set-url origin "https://x-access-token:${GITHUB_TOKEN}@github.com/seanchatmangpt/homebrew-ggen.git"
        fi
      elif [[ "$REMOTE_URL" == *"https://"* ]] && [[ "$REMOTE_URL" != *"@"* ]]; then
        log_warn "Remote URL may need authentication. If push fails:"
        log_warn "  - Set GITHUB_TOKEN environment variable, or"
        log_warn "  - Configure SSH keys, or"
        log_warn "  - Use: git remote set-url origin git@github.com:seanchatmangpt/homebrew-ggen.git"
      fi
      
      if git push origin main; then
        log_info "✅ Changes pushed to remote"
      else
        log_error "Failed to push. Make sure you have:"
        log_error "  1. Write access to the repository"
        log_error "  2. Git credentials configured:"
        log_error "     - SSH keys set up, or"
        log_error "     - GITHUB_TOKEN environment variable set, or"
        log_error "     - Git credential helper configured"
        log_error "  3. Remote branch is up to date (run: git pull origin main)"
        log_error ""
        log_error "To push manually later:"
        log_error "  cd $HOMEBREW_SUBMODULE_DIR"
        log_error "  git push origin main"
        exit 1
      fi
    else
      log_info "Step 10: Skipping push (--skip-push flag)"
    fi
  fi
else
  log_info "Step 9: Dry-run mode - would commit and push changes"
fi

# Step 11: Update main repo submodule reference (optional, script-based only)
if [[ "$DRY_RUN" == "false" ]]; then
  log_info "Step 11: Updating submodule reference in main repo..."
  cd "$REPO_ROOT"
  git add vendors/homebrew-ggen
  if git diff --cached --quiet vendors/homebrew-ggen 2>/dev/null; then
    log_info "No submodule reference changes"
  else
    log_info "Submodule reference updated (staged, not committed)"
    log_info "To commit: git commit -m 'chore: update homebrew-ggen submodule to v${VERSION}'"
    log_info "To push: git push origin <branch>"
  fi
fi

# Summary
log_info ""
log_info "✅ Homebrew formula update complete!"
log_info ""
log_info "Version: v${VERSION}"
log_info "Formula: $FORMULA_FILE"
log_info ""
log_info "📦 Users can install with:"
log_info "   brew tap seanchatmangpt/ggen"
log_info "   brew install ggen"
log_info ""
log_info "To validate the formula locally, run:"
log_info "   ./scripts/validate-homebrew-formula.sh"
log_info ""
if [[ "$DRY_RUN" == "true" ]]; then
  log_warn "This was a dry-run. No changes were made."
fi

