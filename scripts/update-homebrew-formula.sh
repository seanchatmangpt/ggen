#!/usr/bin/env bash
# Update Homebrew formula with SHA256 checksums from GitHub release

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Get version from argument or Cargo.toml
VERSION=${1:-$(grep '^version = ' Cargo.toml | head -1 | sed 's/version = "\(.*\)"/\1/')}

log_info "Updating Homebrew formula for version ${VERSION}"

# Check if release exists
RELEASE_URL="https://github.com/seanchatmangpt/ggen/releases/download/v${VERSION}"
if ! curl -s -f -I "${RELEASE_URL}/ggen-aarch64-apple-darwin.tar.gz" > /dev/null; then
    log_error "Release v${VERSION} not found at ${RELEASE_URL}"
    log_error "Make sure the GitHub release exists and binaries are uploaded"
    exit 1
fi

log_info "Downloading SHA256 checksums from release..."

# Download SHA256 checksums
SHA_AARCH64_DARWIN=$(curl -sL "${RELEASE_URL}/ggen-aarch64-apple-darwin.tar.gz.sha256" | cut -d' ' -f1)
SHA_X86_64_DARWIN=$(curl -sL "${RELEASE_URL}/ggen-x86_64-apple-darwin.tar.gz.sha256" | cut -d' ' -f1)
SHA_X86_64_LINUX=$(curl -sL "${RELEASE_URL}/ggen-x86_64-unknown-linux-gnu.tar.gz.sha256" | cut -d' ' -f1)
SHA_AARCH64_LINUX=$(curl -sL "${RELEASE_URL}/ggen-aarch64-unknown-linux-gnu.tar.gz.sha256" | cut -d' ' -f1)

# Validate checksums
if [[ -z "$SHA_AARCH64_DARWIN" ]] || [[ -z "$SHA_X86_64_DARWIN" ]] || [[ -z "$SHA_X86_64_LINUX" ]] || [[ -z "$SHA_AARCH64_LINUX" ]]; then
    log_error "Failed to download one or more SHA256 checksums"
    exit 1
fi

log_info "Downloaded checksums:"
echo "  macOS ARM64:  ${SHA_AARCH64_DARWIN}"
echo "  macOS x86_64: ${SHA_X86_64_DARWIN}"
echo "  Linux ARM64:  ${SHA_AARCH64_LINUX}"
echo "  Linux x86_64: ${SHA_X86_64_LINUX}"

# Clone or update homebrew-tap
HOMEBREW_TAP_DIR="/tmp/homebrew-tap-$$"
log_info "Cloning homebrew-tap repository..."
git clone https://github.com/seanchatmangpt/homebrew-tap.git "$HOMEBREW_TAP_DIR"
cd "$HOMEBREW_TAP_DIR"

# Update formula
FORMULA_FILE="Formula/ggen.rb"
log_info "Updating ${FORMULA_FILE}..."

# Create updated formula
cat > "$FORMULA_FILE" <<EOF
class Ggen < Formula
  desc "Language-agnostic, graph-aware generator for reproducible projections"
  homepage "https://github.com/seanchatmangpt/ggen"
  version "${VERSION}"
  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/seanchatmangpt/ggen/releases/download/v${VERSION}/ggen-aarch64-apple-darwin.tar.gz"
      sha256 "${SHA_AARCH64_DARWIN}"
    else
      url "https://github.com/seanchatmangpt/ggen/releases/download/v${VERSION}/ggen-x86_64-apple-darwin.tar.gz"
      sha256 "${SHA_X86_64_DARWIN}"
    end
  end
  on_linux do
    if Hardware::CPU.arm?
      url "https://github.com/seanchatmangpt/ggen/releases/download/v${VERSION}/ggen-aarch64-unknown-linux-gnu.tar.gz"
      sha256 "${SHA_AARCH64_LINUX}"
    else
      url "https://github.com/seanchatmangpt/ggen/releases/download/v${VERSION}/ggen-x86_64-unknown-linux-gnu.tar.gz"
      sha256 "${SHA_X86_64_LINUX}"
    end
  end
  def install
    bin.install "ggen"
    generate_completions_from_executable(bin/"ggen", "completion")
  end
  test do
    assert_match "ggen", shell_output("#{bin}/ggen --version")
  end
end
EOF

# Commit and push
log_info "Committing changes..."
git add "$FORMULA_FILE"
git commit -m "chore: update ggen formula to v${VERSION}

- macOS ARM64:  ${SHA_AARCH64_DARWIN}
- macOS x86_64: ${SHA_X86_64_DARWIN}
- Linux ARM64:  ${SHA_AARCH64_LINUX}
- Linux x86_64: ${SHA_X86_64_LINUX}"

log_info "Pushing to GitHub..."
git push origin main

# Cleanup
cd -
rm -rf "$HOMEBREW_TAP_DIR"

log_info "✅ Homebrew formula updated successfully!"
log_info "Users can now install with: brew install seanchatmangpt/tap/ggen"
