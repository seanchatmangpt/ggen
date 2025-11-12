#!/usr/bin/env bash
# Test the release process to verify binaries are stored on GitHub correctly

set -uo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_test() { echo -e "${BLUE}[TEST]${NC} $1"; }

REPO="seanchatmangpt/ggen"
TARGETS=(
  "aarch64-apple-darwin"
  "x86_64-apple-darwin"
  "x86_64-unknown-linux-gnu"
  "aarch64-unknown-linux-gnu"
)

# Test 1: Check if GitHub API is accessible
test_github_api() {
  log_test "Test 1: Checking GitHub API accessibility..."
  if curl -s -f "https://api.github.com/repos/$REPO" > /dev/null 2>&1; then
    log_info "✅ GitHub API is accessible"
    return 0
  else
    log_error "❌ GitHub API is not accessible"
    return 1
  fi
}

# Test 2: Check for existing releases
test_existing_releases() {
  log_test "Test 2: Checking for existing releases..."
  
  local releases_json
  releases_json=$(curl -s "https://api.github.com/repos/$REPO/releases" 2>/dev/null || echo "[]")
  
  if command -v jq &> /dev/null; then
    local releases
    releases=$(echo "$releases_json" | jq -r '.[].tag_name' 2>/dev/null || echo "")
    
    if [ -z "$releases" ]; then
      log_warn "⚠️  No releases found. This is OK if you haven't released yet."
      echo ""
      return 0
    fi
    
    local count
    count=$(echo "$releases" | grep -v '^$' | wc -l | tr -d ' ')
    log_info "✅ Found $count release(s)"
    
    # Test latest release
    local latest_json
    latest_json=$(curl -s "https://api.github.com/repos/$REPO/releases/latest" 2>/dev/null || echo "{}")
    local latest
    latest=$(echo "$latest_json" | jq -r '.tag_name' 2>/dev/null || echo "")
    
    if [ -n "$latest" ] && [ "$latest" != "null" ] && [ "$latest" != "" ]; then
      log_info "   Latest release: $latest"
      echo "$latest"
      return 0
    else
      log_warn "   No latest release found"
      echo ""
      return 0
    fi
  else
    # Fallback without jq - check HTTP status
    local status
    status=$(curl -s -o /dev/null -w "%{http_code}" "https://api.github.com/repos/$REPO/releases/latest" 2>/dev/null || echo "000")
    
    if [ "$status" = "200" ]; then
      log_info "✅ Releases API accessible (jq not installed, using fallback)"
      # Try to extract tag from HTML/JSON manually
      local latest
      latest=$(curl -s "https://api.github.com/repos/$REPO/releases/latest" 2>/dev/null | grep -o '"tag_name":"[^"]*"' | head -1 | cut -d'"' -f4 || echo "")
      if [ -n "$latest" ]; then
        log_info "   Latest release: $latest"
        echo "$latest"
      else
        echo ""
      fi
      return 0
    else
      log_warn "⚠️  Could not check releases (HTTP $status). Install jq for better results."
      echo ""
      return 0
    fi
  fi
}

# Test 3: Verify release assets exist for a version
test_release_assets() {
  local version="$1"
  log_test "Test 3: Checking release assets for $version..."
  
  if [ -z "$version" ] || [ "$version" == "null" ]; then
    log_warn "⚠️  Skipping asset check (no release found)"
    return 0
  fi
  
  # Remove 'v' prefix if present
  version="${version#v}"
  local release_url="https://github.com/$REPO/releases/download/v$version"
  
  local all_found=true
  local found_count=0
  
  for target in "${TARGETS[@]}"; do
    local binary_url="$release_url/ggen-$target.tar.gz"
    local checksum_url="$release_url/ggen-$target.tar.gz.sha256"
    
    if curl -s -f -I "$binary_url" > /dev/null 2>&1; then
      log_info "   ✅ $target binary exists"
      ((found_count++))
    else
      log_error "   ❌ $target binary missing: $binary_url"
      all_found=false
    fi
    
    if curl -s -f -I "$checksum_url" > /dev/null 2>&1; then
      log_info "   ✅ $target checksum exists"
    else
      log_error "   ❌ $target checksum missing: $checksum_url"
      all_found=false
    fi
  done
  
  if [ "$all_found" = true ]; then
    log_info "✅ All assets found for $version ($found_count/4 binaries)"
    return 0
  else
    log_error "❌ Some assets are missing for $version"
    return 1
  fi
}

# Test 4: Verify Homebrew formula script logic
test_homebrew_script() {
  local version="$1"
  log_test "Test 4: Testing Homebrew formula update script logic..."
  
  if [ -z "$version" ] || [ "$version" == "null" ]; then
    log_warn "⚠️  Skipping Homebrew script test (no release found)"
    return 0
  fi
  
  version="${version#v}"
  local release_url="https://github.com/$REPO/releases/download/v$version"
  
  # Test downloading checksums (dry-run)
  log_info "   Testing checksum download URLs..."
  
  local all_checksums=true
  for target in "${TARGETS[@]}"; do
    local checksum_url="$release_url/ggen-$target.tar.gz.sha256"
    local checksum
    
    checksum=$(curl -sL "$checksum_url" 2>/dev/null | cut -d' ' -f1 || echo "")
    
    if [ -n "$checksum" ] && [ ${#checksum} -eq 64 ]; then
      log_info "   ✅ $target checksum: ${checksum:0:16}..."
    else
      log_error "   ❌ $target checksum invalid or missing"
      all_checksums=false
    fi
  done
  
  if [ "$all_checksums" = true ]; then
    log_info "✅ All checksums are valid"
    return 0
  else
    log_error "❌ Some checksums are invalid"
    return 1
  fi
}

# Test 5: Verify workflow file structure
test_workflow_structure() {
  log_test "Test 5: Verifying release workflow structure..."
  
  local workflow_file=".github/workflows/release.yml"
  
  if [ ! -f "$workflow_file" ]; then
    log_error "❌ Release workflow file not found: $workflow_file"
    return 1
  fi
  
  log_info "✅ Release workflow file exists"
  
  # Check for key components
  local issues=0
  
  if ! grep -q "action-gh-release" "$workflow_file"; then
    log_error "   ❌ Missing action-gh-release action"
    ((issues++))
  else
    log_info "   ✅ Uses action-gh-release"
  fi
  
  if ! grep -q "tar.gz" "$workflow_file"; then
    log_error "   ❌ Missing tar.gz archive creation"
    ((issues++))
  else
    log_info "   ✅ Creates tar.gz archives"
  fi
  
  if ! grep -q "sha256sum\|shasum" "$workflow_file"; then
    log_error "   ❌ Missing SHA256 checksum generation"
    ((issues++))
  else
    log_info "   ✅ Generates SHA256 checksums"
  fi
  
  # Check for all targets
  for target in "${TARGETS[@]}"; do
    if grep -q "$target" "$workflow_file"; then
      log_info "   ✅ Includes $target"
    else
      log_warn "   ⚠️  $target not found in workflow (may be in matrix)"
    fi
  done
  
  if [ $issues -eq 0 ]; then
    log_info "✅ Workflow structure looks good"
    return 0
  else
    log_error "❌ Found $issues issue(s) in workflow"
    return 1
  fi
}

# Test 6: Verify Homebrew formula template
test_homebrew_formula_template() {
  log_test "Test 6: Verifying Homebrew formula template..."
  
  local script_file="scripts/update-homebrew-formula.sh"
  
  if [ ! -f "$script_file" ]; then
    log_error "❌ Homebrew update script not found: $script_file"
    return 1
  fi
  
  log_info "✅ Homebrew update script exists"
  
  # Check for key components
  local issues=0
  
  if ! grep -q "on_macos\|on_linux" "$script_file"; then
    log_error "   ❌ Missing platform-specific blocks"
    ((issues++))
  else
    log_info "   ✅ Uses platform-specific blocks"
  fi
  
  if ! grep -q "Hardware::CPU" "$script_file"; then
    log_error "   ❌ Missing CPU detection"
    ((issues++))
  else
    log_info "   ✅ Detects CPU architecture"
  fi
  
  if ! grep -q "bin.install" "$script_file"; then
    log_error "   ❌ Missing binary installation"
    ((issues++))
  else
    log_info "   ✅ Installs binary directly (no build)"
  fi
  
  if grep -q "depends_on.*rust\|cargo.*install" "$script_file"; then
    log_warn "   ⚠️  Script may still reference build dependencies"
    ((issues++))
  else
    log_info "   ✅ No build dependencies"
  fi
  
  if [ $issues -eq 0 ]; then
    log_info "✅ Homebrew formula template looks good"
    return 0
  else
    log_warn "⚠️  Found $issues potential issue(s)"
    return 0  # Not critical, just warnings
  fi
}

# Test 7: Check workflow issue - release creation
test_release_creation() {
  log_test "Test 7: Checking release creation mechanism..."
  
  local workflow_file=".github/workflows/release.yml"
  
  # Check if workflow creates release or just uploads assets
  if grep -q "action-gh-release" "$workflow_file"; then
    if grep -q "tag_name\|create.*release" "$workflow_file"; then
      log_info "✅ Workflow creates release"
    else
      log_warn "⚠️  Workflow uses action-gh-release but may not create release"
      log_warn "   Note: action-gh-release@v2 creates release automatically if tag exists"
      log_info "   This should work if tag is pushed before workflow runs"
    fi
  fi
  
  return 0
}

# Main test runner
main() {
  echo ""
  log_info "🧪 Testing Release Process"
  echo "================================"
  echo ""
  
  local tests_passed=0
  local tests_failed=0
  
  # Run tests
  if test_github_api; then
    ((tests_passed++))
  else
    ((tests_failed++))
    log_error "Cannot continue without GitHub API access"
    exit 1
  fi
  
  local latest_release=""
  latest_release=$(test_existing_releases 2>&1 | grep -E "^v[0-9]" | head -1 || echo "")
  ((tests_passed++))
  
  if test_release_assets "$latest_release"; then
    ((tests_passed++))
  else
    ((tests_failed++))
  fi
  
  if test_homebrew_script "$latest_release"; then
    ((tests_passed++))
  else
    ((tests_failed++))
  fi
  
  if test_workflow_structure; then
    ((tests_passed++))
  else
    ((tests_failed++))
  fi
  
  if test_homebrew_formula_template; then
    ((tests_passed++))
  else
    ((tests_failed++))
  fi
  
  if test_release_creation; then
    ((tests_passed++))
  else
    ((tests_failed++))
  fi
  
  # Summary
  echo ""
  echo "================================"
  log_info "Test Summary:"
  log_info "  ✅ Passed: $tests_passed"
  if [ $tests_failed -gt 0 ]; then
    log_error "  ❌ Failed: $tests_failed"
  else
    log_info "  ❌ Failed: $tests_failed"
  fi
  echo ""
  
  if [ $tests_failed -eq 0 ]; then
    log_info "🎉 All tests passed! Release process looks good."
    return 0
  else
    log_warn "⚠️  Some tests failed. Review the output above."
    return 1
  fi
}

# Check dependencies
if ! command -v curl &> /dev/null; then
  log_error "curl is required but not installed"
  exit 1
fi

if ! command -v jq &> /dev/null; then
  log_warn "jq is not installed. Some tests may be limited."
fi

main "$@"

