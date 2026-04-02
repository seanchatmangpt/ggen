class Ggen < Formula
  desc "Deterministic, language-agnostic code generation framework"
  homepage "https://github.com/seanchatmangpt/ggen"
  url "https://github.com/seanchatmangpt/ggen/archive/refs/tags/v3.0.0.tar.gz"
  sha256 "PLACEHOLDER_SHA256_WILL_BE_UPDATED_ON_RELEASE"
  license "MIT"
  head "https://github.com/seanchatmangpt/ggen.git", branch: "master"

  depends_on "rust" => :build

  def install
    system "cargo", "install", "--path", "crates/ggen-cli", "--bin", "ggen", *std_cargo_args
  end

  test do
    assert_match version.to_s, shell_output("#{bin}/ggen --version")
    
    # Test basic functionality
    system "#{bin}/ggen", "template", "list"
  end
end
