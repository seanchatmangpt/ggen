class Ggen < Formula
  desc "Deterministic, language-agnostic code generation framework"
  homepage "https://github.com/seanchatmangpt/ggen"
  url "https://github.com/seanchatmangpt/ggen/archive/refs/tags/v3.4.0.tar.gz"
  sha256 "5d433cd6da68f5d9be3f9e7e8b650fa1a0870207d79c227848d8e6f31e925fca"
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
