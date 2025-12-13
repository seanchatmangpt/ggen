class Ggen < Formula
  desc "Deterministic, language-agnostic code generation framework"
  homepage "https://github.com/seanchatmangpt/ggen"
  url "https://github.com/seanchatmangpt/ggen/archive/refs/tags/v4.0.0.tar.gz"
  sha256 "2207b4fecf2aacb0c58c71e1cd300585b22f6e2d5ee882d865ec41f6f85adcbe"
  license "MIT"
  head "https://github.com/seanchatmangpt/ggen.git", branch: "master"

  depends_on "rust" => :build

  def install
    system "cargo", "install", *std_cargo_args(path: "crates/ggen-cli")
  end

  test do
    # Test basic functionality
    system bin/"ggen", "template", "list"
  end
end
