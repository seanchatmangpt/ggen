class Ggen < Formula
  desc "Deterministic, language-agnostic code generation framework"
  homepage "https://github.com/seanchatmangpt/ggen"
  url "https://github.com/seanchatmangpt/ggen/archive/refs/tags/v2.5.0.tar.gz"
  sha256 "faaccded2180c337404eb66bded839690a006a9dafbc729fbc8af3844f5ea504"
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
