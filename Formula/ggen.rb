class Ggen < Formula
  desc "Deterministic, language-agnostic code generation framework"
  homepage "https://github.com/seanchatmangpt/ggen"
  url "https://github.com/seanchatmangpt/ggen/archive/refs/tags/v5.0.1.tar.gz"
  sha256 "bc35ce1980a4d9a0552063436d9f26ab0fd8bec0bc0e7b670ba9263dba2295ac"
  license "MIT"
  head "https://github.com/seanchatmangpt/ggen.git", branch: "master"

  depends_on "rust" => :build

  def install
    system "cargo", "install", *std_cargo_args(path: "crates/ggen-cli")
  end

  test do
    # Test version output
    assert_match "ggen 5.0.1", shell_output("#{bin}/ggen --version")
  end
end
