class Ggen < Formula
  desc "Deterministic, language-agnostic code generation framework"
  homepage "https://github.com/seanchatmangpt/ggen"
  url "https://github.com/seanchatmangpt/ggen/archive/refs/tags/v5.0.0.tar.gz"
  sha256 "b950e2e816918e0c92c6d7aad9dcf12c14a34031074bd2a450b64a609426d397"
  license "MIT"
  head "https://github.com/seanchatmangpt/ggen.git", branch: "master"

  depends_on "rust" => :build

  def install
    system "cargo", "install", *std_cargo_args(path: "crates/ggen-cli")
  end

  test do
    # Test version output
    assert_match "ggen 5.0.0", shell_output("#{bin}/ggen --version")
  end
end
