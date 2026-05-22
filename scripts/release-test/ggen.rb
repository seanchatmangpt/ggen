class Ggen < Formula
  desc "Language-agnostic, graph-aware generator for reproducible projections"
  homepage "https://github.com/seanchatmangpt/ggen"
  version "26.5.21"
  url "file:///tmp/ggen.tar.gz"
  sha256 "97a76a897b7b8b5e7aa47528f0e99dded44b9c49b076e2a4d9a4a2b14d8bd6b0"
  def install
    bin.install "ggen"
  end
  test do
    assert_match "ggen", shell_output("#{bin}/ggen --version")
  end
end
