class Ggen < Formula
  desc "Language-agnostic, graph-aware generator for reproducible projections"
  homepage "https://github.com/seanchatmangpt/ggen"
  version "26.5.19"
  url "file:///tmp/ggen-release-val-69000/ggen.tar.gz"
  sha256 "61e68150cbf01bf28eca91f7c98d7eabb71b78edb1ee1f0f09b58f00ea356580"
  def install
    bin.install "ggen"
  end
  test do
    assert_match "ggen", shell_output("#{bin}/ggen --version")
  end
end
