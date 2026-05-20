class Ggen < Formula
  desc "Language-agnostic, graph-aware generator for reproducible projections"
  homepage "https://github.com/seanchatmangpt/ggen"
  version "26.5.19"
  url "file:///tmp/ggen-release-val-43148/ggen.tar.gz"
  sha256 "7730ff928bc9b8e4d1c49521cea2a38f33edb42b255d35a7aa05ae71d2e1a5df"
  def install
    bin.install "ggen"
  end
  test do
    assert_match "ggen", shell_output("#{bin}/ggen --version")
  end
end
