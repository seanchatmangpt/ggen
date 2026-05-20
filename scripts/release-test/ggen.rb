class Ggen < Formula
  desc "Language-agnostic, graph-aware generator for reproducible projections"
  homepage "https://github.com/seanchatmangpt/ggen"
  version "26.5.19"
  url "file:///tmp/ggen-release-val-84583/ggen.tar.gz"
  sha256 "0ac62a1e32038419a27e3d1b86dfe1126d84d6d618360b241fb9c408f4e89ec2"
  def install
    bin.install "ggen"
  end
  test do
    assert_match "ggen", shell_output("#{bin}/ggen --version")
  end
end
