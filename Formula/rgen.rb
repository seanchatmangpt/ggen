class Rgen < Formula
  desc "Language-agnostic, graph-aware generator for reproducible projections"
  homepage "https://github.com/seanchatmangpt/rgen"
  version "0.1.0"
  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/seanchatmangpt/rgen/releases/download/v0.1.0/rgen-aarch64-apple-darwin.tar.gz"
      sha256 "REPLACE_WITH_SHA256"
    else
      url "https://github.com/seanchatmangpt/rgen/releases/download/v0.1.0/rgen-x86_64-apple-darwin.tar.gz"
      sha256 "REPLACE_WITH_SHA256"
    end
  end
  on_linux do
    if Hardware::CPU.arm?
      url "https://github.com/seanchatmangpt/rgen/releases/download/v0.1.0/rgen-aarch64-unknown-linux-gnu.tar.gz"
      sha256 "REPLACE_WITH_SHA256"
    else
      url "https://github.com/seanchatmangpt/rgen/releases/download/v0.1.0/rgen-x86_64-unknown-linux-gnu.tar.gz"
      sha256 "REPLACE_WITH_SHA256"
    end
  end
  def install
    bin.install "rgen"
    generate_completions_from_executable(bin/"rgen", "completion")
  end
  test do
    assert_match "rgen", shell_output("#{bin}/rgen --version")
  end
end
