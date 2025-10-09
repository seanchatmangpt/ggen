class Ggen < Formula
  desc "Language-agnostic, graph-aware generator for reproducible projections"
  homepage "https://github.com/seanchatmangpt/ggen"
  version "0.2.0"
  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/seanchatmangpt/ggen/releases/download/v0.2.0/ggen-aarch64-apple-darwin.tar.gz"
      sha256 "4d04edaa9e386af06e3560b77023f33af0c90f2f635bbd5661b0aa8af5932a46"
    else
      url "https://github.com/seanchatmangpt/ggen/releases/download/v0.2.0/ggen-x86_64-apple-darwin.tar.gz"
      sha256 "7ae60304292b40e560a30746c00fa1b16bb963c4178188f45cc46a4545b98275"
    end
  end
  on_linux do
    if Hardware::CPU.arm?
      url "https://github.com/seanchatmangpt/ggen/releases/download/v0.2.0/ggen-aarch64-unknown-linux-gnu.tar.gz"
      sha256 "4c3bd62090e6b76a2a458cac617dcb60a9a1715bad86abaeb75d33e979f84b30"
    else
      url "https://github.com/seanchatmangpt/ggen/releases/download/v0.2.0/ggen-x86_64-unknown-linux-gnu.tar.gz"
      sha256 "1bb416531decbdac4f168cf606a2cac46e47cc3b21e729c421143fe7a4feb6b3"
    end
  end
  def install
    bin.install "ggen"
    generate_completions_from_executable(bin/"ggen", "completion")
  end
  test do
    assert_match "ggen", shell_output("#{bin}/ggen --version")
  end
end
