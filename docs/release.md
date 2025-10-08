# Release

1. `just all`
2. Compute SHA256 of each tarball:
```bash
shasum -a 256 rgen-*.tar.gz
```
3. Create GitHub release `vX.Y.Z` and upload tarballs.
4. Update `Formula/rgen.rb` URLs and SHA256.
5. `brew tap your-org/tap && brew install --build-from-source Formula/rgen.rb`
6. Verify:
```bash
rgen --version
rgen list
```
