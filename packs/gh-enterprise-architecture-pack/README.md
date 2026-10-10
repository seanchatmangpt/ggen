# gh-enterprise-architecture-pack

This pack turns an observed GitHub Terraform corpus into a bounded enterprise
repository factory. Its control plane is explicit:

```text
observe -> admit/exclude/UNKNOWN -> canonical profile -> generate -> plan -> authorize -> actuate -> receipt
```

The pack does not replace `gh-terraform-pack`. It composes above it:

- `gh-terraform-pack` manages one repository's institutional desired state.
- this pack manufactures a typed set of repositories from admitted patterns.

## Generated surfaces

- `infra/terraform/github-enterprise/*.tf`
- `scripts/gh/terraform-corpus-census.sh`
- `docs/gh-enterprise/TERRAFORM-CORPUS.md`
- `docs/gh-enterprise/ARCHITECTURE.md`

The census performs GitHub reads only. Terraform generation does not grant apply
authority. Issues and repository-file content remain outside this module's
authority boundary.
