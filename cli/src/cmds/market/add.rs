use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct AddArgs {
    /// Gpack ID with optional version (e.g., "io.ggen.rust.cli@1.0.0")
    pub gpack_id: String,
}

#[cfg_attr(test, mockall::automock)]
pub trait GpackInstaller {
    fn install(&self, gpack_id: String, version: Option<String>) -> Result<InstallResult>;
}

#[derive(Debug, Clone)]
pub struct InstallResult {
    pub gpack_id: String,
    pub version: String,
    pub already_installed: bool,
}

fn parse_gpack_spec(spec: &str) -> (String, Option<String>) {
    if let Some(pos) = spec.rfind('@') {
        let id = spec[..pos].to_string();
        let version = spec[pos + 1..].to_string();
        (id, Some(version))
    } else {
        (spec.to_string(), None)
    }
}

pub async fn run(args: &AddArgs) -> Result<()> {
    println!("🚧 Placeholder: market add");
    println!("  Gpack ID: {}", args.gpack_id);
    Ok(())
}

pub async fn run_with_deps(args: &AddArgs, installer: &dyn GpackInstaller) -> Result<()> {
    let (gpack_id, version) = parse_gpack_spec(&args.gpack_id);
    let result = installer.install(gpack_id, version)?;

    if result.already_installed {
        println!("Gpack '{}' is already installed", result.gpack_id);
    } else {
        println!(
            "✅ Successfully added gpack '{}' version {}",
            result.gpack_id, result.version
        );
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[test]
    fn test_parse_gpack_spec_with_version() {
        let (id, version) = parse_gpack_spec("io.ggen.rust.cli@1.0.0");
        assert_eq!(id, "io.ggen.rust.cli");
        assert_eq!(version, Some("1.0.0".to_string()));
    }

    #[test]
    fn test_parse_gpack_spec_without_version() {
        let (id, version) = parse_gpack_spec("io.ggen.rust.cli");
        assert_eq!(id, "io.ggen.rust.cli");
        assert_eq!(version, None);
    }

    #[tokio::test]
    async fn test_add_calls_installer() {
        let mut mock_installer = MockGpackInstaller::new();
        mock_installer
            .expect_install()
            .with(eq("io.ggen.rust.cli"), eq(Some("1.0.0")))
            .times(1)
            .returning(|id, version| {
                Ok(InstallResult {
                    gpack_id: id.to_string(),
                    version: version.unwrap().to_string(),
                    already_installed: false,
                })
            });

        let args = AddArgs {
            gpack_id: "io.ggen.rust.cli@1.0.0".to_string(),
        };

        let result = run_with_deps(&args, &mock_installer).await;
        assert!(result.is_ok());
    }
}
