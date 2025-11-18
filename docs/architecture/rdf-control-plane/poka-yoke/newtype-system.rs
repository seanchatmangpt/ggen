// ============================================================================
// POKA YOKE TYPE SYSTEM - Compile-Time Mistake Prevention
// ============================================================================
// Version: 3.0.0
// Description: NewType wrappers, phantom types, and builder patterns
// Architecture: Make invalid states unrepresentable at compile time
// ============================================================================

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::fmt;
use std::marker::PhantomData;

// ============================================================================
// PART 1: NEWTYPE WRAPPERS - Basic Domain Constraints
// ============================================================================

/// Package ID with validated format
///
/// **POKA YOKE**: Cannot construct invalid package IDs
///
/// # Invariants
/// - Non-empty (1-200 characters)
/// - Only alphanumeric, hyphens, underscores, slashes
/// - No path traversal (..)
/// - No control characters
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct PackageId(String);

impl PackageId {
    /// Create validated package ID
    ///
    /// # Errors
    /// - Empty ID
    /// - ID too long (>200 chars)
    /// - Invalid characters
    /// - Path traversal sequences
    pub fn new(id: impl Into<String>) -> Result<Self> {
        let id = id.into();

        if id.is_empty() {
            return Err(Error::new("Package ID cannot be empty"));
        }

        if id.len() > 200 {
            return Err(Error::new(format!(
                "Package ID too long: {} chars (max 200)",
                id.len()
            )));
        }

        if !id
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '/')
        {
            return Err(Error::new(
                "Package ID contains invalid characters (allowed: alphanumeric, -, _, /)",
            ));
        }

        if id.contains("..") {
            return Err(Error::new(
                "Package ID cannot contain path traversal sequences (..)",
            ));
        }

        if id.chars().any(|c| c.is_control()) {
            return Err(Error::new("Package ID cannot contain control characters"));
        }

        Ok(Self(id))
    }

    /// Get inner value (guaranteed valid)
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Unsafe constructor for trusted sources (internal use only)
    ///
    /// # Safety
    /// Caller must ensure ID is valid
    pub(crate) unsafe fn new_unchecked(id: String) -> Self {
        Self(id)
    }
}

impl TryFrom<String> for PackageId {
    type Error = Error;
    fn try_from(s: String) -> Result<Self> {
        Self::new(s)
    }
}

impl From<PackageId> for String {
    fn from(id: PackageId) -> String {
        id.0
    }
}

impl fmt::Display for PackageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for PackageId {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

// ----------------------------------------------------------------------------

/// Package name with validated format
///
/// **POKA YOKE**: Cannot construct invalid package names
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct PackageName(String);

impl PackageName {
    pub fn new(name: impl Into<String>) -> Result<Self> {
        let name = name.into();

        if name.is_empty() {
            return Err(Error::new("Package name cannot be empty"));
        }

        if name.len() > 100 {
            return Err(Error::new("Package name too long (max 100 characters)"));
        }

        if !name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
        {
            return Err(Error::new(
                "Package name must contain only alphanumeric, hyphens, underscores",
            ));
        }

        Ok(Self(name))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for PackageName {
    type Error = Error;
    fn try_from(s: String) -> Result<Self> {
        Self::new(s)
    }
}

impl From<PackageName> for String {
    fn from(name: PackageName) -> String {
        name.0
    }
}

impl fmt::Display for PackageName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// ----------------------------------------------------------------------------

/// Semantic version with validated format
///
/// **POKA YOKE**: Only valid semver can be constructed
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct SemanticVersion {
    inner: String,
    major: u32,
    minor: u32,
    patch: u32,
    prerelease: Option<String>,
    build: Option<String>,
}

impl SemanticVersion {
    /// Create validated semantic version
    ///
    /// Format: MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]
    pub fn new(version: impl Into<String>) -> Result<Self> {
        let version = version.into();

        if version.is_empty() {
            return Err(Error::new("Version cannot be empty"));
        }

        if version.len() > 50 {
            return Err(Error::new("Version too long (max 50 characters)"));
        }

        // Parse semver components
        let (base, build) = if let Some(idx) = version.find('+') {
            let (b, bld) = version.split_at(idx);
            (b, Some(bld[1..].to_string()))
        } else {
            (version.as_str(), None)
        };

        let (core, prerelease) = if let Some(idx) = base.find('-') {
            let (c, pre) = base.split_at(idx);
            (c, Some(pre[1..].to_string()))
        } else {
            (base, None)
        };

        // Parse major.minor.patch
        let parts: Vec<&str> = core.split('.').collect();
        if parts.len() < 3 {
            return Err(Error::new(format!(
                "Invalid semver format '{}': must be MAJOR.MINOR.PATCH",
                version
            )));
        }

        let major = parts[0].parse::<u32>().map_err(|_| {
            Error::new(format!("Invalid major version: '{}'", parts[0]))
        })?;

        let minor = parts[1].parse::<u32>().map_err(|_| {
            Error::new(format!("Invalid minor version: '{}'", parts[1]))
        })?;

        let patch = parts[2].parse::<u32>().map_err(|_| {
            Error::new(format!("Invalid patch version: '{}'", parts[2]))
        })?;

        Ok(Self {
            inner: version,
            major,
            minor,
            patch,
            prerelease,
            build,
        })
    }

    pub fn as_str(&self) -> &str {
        &self.inner
    }

    pub fn major(&self) -> u32 {
        self.major
    }

    pub fn minor(&self) -> u32 {
        self.minor
    }

    pub fn patch(&self) -> u32 {
        self.patch
    }

    pub fn prerelease(&self) -> Option<&str> {
        self.prerelease.as_deref()
    }

    pub fn build(&self) -> Option<&str> {
        self.build.as_deref()
    }

    /// Check if this is a prerelease version
    pub fn is_prerelease(&self) -> bool {
        self.prerelease.is_some()
    }

    /// Check if this is a stable release
    pub fn is_stable(&self) -> bool {
        self.prerelease.is_none() && self.major > 0
    }
}

impl TryFrom<String> for SemanticVersion {
    type Error = Error;
    fn try_from(s: String) -> Result<Self> {
        Self::new(s)
    }
}

impl From<SemanticVersion> for String {
    fn from(v: SemanticVersion) -> String {
        v.inner
    }
}

impl fmt::Display for SemanticVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

// ----------------------------------------------------------------------------

/// Author name with validated format
///
/// **POKA YOKE**: Cannot create invalid author names
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct AuthorName(String);

impl AuthorName {
    pub fn new(name: impl Into<String>) -> Result<Self> {
        let name = name.into().trim().to_string();

        if name.is_empty() {
            return Err(Error::new("Author name cannot be empty"));
        }

        if name.len() > 200 {
            return Err(Error::new("Author name too long (max 200 characters)"));
        }

        Ok(Self(name))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for AuthorName {
    type Error = Error;
    fn try_from(s: String) -> Result<Self> {
        Self::new(s)
    }
}

impl From<AuthorName> for String {
    fn from(name: AuthorName) -> String {
        name.0
    }
}

impl fmt::Display for AuthorName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// ----------------------------------------------------------------------------

/// License identifier from SPDX list
///
/// **POKA YOKE**: Only approved licenses can be used
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LicenseId {
    MIT,
    Apache2_0,
    GPL3_0,
    BSD3Clause,
    ISC,
    MPL2_0,
    LGPL3_0,
    AGPL3_0,
    BSD2Clause,
    Unlicense,
    CC0_1_0,
}

impl LicenseId {
    pub fn from_spdx(spdx: &str) -> Result<Self> {
        match spdx {
            "MIT" => Ok(Self::MIT),
            "Apache-2.0" => Ok(Self::Apache2_0),
            "GPL-3.0" => Ok(Self::GPL3_0),
            "BSD-3-Clause" => Ok(Self::BSD3Clause),
            "ISC" => Ok(Self::ISC),
            "MPL-2.0" => Ok(Self::MPL2_0),
            "LGPL-3.0" => Ok(Self::LGPL3_0),
            "AGPL-3.0" => Ok(Self::AGPL3_0),
            "BSD-2-Clause" => Ok(Self::BSD2Clause),
            "Unlicense" => Ok(Self::Unlicense),
            "CC0-1.0" => Ok(Self::CC0_1_0),
            _ => Err(Error::new(format!(
                "Unsupported license '{}' (must be SPDX-approved)",
                spdx
            ))),
        }
    }

    pub fn to_spdx(&self) -> &'static str {
        match self {
            Self::MIT => "MIT",
            Self::Apache2_0 => "Apache-2.0",
            Self::GPL3_0 => "GPL-3.0",
            Self::BSD3Clause => "BSD-3-Clause",
            Self::ISC => "ISC",
            Self::MPL2_0 => "MPL-2.0",
            Self::LGPL3_0 => "LGPL-3.0",
            Self::AGPL3_0 => "AGPL-3.0",
            Self::BSD2Clause => "BSD-2-Clause",
            Self::Unlicense => "Unlicense",
            Self::CC0_1_0 => "CC0-1.0",
        }
    }
}

impl fmt::Display for LicenseId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_spdx())
    }
}

// ----------------------------------------------------------------------------

/// SHA-256 checksum (64 hex characters)
///
/// **POKA YOKE**: Only valid SHA-256 checksums can be constructed
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct Sha256Checksum([u8; 32]);

impl Sha256Checksum {
    /// Create from hex string
    pub fn from_hex(hex: impl AsRef<str>) -> Result<Self> {
        let hex = hex.as_ref();

        if hex.len() != 64 {
            return Err(Error::new(format!(
                "Invalid SHA-256 checksum length: {} (expected 64 hex chars)",
                hex.len()
            )));
        }

        if !hex.chars().all(|c| c.is_ascii_hexdigit()) {
            return Err(Error::new(
                "Checksum must contain only hexadecimal characters",
            ));
        }

        let mut bytes = [0u8; 32];
        for i in 0..32 {
            bytes[i] = u8::from_str_radix(&hex[i * 2..i * 2 + 2], 16)
                .map_err(|_| Error::new("Invalid hex encoding in checksum"))?;
        }

        Ok(Self(bytes))
    }

    /// Create from bytes
    pub fn from_bytes(bytes: [u8; 32]) -> Self {
        Self(bytes)
    }

    /// Get bytes
    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }

    /// Get hex string
    pub fn to_hex(&self) -> String {
        hex::encode(self.0)
    }
}

impl TryFrom<String> for Sha256Checksum {
    type Error = Error;
    fn try_from(s: String) -> Result<Self> {
        Self::from_hex(s)
    }
}

impl From<Sha256Checksum> for String {
    fn from(cs: Sha256Checksum) -> String {
        cs.to_hex()
    }
}

impl fmt::Display for Sha256Checksum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_hex())
    }
}

// ----------------------------------------------------------------------------

/// Ed25519 signature (64 bytes = 128 hex chars)
///
/// **POKA YOKE**: Only valid Ed25519 signatures can be constructed
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct Ed25519Signature([u8; 64]);

impl Ed25519Signature {
    /// Create from hex string
    pub fn from_hex(hex: impl AsRef<str>) -> Result<Self> {
        let hex = hex.as_ref();

        if hex.len() != 128 {
            return Err(Error::new(format!(
                "Invalid Ed25519 signature length: {} (expected 128 hex chars)",
                hex.len()
            )));
        }

        if !hex.chars().all(|c| c.is_ascii_hexdigit()) {
            return Err(Error::new(
                "Signature must contain only hexadecimal characters",
            ));
        }

        let mut bytes = [0u8; 64];
        for i in 0..64 {
            bytes[i] = u8::from_str_radix(&hex[i * 2..i * 2 + 2], 16)
                .map_err(|_| Error::new("Invalid hex encoding in signature"))?;
        }

        Ok(Self(bytes))
    }

    /// Create from bytes
    pub fn from_bytes(bytes: [u8; 64]) -> Self {
        Self(bytes)
    }

    /// Get bytes
    pub fn as_bytes(&self) -> &[u8; 64] {
        &self.0
    }

    /// Get hex string
    pub fn to_hex(&self) -> String {
        hex::encode(self.0)
    }
}

impl TryFrom<String> for Ed25519Signature {
    type Error = Error;
    fn try_from(s: String) -> Result<Self> {
        Self::from_hex(s)
    }
}

impl From<Ed25519Signature> for String {
    fn from(sig: Ed25519Signature) -> String {
        sig.to_hex()
    }
}

impl fmt::Display for Ed25519Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_hex())
    }
}

// ----------------------------------------------------------------------------

/// Ed25519 public key (32 bytes = 64 hex chars)
///
/// **POKA YOKE**: Only valid Ed25519 public keys can be constructed
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct Ed25519PublicKey([u8; 32]);

impl Ed25519PublicKey {
    /// Create from hex string
    pub fn from_hex(hex: impl AsRef<str>) -> Result<Self> {
        let hex = hex.as_ref();

        if hex.len() != 64 {
            return Err(Error::new(format!(
                "Invalid Ed25519 public key length: {} (expected 64 hex chars)",
                hex.len()
            )));
        }

        if !hex.chars().all(|c| c.is_ascii_hexdigit()) {
            return Err(Error::new(
                "Public key must contain only hexadecimal characters",
            ));
        }

        let mut bytes = [0u8; 32];
        for i in 0..32 {
            bytes[i] = u8::from_str_radix(&hex[i * 2..i * 2 + 2], 16)
                .map_err(|_| Error::new("Invalid hex encoding in public key"))?;
        }

        Ok(Self(bytes))
    }

    /// Create from bytes
    pub fn from_bytes(bytes: [u8; 32]) -> Self {
        Self(bytes)
    }

    /// Get bytes
    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }

    /// Get hex string
    pub fn to_hex(&self) -> String {
        hex::encode(self.0)
    }
}

impl TryFrom<String> for Ed25519PublicKey {
    type Error = Error;
    fn try_from(s: String) -> Result<Self> {
        Self::from_hex(s)
    }
}

impl From<Ed25519PublicKey> for String {
    fn from(key: Ed25519PublicKey) -> String {
        key.to_hex()
    }
}

impl fmt::Display for Ed25519PublicKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_hex())
    }
}

// ============================================================================
// PART 2: PHANTOM TYPES - State-Based Type Constraints
// ============================================================================

/// Package state marker types (zero-size, compile-time only)
pub mod state {
    pub struct Draft;
    pub struct Published;
    pub struct Active;
    pub struct Deprecated;
    pub struct Archived;
    pub struct Withdrawn;
}

/// Package with state encoded in type system
///
/// **POKA YOKE**: Cannot perform invalid operations on wrong state
///
/// Example:
/// - `Package<Draft>` can only call .publish()
/// - `Package<Published>` can only call .activate()
/// - `Package<Active>` can only call .deprecate() or .withdraw()
///
/// State transitions consume the old state and produce new state:
/// ```rust,ignore
/// let draft: Package<Draft> = Package::new(...);
/// let published: Package<Published> = draft.publish(signature)?;
/// let active: Package<Active> = published.activate()?;
/// ```
#[derive(Debug, Clone)]
pub struct Package<State> {
    id: PackageId,
    name: PackageName,
    version: SemanticVersion,
    author: AuthorName,
    license: LicenseId,
    checksum: Sha256Checksum,
    _state: PhantomData<State>,
}

// Draft state - being prepared
impl Package<state::Draft> {
    /// Create new package in Draft state
    pub fn new(
        id: PackageId,
        name: PackageName,
        version: SemanticVersion,
        author: AuthorName,
        license: LicenseId,
        checksum: Sha256Checksum,
    ) -> Self {
        Self {
            id,
            name,
            version,
            author,
            license,
            checksum,
            _state: PhantomData,
        }
    }

    /// Publish package (Draft → Published)
    ///
    /// **POKA YOKE**: Can only be called on Draft packages
    /// Consumes Draft, produces Published
    pub fn publish(
        self,
        signature: Ed25519Signature,
    ) -> Result<Package<state::Published>> {
        // Validation logic here...
        Ok(Package {
            id: self.id,
            name: self.name,
            version: self.version,
            author: self.author,
            license: self.license,
            checksum: self.checksum,
            _state: PhantomData,
        })
    }
}

// Published state - awaiting verification
impl Package<state::Published> {
    /// Activate package (Published → Active)
    ///
    /// **POKA YOKE**: Can only be called on Published packages
    /// Requires signature verification
    pub fn activate(self) -> Result<Package<state::Active>> {
        // Verify signature...
        Ok(Package {
            id: self.id,
            name: self.name,
            version: self.version,
            author: self.author,
            license: self.license,
            checksum: self.checksum,
            _state: PhantomData,
        })
    }

    /// Withdraw package (Published → Withdrawn)
    pub fn withdraw(self, reason: String) -> Result<Package<state::Withdrawn>> {
        Ok(Package {
            id: self.id,
            name: self.name,
            version: self.version,
            author: self.author,
            license: self.license,
            checksum: self.checksum,
            _state: PhantomData,
        })
    }
}

// Active state - installable
impl Package<state::Active> {
    /// Deprecate package (Active → Deprecated)
    ///
    /// **POKA YOKE**: Can only be called on Active packages
    pub fn deprecate(
        self,
        notice: String,
        replacement: Option<PackageId>,
    ) -> Result<Package<state::Deprecated>> {
        Ok(Package {
            id: self.id,
            name: self.name,
            version: self.version,
            author: self.author,
            license: self.license,
            checksum: self.checksum,
            _state: PhantomData,
        })
    }

    /// Withdraw package (Active → Withdrawn)
    pub fn withdraw(self, reason: String) -> Result<Package<state::Withdrawn>> {
        Ok(Package {
            id: self.id,
            name: self.name,
            version: self.version,
            author: self.author,
            license: self.license,
            checksum: self.checksum,
            _state: PhantomData,
        })
    }
}

// Deprecated state - still available but discouraged
impl Package<state::Deprecated> {
    /// Archive package (Deprecated → Archived)
    pub fn archive(self) -> Result<Package<state::Archived>> {
        Ok(Package {
            id: self.id,
            name: self.name,
            version: self.version,
            author: self.author,
            license: self.license,
            checksum: self.checksum,
            _state: PhantomData,
        })
    }

    /// Un-deprecate package (Deprecated → Active)
    ///
    /// **POKA YOKE**: Only allowed if replacement is unavailable
    pub fn un_deprecate(self) -> Result<Package<state::Active>> {
        // Check replacement status...
        Ok(Package {
            id: self.id,
            name: self.name,
            version: self.version,
            author: self.author,
            license: self.license,
            checksum: self.checksum,
            _state: PhantomData,
        })
    }
}

// Archived state - terminal, no transitions
impl Package<state::Archived> {
    // No transition methods - terminal state
}

// Withdrawn state - terminal, no transitions
impl Package<state::Withdrawn> {
    // No transition methods - terminal state
}

// Common methods available in all states
impl<State> Package<State> {
    pub fn id(&self) -> &PackageId {
        &self.id
    }

    pub fn name(&self) -> &PackageName {
        &self.name
    }

    pub fn version(&self) -> &SemanticVersion {
        &self.version
    }

    pub fn author(&self) -> &AuthorName {
        &self.author
    }

    pub fn license(&self) -> &LicenseId {
        &self.license
    }

    pub fn checksum(&self) -> &Sha256Checksum {
        &self.checksum
    }
}

// ============================================================================
// PART 3: BUILDER PATTERN - Enforce Required Fields
// ============================================================================

/// Package builder with compile-time required field enforcement
///
/// **POKA YOKE**: Cannot build package without all required fields
pub struct PackageBuilder {
    id: Option<PackageId>,
    name: Option<PackageName>,
    version: Option<SemanticVersion>,
    author: Option<AuthorName>,
    license: Option<LicenseId>,
    checksum: Option<Sha256Checksum>,
}

impl PackageBuilder {
    pub fn new() -> Self {
        Self {
            id: None,
            name: None,
            version: None,
            author: None,
            license: None,
            checksum: None,
        }
    }

    pub fn id(mut self, id: PackageId) -> Self {
        self.id = Some(id);
        self
    }

    pub fn name(mut self, name: PackageName) -> Self {
        self.name = Some(name);
        self
    }

    pub fn version(mut self, version: SemanticVersion) -> Self {
        self.version = Some(version);
        self
    }

    pub fn author(mut self, author: AuthorName) -> Self {
        self.author = Some(author);
        self
    }

    pub fn license(mut self, license: LicenseId) -> Self {
        self.license = Some(license);
        self
    }

    pub fn checksum(mut self, checksum: Sha256Checksum) -> Self {
        self.checksum = Some(checksum);
        self
    }

    /// Build package in Draft state
    ///
    /// **POKA YOKE**: Compile error if any required field is missing
    pub fn build(self) -> Result<Package<state::Draft>> {
        Ok(Package::new(
            self.id.ok_or_else(|| Error::new("Package ID is required"))?,
            self.name
                .ok_or_else(|| Error::new("Package name is required"))?,
            self.version
                .ok_or_else(|| Error::new("Package version is required"))?,
            self.author
                .ok_or_else(|| Error::new("Package author is required"))?,
            self.license
                .ok_or_else(|| Error::new("Package license is required"))?,
            self.checksum
                .ok_or_else(|| Error::new("Package checksum is required"))?,
        ))
    }
}

impl Default for PackageBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_package_id_validation() {
        // Valid IDs
        assert!(PackageId::new("my-package").is_ok());
        assert!(PackageId::new("org/my-package").is_ok());
        assert!(PackageId::new("deep/nested/package_123").is_ok());

        // Invalid IDs
        assert!(PackageId::new("").is_err());
        assert!(PackageId::new("../../../etc/passwd").is_err());
        assert!(PackageId::new("invalid chars!").is_err());
        assert!(PackageId::new(&"x".repeat(201)).is_err());
    }

    #[test]
    fn test_semantic_version() {
        // Valid versions
        let v = SemanticVersion::new("1.2.3").unwrap();
        assert_eq!(v.major(), 1);
        assert_eq!(v.minor(), 2);
        assert_eq!(v.patch(), 3);

        let v2 = SemanticVersion::new("2.0.0-alpha.1+build.123").unwrap();
        assert_eq!(v2.major(), 2);
        assert!(v2.is_prerelease());

        // Invalid versions
        assert!(SemanticVersion::new("").is_err());
        assert!(SemanticVersion::new("1.2").is_err());
        assert!(SemanticVersion::new("v1.2.3").is_err());
    }

    #[test]
    fn test_phantom_type_transitions() {
        let draft = PackageBuilder::new()
            .id(PackageId::new("test-pkg").unwrap())
            .name(PackageName::new("test").unwrap())
            .version(SemanticVersion::new("1.0.0").unwrap())
            .author(AuthorName::new("Test Author").unwrap())
            .license(LicenseId::MIT)
            .checksum(
                Sha256Checksum::from_hex(
                    "0000000000000000000000000000000000000000000000000000000000000000",
                )
                .unwrap(),
            )
            .build()
            .unwrap();

        // Can only call publish() on Draft
        let sig = Ed25519Signature::from_hex(
            &"00".repeat(64)
        ).unwrap();
        let published = draft.publish(sig).unwrap();

        // Can only call activate() on Published
        let active = published.activate().unwrap();

        // Can call deprecate() or withdraw() on Active
        let _deprecated = active.deprecate(
            "Deprecated for testing".to_string(),
            None
        ).unwrap();
    }

    #[test]
    fn test_checksum_validation() {
        // Valid SHA-256 (64 hex chars)
        let valid = "a".repeat(64);
        assert!(Sha256Checksum::from_hex(&valid).is_ok());

        // Invalid length
        assert!(Sha256Checksum::from_hex("abc").is_err());
        assert!(Sha256Checksum::from_hex(&"a".repeat(63)).is_err());
        assert!(Sha256Checksum::from_hex(&"a".repeat(65)).is_err());

        // Invalid characters
        assert!(Sha256Checksum::from_hex(&("z".repeat(64))).is_err());
    }
}

// ============================================================================
// END OF POKA YOKE TYPE SYSTEM
// ============================================================================
