//! Trust tier and registry classification for governed marketplace.
//!
//! Implements Fortune 5 CISO requirements for enterprise trust boundaries.

use serde::{Deserialize, Serialize};

/// Trust tier classification for packs.
///
/// Based on Fortune 5 CISO security requirements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
pub enum TrustTier {
    /// Full audit, signed, approved for enterprise production use
    EnterpriseCertified,

    /// Reviewed and allowlisted for enterprise use
    EnterpriseApproved,

    /// Community-reviewed, not yet enterprise-approved
    CommunityReviewed,

    /// Production-ready but not enterprise-certified
    ProductionReady,

    /// Restricted use, under monitoring, not yet fully approved
    Quarantined,

    /// Development/testing only, not for production
    #[default]
    Experimental,

    /// Forbidden by policy, blocked from installation
    Blocked,
}

impl TrustTier {
    /// Check if this tier allows installation in regulated environments
    #[must_use]
    pub const fn is_regulated_compliant(self) -> bool {
        matches!(self, Self::EnterpriseCertified | Self::EnterpriseApproved)
    }

    /// Check if this tier allows installation in production
    #[must_use]
    pub const fn is_production_ready(self) -> bool {
        matches!(
            self,
            Self::EnterpriseCertified | Self::EnterpriseApproved | Self::ProductionReady
        )
    }

    /// Get the numeric priority for tier comparison (lower = more restrictive)
    #[must_use]
    pub const fn priority(self) -> u8 {
        match self {
            Self::EnterpriseCertified => 1,
            Self::EnterpriseApproved => 2,
            Self::CommunityReviewed => 3,
            Self::ProductionReady => 4,
            Self::Quarantined => 5,
            Self::Experimental => 6,
            Self::Blocked => 7,
        }
    }

    /// Check if this tier meets or exceeds the required minimum
    #[must_use]
    pub fn meets_requirement(self, required: Self) -> bool {
        self.priority() <= required.priority()
    }
}

/// Registry class for pack distribution.
///
/// CISO requirement: Cargo is transport, not trust.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RegistryClass {
    /// Public registry (crates.io, public npm, etc.)
    ///
    /// Transport only, not trusted by default.
    Public { url: String },

    /// Private enterprise registry
    ///
    /// Internal registry with enterprise trust policies.
    PrivateEnterprise {
        url: String,
        require_signature: bool,
        allow_unlisted: bool,
    },

    /// Air-gapped mirrored registry
    ///
    /// For regulated environments with no internet access.
    MirroredAirGapped {
        primary_url: String,
        mirror_path: std::path::PathBuf,
        sync_interval_seconds: u64,
    },
}

impl RegistryClass {
    /// Get the base URL for this registry
    #[must_use]
    pub fn url(&self) -> &str {
        match self {
            Self::Public { url } => url,
            Self::PrivateEnterprise { url, .. } => url,
            Self::MirroredAirGapped { primary_url, .. } => primary_url,
        }
    }

    /// Check if this registry requires signature verification
    #[must_use]
    pub fn requires_signature(&self) -> bool {
        match self {
            Self::Public { .. } => false,
            Self::PrivateEnterprise {
                require_signature, ..
            } => *require_signature,
            Self::MirroredAirGapped { .. } => true,
        }
    }

    /// Check if this registry allows unlisted packages
    #[must_use]
    pub fn allows_unlisted(&self) -> bool {
        match self {
            Self::Public { .. } => true,
            Self::PrivateEnterprise { allow_unlisted, .. } => *allow_unlisted,
            Self::MirroredAirGapped { .. } => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trust_tier_compliance() {
        assert!(TrustTier::EnterpriseCertified.is_regulated_compliant());
        assert!(TrustTier::EnterpriseApproved.is_regulated_compliant());
        assert!(!TrustTier::Experimental.is_regulated_compliant());
        assert!(!TrustTier::Blocked.is_regulated_compliant());
    }

    #[test]
    fn test_trust_tier_priority() {
        assert!(TrustTier::EnterpriseCertified.priority() < TrustTier::Experimental.priority());
        assert!(TrustTier::Experimental.priority() < TrustTier::Blocked.priority());
    }

    #[test]
    fn test_trust_tier_meets_requirement() {
        // EnterpriseCertified meets all requirements
        assert!(TrustTier::EnterpriseCertified.meets_requirement(TrustTier::EnterpriseApproved));
        assert!(TrustTier::EnterpriseCertified.meets_requirement(TrustTier::EnterpriseCertified));

        // Experimental does not meet enterprise requirements
        assert!(!TrustTier::Experimental.meets_requirement(TrustTier::EnterpriseApproved));

        // Blocked meets only blocked requirement
        assert!(!TrustTier::Blocked.meets_requirement(TrustTier::Experimental));
    }

    #[test]
    fn test_registry_class_signature_requirement() {
        let public = RegistryClass::Public {
            url: "https://crates.io".to_string(),
        };
        assert!(!public.requires_signature());

        let private = RegistryClass::PrivateEnterprise {
            url: "https://registry.internal.com".to_string(),
            require_signature: true,
            allow_unlisted: false,
        };
        assert!(private.requires_signature());
    }
}
