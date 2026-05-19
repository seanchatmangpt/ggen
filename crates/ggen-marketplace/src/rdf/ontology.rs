//! RDF Ontology Definitions for mcpp Marketplace
//!
//! This module defines the complete RDF ontology for the marketplace,
//! including all classes, properties, and relationships using standard
//! vocabularies (FOAF, Dublin Core, PROV-O, etc.) plus custom extensions.

use std::fmt;

/// Namespace prefix definitions
pub mod namespaces {
    pub const RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    pub const RDFS: &str = "http://www.w3.org/2000/01/rdf-schema#";
    pub const XSD: &str = "http://www.w3.org/2001/XMLSchema#";
    pub const OWL: &str = "http://www.w3.org/2002/07/owl#";
    pub const FOAF: &str = "http://xmlns.com/foaf/0.1/";
    pub const DC: &str = "http://purl.org/dc/terms/";
    pub const PROV: &str = "http://www.w3.org/ns/prov#";
    pub const SHACL: &str = "http://www.w3.org/ns/shacl#";
    pub const DOAP: &str = "http://usefulinc.com/ns/doap#";
    pub const SPDX: &str = "http://spdx.org/rdf/terms#";

    // Custom mcpp namespace
    pub const GGEN: &str = "http://mcpp.dev/ontology#";
    pub const MARKETPLACE: &str = "http://mcpp.dev/marketplace#";
}

// Re-export commonly used namespaces
pub use namespaces::{GGEN as GGEN_NS, MARKETPLACE as MARKETPLACE_NS};

/// Ontology helper type
pub struct Ontology;

impl Ontology {
    /// Get the GGEN namespace
    #[must_use]
    pub fn mcpp_ns() -> &'static str {
        namespaces::GGEN
    }

    /// Get the marketplace namespace
    #[must_use]
    pub fn marketplace_ns() -> &'static str {
        namespaces::MARKETPLACE
    }
}

/// URI builder helper
pub struct UriBuilder;

impl UriBuilder {
    /// Build a GGEN namespace URI
    #[must_use]
    pub fn mcpp(resource: &str) -> String {
        format!("{}{}", namespaces::GGEN, resource)
    }

    /// Build a marketplace namespace URI
    #[must_use]
    pub fn marketplace(resource: &str) -> String {
        format!("{}{}", namespaces::MARKETPLACE, resource)
    }
}

/// RDF Classes in the mcpp ontology
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Class {
    // Core package types
    Package,
    PackageVersion,
    Template,
    Component,
    Dependency,

    // Metadata types
    Author,
    License,
    Category,
    Tag,

    // State and lifecycle
    InstallationState,
    ValidationResult,
    RegistryEntry,

    // Provenance
    PublishEvent,
    InstallEvent,
    UpdateEvent,
    ValidationEvent,

    // Configuration
    MarketplaceConfig,
    RegistryConfig,
    ValidationRule,
    StateMachine,

    // Security and attestation
    SecurityAttestation,
    SignatureRecord,
    AuditRecord,
}

impl Class {
    #[must_use]
    pub fn uri(&self) -> String {
        format!("{}{}", namespaces::GGEN, self.local_name())
    }

    #[must_use]
    pub fn local_name(&self) -> &'static str {
        match self {
            Self::Package => "Package",
            Self::PackageVersion => "PackageVersion",
            Self::Template => "Template",
            Self::Component => "Component",
            Self::Dependency => "Dependency",
            Self::Author => "Author",
            Self::License => "License",
            Self::Category => "Category",
            Self::Tag => "Tag",
            Self::InstallationState => "InstallationState",
            Self::ValidationResult => "ValidationResult",
            Self::RegistryEntry => "RegistryEntry",
            Self::PublishEvent => "PublishEvent",
            Self::InstallEvent => "InstallEvent",
            Self::UpdateEvent => "UpdateEvent",
            Self::ValidationEvent => "ValidationEvent",
            Self::MarketplaceConfig => "MarketplaceConfig",
            Self::RegistryConfig => "RegistryConfig",
            Self::ValidationRule => "ValidationRule",
            Self::StateMachine => "StateMachine",
            Self::SecurityAttestation => "SecurityAttestation",
            Self::SignatureRecord => "SignatureRecord",
            Self::AuditRecord => "AuditRecord",
        }
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.uri())
    }
}

/// RDF Properties in the mcpp ontology
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Property {
    // Package properties
    PackageName,
    PackageVersion,
    PackageDescription,
    PackageHomepage,
    PackageRepository,
    PackageReadme,
    PackageLicense,
    PackageKeywords,

    // Relationship properties
    HasVersion,
    HasDependency,
    HasAuthor,
    HasCategory,
    HasTag,
    HasTemplate,
    HasComponent,

    // Dependency properties
    DependsOn,
    DependencyVersion,
    DependencyType,
    IsOptional,

    // Version properties
    VersionNumber,
    PublishedAt,
    Deprecated,
    Yanked,

    // State properties
    InstallationStatus,
    ValidationStatus,
    LastValidated,

    // Provenance properties (PROV-O)
    WasGeneratedBy,
    WasAttributedTo,
    WasDerivedFrom,
    AtTime,

    // Security properties
    HasSignature,
    HasAttestation,
    SignatureAlgorithm,
    PublicKey,

    // Configuration properties
    ConfigKey,
    ConfigValue,
    Priority,
    Enabled,

    // SHACL validation
    TargetClass,
    PropertyPath,
    MinCount,
    MaxCount,
    Datatype,
    Pattern,

    // Statistics
    DownloadCount,
    Rating,
    LastUpdated,
}

impl Property {
    #[must_use]
    pub fn uri(&self) -> String {
        match self {
            // Use standard vocabularies where appropriate
            Self::PackageName => format!("{}title", namespaces::DC),
            Self::PackageDescription => format!("{}description", namespaces::DC),
            Self::PackageHomepage => format!("{}homepage", namespaces::FOAF),
            Self::HasAuthor => format!("{}maker", namespaces::FOAF),
            Self::PublishedAt => format!("{}created", namespaces::DC),
            Self::PackageLicense => format!("{}license", namespaces::DC),
            Self::WasGeneratedBy => format!("{}wasGeneratedBy", namespaces::PROV),
            Self::WasAttributedTo => format!("{}wasAttributedTo", namespaces::PROV),
            Self::WasDerivedFrom => format!("{}wasDerivedFrom", namespaces::PROV),
            Self::AtTime => format!("{}atTime", namespaces::PROV),

            // SHACL properties
            Self::TargetClass => format!("{}targetClass", namespaces::SHACL),
            Self::PropertyPath => format!("{}path", namespaces::SHACL),
            Self::MinCount => format!("{}minCount", namespaces::SHACL),
            Self::MaxCount => format!("{}maxCount", namespaces::SHACL),
            Self::Datatype => format!("{}datatype", namespaces::SHACL),
            Self::Pattern => format!("{}pattern", namespaces::SHACL),

            // Custom mcpp properties
            _ => format!("{}{}", namespaces::GGEN, self.local_name()),
        }
    }

    #[must_use]
    pub fn local_name(&self) -> &'static str {
        match self {
            Self::PackageName => "packageName",
            Self::PackageVersion => "packageVersion",
            Self::PackageDescription => "description",
            Self::PackageHomepage => "homepage",
            Self::PackageRepository => "repository",
            Self::PackageReadme => "readme",
            Self::PackageLicense => "license",
            Self::PackageKeywords => "keywords",
            Self::HasVersion => "hasVersion",
            Self::HasDependency => "hasDependency",
            Self::HasAuthor => "hasAuthor",
            Self::HasCategory => "hasCategory",
            Self::HasTag => "hasTag",
            Self::HasTemplate => "hasTemplate",
            Self::HasComponent => "hasComponent",
            Self::DependsOn => "dependsOn",
            Self::DependencyVersion => "dependencyVersion",
            Self::DependencyType => "dependencyType",
            Self::IsOptional => "isOptional",
            Self::VersionNumber => "versionNumber",
            Self::PublishedAt => "publishedAt",
            Self::Deprecated => "deprecated",
            Self::Yanked => "yanked",
            Self::InstallationStatus => "installationStatus",
            Self::ValidationStatus => "validationStatus",
            Self::LastValidated => "lastValidated",
            Self::WasGeneratedBy => "wasGeneratedBy",
            Self::WasAttributedTo => "wasAttributedTo",
            Self::WasDerivedFrom => "wasDerivedFrom",
            Self::AtTime => "atTime",
            Self::HasSignature => "hasSignature",
            Self::HasAttestation => "hasAttestation",
            Self::SignatureAlgorithm => "signatureAlgorithm",
            Self::PublicKey => "publicKey",
            Self::ConfigKey => "configKey",
            Self::ConfigValue => "configValue",
            Self::Priority => "priority",
            Self::Enabled => "enabled",
            Self::TargetClass => "targetClass",
            Self::PropertyPath => "path",
            Self::MinCount => "minCount",
            Self::MaxCount => "maxCount",
            Self::Datatype => "datatype",
            Self::Pattern => "pattern",
            Self::DownloadCount => "downloadCount",
            Self::Rating => "rating",
            Self::LastUpdated => "lastUpdated",
        }
    }
}

impl fmt::Display for Property {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.uri())
    }
}

/// XSD Datatypes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XsdType {
    String,
    Integer,
    Boolean,
    DateTime,
    AnyURI,
    Decimal,
}

impl XsdType {
    #[must_use]
    pub fn uri(&self) -> String {
        format!("{}{}", namespaces::XSD, self.local_name())
    }

    #[must_use]
    pub fn local_name(&self) -> &'static str {
        match self {
            Self::String => "string",
            Self::Integer => "integer",
            Self::Boolean => "boolean",
            Self::DateTime => "dateTime",
            Self::AnyURI => "anyURI",
            Self::Decimal => "decimal",
        }
    }
}

/// Generate Turtle prefix declarations
#[must_use]
pub fn generate_prefixes() -> String {
    format!(
        r"@prefix rdf: <{}> .
@prefix rdfs: <{}> .
@prefix xsd: <{}> .
@prefix owl: <{}> .
@prefix foaf: <{}> .
@prefix dc: <{}> .
@prefix prov: <{}> .
@prefix sh: <{}> .
@prefix doap: <{}> .
@prefix spdx: <{}> .
@prefix mcpp: <{}> .

",
        namespaces::RDF,
        namespaces::RDFS,
        namespaces::XSD,
        namespaces::OWL,
        namespaces::FOAF,
        namespaces::DC,
        namespaces::PROV,
        namespaces::SHACL,
        namespaces::DOAP,
        namespaces::SPDX,
        namespaces::GGEN,
    )
}

/// Generate complete ontology definition in Turtle
#[must_use]
#[allow(clippy::too_many_lines)]
pub fn generate_ontology_definition() -> String {
    let mut ttl = generate_prefixes();

    ttl.push_str(
        r#"
# Ontology metadata
mcpp: a owl:Ontology ;
    dc:title "mcpp Marketplace Ontology" ;
    dc:description "RDF ontology for the mcpp template marketplace system" ;
    dc:created "2025-01-18"^^xsd:date ;
    owl:versionInfo "2.0.0" .

# Class definitions
mcpp:Package a owl:Class ;
    rdfs:label "Package" ;
    rdfs:comment "A reusable code template package" .

mcpp:PackageVersion a owl:Class ;
    rdfs:label "Package Version" ;
    rdfs:comment "A specific version of a package" .

mcpp:Template a owl:Class ;
    rdfs:label "Template" ;
    rdfs:comment "A code template within a package" .

mcpp:Component a owl:Class ;
    rdfs:label "Component" ;
    rdfs:comment "A reusable component" .

mcpp:Dependency a owl:Class ;
    rdfs:label "Dependency" ;
    rdfs:comment "A package dependency relationship" .

mcpp:Author a owl:Class ;
    rdfs:subClassOf foaf:Person ;
    rdfs:label "Author" ;
    rdfs:comment "Package author or maintainer" .

mcpp:License a owl:Class ;
    rdfs:label "License" ;
    rdfs:comment "Software license information" .

mcpp:Category a owl:Class ;
    rdfs:label "Category" ;
    rdfs:comment "Package category or classification" .

mcpp:Tag a owl:Class ;
    rdfs:label "Tag" ;
    rdfs:comment "Keyword tag for searchability" .

mcpp:InstallationState a owl:Class ;
    rdfs:label "Installation State" ;
    rdfs:comment "Current installation status of a package" .

mcpp:ValidationResult a owl:Class ;
    rdfs:label "Validation Result" ;
    rdfs:comment "Result of package validation" .

mcpp:RegistryEntry a owl:Class ;
    rdfs:label "Registry Entry" ;
    rdfs:comment "Entry in the package registry" .

# Event classes (PROV-O based)
mcpp:PublishEvent a owl:Class ;
    rdfs:subClassOf prov:Activity ;
    rdfs:label "Publish Event" ;
    rdfs:comment "Package publication activity" .

mcpp:InstallEvent a owl:Class ;
    rdfs:subClassOf prov:Activity ;
    rdfs:label "Install Event" ;
    rdfs:comment "Package installation activity" .

mcpp:UpdateEvent a owl:Class ;
    rdfs:subClassOf prov:Activity ;
    rdfs:label "Update Event" ;
    rdfs:comment "Package update activity" .

mcpp:ValidationEvent a owl:Class ;
    rdfs:subClassOf prov:Activity ;
    rdfs:label "Validation Event" ;
    rdfs:comment "Package validation activity" .

# Configuration classes
mcpp:MarketplaceConfig a owl:Class ;
    rdfs:label "Marketplace Configuration" ;
    rdfs:comment "Marketplace system configuration" .

mcpp:RegistryConfig a owl:Class ;
    rdfs:label "Registry Configuration" ;
    rdfs:comment "Registry endpoint configuration" .

mcpp:ValidationRule a owl:Class ;
    rdfs:subClassOf sh:NodeShape ;
    rdfs:label "Validation Rule" ;
    rdfs:comment "SHACL validation rule" .

mcpp:StateMachine a owl:Class ;
    rdfs:label "State Machine" ;
    rdfs:comment "Finite state machine definition" .

# Security classes
mcpp:SecurityAttestation a owl:Class ;
    rdfs:label "Security Attestation" ;
    rdfs:comment "Security attestation record" .

mcpp:SignatureRecord a owl:Class ;
    rdfs:label "Signature Record" ;
    rdfs:comment "Cryptographic signature record" .

mcpp:AuditRecord a owl:Class ;
    rdfs:label "Audit Record" ;
    rdfs:comment "Audit trail record" .

# Property definitions
mcpp:hasVersion a owl:ObjectProperty ;
    rdfs:domain mcpp:Package ;
    rdfs:range mcpp:PackageVersion ;
    rdfs:label "has version" .

mcpp:hasDependency a owl:ObjectProperty ;
    rdfs:domain mcpp:PackageVersion ;
    rdfs:range mcpp:Dependency ;
    rdfs:label "has dependency" .

mcpp:dependsOn a owl:ObjectProperty ;
    rdfs:domain mcpp:Dependency ;
    rdfs:range mcpp:Package ;
    rdfs:label "depends on" .

mcpp:hasTemplate a owl:ObjectProperty ;
    rdfs:domain mcpp:Package ;
    rdfs:range mcpp:Template ;
    rdfs:label "has template" .

mcpp:hasComponent a owl:ObjectProperty ;
    rdfs:domain mcpp:Package ;
    rdfs:range mcpp:Component ;
    rdfs:label "has component" .

mcpp:hasCategory a owl:ObjectProperty ;
    rdfs:domain mcpp:Package ;
    rdfs:range mcpp:Category ;
    rdfs:label "has category" .

mcpp:hasTag a owl:ObjectProperty ;
    rdfs:domain mcpp:Package ;
    rdfs:range mcpp:Tag ;
    rdfs:label "has tag" .

mcpp:versionNumber a owl:DatatypeProperty ;
    rdfs:domain mcpp:PackageVersion ;
    rdfs:range xsd:string ;
    rdfs:label "version number" .

mcpp:deprecated a owl:DatatypeProperty ;
    rdfs:domain mcpp:PackageVersion ;
    rdfs:range xsd:boolean ;
    rdfs:label "deprecated" .

mcpp:yanked a owl:DatatypeProperty ;
    rdfs:domain mcpp:PackageVersion ;
    rdfs:range xsd:boolean ;
    rdfs:label "yanked" .

mcpp:installationStatus a owl:DatatypeProperty ;
    rdfs:domain mcpp:InstallationState ;
    rdfs:range xsd:string ;
    rdfs:label "installation status" .

mcpp:validationStatus a owl:DatatypeProperty ;
    rdfs:domain mcpp:ValidationResult ;
    rdfs:range xsd:string ;
    rdfs:label "validation status" .

mcpp:downloadCount a owl:DatatypeProperty ;
    rdfs:domain mcpp:Package ;
    rdfs:range xsd:integer ;
    rdfs:label "download count" .

mcpp:rating a owl:DatatypeProperty ;
    rdfs:domain mcpp:Package ;
    rdfs:range xsd:decimal ;
    rdfs:label "rating" .

mcpp:repository a owl:DatatypeProperty ;
    rdfs:domain mcpp:Package ;
    rdfs:range xsd:anyURI ;
    rdfs:label "repository" .

mcpp:readme a owl:DatatypeProperty ;
    rdfs:domain mcpp:Package ;
    rdfs:range xsd:string ;
    rdfs:label "readme" .

mcpp:dependencyVersion a owl:DatatypeProperty ;
    rdfs:domain mcpp:Dependency ;
    rdfs:range xsd:string ;
    rdfs:label "dependency version" .

mcpp:dependencyType a owl:DatatypeProperty ;
    rdfs:domain mcpp:Dependency ;
    rdfs:range xsd:string ;
    rdfs:label "dependency type" .

mcpp:isOptional a owl:DatatypeProperty ;
    rdfs:domain mcpp:Dependency ;
    rdfs:range xsd:boolean ;
    rdfs:label "is optional" .

mcpp:hasSignature a owl:ObjectProperty ;
    rdfs:domain mcpp:PackageVersion ;
    rdfs:range mcpp:SignatureRecord ;
    rdfs:label "has signature" .

mcpp:hasAttestation a owl:ObjectProperty ;
    rdfs:domain mcpp:PackageVersion ;
    rdfs:range mcpp:SecurityAttestation ;
    rdfs:label "has attestation" .

mcpp:signatureAlgorithm a owl:DatatypeProperty ;
    rdfs:domain mcpp:SignatureRecord ;
    rdfs:range xsd:string ;
    rdfs:label "signature algorithm" .

mcpp:publicKey a owl:DatatypeProperty ;
    rdfs:domain mcpp:SignatureRecord ;
    rdfs:range xsd:string ;
    rdfs:label "public key" .

"#,
    );

    ttl
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_class_uris() {
        assert_eq!(Class::Package.uri(), "http://mcpp.dev/ontology#Package");
        assert_eq!(
            Class::PackageVersion.uri(),
            "http://mcpp.dev/ontology#PackageVersion"
        );
    }

    #[test]
    fn test_property_uris() {
        assert_eq!(
            Property::PackageName.uri(),
            "http://purl.org/dc/terms/title"
        );
        assert_eq!(
            Property::HasVersion.uri(),
            "http://mcpp.dev/ontology#hasVersion"
        );
    }

    #[test]
    fn test_xsd_types() {
        assert_eq!(
            XsdType::String.uri(),
            "http://www.w3.org/2001/XMLSchema#string"
        );
        assert_eq!(
            XsdType::DateTime.uri(),
            "http://www.w3.org/2001/XMLSchema#dateTime"
        );
    }

    #[test]
    fn test_ontology_generation() {
        let ttl = generate_ontology_definition();
        assert!(ttl.contains("@prefix mcpp:"));
        assert!(ttl.contains("mcpp:Package a owl:Class"));
        assert!(ttl.contains("mcpp:hasVersion a owl:ObjectProperty"));
    }
}
