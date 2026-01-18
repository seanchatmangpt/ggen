//! RDF Ontology Definitions for ggen Marketplace
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

    // Custom ggen namespace
    pub const GGEN: &str = "http://ggen.dev/ontology#";
    pub const MARKETPLACE: &str = "http://ggen.dev/marketplace#";
}

// Re-export commonly used namespaces
pub use namespaces::{GGEN as GGEN_NS, MARKETPLACE as MARKETPLACE_NS};

/// Ontology helper type
pub struct Ontology;

impl Ontology {
    /// Get the GGEN namespace
    pub fn ggen_ns() -> &'static str {
        namespaces::GGEN
    }

    /// Get the marketplace namespace
    pub fn marketplace_ns() -> &'static str {
        namespaces::MARKETPLACE
    }
}

/// URI builder helper
pub struct UriBuilder;

impl UriBuilder {
    /// Build a GGEN namespace URI
    pub fn ggen(resource: &str) -> String {
        format!("{}{}", namespaces::GGEN, resource)
    }

    /// Build a marketplace namespace URI
    pub fn marketplace(resource: &str) -> String {
        format!("{}{}", namespaces::MARKETPLACE, resource)
    }
}

/// RDF Classes in the ggen ontology
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
    pub fn uri(&self) -> String {
        format!("{}{}", namespaces::GGEN, self.local_name())
    }

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

/// RDF Properties in the ggen ontology
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

            // Custom ggen properties
            _ => format!("{}{}", namespaces::GGEN, self.local_name()),
        }
    }

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
    pub fn uri(&self) -> String {
        format!("{}{}", namespaces::XSD, self.local_name())
    }

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
@prefix ggen: <{}> .

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
pub fn generate_ontology_definition() -> String {
    let mut ttl = generate_prefixes();

    ttl.push_str(
        r#"
# Ontology metadata
ggen: a owl:Ontology ;
    dc:title "ggen Marketplace Ontology" ;
    dc:description "RDF ontology for the ggen template marketplace system" ;
    dc:created "2025-01-18"^^xsd:date ;
    owl:versionInfo "2.0.0" .

# Class definitions
ggen:Package a owl:Class ;
    rdfs:label "Package" ;
    rdfs:comment "A reusable code template package" .

ggen:PackageVersion a owl:Class ;
    rdfs:label "Package Version" ;
    rdfs:comment "A specific version of a package" .

ggen:Template a owl:Class ;
    rdfs:label "Template" ;
    rdfs:comment "A code template within a package" .

ggen:Component a owl:Class ;
    rdfs:label "Component" ;
    rdfs:comment "A reusable component" .

ggen:Dependency a owl:Class ;
    rdfs:label "Dependency" ;
    rdfs:comment "A package dependency relationship" .

ggen:Author a owl:Class ;
    rdfs:subClassOf foaf:Person ;
    rdfs:label "Author" ;
    rdfs:comment "Package author or maintainer" .

ggen:License a owl:Class ;
    rdfs:label "License" ;
    rdfs:comment "Software license information" .

ggen:Category a owl:Class ;
    rdfs:label "Category" ;
    rdfs:comment "Package category or classification" .

ggen:Tag a owl:Class ;
    rdfs:label "Tag" ;
    rdfs:comment "Keyword tag for searchability" .

ggen:InstallationState a owl:Class ;
    rdfs:label "Installation State" ;
    rdfs:comment "Current installation status of a package" .

ggen:ValidationResult a owl:Class ;
    rdfs:label "Validation Result" ;
    rdfs:comment "Result of package validation" .

ggen:RegistryEntry a owl:Class ;
    rdfs:label "Registry Entry" ;
    rdfs:comment "Entry in the package registry" .

# Event classes (PROV-O based)
ggen:PublishEvent a owl:Class ;
    rdfs:subClassOf prov:Activity ;
    rdfs:label "Publish Event" ;
    rdfs:comment "Package publication activity" .

ggen:InstallEvent a owl:Class ;
    rdfs:subClassOf prov:Activity ;
    rdfs:label "Install Event" ;
    rdfs:comment "Package installation activity" .

ggen:UpdateEvent a owl:Class ;
    rdfs:subClassOf prov:Activity ;
    rdfs:label "Update Event" ;
    rdfs:comment "Package update activity" .

ggen:ValidationEvent a owl:Class ;
    rdfs:subClassOf prov:Activity ;
    rdfs:label "Validation Event" ;
    rdfs:comment "Package validation activity" .

# Configuration classes
ggen:MarketplaceConfig a owl:Class ;
    rdfs:label "Marketplace Configuration" ;
    rdfs:comment "Marketplace system configuration" .

ggen:RegistryConfig a owl:Class ;
    rdfs:label "Registry Configuration" ;
    rdfs:comment "Registry endpoint configuration" .

ggen:ValidationRule a owl:Class ;
    rdfs:subClassOf sh:NodeShape ;
    rdfs:label "Validation Rule" ;
    rdfs:comment "SHACL validation rule" .

ggen:StateMachine a owl:Class ;
    rdfs:label "State Machine" ;
    rdfs:comment "Finite state machine definition" .

# Security classes
ggen:SecurityAttestation a owl:Class ;
    rdfs:label "Security Attestation" ;
    rdfs:comment "Security attestation record" .

ggen:SignatureRecord a owl:Class ;
    rdfs:label "Signature Record" ;
    rdfs:comment "Cryptographic signature record" .

ggen:AuditRecord a owl:Class ;
    rdfs:label "Audit Record" ;
    rdfs:comment "Audit trail record" .

# Property definitions
ggen:hasVersion a owl:ObjectProperty ;
    rdfs:domain ggen:Package ;
    rdfs:range ggen:PackageVersion ;
    rdfs:label "has version" .

ggen:hasDependency a owl:ObjectProperty ;
    rdfs:domain ggen:PackageVersion ;
    rdfs:range ggen:Dependency ;
    rdfs:label "has dependency" .

ggen:dependsOn a owl:ObjectProperty ;
    rdfs:domain ggen:Dependency ;
    rdfs:range ggen:Package ;
    rdfs:label "depends on" .

ggen:hasTemplate a owl:ObjectProperty ;
    rdfs:domain ggen:Package ;
    rdfs:range ggen:Template ;
    rdfs:label "has template" .

ggen:hasComponent a owl:ObjectProperty ;
    rdfs:domain ggen:Package ;
    rdfs:range ggen:Component ;
    rdfs:label "has component" .

ggen:hasCategory a owl:ObjectProperty ;
    rdfs:domain ggen:Package ;
    rdfs:range ggen:Category ;
    rdfs:label "has category" .

ggen:hasTag a owl:ObjectProperty ;
    rdfs:domain ggen:Package ;
    rdfs:range ggen:Tag ;
    rdfs:label "has tag" .

ggen:versionNumber a owl:DatatypeProperty ;
    rdfs:domain ggen:PackageVersion ;
    rdfs:range xsd:string ;
    rdfs:label "version number" .

ggen:deprecated a owl:DatatypeProperty ;
    rdfs:domain ggen:PackageVersion ;
    rdfs:range xsd:boolean ;
    rdfs:label "deprecated" .

ggen:yanked a owl:DatatypeProperty ;
    rdfs:domain ggen:PackageVersion ;
    rdfs:range xsd:boolean ;
    rdfs:label "yanked" .

ggen:installationStatus a owl:DatatypeProperty ;
    rdfs:domain ggen:InstallationState ;
    rdfs:range xsd:string ;
    rdfs:label "installation status" .

ggen:validationStatus a owl:DatatypeProperty ;
    rdfs:domain ggen:ValidationResult ;
    rdfs:range xsd:string ;
    rdfs:label "validation status" .

ggen:downloadCount a owl:DatatypeProperty ;
    rdfs:domain ggen:Package ;
    rdfs:range xsd:integer ;
    rdfs:label "download count" .

ggen:rating a owl:DatatypeProperty ;
    rdfs:domain ggen:Package ;
    rdfs:range xsd:decimal ;
    rdfs:label "rating" .

ggen:repository a owl:DatatypeProperty ;
    rdfs:domain ggen:Package ;
    rdfs:range xsd:anyURI ;
    rdfs:label "repository" .

ggen:readme a owl:DatatypeProperty ;
    rdfs:domain ggen:Package ;
    rdfs:range xsd:string ;
    rdfs:label "readme" .

ggen:dependencyVersion a owl:DatatypeProperty ;
    rdfs:domain ggen:Dependency ;
    rdfs:range xsd:string ;
    rdfs:label "dependency version" .

ggen:dependencyType a owl:DatatypeProperty ;
    rdfs:domain ggen:Dependency ;
    rdfs:range xsd:string ;
    rdfs:label "dependency type" .

ggen:isOptional a owl:DatatypeProperty ;
    rdfs:domain ggen:Dependency ;
    rdfs:range xsd:boolean ;
    rdfs:label "is optional" .

ggen:hasSignature a owl:ObjectProperty ;
    rdfs:domain ggen:PackageVersion ;
    rdfs:range ggen:SignatureRecord ;
    rdfs:label "has signature" .

ggen:hasAttestation a owl:ObjectProperty ;
    rdfs:domain ggen:PackageVersion ;
    rdfs:range ggen:SecurityAttestation ;
    rdfs:label "has attestation" .

ggen:signatureAlgorithm a owl:DatatypeProperty ;
    rdfs:domain ggen:SignatureRecord ;
    rdfs:range xsd:string ;
    rdfs:label "signature algorithm" .

ggen:publicKey a owl:DatatypeProperty ;
    rdfs:domain ggen:SignatureRecord ;
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
        assert_eq!(Class::Package.uri(), "http://ggen.dev/ontology#Package");
        assert_eq!(
            Class::PackageVersion.uri(),
            "http://ggen.dev/ontology#PackageVersion"
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
            "http://ggen.dev/ontology#hasVersion"
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
        assert!(ttl.contains("@prefix ggen:"));
        assert!(ttl.contains("ggen:Package a owl:Class"));
        assert!(ttl.contains("ggen:hasVersion a owl:ObjectProperty"));
    }
}
