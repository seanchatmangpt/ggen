//! Typestate markers for compile-time configuration validation
//!
//! This module provides type-level markers that ensure configuration
//! correctness at compile time through Rust's type system.

/// Marker trait for builder states
pub trait BuilderState {}

/// Initial state - no configuration applied
pub struct Initial;
impl BuilderState for Initial {}

/// State after timeout configuration
pub struct WithTimeout;
impl BuilderState for WithTimeout {}

/// State after security policy configuration
pub struct WithSecurity;
impl BuilderState for WithSecurity {}

/// State after resource limits configuration
pub struct WithResources;
impl BuilderState for WithResources {}

/// State after deterministic execution configuration
pub struct WithDeterministic;
impl BuilderState for WithDeterministic {}

/// State ready for building
pub struct Ready;
impl BuilderState for Ready {}

/// State with complete configuration
pub struct Complete;
impl BuilderState for Complete {}

/// Type-level validation for configuration completeness
pub trait ConfigurationComplete: BuilderState {}

impl ConfigurationComplete for Ready {}
impl ConfigurationComplete for Complete {}

/// Type-level validation for security configuration
pub trait SecurityConfigured: BuilderState {}

impl SecurityConfigured for WithSecurity {}
impl SecurityConfigured for WithResources {}
impl SecurityConfigured for WithDeterministic {}
impl SecurityConfigured for Ready {}
impl SecurityConfigured for Complete {}

/// Type-level validation for timeout configuration
pub trait TimeoutConfigured: BuilderState {}

impl TimeoutConfigured for WithTimeout {}
impl TimeoutConfigured for WithSecurity {}
impl TimeoutConfigured for WithResources {}
impl TimeoutConfigured for WithDeterministic {}
impl TimeoutConfigured for Ready {}
impl TimeoutConfigured for Complete {}

/// Type-level validation for resource configuration
pub trait ResourcesConfigured: BuilderState {}

impl ResourcesConfigured for WithResources {}
impl ResourcesConfigured for WithDeterministic {}
impl ResourcesConfigured for Ready {}
impl ResourcesConfigured for Complete {}

/// Type-level validation for deterministic configuration
pub trait DeterministicConfigured: BuilderState {}

impl DeterministicConfigured for WithDeterministic {}
impl DeterministicConfigured for Ready {}
impl DeterministicConfigured for Complete {}

/// Compile-time validation that a state can be built
pub trait CanBuild: BuilderState {}

impl CanBuild for WithTimeout {}
impl CanBuild for WithSecurity {}
impl CanBuild for WithResources {}
impl CanBuild for WithDeterministic {}
impl CanBuild for Ready {}
impl CanBuild for Complete {}

/// State transition validation
pub trait StateTransition<From, To> {
    fn transition_allowed() -> bool;
}

/// Allow transitions from Initial to any state
impl<To: BuilderState> StateTransition<Initial, To> for () {
    fn transition_allowed() -> bool {
        true
    }
}

/// Allow transitions from WithTimeout to security-related states
impl StateTransition<WithTimeout, WithSecurity> for () {
    fn transition_allowed() -> bool {
        true
    }
}

impl StateTransition<WithTimeout, WithResources> for () {
    fn transition_allowed() -> bool {
        true
    }
}

impl StateTransition<WithTimeout, WithDeterministic> for () {
    fn transition_allowed() -> bool {
        true
    }
}

/// Allow transitions from WithSecurity to resource-related states
impl StateTransition<WithSecurity, WithResources> for () {
    fn transition_allowed() -> bool {
        true
    }
}

impl StateTransition<WithSecurity, WithDeterministic> for () {
    fn transition_allowed() -> bool {
        true
    }
}

/// Allow transitions from WithResources to deterministic states
impl StateTransition<WithResources, WithDeterministic> for () {
    fn transition_allowed() -> bool {
        true
    }
}

/// Allow transitions to Ready state from any configured state
impl<From: BuilderState> StateTransition<From, Ready> for () {
    fn transition_allowed() -> bool {
        true
    }
}

/// Allow transitions to Complete state from Ready
impl StateTransition<Ready, Complete> for () {
    fn transition_allowed() -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_transitions() {
        assert!(<() as StateTransition<Initial, WithTimeout>>::transition_allowed());
        assert!(<() as StateTransition<WithTimeout, WithSecurity>>::transition_allowed());
        assert!(<() as StateTransition<WithSecurity, WithResources>>::transition_allowed());
        assert!(<() as StateTransition<WithResources, WithDeterministic>>::transition_allowed());
        assert!(<() as StateTransition<WithDeterministic, Ready>>::transition_allowed());
        assert!(<() as StateTransition<Ready, Complete>>::transition_allowed());
    }

    #[test]
    fn test_configuration_completeness() {
        fn assert_complete<T: ConfigurationComplete>() {}
        
        assert_complete::<Ready>();
        assert_complete::<Complete>();
    }

    #[test]
    fn test_can_build() {
        fn assert_can_build<T: CanBuild>() {}
        
        assert_can_build::<WithTimeout>();
        assert_can_build::<WithSecurity>();
        assert_can_build::<WithResources>();
        assert_can_build::<WithDeterministic>();
        assert_can_build::<Ready>();
        assert_can_build::<Complete>();
    }
}
