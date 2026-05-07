//! Phases of the semantic lifecycle

pub struct Input;
pub struct Validated;
pub struct Selected<C>(core::marker::PhantomData<C>);
pub struct Admitted<C>(core::marker::PhantomData<C>);
pub struct Receipted<R>(core::marker::PhantomData<R>);
pub struct Exited<E>(core::marker::PhantomData<E>);
pub struct Replayable;
