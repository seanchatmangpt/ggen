pub mod patterns {
    pub mod ownership { pub fn demo() { let _x = String::from("owned"); } }
    pub mod traits { pub trait Demo { fn demo(&self); } }
    pub mod generics { pub fn demo<T: std::fmt::Debug>(x: T) { println!("{:?}", x); } }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_ownership() { crate::patterns::ownership::demo(); }
    #[test]
    fn test_traits() { assert!(true); }
    #[test]
    fn test_generics() { crate::patterns::generics::demo(42); }
}
