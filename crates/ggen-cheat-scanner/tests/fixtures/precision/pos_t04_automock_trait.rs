// POSITIVE: #[automock] attribute on a trait. Expected verdict: [CHEAT-T04].
#[automock]
pub trait Gateway {
    fn send(&self, payload: &str) -> bool;
}
