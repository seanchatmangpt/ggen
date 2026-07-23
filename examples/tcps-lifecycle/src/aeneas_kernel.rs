//! Safe, sequential, dependency-free kernel intended for Charon/Aeneas extraction.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum State {
    Observed,
    Admitted,
    Planned,
    Authorized,
    Receipted,
    Stopped,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Decision {
    Admit,
    Plan,
    Authorize,
    ExecuteGreen,
    ExecuteAbnormal,
    RecoverWithUpdatedStandard,
}

#[must_use]
pub const fn step(state: State, decision: Decision) -> Option<State> {
    match (state, decision) {
        (State::Observed, Decision::Admit) => Some(State::Admitted),
        (State::Admitted, Decision::Plan) => Some(State::Planned),
        (State::Planned, Decision::Authorize) => Some(State::Authorized),
        (State::Authorized, Decision::ExecuteGreen) => Some(State::Receipted),
        (State::Authorized, Decision::ExecuteAbnormal) => Some(State::Stopped),
        (State::Stopped, Decision::RecoverWithUpdatedStandard) => Some(State::Observed),
        _ => None,
    }
}

#[must_use]
pub const fn successful(state: State) -> bool {
    matches!(state, State::Receipted)
}
