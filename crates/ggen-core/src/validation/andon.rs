use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AndonSignal {
    /// 🔴 RED: Error - STOP immediately
    Red,
    /// 🟡 YELLOW: Warning - Investigate
    Yellow,
    /// 🟢 GREEN: Success - Continue
    Green,
}

impl AndonSignal {
    pub fn emoji(&self) -> &'static str {
        match self {
            AndonSignal::Red => "🔴",
            AndonSignal::Yellow => "🟡",
            AndonSignal::Green => "🟢",
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            AndonSignal::Red => "RED",
            AndonSignal::Yellow => "YELLOW",
            AndonSignal::Green => "GREEN",
        }
    }

    pub fn meaning(&self) -> &'static str {
        match self {
            AndonSignal::Red => "Error - STOP immediately",
            AndonSignal::Yellow => "Warning - Investigate",
            AndonSignal::Green => "Success - Continue",
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, AndonSignal::Red)
    }

    pub fn is_warning(&self) -> bool {
        matches!(self, AndonSignal::Yellow)
    }

    pub fn is_success(&self) -> bool {
        matches!(self, AndonSignal::Green)
    }
}

impl fmt::Display for AndonSignal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} ({})", self.emoji(), self.name(), self.meaning())
    }
}

pub struct AndonContext {
    pub signal: AndonSignal,
    pub message: String,
    pub source: String,
}

impl AndonContext {
    pub fn new(signal: AndonSignal, message: impl Into<String>, source: impl Into<String>) -> Self {
        Self {
            signal,
            message: message.into(),
            source: source.into(),
        }
    }

    pub fn red(message: impl Into<String>, source: impl Into<String>) -> Self {
        Self::new(AndonSignal::Red, message, source)
    }

    pub fn yellow(message: impl Into<String>, source: impl Into<String>) -> Self {
        Self::new(AndonSignal::Yellow, message, source)
    }

    pub fn green(message: impl Into<String>, source: impl Into<String>) -> Self {
        Self::new(AndonSignal::Green, message, source)
    }
}

impl fmt::Display for AndonContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [{}] {}: {}",
            self.signal.emoji(),
            self.signal.name(),
            self.source,
            self.message
        )
    }
}
