//! Interactive REPL for `ggen`.
//!
//! Enabled via the `repl` Cargo feature. Rebuild with `--features repl` to
//! activate. Provides a [`Repl`] struct backed by [`rustyline`] with a
//! [`ReplHelper`] that offers noun/verb tab-completion.

// ---------------------------------------------------------------------------
// Quote-aware shell word splitter (always compiled)
// ---------------------------------------------------------------------------

/// Split `s` into shell words respecting single- and double-quoted spans and
/// backslash escapes.
///
/// Returns `None` if the string contains an unmatched quote.
pub fn split_shell_words(s: &str) -> Option<Vec<String>> {
    let mut words = Vec::new();
    let mut cur = String::new();
    let mut in_double = false;
    let mut in_single = false;
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                cur.push(chars.next()?);
            }
            '"' if !in_single => {
                in_double = !in_double;
            }
            '\'' if !in_double => {
                in_single = !in_single;
            }
            ' ' | '\t' if !in_double && !in_single => {
                if !cur.is_empty() {
                    words.push(std::mem::take(&mut cur));
                }
            }
            c => cur.push(c),
        }
    }
    if in_double || in_single {
        return None; // unmatched quote
    }
    if !cur.is_empty() {
        words.push(cur);
    }
    Some(words)
}

// ---------------------------------------------------------------------------
// feature = "repl"  — full implementation
// ---------------------------------------------------------------------------

#[cfg(feature = "repl")]
mod inner {
    use rustyline::{
        completion::{Completer, Pair},
        error::ReadlineError,
        highlight::Highlighter,
        hint::Hinter,
        validate::Validator,
        CompletionType, Config, Context, Editor, Helper,
    };

    /// Tab-completion helper for noun/verb pairs.
    ///
    /// The completer suggests nouns first; once a noun is present it suggests
    /// matching verbs for that noun.
    pub struct ReplHelper {
        /// `(noun, vec_of_verbs)` pairs registered at construction time.
        noun_verbs: Vec<(&'static str, Vec<&'static str>)>,
    }

    impl ReplHelper {
        /// Create a helper from a slice of `(noun, &[verb])` pairs.
        #[must_use]
        pub fn new(noun_verbs: &[(&'static str, &'static [&'static str])]) -> Self {
            Self {
                noun_verbs: noun_verbs.iter().map(|(n, vs)| (*n, vs.to_vec())).collect(),
            }
        }
    }

    impl Helper for ReplHelper {}
    impl Highlighter for ReplHelper {}
    impl Hinter for ReplHelper {
        type Hint = String;
    }
    impl Validator for ReplHelper {}

    impl Completer for ReplHelper {
        type Candidate = Pair;

        fn complete(
            &self, line: &str, pos: usize, _ctx: &Context<'_>,
        ) -> rustyline::Result<(usize, Vec<Pair>)> {
            let prefix = &line[..pos];
            let tokens: Vec<&str> = prefix.split_whitespace().collect();

            match tokens.as_slice() {
                // Nothing typed yet, or only whitespace — suggest all nouns.
                [] => {
                    let candidates = self
                        .noun_verbs
                        .iter()
                        .map(|(n, _)| Pair {
                            display: (*n).to_owned(),
                            replacement: format!("{n} "),
                        })
                        .collect();
                    Ok((0, candidates))
                }
                // One token and no trailing space — complete the noun.
                [partial] if !prefix.ends_with(' ') => {
                    let candidates = self
                        .noun_verbs
                        .iter()
                        .filter(|(n, _)| n.starts_with(partial))
                        .map(|(n, _)| Pair {
                            display: (*n).to_owned(),
                            replacement: format!("{n} "),
                        })
                        .collect();
                    Ok((0, candidates))
                }
                // Noun present (with trailing space or a partial verb) — suggest verbs.
                [noun, ..] => {
                    let partial_verb = tokens.get(1).copied().unwrap_or("");
                    let start = pos.saturating_sub(partial_verb.len());
                    let candidates = self
                        .noun_verbs
                        .iter()
                        .find(|(n, _)| *n == *noun)
                        .map(|(_, vs)| {
                            vs.iter()
                                .filter(|v| v.starts_with(partial_verb))
                                .map(|v| Pair {
                                    display: (*v).to_owned(),
                                    replacement: (*v).to_owned(),
                                })
                                .collect()
                        })
                        .unwrap_or_default();
                    Ok((start, candidates))
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // Repl
    // -----------------------------------------------------------------------

    /// Interactive REPL backed by `rustyline`.
    ///
    /// ```no_run
    /// use ggen_engine::repl::Repl;
    ///
    /// const TABLE: &[(&str, &[&str])] =
    ///     &[("emit", &["--type", "--object"]), ("verify", &["--strict"])];
    /// let repl = Repl::new(TABLE).unwrap();
    /// repl.run().unwrap();
    /// ```
    pub struct Repl {
        editor: Editor<ReplHelper, rustyline::history::DefaultHistory>,
    }

    impl Repl {
        /// Build a `Repl` with the given noun/verb completion table.
        ///
        /// # Errors
        ///
        /// Returns an error if `rustyline` cannot initialise (e.g., terminal
        /// is not available).
        pub fn new(noun_verbs: &[(&'static str, &'static [&'static str])]) -> anyhow::Result<Self> {
            let config = Config::builder()
                .completion_type(CompletionType::List)
                .build();
            let helper = ReplHelper::new(noun_verbs);
            let mut editor = Editor::with_config(config)?;
            editor.set_helper(Some(helper));
            Ok(Self { editor })
        }

        /// Run the REPL loop, returning when the user sends EOF (`Ctrl-D`) or
        /// `exit` / `quit`.
        ///
        /// Each non-empty line is split via [`super::split_shell_words`] and
        /// dispatched as a `ggen` invocation (the current executable re-run
        /// with the parsed argv, stdio inherited). A failing command reports
        /// its exit status and the loop continues.
        ///
        /// # Errors
        ///
        /// Propagates terminal I/O errors from `rustyline`.
        pub fn run(mut self) -> anyhow::Result<()> {
            loop {
                match self.editor.readline("> ") {
                    Ok(line) => {
                        let _ = self.editor.add_history_entry(line.as_str());
                        let trimmed = line.trim();
                        if trimmed.is_empty() {
                            continue;
                        }
                        if trimmed == "exit" || trimmed == "quit" {
                            break;
                        }
                        match super::split_shell_words(trimmed) {
                            Some(argv) if !argv.is_empty() => {
                                match std::env::current_exe().and_then(|exe| {
                                    std::process::Command::new(exe).args(&argv).status()
                                }) {
                                    Ok(status) if !status.success() => {
                                        eprintln!("command exited with {status}");
                                    }
                                    Ok(_) => {}
                                    Err(e) => eprintln!("error: failed to dispatch: {e}"),
                                }
                            }
                            Some(_) => {}
                            None => {
                                eprintln!("error: unmatched quote");
                            }
                        }
                    }
                    Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
                    Err(e) => return Err(e.into()),
                }
            }
            Ok(())
        }
    }
}

#[cfg(feature = "repl")]
pub use inner::{Repl, ReplHelper};

// ---------------------------------------------------------------------------
// feature != "repl"  — stub
// ---------------------------------------------------------------------------

/// Launch the interactive REPL.
///
/// # Errors
///
/// Always returns an error when the `repl` feature is not enabled. Rebuild
/// with `--features repl` to activate.
#[cfg(not(feature = "repl"))]
pub fn run_repl() -> anyhow::Result<()> {
    anyhow::bail!("REPL feature not enabled. Rebuild with --features repl.")
}

#[cfg(feature = "repl")]
/// Launch the interactive REPL using default noun/verb completions.
///
/// # Errors
///
/// Propagates terminal I/O errors from `rustyline`.
pub fn run_repl() -> anyhow::Result<()> {
    let repl = Repl::new(&[])?;
    repl.run()
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::split_shell_words;

    #[test]
    fn basic_split() {
        assert_eq!(
            split_shell_words("emit --type build"),
            Some(vec!["emit".into(), "--type".into(), "build".into()])
        );
    }

    #[test]
    fn double_quoted_space() {
        assert_eq!(
            split_shell_words(r#"emit --payload "hello world""#),
            Some(vec![
                "emit".into(),
                "--payload".into(),
                "hello world".into()
            ])
        );
    }

    #[test]
    fn single_quoted_space() {
        assert_eq!(
            split_shell_words("emit --payload 'hello world'"),
            Some(vec![
                "emit".into(),
                "--payload".into(),
                "hello world".into()
            ])
        );
    }

    #[test]
    fn backslash_escape() {
        assert_eq!(
            split_shell_words(r"emit\ arg"),
            Some(vec!["emit arg".into()])
        );
    }

    #[test]
    fn unmatched_double_quote_returns_none() {
        assert_eq!(split_shell_words(r#"emit "unterminated"#), None);
    }

    #[test]
    fn unmatched_single_quote_returns_none() {
        assert_eq!(split_shell_words("emit 'unterminated"), None);
    }

    #[test]
    fn empty_string() {
        assert_eq!(split_shell_words(""), Some(vec![]));
    }

    #[test]
    fn whitespace_only() {
        assert_eq!(split_shell_words("   "), Some(vec![]));
    }

    #[cfg(not(feature = "repl"))]
    #[test]
    fn stub_returns_error() {
        let err = super::run_repl().unwrap_err();
        assert!(err.to_string().contains("--features repl"));
    }
}
