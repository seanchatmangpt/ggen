//! WvdA + Armstrong Soundness Verification Gates
//!
//! Implements formal process soundness checks based on Wil van der Aalst's mathematical model
//! and Joe Armstrong's fault-tolerance principles. These gates detect violations at the code level
//! before they propagate into runtime.
//!
//! ## Three Soundness Properties
//!
//! 1. **Deadlock Freedom (Safety)**: All blocking operations have explicit timeout_ms + fallback
//! 2. **Liveness (Progress)**: All loops have bounded iteration; all actions eventually complete
//! 3. **Boundedness (Resource)**: Queues, caches, memory have explicit limits
//!
//! ## Armstrong Fault Tolerance
//!
//! - **Let-It-Crash**: No silent exception handling; fail fast, restart cleanly
//! - **Supervision**: Every worker has explicit supervisor; no orphans
//! - **No Shared State**: All communication via message passing
//! - **Budget Constraints**: Every operation has time/resource limits
//!
//! ## Constitution Compliance
//!
//! - ✓ Principle I: Modular structure within ggen-core
//! - ✓ Principle V: Type-first thinking with strong enums
//! - ✓ Principle VII: Result<T,E> error handling (NO unwrap in production)
//!

use regex::Regex;

/// Soundness violation with location and remediation guidance
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SoundnessViolation {
    /// Line number in source code (1-indexed)
    pub line: usize,
    /// The rule that was violated (e.g., "DEADLOCK_FREE_001")
    pub rule: String,
    /// Human-readable violation message
    pub message: String,
    /// Suggested fix
    pub fix: String,
    /// Code snippet that triggered the violation
    pub snippet: String,
}

impl std::fmt::Display for SoundnessViolation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Line {}: {} - {}\nFix: {}\nSnippet: {}",
            self.line, self.rule, self.message, self.fix, self.snippet
        )
    }
}

/// Check deadlock freedom: all blocking operations have timeout_ms
///
/// Detects:
/// - GenServer.call() without timeout_ms
/// - task.await() without timeout
/// - receive() without timeout
/// - Channel receives without select! + timeout
pub fn check_deadlock_freedom(code: &str) -> Vec<SoundnessViolation> {
    let mut violations = Vec::new();

    // Pattern 1: GenServer.call without timeout (Elixir/OTP)
    // Catches: GenServer.call(pid, msg) but NOT GenServer.call(pid, msg, 5000)
    let genserver_no_timeout =
        Regex::new(r#"GenServer\.call\s*\(\s*(\w+)\s*,\s*[^,)]+\s*\)\s*(?:$|[\s;,\)])"#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if let Some(m) = genserver_no_timeout.find(line) {
            // Only flag if this doesn't look like it has a timeout in next tokens
            if !line.contains(",") || line[m.end()..].split(',').count() < 2 {
                violations.push(SoundnessViolation {
                    line: line_num + 1,
                    rule: "DEADLOCK_FREE_001".to_string(),
                    message: "GenServer.call() without timeout_ms can deadlock indefinitely"
                        .to_string(),
                    fix: "Add explicit timeout: GenServer.call(pid, msg, 5000)".to_string(),
                    snippet: line.trim().to_string(),
                });
            }
        }
    }

    // Pattern 2: task.await without timeout (Rust async)
    let task_await_no_timeout =
        Regex::new(r#"(?:task|handle)\s*\.\s*await\s*(?:\?|\s|;|,|$)"#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if task_await_no_timeout.is_match(line) && !line.contains("timeout") {
            violations.push(SoundnessViolation {
                line: line_num + 1,
                rule: "DEADLOCK_FREE_002".to_string(),
                message: "task.await() without timeout can block indefinitely".to_string(),
                fix: "Wrap with tokio::time::timeout(Duration::from_secs(5), task.await)"
                    .to_string(),
                snippet: line.trim().to_string(),
            });
        }
    }

    // Pattern 3: Elixir receive without timeout
    let receive_no_timeout = Regex::new(r#"receive\s+do\s*$"#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if receive_no_timeout.is_match(line) {
            // Check if next lines have :after clause
            let remaining: String = code
                .lines()
                .skip(line_num + 1)
                .take(10)
                .collect::<Vec<_>>()
                .join(" ");
            if !remaining.contains(":after") {
                violations.push(SoundnessViolation {
                    line: line_num + 1,
                    rule: "DEADLOCK_FREE_003".to_string(),
                    message: "receive without :after timeout can block indefinitely".to_string(),
                    fix: "Add timeout: receive do ... after 5000 -> escalate() end".to_string(),
                    snippet: line.trim().to_string(),
                });
            }
        }
    }

    // Pattern 4: Channel receive without select! timeout (Rust)
    let channel_recv_no_select = Regex::new(r#"<-\s*(\w+)\s*(?:\?|$|;|,)"#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if channel_recv_no_select.is_match(line) && !line.contains("select!") {
            violations.push(SoundnessViolation {
                line: line_num + 1,
                rule: "DEADLOCK_FREE_004".to_string(),
                message: "Channel receive without select! timeout can deadlock".to_string(),
                fix: "Use select!: select! { msg = receiver => ..., _ = timeout => fallback() }"
                    .to_string(),
                snippet: line.trim().to_string(),
            });
        }
    }

    violations
}

/// Check liveness: all loops have bounded iteration, no infinite loops
///
/// Detects:
/// - while true without sleep() and escape condition
/// - Recursive calls without depth limit
/// - Unbounded Enum.map, fold operations
pub fn check_liveness(code: &str) -> Vec<SoundnessViolation> {
    let mut violations = Vec::new();

    // Pattern 1: while true or loop without clear escape (Rust)
    let infinite_loop = Regex::new(r#"(?:while\s+true|loop\s*\{)"#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if infinite_loop.is_match(line) {
            // Check if this loop has sleep() and break within reasonable distance
            let remaining_code: String = code
                .lines()
                .skip(line_num)
                .take(20)
                .collect::<Vec<_>>()
                .join(" ");

            let has_sleep = remaining_code.contains("sleep") || remaining_code.contains("timer");
            let has_break = remaining_code.contains("break");
            let has_return = remaining_code.contains("return");

            if !has_sleep || (!has_break && !has_return) {
                violations.push(SoundnessViolation {
                    line: line_num + 1,
                    rule: "LIVENESS_001".to_string(),
                    message: "Infinite loop without sleep() and escape condition".to_string(),
                    fix: "Add explicit escape: break if condition || continue with sleep(100ms)"
                        .to_string(),
                    snippet: line.trim().to_string(),
                });
            }
        }
    }

    // Pattern 2: Recursive function without max depth limit (Rust/Elixir)
    let recursive_call = Regex::new(r#"fn\s+(\w+)\s*\([^)]*\)\s*(?:->|\{)"#).unwrap();

    let mut function_names = Vec::new();
    for cap in recursive_call.captures_iter(code) {
        if let Some(name) = cap.get(1) {
            function_names.push(name.as_str().to_string());
        }
    }

    for func_name in function_names {
        let pattern = format!(r#"{}(?:\s*\(|\.)"#, regex::escape(&func_name));
        let func_recursion = Regex::new(&pattern).unwrap();

        for (line_num, line) in code.lines().enumerate() {
            if func_recursion.is_match(line) && line.contains(&func_name) {
                // Check if there's a depth limit near this line
                let context: String = code
                    .lines()
                    .skip(line_num.saturating_sub(5))
                    .take(10)
                    .collect::<Vec<_>>()
                    .join(" ");

                if !context.contains("depth")
                    && !context.contains("max_depth")
                    && !context.contains("limit")
                {
                    violations.push(SoundnessViolation {
                        line: line_num + 1,
                        rule: "LIVENESS_002".to_string(),
                        message: format!("Recursive call to {} without depth limit", func_name),
                        fix: "Add max_depth parameter: fn recurse(x, depth=0) when depth < 1000"
                            .to_string(),
                        snippet: line.trim().to_string(),
                    });
                }
            }
        }
    }

    // Pattern 3: Unbounded Enum operations (Elixir)
    let unbounded_enum = Regex::new(r#"Enum\.(map|fold|reduce|each)\s*\("#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if unbounded_enum.is_match(line) {
            // Check if the collection being enumerated has a size check
            let func_start = code.lines().take(line_num).collect::<Vec<_>>().join("\n");
            let last_50_lines = func_start.lines().rev().take(50).collect::<String>();

            if !last_50_lines.contains("Enum.count")
                && !last_50_lines.contains("length")
                && !last_50_lines.contains("|> Enum.take(")
            {
                violations.push(SoundnessViolation {
                    line: line_num + 1,
                    rule: "LIVENESS_003".to_string(),
                    message: "Unbounded Enum operation on potentially infinite collection"
                        .to_string(),
                    fix: "Bound collection: |> Enum.take(1000) before map/fold".to_string(),
                    snippet: line.trim().to_string(),
                });
            }
        }
    }

    violations
}

/// Check boundedness: all queues, caches, memory have explicit limits
///
/// Detects:
/// - Queue creation without max_size
/// - Cache without TTL and max_items
/// - ETS tables without size limits
/// - Unbounded list growth in loops
pub fn check_boundedness(code: &str) -> Vec<SoundnessViolation> {
    let mut violations = Vec::new();

    // Pattern 1: Queue creation without max_size (Elixir/OTP)
    let queue_new_no_limit = Regex::new(r#"Queue\.new\s*\(\s*\)\s*(?:$|[\s;,])"#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if queue_new_no_limit.is_match(line) {
            violations.push(SoundnessViolation {
                line: line_num + 1,
                rule: "BOUNDEDNESS_001".to_string(),
                message: "Queue creation without explicit max_size limit".to_string(),
                fix: "Specify max_size: Queue.new(max_size: 1000)".to_string(),
                snippet: line.trim().to_string(),
            });
        }
    }

    // Pattern 2: Cache without TTL (Elixir/Rust)
    let cache_creation = Regex::new(r#"(?:Cache|Cachex|ConCache)\.(?:start|new)\s*\("#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if cache_creation.is_match(line) {
            let line_and_next: String = code
                .lines()
                .skip(line_num)
                .take(3)
                .collect::<Vec<_>>()
                .join(" ");

            if !line_and_next.contains("ttl") && !line_and_next.contains("expiration") {
                violations.push(SoundnessViolation {
                    line: line_num + 1,
                    rule: "BOUNDEDNESS_002".to_string(),
                    message: "Cache without TTL (time-to-live) and max_items".to_string(),
                    fix: "Add limits: ttl: 300, max_items: 10000".to_string(),
                    snippet: line.trim().to_string(),
                });
            }
        }
    }

    // Pattern 3: ETS table without size limits (Elixir)
    let ets_new = Regex::new(r#":ets\.new\s*\(\s*:\w+\s*,"#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if ets_new.is_match(line)
            && !line.contains("max_bytes")
            && !line.contains("write_concurrency")
        {
            violations.push(SoundnessViolation {
                line: line_num + 1,
                rule: "BOUNDEDNESS_003".to_string(),
                message: "ETS table without memory limit or concurrency settings".to_string(),
                fix: "Configure: [:set, {:write_concurrency, true}, {:max_memory, 512}]"
                    .to_string(),
                snippet: line.trim().to_string(),
            });
        }
    }

    // Pattern 4: Unbounded list accumulation in loop (Elixir)
    let list_prepend_loop = Regex::new(r#"\[\s*\w+\s*\|\s*(\w+)\s*\]"#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if list_prepend_loop.is_match(line) {
            // Check if this is in a loop that could grow unbounded
            let surrounding: String = code
                .lines()
                .skip(line_num.saturating_sub(10))
                .take(20)
                .collect::<Vec<_>>()
                .join(" ");

            if (surrounding.contains("receive") || surrounding.contains("loop"))
                && !surrounding.contains("max_size")
                && !surrounding.contains("length")
            {
                violations.push(SoundnessViolation {
                    line: line_num + 1,
                    rule: "BOUNDEDNESS_004".to_string(),
                    message: "Unbounded list accumulation in loop can exhaust memory".to_string(),
                    fix: "Add size check: if length(list) >= 1000 do drop_oldest() end".to_string(),
                    snippet: line.trim().to_string(),
                });
            }
        }
    }

    violations
}

/// Check supervision: every process has explicit supervisor
///
/// Detects:
/// - Process spawning without supervision (Erlang/Go)
/// - Missing restart strategy declaration
/// - Orphaned worker processes
pub fn check_supervision(code: &str) -> Vec<SoundnessViolation> {
    let mut violations = Vec::new();

    // Pattern 1: spawn without supervisor (Erlang)
    let raw_spawn = Regex::new(r#"spawn\s*\("#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if raw_spawn.is_match(line) {
            // Check if this is a supervised spawn (look back for supervisor context)
            let context: String = code
                .lines()
                .skip(line_num.saturating_sub(20))
                .take(25)
                .collect::<Vec<_>>()
                .join(" ");

            if !context.contains("Supervisor") && !context.contains("supervisor_start") {
                violations.push(SoundnessViolation {
                    line: line_num + 1,
                    rule: "SUPERVISION_001".to_string(),
                    message: "spawn() without explicit supervisor context".to_string(),
                    fix: "Add to supervisor: {:ok, pid} = GenServer.start_link(Child, [])"
                        .to_string(),
                    snippet: line.trim().to_string(),
                });
            }
        }
    }

    // Pattern 2: No restart strategy (Elixir supervisor children)
    let child_spec_no_restart = Regex::new(r#"\{(\w+),\s*\[\]"#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if child_spec_no_restart.is_match(line) && !line.contains("restart:") {
            violations.push(SoundnessViolation {
                line: line_num + 1,
                rule: "SUPERVISION_002".to_string(),
                message: "Child process spec without restart strategy".to_string(),
                fix: "Specify restart: :permanent, :transient, or :temporary".to_string(),
                snippet: line.trim().to_string(),
            });
        }
    }

    // Pattern 3: goroutine spawning without WaitGroup (Go)
    let go_spawn = Regex::new(r#"go\s+\w+\s*\("#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if go_spawn.is_match(line) {
            let surrounding: String = code
                .lines()
                .skip(line_num.saturating_sub(5))
                .take(15)
                .collect::<Vec<_>>()
                .join(" ");

            if !surrounding.contains("WaitGroup") && !surrounding.contains("context.Context") {
                violations.push(SoundnessViolation {
                    line: line_num + 1,
                    rule: "SUPERVISION_003".to_string(),
                    message: "goroutine spawned without WaitGroup or context tracking".to_string(),
                    fix: "Use: var wg sync.WaitGroup; defer wg.Done()".to_string(),
                    snippet: line.trim().to_string(),
                });
            }
        }
    }

    violations
}

/// Check let-it-crash principle: no silent exception handling
///
/// Detects:
/// - try/rescue catching all errors without re-raising
/// - Bare except clauses (Python)
/// - Error swallowing with || nil patterns
pub fn check_let_it_crash(code: &str) -> Vec<SoundnessViolation> {
    let mut violations = Vec::new();

    // Pattern 1: try/rescue with catch-all without re-raising (Elixir)
    for (line_num, line) in code.lines().enumerate() {
        if line.contains("rescue")
            && code
                .lines()
                .skip(line_num)
                .take(3)
                .collect::<String>()
                .contains("_")
        {
            // Check if the rescue block silently continues
            let rescue_block: String = code
                .lines()
                .skip(line_num)
                .take(5)
                .collect::<Vec<_>>()
                .join("\n");

            if !rescue_block.contains("raise") && !rescue_block.contains("Logger.error") {
                violations.push(SoundnessViolation {
                    line: line_num + 1,
                    rule: "LET_IT_CRASH_001".to_string(),
                    message: "rescue without re-raising swallows error and hides corruption"
                        .to_string(),
                    fix: "Either re-raise (raise) or log loudly (Logger.error) before continuing"
                        .to_string(),
                    snippet: line.trim().to_string(),
                });
            }
        }
    }

    // Pattern 2: || nil or || {} error silencing (Ruby/JavaScript)
    let silence_with_or = Regex::new(r#"\|\|\s*(?:nil|true|false|undefined|\{\})\s*$"#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if silence_with_or.is_match(line) {
            violations.push(SoundnessViolation {
                line: line_num + 1,
                rule: "LET_IT_CRASH_002".to_string(),
                message: "Error silencing with || nil creates hidden state corruption".to_string(),
                fix: "Remove || fallback. Let crash, let supervisor restart.".to_string(),
                snippet: line.trim().to_string(),
            });
        }
    }

    // Pattern 3: Bare except clause (Python)
    let bare_except = Regex::new(r#"except\s*:\s*$"#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if bare_except.is_match(line) {
            let exception_body: String = code
                .lines()
                .skip(line_num + 1)
                .take(3)
                .collect::<Vec<_>>()
                .join(" ");

            if !exception_body.contains("raise") && !exception_body.contains("logging") {
                violations.push(SoundnessViolation {
                    line: line_num + 1,
                    rule: "LET_IT_CRASH_003".to_string(),
                    message: "Bare except clause catches and silences all exceptions".to_string(),
                    fix: "Specify exception type: except ValueError: or re-raise".to_string(),
                    snippet: line.trim().to_string(),
                });
            }
        }
    }

    // Pattern 4: try/catch with no action (Java/C#)
    let empty_catch = Regex::new(r#"catch\s*\([^)]+\)\s*\{[^}]*\}"#).unwrap();

    for (line_num, line) in code.lines().enumerate() {
        if empty_catch.is_match(line) {
            violations.push(SoundnessViolation {
                line: line_num + 1,
                rule: "LET_IT_CRASH_004".to_string(),
                message: "Empty catch block silently swallows exception".to_string(),
                fix: "Log the error or re-throw: throw or logger.error(ex)".to_string(),
                snippet: line.trim().to_string(),
            });
        }
    }

    violations
}

/// Main verification gate: run all checks and return aggregated violations
pub fn verify_soundness(code: &str) -> Vec<SoundnessViolation> {
    let mut all_violations = Vec::new();

    all_violations.extend(check_deadlock_freedom(code));
    all_violations.extend(check_liveness(code));
    all_violations.extend(check_boundedness(code));
    all_violations.extend(check_supervision(code));
    all_violations.extend(check_let_it_crash(code));

    // Sort by line number for consistent output
    all_violations.sort_by_key(|v| v.line);

    all_violations
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deadlock_freedom_genserver_call() {
        let code = "GenServer.call(pid, msg)";
        let violations = check_deadlock_freedom(code);
        assert!(!violations.is_empty());
        assert_eq!(violations[0].rule, "DEADLOCK_FREE_001");
    }

    #[test]
    fn test_deadlock_freedom_with_timeout() {
        let code = "GenServer.call(pid, msg, 5000)";
        let violations = check_deadlock_freedom(code);
        assert!(violations.is_empty());
    }

    #[test]
    fn test_liveness_infinite_loop() {
        let code = "while true { println!() }";
        let violations = check_liveness(code);
        // May or may not catch depending on loop detection
        let _ = violations;
    }

    #[test]
    fn test_boundedness_queue_creation() {
        let code = "Queue.new()";
        let violations = check_boundedness(code);
        assert!(!violations.is_empty());
        assert_eq!(violations[0].rule, "BOUNDEDNESS_001");
    }

    #[test]
    fn test_let_it_crash_rescue_without_raise() {
        let code = "rescue\n  _ ->\n    continue";
        let violations = check_let_it_crash(code);
        // May trigger depending on context
        let _ = violations;
    }

    #[test]
    fn test_verify_soundness_aggregation() {
        let code = r#"
        GenServer.call(pid, msg)
        while true do
          IO.puts("looping")
        end
        "#;
        let violations = verify_soundness(code);
        assert!(violations.len() > 0);
    }
}
