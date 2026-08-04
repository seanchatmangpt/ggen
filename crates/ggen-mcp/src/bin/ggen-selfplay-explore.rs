//! `ggen-selfplay-explore` — grow the self-play corpus with a local LLM.
//!
//! Reads each pack's REAL ontology, asks a local Gemma (TurboFieldfare's
//! OpenAI-compatible server, Metal/GPU) to write SPARQL designed to break
//! ggen, plays every proposal through the deterministic referee, and writes
//! any case that trips an invariant into `tests/corpus/`.
//!
//! # Why this is not a test
//!
//! An LLM is nondeterministic. Putting it in the assertion path would make
//! a red suite unreproducible and a green suite meaningless. So it lives
//! here instead: exploration is a separate, explicitly-invoked activity
//! whose only output is *new corpus files*, and those replay deterministically
//! forever after in `self_play_test.rs`.
//!
//! This is the libFuzzer split. The model proposes; the referee disposes.
//! Nothing the model says is treated as a finding — only what the
//! deterministic engine does with the model's bytes.
//!
//! # Usage
//!
//! ```bash
//! # server must already be running (see turbo-fieldfare docs/OPENAI_SERVER.md)
//! ggen-selfplay-explore --packs 20 --cases-per-pack 4 --concurrency 4
//! ```

use std::path::{Path, PathBuf};
use std::sync::Arc;

use ggen_mcp::selfplay::{Board, Case, CaseOrigin};

const DEFAULT_ENDPOINT: &str = "http://127.0.0.1:8080/v1/chat/completions";
const DEFAULT_MODEL: &str = "gemma-4-26b-a4b-it";

/// Ontology bytes shown to the model. Enough to see real predicates and
/// shapes; bounded so a 2 MB pack does not blow the context.
const ONTOLOGY_EXCERPT_BYTES: usize = 6000;

struct Args {
    packs: usize,
    cases_per_pack: usize,
    concurrency: usize,
    endpoint: String,
    model: String,
    dry_run: bool,
}

fn parse_args() -> Args {
    let mut a = Args {
        packs: usize::MAX,
        cases_per_pack: 3,
        concurrency: 3,
        endpoint: std::env::var("GGEN_SELFPLAY_ENDPOINT")
            .unwrap_or_else(|_| DEFAULT_ENDPOINT.to_string()),
        model: std::env::var("GGEN_SELFPLAY_MODEL").unwrap_or_else(|_| DEFAULT_MODEL.to_string()),
        dry_run: false,
    };
    let argv: Vec<String> = std::env::args().collect();
    let mut i = 1;
    while i < argv.len() {
        let get = |i: usize| argv.get(i + 1).cloned().unwrap_or_default();
        match argv[i].as_str() {
            "--packs" => {
                a.packs = get(i).parse().unwrap_or(a.packs);
                i += 1;
            }
            "--cases-per-pack" => {
                a.cases_per_pack = get(i).parse().unwrap_or(a.cases_per_pack);
                i += 1;
            }
            "--concurrency" => {
                a.concurrency = get(i).parse().unwrap_or(a.concurrency).max(1);
                i += 1;
            }
            "--endpoint" => {
                a.endpoint = get(i);
                i += 1;
            }
            "--model" => {
                a.model = get(i);
                i += 1;
            }
            "--dry-run" => a.dry_run = true,
            other => eprintln!("warning: ignoring unknown argument {other:?}"),
        }
        i += 1;
    }
    a
}

fn repo_root() -> anyhow::Result<PathBuf> {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .map(Path::to_path_buf)
        .ok_or_else(|| {
            anyhow::anyhow!(
                "CARGO_MANIFEST_DIR ({}) has fewer than two parent directories -- \
                 this binary assumes it is built from crates/ggen-mcp/",
                env!("CARGO_MANIFEST_DIR")
            )
        })
}

fn packs_with_ontology(root: &Path) -> Vec<PathBuf> {
    let mut out: Vec<PathBuf> = std::fs::read_dir(root.join("packs"))
        .map(|rd| {
            rd.flatten()
                .map(|e| e.path())
                .filter(|p| p.is_dir() && p.join("ontology.ttl").is_file())
                .collect()
        })
        .unwrap_or_default();
    out.sort();
    out
}

/// The prompt. Deliberately asks for *hostile* input and states plainly
/// that correct refusal is the expected outcome — the model is not being
/// asked to produce working queries, which is a different and much easier
/// task that would find nothing.
fn build_prompt(pack: &str, ontology_excerpt: &str) -> String {
    format!(
        "You are fuzzing a SPARQL+RDF code generator called ggen. Your job is to write \
SPARQL queries that might make it behave INCORRECTLY.\n\n\
The generator is expected to REFUSE bad input loudly and to COUNT rows honestly. \
Correct refusal is a pass, not a failure. You are hunting for the opposite: silent \
wrong answers, miscounts, crashes, or writes escaping the project directory.\n\n\
Here is a real excerpt of the `{pack}` ontology you will be querying:\n\n\
```turtle\n{ontology_excerpt}\n```\n\n\
Write {n} DIFFERENT adversarial SPARQL queries against this graph. Aim at:\n\
- predicates that do not exist (must return 0 rows honestly, never silently)\n\
- huge cartesian self-joins (truncation must be declared)\n\
- deeply nested subqueries, UNION, OPTIONAL, MINUS, aggregates without GROUP BY\n\
- malformed or truncated syntax (must be refused, never reported as success)\n\
- degenerate shapes: empty WHERE, LIMIT 0, contradictory FILTERs\n\n\
Respond with ONLY a JSON array, no prose and no markdown fence. Each element:\n\
{{\"sparql\": \"<the query>\", \"intent\": \"<what you expect to break, one short line>\"}}",
        n = 3
    )
    .replace("{n} DIFFERENT", "3 DIFFERENT")
}

#[derive(serde::Deserialize)]
struct Proposal {
    sparql: String,
    #[serde(default)]
    intent: String,
}

/// Ask the model for proposals. Any transport/parse problem yields an empty
/// vec rather than an error: a flaky model is a reason to explore less, not
/// a reason to fail a run, and it must never be mistaken for a finding.
async fn propose(
    client: &reqwest::Client, args: &Args, pack: &str, excerpt: &str, n: usize,
) -> Vec<Proposal> {
    let body = serde_json::json!({
        "model": args.model,
        "messages": [{"role": "user", "content": build_prompt(pack, excerpt)}],
        // Non-zero: diversity across packs is the point of exploration.
        "temperature": 0.9,
        "max_completion_tokens": 1400,
    });
    let Ok(resp) = client.post(&args.endpoint).json(&body).send().await else {
        return Vec::new();
    };
    let Ok(v) = resp.json::<serde_json::Value>().await else {
        return Vec::new();
    };
    let Some(text) = v["choices"][0]["message"]["content"].as_str() else {
        return Vec::new();
    };

    // Models fence JSON despite instructions; recover the array rather than
    // discarding an otherwise-usable response.
    let start = text.find('[');
    let end = text.rfind(']');
    let slice = match (start, end) {
        (Some(s), Some(e)) if e > s => &text[s..=e],
        _ => return Vec::new(),
    };
    let mut out: Vec<Proposal> = serde_json::from_str(slice).unwrap_or_default();
    out.retain(|p| !p.sparql.trim().is_empty());
    out.truncate(n);
    out
}

fn slug(s: &str) -> String {
    s.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() {
                c.to_ascii_lowercase()
            } else {
                '-'
            }
        })
        .collect::<String>()
        .split('-')
        .filter(|p| !p.is_empty())
        .collect::<Vec<_>>()
        .join("-")
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Arc::new(parse_args());
    let root = repo_root()?;
    let corpus_dir = root.join("crates/ggen-mcp/tests/corpus");
    std::fs::create_dir_all(&corpus_dir)?;

    let mut packs = packs_with_ontology(&root);
    packs.truncate(args.packs);
    println!(
        "self-play explore: {} pack(s), {} case(s) each, concurrency {}, model {}",
        packs.len(),
        args.cases_per_pack,
        args.concurrency,
        args.model
    );

    let client = reqwest::Client::builder()
        .timeout(std::time::Duration::from_secs(300))
        .build()?;

    let sem = Arc::new(tokio::sync::Semaphore::new(args.concurrency));
    let mut handles = Vec::new();

    for pack_dir in packs {
        let (args, client, sem) = (Arc::clone(&args), client.clone(), Arc::clone(&sem));
        handles.push(tokio::spawn(async move {
            // `acquire_owned` only errs if the semaphore was `.close()`d,
            // and `sem` (constructed once above, cloned per task) is never
            // closed anywhere in this file -- a structural invariant, not
            // an input-dependent one. Even if that changed, this runs
            // inside a spawned task: a panic here surfaces as a `JoinError`
            // at the `h.await` call site below, which is already handled
            // (logged and skipped), not a process crash.
            let _permit = sem.acquire_owned().await.expect("semaphore");
            let name = pack_dir
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string();
            let ttl = std::fs::read_to_string(pack_dir.join("ontology.ttl")).unwrap_or_default();
            let excerpt: String = ttl.chars().take(ONTOLOGY_EXCERPT_BYTES).collect();

            let proposals = propose(&client, &args, &name, &excerpt, args.cases_per_pack).await;
            if proposals.is_empty() {
                return (name, 0usize, 0usize, Vec::new());
            }

            // Playing is blocking and CPU-bound; keep it off the async
            // runtime's core threads so GPU requests keep flowing.
            let pack_dir2 = pack_dir.clone();
            let name2 = name.clone();
            tokio::task::spawn_blocking(move || {
                let mut played = 0usize;
                let mut found = Vec::new();
                let Ok(board) = Board::new(&pack_dir2) else {
                    return (name2, 0, 0, found);
                };
                for (i, p) in proposals.iter().enumerate() {
                    let case = Case {
                        id: format!("gemma-{}-{i}", slug(&name2)),
                        pack: name2.clone(),
                        sparql: p.sparql.clone(),
                        to: "out/explore.txt".to_string(),
                        body: "{% for row in probe %}{{ row }}\n{% endfor %}".to_string(),
                        origin: CaseOrigin::Gemma,
                        expected_violation: None,
                        note: (!p.intent.trim().is_empty())
                            .then(|| format!("gemma intent: {}", p.intent.trim())),
                    };
                    let (verdict, _obs) = board.play(&case);
                    played += 1;
                    if !verdict.clean() {
                        let broken: Vec<String> =
                            verdict.broken().iter().map(|i| format!("{i:?}")).collect();
                        found.push((case, broken, verdict));
                    }
                }
                (name2, played, found.len(), found)
            })
            .await
            .unwrap_or((name, 0, 0, Vec::new()))
        }));
    }

    let mut total_played = 0usize;
    let mut total_found = 0usize;
    let mut written = 0usize;
    for h in handles {
        let (name, played, nfound, found) = match h.await {
            Ok(v) => v,
            Err(e) => {
                eprintln!("  task panicked: {e}");
                continue;
            }
        };
        total_played += played;
        total_found += nfound;
        if played > 0 {
            println!("  {name:<44} played {played:>2}  findings {nfound}");
        }
        for (mut case, broken, verdict) in found {
            for v in &verdict.violations {
                println!("      !! {:?}: {}", v.invariant, v.observed);
            }
            case.expected_violation = broken.first().cloned();
            if args.dry_run {
                continue;
            }
            let path = corpus_dir.join(format!("{}.json", case.id));
            if path.exists() {
                continue; // already-known finding; do not churn the corpus
            }
            std::fs::write(&path, serde_json::to_string_pretty(&case)? + "\n")?;
            written += 1;
            println!("      -> corpus/{}.json", case.id);
        }
    }

    println!(
        "\nplayed {total_played} case(s); {total_found} tripped an invariant; \
         {written} new corpus file(s)"
    );
    if total_played == 0 {
        eprintln!(
            "no cases were played -- is the model server reachable at {}?",
            args.endpoint
        );
    }
    Ok(())
}
