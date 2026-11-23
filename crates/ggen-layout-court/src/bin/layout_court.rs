//! `layout_court` — zcode-cli UiLayoutCourtAdapter Taffy subject binary.

use std::env;
use std::process::ExitCode;

use ggen_layout_court::{parse_cli_args, run_subject_path, CourtStanding};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().skip(1).collect();
    let (path, viewports) = match parse_cli_args(&args) {
        Ok(v) => v,
        Err(standing) => return emit(standing),
    };
    emit(run_subject_path(&path, &viewports))
}

fn emit(standing: CourtStanding) -> ExitCode {
    match &standing {
        CourtStanding::Alive { kind, detail } => {
            println!("layout_court: ALIVE kind={kind} {detail}");
        }
        CourtStanding::Refused { code, detail } => {
            eprintln!("layout_court: REFUSED {code}: {detail}");
        }
        CourtStanding::SubjectMissing { detail } => {
            eprintln!("layout_court: SUBJECT_MISSING: {detail}");
        }
    }
    ExitCode::from(standing.exit_code() as u8)
}
