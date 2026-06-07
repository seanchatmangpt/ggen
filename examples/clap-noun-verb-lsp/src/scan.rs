use clap_noun_verb_macros::verb;
use clap_noun_verb::Result;
use crate::types::CommandResult;

#[verb("workspace")]
pub fn cmd_workspace(format: String) -> Result<CommandResult> {
    Ok(CommandResult { success: true })
}
