use clap_noun_verb_macros::verb;
use clap_noun_verb::Result;
use crate::types::CommandResult;

#[verb("export")]
pub fn cmd_export(format: String) -> Result<CommandResult> {
    Ok(CommandResult { success: true })
}
