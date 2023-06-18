use clap::{Parser, ValueEnum};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct CliArgs {
    /// Type of output to emit
    #[arg(short, long, value_enum, default_value_t=Output::LlvmBc)]
    pub emit: Output,

    /// Output file path
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Input source files
    pub inputs: Vec<PathBuf>,
}

impl CliArgs {
    /// Get output path
    /// this will use the specified path or a reasonable default
    pub fn get_output(&self, module_name: &str) -> PathBuf {
        self.output.clone().unwrap_or_else(|| match self.emit {
            Output::LlvmIr => format!("{}.ll", module_name).into(),
            Output::LlvmBc => format!("{}.bc", module_name).into(),
            Output::Obj => format!("{}.o", module_name).into(),
        })
    }
}

#[derive(Clone, Parser, Debug, ValueEnum)]
pub enum Output {
    LlvmIr,
    LlvmBc,
    Obj,
}
