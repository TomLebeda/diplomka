use clap::{command, ValueEnum};
use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

#[derive(Parser)]
#[command(author = "Tomáš Lebeda <tom.lebeda@gmail.com>")]
/// basic structure that represents the CLI input
pub struct Cli {
    /// command to execute
    #[command(subcommand)]
    pub command: Commands,

    /// level of logging details (into stderr)
    #[arg(short, long, value_enum, default_value_t = LogLevel::Info)]
    pub log_level: LogLevel,
}

#[derive(Subcommand, Debug)]
/// Basic commands available via the CLI
pub enum Commands {
    /// Check a scene JSON if everything is OK.
    Check(CheckArgs),

    /// Print out summary about provided scene.
    Info(InfoArgs),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
#[allow(clippy::missing_docs_in_private_items)]
pub enum LogLevel {
    Warn,
    Info,
    Debug,
    Trace,
    Error,
    Off,
}

#[derive(Args, Debug)]
/// Arguments for the "check" CLI command
pub struct CheckArgs {
    /// JSON file with scene description to check
    pub path: PathBuf,
}

#[derive(Args, Debug)]
/// Arguments for the "info" CLI command
pub struct InfoArgs {
    /// JSON file with scene description
    pub path: PathBuf,
}

impl LogLevel {
    /// converts LogLevel into log::LevelFilter for env_logger initialization
    pub fn to_log_filter(self) -> log::LevelFilter {
        match self {
            LogLevel::Trace => return log::LevelFilter::Trace,
            LogLevel::Warn => return log::LevelFilter::Warn,
            LogLevel::Info => return log::LevelFilter::Info,
            LogLevel::Debug => return log::LevelFilter::Debug,
            LogLevel::Error => return log::LevelFilter::Error,
            LogLevel::Off => return log::LevelFilter::Off,
        }
    }
}
