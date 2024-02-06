use clap::{command, ValueEnum};
use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

#[derive(Parser)]
#[command(author = "Tomáš Lebeda <tom.lebeda@gmail.com>")]
pub struct Cli {
    /// command to execute
    #[command(subcommand)]
    pub command: Commands,

    /// level of logging details (into stderr)
    #[arg(short, long, value_enum, default_value_t = LogLevel::Info)]
    pub log_level: LogLevel,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    Check(CheckArgs),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum LogLevel {
    Warn,
    Info,
    Debug,
    Trace,
    Error,
    Off,
}

impl LogLevel {
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

#[derive(Args, Debug)]
pub struct CheckArgs {
    /// JSON file with scene description to check
    pub path: PathBuf,
}
