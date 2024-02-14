use clap::{command, ValueEnum};
use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

#[derive(Parser)]
#[command(author = "Tomáš Lebeda <tom.lebeda@gmail.com>")]
#[command(about = "Software for working with scenes")]
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
    /// Print out statistics (summary) about provided scene.
    Stats(StatsArgs),
    /// List selected items from the provided scene.
    List(ListArgs),
    /// Crumbles the scene into triplets (including object attributes) and prints them.
    Crumble(CrumbleArgs),
    /// Extracts triplets from provided text using provided grammar
    Triplets(TripletsArgs),
}

#[derive(Args, Debug)]
/// Arguments for the "crumble" CLI command
pub struct TripletsArgs {
    /// path to the abnf file that should be used for parsing
    pub grammar: PathBuf,
    /// string that will be parsed with the grammar
    pub text: String,
}

#[derive(Args, Debug)]
/// Arguments for the "crumble" CLI command
pub struct CrumbleArgs {
    /// path of the JSON file containing scene description
    pub path: PathBuf,
}

#[derive(Args, Debug)]
/// Arguments for the "list" CLI command
pub struct ListArgs {
    /// what to list from the scene, items are always sorted alphabetically
    #[arg(value_enum)]
    pub items: ListItems,
    /// path of the JSON file containing scene description
    pub path: PathBuf,
}

/// What to actually list from the scene
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum ListItems {
    /// List all the objects defined in the scene
    Objects,
    /// List all the triplets defined in the scene, without crumbling.
    Triplets,
    /// List all the predicates used in the scene.
    Predicates,
    /// List all the unique attributes (name, value) in the scene.
    Attributes,
    /// List all the attribute names used in the scene.
    AttributeNames,
    /// List all the attribute values used in the scene.
    AttributeVals,
    /// List all the attributes grouped by key
    AttributesGrouped,
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
pub struct StatsArgs {
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
