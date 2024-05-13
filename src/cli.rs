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
    #[arg(short, long, value_enum, default_value_t = LogLevel::Trace)]
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
    /// Fetch forms or synonyms of given word from the internet or other data sources
    Fetch(FetchArgs),
    /// Prepare files with words for provided scene description.
    /// This provides checkpoint for expert to verify the data before ABNF grammar is generated.
    Prepare(PrepareArgs),
    /// Generate ABNF grammar files from the output of 'prepare' command.
    Generate(GenerateArgs),
    /// Render graph from scene
    Render(RenderArgs),
    /// Parse the provided text with provided grammar and print the results
    Parse(ParseArgs),
    /// Extract the semantics of provided text with a reference scene description.
    Extract(ExtractArgs),
    /// Load output from 'extract' command and compute final evaluation
    Evaluate(EvalArgs),
}

#[derive(Args, Debug)]
/// Arguments for the "parse" CLI command
pub struct EvalArgs {
    /// Path to the extracts file (which is output from 'extract' command)
    pub extracts: PathBuf,
    /// Path to the scene file (JSON)
    pub scene_file: PathBuf,
    /// Path to the loss-table file (JSON)
    pub loss_file: PathBuf,
}

#[derive(Args, Debug)]
/// Arguments for the "parse" CLI command
pub struct ParseArgs {
    /// Path to the grammar file
    pub grammar: PathBuf,
    /// Text to parse
    pub text: String,
    /// if true, print out extra details
    #[arg(short, long)]
    pub verbose: bool,
}

#[derive(Args, Debug)]
/// Arguments for the "extract" CLI command
pub struct ExtractArgs {
    /// Path to the scene file (JSON)
    pub scene_file: PathBuf,
    /// Path to the grammar file
    pub grammar: PathBuf,
    /// Path to the file containing text to parse.
    /// Each sentence must be on it's own line
    pub text_file: PathBuf,
    /// Path to the output file where extracts will be stored
    pub out_file: PathBuf,
    /// if true, print out extra details
    #[arg(short, long)]
    pub verbose: bool,
    /// if true, continue with the evaluation as if `evaluate` command was called right after
    #[arg(short, long)]
    pub eval: bool,
}

#[derive(Args, Debug)]
/// Arguments for the "generate" CLI command
pub struct RenderArgs {
    /// Path to the scene file (JSON)
    pub scene_file: PathBuf,
    /// Path to the output SVG file (will be created if doesn't exist and replaced if does)
    pub out_file: PathBuf,
    /// whether to use 'unflatten' preprocessing command
    #[arg(short, long)]
    pub unflatten: bool,
    /// if used, only the raw DOT string will be dumped into stdout without rendering the graph
    #[arg(short, long)]
    pub dump: bool,
}

#[derive(Args, Debug)]
/// Arguments for the "generate" CLI command
pub struct GenerateArgs {
    /// Path to the output file from 'prepare' command
    pub prep_file: PathBuf,
    /// Path to the output file (will be created if doesn't exist and replaced if does)
    pub out_file: PathBuf,
}

#[derive(Args, Debug)]
/// Arguments for the "prepare" CLI command
pub struct PrepareArgs {
    /// Path to the scene file (JSON)
    pub scene_file: PathBuf,
    /// Path to the output file (will be created if doesn't exist and replaced if does)
    pub out_file: PathBuf,
}

#[derive(Args, Debug)]
/// Arguments for the "crumble" CLI command
pub struct CrumbleArgs {
    /// path to the JSON file with scene description
    pub scene: PathBuf,
}

#[derive(Args, Debug)]
/// Arguments for the "list" CLI command
pub struct ListArgs {
    /// what to list from the scene, items are always sorted alphabetically
    #[arg(value_enum)]
    pub items: ListItems,
    /// path to the JSON file with scene description
    pub scene: PathBuf,
}

#[derive(Args, Debug)]
/// Arguments for the "fetch" CLI command
pub struct FetchArgs {
    /// for what word to fetch the data
    pub word: String,
}

/// What to actually list from the scene
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
pub enum ListItems {
    /// List all the objects defined in the scene
    Objects,
    /// List all the triplets defined in the scene, without crumbling.
    Triplets,
    /// List all the triplets grouped by the predicate.
    CrumblesGrouped,
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
    /// List all the tags used in the scene
    Tags,
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
    /// path to the JSON file with scene description
    pub scene: PathBuf,
}

#[derive(Args, Debug)]
/// Arguments for the "info" CLI command
pub struct StatsArgs {
    /// path to the JSON file with scene description
    pub scene: PathBuf,
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
