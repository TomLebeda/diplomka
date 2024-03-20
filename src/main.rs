//! Diploma thesis software by Tomáš Lebeda

/// command-line-interface functions and types
mod cli;
/// functions and types for loading data (scenes, images, etc.)
mod dataloader;
/// error types and associated functions
mod errors;
/// contains fetching of word forms and synonyms from the internet and local sources
mod fetch;
/// contains functions and types for generating grammars (including the "prepare" step)
mod generator;
/// contains functions and types for grammar parsing
mod parser;
/// contains functions and types related to visualization and rendering
mod renderer;
/// spoken language understanding module for handling the natural language (text) processing
mod slu;
/// utilities that don't fit into other categories
mod utils;

use clap::Parser;
use cli::*;
use dataloader::Scene;
use fetch::*;
use itertools::Itertools;
use log::*;
use slu::get_triplets;

use crate::{
    generator::{generate_grammar, prepare_files},
    parser::Grammar,
    utils::merge_number_tags,
};

fn main() {
    let cli = Cli::parse();

    env_logger::Builder::new()
        .filter_module("diplomka", cli.log_level.to_log_filter())
        .format_timestamp_micros()
        .init();

    match cli.command {
        Commands::Check(args) => print_check(args),
        Commands::Stats(args) => print_stats(args),
        Commands::List(args) => print_list(args),
        Commands::Crumble(args) => print_crumbles(args),
        Commands::Triplets(args) => print_triplets(args),
        Commands::Fetch(args) => print_fetch(args),
        Commands::Prepare(args) => print_prepare(args),
        Commands::Generate(args) => print_generate(args),
        Commands::Render(args) => print_render(args),
        Commands::SemanticParse(args) => print_parse(args),
    };
}

/// Print the process of 'render' CLI command
fn print_render(args: RenderArgs) {
    trace!("executing 'render' command");
    match Scene::from_file(&args.scene_file) {
        Ok(scene) => {
            trace!("scene loaded");
            scene.render_dot_graph(&args.out_file, args.unflatten, args.dump);
        }
        Err(err) => {
            error!("can't load scene: {}", err);
        }
    }
}

/// Print the process and result of 'generate' CLI command
fn print_generate(args: GenerateArgs) {
    trace!("executing 'generate' command");
    generate_grammar(args.prep_file, args.out_file);
}

/// Print the process and result of 'prepare' CLI command
fn print_prepare(args: PrepareArgs) {
    trace!("executing 'prepare' command");
    prepare_files(args.scene_file, args.out_file);
}

/// Print the process result of 'fetch' CLI command
fn print_fetch(args: FetchArgs) {
    trace!("executing 'fetch' command");
    let forms = get_forms(&args.word);
    println!("{} - forms: {}", &args.word, forms.join(", "));
    let synonyms = get_synonyms(&args.word);
    println!("{} - synonyms: {}", &args.word, synonyms.join(", "));
    let related = get_related(&args.word);
    println!("{} - related: {}", &args.word, related.join(", "));
}

/// Print all the triplets found in the text using the provided grammar.
/// Triplet is detected as a [gasp::ParseResult] that contains all of the following patterns inside it's tags:
///     1. object: {obj_start} {object-label} {obj_end}
///     2. predicate: {predicate=predicate-label}
///     3. subject: {subj_start} {subj-label} {subj_end}
fn print_triplets(args: TripletsArgs) {
    trace!("executing 'triplets' command");
    match Grammar::from_file(args.grammar) {
        Ok(grammar) => {
            trace!("grammar loaded");
            get_triplets(&args.text, grammar)
                .iter()
                .for_each(|t| println!("{}", t));
        }
        Err(errs) => {
            error!("can't parse grammar:");
            for (i, e) in errs.iter().enumerate() {
                error!("parsing err #{i}: {e}");
            }
        }
    }
}

/// Print the results of semantic parsing
fn print_parse(args: ParseArgs) {
    trace!("executing 'parse' command");
    match Grammar::from_file(args.grammar) {
        Ok(grammar) => {
            trace!("grammar loaded");
            let greedy_results = grammar.find_all(&args.text, &parser::ParsingStyle::Greedy);
            let lazy_results = grammar.find_all(&args.text, &parser::ParsingStyle::Lazy);
            println!("{}", "=".repeat(60));
            greedy_results.iter().for_each(|res| {
                println!("{}", res);
                let mut tags = res.node.tags_dfpo();
                merge_number_tags(&mut tags);
                println!("{}", "-".repeat(40));
            });
            println!("{}", "=".repeat(60));
            lazy_results.iter().for_each(|res| {
                println!("{}", res);
                let mut tags = res.node.tags_dfpo();
                merge_number_tags(&mut tags);
                println!("{}", "-".repeat(40));
            });
        }
        Err(errs) => {
            error!("can't parse grammar:");
            for (i, e) in errs.iter().enumerate() {
                error!("parsing err #{i}: {e}");
            }
        }
    }
}

/// Crumbles the scene and prints out the generates triplets.
fn print_crumbles(args: CrumbleArgs) {
    trace!("executing 'crumble' command");
    match Scene::from_file(&args.scene) {
        Ok(scene) => scene
            .crumble()
            .iter()
            .for_each(|crumb| println!("{}", crumb)),
        Err(e) => {
            error!("{}", e)
        }
    }
}

/// Lists selected information about the provided scene.
fn print_list(args: ListArgs) {
    trace!("executing 'list' command");
    match Scene::from_file(&args.scene) {
        Ok(scene) => match args.items {
            ListItems::CrumblesGrouped => {
                let triplets = &scene.crumble();
                let predicates = triplets
                    .iter()
                    .map(|t| return &t.predicate)
                    .sorted_unstable()
                    .unique();
                for pred in predicates {
                    if ["x max", "y max", "x min", "y min"].contains(&pred.as_str()) {
                        continue;
                    }
                    let mut froms = vec![];
                    let mut tos = vec![];
                    for triplet in triplets {
                        if &triplet.predicate == pred {
                            froms.push(triplet.from.clone());
                            tos.push(triplet.to.clone());
                        }
                    }
                    froms.sort_unstable();
                    tos.sort_unstable();
                    froms.dedup();
                    tos.dedup();
                    println!("{:?} -- {} --> {:?}", froms, pred, tos);
                }
            }
            ListItems::Tags => scene.get_tags().iter().for_each(|tag| println!("{}", tag)),
            ListItems::Objects => scene
                .get_object_names()
                .iter()
                .for_each(|name| println!("{}", name)),
            ListItems::Triplets => scene
                .get_all_triplets()
                .iter()
                .for_each(|triplet| println!("{}", triplet)),
            ListItems::AttributeNames => scene
                .get_attribute_names()
                .iter()
                .for_each(|name| println!("{}", name)),
            ListItems::Predicates => scene
                .get_predicates()
                .iter()
                .for_each(|pred| println!("{}", pred)),
            ListItems::Attributes => scene
                .get_attributes()
                .iter()
                .for_each(|attr| println!("{}: {}", attr.0, attr.1)),
            ListItems::AttributeVals => scene
                .get_attribute_values()
                .iter()
                .for_each(|val| println!("{}", val)),
            ListItems::AttributesGrouped => scene
                .get_attributes_grouped()
                .iter()
                .for_each(|(key, vals)| println!("{}: {}", key, vals.join(", "))),
        },
        Err(e) => {
            error!("{}", e)
        }
    }
}

/// Handler for CLI command 'info'.
/// Prints out information about specified scene.
fn print_stats(args: StatsArgs) {
    trace!("executing 'stats' command");
    match Scene::from_file(&args.scene) {
        Ok(scene) => {
            let problems = scene.check();
            if problems.is_empty() {
                println!("scene file:    {}", args.scene.display());
                println!("image file:    {}", scene.get_image_path().display());
                println!("image size:    {:?}", scene.get_image_size());
                println!("# of objects:  {}", scene.get_object_count());
                println!("# of triplets: {}", scene.get_triplet_count());
            } else {
                error!(
                    "found {} problems in given scene, run 'check' for more info",
                    problems.len()
                )
            }
        }
        Err(e) => {
            error!("{}", e)
        }
    }
}

/// Handler for CLI command 'check'.
/// Loads and checks provided scene, prints out potential problems.
fn print_check(args: CheckArgs) {
    trace!("executing 'check' command");
    match Scene::from_file(&args.scene) {
        Ok(scene) => {
            let problems = scene.check();
            if problems.is_empty() {
                info!("scene file is OK");
            } else {
                for problem in problems {
                    error!("scene err: {}", problem.long_info())
                }
            }
        }
        Err(e) => {
            error!("{}", e)
        }
    }
}
