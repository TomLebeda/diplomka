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
/// contains functions for semantic parsing of text with grammars
mod semantic_parser;
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
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde_json::Value;
use utils::remove_number_from_obj;

use crate::dataloader::{Extract, ExtractData, Score};
#[allow(deprecated)]
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
        Commands::Fetch(args) => print_fetch(args),
        Commands::Prepare(args) => print_prepare(args),
        Commands::Generate(args) => print_generate(args),
        Commands::Render(args) => print_render(args),
        Commands::Parse(args) => print_parse(args),
        Commands::Extract(args) => print_extract(args),
        Commands::Evaluate(args) => print_eval(args),
    };
}

/// Print the process of 'eval' command
fn print_eval(args: EvalArgs) {
    trace!("executing 'evaluate' command");
    let scene = match Scene::from_file(&args.scene_file) {
        Ok(scene) => scene,
        Err(e) => {
            error!("can't load scene: {e}");
            std::process::exit(1);
        }
    };
    let Ok(loss_table_str) = std::fs::read_to_string(&args.loss_file) else {
        error!(
            "can't read file {:?} (file missing or corrupted?)",
            &args.loss_file
        );
        std::process::exit(1);
    };
    let Ok(loss_table) = serde_json::from_str::<Value>(&loss_table_str) else {
        error!(
            "can't parse content of file {:?} into JSON (invalid format?)",
            &args.extracts
        );
        std::process::exit(1);
    };
    let Ok(extracts_str) = std::fs::read_to_string(&args.extracts) else {
        error!(
            "can't read file {:?} (file missing or corrupted?)",
            &args.extracts
        );
        std::process::exit(1);
    };
    let Ok(extracts) = serde_json::from_str(&extracts_str) else {
        error!(
            "can't parse content of file {:?} into JSON (invalid format?)",
            &args.extracts
        );
        std::process::exit(1);
    };
    eval(scene, extracts, loss_table);
}

/// Runs evaluation on the provided scene and list of extracts.
/// Prints out some statistics and comparisons that could be used as a feature vectors.
fn eval(scene: Scene, extracts: Vec<Extract>, loss_map: Value) -> Score {
    let extracted_objects = extracts
        .iter()
        .filter_map(|ext| {
            return match &ext.data {
                ExtractData::Object(obj) => Some(obj.as_str()),
                _ => None,
            };
        })
        .collect_vec();
    let missing_objects = scene
        .get_all_objects()
        .par_iter()
        .map(|obj| -> f32 {
            // extracted_objects don't have numbering
            let name = remove_number_from_obj(obj.name.as_str());
            if extracted_objects.contains(&name) {
                // we found the object => no loss
                return 0.0;
            }
            return 1.0;
        })
        .sum();
    return Score {
        missing_objects,
        missing_attributes: todo!(),
        missing_triplets: todo!(),
        wrong_values: todo!(),
        grouped_missing_objects: todo!(),
        grouped_missing_attributes: todo!(),
        grouped_missing_triplets: todo!(),
        grouped_wrong_values: todo!(),
    };
    // println!("# of attributes: {}/{}", scene.get_attributes().len());
    // println!("# of triplets: {}/{}", scene.get_triplet_count());
}

/// Print the process of 'compare' command
fn print_extract(args: ExtractArgs) {
    trace!("executing 'extract' command");
    let scene = match Scene::from_file(&args.scene_file) {
        Ok(scene) => scene,
        Err(e) => {
            error!("can't load scene: {e}");
            std::process::exit(1);
        }
    };
    let grammar = match Grammar::from_file(&args.grammar) {
        Ok(grammar) => grammar,
        Err(errs) => {
            error!("can't load grammar:");
            for (i, e) in errs.iter().enumerate() {
                error!("problem #{}: {}", i + 1, e);
            }
            std::process::exit(1);
        }
    };
    let Ok(text) = std::fs::read_to_string(&args.text_file) else {
        error!("failed to read text file");
        std::process::exit(1);
    };
    let mut extracts: Vec<Extract> = vec![];
    for line in text.lines() {
        trace!("processing line: {:?}", line);
        let results = grammar.semantic_parse(line, &parser::ParsingStyle::Thorough, false);
        // some rules can match the same thing in multiple ways, so filter only the results that have unique tag sequences
        let results = results
            .iter()
            .unique_by(|res| return res.node.tags_dfpo())
            .collect_vec();
        trace!("obtained {} raw semantic parse-trees", results.len());
        for res in results {
            if let Some(obj) = res.find_object() {
                match scene.contains_object(&obj, true) {
                    false => {
                        trace!("[remove] object {} (not in scene)", obj);
                    }
                    true => {
                        trace!("[keep] object {}", obj);
                        extracts.push(Extract {
                            timestamp: None,
                            sentence: line.to_string(),
                            data: ExtractData::Object(obj.clone()),
                        });
                    }
                }
            }
            if let Some(attr) = res.find_attribute() {
                match scene.contains_attribute(&attr, true, true) {
                    false => {
                        trace!("[remove] attribute {} (not in scene)", attr);
                    }
                    true => {
                        trace!("[keep] attribute {}", attr);
                        extracts.push(Extract {
                            timestamp: None,
                            sentence: line.to_string(),
                            data: ExtractData::Attribute(attr.clone()),
                        });
                    }
                }
            }
            if let Some(trip) = res.find_triplet() {
                match scene.contains_triplet(&trip, true) {
                    false => {
                        trace!("[remove] triplet {} (not in scene)", trip);
                    }
                    true => {
                        trace!("[keep] triplet {}", trip);
                        extracts.push(Extract {
                            timestamp: None,
                            sentence: line.to_string(),
                            data: ExtractData::Triplet(trip.clone()),
                        });
                    }
                }
            }
        }
    }
    trace!("processed all lines");
    let Ok(extracts_json) = serde_json::to_string_pretty(&extracts) else {
        error!("failed to serializace extracts into json");
        std::process::exit(1)
    };
    let res = std::fs::write(&args.out_file, extracts_json);
    if res.is_err() {
        error!("received io error when writing output")
    }
    info!("oputput written into {}", &args.out_file.to_string_lossy());
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

/// Print the results of semantic parsing
fn print_parse(args: ParseArgs) {
    trace!("executing 'parse' command");
    match Grammar::from_file(&args.grammar) {
        Ok(grammar) => {
            trace!("grammar loaded");
            let mut results = vec![];
            // let greedy_results = grammar.find_all(&args.text, &parser::ParsingStyle::Greedy);
            // trace!("# of greedy results: {}", greedy_results.len());
            // let lazy_results = grammar.find_all(&args.text, &parser::ParsingStyle::Lazy);
            // trace!("# of lazy results: {}", lazy_results.len());
            results.append(&mut grammar.semantic_parse(
                &args.text,
                &parser::ParsingStyle::Thorough,
                false,
            ));
            for result in results {
                if args.verbose {
                    let mut tags = result.node.tags_dfpo();
                    merge_number_tags(&mut tags);
                    println!();
                    println!("{}", result);
                }
                if let Some(obj) = result.find_object() {
                    println!("   [object] {}", obj);
                }
                if let Some(attr) = result.find_attribute() {
                    println!("[attribute] {}", attr);
                }
                if let Some(triplet) = result.find_triplet() {
                    println!("  [triplet] {}", triplet);
                }
            }
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
