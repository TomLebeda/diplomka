//! Diploma thesis software by Tomáš Lebeda

/// command-line-interface functions and types
mod cli;
/// functions and types for loading data (scenes, images, etc.)
mod dataloader;
/// error types and associated functions
mod errors;
/// functions for evaluation of extracted features
mod eval;
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
use dataloader::{LossTable, Scene};
use fetch::*;
use itertools::Itertools;
use log::*;
use rayon::iter::ParallelIterator;
use utils::remove_number_from_obj;

use crate::{
    dataloader::{Extract, ExtractData},
    eval::eval,
    parser::ParseNode,
};
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
    let Ok(loss_table) = serde_json::from_str::<LossTable>(&loss_table_str) else {
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
        let mut line_extracts: Vec<Extract> = vec![];
        trace!("processing line: {:?}", line);
        let results = grammar.semantic_parse(line, &parser::ParsingStyle::Thorough, false);
        trace!("obtained {} raw semantic parse-trees", results.len());
        for res in results {
            if let Some(obj) = res.node.extract_object() {
                match scene.contains_object(&obj, true) {
                    false => {
                        trace!("remove: object: {} (not in scene)", obj);
                    }
                    true => {
                        trace!("keep: object: {}", obj);
                        line_extracts.push(Extract {
                            timestamp: None,
                            sentence: line.to_string(),
                            data: ExtractData::Object(obj.clone()),
                            tree: res.node.clone(),
                        });
                    }
                }
            }
            if let Some(attr) = res.node.extract_attribute() {
                match scene.contains_attribute(&attr, true, true) {
                    false => {
                        trace!("remove: attribute: {} (not in scene)", attr);
                    }
                    true => {
                        trace!("keep: attribute: {}", attr);
                        line_extracts.push(Extract {
                            timestamp: None,
                            sentence: line.to_string(),
                            data: ExtractData::Attribute(attr.clone()),
                            tree: res.node.clone(),
                        });
                    }
                }
            }
            if let Some(trip) = res.node.extract_triplet() {
                match scene.contains_triplet(&trip, true) {
                    false => {
                        trace!("remove: triplet: {} (not in scene)", trip);
                    }
                    true => {
                        trace!("keep: triplet: {}", trip);
                        line_extracts.push(Extract {
                            timestamp: None,
                            sentence: line.to_string(),
                            data: ExtractData::Triplet(trip.clone()),
                            tree: res.node.clone(),
                        });
                    }
                }
            }
        }

        #[allow(clippy::missing_docs_in_private_items)]
        #[derive(Debug)]
        struct TokenHelper {
            object_name: String,
            number_of_matches: u32,
            can_be_numbered: bool,
        }

        let mut tokens: Vec<TokenHelper> = line
            .split_whitespace()
            .enumerate()
            .map(|_| {
                return TokenHelper {
                    object_name: "".to_string(),
                    number_of_matches: 0,
                    can_be_numbered: true,
                };
            })
            .collect_vec();
        let object_nodes = line_extracts
            .iter()
            .flat_map(|ex| return ex.tree.find_rules("object"))
            .unique();
        for on in object_nodes {
            match on {
                ParseNode::Rule { expansion, .. } => {
                    let Some(obj_node) = expansion.first() else {
                        continue;
                    };
                    match obj_node {
                        ParseNode::Rule {
                            start,
                            shift,
                            rule_name,
                            ..
                        } => {
                            for n in 0..*shift {
                                let token = tokens.get_mut((start + n) as usize).unwrap();
                                token.object_name = rule_name.replace('_', " ");
                            }
                        }
                        _ => error!("got non-Rule node {:?} in obj_node!?", on),
                    }
                }
                _ => error!("got non-Rule node {:?} in object_nodes!?", on),
            }
        }
        for token in &mut tokens {
            let obj_names = scene.get_object_names();
            let scene_objets_name_match = obj_names
                .iter()
                .map(|name| return remove_number_from_obj(name))
                .filter(|name| return name == &token.object_name.as_str())
                .collect_vec();
            token.number_of_matches = scene_objets_name_match.len() as u32;
            if token.number_of_matches <= 1 {
                token.can_be_numbered = false
            }
        }
        let mut try_add_numbers = true;
        let reference_attributes = scene.get_attributes();
        let reference_triplets = scene.get_all_triplets_with_hierarchy();
        while try_add_numbers {
            try_add_numbers = false;
            let unceratin_objects = tokens
                .iter()
                .filter_map(|token| {
                    if token.can_be_numbered {
                        return Some(&token.object_name);
                    } else {
                        return None;
                    }
                })
                .collect_vec();
            let (mut token_idx_start, mut token_count) = (0, 0);
            let mut new_object_name = String::new();
            let mut old_object_name = String::new();
            // this loop searches for first line extract that can be specified and does that
            for line_extract in &mut line_extracts {
                match &mut line_extract.data {
                    ExtractData::Attribute(extracted_attribute) => {
                        if !unceratin_objects.contains(&&extracted_attribute.object) {
                            // if the extracted attribute's object isn't one of the uncertain,
                            // then skip to the next line extract
                            continue;
                        }
                        trace!("trying to specify attribute: {}", extracted_attribute);
                        let candidate_attributes = reference_attributes
                            .iter()
                            .filter(|ref_attr| {
                                return ref_attr.attribute == extracted_attribute.attribute;
                            })
                            .filter(|ref_attr| {
                                return remove_number_from_obj(ref_attr.object.as_str())
                                    == extracted_attribute.object;
                            })
                            .filter(|ref_attr| return ref_attr.value == extracted_attribute.value)
                            .collect_vec();
                        trace!(
                            "number of candidate attributes: {}",
                            candidate_attributes.len()
                        );
                        if candidate_attributes.len() == 1 {
                            let reference_attribute = candidate_attributes[0];
                            let obj_nodes = line_extract.tree.find_rules("object");
                            if obj_nodes.len() != 1 {
                                warn!(
                                    "found {} nodes with name 'object', but expected 1",
                                    obj_nodes.len()
                                );
                                continue;
                            }
                            (token_idx_start, token_count) = match obj_nodes[0] {
                                ParseNode::Rule { start, shift, .. } => (*start, *shift),
                                _ => unreachable!(),
                            };
                            new_object_name = reference_attribute.object.clone();
                            old_object_name = extracted_attribute.object.clone();
                            try_add_numbers = true;
                            break;
                        }
                    }
                    ExtractData::Triplet(extracted_triplet) => {
                        let uncertain_src_obj =
                            unceratin_objects.contains(&&extracted_triplet.from);
                        let uncertain_target_obj =
                            unceratin_objects.contains(&&extracted_triplet.to);
                        if !uncertain_target_obj && !uncertain_src_obj {
                            continue;
                        }
                        trace!("trying to specify triplet: {}", extracted_triplet);
                        let candidate_triplets = reference_triplets
                            .iter()
                            .filter(|ref_trip| {
                                return ref_trip.predicate == extracted_triplet.predicate;
                            })
                            .filter(|ref_trip| {
                                let from_matches = remove_number_from_obj(ref_trip.from.as_str())
                                    == remove_number_from_obj(extracted_triplet.from.as_str());
                                let to_matches = remove_number_from_obj(ref_trip.to.as_str())
                                    == remove_number_from_obj(extracted_triplet.to.as_str());
                                return from_matches && to_matches;
                            })
                            .collect_vec();
                        trace!("number of candidate triplets: {}", candidate_triplets.len());
                        if candidate_triplets.len() == 1 {
                            let reference_triplet = candidate_triplets[0];
                            let obj_nodes = line_extract.tree.find_rules("object");
                            if obj_nodes.len() != 2 {
                                warn!(
                                    "found {} nodes with name 'object', but expected 2",
                                    obj_nodes.len()
                                );
                                continue;
                            }
                            if uncertain_src_obj {
                                // the "from" object is uncertain
                                let Some(from_node) = obj_nodes.iter().find(|node| {
                                    return node.tags_dfpo().contains(&extracted_triplet.from);
                                }) else {
                                    warn!(
                                        "didn't found node with tag {}, but expected it",
                                        &extracted_triplet.from
                                    );
                                    continue;
                                };
                                (token_idx_start, token_count) = match from_node {
                                    ParseNode::Rule { start, shift, .. } => (*start, *shift),
                                    _ => unreachable!(),
                                };
                                new_object_name = reference_triplet.from.clone();
                                old_object_name = extracted_triplet.from.clone();
                                try_add_numbers = true;
                                break;
                            } else {
                                // "to" object is uncertain
                                let Some(to_node) = obj_nodes.iter().find(|node| {
                                    return node.tags_dfpo().contains(&extracted_triplet.to);
                                }) else {
                                    warn!(
                                        "didn't found node with tag {}, but expected it",
                                        &extracted_triplet.from
                                    );
                                    continue;
                                };
                                (token_idx_start, token_count) = match to_node {
                                    ParseNode::Rule { start, shift, .. } => (*start, *shift),
                                    _ => unreachable!(),
                                };
                                new_object_name = reference_triplet.to.clone();
                                old_object_name = extracted_triplet.to.clone();
                                try_add_numbers = true;
                                break;
                            }
                        }
                    }
                    ExtractData::Object(_) => {
                        // objects that are part of some attributes or triplets will be specified
                        // when handling those and the rest can't be specified either way
                        continue;
                    }
                };
            }
            for line_extract in &mut line_extracts {
                let updated = line_extract.tree.specify_obj_numbers(
                    token_idx_start,
                    token_count,
                    &old_object_name,
                    &new_object_name,
                );
                // tree has been updated, now update the Extract itself
                if !updated {
                    continue;
                }
                match line_extract.data {
                    ExtractData::Object(ref mut o) => {
                        if let Some(new_data) = line_extract.tree.extract_object() {
                            trace!("updated object: {} -> {}", o, new_data);
                            *o = new_data;
                        } else {
                            warn!("can't extract object from updated line_extract!")
                        }
                    }
                    ExtractData::Attribute(ref mut a) => {
                        if let Some(new_data) = line_extract.tree.extract_attribute() {
                            trace!("updated attribute: {} -> {}", a, new_data);
                            *a = new_data;
                        } else {
                            warn!("can't extract attribute from updated line_extract!")
                        }
                    }
                    ExtractData::Triplet(ref mut t) => {
                        if let Some(new_data) = line_extract.tree.extract_triplet() {
                            trace!("updated triplet: {} -> {}", t, new_data);
                            *t = new_data;
                        } else {
                            warn!("can't extract triplet from updated line_extract!")
                        }
                    }
                }
            }
        }
        extracts.append(&mut line_extracts);
    }
    trace!("processed all lines");
    info!("final extracts:");
    for e in &extracts {
        info!(" - {}", e.data);
    }
    let Ok(extracts_json) = serde_json::to_string_pretty(&extracts) else {
        error!("failed to serializace extracts into json");
        std::process::exit(1)
    };
    let res = std::fs::write(&args.out_file, extracts_json);
    if res.is_err() {
        error!("received io error when writing output")
    }
    info!("ouptput written into {}", &args.out_file.to_string_lossy());
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
                if let Some(obj) = result.node.extract_object() {
                    println!("   [object] {}", obj);
                }
                if let Some(attr) = result.node.extract_attribute() {
                    println!("[attribute] {}", attr);
                }
                if let Some(triplet) = result.node.extract_triplet() {
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
                .for_each(|attr| println!("{}: {}", attr.attribute, attr.value)),
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
