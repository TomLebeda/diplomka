#![allow(unused)]
//! Diploma thesis software by Tomáš Lebeda

/// command-line-interface functions and types
mod cli;
/// functions and types for loading data (scenes, images, etc.)
mod dataloader;
/// error types and associated functions
mod errors;

use std::path::PathBuf;

use clap::Parser;
use cli::*;
use dataloader::Scene;
use log::*;

fn main() {
    let cli = Cli::parse();

    env_logger::Builder::new()
        .filter_level(cli.log_level.to_log_filter())
        .init();

    match cli.command {
        Commands::Check(args) => print_check(args),
        Commands::Stats(args) => print_stats(args),
        Commands::List(args) => print_list(args),
        Commands::Crumble(args) => print_crumbles(args),
    };
}

/// Crumbles the scene and prints out the generates triplets.
fn print_crumbles(args: CrumbleArgs) {
    match Scene::from_file(&args.path) {
        Ok(scene) => scene
            .crumble()
            .iter()
            .for_each(|crumb| println!("{}", crumb.to_string())),
        Err(e) => {
            error!("{}", e)
        }
    }
}

/// Lists selected information about the provided scene.
fn print_list(args: ListArgs) {
    match Scene::from_file(&args.path) {
        Ok(scene) => match args.items {
            ListItems::Objects => scene
                .get_object_names()
                .iter()
                .for_each(|name| println!("{}", name)),
            ListItems::Triplets => scene
                .get_all_triplets()
                .iter()
                .for_each(|triplet| println!("{}", triplet.to_string())),
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
    match Scene::from_file(&args.path) {
        Ok(scene) => {
            let problems = scene.check();
            if problems.is_empty() {
                println!("scene file:    {}", args.path.display());
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
    match Scene::from_file(&args.path) {
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
