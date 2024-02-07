#![allow(unused)]
//! Diploma thesis software by Tomáš Lebeda

/// command-line-interface functions and types
mod cli;
/// functions and types for loading data (scenes, images, etc.)
mod dataloader;
/// error types and associated functions
mod errors;

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
        Commands::Check(args) => check_scene(args),
        Commands::Info(args) => print_info(args),
    };
}

/// Handler for CLI command 'info'.
/// Prints out information about specified scene.
fn print_info(args: cli::InfoArgs) {
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
fn check_scene(args: cli::CheckArgs) {
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
