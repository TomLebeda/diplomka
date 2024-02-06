mod cli;
mod dataloader;
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
    };
}

fn check_scene(args: cli::CheckArgs) {
    match Scene::from_file(&args.path) {
        Ok(scene) => {
            let problems = scene.check();
            if problems.is_empty() {
                info!("Scene file is OK.");
            } else {
                for problem in problems {
                    error!("Scene problem: {}", problem.long_info())
                }
            }
        }
        Err(e) => {
            error!("Can't load scene from file: {}", e)
        }
    }
}
