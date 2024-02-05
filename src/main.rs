use std::path::Path;

use dataloader::Scene;
use log::debug;

mod dataloader;
mod errors;

fn main() {
    env_logger::Builder::new()
        .filter_level(log::LevelFilter::Trace)
        .init();

    let fp = Path::new("data/summer_simple.json");
    match Scene::from_file(fp) {
        Ok(scene) => {
            debug!(
                "Loaded succesfully scene with {} objects and {} triplets.",
                scene.get_object_count(),
                scene.get_triplet_count()
            );
            for triplet in scene.crumble() {
                println!("{}", triplet.to_string())
            }
            // scene.show_image(true);
        }
        Err(e) => {
            println!("Can't load scene from file: {}", e);
            std::process::exit(1)
        }
    };
}
