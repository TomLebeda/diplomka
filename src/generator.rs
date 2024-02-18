use std::{collections::HashMap, path::PathBuf};

use itertools::Itertools;
use log::*;
use regex::Regex;
use serde_json::Value;

use crate::{
    dataloader::{Checkpoint, Scene},
    get_related, get_synonyms, get_translations,
    utils::remove_number_from_obj,
};

/// Generates grammar file for provided scenes.
pub fn prepare_files(scene_path: PathBuf, out_path: PathBuf) {
    // 1: load scene
    let scene = match Scene::from_file(&scene_path) {
        Ok(scene) => {
            trace!("loaded scene from \"{}\"", scene_path.to_string_lossy());
            scene
        }
        Err(e) => {
            error!("can't load scene: {}", e);
            return;
        }
    };
    // 2: extract objects, predicates and subjects
    let words_to_expand = get_words_to_expand(&scene);

    // 3: translate the words into Czech
    trace!("translating objects into Czech...");
    let mut word_map: HashMap<String, Vec<String>> = HashMap::new();
    let translations = get_translations();
    for word in words_to_expand {
        match translations.get(&word) {
            None => {
                error!("no translation for \"{}\"", word);
            }
            Some(found) => {
                word_map.insert(word, found.to_owned());
            }
        }
    }
    trace!("translation step complete");

    // 4: expand the translations with synonyms and related words
    let mut word_map_expanded: HashMap<String, Vec<String>> = HashMap::new();
    trace!("expanding words with synonyms and related words...");
    for (word, cz_words) in word_map.iter() {
        let mut expanded_words = cz_words.clone();
        for czw in cz_words {
            expanded_words.append(&mut get_synonyms(czw));
            expanded_words.append(&mut get_related(czw));
        }
        expanded_words.sort_unstable();
        expanded_words.dedup();
        word_map_expanded.insert(word.clone(), expanded_words);
    }

    // 5: construct the checkpoint
    let object_names = scene.get_object_names();
    let attr_groups = scene.get_attributes_grouped();
    let mut object_map = word_map_expanded.clone();
    object_map.retain(|key, val| return object_names.contains(key));
    let mut attr_map = HashMap::<String, Vec<String>>::new();
    for attr in attr_groups {
        let key = format!("{}_VALS", attr.0.replace(' ', "_"));
        let vals = attr.1;
        attr_map.insert(key, vals);
    }
    let mut predicates = scene.get_predicates();
    predicates.append(&mut scene.get_attribute_names());
    predicates.sort_unstable();
    predicates.dedup();

    let checkpoint = Checkpoint {
        objects: object_map,
        attributes: attr_map,
        predicates,
    };
    // 6: construct and write the final json output
    let Ok(json_str) = serde_json::to_string_pretty(&checkpoint) else {
        error!(
            "can't produce JSON from checkpoint, here is raw checkpoint:\n{:?}",
            checkpoint
        );
        return;
    };
    std::fs::write(out_path, json_str);
}

/// Returns a list of words that need to be expanded for grammar generation
/// Result is sorted and unique list of all object names, attribute values and attribute names.
fn get_words_to_expand(scene: &Scene) -> Vec<String> {
    let number_regex = Regex::new(r"#\d+").expect("invalid regex for object number matching");
    let mut out = scene.get_object_names();
    out.append(&mut scene.get_attribute_names());
    out.append(&mut scene.get_attribute_values());
    return out
        .iter()
        .map(|s| return remove_number_from_obj(s))
        .sorted_unstable()
        .unique()
        .collect_vec();
}
