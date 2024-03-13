use std::{collections::HashMap, fmt::format, path::PathBuf};

use itertools::Itertools;
use log::*;
use regex::Regex;
use serde_json::Value;

use crate::{
    dataloader::{Checkpoint, Scene},
    get_forms, get_related, get_synonyms, get_translations,
    utils::remove_number_from_obj,
};

/// Generates ABNF file from the checkpoint file that is produced by 'prepare' command
pub fn generate_grammar(prep_path: PathBuf, out_path: PathBuf) {
    let Ok(raw_str) = std::fs::read_to_string(&prep_path) else {
        error!(
            "can't find or read file \"{}\"",
            prep_path.to_string_lossy()
        );
        return;
    };
    trace!(
        "file \"{}\" succesfully loaded.",
        prep_path.to_string_lossy()
    );

    let checkpoint: Checkpoint = match serde_json::from_str(&raw_str) {
        Ok(checkpoint) => checkpoint,
        Err(e) => {
            error!(
                "can't parse file \"{}\": {}",
                prep_path.to_string_lossy(),
                e
            );
            return;
        }
    };
    trace!(
        "file \"{}\" parsed into Checkpoint",
        prep_path.to_string_lossy()
    );

    // create the string buffer with valid ABNF header
    let mut str_buf = String::from("#ABNF 1.0 UTF-8;\n");

    // define the root rule
    let mut triplet_names = checkpoint
        .predicates
        .keys()
        .map(|key| return key.replace(' ', "_"))
        .collect_vec();
    triplet_names.append(&mut vec![
        String::from("has_child_object"),
        String::from("has_parent_object"),
    ]);
    str_buf += format!(
        "public $triplet = {};\n\n",
        triplet_names
            .iter()
            .map(|s| return format!("${s}"))
            .sorted_unstable()
            .unique()
            .join(" | ")
    )
    .as_str();

    let obj_regex = Regex::new(r"\{OBJ=(?<obj>[^\}]+)\}")
        .expect("regex for OBJ replacement in generation invalid");
    let subj_regex = Regex::new(r"\{SUBJ=(?<subj>[^\}]+)\}")
        .expect("regex for SUBJ replacement in generation invalid");

    // add the predicate rules
    for (pred, mut predicate_pattern) in checkpoint.predicates {
        let rule_name = format!("${}", pred).trim().replace(' ', "_");
        if let Some(obj_name) = obj_regex
            .captures(&predicate_pattern)
            .and_then(|cap| return cap.name("obj"))
        {
            let obj_rule_name = obj_name.as_str().trim().replace(' ', "_");
            // double dollar because we want literal $, not reference to named capture group
            let obj_replacement_pattern = format!("{{obj_start}} $${obj_rule_name} {{obj_end}}");
            predicate_pattern = obj_regex
                .replace_all(&predicate_pattern, obj_replacement_pattern)
                .to_string();
        }
        if let Some(subj_name) = subj_regex
            .captures(&predicate_pattern)
            .and_then(|cap| return cap.name("subj"))
        {
            let subj_rule_name = subj_name.as_str().trim().replace(' ', "_");
            // double dollar because we want literal $, not reference to named capture group
            let subj_replacement_pattern =
                format!("{{subj_start}} $${subj_rule_name} {{subj_end}}");
            predicate_pattern = subj_regex
                .replace_all(&predicate_pattern, subj_replacement_pattern)
                .to_string();
        }
        let predicate_label = pred.trim().replace(' ', "_");
        let rule_expansion = format!("({predicate_pattern}) {{predicate={predicate_label}}}");
        let rule = format!("{rule_name} = {rule_expansion};\n");
        str_buf += rule.as_str();
    }
    // add the generic hierarchy predicates
    str_buf += "$has_child_object = {obj_start} $object {obj_end} má {subj_start} $object {subj_end} {predicate=has_child_object};\n";
    str_buf += "$has_parent_object = {obj_start} $object {obj_end} (je [sou]částí | (patří|náleží) [pod|k] | tvoří) {subj_start} $object {subj_end} {predicate=has_parent_object};\n";
    str_buf += "\n";

    // add the general object rule
    str_buf += format!(
        "$object = {};\n\n",
        checkpoint
            .objects
            .keys()
            .map(|key| return format!("${}", key.replace(' ', "_")))
            .join(" | ")
    )
    .as_str();

    // add the object rules themselves
    for (obj, variants) in &checkpoint.objects {
        let rule_name = format!("${}", obj).trim().replace(' ', "_");
        let rule_exp = variants
            .iter()
            .map(|s| return format!("${}", s).trim().replace(' ', "_"))
            .join(" | ")
            .to_string();
        let rule = format!("{} = ({}) {{{}}};\n", rule_name, rule_exp, obj);
        str_buf += rule.as_str();
    }
    str_buf += "\n";

    // add the individual variants of the objects
    for (obj, variants) in &checkpoint.objects {
        for var in variants {
            let rule_name = format!("${}", var).trim().replace(' ', "_");
            let mut rule_exp = get_forms(var);
            rule_exp.push(var.clone());
            let rule_exp = rule_exp
                .iter()
                .map(|s| return s.trim())
                .sorted_unstable()
                .unique()
                .join(" | ");
            let rule = format!("{} = {};\n", rule_name, rule_exp);
            str_buf += rule.as_str();
        }
    }
    str_buf += "\n";

    // add the generic attributes
    checkpoint.attributes.iter().for_each(|(attr, values)| {
        let rule_name = format!("${}", attr.trim().replace(' ', "_"));
        let rule_expansion = values
            .keys()
            .map(|s| return s.trim().replace(' ', "_"))
            .map(|s| return format!("${s}"))
            .join(" | ");
        let rule = format!("{rule_name} = ({rule_expansion});\n");
        str_buf += rule.as_str();
    });

    // add the individual variants of attribute values
    for (attr, variants) in &checkpoint.attributes {
        for (en_attr, cz_variants) in variants {
            let rule_name = format!("${}", en_attr.trim().replace(' ', "_"));
            let rule_exp = cz_variants
                .iter()
                .map(|s| return s.trim().replace(' ', "_"))
                .map(|s| return format!("${s}"))
                .sorted_unstable()
                .unique()
                .join(" | ");
            let rule = format!(
                "{} = ({}) {{{}}};\n",
                rule_name,
                rule_exp,
                en_attr.trim().replace(' ', "_")
            );
            str_buf += rule.as_str();
            // and add the forms of each Czech variant
            for cz in cz_variants {
                let rule_name = format!("${}", cz.trim().replace(' ', "_"));
                let rule_exp = get_forms(cz)
                    .iter()
                    .map(|s| return s.trim().replace(' ', "_"))
                    .join(" | ");
                let rule = format!("{} = ({});\n", rule_name, rule_exp);
                str_buf += rule.as_str();
            }
        }
    }
    str_buf += "\n";
    match std::fs::write(&out_path, &str_buf) {
        Ok(_) => info!(
            "{} bytes of data written into \"{}\"",
            str_buf.len(),
            out_path.to_string_lossy()
        ),
        Err(e) => error!(
            "failed to write output into file \"{}\" (check if directory exists?)",
            out_path.to_string_lossy()
        ),
    }
}

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
            // NOTE: related words require a lot of cleanup
            expanded_words.append(&mut get_related(czw));
        }
        expanded_words.sort_unstable();
        expanded_words.dedup();
        word_map_expanded.insert(word.clone(), expanded_words);
    }

    // 5: construct the checkpoint
    let object_names = scene
        .get_object_names()
        .iter()
        .map(|o| return remove_number_from_obj(o))
        .collect_vec();
    let attr_groups = scene.get_attributes_grouped();
    let mut object_map = word_map_expanded.clone();
    object_map.retain(|key, val| return object_names.contains(key));
    let mut attr_map = HashMap::<String, HashMap<String, Vec<String>>>::new();
    for (attr_name, en_values) in attr_groups {
        let attr_name = format!("{}_VALS", attr_name.replace(' ', "_"));
        let mut attr_en_variants = HashMap::<String, Vec<String>>::new();
        for en_variant in en_values {
            if let Some(cz) = translations.get(&en_variant) {
                attr_en_variants.insert(en_variant, cz.clone());
            } else {
                error!("no translation for \"{}\"", en_variant);
            }
        }
        attr_map.insert(attr_name, attr_en_variants);
    }
    let mut predicate_list = scene.get_predicates();
    predicate_list.append(&mut scene.get_attribute_names());
    predicate_list.sort_unstable();
    predicate_list.dedup();
    let mut predicate_map = HashMap::<String, String>::new();
    for pred in predicate_list {
        predicate_map.insert(pred, String::from("{OBJ} {SUBJ}"));
    }

    let checkpoint = Checkpoint {
        objects: object_map,
        attributes: attr_map,
        predicates: predicate_map,
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
