use gasp::Grammar;
use itertools::Itertools;
use log::debug;
use regex::Regex;

use crate::dataloader::Triplet;

/// extracts triplets from provided text using the provided grammar
pub fn get_triplets(text: &str, grammar: Grammar) -> Vec<Triplet> {
    let predicate_regex =
        Regex::new(r"^predicate=(.*)$").expect("invalid regex for predicate matching");
    let parsed_results = grammar.find_all(text);
    return parsed_results
        .iter()
        .filter_map(|pr| {
            let tokens = pr.root.tags_dfpo();
            let Some((obj_start_idx, _)) = tokens
                .iter()
                .find_position(|s| return s.as_str() == "obj_start")
            else {
                return None;
            };
            let Some((obj_end_idx, _)) = tokens
                .iter()
                .find_position(|s| return s.as_str() == "obj_end")
            else {
                return None;
            };
            let Some((subj_start_idx, _)) = tokens
                .iter()
                .find_position(|s| return s.as_str() == "subj_start")
            else {
                return None;
            };
            let Some((subj_end_idx, _)) = tokens
                .iter()
                .find_position(|s| return s.as_str() == "subj_end")
            else {
                return None;
            };
            let Some(obj) = tokens.get(obj_start_idx + 1..obj_end_idx) else {
                return None;
            };
            let Some(subj) = tokens.get(subj_start_idx + 1..subj_end_idx) else {
                return None;
            };
            let Some(pred) = tokens.iter().find_map(|s| {
                let Some(captures) = predicate_regex.captures(s) else {
                    return None;
                };
                return captures.get(1);
            }) else {
                return None;
            };
            return Some(Triplet {
                from: obj.join(" "),
                to: subj.join(" "),
                predicate: pred.as_str().to_owned(),
            });
        })
        .collect_vec();
}