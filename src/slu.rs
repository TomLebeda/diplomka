use itertools::Itertools;
use log::{trace, warn};
use regex::Regex;

use crate::{
    dataloader::Triplet,
    parser::{Grammar, ParsingStyle},
};

/// extracts triplets from provided text using the provided grammar
pub fn get_triplets(text: &str, grammar: Grammar) -> Vec<Triplet> {
    trace!("extracting triplets from text {:?}", text);
    let predicate_regex =
        Regex::new(r"^predicate=(.*)$").expect("invalid regex for predicate matching");
    let parsed_results = grammar.find_all(text, &ParsingStyle::Greedy);
    trace!("obtained {} parse trees", parsed_results.len());
    return parsed_results
        .iter()
        .filter_map(|res| {
            let tokens = res.node.tags_dfpo();
            trace!("obtained tokens (dfpo): [{}]", tokens.join(", "));
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
            if obj.len() > 1 {
                warn!(
                    "found multiple tags \"{:?}\" as an OBJECT when extracting triplet",
                    obj
                );
            }
            if subj.len() > 1 {
                warn!(
                    "found multiple tags \"{:?}\" as an SUBJECT when extracting triplet",
                    subj
                );
            }
            return Some(Triplet {
                from: obj.join(" "),
                to: subj.join(" "),
                predicate: pred.as_str().to_owned(),
            });
        })
        .collect_vec();
}
