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
            let (obj_start_idx, _) = tokens
                .iter()
                .find_position(|s| return s.as_str() == "obj_start")?;
            let (obj_end_idx, _) = tokens
                .iter()
                .find_position(|s| return s.as_str() == "obj_end")?;
            let (subj_start_idx, _) = tokens
                .iter()
                .find_position(|s| return s.as_str() == "subj_start")?;
            let (subj_end_idx, _) = tokens
                .iter()
                .find_position(|s| return s.as_str() == "subj_end")?;
            let obj = tokens.get(obj_start_idx + 1..obj_end_idx)?;
            let subj = tokens.get(subj_start_idx + 1..subj_end_idx)?;
            let pred = tokens.iter().find_map(|s| {
                let captures = predicate_regex.captures(s)?;
                return captures.get(1);
            })?;
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
