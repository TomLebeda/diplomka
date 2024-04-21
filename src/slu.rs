use itertools::Itertools;
use log::{trace, warn};
use regex::Regex;

use crate::{
    dataloader::{Attribute, Triplet},
    parser::{Grammar, ParseNode, ParsingStyle},
    utils::merge_number_tags,
};

impl ParseNode {
    /// Attempts to find a Triplet within the ParseResult.
    ///
    /// Uses the following scheme of tags:
    /// TRIPLETS: type: triplet
    ///           └─ predicate: {predicate name}
    ///              ├─ part: obj
    ///              │  └─ {object name}
    ///              └─ part: subj
    ///                 └─ {subject name}
    pub fn extract_triplet(&self) -> Option<Triplet> {
        let mut tags = self.tags_dfpo();
        merge_number_tags(&mut tags);
        let tags_refs = tags.iter().map(|s| return s.as_str()).collect_vec();
        match tags_refs[..] {
            [obj_name, "part=obj", subj_name, "part=subj", predicate, "type=triplet"] => {
                let predicate = predicate.trim_start_matches("predicate=");
                return Some(Triplet {
                    from: obj_name.to_string(),
                    predicate: predicate.to_string(),
                    to: subj_name.to_string(),
                });
            }
            _ => return None,
        };
    }

    /// Attempts to find an attribute withing the ParseResult
    /// Returns tuple of 3 strings: (object, attribute_name, attribute_value)
    ///
    /// Uses following scheme of tags:
    /// ATTRIBUTES: type: attribute
    ///             ├─ part: obj
    ///             │  └─ {object name}
    ///             └─ part: attr
    ///                └─ attr: {attribute type}
    ///                   └─ {attribute value}
    pub fn extract_attribute(&self) -> Option<Attribute> {
        let mut tags = self.tags_dfpo();
        merge_number_tags(&mut tags);
        let tags_refs = tags.iter().map(|s| return s.as_str()).collect_vec();
        match tags_refs[..] {
            [obj_name, "part=obj", attr_value, attr_name, "part=attr", "type=attribute"] => {
                let attr_name = attr_name.trim_start_matches("attr=");
                return Some(Attribute {
                    object: obj_name.to_string(),
                    attribute: attr_name.to_string(),
                    value: attr_value.to_string(),
                });
            }
            [attr_value, attr_name, "part=attr", obj_name, "part=obj", "type=attribute"] => {
                let attr_name = attr_name.trim_start_matches("attr=");
                return Some(Attribute {
                    object: obj_name.to_string(),
                    attribute: attr_name.to_string(),
                    value: attr_value.to_string(),
                });
            }
            _ => return None,
        }
    }

    /// Attempts to find an object within the ParseResult.
    /// Returns a String that is the name of the object (without numbering).
    ///
    /// Uses following scheme of tags:
    /// OBJECTS: type: object
    ///          └─ {object name}
    ///
    /// Since objects are part of Triplets and Attributes as well,
    /// they could be find within those results as well.
    /// But since the spgf grammar defines custom rule just for objects,
    /// only those outputs will be used for object detection.
    pub fn extract_object(&self) -> Option<String> {
        let mut tags = self.tags_dfpo();
        merge_number_tags(&mut tags);
        let tags_refs = tags.iter().map(|s| return s.as_str()).collect_vec();
        match tags_refs[..] {
            [obj_name, "type=object"] => {
                return Some(obj_name.to_string());
            }
            _ => return None,
        }
    }
}

/// extracts triplets from provided text using the provided grammar
#[deprecated]
#[allow(dead_code)]
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
