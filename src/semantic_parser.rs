use itertools::Itertools;
use log::*;

use crate::parser::{Alternative, Element, Grammar, ParseNode, ParseResult, ParsingStyle, Rule};

impl Grammar {
    /// runs semantic parsing on the provided text, returns all parsing results according to given
    /// default parsing style
    pub fn semantic_parse(
        &self,
        text: &str,
        default_style: &ParsingStyle,
        force_style: bool,
    ) -> Vec<ParseResult> {
        let text = text.trim().trim_end_matches('.').to_lowercase();
        let text_tokens = text.split_whitespace().collect_vec();
        let rule_heap = self.get_rules();
        let public_rules = self.get_public_rules();
        let mut result_buffer: Vec<ParseResult> = vec![];
        for rule in public_rules {
            if let Ok(results) =
                rule.semantic_parse(0, &text_tokens, rule_heap, default_style, force_style)
            {
                for (_start_idx, root_node) in results {
                    let parse_result = ParseResult {
                        rule: rule.get_name(),
                        node: root_node,
                        text: text.clone(),
                        style: *default_style,
                    };
                    result_buffer.push(parse_result);
                }
            }
        }
        return result_buffer;
    }
}

impl Rule {
    /// runs semantic parsing on the provided token string, returns all all tuples `(shift, node)`
    /// where `shift` is the new offset (index of token where the parsing ended) and `node` is the
    /// top-level ParseNode that should always be [ParseNode::Rule]
    pub fn semantic_parse(
        &self,
        shift: usize,
        text_tokens: &[&str],
        rule_heap: &[Rule],
        default_style: &ParsingStyle,
        force_style: bool,
    ) -> Result<Vec<(usize, ParseNode)>, ()> {
        let mut result_buffer: Vec<(usize, ParseNode)> = vec![];
        'outer: for alternative in self.get_alternatives() {
            // try all of the alternatives and collect the results
            if let Ok(results) = alternative.semantic_parse(
                shift,
                text_tokens,
                rule_heap,
                default_style,
                force_style,
            ) {
                for (idx, nodes) in results {
                    let rule_node = ParseNode::Rule {
                        rule_name: self.get_name(),
                        expansion: nodes,
                    };
                    result_buffer.push((idx, rule_node));
                    if default_style == &ParsingStyle::Lazy {
                        break 'outer;
                    }
                }
            }
        }
        // check for empty vec, because later we use method remove() that may panic!
        if result_buffer.is_empty() {
            // no alternative matched, so the rule failed
            return Err(());
        }
        match default_style {
            ParsingStyle::Lazy => {
                // in lazy parsing, we return the first result
                // we checked if empty, so this should be safe
                return Ok(vec![result_buffer.remove(0)]);
            }
            ParsingStyle::Greedy => {
                // in greedy matching, we want the largest result
                // largest result will have the biggest shift index
                let longest_result = result_buffer.iter().max_by_key(|(shift, _)| return shift);
                match longest_result {
                    Some((shift, node)) => return Ok(vec![(*shift, node.clone())]),
                    None => return Err(()),
                }
            }
            ParsingStyle::Thorough => {
                return Ok(result_buffer);
                // In thorough matching, we return all of the results,
                // but first filter out those that are empty.
                // We know that all rules must have some expansion, so filter out those that don't.
                // return Ok(result_buffer
                //     .into_iter()
                //     .filter(|(_shift, rule_node)| {
                //         return match rule_node {
                //             ParseNode::Rule { expansion, .. } => !expansion.is_empty(),
                //             _ => unreachable!(),
                //         };
                //     })
                //     .collect_vec());
            }
        }
    }
}

impl Alternative {
    /// runs semantic parsing on the provided token string, returns all all tuples (idx, node)
    /// where `idx` is the new offset (index of token where the parsing ended) and `node` is the
    /// top-level ParseNode that should always be [ParseNode::Rule]
    pub fn semantic_parse(
        &self,
        shift: usize,
        text_tokens: &[&str],
        rule_heap: &[Rule],
        default_style: &ParsingStyle,
        force_style: bool,
    ) -> Result<Vec<(usize, Vec<ParseNode>)>, ()> {
        let results = semantic_parse_list_of_elements(
            shift,
            0,
            text_tokens,
            self.get_elements(),
            rule_heap,
            default_style,
            force_style,
        );
        return results;
    }
}

impl Element {
    /// Tries to parse provided tokens with the given [Element].
    ///
    /// All elements return vector of tuples `(shift, nodes)`, where shift is the index of the
    /// next token that should be parsed (where the parsing ended) and nodes is a vector of nodes.
    /// The `nodes` is a vector because element can be sequence (which returns multiple nodes) or
    /// it can have assigned tags (which are returned as a nodes as well).
    ///
    /// The whole return type is a vector because single element may return multiple possible ways
    /// of parsing the provided text when using [ParsingStyle::Thorough] (element can be even rule
    /// for example). In case different parsing style is returned, the vector will have at most one
    /// element.
    ///
    /// Empty return vector indicates that the parsing failed and nothing was matched.
    pub fn semantic_parse(
        &self,
        shift: usize,
        text_tokens: &[&str],
        rule_heap: &[Rule],
        default_style: &ParsingStyle,
        force_style: bool,
    ) -> Result<Vec<(usize, Vec<ParseNode>)>, ()> {
        match self {
            Element::Token { token, tags, .. } => {
                if text_tokens
                    .get(shift)
                    .is_some_and(|text_token| return text_token == token)
                {
                    let token_node = ParseNode::Token(token.clone());
                    let mut output_nodes = vec![token_node];
                    let tag_nodes = tags.iter().map(|tag| return tag.to_node());
                    output_nodes.extend(tag_nodes);
                    return Ok(vec![(shift + 1, output_nodes)]);
                } else {
                    // the token didn't match => fail
                    return Err(());
                }
            }
            Element::RuleRef {
                name, style, tags, ..
            } => {
                // get the referenced rule
                let target_rule_index = rule_heap
                    .binary_search_by_key(name, |r| return r.get_name())
                    .expect("rule linking should be already checked during semantic parsing");
                let target_rule = &rule_heap[target_rule_index]; // should be safe now
                assert_eq!(&target_rule.get_name(), name);
                // local style overwrites the default style
                let new_style = if force_style {
                    *default_style
                } else {
                    style.unwrap_or(*default_style)
                };
                // run the parsing
                let results = target_rule.semantic_parse(
                    shift,
                    text_tokens,
                    rule_heap,
                    &new_style,
                    force_style,
                )?;
                // and for each branch-result, add tags to the node and push the result
                let mut output: Vec<(usize, Vec<ParseNode>)> = vec![];
                for (new_shift, rule_node) in results {
                    let mut nodes = vec![rule_node];
                    nodes.extend(tags.iter().map(|t| return t.to_node()));
                    output.push((new_shift, nodes))
                }
                return Ok(output);
            }
            Element::Garbage { tags, .. } => match text_tokens.get(shift) {
                // garbage matches whatever single token
                Some(token) => {
                    let mut nodes = vec![ParseNode::Token(token.to_string())];
                    nodes.extend(tags.iter().map(|t| return t.to_node()));
                    // it is technically a rule, so it will return [ParseNode::Rule]
                    let rule_node = ParseNode::Rule {
                        rule_name: "GARBAGE".to_string(),
                        expansion: nodes,
                    };
                    return Ok(vec![(shift + 1, vec![rule_node])]);
                }
                None => {
                    return Err(());
                }
            },
            Element::Void => {
                // Void always fails by definition
                return Err(());
            }
            Element::Null { tags } => {
                let rule_node = ParseNode::Rule {
                    rule_name: "NULL".to_string(),
                    expansion: tags.iter().map(|t| return t.to_node()).collect_vec(),
                };
                return Ok(vec![(shift, vec![rule_node])]);
            }
            Element::End { tags } => {
                if shift == text_tokens.len() {
                    let rule_node = ParseNode::Rule {
                        rule_name: "END".to_string(),
                        expansion: tags.iter().map(|t| return t.to_node()).collect_vec(),
                    };
                    return Ok(vec![(shift, vec![rule_node])]);
                } else {
                    return Err(());
                }
            }
            Element::Begin { tags } => {
                if shift == 0 {
                    let rule_node = ParseNode::Rule {
                        rule_name: "BEGIN".to_string(),
                        expansion: tags.iter().map(|t| return t.to_node()).collect_vec(),
                    };
                    return Ok(vec![(shift, vec![rule_node])]);
                } else {
                    return Err(());
                }
            }
            Element::Sequence {
                alternatives,
                style,
                tags,
                ..
            } => {
                let new_style = if force_style {
                    *default_style
                } else {
                    style.unwrap_or(*default_style)
                };
                let mut result_buffer: Vec<(usize, Vec<ParseNode>)> = vec![];
                'outer: for alternative in alternatives {
                    if let Ok(results_from_alt) = alternative.semantic_parse(
                        shift,
                        text_tokens,
                        rule_heap,
                        &new_style,
                        force_style,
                    ) {
                        for (new_shift, mut nodes) in results_from_alt {
                            nodes.extend(tags.iter().map(|t| return t.to_node()));
                            result_buffer.push((new_shift, nodes));
                            if new_style == ParsingStyle::Lazy {
                                break 'outer;
                            }
                        }
                    }
                }
                // check for empty vec, because later we use method remove() that may panic!
                if result_buffer.is_empty() {
                    // no alternative matched => fail
                    return Err(());
                }
                match new_style {
                    ParsingStyle::Lazy => {
                        // in lazy matching, we want the first result
                        return Ok(vec![result_buffer.remove(0)]);
                    }
                    ParsingStyle::Greedy => {
                        // greedy matching => we want largest result
                        // largest result will have the biggest shift index
                        let longest = result_buffer.iter().max_by_key(|(shift, _)| return shift);
                        match longest {
                            Some((shift, node)) => return Ok(vec![(*shift, node.clone())]),
                            None => return Err(()),
                        }
                    }
                    ParsingStyle::Thorough => {
                        // in thorough matching, we return all of the results
                        return Ok(result_buffer);
                    }
                }
            }
        }
    }
}

/// runs semantic parsing for provided list of [Element], returns all tuples (shift, nodes)
/// where `shift` is the new offset (index of the token where parsing ended) and `nodes` is the
fn semantic_parse_list_of_elements(
    shift: usize,
    element_idx: usize,
    text_tokens: &[&str],
    elements: &[Element],
    rule_heap: &[Rule],
    default_style: &ParsingStyle,
    force_style: bool,
) -> Result<Vec<(usize, Vec<ParseNode>)>, ()> {
    let Some(current_element) = elements.get(element_idx) else {
        // if we don't have next element, return just an empty vector
        // this is one stopping state for the recursion
        return Err(());
    };
    let style = if force_style {
        *default_style
    } else {
        current_element.get_style().unwrap_or(*default_style)
    };
    match style {
        ParsingStyle::Lazy => {
            let rep_range = current_element.get_min()..=current_element.get_max();
            'outer: for reps in rep_range {
                // the shift index will reset every each outer loop,
                // because we want to parse from the original starting point
                let mut inner_shift = shift;
                // accumulator for the results => in lazy parsing we want always the first one
                let mut output_nodes: Vec<ParseNode> = vec![];
                for _ in 0..reps {
                    if let Ok(mut results) = current_element.semantic_parse(
                        inner_shift,
                        text_tokens,
                        rule_heap,
                        &style,
                        force_style,
                    ) {
                        let first_result = results.first_mut();
                        match first_result {
                            None => {
                                // the inner element didn't return anything => didn't match
                                // => we couldn't match enough times => go to next outer loop
                                continue 'outer;
                            }
                            Some(first_result) => {
                                inner_shift = first_result.0;
                                output_nodes.append(&mut first_result.1);
                            }
                        }
                    } else {
                        // the current element didn't match anymore
                        // => we couldn't match enough times => go to next outer loop
                        continue 'outer;
                    }
                }
                // now we applied the element 'reps' times, so try to shift the element index
                if elements.get(element_idx + 1).is_some() {
                    // there is some next element, so continue parsing
                    if let Ok(mut shifted_results) = semantic_parse_list_of_elements(
                        inner_shift,
                        element_idx + 1,
                        text_tokens,
                        elements,
                        rule_heap,
                        &style,
                        force_style,
                    ) {
                        match shifted_results.first_mut() {
                            None => {
                                // the shifted sequence didn't succeed, so try next outer loop
                                continue 'outer;
                            }
                            Some(first_shifted_result) => {
                                // the shifted sequence succeeded, so merge the results and return them
                                output_nodes.append(&mut first_shifted_result.1);
                                return Ok(vec![(first_shifted_result.0, output_nodes)]);
                            }
                        }
                    } else {
                        // the shifted sequence didn't succeed, so try next outer loop
                        continue 'outer;
                    }
                } else {
                    // there is no next element, so return the results
                    return Ok(vec![(inner_shift, output_nodes)]);
                }
            }
            // we exhausted the current element without matching anything => fail
            return Err(());
        }
        ParsingStyle::Greedy => {
            // try how many times the element matches to get actual max value
            // we can't start from the theoretical max, because in case of <n->,
            // it will be REPEAT_MAX_DEFAULT which is probably huge.
            let mut counter = 0;
            let mut inner_shift = shift;
            loop {
                if counter == current_element.get_max() {
                    // we hit the theoretical max => break the counter loop
                    break;
                }
                if let Ok(results) = current_element.semantic_parse(
                    inner_shift,
                    text_tokens,
                    rule_heap,
                    &ParsingStyle::Greedy, // always greedy for max-rep counting
                    true,                  // force the greedy style for max-rep counting
                ) {
                    // we are greedy => in case there are multiple returned branches,
                    // pick the one with biggest shift (as it was the longest parsing)
                    let best = results.iter().max_by_key(|(s, _)| return s);
                    match best {
                        Some((best_shift, _)) => {
                            inner_shift = *best_shift;
                            counter += 1;
                        }
                        None => {
                            // we can't parse anymore => break the counter loop
                            break;
                        }
                    }
                } else {
                    // we can't parse anymore => break the counter loop
                    break;
                }
            }
            // as the max we use the counter value => it's either smaller than the theoretical
            // or equal (if we hit the stopping condition inside the counter loop)
            let rep_range = current_element.get_min()..=counter;
            // in greedy matching, we want reverse repetitions, to start from the max value
            'outer: for reps in rep_range.rev() {
                // the shift index will reset every each outer loop,
                // because we want to parse from the original starting point
                let mut inner_shift = shift;
                // accumulator for the results => in lazy parsing we want always the first one
                let mut output_nodes: Vec<ParseNode> = vec![];
                for _ in 0..reps {
                    if let Ok(mut results) = current_element.semantic_parse(
                        inner_shift,
                        text_tokens,
                        rule_heap,
                        &style,
                        force_style,
                    ) {
                        let first_result = results.first_mut();
                        match first_result {
                            None => {
                                // the current element didn't return anything => didn't match
                                // => we couldn't match enough times => go to next outer loop
                                continue 'outer;
                            }
                            Some(first_result) => {
                                inner_shift = first_result.0;
                                output_nodes.append(&mut first_result.1);
                            }
                        }
                    } else {
                        // the current element failed to parse
                        // => we couldn't match enough times => go to next outer loop
                        continue 'outer;
                    }
                }
                // now we applied the element 'reps' times, so try to shift the element index
                if elements.get(element_idx + 1).is_some() {
                    // there is some next element, so continue parsing
                    if let Ok(mut shifted_results) = semantic_parse_list_of_elements(
                        inner_shift,
                        element_idx + 1,
                        text_tokens,
                        elements,
                        rule_heap,
                        &style,
                        force_style,
                    ) {
                        match shifted_results.first_mut() {
                            None => {
                                // the shifted sequence didn't succeed, so try next outer loop
                                // TODO: should this really fail or OK with empty vec?
                                continue 'outer;
                            }
                            Some(first_shifted_result) => {
                                // the shifted sequence succeeded, so merge the results and return them
                                output_nodes.append(&mut first_shifted_result.1);
                                return Ok(vec![(first_shifted_result.0, output_nodes)]);
                            }
                        }
                    } else {
                        // the shifted sequence didn't succeed, so try next outer loop
                        continue 'outer;
                    }
                } else {
                    // there is no next element, so return the results
                    return Ok(vec![(inner_shift, output_nodes)]);
                }
            }
            // we exhausted current element without success => fail
            return Err(());
        }
        ParsingStyle::Thorough => {
            info!("{{");
            trace!("current element: {:?}", current_element);
            trace!("current shift: {:?}", shift);
            // try how many times the element matches to get actual max value
            // even in thorough parsing we need to limit because of <0-> could go forever
            // it will be REPEAT_MAX_DEFAULT which is probably huge.
            let mut max_counter = 0;
            let mut inner_shift = shift;
            loop {
                if max_counter == current_element.get_max() {
                    // we hit the theoretical max => break the counter loop
                    break;
                }
                if let Ok(results) = current_element.semantic_parse(
                    inner_shift,
                    text_tokens,
                    rule_heap,
                    &ParsingStyle::Greedy, // always greedy for max-rep counting
                    true,                  // force the greedy style for max-rep counting
                ) {
                    // we are always greedy when just counting repeats
                    // => in case there are multiple returned branches,
                    // pick the one with biggest shift (as it was the longest parsing)
                    let best = results.iter().max_by_key(|(s, _)| return s);
                    match best {
                        Some((best_shift, _)) => {
                            inner_shift = *best_shift;
                            max_counter += 1;
                        }
                        None => {
                            // we can't parse anymore => break the counter loop
                            break;
                        }
                    }
                } else {
                    // we can't parse anymore => break the counter loop
                    break;
                }
            }
            let mut use_counter = 0;
            let mut total_branches: Vec<(usize, Vec<ParseNode>)> = vec![];
            let mut current_branches: Vec<(usize, Vec<ParseNode>)> = vec![(shift, vec![])];
            trace!("before loop branches: {:?}", current_branches);
            loop {
                if use_counter >= current_element.get_min() {
                    // we are in the legal range =>
                    // all branches could possibly end here
                    // so clone them into the total branches
                    total_branches.append(&mut current_branches.clone());
                }
                if use_counter == max_counter {
                    // this needs to be after the total_branches append operation
                    break;
                }
                trace!("loop started, use_counter: {}", use_counter);
                let mut new_branches = vec![];
                for branch in current_branches {
                    trace!("loop: processing branch: {:?}", branch);
                    if let Ok(mut new_sub_branches) = current_element.semantic_parse(
                        branch.0,    // sub-branch starts from the previous branch's shift
                        text_tokens, // text tokens are always the same
                        rule_heap,   // rule heap is always the same
                        &style,      // style doesn't change
                        force_style, // inherit whether to force the style or not
                    ) {
                        trace!("loop: obtained new sub-branches: {:?}", new_sub_branches);
                        let full_branches = new_sub_branches.iter_mut().map(|sub_branch| {
                            let mut full_branch = branch.1.clone();
                            full_branch.append(&mut sub_branch.1);
                            return (sub_branch.0, full_branch);
                        });
                        new_branches.extend(full_branches);
                    } else {
                        trace!("loop: branch failed");
                    }
                }
                current_branches = new_branches;
                use_counter += 1;
            }
            debug!(
                "total branches [{}]: {:?}",
                total_branches.len(),
                total_branches
            );
            trace!(
                "loop ended, use counter: {}, # of branches: {}",
                use_counter,
                current_branches.len()
            );
            let current_branches = total_branches;
            if use_counter < current_element.get_min() {
                // we failed to match at least min times, so this branch fails => return empty vec
                trace!("failed to match min times, returning empty");
                info!("}}");
                return Err(());
            }
            // now we exhausted the current element and we need all the branches to be processed by
            // the next sub-sequence (shifting the element index by +1)
            trace!("before shift branches: {:?}", current_branches);
            if elements.get(element_idx + 1).is_some() {
                let mut output_branches = vec![];
                for branch in current_branches {
                    trace!("shift: processing branch: {:?}", branch);
                    if let Ok(mut new_sub_branches) = semantic_parse_list_of_elements(
                        branch.0,        // use the shift of the branch
                        element_idx + 1, // start from the next element
                        text_tokens,
                        elements,
                        rule_heap,
                        &style,
                        force_style,
                    ) {
                        trace!("mapped new_sub_branches: {:?}", &new_sub_branches);
                        let full_branches = new_sub_branches.iter_mut().map(|sub_branch| {
                            let mut full_branch = branch.1.clone();
                            full_branch.append(&mut sub_branch.1);
                            return (sub_branch.0, full_branch);
                        });
                        output_branches.extend(full_branches);
                    } else {
                        trace!("shifted: branch failed");
                    }
                }

                // and return the results
                info!("}}");
                return Ok(output_branches);
            }
            // if we are here, then there is no next element
            info!("}}");
            return Ok(current_branches);
        }
    }
}
/// unit tests for the 'parser.rs'
#[cfg(test)]
mod tests {

    /// initialize logger for testings
    fn init() {
        let _ = env_logger::builder()
            .is_test(true)
            .filter_level(log::LevelFilter::Trace)
            .format_timestamp(None)
            .format_module_path(false)
            .format_target(false)
            .try_init();
    }

    mod lazy {
        use super::init;
        use crate::parser::{parse_grammar, ParseNode, ParseResult, ParsingStyle};

        #[test]
        fn single_token() {
            init();
            let gram = parse_grammar("public $root = foo;").unwrap();
            let text = "foo";
            let parsed = gram.semantic_parse(text, &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                style: ParsingStyle::Lazy,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Token("foo".to_string())],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn token_with_tags() {
            init();
            let gram = parse_grammar("public $root = foo {tag1} {tag2};").unwrap();
            let text = "foo";
            let parsed = gram.semantic_parse(text, &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                style: ParsingStyle::Lazy,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Tag("tag1".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_sequence() {
            init();
            let gr = parse_grammar("public $root = t1 $NULL {tag1} t2 {tag2} t3 {tag3};").unwrap();
            let text = "t1 t2 t3";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Rule {
                            rule_name: "NULL".to_string(),
                            expansion: vec![ParseNode::Tag("tag1".to_string())],
                        },
                        ParseNode::Token("t2".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                        ParseNode::Token("t3".to_string()),
                        ParseNode::Tag("tag3".to_string()),
                    ],
                },
                rule: "root".to_string(),
                text: text.to_string(),
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_sequence_with_brackets() {
            init();
            let gr = parse_grammar("public $root = ((t1) $NULL) {tag1} (t2 {tag2} t3);").unwrap();
            let text = "t1 t2 t3";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Rule {
                            rule_name: "NULL".to_string(),
                            expansion: vec![],
                        },
                        ParseNode::Tag("tag1".to_string()),
                        ParseNode::Token("t2".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                        ParseNode::Token("t3".to_string()),
                    ],
                },
                rule: "root".to_string(),
                text: text.to_string(),
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_alternative() {
            init();
            let gr = parse_grammar("public $root = t1 | t2 | t3 ;").unwrap();
            let text = "t2";
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Token("t2".to_string())],
                },
            }];
            let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn alternative_with_null() {
            init();
            let gr = parse_grammar("public $root = t1 | t2 | $NULL;").unwrap();
            let text = "";
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "NULL".to_string(),
                        expansion: vec![],
                    }],
                },
            }];
            let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn alternative_ambiguity() {
            init();
            let gr = parse_grammar("public $root = t1 {tag1} | t1 {tag2} | t2;").unwrap();
            let text = "t1";
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag1".to_string()),
                    ],
                },
            }];
            let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_garbage() {
            init();
            let gr = parse_grammar("public $root = $GARBAGE;").unwrap();
            let text = "foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "GARBAGE".to_string(),
                        expansion: vec![ParseNode::Token("foo".to_string())],
                    }],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn repeat_simple() {
            init();
            let gr = parse_grammar("public $root = t1 <0-2>;").unwrap();
            let text = "";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![],
                },
            }];
            assert_eq!(parsed, expected);
            let gr = parse_grammar("public $root = t1 <1-2>;").unwrap();
            let text2 = "t1";
            let parsed2 = gr.semantic_parse(text2, &ParsingStyle::Lazy, false);
            let expected2 = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text2.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Token("t1".to_string())],
                },
            }];
            assert_eq!(parsed2, expected2);
            let gr = parse_grammar("public $root = t1 <2-3>;").unwrap();
            let text3 = "t1 t1";
            let parsed3 = gr.semantic_parse(text3, &ParsingStyle::Lazy, false);
            let expected3 = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text3.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Token("t1".to_string()),
                    ],
                },
            }];
            assert_eq!(parsed3, expected3);
        }

        #[test]
        fn repeat_sequence() {
            init();
            let gr = parse_grammar("public $root = (t1 {tag}) <0-3>;").unwrap();
            let parsed = gr.semantic_parse("", &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: "".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![],
                },
            }];
            assert_eq!(parsed, expected);
            let gr = parse_grammar("public $root = (t1 {tag}) <1-3>;").unwrap();
            let parsed = gr.semantic_parse("t1", &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: "t1".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag".to_string()),
                    ],
                },
            }];
            assert_eq!(parsed, expected);
            let gr = parse_grammar("public $root = (t1 {tag}) <3-4>;").unwrap();
            let parsed = gr.semantic_parse("t1 t1 t1", &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: "t1 t1 t1".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag".to_string()),
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag".to_string()),
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag".to_string()),
                    ],
                },
            }];
            assert_eq!(parsed, expected);
        }

        #[test]
        fn repeat_undef_max() {
            init();
            for n in [1, 2, 5, 100] {
                let gr = parse_grammar(&format!("public $root = (t1 {{tag}}) <{n}->;")).unwrap();
                let text = "t1 ".repeat(n);
                let text = text.trim();
                let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
                let mut expansion = vec![];
                for _ in 0..n {
                    expansion.push(ParseNode::Token("t1".to_string()));
                    expansion.push(ParseNode::Tag("tag".to_string()));
                }
                let expected = vec![ParseResult {
                    style: ParsingStyle::Lazy,
                    rule: "root".to_string(),
                    text: text.to_string(),
                    node: ParseNode::Rule {
                        rule_name: "root".to_string(),
                        expansion,
                    },
                }];
                assert_eq!(parsed, expected);
            }
        }

        #[test]
        fn special_garbage_with_tags() {
            init();
            let gr = parse_grammar("public $root = $GARBAGE {tag1} {tag2};").unwrap();
            let text = "foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "GARBAGE".to_string(),
                        expansion: vec![
                            ParseNode::Token("foo".to_string()),
                            ParseNode::Tag("tag1".to_string()),
                            ParseNode::Tag("tag2".to_string()),
                        ],
                    }],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_garbage_with_tags_and_repeat() {
            init();
            let gr = parse_grammar("public $root = $GARBAGE <3> {tag};").unwrap();
            let text = "foo bar baz";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Rule {
                            rule_name: "GARBAGE".to_string(),
                            expansion: vec![
                                ParseNode::Token("foo".to_string()),
                                ParseNode::Tag("tag".to_string()),
                            ],
                        },
                        ParseNode::Rule {
                            rule_name: "GARBAGE".to_string(),
                            expansion: vec![
                                ParseNode::Token("bar".to_string()),
                                ParseNode::Tag("tag".to_string()),
                            ],
                        },
                        ParseNode::Rule {
                            rule_name: "GARBAGE".to_string(),
                            expansion: vec![
                                ParseNode::Token("baz".to_string()),
                                ParseNode::Tag("tag".to_string()),
                            ],
                        },
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_null() {
            init();
            let gr = parse_grammar("public $root = $NULL {tag};").unwrap();
            let text = "";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "NULL".to_string(),
                        expansion: vec![ParseNode::Tag("tag".to_string())],
                    }],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_null_repeat() {
            init();
            let gr = parse_grammar("public $root = $NULL <1-3> {tag};");
            assert!(gr.is_err());
        }

        #[test]
        fn text_parse_grammar_simple() {
            init();
            let s = "#ABNF 1.0 UTF-8; public $root = token {tag};";
            let g = parse_grammar(s).unwrap();
            let text = "token";
            let p = g.semantic_parse(text, &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("token".to_string()),
                        ParseNode::Tag("tag".to_string()),
                    ],
                },
            }];
            assert_eq!(p, expected);
        }

        #[test]
        fn link_simple() {
            init();
            let s = "#ABNF 1.0 UTF-8; public $root = $other; $other = token {tag} {second tag};";
            let g = parse_grammar(s).unwrap();
            let text = "token";
            let p = g.semantic_parse(text, &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "other".to_string(),
                        expansion: vec![
                            ParseNode::Token("token".to_string()),
                            ParseNode::Tag("tag".to_string()),
                            ParseNode::Tag("second tag".to_string()),
                        ],
                    }],
                },
            }];
            assert_eq!(p, expected);
        }

        #[test]
        fn ambiguous_diff_tags() {
            init();
            let gr =
                parse_grammar("public $rule = (t1 {tag1}) <0-2> (t1 {tag2}) <0-2> t2;").unwrap();
            let text = "t1 t1 t1 t1 t2";
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "rule".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "rule".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag1".to_string()),
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag1".to_string()),
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                        ParseNode::Token("t2".to_string()),
                    ],
                },
            }];
            let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_rule_ref() {
            init();
            let gr = parse_grammar("public $root = foo $other; $other = bar;").unwrap();
            let parsed = gr.semantic_parse("foo bar", &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: "foo bar".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![ParseNode::Token("bar".to_string())],
                        },
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_rule_ref_with_tags() {
            init();
            let src = "public $root = foo {tag foo} $other {tag ref}; $other = bar {tag bar};";
            let gr = parse_grammar(src).unwrap();
            let parsed = gr.semantic_parse("foo bar", &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: "foo bar".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Tag("tag foo".to_string()),
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![
                                ParseNode::Token("bar".to_string()),
                                ParseNode::Tag("tag bar".to_string()),
                            ],
                        },
                        ParseNode::Tag("tag ref".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn optional_sequence() {
            init();
            let gr = parse_grammar("public $root = t1 [t2 {tag}] t3;").unwrap();
            let parsed = gr.semantic_parse("t1 t2 t3", &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: "t1 t2 t3".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Token("t2".to_string()),
                        ParseNode::Tag("tag".to_string()),
                        ParseNode::Token("t3".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed);
            let parsed2 = gr.semantic_parse("t1 t3", &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: "t1 t3".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Token("t3".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed2);
        }

        #[test]
        fn optional_rule_ref() {
            init();
            let gr = parse_grammar("public $rule = t1 [$other] t3; $other = t2;").unwrap();
            let parsed = gr.semantic_parse("t1 t2 t3", &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "rule".to_string(),
                text: "t1 t2 t3".to_string(),
                node: ParseNode::Rule {
                    rule_name: "rule".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![ParseNode::Token("t2".to_string())],
                        },
                        ParseNode::Token("t3".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn optional_rule_ref_missing() {
            init();
            let gr = parse_grammar("public $rule = t1 [$other] t2; $other = t2;").unwrap();
            let parsed2 = gr.semantic_parse("t1 t2", &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "rule".to_string(),
                text: "t1 t2".to_string(),
                node: ParseNode::Rule {
                    rule_name: "rule".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Token("t2".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed2);
        }

        #[test]
        fn rule_ref_multiple_same() {
            init();
            let gr = parse_grammar("public $root = $other $other; $other = foo;").unwrap();
            let parsed = gr.semantic_parse("foo foo", &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: "foo foo".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![ParseNode::Token("foo".to_string())],
                        },
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![ParseNode::Token("foo".to_string())],
                        },
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn rule_ref_multiple_repeat() {
            init();
            for n in [1, 2, 5, 10, 100] {
                let grs = format!("public $root = $other <{n}-> {{baz}}; $other = foo {{bar}};");
                let gr = parse_grammar(&grs).unwrap();
                let text = "foo ".repeat(n);
                let text = text.trim();
                let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
                let mut expansion = vec![];
                for _ in 0..n {
                    expansion.push(ParseNode::Rule {
                        rule_name: "other".to_string(),
                        expansion: vec![
                            ParseNode::Token("foo".to_string()),
                            ParseNode::Tag("bar".to_string()),
                        ],
                    });
                    expansion.push(ParseNode::Tag("baz".to_string()));
                }
                let expected = vec![ParseResult {
                    style: ParsingStyle::Lazy,
                    rule: "root".to_string(),
                    text: text.to_string(),
                    node: ParseNode::Rule {
                        rule_name: "root".to_string(),
                        expansion,
                    },
                }];
                assert_eq!(parsed, expected);
            }
        }

        #[test]
        fn recursion_simple() {
            init();
            // the first applicable rule alternative will be applied,
            // so recursive definition must be before the terminals,
            // otherwise the recursion will not be actually applied
            let s = "public $root = $other; $other = t1 $other | t1 {last} ;";
            let grammar = parse_grammar(s).unwrap();
            let parsed = grammar.semantic_parse("t1 t1 t1", &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: "t1 t1 t1".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "other".to_string(),
                        expansion: vec![
                            ParseNode::Token("t1".to_string()),
                            ParseNode::Rule {
                                rule_name: "other".to_string(),
                                expansion: vec![
                                    ParseNode::Token("t1".to_string()),
                                    ParseNode::Rule {
                                        rule_name: "other".to_string(),
                                        expansion: vec![
                                            ParseNode::Token("t1".to_string()),
                                            ParseNode::Tag("last".to_string()),
                                        ],
                                    },
                                ],
                            },
                        ],
                    }],
                },
            }];
            assert_eq!(parsed, expected)
        }

        #[test]
        fn recursion_embedded() {
            init();
            let gr = parse_grammar("public $root = $other; $other = (t1 $other t2) | $NULL {end};")
                .unwrap();
            let text = "t1 t1 t2 t2";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Lazy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Lazy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "other".to_string(),
                        expansion: vec![
                            ParseNode::Token("t1".to_string()),
                            ParseNode::Rule {
                                rule_name: "other".to_string(),
                                expansion: vec![
                                    ParseNode::Token("t1".to_string()),
                                    ParseNode::Rule {
                                        rule_name: "other".to_string(),
                                        expansion: vec![ParseNode::Rule {
                                            rule_name: "NULL".to_string(),
                                            expansion: vec![ParseNode::Tag("end".to_string())],
                                        }],
                                    },
                                    ParseNode::Token("t2".to_string()),
                                ],
                            },
                            ParseNode::Token("t2".to_string()),
                        ],
                    }],
                },
            }];
            assert_eq!(parsed, expected);
        }

        #[test]
        fn special_end() {
            init();
            // with the END the rule should match both foos even if lazy
            let gr = parse_grammar("public $root = foo<0-2> $END;").unwrap();
            let text = "foo foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                style: ParsingStyle::Greedy,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Rule {
                            rule_name: "END".to_string(),
                            expansion: vec![],
                        },
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_end_unreachable() {
            init();
            let gr = parse_grammar("public $root = foo<0-2> $END;").unwrap();
            let text = "foo foo foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            // we can't reach the END with only 2 foos, so the shouldn't be any results
            assert!(parsed.is_empty());
        }
    }

    mod greedy {
        use crate::{parser::*, semantic_parser::tests::init};

        #[test]
        fn single_token() {
            init();
            let gr = parse_grammar("public $root = foo;").unwrap();
            let text = "foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                style: ParsingStyle::Greedy,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Token("foo".to_string())],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn token_with_tags() {
            init();
            let gr = parse_grammar("public $root = foo {tag1} {tag2};").unwrap();
            let text = "foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Tag("tag1".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                    ],
                },
                rule: "root".to_string(),
                text: text.to_string(),
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_sequence() {
            init();
            let gr = parse_grammar("public $root = t1 $NULL {tag1} t2 {tag2} t3 {tag3};").unwrap();
            let text = "t1 t2 t3";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Rule {
                            rule_name: "NULL".to_string(),
                            expansion: vec![ParseNode::Tag("tag1".to_string())],
                        },
                        ParseNode::Token("t2".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                        ParseNode::Token("t3".to_string()),
                        ParseNode::Tag("tag3".to_string()),
                    ],
                },
                rule: "root".to_string(),
                text: text.to_string(),
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_sequence_with_brackets() {
            init();
            let gr = parse_grammar("public $root = ((t1) $NULL) {tag1} (t2 {tag2} t3);").unwrap();
            let text = "t1 t2 t3";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Rule {
                            rule_name: "NULL".to_string(),
                            expansion: vec![],
                        },
                        ParseNode::Tag("tag1".to_string()),
                        ParseNode::Token("t2".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                        ParseNode::Token("t3".to_string()),
                    ],
                },
                rule: "root".to_string(),
                text: text.to_string(),
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_alternative() {
            init();
            let gr = parse_grammar("public $root = t1 | t2 | t3 ;").unwrap();
            let text = "t2";
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Token("t2".to_string())],
                },
            }];
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn alternative_with_null() {
            init();
            let gr = parse_grammar("public $root = t1 | t2 | $NULL;").unwrap();
            let text = "";
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "NULL".to_string(),
                        expansion: vec![],
                    }],
                },
            }];
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn alternative_ambiguity() {
            init();
            let gr = parse_grammar("public $root = t1 {tag1} | t1 {tag2} | t2;").unwrap();
            let text = "t1";
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                    ],
                },
            }];
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_garbage() {
            init();
            let gr = parse_grammar("public $root = $GARBAGE;").unwrap();
            let text = "foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "GARBAGE".to_string(),
                        expansion: vec![ParseNode::Token("foo".to_string())],
                    }],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn repeat_simple() {
            init();
            let gr = parse_grammar("public $root = t1 <0-2>;").unwrap();
            let text = "";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![],
                },
            }];
            assert_eq!(parsed, expected);

            let text2 = "t1";
            let parsed2 = gr.semantic_parse(text2, &ParsingStyle::Greedy, false);
            let expected2 = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text2.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Token("t1".to_string())],
                },
            }];
            assert_eq!(parsed2, expected2);

            let text3 = "t1 t1";
            let parsed3 = gr.semantic_parse(text3, &ParsingStyle::Greedy, false);
            let expected3 = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text3.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Token("t1".to_string()),
                    ],
                },
            }];
            assert_eq!(parsed3, expected3);
        }

        #[test]
        fn repeat_sequence() {
            init();
            let gr = parse_grammar("public $root = (t1 {tag}) <0-3>;").unwrap();
            let parsed = gr.semantic_parse("", &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: "".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![],
                },
            }];
            assert_eq!(parsed, expected);

            let parsed = gr.semantic_parse("t1", &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: "t1".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag".to_string()),
                    ],
                },
            }];
            assert_eq!(parsed, expected);

            let parsed = gr.semantic_parse("t1 t1 t1", &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: "t1 t1 t1".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag".to_string()),
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag".to_string()),
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag".to_string()),
                    ],
                },
            }];
            assert_eq!(parsed, expected);
        }

        #[test]
        fn repeat_undef_max() {
            init();
            let gr = parse_grammar("public $root = (t1 {tag}) <0->;").unwrap();
            for n in [0, 1, 2] {
                let text = "t1 ".repeat(n);
                let text = text.trim();
                let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
                let mut expansion = vec![];
                for _ in 0..n {
                    expansion.push(ParseNode::Token("t1".to_string()));
                    expansion.push(ParseNode::Tag("tag".to_string()));
                }
                let expected = vec![ParseResult {
                    style: ParsingStyle::Greedy,
                    rule: "root".to_string(),
                    text: text.to_string(),
                    node: ParseNode::Rule {
                        rule_name: "root".to_string(),
                        expansion,
                    },
                }];
                assert_eq!(parsed, expected);
            }
        }

        #[test]
        fn special_garbage_with_tags() {
            init();
            let gr = parse_grammar("public $root = $GARBAGE {tag1} {tag2};").unwrap();
            let text = "foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "GARBAGE".to_string(),
                        expansion: vec![
                            ParseNode::Token("foo".to_string()),
                            ParseNode::Tag("tag1".to_string()),
                            ParseNode::Tag("tag2".to_string()),
                        ],
                    }],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_garbage_with_tags_and_repeat() {
            init();
            let gr = parse_grammar("public $root = $GARBAGE <3> {tag};").unwrap();
            let text = "foo bar baz";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Rule {
                            rule_name: "GARBAGE".to_string(),
                            expansion: vec![
                                ParseNode::Token("foo".to_string()),
                                ParseNode::Tag("tag".to_string()),
                            ],
                        },
                        ParseNode::Rule {
                            rule_name: "GARBAGE".to_string(),
                            expansion: vec![
                                ParseNode::Token("bar".to_string()),
                                ParseNode::Tag("tag".to_string()),
                            ],
                        },
                        ParseNode::Rule {
                            rule_name: "GARBAGE".to_string(),
                            expansion: vec![
                                ParseNode::Token("baz".to_string()),
                                ParseNode::Tag("tag".to_string()),
                            ],
                        },
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_null() {
            init();
            let gr = parse_grammar("public $root = $NULL {tag};").unwrap();
            let text = "";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "NULL".to_string(),
                        expansion: vec![ParseNode::Tag("tag".to_string())],
                    }],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_null_repeat() {
            init();
            let gr = parse_grammar("public $root = $NULL <1-3> {tag};");
            assert!(gr.is_err());
        }

        #[test]
        fn text_parse_grammar_simple() {
            init();
            let s = "#ABNF 1.0 UTF-8; public $root = token {tag};";
            let g = parse_grammar(s).unwrap();
            let text = "token";
            let p = g.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("token".to_string()),
                        ParseNode::Tag("tag".to_string()),
                    ],
                },
            }];
            assert_eq!(p, expected);
        }

        #[test]
        fn link_simple() {
            init();
            let s = "#ABNF 1.0 UTF-8; public $root = $other; $other = token {tag} {second tag};";
            let g = parse_grammar(s).unwrap();
            let text = "token";
            let p = g.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "other".to_string(),
                        expansion: vec![
                            ParseNode::Token("token".to_string()),
                            ParseNode::Tag("tag".to_string()),
                            ParseNode::Tag("second tag".to_string()),
                        ],
                    }],
                },
            }];
            assert_eq!(p, expected);
        }

        #[test]
        fn ambiguous_diff_tags() {
            init();
            let gr = parse_grammar("public $rule = (t1 {tag1}) <0-2> (t1 {tag2}) <0-2>;").unwrap();
            let text = "t1 t1 t1";
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "rule".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "rule".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag1".to_string()),
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag1".to_string()),
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                    ],
                },
            }];
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_rule_ref() {
            init();
            let gr = parse_grammar("public $root = foo $other; $other = bar;").unwrap();
            let parsed = gr.semantic_parse("foo bar", &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: "foo bar".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![ParseNode::Token("bar".to_string())],
                        },
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_rule_ref_with_tags() {
            init();
            let src = "public $root = foo {tag foo} $other {tag ref}; $other = bar {tag bar};";
            let gr = parse_grammar(src).unwrap();
            let parsed = gr.semantic_parse("foo bar", &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: "foo bar".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Tag("tag foo".to_string()),
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![
                                ParseNode::Token("bar".to_string()),
                                ParseNode::Tag("tag bar".to_string()),
                            ],
                        },
                        ParseNode::Tag("tag ref".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn optional_sequence() {
            init();
            let gr = parse_grammar("public $root = t1 [t2 {tag}] t2;").unwrap();
            let parsed = gr.semantic_parse("t1 t2 t2", &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: "t1 t2 t2".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Token("t2".to_string()),
                        ParseNode::Tag("tag".to_string()),
                        ParseNode::Token("t2".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed);
            let parsed2 = gr.semantic_parse("t1 t2", &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: "t1 t2".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Token("t2".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed2);
        }

        #[test]
        fn optional_rule_ref() {
            init();
            let gr = parse_grammar("public $rule = t1 [$other] t2; $other = t2;").unwrap();
            let parsed = gr.semantic_parse("t1 t2 t2", &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "rule".to_string(),
                text: "t1 t2 t2".to_string(),
                node: ParseNode::Rule {
                    rule_name: "rule".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![ParseNode::Token("t2".to_string())],
                        },
                        ParseNode::Token("t2".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn optional_rule_ref_missing() {
            init();
            let gr = parse_grammar("public $rule = t1 [$other] t2; $other = t2;").unwrap();
            let parsed2 = gr.semantic_parse("t1 t2", &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "rule".to_string(),
                text: "t1 t2".to_string(),
                node: ParseNode::Rule {
                    rule_name: "rule".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Token("t2".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed2);
        }

        #[test]
        fn rule_ref_multiple_same() {
            init();
            let gr = parse_grammar("public $root = $other $other; $other = foo;").unwrap();
            let parsed = gr.semantic_parse("foo foo", &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: "foo foo".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![ParseNode::Token("foo".to_string())],
                        },
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![ParseNode::Token("foo".to_string())],
                        },
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn rule_ref_multiple_repeat() {
            init();
            let gr =
                parse_grammar("public $root = $other <0-> {baz}; $other = foo {bar};").unwrap();
            for n in [0, 1, 2, 5, 10] {
                let text = "foo ".repeat(n);
                let text = text.trim();
                let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
                let mut expansion = vec![];
                for _ in 0..n {
                    expansion.push(ParseNode::Rule {
                        rule_name: "other".to_string(),
                        expansion: vec![
                            ParseNode::Token("foo".to_string()),
                            ParseNode::Tag("bar".to_string()),
                        ],
                    });
                    expansion.push(ParseNode::Tag("baz".to_string()));
                }
                let expected = vec![ParseResult {
                    style: ParsingStyle::Greedy,
                    rule: "root".to_string(),
                    text: text.to_string(),
                    node: ParseNode::Rule {
                        rule_name: "root".to_string(),
                        expansion,
                    },
                }];
                assert_eq!(parsed, expected);
            }
        }

        #[test]
        fn recursion_simple() {
            init();
            // the first applicable rule alternative will be applied,
            // so recursive definition must be before the terminals,
            // otherwise the recursion will not be actually applied
            let s = "public $root = $other; $other = t1 $other | t1 {last} ;";
            let grammar = parse_grammar(s).unwrap();
            let parsed = grammar.semantic_parse("t1 t1 t1", &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: "t1 t1 t1".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "other".to_string(),
                        expansion: vec![
                            ParseNode::Token("t1".to_string()),
                            ParseNode::Rule {
                                rule_name: "other".to_string(),
                                expansion: vec![
                                    ParseNode::Token("t1".to_string()),
                                    ParseNode::Rule {
                                        rule_name: "other".to_string(),
                                        expansion: vec![
                                            ParseNode::Token("t1".to_string()),
                                            ParseNode::Tag("last".to_string()),
                                        ],
                                    },
                                ],
                            },
                        ],
                    }],
                },
            }];
            assert_eq!(parsed, expected)
        }

        #[test]
        fn recursion_embedded() {
            init();
            let gr = parse_grammar("public $root = $other; $other = (t1 $other t2) | $NULL {end};")
                .unwrap();
            let text = "t1 t1 t2 t2";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Greedy,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "other".to_string(),
                        expansion: vec![
                            ParseNode::Token("t1".to_string()),
                            ParseNode::Rule {
                                rule_name: "other".to_string(),
                                expansion: vec![
                                    ParseNode::Token("t1".to_string()),
                                    ParseNode::Rule {
                                        rule_name: "other".to_string(),
                                        expansion: vec![ParseNode::Rule {
                                            rule_name: "NULL".to_string(),
                                            expansion: vec![ParseNode::Tag("end".to_string())],
                                        }],
                                    },
                                    ParseNode::Token("t2".to_string()),
                                ],
                            },
                            ParseNode::Token("t2".to_string()),
                        ],
                    }],
                },
            }];
            assert_eq!(parsed, expected);
        }

        #[test]
        fn special_end() {
            init();
            let gr = parse_grammar("public $root = foo<0-2> $END;").unwrap();
            let text = "foo foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                style: ParsingStyle::Greedy,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Rule {
                            rule_name: "END".to_string(),
                            expansion: vec![],
                        },
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_end_unreachable() {
            init();
            let gr = parse_grammar("public $root = foo<0-2> $END;").unwrap();
            let text = "foo foo foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Greedy, false);
            // we can't reach the END with only 2 foos, so the shouldn't be any results
            assert!(parsed.is_empty());
        }
    }

    mod thorough {
        use crate::{parser::*, semantic_parser::tests::init};

        #[test]
        fn single_token() {
            init();
            let gr = parse_grammar("public $root = foo;").unwrap();
            let text = "foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                style: ParsingStyle::Thorough,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Token("foo".to_string())],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn token_with_tags() {
            init();
            let gr = parse_grammar("public $root = foo {tag1} {tag2};").unwrap();
            let text = "foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Tag("tag1".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                    ],
                },
                rule: "root".to_string(),
                text: text.to_string(),
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_sequence() {
            init();
            let gr = parse_grammar("public $root = t1 $NULL {tag1} t2 {tag2} t3 {tag3};").unwrap();
            let text = "t1 t2 t3";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Rule {
                            rule_name: "NULL".to_string(),
                            expansion: vec![ParseNode::Tag("tag1".to_string())],
                        },
                        ParseNode::Token("t2".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                        ParseNode::Token("t3".to_string()),
                        ParseNode::Tag("tag3".to_string()),
                    ],
                },
                rule: "root".to_string(),
                text: text.to_string(),
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_sequence_with_brackets() {
            init();
            let gr = parse_grammar("public $root = ((t1) $NULL) {tag1} (t2 {tag2} t3);").unwrap();
            let text = "t1 t2 t3";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Rule {
                            rule_name: "NULL".to_string(),
                            expansion: vec![],
                        },
                        ParseNode::Tag("tag1".to_string()),
                        ParseNode::Token("t2".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                        ParseNode::Token("t3".to_string()),
                    ],
                },
                rule: "root".to_string(),
                text: text.to_string(),
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_alternative() {
            init();
            let gr = parse_grammar("public $root = t1 | t2 | t3 ;").unwrap();
            let text = "t2";
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Token("t2".to_string())],
                },
            }];
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn alternative_with_null() {
            init();
            let gr = parse_grammar("public $root = t1 | t2 | $NULL;").unwrap();
            let text = "";
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "NULL".to_string(),
                        expansion: vec![],
                    }],
                },
            }];
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn alternative_ambiguity() {
            init();
            let gr = parse_grammar("public $root = t1 {tag1} | t1 {tag2} | t2;").unwrap();
            let text = "t1";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            assert!(parsed.len() == 2);
        }

        #[test]
        fn special_garbage() {
            init();
            let gr = parse_grammar("public $root = $GARBAGE;").unwrap();
            let text = "foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "GARBAGE".to_string(),
                        expansion: vec![ParseNode::Token("foo".to_string())],
                    }],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn repeat_simple_empty_0() {
            init();
            let gr = parse_grammar("public $root = t1 <0-3>;").unwrap();
            let text = "";
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                style: ParsingStyle::Thorough,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![],
                },
            }];
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            assert_eq!(parsed, expected);
        }

        #[test]
        fn repeat_simple_empty_1() {
            init();
            let gr = parse_grammar("public $root = t1 <0-3>;").unwrap();
            let text = "t1";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![
                ParseResult {
                    rule: "root".to_string(),
                    text: text.to_string(),
                    style: ParsingStyle::Thorough,
                    node: ParseNode::Rule {
                        rule_name: "root".to_string(),
                        expansion: vec![],
                    },
                },
                ParseResult {
                    style: ParsingStyle::Thorough,
                    rule: "root".to_string(),
                    text: text.to_string(),
                    node: ParseNode::Rule {
                        rule_name: "root".to_string(),
                        expansion: vec![ParseNode::Token("t1".to_string())],
                    },
                },
            ];
            assert_eq!(parsed, expected);
        }

        #[test]
        fn repeat_simple_empty_2() {
            init();
            let gr = parse_grammar("public $root = t1 <0-3>;").unwrap();
            let text = "t1 t1";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            assert!(parsed.len() == 3);
        }

        #[test]
        fn repeat_sequence_0() {
            init();
            let gr = parse_grammar("public $root = (t1 {tag}) <0-3>;").unwrap();
            let text = "";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                style: ParsingStyle::Thorough,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![],
                },
            }];
            assert_eq!(parsed, expected);
        }

        #[test]
        fn repeat_sequence_1() {
            init();
            let gr = parse_grammar("public $root = (t1 {tag}) <0-3>;").unwrap();
            let text = "t1";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![
                ParseResult {
                    rule: "root".to_string(),
                    text: text.to_string(),
                    style: ParsingStyle::Thorough,
                    node: ParseNode::Rule {
                        rule_name: "root".to_string(),
                        expansion: vec![],
                    },
                },
                ParseResult {
                    style: ParsingStyle::Thorough,
                    rule: "root".to_string(),
                    text: text.to_string(),
                    node: ParseNode::Rule {
                        rule_name: "root".to_string(),
                        expansion: vec![
                            ParseNode::Token("t1".to_string()),
                            ParseNode::Tag("tag".to_string()),
                        ],
                    },
                },
            ];
            assert_eq!(parsed, expected);
        }

        #[test]
        fn repeat_sequence_3() {
            init();
            let gr = parse_grammar("public $root = (t1 {tag}) <0-3>;").unwrap();
            let text = "t1 t1 t1";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            assert!(parsed.len() == 4);
        }

        #[test]
        fn repeat_undef_max() {
            init();
            let gr = parse_grammar("public $root = (t1 {tag}) <0->;").unwrap();
            let text = "t1 t1 t1";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            assert!(parsed.len() == 4);
        }

        #[test]
        fn special_garbage_with_tags() {
            init();
            let gr = parse_grammar("public $root = $GARBAGE {tag1} {tag2};").unwrap();
            let text = "foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "GARBAGE".to_string(),
                        expansion: vec![
                            ParseNode::Token("foo".to_string()),
                            ParseNode::Tag("tag1".to_string()),
                            ParseNode::Tag("tag2".to_string()),
                        ],
                    }],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_garbage_with_tags_and_repeat() {
            init();
            let gr = parse_grammar("public $root = $GARBAGE <3> {tag};").unwrap();
            let text = "foo bar baz";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Rule {
                            rule_name: "GARBAGE".to_string(),
                            expansion: vec![
                                ParseNode::Token("foo".to_string()),
                                ParseNode::Tag("tag".to_string()),
                            ],
                        },
                        ParseNode::Rule {
                            rule_name: "GARBAGE".to_string(),
                            expansion: vec![
                                ParseNode::Token("bar".to_string()),
                                ParseNode::Tag("tag".to_string()),
                            ],
                        },
                        ParseNode::Rule {
                            rule_name: "GARBAGE".to_string(),
                            expansion: vec![
                                ParseNode::Token("baz".to_string()),
                                ParseNode::Tag("tag".to_string()),
                            ],
                        },
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_null() {
            init();
            let gr = parse_grammar("public $root = $NULL {tag};").unwrap();
            let text = "";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "NULL".to_string(),
                        expansion: vec![ParseNode::Tag("tag".to_string())],
                    }],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_null_repeat() {
            init();
            let gr = parse_grammar("public $root = $NULL <1-3> {tag};");
            assert!(gr.is_err());
        }

        #[test]
        fn text_parse_grammar_simple() {
            init();
            let s = "#ABNF 1.0 UTF-8; public $root = token {tag};";
            let g = parse_grammar(s).unwrap();
            let text = "token";
            let p = g.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("token".to_string()),
                        ParseNode::Tag("tag".to_string()),
                    ],
                },
            }];
            assert_eq!(p, expected);
        }

        #[test]
        fn link_simple() {
            init();
            let s = "#ABNF 1.0 UTF-8; public $root = $other; $other = token {tag} {second tag};";
            let g = parse_grammar(s).unwrap();
            let text = "token";
            let p = g.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Rule {
                        rule_name: "other".to_string(),
                        expansion: vec![
                            ParseNode::Token("token".to_string()),
                            ParseNode::Tag("tag".to_string()),
                            ParseNode::Tag("second tag".to_string()),
                        ],
                    }],
                },
            }];
            assert_eq!(p, expected);
        }

        #[test]
        fn ambiguous_diff_tags() {
            init();
            // NOTE: in this case, even in Throrough parsing,
            // the elements will be exhausted in the order they are written
            // even though it could be possible to match the {tag1} 1x and {tag2} 2x
            let gr = parse_grammar("public $rule = t <0-2> {1} t<0-2> {2} ;").unwrap();
            let text = "t t t";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            // there is 8 possible ways to parse this:
            // 1: []
            // 2: [Token("t"), Tag("2")]
            // 3: [Token("t"), Tag("1")]
            // 4: [Token("t"), Tag("2"), Token("t"), Tag("2")]
            // 5: [Token("t"), Tag("1"), Token("t"), Tag("2")]
            // 6: [Token("t"), Tag("1"), Token("t"), Tag("1")]
            // 7: [Token("t"), Tag("1"), Token("t"), Tag("2"), Token("t"), Tag("2")]
            // 8: [Token("t"), Tag("1"), Token("t"), Tag("1"), Token("t"), Tag("2")]
            assert!(parsed.len() == 8);
        }

        #[test]
        fn simple_rule_ref() {
            init();
            let gr = parse_grammar("public $root = foo $other; $other = bar;").unwrap();
            let parsed = gr.semantic_parse("foo bar", &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: "foo bar".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![ParseNode::Token("bar".to_string())],
                        },
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_rule_ref_with_tags() {
            init();
            let src = "public $root = foo {tag foo} $other {tag ref}; $other = bar {tag bar};";
            let gr = parse_grammar(src).unwrap();
            let parsed = gr.semantic_parse("foo bar", &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: "foo bar".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Tag("tag foo".to_string()),
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![
                                ParseNode::Token("bar".to_string()),
                                ParseNode::Tag("tag bar".to_string()),
                            ],
                        },
                        ParseNode::Tag("tag ref".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn optional_sequence_may_use() {
            init();
            let gr = parse_grammar("public $root = t1 [t2 {tag}] t2;").unwrap();
            let parsed = gr.semantic_parse("t1 t2 t2", &ParsingStyle::Thorough, false);
            assert!(parsed.len() == 2);
        }

        #[test]
        fn optional_sequence_must_use() {
            init();
            // because there is $END as last element, the full sentence must be covered
            // and therefore the optional sequence must be used
            let gr = parse_grammar("public $root = t1 [t2 {tag}] t2 $END;").unwrap();
            let parsed = gr.semantic_parse("t1 t2 t2", &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: "t1 t2 t2".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Token("t2".to_string()),
                        ParseNode::Tag("tag".to_string()),
                        ParseNode::Token("t2".to_string()),
                        ParseNode::Rule {
                            rule_name: "END".to_string(),
                            expansion: vec![],
                        },
                    ],
                },
            }];
            assert_eq!(parsed, expected);
        }

        #[test]
        fn optional_sequence_no_use() {
            init();
            let gr = parse_grammar("public $root = t1 [t2 {tag}] t2;").unwrap();
            let parsed = gr.semantic_parse("t1 t2", &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: "t1 t2".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Token("t2".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn optional_rule_ref_may_use() {
            init();
            let gr = parse_grammar("public $rule = t1 [$other] t2; $other = t2;").unwrap();
            let parsed = gr.semantic_parse("t1 t2 t2", &ParsingStyle::Thorough, false);
            assert!(parsed.len() == 2);
        }

        #[test]
        fn optional_rule_ref_must_use() {
            init();
            let gr = parse_grammar("public $rule = t1 [$other] t2 $END; $other = t2;").unwrap();
            let parsed = gr.semantic_parse("t1 t2 t2", &ParsingStyle::Thorough, false);
            assert!(parsed.len() == 1);
        }

        #[test]
        fn optional_rule_ref_missing() {
            init();
            let gr = parse_grammar("public $rule = t1 [$other] t2; $other = t2;").unwrap();
            let parsed2 = gr.semantic_parse("t1 t2", &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "rule".to_string(),
                text: "t1 t2".to_string(),
                node: ParseNode::Rule {
                    rule_name: "rule".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Token("t2".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed2);
        }

        #[test]
        fn rule_ref_multiple_same() {
            init();
            let gr = parse_grammar("public $root = $other $other; $other = foo;").unwrap();
            let parsed = gr.semantic_parse("foo foo", &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: "foo foo".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![ParseNode::Token("foo".to_string())],
                        },
                        ParseNode::Rule {
                            rule_name: "other".to_string(),
                            expansion: vec![ParseNode::Token("foo".to_string())],
                        },
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn rule_ref_multiple_repeat_0() {
            init();
            let gr =
                parse_grammar("public $root = $other <0-> {baz}; $other = foo {bar};").unwrap();
            let text = "";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                style: ParsingStyle::Thorough,
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![],
                },
            }];
            assert_eq!(parsed, expected);
        }

        #[test]
        fn rule_ref_multiple_repeat_1() {
            init();
            let gr =
                parse_grammar("public $root = $other <0-> {baz}; $other = foo {bar};").unwrap();
            let text = "foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![
                ParseResult {
                    style: ParsingStyle::Thorough,
                    rule: "root".to_string(),
                    text: text.to_string(),
                    node: ParseNode::Rule {
                        rule_name: "root".to_string(),
                        expansion: vec![],
                    },
                },
                ParseResult {
                    style: ParsingStyle::Thorough,
                    rule: "root".to_string(),
                    text: text.to_string(),
                    node: ParseNode::Rule {
                        rule_name: "root".to_string(),
                        expansion: vec![
                            ParseNode::Rule {
                                rule_name: "other".to_string(),
                                expansion: vec![
                                    ParseNode::Token("foo".to_string()),
                                    ParseNode::Tag("bar".to_string()),
                                ],
                            },
                            ParseNode::Tag("baz".to_string()),
                        ],
                    },
                },
            ];
            assert_eq!(parsed, expected);
        }

        #[test]
        fn rule_ref_multiple_repeat_2() {
            init();
            let gr =
                parse_grammar("public $root = $other <0-> {baz}; $other = foo {bar};").unwrap();
            let text = "foo foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            assert!(parsed.len() == 3);
        }

        #[test]
        fn recursion_simple() {
            init();
            // the first applicable rule alternative will be applied,
            // so recursive definition must be before the terminals,
            // otherwise the recursion will not be actually applied
            let s = "public $root = $other; $other = t1 $other | t1 {last} ;";
            let grammar = parse_grammar(s).unwrap();
            let parsed = grammar.semantic_parse("t1 t1 t1", &ParsingStyle::Thorough, false);
            assert!(parsed.len() == 3);
        }

        #[test]
        fn recursion_embedded() {
            init();
            let gr = parse_grammar("public $root = $other; $other = (t1 $other t2) | $NULL {end};")
                .unwrap();
            let text = "t1 t1 t2 t2";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            assert!(parsed.len() == 2);
        }

        #[test]
        fn special_end() {
            init();
            let gr = parse_grammar("public $root = foo<0-2> $END;").unwrap();
            let text = "foo foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                style: ParsingStyle::Thorough,
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Token("foo".to_string()),
                        ParseNode::Rule {
                            rule_name: "END".to_string(),
                            expansion: vec![],
                        },
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_end_unreachable() {
            init();
            let gr = parse_grammar("public $root = foo<0-2> $END;").unwrap();
            let text = "foo foo foo";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            // we can't reach the END with only 2 foos, so the shouldn't be any results
            assert!(parsed.is_empty());
        }

        #[test]
        fn ambiguous_alternatives_with_repeat() {
            init();
            let gr = parse_grammar("public $root = (a {t1} | a {t2}) <0-> b;").unwrap();
            let text = "a a a b";
            let parsed = gr.semantic_parse(text, &ParsingStyle::Thorough, false);
            assert!(parsed.len() == 8);
        }
    }
}
