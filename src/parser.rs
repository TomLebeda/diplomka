use std::path::PathBuf;

use itertools::Itertools;
use log::*;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_until, take_while, take_while1, take_while_m_n};
use nom::character::complete::line_ending;
use nom::combinator::opt;
use nom::combinator::peek;
use nom::multi::{many0, many1, separated_list0, separated_list1};
use nom::number::complete::float;
use nom::sequence::tuple;
use nom::sequence::{delimited, preceded};
use nom::{error_position, IResult};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use rayon::str::ParallelString;

/// if repeat is <m->, then this value will be used as the max
const REPEAT_MAX_DEFAULT: u32 = std::u32::MAX;

#[derive(Debug)]
/// grammar consists of one or more rules
pub struct Grammar {
    /// list of the rules that are defined in this grammar
    rules: Vec<Rule>,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize)]
/// Node in a parse tree from parsing text with semantic grammars.
pub enum ParseNode {
    /// Node representing some token that was matched during semantic parsing (by parent rule-node)
    Token(String),
    /// Node representing some tag that was matched as a result of successful match of left sibling (rule or token or another tag)
    Tag(String),
    /// Node representing some rule that was successfully matched, this is the only node that contains children (sub-tree)
    Rule {
        /// name of the rule that has matched
        rule_name: String,
        /// Expansion of the node => rule match will always expand into sub-tree
        expansion: Vec<ParseNode>,
    },
}

impl Grammar {
    /// Loads and parses grammar from provided file.
    /// Returns Grammar if parsing is successful or prints error and terminates process otherwise.
    pub fn from_file(path: &PathBuf) -> Result<Grammar, Vec<String>> {
        let Ok(raw_str) = std::fs::read_to_string(path) else {
            error!("can't find/read file {}", path.to_string_lossy());
            return Err(vec![format!(
                "can't find/read file {}",
                path.to_string_lossy()
            )]);
        };
        return parse_grammar(&raw_str);
    }

    /// Parse the provided text from the beginning as far as possible.
    /// Attempt to parse each public rule and return vector of ParseResult.
    ///
    /// This implementation starts the parsing process for each public rule in the grammar
    /// at the beginning of the provided string and continues as far as possible.
    /// Returns empty vector if the all public rules fails right at the beginning.
    pub fn parse_text_from_start<'a>(&'a self, text: &'a str) -> Vec<ParseNode> {
        let rule_heap = &self.rules;
        let results = self
            .rules
            .iter()
            .filter(|r| return r.public)
            .filter_map(|rule| {
                let Ok((rest, root_node)) = rule.parse_text(text, rule_heap) else {
                    return None;
                };
                return Some(root_node);
            })
            .collect_vec();
        return results;
    }
}

/// Rule consists of one or more rule alternatives
#[derive(Debug, PartialEq)]
pub struct Rule {
    /// public rules return it's matches from the grammar, private rules are 'hidden'
    public: bool,
    /// name of the rule, must be unique in it's grammar
    name: String,
    /// list of the alternatives, in order
    alternatives: Vec<Alternative>,
}

impl Rule {
    /// Parse provide text with the rule.
    /// Parsing starts at the beginning are continues as far as possible.
    /// If rule has multiple alternatives, they will be tested in the order of declaration
    /// and the first that matches will be returned.
    /// If no alternative matches, Error will be returned as if it was nom::branch::alt() function
    pub fn parse_text<'a>(&'a self, text: &'a str, rule_heap: &[Rule]) -> IResult<&str, ParseNode> {
        for alternative in &self.alternatives {
            if let Ok((rest, matched)) = alternative.parse_text(text, rule_heap) {
                return Ok((rest, matched));
            }
        }
        return Err(nom::Err::Error(error_position!(
            text,
            nom::error::ErrorKind::Alt
        )));
    }
}

/// Rule Alternative consists of one or more Rule Elements
#[derive(Debug, PartialEq)]
pub struct Alternative(Vec<Element>);

impl Alternative {
    /// Parse provide text with the alternative.
    /// Parsing starts at the beginning are continues as far as possible.
    pub fn parse_text<'a>(&'a self, text: &'a str, rule_heap: &[Rule]) -> IResult<&str, ParseNode> {
        let elements = &self.0;
        /// in each step, we can either repeat the same Element or continue with the next one
        enum Operator {
            /// repeat the same Element again
            Repeat,
            /// use the next Element
            Next,
            /// successfully finishing the parsing with the last Element
            End,
        };

        let mut stack: Vec<(&Element, u32, Vec<Operator>)> = vec![];
        let mut first_el: Option<&Element> = None;
        // let Some(first_element) = elements.iter() else {
        //     /// the element list was empty
        //     return Err(nom::Err::Error(error_position!(
        //         text,
        //         nom::error::ErrorKind::SeparatedList
        //     )));
        // };
        while let Some((top_el, top_count, top_ops)) = stack.pop() {
            todo!()
        }
        todo!("parse_text for rule_alternative");
    }
}

/// Tags are attached to some Elements and they will be returned
/// every time the Element is matched.
#[derive(Debug, PartialEq)]
pub struct Tag(String);

/// rule element is an atomic part of rule definition
#[derive(Debug, PartialEq)]
pub enum Element {
    /// literal token (string) that matches the given string
    Token {
        /// string that needs to be matched
        token: String,
        /// minimum number of successive repetitions
        min: u32,
        /// maximum number of successive repetitions
        max: u32,
        /// tags that will be returned every time the token is matched
        tags: Vec<Tag>,
    },
    /// reference to some other rule
    RuleRef {
        /// name of the rule that is referenced
        name: String,
        // rule: &'a Rule<'a>,
        /// minimum number of successive repetitions
        min: u32,
        /// maximum number of successive repetitions
        max: u32,
        /// tags that will be returned every time the referenced Rule is matched
        tags: Vec<Tag>,
    },
    /// Special rule $GARBAGE that matches any single token
    Garbage {
        /// minimum number of repeats
        min: u32,
        /// maximum number of repeats
        max: u32,
        /// tags that will be returned every time the GARBAGE is matched
        tags: Vec<Tag>,
    },
    /// Special rule $VOID that never matches anything
    Void {
        /// minimum number of repeats
        min: u32,
        /// maximum number of repeats
        max: u32,
    },
    /// Special rule $NULL that always matches (matches zero-length string)
    Null {
        /// minimum number of repeats
        min: u32,
        /// maximum number of repeats
        max: u32,
        /// tags that will be returned every time the NULL is matched
        tags: Vec<Tag>,
    },
    /// tag that is returned when previous non-tag element is matched
    // Tag {
    //     /// contents of the tag, can't contain curly brackets and semicolons
    //     tag: String,
    // },
    /// Sequence of rule alternatives behaves basically the same as a Rule with additional repeat operators,
    /// but it is 'baked in' and can't be referenced from outside.
    Sequence {
        /// list of the alternatives in the sequence
        alternatives: Vec<Alternative>,
        /// minimum number of successive repetitions
        min: u32,
        /// maximum number of successive repetitions
        max: u32,
        /// tags that will be returned every time the sequence is matched
        tags: Vec<Tag>,
    },
}

/// Tries to parse grammar from provided string.
/// Returns Some(Grammar) if parsing is successful or None,
pub fn parse_grammar(s: &str) -> Result<Grammar, Vec<String>> {
    let input = s.trim_start();
    let Ok((rest, _)) = header(input) else {
        return Err(vec!["missing or invalid header".to_string()]);
    };
    match rules(rest) {
        Ok(rules) => return Ok(Grammar { rules }),
        Err(errs) => {
            return Err(errs);
        }
    }
}

/// Tries to parse header "#ABNF 1.0 UTF-8;" from provided string.
/// This exists for compatibility reasons with full SRGS-ABNF specification.
fn header(s: &str) -> IResult<&str, ()> {
    let input = s.trim_start();
    let (rest, _) = tag("#ABNF 1.0 UTF-8;")(input)?;
    return Ok((rest, ()));
}

/// tries to parse provided string into list of rules
fn rules(s: &str) -> Result<Vec<Rule>, Vec<String>> {
    let (rules, errs): (Vec<_>, Vec<_>) = s
        .par_split_terminator(|c| return c == ';')
        .map(|s| return s.trim_start())
        .map(rule)
        .partition(Result::is_ok);
    let rules: Vec<Rule> = rules.into_iter().map(Result::unwrap).collect_vec();
    let errs: Vec<String> = errs.into_iter().map(Result::unwrap_err).collect_vec();
    if errs.is_empty() {
        return Ok(rules);
    } else {
        return Err(errs);
    }
}

/// Tries to parse provided string into Rule
fn rule(s: &str) -> Result<Rule, String> {
    let input = s.trim_start();
    match tuple((
        opt(tag("public")),
        whitespace,
        rule_name,
        whitespace,
        tag("="),
        whitespace,
        rule_body,
    ))(input)
    {
        Ok((rest, matched)) => {
            let (maybe_public, _, name, _, _, _, body) = matched;
            let rule = Rule {
                public: maybe_public.is_some(),
                name: name.to_string(),
                alternatives: body,
            };
            return Ok(rule);
        }
        Err(e) => {
            return Err(format!("failed to parse rule from: {s}, err: {e}"));
        }
    };
}

/// Tries to parse provided string into the body of a Rule
/// Rule body is one or more rule alternatives separated by pipeline
fn rule_body(s: &str) -> IResult<&str, Vec<Alternative>> {
    let input = s.trim_start();
    return separated_list1(tuple((whitespace, tag("|"), whitespace)), rule_alternative)(input);
}

/// Tries to parse provided string into the body of
/// Rule Alternative is a sequence of one or more [Element]s separated by space
fn rule_alternative(s: &str) -> IResult<&str, Alternative> {
    let input = s.trim_start();
    let (rest, elements) = many1(element)(input)?;
    return Ok((rest, Alternative(elements)));
}

/// Tries to parse provided string into valid rule name, that being $ followed by any unicode alphanumeric character or underscore
fn rule_name(s: &str) -> IResult<&str, &str> {
    let (rest, (name)) = preceded(
        tag("$"),
        take_while1(|c| return nom_unicode::is_alphanumeric(c) || c == '_'),
    )(s)?;
    return Ok((rest, name));
}

/// Tries to parse input str and returns an element
/// Element is one of Token, Tag, Rule (reference by name) or Sequence
fn element(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    // NOTE: rule_ref must be after the special variants!
    let res = alt((
        token,
        special_garbage,
        special_void,
        special_null,
        rule_ref,
        sequence,
    ))(input);
    return res;
}

/// Tries to parse input str and return [Element::Void]
fn special_void(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    let (rest, (rule_name, _, maybe_repeat)) =
        tuple((tag("$VOID"), whitespace, opt(repeat)))(input)?;
    if let Some((min, max)) = maybe_repeat {
        return Ok((rest, Element::Void { min, max }));
    } else {
        return Ok((rest, Element::Void { min: 1, max: 1 }));
    }
}

/// Tries to parse input str and return [Element::Null]
fn special_null(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    let (rest, (rule_name, _, maybe_repeat, _, tags)) = tuple((
        tag("$NULL"),
        whitespace,
        opt(repeat),
        whitespace,
        many0(grammar_tag),
    ))(input)?;
    if let Some((min, max)) = maybe_repeat {
        return Ok((rest, Element::Null { min, max, tags }));
    } else {
        return Ok((
            rest,
            Element::Null {
                min: 1,
                max: 1,
                tags,
            },
        ));
    }
}

/// Tries to parse input str and return [Element::Garbage]
fn special_garbage(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    let (rest, (rule_name, _, maybe_repeat, _, tags)) = tuple((
        tag("$GARBAGE"),
        whitespace,
        opt(repeat),
        whitespace,
        many0(grammar_tag),
    ))(input)?;
    if let Some((min, max)) = maybe_repeat {
        return Ok((rest, Element::Garbage { min, max, tags }));
    } else {
        return Ok((
            rest,
            Element::Garbage {
                min: 1,
                max: 1,
                tags,
            },
        ));
    }
}

/// Tries to parse input str and return [Element::RuleRef]
fn rule_ref(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    let (rest, (rule_name, _, maybe_repeat, _, tags)) = tuple((
        rule_name,
        whitespace,
        opt(repeat),
        whitespace,
        many0(grammar_tag),
    ))(input)?;
    let name = rule_name.to_string();
    if let Some((min, max)) = maybe_repeat {
        return Ok((
            rest,
            Element::RuleRef {
                name,
                min,
                max,
                tags,
            },
        ));
    } else {
        return Ok((
            rest,
            Element::RuleRef {
                name,
                min: 1,
                max: 1,
                tags,
            },
        ));
    }
}

/// Tries to parse input str and return [Element::Tag]
/// Tag is any string between curly brackets.
/// Tags must not contain curly brackets and semicolons as it will break parsing.
fn grammar_tag(s: &str) -> IResult<&str, Tag> {
    let input = s.trim_start();
    let (rest, matched) = delimited(tag("{"), take_until("}"), tag("}"))(input)?;
    return Ok((rest, Tag(matched.to_string())));
}

/// Tries to parse input str and return [Element::Sequence]
fn sequence(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    return alt((braced_sequnece, optional_sequence))(input);
}

/// Tries to parse provided string into [Element::Sequence]
/// where the sequence is surrounded in square brackets [].
/// In this way, the whole sequence is optional, that being as if it had repeat <0-1>
fn optional_sequence(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    let (rest, (mut sequence, _, mut seq_tags)) = tuple((
        delimited(
            tuple((tag("["), whitespace)),
            bare_sequence,
            tuple((whitespace, tag("]"))),
        ),
        whitespace,
        many0(grammar_tag),
    ))(input)?;
    match sequence {
        Element::Sequence {
            mut alternatives,
            min,
            max,
            tags: _,
        } => {
            return Ok((
                rest,
                Element::Sequence {
                    alternatives,
                    min: 0,
                    max,
                    tags: seq_tags,
                },
            ));
        }
        Element::Token {
            token,
            min,
            max,
            tags,
        } => {
            // if the inner element has only the default repeat <1-1>,
            // then it can take over the repeat operator of the sequence
            if min == 1 && max == 1 {
                let min = 0;
                let mut merged_tags = tags;
                merged_tags.append(&mut seq_tags);
                return Ok((
                    rest,
                    Element::Token {
                        token,
                        min,
                        max,
                        tags: merged_tags,
                    },
                ));
            } else {
                // otherwise don't mess with the repeats and return proper one-element sequence
                // where the repeats are individual
                return Ok((
                    rest,
                    Element::Sequence {
                        alternatives: vec![Alternative(vec![Element::Token {
                            token,
                            min,
                            max,
                            tags,
                        }])],
                        min: 0,
                        max: 1,
                        tags: seq_tags,
                    },
                ));
            }
        }
        Element::RuleRef {
            name,
            min,
            max,
            tags,
        } => {
            if min == 1 && max == 1 {
                let min = 0;
                let mut merged_tags = tags;
                merged_tags.append(&mut seq_tags);
                return Ok((
                    rest,
                    Element::RuleRef {
                        name,
                        min,
                        max,
                        tags: merged_tags,
                    },
                ));
            } else {
                let min = 0;
                return Ok((
                    rest,
                    Element::Sequence {
                        alternatives: vec![Alternative(vec![Element::RuleRef {
                            name,
                            min,
                            max,
                            tags,
                        }])],
                        min,
                        max,
                        tags: seq_tags,
                    },
                ));
            }
        }
        Element::Garbage { min, max, tags } => {
            if min == 1 && max == 1 {
                let min = 0;
                let mut merged_tags = tags;
                merged_tags.append(&mut seq_tags);
                return Ok((
                    rest,
                    Element::Garbage {
                        min,
                        max,
                        tags: merged_tags,
                    },
                ));
            } else {
                let min = 0;
                return Ok((
                    rest,
                    Element::Sequence {
                        alternatives: vec![Alternative(vec![Element::Garbage { min, max, tags }])],
                        min,
                        max,
                        tags: seq_tags,
                    },
                ));
            }
        }
        Element::Void { min, max } => {
            if min == 1 && max == 1 {
                let min = 0;
                return Ok((rest, Element::Void { min, max }));
            } else {
                let min = 0;
                return Ok((
                    rest,
                    Element::Sequence {
                        alternatives: vec![Alternative(vec![Element::Void { min, max }])],
                        min,
                        max,
                        tags: seq_tags,
                    },
                ));
            }
        }
        Element::Null { min, max, tags } => {
            if min == 1 && max == 1 {
                let min = 0;
                let mut merged_tags = tags;
                merged_tags.append(&mut seq_tags);
                return Ok((
                    rest,
                    Element::Null {
                        min,
                        max,
                        tags: merged_tags,
                    },
                ));
            } else {
                let min = 0;
                return Ok((
                    rest,
                    Element::Sequence {
                        alternatives: vec![Alternative(vec![Element::Null { min, max, tags }])],
                        min,
                        max,
                        tags: seq_tags,
                    },
                ));
            }
        }
    }
}

/// Tries to parse provided string into [Element::Sequence]
/// where the sequence is surrounded in braces ().
/// In this way, the whole sequence may have some additional repeat operator attached to it.
fn braced_sequnece(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    let (rest, (mut sequence, _, maybe_repeat, _, mut seq_tags)) = tuple((
        delimited(
            tuple((tag("("), whitespace)),
            bare_sequence,
            tuple((whitespace, tag(")"))),
        ),
        whitespace,
        opt(repeat),
        whitespace,
        many0(grammar_tag),
    ))(input)?;
    if let Some((seq_min, seq_max)) = maybe_repeat {
        match sequence {
            Element::Sequence {
                alternatives,
                min,
                max,
                tags,
            } => {
                return Ok((
                    rest,
                    Element::Sequence {
                        alternatives,
                        min: seq_min,
                        max: seq_max,
                        tags: seq_tags,
                    },
                ));
            }
            Element::Token {
                token,
                min,
                max,
                tags,
            } => {
                // if the inner element has only the default repeat <1-1>,
                // then it can take over the repeat operator of the sequence
                if min == 1 && max == 1 {
                    let mut merged_tags = tags;
                    merged_tags.append(&mut seq_tags);
                    return Ok((
                        rest,
                        Element::Token {
                            token,
                            min: seq_min,
                            max: seq_max,
                            tags: merged_tags,
                        },
                    ));
                } else {
                    // otherwise don't mess with the repeats and return proper one-element sequence
                    // where the repeats are individual
                    return Ok((
                        rest,
                        Element::Sequence {
                            alternatives: vec![Alternative(vec![Element::Token {
                                token,
                                min,
                                max,
                                tags,
                            }])],
                            min: seq_min,
                            max: seq_max,
                            tags: seq_tags,
                        },
                    ));
                }
            }
            Element::RuleRef {
                name,
                min,
                max,
                tags,
            } => {
                if min == 1 && max == 1 {
                    let mut merged_tags = tags;
                    merged_tags.append(&mut seq_tags);
                    return Ok((
                        rest,
                        Element::RuleRef {
                            name,
                            min: seq_min,
                            max: seq_max,
                            tags: merged_tags,
                        },
                    ));
                } else {
                    return Ok((
                        rest,
                        Element::Sequence {
                            alternatives: vec![Alternative(vec![Element::RuleRef {
                                name,
                                min,
                                max,
                                tags,
                            }])],
                            min: seq_min,
                            max: seq_max,
                            tags: seq_tags,
                        },
                    ));
                }
            }
            Element::Garbage { min, max, tags } => {
                if min == 1 && max == 1 {
                    let mut merged_tags = tags;
                    merged_tags.append(&mut seq_tags);
                    return Ok((
                        rest,
                        Element::Garbage {
                            min: seq_min,
                            max: seq_max,
                            tags: merged_tags,
                        },
                    ));
                } else {
                    return Ok((
                        rest,
                        Element::Sequence {
                            alternatives: vec![Alternative(vec![Element::Garbage {
                                min,
                                max,
                                tags,
                            }])],
                            min: seq_min,
                            max: seq_max,
                            tags: seq_tags,
                        },
                    ));
                }
            }
            Element::Void { min, max } => {
                if min == 1 && max == 1 {
                    return Ok((rest, Element::Void { min, max }));
                } else {
                    return Ok((
                        rest,
                        Element::Sequence {
                            alternatives: vec![Alternative(vec![Element::Void { min, max }])],
                            min: seq_min,
                            max: seq_max,
                            tags: seq_tags,
                        },
                    ));
                }
            }
            Element::Null { min, max, tags } => {
                if min == 1 && max == 1 {
                    let mut merged_tags = tags;
                    merged_tags.append(&mut seq_tags);
                    return Ok((
                        rest,
                        Element::Null {
                            min,
                            max,
                            tags: merged_tags,
                        },
                    ));
                } else {
                    return Ok((
                        rest,
                        Element::Sequence {
                            alternatives: vec![Alternative(vec![Element::Null { min, max, tags }])],
                            min: seq_min,
                            max: seq_max,
                            tags: seq_tags,
                        },
                    ));
                }
            }
        }
    }
    return Ok((rest, sequence));
}

/// Tries to parse input str and return [Element::Sequence]
/// where the sequence isn't surrounded by [] nor ().
/// The sequence is one or more Elements separated by spaces.
fn bare_sequence(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    // Element::Sequence has the same form as a rule body
    let (rest, mut alternatives) = rule_body(input)?;

    // OPTIM:
    // if there is only one alternative in the sequence,
    // and inside that alternative is only one element,
    // then simplify by returning the single element directly
    // and not wrapped in a sequence

    // we need to check both conditions at once, otherwise we may pass the first one,
    // remove item from the vector and not pass the second one leaving empty vector and invalid output
    let only_one_element = alternatives.len() == 1 && alternatives[0].0.len() == 1;
    if only_one_element {
        let elem = alternatives.swap_remove(0).0.swap_remove(0);
        return Ok((rest, elem));
    }
    /// sequence "seq" that isn't surrounded by [] or () must be matched once
    let sequence = Element::Sequence {
        alternatives,
        min: 1,
        max: 1,
        tags: vec![], // bare sequence doesn't have any tags, they are specified after () or []
    };
    return Ok((rest, sequence));
}

/// Tries to parse input str and return [Element::Token]
fn token(input: &str) -> IResult<&str, Element> {
    let input = input.trim_start();
    let (rest, (token, tags)) = tuple((
        take_while1(|c| return nom_unicode::is_alphanumeric(c) || ".-_:".contains(c)),
        many0(grammar_tag),
    ))(input)?;
    let token = token.to_owned();
    let input = rest.trim_start();
    if input.starts_with('<') {
        // the next char is <, so there must be some repeat specifier
        let (rest, (min, max)) = repeat(input)?;
        return Ok((
            rest,
            Element::Token {
                token,
                min,
                max,
                tags,
            },
        ));
    } else {
        // the next char is not <, so there is not a repeat specifier
        let min = 1;
        let max = 1;
        return Ok((
            input,
            Element::Token {
                token,
                min,
                max,
                tags,
            },
        ));
    }
}

/// Tries to parse input str and returns a repeat specifier
/// allowed syntax:
/// <m-n> = element must be repeated at least m times and at most n times (both inclusive)
/// <m-> = element must be repeated at least m times (inclusive) and at most [REPEAT_MAX_DEFAULT]
/// <m> = element must be repeated exactly m times
/// no whitespace allowed inside the angle brackets
/// no repeat specification is equal to <1>
fn repeat(input: &str) -> IResult<&str, (u32, u32)> {
    let input = input.trim_start();
    let (rest, (min, maybe_second_part)) = delimited(
        tag("<"),
        tuple((
            nom::character::complete::u32,
            opt(tuple((tag("-"), opt(nom::character::complete::u32)))),
        )),
        tag(">"),
    )(input)?;
    // if there is <m>, then fill it to be <m-n>, where n = m
    // if there is <m->, then fill it to be <m-n>, where n = [REPEAT_MAX_DEFAULT]
    // and unwrap the n value into 'max'
    let max = maybe_second_part
        .unwrap_or(("", Some(min)))
        .1
        .unwrap_or(REPEAT_MAX_DEFAULT);
    return Ok((rest, (min, max)));
}

/// Consumes all leading whitespace including line breaks
fn whitespace(input: &str) -> IResult<&str, ()> {
    let (remaining, _matched) = take_while(|c: char| -> bool {
        return c.is_whitespace();
    })(input)?;
    return Ok((remaining, ()));
}

/// unit tests for the 'parser.rs'
#[cfg(test)]
mod test {
    use itertools::Itertools;

    use crate::parser::{
        repeat, rule_alternative, rule_body, rule_name, Alternative, Element, Rule,
        REPEAT_MAX_DEFAULT,
    };

    use super::{grammar_tag, rule, sequence, token, Tag};

    fn new_token(s: &str, min: u32, max: u32, tags: &[&str]) -> Element {
        return Element::Token {
            token: s.to_string(),
            min,
            max,
            tags: tags.iter().map(|t| return Tag(t.to_string())).collect_vec(),
        };
    }

    #[test]
    /// test the repeat parsing
    fn parse_repeat() {
        let (rest, rep) = repeat("<1-2>").unwrap();
        assert_eq!((1, 2), rep);
        assert!(rest.is_empty());

        let (rest, rep) = repeat("<1->").unwrap();
        assert_eq!((1, REPEAT_MAX_DEFAULT), rep);
        assert!(rest.is_empty());

        let (rest, rep) = repeat("<3>").unwrap();
        assert_eq!((3, 3), rep);
        assert!(rest.is_empty());
    }

    #[test]
    fn parse_token() {
        for s in ["foo", "foo<1>", "foo <1>", "foo <1-1>"] {
            let (rest, matched) = token(s).unwrap();
            assert_eq!("", rest);
            assert_eq!(matched, new_token("foo", 1, 1, &[]));
        }

        for s in ["foo<1-3>", "foo <1-3>"] {
            let (rest, matched) = token(s).unwrap();
            assert_eq!("", rest);
            assert_eq!(matched, new_token("foo", 1, 3, &[]));
        }

        for s in ["foo<3->", "foo <3->"] {
            let (rest, matched) = token(s).unwrap();
            assert_eq!("", rest);
            assert_eq!(matched, new_token("foo", 3, REPEAT_MAX_DEFAULT, &[]));
        }
    }

    #[test]
    fn rule_names() {
        let s = "$rule";
        let (rest, name) = rule_name(s).unwrap();
        assert_eq!("", rest);
        assert_eq!("rule", name);
    }

    #[test]
    fn rule_alternatives() {
        let s = "foo bar baz";
        let (rest, matched) = rule_alternative(s).unwrap();
        assert_eq!("", rest);
        let expected = Alternative(vec![
            new_token("foo", 1, 1, &[]),
            new_token("bar", 1, 1, &[]),
            new_token("baz", 1, 1, &[]),
        ]);
        assert_eq!(expected, matched);
    }

    // #[test]
    // fn rule_special() {
    //     assert_eq!(Rule::Garbage, rule("$GARBAGE").unwrap());
    //     assert_eq!(Rule::Void, rule("$VOID").unwrap());
    //     assert_eq!(Rule::Null, rule("$NULL").unwrap());
    // }

    #[test]
    fn rule_simple() {
        let s = "$rule = foo bar | baz";
        let rule = rule(s).unwrap();
        let expected = Rule {
            public: false,
            name: String::from("rule"),
            alternatives: vec![
                Alternative(vec![
                    new_token("foo", 1, 1, &[]),
                    new_token("bar", 1, 1, &[]),
                ]),
                Alternative(vec![new_token("baz", 1, 1, &[])]),
            ],
        };
        assert_eq!(expected, rule);
    }

    #[test]
    fn rule_public() {
        let s = "public $rule = foo ";
        let rule = rule(s).unwrap();
        let expected = Rule {
            public: true,
            name: String::from("rule"),
            alternatives: vec![Alternative(vec![new_token("foo", 1, 1, &[])])],
        };
        assert_eq!(expected, rule);
    }

    #[test]
    fn rule_with_opts() {
        let s = "$rule = [foo bar] ";
        let rule = rule(s).unwrap();
        let expected = Rule {
            public: false,
            name: String::from("rule"),
            alternatives: vec![Alternative(vec![Element::Sequence {
                alternatives: vec![Alternative(vec![
                    new_token("foo", 1, 1, &[]),
                    new_token("bar", 1, 1, &[]),
                ])],
                min: 0,
                max: 1,
                tags: vec![],
            }])],
        };
        assert_eq!(expected, rule);
    }

    #[test]
    fn rule_with_repeats() {
        let s = "$rule = small <3-10> | medium <5-> | large";
        let expected = Rule {
            public: false,
            name: String::from("rule"),
            alternatives: vec![
                Alternative(vec![new_token("small", 3, 10, &[])]),
                Alternative(vec![new_token("medium", 5, REPEAT_MAX_DEFAULT, &[])]),
                Alternative(vec![new_token("medium", 1, 1, &[])]),
            ],
        };
    }

    #[test]
    fn tag() {
        let s = "{tag}";
        let (rest, t) = grammar_tag(s).unwrap();
        let expected = Tag("tag".to_string());
        assert_eq!(t, expected);
    }

    #[test]
    fn rule_with_tag() {
        let s = "$rule = foo {tag}";
        let r = rule(s).unwrap();
        let expected = Rule {
            public: false,
            name: String::from("rule"),
            alternatives: vec![Alternative(vec![new_token("foo", 1, 1, &["tag"])])],
        };
        assert_eq!(expected, r);
    }

    #[test]
    fn rule_with_tags() {
        let s = "$rule = foo {t} {t2}";
        let r = rule(s).unwrap();
        let expected = Rule {
            public: false,
            name: String::from("rule"),
            alternatives: vec![Alternative(vec![new_token("foo", 1, 1, &["t", "t2"])])],
        };
        assert_eq!(expected, r);
    }

    #[test]
    fn rule_with_refs() {
        let s = "$rule = $another";
        let r = rule(s).unwrap();
        let expected = Rule {
            public: false,
            name: String::from("rule"),
            alternatives: vec![Alternative(vec![Element::RuleRef {
                name: String::from("another"),
                min: 1,
                max: 1,
                tags: vec![],
            }])],
        };
        assert_eq!(expected, r);
    }

    #[test]
    fn rule_with_tag_simple() {
        let s = "$rule = foo {tag}";
        let rule = rule(s).unwrap();
        let expected = Rule {
            public: false,
            name: "rule".to_string(),
            alternatives: vec![Alternative(vec![new_token("foo", 1, 1, &["tag"])])],
        };
        assert_eq!(expected, rule);
    }

    #[test]
    fn rule_with_tag_complex() {
        let s = "$rule = (foo {tag} {tag2})";
        let rule = rule(s).unwrap();
        let expected = Rule {
            public: false,
            name: "rule".to_string(),
            alternatives: vec![Alternative(vec![new_token("foo", 1, 1, &["tag", "tag2"])])],
        };
        assert_eq!(expected, rule);
    }

    #[test]
    fn rule_with_sequence_with_repeat() {
        let s = "$rule = (foo bar)<3-4>";
        let r = rule(s).unwrap();
        let expected = Rule {
            public: false,
            name: "rule".to_string(),
            alternatives: vec![Alternative(vec![Element::Sequence {
                alternatives: vec![Alternative(vec![
                    new_token("foo", 1, 1, &[]),
                    new_token("bar", 1, 1, &[]),
                ])],
                min: 3,
                max: 4,
                tags: vec![],
            }])],
        };
        assert_eq!(r, expected);
    }

    #[test]
    fn optimize_sequence() {
        let s = "$rule = (foo)";
        let r = rule(s).unwrap();
        let expected = Rule {
            public: false,
            name: "rule".to_string(),
            alternatives: vec![Alternative(vec![new_token("foo", 1, 1, &[])])],
        };
        assert_eq!(expected, r);
    }

    #[test]
    fn optimize_optional_sequence() {
        let s = "$rule = [foo]";
        let r = rule(s).unwrap();
        let expected = Rule {
            public: false,
            name: "rule".to_string(),
            alternatives: vec![Alternative(vec![new_token("foo", 0, 1, &[])])],
        };
        assert_eq!(expected, r);
    }

    #[test]
    fn optimize_sequence_with_outer_repeat() {
        let s = "$rule = (foo)<3-4>";
        let r = rule(s).unwrap();
        let expected = Rule {
            public: false,
            name: "rule".to_string(),
            alternatives: vec![Alternative(vec![new_token("foo", 3, 4, &[])])],
        };
        assert_eq!(expected, r);
    }

    #[test]
    fn optimize_sequence_with_inner_repeat() {
        let s = "$rule = [foo<3-4>]";
        let r = rule(s).unwrap();
        let expected = Rule {
            public: false,
            name: "rule".to_string(),
            alternatives: vec![Alternative(vec![Element::Sequence {
                alternatives: vec![Alternative(vec![new_token("foo", 3, 4, &[])])],
                min: 0,
                max: 1,
                tags: vec![],
            }])],
        };
        assert_eq!(expected, r);
    }
}
