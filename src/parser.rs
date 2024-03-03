use std::borrow::Cow;
use std::fmt::Display;
use std::path::PathBuf;

use itertools::Itertools;
use log::*;
use nom::branch::alt;
use nom::bytes::complete::{
    is_not, tag, take_till, take_till1, take_until, take_while, take_while1, take_while_m_n,
};
use nom::character::complete::line_ending;
use nom::character::is_newline;
use nom::combinator::peek;
use nom::combinator::{fail, opt};
use nom::multi::{count, many0, many1, many_m_n, separated_list0, separated_list1};
use nom::number::complete::float;
use nom::sequence::tuple;
use nom::sequence::{delimited, preceded};
use nom::{error_position, IResult};
use nom_unicode::is_whitespace;
use ptree::print_config::StyleWhen;
use ptree::{print_tree, write_tree, write_tree_with, PrintConfig, Style, TreeItem};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use rayon::str::ParallelString;
use reqwest::blocking::get;

/// if repeat is <m->, then this value will be used as the max
const REPEAT_MAX_DEFAULT: u32 = std::u32::MAX;

#[derive(Debug, PartialEq)]
/// grammar consists of one or more rules
pub struct Grammar {
    /// list of the rules that are defined in this grammar
    rules: Vec<Rule>,
}

#[derive(Debug, PartialEq)]
/// top-level wrapper around [ParseNode] trees
/// with some additional information about the parsing result
pub struct ParseResult {
    /// name of the root rule (marked as public in the grammar)
    pub rule: String,
    /// the top-level [ParseNode]
    pub node: ParseNode,
    /// the original raw parsed string
    pub text: String,
}

#[derive(Debug, PartialEq, serde::Serialize, serde::Deserialize, Clone)]
/// Node in a parse tree from parsing text with semantic grammars.
pub enum ParseNode {
    /// Node representing some token that was matched during semantic parsing (by parent rule-node)
    Token(String),
    /// Node representing some tag that was returned by previously matched [Element]
    Tag(String),
    /// Node representing some rule that was successfully matched, this is the only node that contains children (sub-tree)
    Rule {
        /// name of the rule that has matched
        rule_name: String,
        /// Expansion of the node => rule match will always expand into sub-tree
        expansion: Vec<ParseNode>,
    },
}

impl ParseNode {
    /// Traverses the [ParseNode] tree and returns a vector of all tags that have been visited.
    /// The traversal is depth-first-post-order.
    pub fn tags_dfpo(&self) -> Vec<String> {
        match self {
            ParseNode::Token(_) => return vec![],
            ParseNode::Tag(tag) => return vec![tag.clone()],
            ParseNode::Rule {
                rule_name,
                expansion,
            } => {
                return expansion.iter().fold(vec![], |mut acc, e: &ParseNode| {
                    acc.append(&mut e.tags_dfpo());
                    return acc;
                })
            }
        }
    }

    /// Constructs a compact human-readable text representation of the [ParseTree].
    /// SPT stands for Semantic Parse Tree format
    pub fn construct_spt(&self, styled: bool) -> String {
        let mut buf = Vec::new();
        let conf = PrintConfig {
            styled: StyleWhen::Never,
            // styled: if styled {
            //     StyleWhen::Always
            // } else {
            //     StyleWhen::Never
            // },
            ..Default::default()
        };
        write_tree_with(self, &mut buf, &conf);
        let s = String::from_utf8_lossy(&buf);
        return s.to_string();
    }

    /// returns the default style for the node type
    pub fn get_spt_style(&self) -> Style {
        match &self {
            ParseNode::Token(..) => {
                return Style {
                    foreground: Some(ptree::Color::White),
                    background: Some(ptree::Color::Black),
                    bold: false,
                    dimmed: false,
                    italic: false,
                    underline: false,
                    blink: false,
                    reverse: false,
                    hidden: false,
                    strikethrough: false,
                }
            }
            ParseNode::Tag(..) => {
                return Style {
                    foreground: Some(ptree::Color::Blue),
                    background: Some(ptree::Color::Black),
                    bold: false,
                    dimmed: false,
                    italic: false,
                    underline: false,
                    blink: false,
                    reverse: false,
                    hidden: false,
                    strikethrough: false,
                }
            }
            ParseNode::Rule { .. } => {
                return Style {
                    foreground: Some(ptree::Color::Purple),
                    background: Some(ptree::Color::Black),
                    bold: true,
                    dimmed: false,
                    italic: false,
                    underline: false,
                    blink: false,
                    reverse: false,
                    hidden: false,
                    strikethrough: false,
                }
            }
        }
    }
}

impl TreeItem for ParseNode {
    type Child = Self;

    fn write_self<W: std::io::Write>(
        &self,
        f: &mut W,
        style: &ptree::Style,
    ) -> std::io::Result<()> {
        match self {
            ParseNode::Token(s) => return write!(f, "\"{}\"", self.get_spt_style().paint(s)),
            ParseNode::Tag(s) => return write!(f, "{{{}}}", self.get_spt_style().paint(s)),
            ParseNode::Rule { rule_name, .. } => {
                return write!(
                    f,
                    "{}",
                    self.get_spt_style().paint(format!("${}", rule_name))
                )
            }
        }
    }

    fn children(&self) -> std::borrow::Cow<[Self::Child]> {
        match self {
            ParseNode::Token(..) => return Cow::from(vec![]),
            ParseNode::Tag(..) => return Cow::from(vec![]),
            ParseNode::Rule { expansion, .. } => return Cow::from(expansion),
        }
    }
}

impl Display for ParseNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            ParseNode::Tag(s) => {
                return write!(f, "Tag({s})");
            }
            ParseNode::Token(s) => {
                return write!(f, "Token({s})");
            }
            ParseNode::Rule {
                rule_name,
                expansion,
            } => {
                return write!(
                    f,
                    "Rule(${}) -> [\n{}]",
                    rule_name,
                    expansion
                        .iter()
                        .map(|node| return format!("\t{}\n", node))
                        .join("")
                )
            }
        }
    }
}

impl Grammar {
    /// Loads and parses grammar from provided file.
    /// Returns Grammar if parsing is successful or prints error and terminates process otherwise.
    pub fn from_file(path: PathBuf) -> Result<Grammar, Vec<String>> {
        let Ok(raw_str) = std::fs::read_to_string(&path) else {
            error!("can't find/read file {}", &path.to_string_lossy());
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
    pub fn parse_text_from_start<'a>(&'a self, text: &'a str) -> Vec<ParseResult> {
        let rule_heap = &self.rules;
        let results = self
            .rules
            .iter()
            .filter(|r| return r.public)
            .filter_map(|rule| {
                let Ok((rest, root_node)) = rule.parse_text(text, rule_heap) else {
                    return None;
                };
                return Some(ParseResult {
                    rule: rule.name.clone(),
                    node: root_node,
                    text: text.to_string(),
                });
            })
            .collect_vec();
        return results;
    }

    /// Searches for all possible parse trees within provided text.
    ///
    /// This implementation tries to parse the string from the beginning as far as possible.
    /// After the parsing fails (or reaches the end), the starting point is shifted one word over
    /// (to the next whitespace) and tries to parse again.
    /// All the successful parsing results are collected and returned.
    pub fn find_all(&self, text: &str) -> Vec<ParseResult> {
        let mut results = self.parse_text_from_start(text);
        text.match_indices(|c: char| return c.is_whitespace())
            .for_each(|(idx, _)| {
                let (lead, to_parse) = text.split_at(idx);
                results.append(&mut self.parse_text_from_start(to_parse));
            });
        return results;
    }
}

/// Rule consists of one or more rule alternatives
#[derive(Debug, PartialEq, PartialOrd)]
pub struct Rule {
    /// public rules return it's matches from the grammar, private rules are 'hidden'
    public: bool,
    /// name of the rule, must be unique in it's grammar
    name: String,
    /// list of the alternatives, in order
    expansion: Vec<Alternative>,
}

impl Rule {
    /// Parse provide text with the rule.
    /// Parsing starts at the beginning are continues as far as possible.
    /// If rule has multiple alternatives, they will be tested in the order of declaration
    /// and the first that matches will be returned.
    /// If no alternative matches, Error will be returned as if it was [nom::branch::alt()] function
    pub fn parse_text<'a>(
        &'a self,
        text: &'a str,
        rule_heap: &'a [Rule],
    ) -> IResult<&str, ParseNode> {
        for alternative in &self.expansion {
            if let Ok((rest, matched)) = alternative.parse_text(text, rule_heap) {
                let node = ParseNode::Rule {
                    rule_name: self.name.clone(),
                    expansion: matched,
                };
                return Ok((rest, node));
            }
        }
        return Err(nom::Err::Error(error_position!(
            text,
            nom::error::ErrorKind::Alt
        )));
    }

    /// Returns all names of referenced rules as a vector of &String
    /// for linking after all the rules are parsed.
    fn get_rule_refs(&self) -> Vec<&String> {
        let refs = self
            .expansion
            .iter()
            .flat_map(|alt| return alt.get_rule_refs())
            .collect_vec();
        return refs;
    }
}

/// Rule Alternative consists of one or more Rule Elements
#[derive(Debug, PartialEq, PartialOrd)]
pub struct Alternative(Vec<Element>);

impl Alternative {
    /// Parse provide text with the alternative.
    /// Parsing starts at the beginning are continues as far as possible.
    pub fn parse_text<'a>(
        &'a self,
        text: &'a str,
        rule_heap: &'a [Rule],
    ) -> IResult<&str, Vec<ParseNode>> {
        let elements = &self.0;
        return parse_list_of_elems_recursive(text, 0, elements, rule_heap);
    }

    /// Returns
    /// for linking after all the rules are parsed.
    fn get_rule_refs(&self) -> Vec<&String> {
        return self
            .0
            .iter()
            .flat_map(|elem| return elem.get_rule_refs())
            .collect_vec();
    }
}

/// Tries to parse provided string with the sequence elements.
/// This algorithm is recursive greedy matching, similar to what regex engines use for .* matching
///
/// * `text` - Text to parse
/// * `idx` - index of the current (top-level) element
/// * `elements` - slice of all elements
fn parse_list_of_elems_recursive<'a>(
    text: &'a str,
    idx: usize,
    elements: &'a [Element],
    rule_heap: &'a [Rule],
) -> IResult<&'a str, Vec<ParseNode>> {
    let Some(current_elem) = &elements.get(idx) else {
        return Err(nom::Err::Error(error_position!(
            text,
            nom::error::ErrorKind::ManyMN
        )));
    };

    let min = current_elem.get_min();
    // try how many times the element matches to get actual max value
    // we can't start from the theoretical max, because in case of <n->,
    // it will be REPEAT_MAX_DEFAULT which is probably huge.
    let mut counter = 0;
    let mut rest = text;
    loop {
        let Ok((r, _matched)) = current_elem.parse_text(rest, rule_heap) else {
            // if we can't parse anymore, we stop the counter
            break;
        };
        if counter == current_elem.get_max() {
            // if we hit the theoretical max value, we can stop
            break;
        }
        counter += 1;
        rest = r;
    }
    // as the actual max number of repeats use the smaller out of counter or theoretical max
    let max = current_elem.get_max().min(counter);
    // we want lazy matching => start with max and decrease if necessary
    // for greedy matching, we would need to add `.rev()` after the interval (min..=max)
    'outer: for reps in (min..=max) {
        // remainder to parse resets at the start each loop, we want to parse from the original text
        let mut rest = text;
        // accumulator for the results
        let mut nodes: Vec<ParseNode> = vec![];
        for i in 0..reps {
            let Ok((r, mut m)) = current_elem.parse_text(rest, rule_heap) else {
                // we couldn't match the element enough times => try next loop with decreased goal
                continue 'outer;
            };
            rest = r; // the new remainder is the leftover for next loop
            nodes.append(&mut m); // add the parse nodes
        }
        // now we used the element `reps` times, so try to shift the element
        if elements.get(idx + 1).is_some() {
            // there is some next element, so continue
            let Ok((r, mut m)) = parse_list_of_elems_recursive(rest, idx + 1, elements, rule_heap)
            else {
                // the shifted sequence didn't succeed, so try to parse one-less and try again
                continue 'outer;
            };
            // the shifted sequence did succeed, so merge the results and return the results
            nodes.append(&mut m);
            return Ok((r, nodes));
        } else {
            // there is no next element, so return the results
            return Ok((rest, nodes));
        }
    }
    // no match was found => return error
    return Err(nom::Err::Error(error_position!(
        text,
        nom::error::ErrorKind::ManyMN
    )));
}

/// Tags are attached to some Elements and they will be returned
/// every time the Element is matched.
#[derive(Debug, PartialEq, PartialOrd)]
pub struct Tag(String);

impl Tag {
    /// constructs a [ParseNode::Tag] from the tag
    pub fn to_node(&self) -> ParseNode {
        return ParseNode::Tag(self.0.to_string());
    }
}

/// rule element is an atomic part of rule definition
#[derive(Debug, PartialEq, PartialOrd)]
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

impl Element {
    /// Tries to parse provided text into vector of parse nodes
    ///
    /// All elements return vector of parse nodes, because Element may be a sequence
    /// or contain multiple tags that are returned as a nodes as well.
    /// Returned nodes are flattened into vector at the Rule level.
    fn parse_text<'a>(
        &'a self,
        text: &'a str,
        rule_heap: &'a [Rule],
    ) -> IResult<&str, Vec<ParseNode>> {
        let text = text.trim_start();
        match self {
            // void never matches anything
            Element::Void { .. } => return fail(text),
            Element::Token { token, tags, .. } => {
                let (rest, matched) = tag(token.as_str())(text)?;
                let token_node = ParseNode::Token(matched.to_string());
                let mut all_nodes = tags.iter().map(|t| return t.to_node()).collect_vec();
                all_nodes.insert(0, token_node);
                return Ok((rest, all_nodes));
            }
            Element::RuleRef { name, tags, .. } => {
                let target_rule_idx = rule_heap
                    .binary_search_by_key(&name, |r| return &r.name)
                    .expect("rule linking should be already checked during text parsing");
                let target_rule = rule_heap
                    .get(target_rule_idx)
                    .expect("target_rule_idx should be a valid index");
                assert_eq!(&target_rule.name, name);
                let (rest, matched) = target_rule.parse_text(text, rule_heap)?;
                let mut nodes = vec![matched];
                nodes.extend(tags.iter().map(|t| return t.to_node()));
                return Ok((rest, nodes));
            }
            Element::Garbage { min, max, tags } => {
                // consume the string up until next whitespace and return the tags
                let (rest, matched) = take_till1(|c: char| return c.is_whitespace())(text)?;
                let nodes = tags.iter().map(|t| return t.to_node()).collect_vec();
                return Ok((rest, nodes));
            }
            Element::Null { min, max, tags } => {
                return Ok((text, tags.iter().map(|t| return t.to_node()).collect_vec()));
            }
            Element::Sequence {
                alternatives,
                min,
                max,
                tags,
            } => {
                for alternative in alternatives {
                    if let Ok((rest, mut nodes)) = alternative.parse_text(text, rule_heap) {
                        nodes.extend(tags.iter().map(|t| return t.to_node()));
                        return Ok((rest, nodes));
                    }
                }
                return Err(nom::Err::Error(error_position!(
                    text,
                    nom::error::ErrorKind::Alt
                )));
            }
        }
    }

    /// Returns the minimum number of repeats that is allowed for the Element.
    /// Because all variants have the field 'min', this function is short-cut to avoid using match each time.
    fn get_min(&self) -> u32 {
        match &self {
            Element::Token { min, .. } => return *min,
            Element::RuleRef { min, .. } => return *min,
            Element::Garbage { min, .. } => return *min,
            Element::Void { min, .. } => return *min,
            Element::Null { min, .. } => return *min,
            Element::Sequence { min, .. } => return *min,
        }
    }

    /// Returns the maximum number of repeats that is allowed for the Element.
    /// Because all variants have the field 'max', this function is short-cut to avoid using match each time.
    fn get_max(&self) -> u32 {
        match &self {
            Element::Token { max, .. } => return *max,
            Element::RuleRef { max, .. } => return *max,
            Element::Garbage { max, .. } => return *max,
            Element::Void { max, .. } => return *max,
            Element::Null { max, .. } => return *max,
            Element::Sequence { max, .. } => return *max,
        }
    }

    /// Returns references to all rule refs that are contained in this element
    /// for linking after all the rules are parsed.
    fn get_rule_refs(&self) -> Vec<&String> {
        match self {
            Element::RuleRef { name, .. } => return vec![name],
            Element::Sequence {
                alternatives,
                min,
                max,
                tags,
            } => {
                return alternatives
                    .iter()
                    .flat_map(|alt| return alt.get_rule_refs())
                    .collect_vec()
            }
            _ => return vec![],
        }
    }
}

/// Tries to parse grammar from provided string.
/// Returns Some(Grammar) if parsing is successful or None,
pub fn parse_grammar(s: &str) -> Result<Grammar, Vec<String>> {
    let input = s.trim();
    // remove lines that are comments
    let input = input
        .lines()
        .filter(|line| return !line.trim().starts_with("//"))
        .join("\n");
    let Ok((rest, _)) = opt(header)(&input) else {
        return Err(vec!["invalid header".to_string()]);
    };
    match rules(rest) {
        Ok(mut rules) => {
            rules.sort_unstable_by_key(|rule| return rule.name.clone());
            let rule_names = rules.iter().map(|r| return &r.name).collect_vec();
            let duplicit_names = rule_names.iter().duplicates().sorted().collect_vec();
            if !duplicit_names.is_empty() {
                return Err(duplicit_names
                    .iter()
                    .map(|s| return format!("illegal re-definition of rule named {:?}", s))
                    .collect_vec());
            }
            let rule_refs = rules
                .iter()
                .flat_map(|r| return r.get_rule_refs())
                .collect_vec();
            let link_problems: Vec<String> = rule_refs
                .par_iter()
                .filter_map(|rule_ref| {
                    if rule_names.binary_search(rule_ref).is_err() {
                        return Some(format!("can't find a rule referenced by ${}", &rule_ref));
                    }
                    return None;
                })
                .collect();
            if link_problems.is_empty() {
                return Ok(Grammar { rules });
            } else {
                return Err(link_problems);
            }
        }
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
        .split_inclusive(|c| return c == ';')
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
        tag(";"),
    ))(input)
    {
        Ok((rest, matched)) => {
            if !rest.trim().is_empty() {
                return Err(format!("rule parsing left non-empty remainder: {rest}"));
            }
            let (maybe_public, _, name, _, _, _, body, _) = matched;
            let rule = Rule {
                public: maybe_public.is_some(),
                name: name.to_string(),
                expansion: body,
            };
            return Ok(rule);
        }
        Err(e) => {
            return Err(format!("failed to parse rule from: {}, err: {}", s, e));
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
    let (rest, (rule_name, tags)) = tuple((tag("$NULL"), many0(grammar_tag)))(input)?;
    return Ok((
        rest,
        Element::Null {
            min: 1,
            max: 1,
            tags,
        },
    ));
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
    if ["$NULL", "$VOID", "$GARBAGE"].contains(&name.as_str()) {
        // we don't want special rules to be parsed as regular rule refs
        return Err(nom::Err::Error(error_position!(
            input,
            nom::error::ErrorKind::Tag
        )));
    };
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
            alternatives,
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
    let (rest, (mut sequence, maybe_repeat, mut seq_tags)) = tuple((
        delimited(
            tuple((tag("("), whitespace)),
            bare_sequence,
            tuple((whitespace, tag(")"))),
        ),
        opt(repeat),
        many0(grammar_tag),
    ))(input)?;
    let (seq_min, seq_max) = maybe_repeat.unwrap_or((1, 1));
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
                        alternatives: vec![Alternative(vec![Element::Garbage { min, max, tags }])],
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
mod tests {
    use itertools::Itertools;

    use crate::parser::{
        repeat, rule_alternative, rule_body, rule_name, tests::grammar::new_token, Alternative,
        Element, Grammar, Rule, REPEAT_MAX_DEFAULT,
    };

    use super::{grammar_tag, parse_grammar, rule, sequence, token, ParseNode, Tag};

    mod grammar {
        use crate::parser::tests::*;
        use crate::parser::*;

        pub(super) fn new_token(s: &str, min: u32, max: u32, tags: &[&str]) -> Element {
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

        #[test]
        fn rule_simple() {
            let s = "$rule = foo bar | baz;";
            let rule = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: String::from("rule"),
                expansion: vec![
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
            let s = "public $rule = foo ;";
            let rule = rule(s).unwrap();
            let expected = Rule {
                public: true,
                name: String::from("rule"),
                expansion: vec![Alternative(vec![new_token("foo", 1, 1, &[])])],
            };
            assert_eq!(expected, rule);
        }

        #[test]
        fn rule_with_opts() {
            let s = "$rule = [foo bar] ;";
            let rule = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: String::from("rule"),
                expansion: vec![Alternative(vec![Element::Sequence {
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
            let s = "$rule = small <3-10> | medium <5-> | large;";
            let expected = Rule {
                public: false,
                name: String::from("rule"),
                expansion: vec![
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
            let s = "$rule = foo {tag};";
            let r = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: String::from("rule"),
                expansion: vec![Alternative(vec![new_token("foo", 1, 1, &["tag"])])],
            };
            assert_eq!(expected, r);
        }

        #[test]
        fn rule_with_tags() {
            let s = "$rule = foo {t} {t2};";
            let r = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: String::from("rule"),
                expansion: vec![Alternative(vec![new_token("foo", 1, 1, &["t", "t2"])])],
            };
            assert_eq!(expected, r);
        }

        #[test]
        fn rule_with_refs() {
            let s = "$rule = $another;";
            let r = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: String::from("rule"),
                expansion: vec![Alternative(vec![Element::RuleRef {
                    name: String::from("another"),
                    min: 1,
                    max: 1,
                    tags: vec![],
                }])],
            };
            assert_eq!(expected, r);
        }

        #[test]
        fn sequence_with_tags() {
            let s = "$rule = (foo {inner tag} bar) {tag1} {tag2};";
            let parsed = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: "rule".to_string(),
                expansion: vec![Alternative(vec![Element::Sequence {
                    alternatives: vec![Alternative(vec![
                        new_token("foo", 1, 1, &["inner tag"]),
                        new_token("bar", 1, 1, &[]),
                    ])],
                    min: 1,
                    max: 1,
                    tags: vec![Tag("tag1".to_string()), Tag("tag2".to_string())],
                }])],
            };
            assert_eq!(parsed, expected);
        }

        #[test]
        fn rule_with_tag_simple() {
            let s = "$rule = foo {tag};";
            let rule = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: "rule".to_string(),
                expansion: vec![Alternative(vec![new_token("foo", 1, 1, &["tag"])])],
            };
            assert_eq!(expected, rule);
        }

        #[test]
        fn rule_with_tag_complex() {
            let s = "$rule = (foo {tag} {tag2});";
            let rule = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: "rule".to_string(),
                expansion: vec![Alternative(vec![new_token("foo", 1, 1, &["tag", "tag2"])])],
            };
            assert_eq!(expected, rule);
        }

        #[test]
        fn rule_with_sequence_with_repeat() {
            let s = "$rule = (foo bar)<3-4>;";
            let r = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: "rule".to_string(),
                expansion: vec![Alternative(vec![Element::Sequence {
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
            let s = "$rule = (foo);";
            let r = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: "rule".to_string(),
                expansion: vec![Alternative(vec![new_token("foo", 1, 1, &[])])],
            };
            assert_eq!(expected, r);
        }

        #[test]
        fn optimize_optional_sequence() {
            let s = "$rule = [foo];";
            let r = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: "rule".to_string(),
                expansion: vec![Alternative(vec![new_token("foo", 0, 1, &[])])],
            };
            assert_eq!(expected, r);
        }

        #[test]
        fn optimize_sequence_with_outer_repeat() {
            let s = "$rule = (foo)<3-4>;";
            let r = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: "rule".to_string(),
                expansion: vec![Alternative(vec![new_token("foo", 3, 4, &[])])],
            };
            assert_eq!(expected, r);
        }

        #[test]
        fn grammar_optional_header() {
            let s1 = "#ABNF 1.0 UTF-8; public $root = foo;";
            let s2 = "public $root = foo;";
            let g1 = parse_grammar(s1).unwrap();
            let g2 = parse_grammar(s2).unwrap();
            let expected = Grammar {
                rules: vec![Rule {
                    public: true,
                    name: "root".to_string(),
                    expansion: vec![Alternative(vec![new_token("foo", 1, 1, &[])])],
                }],
            };
            assert_eq!(expected, g1);
            assert_eq!(expected, g2);
        }

        #[test]
        fn optimize_sequence_with_inner_repeat() {
            let s = "$rule = [foo<3-4>];";
            let r = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: "rule".to_string(),
                expansion: vec![Alternative(vec![Element::Sequence {
                    alternatives: vec![Alternative(vec![new_token("foo", 3, 4, &[])])],
                    min: 0,
                    max: 1,
                    tags: vec![],
                }])],
            };
            assert_eq!(expected, r);
        }

        #[test]
        fn rule_starting_with_tag() {
            // it is illegal for rule to start with tag, because tags are
            // linked to previous tokens, rule references or sequences.
            // It can however be done by using $NULL as a first item followed by the tag.
            let s = "$rule = {tag} foo {tag 2};";
            let r = rule(s);
            assert!(r.is_err());
        }

        #[test]
        fn remove_comments() {
            let s = "// comment 
                    public $root = foo;
                    // another comment";
            let gr = parse_grammar(s).unwrap();
            let expected = Grammar {
                rules: vec![Rule {
                    public: true,
                    name: "root".to_string(),
                    expansion: vec![Alternative(vec![new_token("foo", 1, 1, &[])])],
                }],
            };
            assert_eq!(gr, expected);
        }

        #[test]
        fn duplicit_rule_names() {
            let gr = parse_grammar("public $root = foo $other; $other = bar; $other = baz;")
                .unwrap_err();
            let expected_err = vec!["illegal re-definition of rule named \"other\"".to_string()];
            assert_eq!(gr, expected_err);
        }
    }

    mod text {
        use crate::parser::tests::*;
        use crate::parser::*;

        #[test]
        fn single_token() {
            let gr = parse_grammar("public $root = foo;").unwrap();
            let text = "foo";
            let parsed = gr.parse_text_from_start(text);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Token("foo".to_string())],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn token_with_tags() {
            let gr = parse_grammar("public $root = foo {tag1} {tag2};").unwrap();
            let text = "foo";
            let parsed = gr.parse_text_from_start(text);
            let expected = vec![ParseResult {
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
            let gr = parse_grammar("public $root = t1 $NULL {tag1} t2 {tag2} t3 {tag3};").unwrap();
            let text = "t1 t2 t3";
            let parsed = gr.parse_text_from_start(text);
            let expected = vec![ParseResult {
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
                        ParseNode::Tag("tag1".to_string()),
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
            let gr = parse_grammar("public $root = ((t1) $NULL) {tag1} (t2 {tag2} t3);").unwrap();
            let text = "t1 t2 t3";
            let parsed = gr.parse_text_from_start(text);
            let expected = vec![ParseResult {
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Token("t1".to_string()),
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
            let gr = parse_grammar("public $root = t1 | t2 | t3 ;").unwrap();
            let text = "t2";
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Token("t2".to_string())],
                },
            }];
            let parsed = gr.parse_text_from_start(text);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn alternative_with_null() {
            let gr = parse_grammar("public $root = t1 | t2 | $NULL;").unwrap();
            let text = "";
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![],
                },
            }];
            let parsed = gr.parse_text_from_start(text);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn alternative_ambiguity() {
            let gr = parse_grammar("public $root = t1 {tag1} | t1 {tag2} | t2;").unwrap();
            let text = "t1";
            let expected = vec![ParseResult {
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
            let parsed = gr.parse_text_from_start(text);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_garbage() {
            let gr = parse_grammar("public $root = $GARBAGE;").unwrap();
            let text = "foo";
            let parsed = gr.parse_text_from_start(text);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn repeat_simple() {
            let gr = parse_grammar("public $root = t1 <0-2>;").unwrap();
            let text = "";
            let parsed = gr.parse_text_from_start(text);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![],
                },
            }];
            assert_eq!(parsed, expected);

            let text2 = "t1";
            let parsed2 = gr.parse_text_from_start(text2);
            let expected2 = vec![ParseResult {
                rule: "root".to_string(),
                text: text2.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Token("t1".to_string())],
                },
            }];
            assert_eq!(parsed2, expected2);

            let text3 = "t1 t1";
            let parsed3 = gr.parse_text_from_start(text3);
            let expected3 = vec![ParseResult {
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
            let gr = parse_grammar("public $root = (t1 {tag}) <0-3>;").unwrap();
            let parsed = gr.parse_text_from_start("");
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: "".to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![],
                },
            }];
            assert_eq!(parsed, expected);

            let parsed = gr.parse_text_from_start("t1");
            let expected = vec![ParseResult {
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

            let parsed = gr.parse_text_from_start("t1 t1 t1");
            let expected = vec![ParseResult {
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
            let gr = parse_grammar("public $root = (t1 {tag}) <0->;").unwrap();
            for n in [0, 1, 2, 10, 100, 1000, 10_000] {
                let text = "t1".repeat(n);
                let parsed = gr.parse_text_from_start(&text);
                let mut expansion = vec![];
                for _ in 0..n {
                    expansion.push(ParseNode::Token("t1".to_string()));
                    expansion.push(ParseNode::Tag("tag".to_string()));
                }
                let expected = vec![ParseResult {
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
            let gr = parse_grammar("public $root = $GARBAGE {tag1} {tag2};").unwrap();
            let text = "foo";
            let parsed = gr.parse_text_from_start(text);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Tag("tag1".to_string()),
                        ParseNode::Tag("tag2".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_garbage_with_tags_and_repeat() {
            let gr = parse_grammar("public $root = $GARBAGE <3> {tag};").unwrap();
            let text = "foo bar baz";
            let parsed = gr.parse_text_from_start(text);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![
                        ParseNode::Tag("tag".to_string()),
                        ParseNode::Tag("tag".to_string()),
                        ParseNode::Tag("tag".to_string()),
                    ],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_null() {
            let gr = parse_grammar("public $root = $NULL {tag};").unwrap();
            let text = "";
            let parsed = gr.parse_text_from_start(text);
            let expected = vec![ParseResult {
                rule: "root".to_string(),
                text: text.to_string(),
                node: ParseNode::Rule {
                    rule_name: "root".to_string(),
                    expansion: vec![ParseNode::Tag("tag".to_string())],
                },
            }];
            assert_eq!(expected, parsed);
        }

        #[test]
        fn special_null_repeat() {
            let gr = parse_grammar("public $root = $NULL <1-3> {tag};");
            assert!(gr.is_err());
        }

        #[test]
        fn grammar_simple() {
            let s = "#ABNF 1.0 UTF-8; public $foo = bar baz {tag};";
            let g = parse_grammar(s).unwrap();
            let expected = Grammar {
                rules: vec![Rule {
                    public: true,
                    name: "foo".to_string(),
                    expansion: vec![Alternative(vec![
                        new_token("bar", 1, 1, &[]),
                        new_token("baz", 1, 1, &["tag"]),
                    ])],
                }],
            };
            assert_eq!(g, expected);
        }

        #[test]
        fn text_parse_grammar_simple() {
            let s = "#ABNF 1.0 UTF-8; public $root = token {tag};";
            let g = parse_grammar(s).unwrap();
            let text = "token";
            let p = g.parse_text_from_start(text);
            let expected = vec![ParseResult {
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
            let s = "#ABNF 1.0 UTF-8; public $root = $other; $other = token {tag} {second tag};";
            let g = parse_grammar(s).unwrap();
            let text = "token";
            let p = g.parse_text_from_start(text);
            let expected = vec![ParseResult {
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
            let gr = parse_grammar("public $rule = (t1 {tag1}) <0-2> (t1 {tag2}) <0-2>;").unwrap();
            let text = "t1 t1 t1";
            let expected = vec![ParseResult {
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
            let parsed = gr.parse_text_from_start(text);
            assert_eq!(expected, parsed);
        }

        #[test]
        fn simple_rule_ref() {
            let gr = parse_grammar("public $root = foo $other; $other = bar;").unwrap();
            let parsed = gr.parse_text_from_start("foo bar");
            let expected = vec![ParseResult {
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
            let src = "public $root = foo {tag foo} $other {tag ref}; $other = bar {tag bar};";
            let gr = parse_grammar(src).unwrap();
            let parsed = gr.parse_text_from_start("foo bar");
            let expected = vec![ParseResult {
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
            let gr = parse_grammar("public $root = t1 [t2 {tag}] t2;").unwrap();
            let parsed = gr.parse_text_from_start("t1 t2 t2");
            let expected = vec![ParseResult {
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

            let parsed2 = gr.parse_text_from_start("t1 t2");
            let expected = vec![ParseResult {
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
            let gr = parse_grammar("public $rule = t1 [$other] t2; $other = t2;").unwrap();
            let parsed = gr.parse_text_from_start("t1 t2 t2");
            let expected = vec![ParseResult {
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

            let parsed2 = gr.parse_text_from_start("t1 t2");
            let expected = vec![ParseResult {
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
            let gr = parse_grammar("public $root = $other $other; $other = foo;").unwrap();
            let parsed = gr.parse_text_from_start("foo foo");
            let expected = vec![ParseResult {
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
            let gr =
                parse_grammar("public $root = $other <0-> {baz}; $other = foo {bar};").unwrap();
            for n in [0, 1, 2, 5, 10, 100] {
                let text = "foo".repeat(n);
                let parsed = gr.parse_text_from_start(&text);
                let mut expansion = vec![];
                for i in 0..n {
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
            // the first applicable rule alternative will be applied,
            // so recursive definition must be before the terminals,
            // otherwise the recursion will not be actually applied
            let s = "public $root = $other; $other = t1 $other | t1 {last} ;";
            let grammar = parse_grammar(s).unwrap();
            let parsed = grammar.parse_text_from_start("t1 t1 t1");
            let expected = vec![ParseResult {
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
            let gr = parse_grammar("public $root = $other; $other = (t1 $other t2) | $NULL {end};")
                .unwrap();
            let text = "t1 t1 t2 t2";
            let parsed = gr.parse_text_from_start(text);
            let expected = vec![ParseResult {
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
                                        expansion: vec![ParseNode::Tag("end".to_string())],
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
    }
}
