use std::borrow::Cow;
use std::fmt::Display;
use std::path::PathBuf;

use clap::ValueEnum;
use itertools::Itertools;
use log::*;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while, take_while1};
use nom::combinator::opt;
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::tuple;
use nom::sequence::{delimited, preceded};
use nom::{error_position, IResult};
use ptree::print_config::StyleWhen;
use ptree::{write_tree_with, PrintConfig, Style, TreeItem};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use regex::Regex;

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
    /// what was the style used to obtain this result
    pub style: ParsingStyle,
}

impl Display for ParseResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let style = &self.style;
        let tree = &self.node.construct_spt(true);
        let tags = self.node.tags_dfpo().join(", ");
        let raw = &self.text;
        return write!(
            f,
            "Result ({}) {{\n-  raw: {:?}\n- tags: [{}]\n- tree:\n{}}}",
            style, raw, tags, tree
        );
    }
}

/// Describes the behavior of parsing when multiple matches are possible
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum ParsingStyle {
    /// match as few tokens as possible
    Lazy,
    /// match as many tokens as possible
    Greedy,
    /// if there is multiple ways to match, return all of them
    Thorough,
}

impl Display for ParsingStyle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingStyle::Lazy => return write!(f, "Lazy"),
            ParsingStyle::Greedy => return write!(f, "Greedy"),
            ParsingStyle::Thorough => return write!(f, "Thorough"),
        }
    }
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
            ParseNode::Rule { expansion, .. } => {
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
            styled: if styled {
                StyleWhen::Always
            } else {
                StyleWhen::Never
            },
            ..Default::default()
        };
        let res = write_tree_with(self, &mut buf, &conf);
        if res.is_err() {
            error!("received empty Error when writing spt tree");
        }
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
            ParseNode::Token(s) => return write!(f, "\"{}\"", style.paint(s)),
            ParseNode::Tag(s) => return write!(f, "{{{}}}", style.paint(s)),
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

    /// returns a reference to the vector of rules in the grammar
    pub fn get_rules(&self) -> &Vec<Rule> {
        return &self.rules;
    }

    /// return a list of references to public rules in the grammars
    pub fn get_public_rules(&self) -> Vec<&Rule> {
        return self.rules.iter().filter(|r| return r.public).collect_vec();
    }

    /// Searches for all possible parse trees within provided text.
    ///
    /// This implementation tries to parse the string from the beginning as far as possible.
    /// After the parsing fails (or reaches the end), the starting point is shifted one word over
    /// (to the next whitespace) and tries to parse again.
    /// All the successful parsing results are collected and returned.
    pub fn find_all(&self, text: &str, style: &ParsingStyle) -> Vec<ParseResult> {
        let mut results = self.semantic_parse_text_from_start(text, style);
        text.match_indices(|c: char| return c.is_whitespace())
            .for_each(|(idx, _)| {
                let (_lead, to_parse) = text.split_at(idx);
                results.append(&mut self.semantic_parse_text_from_start(to_parse, style));
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
    /// returns reference to the vector of alternatives
    pub fn get_alternatives(&self) -> &Vec<Alternative> {
        return &self.expansion;
    }
    /// returns a cloned name of itself
    pub fn get_name(&self) -> String {
        return self.name.clone();
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
    /// Returns rule references for linking after all the rules are parsed.
    fn get_rule_refs(&self) -> Vec<&String> {
        return self
            .0
            .iter()
            .flat_map(|elem| return elem.get_rule_refs())
            .collect_vec();
    }

    /// return a reference to the inner list of elements
    pub fn get_elements(&self) -> &Vec<Element> {
        return &self.0;
    }
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
        /// overrides the default parsing style
        style: Option<ParsingStyle>,
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
        /// overrides the default parsing style
        style: Option<ParsingStyle>,
        /// tags that will be returned every time the referenced Rule is matched
        tags: Vec<Tag>,
    },
    /// Special rule $GARBAGE that matches any single token
    Garbage {
        /// minimum number of repeats
        min: u32,
        /// maximum number of repeats
        max: u32,
        /// overrides the default parsing style
        style: Option<ParsingStyle>,
        /// tags that will be returned every time the GARBAGE is matched
        tags: Vec<Tag>,
    },
    /// Special rule $VOID that never matches anything
    Void,
    /// Special rule $END that matches the ending of sentence
    End {
        /// tags associated with the end of sentence
        tags: Vec<Tag>,
    },
    /// Special rule $NULL that always matches (matches zero-length string)
    Null {
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
        /// overrides the default parsing style
        style: Option<ParsingStyle>,
        /// tags that will be returned every time the sequence is matched
        tags: Vec<Tag>,
    },
}

impl Element {
    /// Returns the matching style of repeats for the Element.
    /// Because all variants have the field 'style', this function is short-cut to avoid using match each time.
    /// [Element::Void] and [Element::Null] always returns 1.
    pub fn get_style(&self) -> Option<ParsingStyle> {
        match &self {
            Element::Token { style, .. } => return *style,
            Element::RuleRef { style, .. } => return *style,
            Element::Garbage { style, .. } => return *style,
            Element::Void => return None,
            Element::Null { .. } => return None,
            Element::End { .. } => return None,
            Element::Sequence { style, .. } => return *style,
        }
    }

    /// Returns the minimum number of repeats that is allowed for the Element.
    /// Because all variants have the field 'min', this function is short-cut to avoid using match each time.
    /// [Element::Void] and [Element::Null] always returns 1.
    pub fn get_min(&self) -> u32 {
        match &self {
            Element::Token { min, .. } => return *min,
            Element::RuleRef { min, .. } => return *min,
            Element::Garbage { min, .. } => return *min,
            Element::Void => return 1,
            Element::Null { .. } => return 1,
            Element::End { .. } => return 1,
            Element::Sequence { min, .. } => return *min,
        }
    }

    /// Returns the maximum number of repeats that is allowed for the Element.
    /// Because all variants have the field 'max', this function is short-cut to avoid using match each time.
    /// [Element::Void] and [Element::Null] always returns 1.
    pub fn get_max(&self) -> u32 {
        match &self {
            Element::Token { max, .. } => return *max,
            Element::RuleRef { max, .. } => return *max,
            Element::Garbage { max, .. } => return *max,
            Element::Void => return 1,
            Element::Null { .. } => return 1,
            Element::End { .. } => return 1,
            Element::Sequence { max, .. } => return *max,
        }
    }

    /// Returns references to all rule refs that are contained in this element
    /// for linking after all the rules are parsed.
    fn get_rule_refs(&self) -> Vec<&String> {
        match self {
            Element::RuleRef { name, .. } => return vec![name],
            Element::Sequence { alternatives, .. } => {
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
    let input = remove_comments(input);
    let input = input.trim();
    let Ok((rest, _)) = opt(header)(input) else {
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
        whitespace,
        tag(";"),
    ))(input)
    {
        Ok((rest, matched)) => {
            if !rest.trim().is_empty() {
                return Err(format!("rule parsing left non-empty remainder: {rest}"));
            }
            let (maybe_public, _, name, _, _, _, body, _, _) = matched;
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
    let (rest, name) = preceded(
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
        special_end,
        rule_ref,
        sequence,
    ))(input);
    return res;
}

/// Tries to parse input str and return [Element::Void]
fn special_void(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    let (rest, _rule_name) = tag("$VOID")(input)?;
    return Ok((rest, Element::Void));
}

/// Tries to parse input str and return [Element::Null]
fn special_null(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    let (rest, (_rule_name, tags)) = tuple((tag("$NULL"), many0(grammar_tag)))(input)?;
    return Ok((rest, Element::Null { tags }));
}

/// Tries to parse input str and return [Element::End]
fn special_end(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    let (rest, (_rule_name, tags)) = tuple((tag("$END"), many0(grammar_tag)))(input)?;
    return Ok((rest, Element::End { tags }));
}

/// Tries to parse input str and return [Element::Garbage]
fn special_garbage(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    let (rest, (_rule_name, _, maybe_repeat, _, tags)) = tuple((
        tag("$GARBAGE"),
        whitespace,
        opt(repeat),
        whitespace,
        many0(grammar_tag),
    ))(input)?;
    if let Some((min, max, style)) = maybe_repeat {
        return Ok((
            rest,
            Element::Garbage {
                min,
                max,
                style,
                tags,
            },
        ));
    } else {
        return Ok((
            rest,
            Element::Garbage {
                min: 1,
                max: 1,
                style: None,
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
    if let Some((min, max, style)) = maybe_repeat {
        return Ok((
            rest,
            Element::RuleRef {
                name,
                min,
                max,
                style,
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
                style: None,
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
    let (rest, (sequence, _, mut seq_tags)) = tuple((
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
            max,
            style,
            ..
        } => {
            return Ok((
                rest,
                Element::Sequence {
                    alternatives,
                    min: 0,
                    max,
                    style,
                    tags: seq_tags,
                },
            ));
        }
        Element::Token {
            token,
            style,
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
                        style,
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
                            style,
                            tags,
                        }])],
                        min: 0,
                        max: 1,
                        style: None,
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
            style,
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
                        style,
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
                            style,
                            tags,
                        }])],
                        min,
                        max,
                        style: None,
                        tags: seq_tags,
                    },
                ));
            }
        }
        Element::Garbage {
            min,
            max,
            tags,
            style,
        } => {
            if min == 1 && max == 1 {
                let min = 0;
                let mut merged_tags = tags;
                merged_tags.append(&mut seq_tags);
                return Ok((
                    rest,
                    Element::Garbage {
                        min,
                        max,
                        style,
                        tags: merged_tags,
                    },
                ));
            } else {
                let min = 0;
                return Ok((
                    rest,
                    Element::Sequence {
                        alternatives: vec![Alternative(vec![Element::Garbage {
                            min,
                            max,
                            tags,
                            style,
                        }])],
                        min,
                        max,
                        style: None,
                        tags: seq_tags,
                    },
                ));
            }
        }
        Element::Void => {
            return Ok((rest, Element::Void));
        }
        Element::Null { tags } => {
            let mut merged_tags = tags;
            merged_tags.append(&mut seq_tags);
            return Ok((rest, Element::Null { tags: merged_tags }));
        }
        Element::End { tags } => {
            let mut merged_tags = tags;
            merged_tags.append(&mut seq_tags);
            return Ok((rest, Element::End { tags: merged_tags }));
        }
    }
}

/// Tries to parse provided string into [Element::Sequence]
/// where the sequence is surrounded in braces ().
/// In this way, the whole sequence may have some additional repeat operator attached to it.
fn braced_sequnece(s: &str) -> IResult<&str, Element> {
    let input = s.trim_start();
    let (rest, (sequence, maybe_repeat, mut seq_tags)) = tuple((
        delimited(
            tuple((tag("("), whitespace)),
            bare_sequence,
            tuple((whitespace, tag(")"))),
        ),
        opt(repeat),
        many0(grammar_tag),
    ))(input)?;
    let (seq_min, seq_max, seq_style) = maybe_repeat.unwrap_or((1, 1, None));
    match sequence {
        Element::Sequence { alternatives, .. } => {
            return Ok((
                rest,
                Element::Sequence {
                    alternatives,
                    min: seq_min,
                    max: seq_max,
                    style: seq_style,
                    tags: seq_tags,
                },
            ));
        }
        Element::Token {
            token,
            min,
            style,
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
                        style: seq_style,
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
                            style,
                            min,
                            max,
                            tags,
                        }])],
                        min: seq_min,
                        max: seq_max,
                        style: seq_style,
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
            style,
        } => {
            if min == 1 && max == 1 {
                let mut merged_tags = tags;
                merged_tags.append(&mut seq_tags);
                return Ok((
                    rest,
                    Element::RuleRef {
                        name,
                        min: seq_min,
                        style: seq_style,
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
                            style,
                            min,
                            max,
                            tags,
                        }])],
                        min: seq_min,
                        max: seq_max,
                        style: seq_style,
                        tags: seq_tags,
                    },
                ));
            }
        }
        Element::Garbage {
            min,
            max,
            tags,
            style,
        } => {
            if min == 1 && max == 1 {
                let mut merged_tags = tags;
                merged_tags.append(&mut seq_tags);
                return Ok((
                    rest,
                    Element::Garbage {
                        min: seq_min,
                        style: seq_style,
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
                            style,
                        }])],
                        min: seq_min,
                        max: seq_max,
                        style: seq_style,
                        tags: seq_tags,
                    },
                ));
            }
        }
        Element::Void => {
            return Ok((rest, Element::Void));
        }
        Element::Null { tags } => {
            let mut merged_tags = tags;
            merged_tags.append(&mut seq_tags);
            return Ok((rest, Element::Null { tags: merged_tags }));
        }
        Element::End { tags } => {
            let mut merged_tags = tags;
            merged_tags.append(&mut seq_tags);
            return Ok((rest, Element::End { tags: merged_tags }));
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
    // sequence "seq" that isn't surrounded by [] or () must be matched once
    let sequence = Element::Sequence {
        alternatives,
        min: 1,
        max: 1,
        style: None,
        tags: vec![], // bare sequence doesn't have any tags, they are specified after () or []
    };
    return Ok((rest, sequence));
}

/// Tries to parse input str and return [Element::Token]
fn token(input: &str) -> IResult<&str, Element> {
    let input = input.trim_start();
    let (rest, (token, _, maybe_repeat, _, tags)) = tuple((
        take_while1(|c| return nom_unicode::is_alphanumeric(c) || ".-_:".contains(c)),
        whitespace,
        opt(repeat),
        whitespace,
        many0(grammar_tag),
    ))(input)?;
    let repeat_operator = maybe_repeat.unwrap_or((1, 1, None));
    return Ok((
        rest,
        Element::Token {
            token: token.to_string(),
            min: repeat_operator.0,
            max: repeat_operator.1,
            style: repeat_operator.2,
            tags,
        },
    ));
}

/// Tries to parse input str and returns a repeat specifier
/// allowed syntax:
/// <m-n> = element must be repeated at least m times and at most n times (both inclusive)
/// <m-> = element must be repeated at least m times (inclusive) and at most [REPEAT_MAX_DEFAULT]
/// <m> = element must be repeated exactly m times
/// no whitespace allowed inside the angle brackets
/// no repeat specification is equal to <1>
fn repeat(input: &str) -> IResult<&str, (u32, u32, Option<ParsingStyle>)> {
    let input = input.trim_start();
    let (rest, (maybe_style, min, maybe_second_part)) = delimited(
        tag("<"),
        tuple((
            opt(tuple((alt((tag("L"), tag("G"))), tag(":")))),
            nom::character::complete::u32,
            opt(tuple((tag("-"), opt(nom::character::complete::u32)))),
        )),
        tag(">"),
    )(input)?;
    // if there is <m>, then fill it to be <m-n>, where n = m
    // if there is <m->, then fill it to be <m-n>, where n = [REPEAT_MAX_DEFAULT]
    // and unwrap the n value into 'max'
    let style = match maybe_style {
        None => None,
        Some((style_str, _)) => match style_str {
            "L" => Some(ParsingStyle::Lazy),
            "G" => Some(ParsingStyle::Greedy),
            _ => unreachable!(),
        },
    };
    let max = maybe_second_part
        .unwrap_or(("", Some(min)))
        .1
        .unwrap_or(REPEAT_MAX_DEFAULT);
    return Ok((rest, (min, max, style)));
}

/// Consumes all leading whitespace including line breaks
fn whitespace(input: &str) -> IResult<&str, ()> {
    let (remaining, _matched) = take_while(|c: char| -> bool {
        return c.is_whitespace();
    })(input)?;
    return Ok((remaining, ()));
}

/// Removes all comments from input text.
///
/// The comments can be C/Java style, which means:
/// 1. single-line comments starting by `//` and continuing for the rest of the line (because of URLs, `://` is skipped)
/// 2. multi-line comments surrounded by `/*` and `*/`
/// 3. documentation (multi-line) comments surrounded by `/**` and `*/`
pub fn remove_comments(input: &str) -> String {
    // black magic that should find "//" but ignore "://" because of URLs
    let single_line_comments_regex = Regex::new(r"(?m:(^|[^:])//.*$)").unwrap();

    // documentation comments are included in the multi-line regex
    let multi_line_comments_regex = Regex::new(r"(?ms:/\*.*?\*/)").unwrap();

    // multi-line comments need to be removed first, because they can contain single-line comments
    // removing single-line comments first might cause issues
    let input = multi_line_comments_regex.replace_all(input, "");
    // put back the single consumed letter before // (due to URL checking)
    let input = single_line_comments_regex.replace_all(&input, "$1");

    return input.to_string();
}

/// unit tests for the 'parser.rs'
#[cfg(test)]
mod tests {
    #[allow(deprecated)]
    use itertools::Itertools;

    use crate::parser::{repeat, rule_alternative, rule_name, REPEAT_MAX_DEFAULT};

    use super::{grammar_tag, parse_grammar, rule, token};

    mod grammar {
        #[allow(deprecated)]
        use crate::parser::tests::*;
        use crate::parser::*;

        pub(super) fn new_token(s: &str, min: u32, max: u32, tags: &[&str]) -> Element {
            return Element::Token {
                token: s.to_string(),
                min,
                max,
                style: None,
                tags: tags.iter().map(|t| return Tag(t.to_string())).collect_vec(),
            };
        }

        #[test]
        /// test the repeat parsing
        fn parse_repeat() {
            let (rest, rep) = repeat("<1-2>").unwrap();
            assert_eq!((1, 2, None), rep);
            assert!(rest.is_empty());

            let (rest, rep) = repeat("<1->").unwrap();
            assert_eq!((1, REPEAT_MAX_DEFAULT, None), rep);
            assert!(rest.is_empty());

            let (rest, rep) = repeat("<3>").unwrap();
            assert_eq!((3, 3, None), rep);
            assert!(rest.is_empty());
        }

        #[test]
        fn parse_repeat_with_style() {
            let (rest, rep) = repeat("<1-2>").unwrap();
            assert_eq!((1, 2, None), rep);
            assert!(rest.is_empty());

            let (rest, rep) = repeat("<L:1->").unwrap();
            assert_eq!((1, REPEAT_MAX_DEFAULT, Some(ParsingStyle::Lazy)), rep);
            assert!(rest.is_empty());

            let (rest, rep) = repeat("<G:3>").unwrap();
            assert_eq!((3, 3, Some(ParsingStyle::Greedy)), rep);
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
                    style: None,
                    tags: vec![],
                }])],
            };
            assert_eq!(expected, rule);
        }

        #[test]
        fn rule_with_repeats() {
            let s = "$rule = small <3-10> | medium <5-> | large;";
            let r = rule(s).unwrap();
            let expected = Rule {
                public: false,
                name: String::from("rule"),
                expansion: vec![
                    Alternative(vec![new_token("small", 3, 10, &[])]),
                    Alternative(vec![new_token("medium", 5, REPEAT_MAX_DEFAULT, &[])]),
                    Alternative(vec![new_token("large", 1, 1, &[])]),
                ],
            };
            assert_eq!(r, expected);
        }

        #[test]
        fn tag() {
            let s = "{tag}";
            let (_rest, t) = grammar_tag(s).unwrap();
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
                    style: None,
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
                    style: None,
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
                    style: None,
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
                    style: None,
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
}
