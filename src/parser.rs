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
use nom::IResult;

/// if repeat is <m->, then this value will be used as the max
const REPEAT_MAX_DEFAULT: u32 = std::u32::MAX;

#[derive(Debug)]
/// grammar consists of one or more rules
pub struct Grammar {
    /// list of the rules that are defined in this grammar
    rules: Vec<Rule>,
}

/// Rule consists of one or more rule alternatives
#[derive(Debug, PartialEq)]
pub enum Rule {
    /// local rule defined in this grammar
    Normal {
        /// public rules return it's matches from the grammar, private rules are 'hidden'
        public: bool,
        /// name of the rule, must be unique in it's grammar
        name: String,
        /// list of the alternatives, in order
        alternatives: Vec<Alternative>,
    },
    /// special rule $VOID that never matches anything
    Void,
    /// special rule $GARBAGE that always matches any single word
    Garbage,
    /// special rule $NULL that matches empty string
    Null,
}

/// Rule Alternative consists of one or more Rule Elements
#[derive(Debug, PartialEq)]
pub struct Alternative(Vec<Element>);

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
    },
    /// tag that is returned when previous non-tag element is matched
    Tag {
        /// contents of the tag, can't contain curly brackets and semicolons
        tag: String,
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
    },
}

pub fn parse_grammar(s: &str) -> Option<Grammar> {
    let input = s.trim();
    let Ok((rest, _)) = header(input) else {
        println!("Parsing error: Missing header");
        return None;
    };
    match rules(rest) {
        Ok(rules) => return Some(Grammar { rules }),
        Err(errs) => {
            for e in errs {
                println!("Parsing error: {e}");
            }
            return None;
        }
    }
}

fn header(s: &str) -> IResult<&str, ()> {
    let input = s.trim();
    let (rest, _) = tag("#ABNF 1.0 UTF-8;")(input)?;
    return Ok((rest, ()));
}

/// tries to parse provided string into list of rules
fn rules(s: &str) -> Result<Vec<Rule>, Vec<String>> {
    let (rules, errs): (Vec<_>, Vec<_>) = s
        .split_terminator(';')
        .map(|s| return s.trim())
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
    let input = s.trim();
    match input {
        "$VOID" => return Ok(Rule::Void),
        "$NULL" => return Ok(Rule::Null),
        "$GARBAGE" => return Ok(Rule::Garbage),
        s => {
            match tuple((
                opt(tag("public")),
                whitespace,
                rule_name,
                whitespace,
                tag("="),
                whitespace,
                rule_body,
            ))(s)
            {
                Ok((rest, matched)) => {
                    let (maybe_public, _, name, _, _, _, body) = matched;
                    let rule = Rule::Normal {
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
    }
}

/// Tries to parse provided string into the body of a Rule
/// Rule body is one or more rule alternatives separated by pipeline
fn rule_body(s: &str) -> IResult<&str, Vec<Alternative>> {
    let input = s.trim();
    return separated_list1(tuple((whitespace, tag("|"), whitespace)), rule_alternative)(input);
}

/// Tries to parse provided string into the body of
/// Rule Alternative is a sequence of one or more [Element]s separated by space
fn rule_alternative(s: &str) -> IResult<&str, Alternative> {
    let input = s.trim();
    // let (rest, elements) = separated_list1(whitespace, element)(input)?;
    let (rest, elements) = many1(element)(input)?;
    // let elements = a.into_iter().map(|e| return e.1).collect_vec();
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
    let input = s.trim();
    let res = alt((token, grammar_tag, rule_ref, sequence))(input);
    return res;
}

/// Tries to parse input str and return [Element::Rule]
fn rule_ref(s: &str) -> IResult<&str, Element> {
    let input = s.trim();
    let (rest, (rule_name, _, maybe_repeat)) = tuple((rule_name, whitespace, opt(repeat)))(input)?;
    let name = rule_name.to_string();
    if let Some((min, max)) = maybe_repeat {
        return Ok((rest, Element::RuleRef { name, min, max }));
    } else {
        return Ok((
            rest,
            Element::RuleRef {
                name,
                min: 1,
                max: 1,
            },
        ));
    }
}

/// Tries to parse input str and return [Element::Tag]
/// Tag is any string between curly brackets.
/// Tags must not contain curly brackets and semicolons as it will break parsing.
fn grammar_tag(s: &str) -> IResult<&str, Element> {
    let input = s.trim();
    let (rest, matched) = delimited(tag("{"), take_until("}"), tag("}"))(input)?;
    return Ok((
        rest,
        Element::Tag {
            tag: matched.to_string(),
        },
    ));
}

/// Tries to parse input str and return [Element::Sequence]
fn sequence(s: &str) -> IResult<&str, Element> {
    let input = s.trim();
    return alt((braced_sequnece, optional_sequence))(input);
}

/// Tries to parse provided string into [Element::Sequence]
/// where the sequence is surrounded in square brackets [].
/// In this way, the whole sequence is optional, that being as if it had repeat <0-1>
fn optional_sequence(s: &str) -> IResult<&str, Element> {
    let input = s.trim();
    let (rest, (mut sequence)) = delimited(
        tuple((tag("["), whitespace)),
        bare_sequence,
        tuple((whitespace, tag("]"))),
    )(input)?;
    match sequence {
        Element::Sequence {
            alternatives,
            min,
            max,
        } => {
            return Ok((
                rest,
                Element::Sequence {
                    alternatives,
                    min: 0,
                    max,
                },
            ));
        }
        other => unreachable!(),
    }
}

/// Tries to parse provided string into [Element::Sequence]
/// where the sequence is surrounded in braces ().
/// In this way, the whole sequence may have some additional repeat operator attached to it.
fn braced_sequnece(s: &str) -> IResult<&str, Element> {
    let input = s.trim();
    let (rest, (mut sequence, _, maybe_repeat)) = tuple((
        delimited(
            tuple((tag("("), whitespace)),
            bare_sequence,
            tuple((whitespace, tag(")"))),
        ),
        whitespace,
        opt(repeat),
    ))(input)?;
    if let Some((new_min, new_max)) = maybe_repeat {
        match sequence {
            Element::Sequence {
                ref alternatives,
                mut min,
                mut max,
            } => {
                min = new_min;
                max = new_max;
            }
            other => unreachable!(),
        }
    }
    return Ok((rest, sequence));
}

/// Tries to parse input str and return [Element::Sequence]
/// where the sequence isn't surrounded by [] nor ().
/// The sequence is one or more Elements separated by spaces.
fn bare_sequence(s: &str) -> IResult<&str, Element> {
    let input = s.trim();
    // Element::Sequence has the same form as a rule body
    let (rest, matched) = rule_body(input)?;
    /// sequence "seq" that isn't surrounded by [] or () must be matched once
    let sequence = Element::Sequence {
        alternatives: matched,
        min: 1,
        max: 1,
    };
    return Ok((rest, sequence));
}

/// Tries to parse input str and return [Element::Token]
fn token(input: &str) -> IResult<&str, Element> {
    let input = input.trim_start();
    let (rest, token) = take_while1(|c| -> bool {
        return nom_unicode::is_alphanumeric(c) || ".-_:".contains(c);
    })(input)?;
    let token = token.to_owned();
    let input = rest.trim_start();
    if input.starts_with('<') {
        // the next char is <, so there must be some repeat specifier
        let (rest, (min, max)) = repeat(input)?;
        return Ok((rest, Element::Token { token, min, max }));
    } else {
        // the next char is not <, so there is not a repeat specifier
        let min = 1;
        let max = 1;
        return Ok((input, Element::Token { token, min, max }));
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
    use crate::parser::{
        repeat, rule_alternative, rule_body, rule_name, Alternative, Element, Rule,
        REPEAT_MAX_DEFAULT,
    };

    use super::{grammar_tag, rule, token};

    fn new_token(s: &str, min: u32, max: u32) -> Element {
        return Element::Token {
            token: s.to_string(),
            min,
            max,
        };
    }

    fn new_tag(s: &str) -> Element {
        return Element::Tag {
            tag: String::from(s),
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
            assert_eq!(matched, new_token("foo", 1, 1));
        }

        for s in ["foo<1-3>", "foo <1-3>"] {
            let (rest, matched) = token(s).unwrap();
            assert_eq!("", rest);
            assert_eq!(matched, new_token("foo", 1, 3));
        }

        for s in ["foo<3->", "foo <3->"] {
            let (rest, matched) = token(s).unwrap();
            assert_eq!("", rest);
            assert_eq!(matched, new_token("foo", 3, REPEAT_MAX_DEFAULT));
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
            new_token("foo", 1, 1),
            new_token("bar", 1, 1),
            new_token("baz", 1, 1),
        ]);
        assert_eq!(expected, matched);
    }

    #[test]
    fn rule_special() {
        assert_eq!(Rule::Garbage, rule("$GARBAGE").unwrap());
        assert_eq!(Rule::Void, rule("$VOID").unwrap());
        assert_eq!(Rule::Null, rule("$NULL").unwrap());
    }

    #[test]
    fn rule_simple() {
        let s = "$rule = foo bar | baz";
        let rule = rule(s).unwrap();
        let expected = Rule::Normal {
            public: false,
            name: String::from("rule"),
            alternatives: vec![
                Alternative(vec![new_token("foo", 1, 1), new_token("bar", 1, 1)]),
                Alternative(vec![new_token("baz", 1, 1)]),
            ],
        };
        assert_eq!(expected, rule);
    }

    #[test]
    fn rule_public() {
        let s = "public $rule = foo ";
        let rule = rule(s).unwrap();
        let expected = Rule::Normal {
            public: true,
            name: String::from("rule"),
            alternatives: vec![Alternative(vec![new_token("foo", 1, 1)])],
        };
        assert_eq!(expected, rule);
    }

    #[test]
    fn rule_with_opts() {
        let s = "$rule = [foo bar] ";
        let rule = rule(s).unwrap();
        let expected = Rule::Normal {
            public: false,
            name: String::from("rule"),
            alternatives: vec![Alternative(vec![Element::Sequence {
                alternatives: vec![Alternative(vec![
                    new_token("foo", 1, 1),
                    new_token("bar", 1, 1),
                ])],
                min: 0,
                max: 1,
            }])],
        };
        assert_eq!(expected, rule);
    }

    #[test]
    fn rule_with_repeats() {
        let s = "$rule = small <3-10> | medium <5-> | large";
        let expected = Rule::Normal {
            public: false,
            name: String::from("rule"),
            alternatives: vec![
                Alternative(vec![new_token("small", 3, 10)]),
                Alternative(vec![new_token("medium", 5, REPEAT_MAX_DEFAULT)]),
                Alternative(vec![new_token("medium", 1, 1)]),
            ],
        };
    }

    #[test]
    fn rule_with_empty_tag() {
        let s = "$rule = foo {}";
        let r = rule(s).unwrap();
        let expected = Rule::Normal {
            public: false,
            name: String::from("rule"),
            alternatives: vec![Alternative(vec![new_token("foo", 1, 1), new_tag("")])],
        };
        assert_eq!(expected, r);
    }

    #[test]
    fn rule_with_refs() {
        let s = "$rule = $another";
        let r = rule(s).unwrap();
        let expected = Rule::Normal {
            public: false,
            name: String::from("rule"),
            alternatives: vec![Alternative(vec![Element::RuleRef {
                name: String::from("another"),
                min: 1,
                max: 1,
            }])],
        };
        assert_eq!(expected, r);
    }

    #[test]
    fn tag_simple() {
        let s = "{tag}";
        let (rest, t) = grammar_tag(s).unwrap();
        let expected = Element::Tag {
            tag: "tag".to_string(),
        };
        assert_eq!(expected, t);
    }

    #[test]
    fn rule_with_tag_simple() {
        let s = "$rule = foo {tag}";
        let rule = rule(s).unwrap();
        let expected = Rule::Normal {
            public: false,
            name: "rule".to_string(),
            alternatives: vec![Alternative(vec![new_token("foo", 1, 1), new_tag("tag")])],
        };
        assert_eq!(expected, rule);
    }

    #[test]
    fn rule_with_tag_complex() {
        let s = "$rule = (foo {tag} {tag2} )";
        let rule = rule(s).unwrap();
        let expected = Rule::Normal {
            public: false,
            name: "rule".to_string(),
            alternatives: vec![Alternative(vec![Element::Sequence {
                alternatives: vec![Alternative(vec![
                    new_token("foo", 1, 1),
                    new_tag("tag"),
                    new_tag("tag2"),
                ])],
                min: 1,
                max: 1,
            }])],
        };
        assert_eq!(expected, rule);
    }

    // #[test]
    // fn rule_complex() {
    //     // let s = "$chasing = ( {obj_start} $object {obj_end} ( (honí | pronásleduj(í|ou|e)) | (běží | utík(á|ají)) za | prohán(í|ějí) ) $GARBAGE<0-1> {subj_start} $object {subj_end} ) {predicate=chasing}";
    //     // let s = "$chasing = ( {obj_start} $object {obj_end}  $GARBAGE<0-1> {subj_start} $object {subj_end} ) {predicate=chasing}";
    //     let s = "$chasing = ( foo {obj_start} $object {obj} ) {predicate=chasing}";
    //     // let s = "$chasing = (  $object  ) {predicate=chasing}";
    //     let r = rule(s).unwrap();
    //     dbg!(r);
    //     // panic!();
    // }
}
