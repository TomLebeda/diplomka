use std::{collections::HashMap, fmt::Display};

use itertools::Itertools;
use log::*;
use regex::Regex;
use serde_json::Value;

/// Simplified error type for when fetching fails
pub enum FetchErr {
    /// no response was received,
    /// that could be for example network error or bad endpoint
    NoResponse,
    /// some response was received, but the status code wasn't OK
    Status(u16),
    /// fetched response had unexpected content and couldn't be parsed
    ParsingFailed,
    /// response didn't contain required key
    KeyNotFound,
    /// structure of returned response doesn't match expected form
    StructureMismatch,
    /// section "čeština" was not found
    MissingSection(String),
}

/// Struct that represents sections in given wiktionary page
#[derive(Debug)]
struct WikiSection {
    /// name of the section
    line: String,
    /// identification number of the section, i.e. "1.2.3"
    number: String,
    /// index of the section in the original list (used in URL request)
    idx: usize,
}

impl Display for FetchErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            FetchErr::NoResponse => write!(f, "no response"),
            FetchErr::Status(code) => write!(f, "status {}", code),
            FetchErr::ParsingFailed => write!(f, "parsing failed"),
            FetchErr::KeyNotFound => write!(f, "key not found"),
            FetchErr::StructureMismatch => write!(f, "structure mismatch"),
            FetchErr::MissingSection(sec) => write!(f, "missing section \"{}\"", sec),
        };
    }
}

/// Fetches word forms from all available data sources and
/// returns them as a vector of strings.
/// Results will be unique and sorted.
pub fn fetch_word_forms(word: &str) -> Result<Vec<String>, FetchErr> {
    trace!("fetching word forms for \"{}\"", word);
    let sections = fetch_wiki_sections(word)?;
    let Some(czech_prefix) = sections
        .iter()
        .find(|sec| return sec.line == "čeština")
        .map(|sec| return &sec.number)
    else {
        return Err(FetchErr::MissingSection(String::from("čeština")));
    };
    let sections_of_interest = sections
        .iter()
        .filter(|sec| return sec.number.starts_with(czech_prefix))
        .filter(|sec| return sec.line == "skloňování" || sec.line == "časování")
        .collect_vec();
    trace!(
        "found {} sections of interest for \"{}\"",
        sections_of_interest.len(),
        word
    );
    let forms = sections_of_interest
        .iter()
        .flat_map(|soi| return fetch_wiki_forms(word, soi))
        .collect_vec();
    return Ok(forms);
}

/// Fetches and parses word forms from wiktionary from provided section.
/// Returns empty vector if there is nothing to parse or some error occurs.
fn fetch_wiki_forms(word: &str, sec: &WikiSection) -> Vec<String> {
    trace!(
        "fetching wiki section \"{}\" [{}] for word {}",
        sec.line,
        sec.number,
        word
    );
    match ureq::get(
        format!(
            "https://cs.wiktionary.org/w/api.php?action={}&format={}&page={}&section={}&prop={}",
            "parse", "json", word, sec.idx, "parsetree"
        )
        .as_str(),
    )
    .call()
    {
        Ok(resp) => match resp.status() {
            200 => {
                let value_regex = Regex::new(r"<value>(.*?)</value>")
                    .expect("regex for value extraction is invalid");
                trace!("recevied response with code OK-200, processing further");
                let Ok(json) = resp.into_json::<Value>() else {
                    return vec![];
                };
                let Some(parse) = json.get("parse") else {
                    return vec![];
                };
                let Some(parsetree) = parse.get("parsetree").and_then(|v| return v.get("*")) else {
                    return vec![];
                };
                let Some(haystack) = parsetree.as_str() else {
                    return vec![];
                };
                let haystack = haystack.replace('\n', " ");
                trace!("obtained hasystack, extracting forms",);
                let forms = value_regex
                    .find_iter(&haystack)
                    .map(|v| return v.as_str())
                    .map(|s| return s.trim_start_matches("<value>"))
                    .map(|s| return s.trim_end_matches("</value>"))
                    .map(|s| return s.replace(['[', ']'], ""))
                    .flat_map(|s| {
                        return s
                            .split([',', ';', '/'])
                            .map(|v| return v.to_owned())
                            .collect_vec();
                    })
                    .map(|val: String| return val.trim().to_owned())
                    .sorted_unstable()
                    .unique()
                    .map(|s| return s.to_owned())
                    .collect_vec();
                trace!("obtained {} forms for word {}", forms.len(), word);
                // "skrýt" causes false positives, see: https://cs.wiktionary.org/wiki/Šablona:Sloveso_(cs)#Skrývání_tabulek
                if word == "skrýt" {
                    // we don't care if the word is "skrýt" itself
                    return forms;
                } else {
                    // otherwise filter it out
                    return forms
                        .iter()
                        .filter(|s| return s.as_str() != "skrýt")
                        .cloned()
                        .collect_vec();
                }
            }
            code => {
                error!("received response with code {}", code);
                return vec![];
            }
        },
        Err(e) => {
            error!("didn't receive any valid response");
            return vec![];
        }
    }
}

/// Fetch and return list of section headings for given wiktionary word page
fn fetch_wiki_sections(word: &str) -> Result<Vec<WikiSection>, FetchErr> {
    trace!("fetching list of wiki sections for \"{}\"", word);
    match ureq::get(
        format!(
            "https://cs.wiktionary.org/w/api.php?action={}&format={}&prop={}&page={}",
            "parse", "json", "sections", word
        )
        .as_str(),
    )
    .call()
    {
        Ok(resp) => {
            match resp.status() {
                200 => {
                    trace!("received response with code OK-200, processing further");
                    let Ok(json) = resp.into_json::<serde_json::Value>() else {
                        error!("can't parse response into JSON values");
                        return Err(FetchErr::ParsingFailed);
                    };
                    let Some(raw_sections) = json
                        .get("parse")
                        .and_then(|parse| return parse.get("sections"))
                    else {
                        error!("can't find keys \"parse\" followed by \"sections\" in response");
                        return Err(FetchErr::KeyNotFound);
                    };
                    let Some(objects) = raw_sections.as_array() else {
                        error!("value under key \"parse.sections\" isn't array of objects");
                        return Err(FetchErr::StructureMismatch);
                    };
                    let wiki_sections = objects
                        .iter()
                        .enumerate()
                        .filter_map(|(idx, obj)| {
                            let Some(line) = obj
                                .get("line")
                                .and_then(|v| return v.as_str())
                                .map(|s| return s.to_owned())
                            else {
                                return None;
                            };
                            let Some(number) = obj
                                .get("number")
                                .and_then(|v| return v.as_str())
                                .map(|s| return s.to_owned())
                            else {
                                return None;
                            };
                            return Some(WikiSection {
                                line,
                                number,
                                idx: idx + 1, // Wiki API section index starts at 1
                            });
                        })
                        .collect_vec();
                    trace!("succesfully obtained wiki sections for \"{}\"", word);
                    return Ok(wiki_sections);
                }
                code => {
                    error!("received response with code {}", code);
                    return Err(FetchErr::Status(code));
                }
            }
        }
        Err(e) => {
            error!("didn't reacevie any valid response");
            return Err(FetchErr::NoResponse);
        }
    }
}
