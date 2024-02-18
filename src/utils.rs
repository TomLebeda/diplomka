use regex::Regex;

/// removes the trailing numbering #n of objects (everything after the # is removed)
pub fn remove_number_from_obj(str: &str) -> String {
    // I don't use regex because I don't want to compile the expression on every call
    let s = str.trim();
    if let Some(idx) = s.find('#') {
        return s[..idx].trim().to_owned();
    } else {
        return s.to_owned();
    }
}
