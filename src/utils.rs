/// Removes the trailing numbering #n of objects (everything after the # is removed)
pub fn remove_number_from_obj(str: &str) -> &str {
    // I don't use regex because I don't want to compile the expression on every call
    let s = str.trim();
    if let Some(idx) = s.find('#') {
        return s[..idx].trim();
    } else {
        return s;
    }
}

/// Modifies the sequence of tags so that numbers are merged as one tag.
///
/// For example number 123 will be originally caught as ["100", "20", "3"],
/// but this function will merge those individual tags into ["123"].
pub fn merge_number_tags(tags: &mut Vec<String>) {
    let mut changed = true;
    let mut seqeunce_start = 0;
    let mut seqeunce_end = 0;
    while changed {
        let tag_count = tags.len();
        changed = false;
        let mut sequence_active = false;
        // iterate until there is no change
        // find next continuous numeric range that is more than 1 number long
        for (idx, item) in tags.iter_mut().enumerate() {
            let is_numeric = item.parse::<u32>().is_ok();
            if is_numeric {
                if !sequence_active {
                    sequence_active = true;
                    seqeunce_start = idx;
                }
            } else {
                // end current sequence
                if sequence_active {
                    seqeunce_end = idx;
                    let seqeunce_length = seqeunce_end - seqeunce_start;
                    if seqeunce_length > 1 {
                        // we have sequence longer than 1 element => merge
                        break; // break the searching and continue with the merge
                    } else {
                        // we have sequence of single number => continue
                        sequence_active = false;
                        continue;
                    }
                }
            }
            let last = idx == tag_count - 1;
            if last && is_numeric {
                seqeunce_end = idx + 1;
                let seqeunce_length = seqeunce_end - seqeunce_start;
                if seqeunce_length <= 1 {
                    // we have sequence of single number => continue
                    sequence_active = false;
                    continue;
                }
            }
        }
        let seqeunce_length = seqeunce_end - seqeunce_start;
        if sequence_active && seqeunce_length > 1 {
            // sequence_active = false;
            let items = tags.get(seqeunce_start..seqeunce_end).unwrap();
            let mut buf: Vec<u32> = vec![];
            let mut inner_buf: u32 = 0;
            for (i, num) in items.iter().enumerate() {
                let current_num = num.parse::<u32>().unwrap();
                if inner_buf == 0 {
                    inner_buf = current_num;
                } else {
                    let prev_num = items[i - 1].parse::<u32>().unwrap();
                    if prev_num < current_num {
                        inner_buf *= current_num;
                    } else {
                        buf.push(inner_buf);
                        inner_buf = current_num;
                    }
                }
            }
            // don't forget to push the last inner buffer
            buf.push(inner_buf);
            let value = buf.iter().sum::<u32>();
            // do the actual replacement
            tags.splice(seqeunce_start..seqeunce_end, vec![value.to_string()]);
            // make sure to do next check
            changed = true;
        }
    }
}

/// unit tests for the 'utils.rs'
#[cfg(test)]
mod tests {
    use crate::utils::merge_number_tags;

    #[test]
    fn merge_number_tags_no_numbers() {
        let mut tags = vec![
            "foo".to_string(),
            "bar".to_string(),
            "boo".to_string(),
            "baz".to_string(),
        ];
        merge_number_tags(&mut tags);
        let expected = ["foo", "bar", "boo", "baz"];
        assert_eq!(tags, expected);
    }

    #[test]
    fn merge_number_tags_single_number() {
        let mut tags = vec![
            "foo".to_string(),
            "bar".to_string(),
            "1".to_string(),
            "baz".to_string(),
        ];
        merge_number_tags(&mut tags);
        let expected = ["foo", "bar", "1", "baz"];
        assert_eq!(tags, expected);
    }

    #[test]
    fn merge_number_tags_single_number_at_start() {
        let mut tags = vec![
            "1".to_string(),
            "foo".to_string(),
            "bar".to_string(),
            "baz".to_string(),
        ];
        merge_number_tags(&mut tags);
        let expected = ["1", "foo", "bar", "baz"];
        assert_eq!(tags, expected);
    }

    #[test]
    fn merge_number_tags_single_number_at_end() {
        let mut tags = vec![
            "foo".to_string(),
            "bar".to_string(),
            "baz".to_string(),
            "1".to_string(),
        ];
        merge_number_tags(&mut tags);
        let expected = ["foo", "bar", "baz", "1"];
        assert_eq!(tags, expected);
    }

    #[test]
    fn merge_number_tags_number_sequence() {
        let mut tags = vec![
            "foo".to_string(),
            "100".to_string(),
            "20".to_string(),
            "3".to_string(),
            "bar".to_string(),
            "baz".to_string(),
        ];
        merge_number_tags(&mut tags);
        let expected = ["foo", "123", "bar", "baz"];
        assert_eq!(tags, expected);
    }

    #[test]
    fn merge_number_tags_number_sequence_at_start() {
        let mut tags = vec![
            "1000".to_string(),
            "20".to_string(),
            "3".to_string(),
            "foo".to_string(),
            "bar".to_string(),
            "baz".to_string(),
        ];
        merge_number_tags(&mut tags);
        let expected = ["1023", "foo", "bar", "baz"];
        assert_eq!(tags, expected);
    }

    #[test]
    fn merge_number_tags_number_sequence_at_end() {
        let mut tags = vec![
            "foo".to_string(),
            "bar".to_string(),
            "baz".to_string(),
            "15".to_string(),
            "1000".to_string(),
            "20".to_string(),
            "3".to_string(),
        ];
        merge_number_tags(&mut tags);
        let expected = ["foo", "bar", "baz", "15023"];
        assert_eq!(tags, expected);
    }

    #[test]
    fn merge_number_tags_number_sequences_multiple() {
        let mut tags = vec![
            "foo".to_string(),
            "17".to_string(),
            "1000".to_string(),
            "100".to_string(),
            "3".to_string(),
            "bar".to_string(),
            "30".to_string(),
            "1000".to_string(),
            "3".to_string(),
            "100".to_string(),
            "20".to_string(),
            "6".to_string(),
            "baz".to_string(),
            "3".to_string(),
            "1000".to_string(),
            "3".to_string(),
        ];
        merge_number_tags(&mut tags);
        let expected = ["foo", "17103", "bar", "30326", "baz", "3003"];
        assert_eq!(tags, expected);
    }
}
