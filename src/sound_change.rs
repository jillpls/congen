// TODO: Rethink completely :panGCry:

use crate::{ConGenError, ConGenResult};
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap};
use std::slice::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SoundChange {
    from: String,
    to: String,
    before: Vec<SoundChangeRule>,
    after: Vec<SoundChangeRule>,
}

fn find_all_occurrences(haystack: &str, needle: &str) -> Vec<usize> {
    let needle_b = needle.as_bytes();
    haystack
        .as_bytes()
        .windows(needle_b.len())
        .enumerate()
        .filter(|(_, w)| *w == needle_b)
        .map(|(i, _)| i)
        .collect()
}

impl SoundChange {
    pub fn apply_to_str(
        &self,
        input: String,
        categories: &HashMap<char, Vec<char>>,
    ) -> (String, bool) {
        let positions = find_all_occurrences(input.as_str(), &self.from);
        for i in positions {
            let input_before = (&input[0..i]).chars().rev().collect::<Vec<_>>();
            let input_after = (&input[i + self.from.len()..]).chars().collect::<Vec<_>>();
            let (before_matches, _) =
                Self::test_rules_rec(&self.before, 0, &input_before, 0, categories);
            let (after_matches, _) =
                Self::test_rules_rec(&self.after, 0, &input_after, 0, categories);
            if (before_matches || self.before.is_empty())
                && (after_matches || self.after.is_empty())
            {
                return (
                    format!(
                        "{}{}{}",
                        input_before.into_iter().rev().collect::<String>(),
                        &self.to,
                        input_after.into_iter().collect::<String>()
                    ),
                    true,
                );
            }
        }
        (input, false)
    }

    fn test_rules_rec(
        rules: &[SoundChangeRule],
        mut r_idx: usize,
        input: &[char],
        mut i_idx: usize,
        categories: &HashMap<char, Vec<char>>,
    ) -> (bool, Vec<usize>) {
        if r_idx >= rules.len() {
            return (true, vec![r_idx]);
        } // All rules applied
        if i_idx >= input.len() {
            return if matches!(rules[r_idx], SoundChangeRule::StartEnd) {
                (true, vec![r_idx])
            } else {
                (false, vec![])
            };
        }
        let mut to_check = vec![i_idx + 1];
        let current_matches = match &rules[r_idx] {
            SoundChangeRule::StartEnd => i_idx == input.len(),
            SoundChangeRule::Category(c) => {
                let to_check = input[i_idx];
                categories
                    .get(c)
                    .map(|v| v.contains(&to_check))
                    .unwrap_or_default()
            }
            SoundChangeRule::Literal(l) => input[i_idx] == *l,
            SoundChangeRule::Option(inner) => {
                let (inner_r, mut possible) =
                    Self::test_rules_rec(&inner, 0, input, i_idx, categories);
                to_check = vec![i_idx];
                if inner_r {
                    to_check.append(&mut possible.into_iter().map(|p| p + i_idx).collect());
                }
                true
            }
        };

        if !current_matches {
            return (false, vec![]);
        }
        let inner = to_check
            .into_iter()
            .map(|c| Self::test_rules_rec(rules, r_idx + 1, input, c, categories))
            .filter(|(b, _)| *b)
            .map(|(_, v)| v)
            .flatten()
            .dedup()
            .collect::<Vec<_>>();
        (!inner.is_empty(), inner)
    }
}

fn reverse_rules_rec(rules: &mut Vec<SoundChangeRule>) {
    rules.reverse();
    for r in rules.iter_mut() {
        match r {
            SoundChangeRule::Option(i) => {
                reverse_rules_rec(i);
            }
            _ => {}
        }
    }
}

pub fn try_parse(input: &str, categories: &Vec<char>) -> ConGenResult<SoundChange> {
    let splits = input.split("/").collect::<Vec<_>>();
    if splits.len() != 3 {
        return Err(ConGenError::Todo);
    }
    let from = splits[0].to_string();
    let to = splits[1].to_string();
    let rules = splits[2].to_string();
    let rules = rules.split("_").collect::<Vec<_>>();
    if rules.len() != 2 {
        return Err(ConGenError::Todo);
    }
    let mut before = try_parse_rules(&rules[0], categories)?;
    reverse_rules_rec(&mut before);
    let after = try_parse_rules(&rules[1], categories)?;
    Ok(SoundChange {
        from,
        to,
        before,
        after,
    })
}

pub fn try_parse_rules(
    input: &str,
    categories: &Vec<char>,
) -> ConGenResult<Vec<SoundChangeRule>> {
    let mut stack = vec![vec![]];
    for c in input.chars() {
        match c {
            '#' => stack
                .last_mut()
                .ok_or(ConGenError::Todo)?
                .push(SoundChangeRule::StartEnd), // Start/End
            '(' => {
                stack.push(vec![]);
            } // Start Optional
            ')' => {
                let inner = stack.pop().ok_or(ConGenError::Todo)?;
                stack
                    .last_mut()
                    .ok_or(ConGenError::Todo)?
                    .push(SoundChangeRule::Option(Box::new(inner)))
            } // End optional
            _ => {
                let to_push = if categories.contains(&c) {
                    SoundChangeRule::Category(c)
                } else {
                    SoundChangeRule::Literal(c)
                };
                stack.last_mut().ok_or(ConGenError::Todo)?.push(to_push);
            }
        }
    }
    if stack.len() != 1 {
        Err(ConGenError::Todo)
    } else {
        stack.pop().ok_or(ConGenError::Todo)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub enum SoundChangeRule {
    StartEnd,
    Category(char),
    Literal(char),
    Option(Box<Vec<SoundChangeRule>>),
}

#[cfg(test)]
mod test {
    use crate::sound_change::SoundChangeRule;
    use std::collections::{HashMap};

    #[test]
    fn t_parse() {
        let input = "a/b/#b_cc";
        let parsed = super::try_parse(input, &Vec::new());
        let parsed = parsed.unwrap();
        assert_eq!(parsed.before.len(), 2);
        assert_eq!(parsed.after.len(), 2);
        assert_eq!(parsed.before[1], SoundChangeRule::StartEnd);
    }

    #[test]
    fn t_apply() {
        let input = "a/b/#b_(x)CC";
        let mut categories = HashMap::new();
        categories.insert('C', ['c', 'b', 'd'].into_iter().collect::<Vec<_>>());
        let keys = categories.keys().copied().collect::<Vec<_>>();
        let parsed = super::try_parse(input, &keys).unwrap();
        let (r, changed) = parsed.apply_to_str("bacdd".to_string(), &categories);
        assert_eq!(r.as_str(), "bbcdd");
        let input = "aa/a/_#";
        let parsed = super::try_parse(input, &keys).unwrap();
        let (r, changed) = parsed.apply_to_str("xxxaaa".to_string(), &categories);
        assert_eq!(r.as_str(), "xxxaa");
    }
}
