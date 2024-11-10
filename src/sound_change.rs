use crate::generation::GenerationInstructionInner;
use crate::ConGenError::GenericParseError;
use crate::ConGenResult;
use std::collections::{HashMap, HashSet};

pub struct SoundChangeRule {
    gap_pos: usize,
    from: SoundChangeReplacement,
    to: SoundChangeReplacement,
    find: SoundChangePattern,
}

impl SoundChangeRule {
    pub fn apply_to_str(
        &self,
        input: &str,
        categories: &HashMap<String, GenerationInstructionInner>,
    ) -> String {
        // TODO: Categories
        let from = self.from.as_literal();
        let to = self.to.as_literal();
        let mut result = input.to_string();
        // TODO: Options
        let mut max = 0;
        while let Some(p) = &result[max..].find(from) {
            let p = *p + max;
            max += 1;
            let mut matches = true;
            for i in 0..result.len() - (p + from.len()) {
                let str_pos = p + from.len() + i;
                let pattern_pos = self.gap_pos + 1 + i;
                if str_pos >= result.len() || pattern_pos >= self.find.pattern.len() {
                    break;
                }
                if !self.find.pattern[pattern_pos].check(
                    &result[str_pos..str_pos + 1],
                    str_pos,
                    &result,
                ) {
                    matches = false;
                    break;
                }
            }
            if !matches {
                continue;
            }
            for i in 0..p {
                if p == 0 || self.gap_pos == 0 {
                    break;
                }
                let str_pos = p - 1 - i;
                let pattern_pos = self.gap_pos - 1 - i;
                if str_pos >= result.len() || pattern_pos >= self.find.pattern.len() {
                    break;
                }
                if !self.find.pattern[pattern_pos].check(
                    &result[str_pos..str_pos + 1],
                    str_pos,
                    &result,
                ) {
                    matches = false;
                    break;
                }
            }
            if matches {
                let start = &result[0..p];
                let end = &result[p + from.len()..];
                result = format!("{}{}{}", start, to, end);
            }
        }

        result
    }

    pub fn try_parse(input: &str, categories: &HashSet<String>) -> ConGenResult<Self> {
        let splits = input.split('/').collect::<Vec<_>>();
        if splits.len() != 3 {
            return Err(GenericParseError(format!(
                "Expected {} separators, found {}",
                3,
                splits.len() - 1
            )));
        }
        let from = SoundChangeReplacement::parse(splits[0]);
        let to = SoundChangeReplacement::parse(splits[1]);
        let find = SoundChangePattern::try_parse(splits[2], categories)?; // TODO: Categories
        Ok(Self {
            gap_pos: find
                .pattern
                .iter()
                .position(|v| v == &SoundChangePatternElement::Gap)
                .ok_or(GenericParseError(
                    "SoundChangePattern does not contain a gap".to_string(),
                ))?,
            from,
            to,
            find,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SoundChangePattern {
    pattern: Vec<SoundChangePatternElement>,
}

impl SoundChangePattern {
    pub fn try_parse(input: &str, categories: &HashSet<String>) -> ConGenResult<Self> {
        let mut gap_found = false;
        let mut stack: Vec<Vec<SoundChangePatternElement>> = vec![vec![]];
        for (i, c) in input.chars().enumerate() {
            match c {
                '(' => {
                    stack.push(vec![]);
                    continue;
                }
                ')' => {
                    let to_add = stack
                        .pop()
                        .ok_or(GenericParseError("Too many ')'".to_string()))?;
                    let to_add =
                        SoundChangePatternElement::Optional(Box::new(SoundChangePattern {
                            pattern: to_add,
                        }));
                    stack
                        .last_mut()
                        .ok_or(GenericParseError("Too many ')'".to_string()))?
                        .push(to_add);
                    continue;
                }
                _ => {}
            }
            let stack_size = stack.len();
            let current = stack
                .last_mut()
                .ok_or(GenericParseError("Too many ')'".to_string()))?;
            match c {
                '#' => {
                    if i == 0 {
                        current.push(SoundChangePatternElement::Start)
                    } else if i == input.len() - 1 {
                        current.push(SoundChangePatternElement::End)
                    } else {
                        return Err(GenericParseError("Found # in the middle".to_string()));
                    }
                }
                '_' => {
                    if stack_size > 1 {
                        return Err(GenericParseError("Gap in optional part.".to_string()));
                    }
                    if !gap_found {
                        current.push(SoundChangePatternElement::Gap);
                        gap_found = true;
                    } else {
                        return Err(GenericParseError(
                            "Found multiple '_' gap symbols".to_string(),
                        ));
                    }
                }
                _ => {
                    current.push(if categories.contains(&c.to_string()) {
                        SoundChangePatternElement::Category(c.to_string())
                    } else {
                        SoundChangePatternElement::Literal(c.to_string())
                    });
                }
            }
        }
        if !gap_found {
            return Err(GenericParseError("No gap in input".to_string()));
        }
        if stack.len() != 1 {
            Err(GenericParseError("Too many '('".to_string()))
        } else {
            let r = stack.pop().unwrap();
            Ok(Self { pattern: r })
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SoundChangePatternElement {
    Gap,
    Start,
    End,
    Literal(String),
    Category(String),
    Optional(Box<SoundChangePattern>),
}

impl SoundChangePatternElement {
    pub fn check(&self, input: &str, pos: usize, current: &str) -> bool {
        match self {
            SoundChangePatternElement::Gap => {
                true // TODO
            }
            SoundChangePatternElement::Start => pos == 0,
            SoundChangePatternElement::End => pos == current.len() - 1,
            SoundChangePatternElement::Literal(l) => l == input,
            _ => {
                todo!()
            }
        }
    }
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SoundChangeReplacement {
    Literal(String),
}

impl SoundChangeReplacement {
    pub fn parse(input: &str) -> Self {
        // TODO: Categories
        Self::Literal(input.to_string())
    }

    pub fn as_literal(&self) -> &String {
        match &self {
            SoundChangeReplacement::Literal(l) => l,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::sound_change::{
        SoundChangePattern, SoundChangePatternElement, SoundChangeReplacement, SoundChangeRule,
    };
    use std::collections::{HashMap, HashSet};

    #[test]
    fn t_parse() {
        let input = "a/b/#_(f)F#";
        let r =
            SoundChangeRule::try_parse(input, &["F".to_string()].into_iter().collect()).unwrap();
        assert_eq!(
            r.from,
            SoundChangeReplacement::Literal("a".to_string()),
            "from string"
        );
        assert_eq!(
            r.to,
            SoundChangeReplacement::Literal("b".to_string()),
            "to string"
        );
        let pattern = SoundChangePattern {
            pattern: vec![
                SoundChangePatternElement::Start,
                SoundChangePatternElement::Gap,
                SoundChangePatternElement::Optional(
                    SoundChangePattern {
                        pattern: vec![SoundChangePatternElement::Literal("f".to_string())],
                    }
                    .into(),
                ),
                SoundChangePatternElement::Category("F".to_string()),
                SoundChangePatternElement::End,
            ],
        };
        assert_eq!(r.find, pattern, "pattern should match");
    }

    #[test]
    fn t_parse_err() {
        let input = "a/b/_#a";
        assert!(
            SoundChangeRule::try_parse(input, &HashSet::new()).is_err(),
            "input included # in the middle"
        );
        let input = "a/b/__";
        assert!(
            SoundChangeRule::try_parse(input, &HashSet::new()).is_err(),
            "input included multipel gaps"
        );
        let input = "a/b/a(_)";
        assert!(
            SoundChangeRule::try_parse(input, &HashSet::new()).is_err(),
            "input included gap in option"
        );
        let input = "a/b";
        assert!(
            SoundChangeRule::try_parse(input, &HashSet::new()).is_err(),
            "input has missing separators"
        );
        let input = "a/b/_((a)";
        assert!(
            SoundChangeRule::try_parse(input, &HashSet::new()).is_err(),
            "input has unmatched ("
        );
        let input = "a/b/_(a))";
        assert!(
            SoundChangeRule::try_parse(input, &HashSet::new()).is_err(),
            "input has unmatched )"
        );
        let input = "a/b/c";
        assert!(
            SoundChangeRule::try_parse(input, &HashSet::new()).is_err(),
            "input has no gap"
        );
    }

    #[test]
    fn t_apply() {
        let sound_change = "a/b/_";
        let sound_change = SoundChangeRule::try_parse(sound_change, &HashSet::new()).unwrap();
        let input = "blablab";
        assert_eq!("blbblbb", sound_change.apply_to_str(input, &HashMap::new()));
        let sound_change = "a/cc/#_";
        let sound_change = SoundChangeRule::try_parse(sound_change, &HashSet::new()).unwrap();
        let input = "alablab";
        assert_eq!(
            "cclablab",
            sound_change.apply_to_str(input, &HashMap::new())
        );
    }
}
