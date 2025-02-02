use super::instruction::*;
use crate::app::Categories;
use crate::{ConGenError, ConGenResult};
use std::cmp::max;
use std::collections::HashMap;
use uuid::Uuid;

pub trait Contains<T> {
    fn contains(&self, val: &T) -> bool;
}

impl<T, V> Contains<T> for HashMap<T, V> {
    fn contains(&self, val: &T) -> bool {
        self.contains(val)
    }
}

const RESERVED: [char; 9] = ['(', ')', '[', ']', '\\', '=', '*', '{', '}'];

fn parse_instruction_inner<U>(
    input: &str,
    reserved: &HashMap<String, U>,
    sounds_rev: &HashMap<String, Uuid>,
) -> ConGenResult<GenerationInstruction<String>> {
    let mut escape = false;
    let mut count = false;
    let mut num = String::new();
    // let mut result = vec![];
    let mut stack = vec![GenerationInstruction::List(vec![])];
    for c in input.chars() {
        if count {
            if c.is_numeric() {
                num.push(c);
                continue;
            } else {
                count = false;
                let max_num = num.parse::<usize>()?;
                let last_list = stack
                    .last_mut()
                    .ok_or(ConGenError::GenericParseError("Stack error".to_string()))?
                    .as_mut_list()?;
                let part = last_list
                    .pop()
                    .ok_or(ConGenError::GenericParseError("Stack error".to_string()))?;
                let same = c == '!';
                let part = GenerationInstruction::Repetition(part, (1, max_num), false);
                last_list.push(Box::new(part));
                num = String::new();
                if same {
                    continue;
                }
            }
        }
        let c_str = c.to_string();
        let instruction = if !escape && !c.is_numeric() {
            match c {
                '{' => {
                    stack.push(GenerationInstruction::List(vec![]));
                    continue;
                }
                '}' => {
                    if !matches!(
                        stack.last().ok_or(ConGenError::Todo)?,
                        GenerationInstruction::List(_)
                    ) {
                        return Err(ConGenError::Todo);
                    }
                    let part = stack.pop().ok_or(ConGenError::Todo)?;
                    stack.last_mut().ok_or(ConGenError::Todo)?.add_part(part);
                    continue;
                }
                '(' => {
                    stack.push(GenerationInstruction::List(vec![]));
                    continue;
                }
                ')' => {
                    if !matches!(
                        stack.last().ok_or(ConGenError::Todo)?,
                        GenerationInstruction::List(_)
                    ) {
                        return Err(ConGenError::Todo);
                    }
                    let part = stack.pop().ok_or(ConGenError::Todo)?;
                    let part = if part.len() == 1 {
                        part.to_first()
                    } else {
                        Box::new(part)
                    };
                    let part = GenerationInstruction::Repetition(part, (0, 1), false);
                    stack.last_mut().ok_or(ConGenError::Todo)?.add_part(part);
                    continue;
                }
                '[' => {
                    stack.push(GenerationInstruction::Options(vec![]));
                    continue;
                }
                ']' => {
                    if !matches!(
                        stack.last().ok_or(ConGenError::Todo)?,
                        GenerationInstruction::Options(_)
                    ) {
                        return Err(ConGenError::Todo);
                    }
                    let part = stack.pop().ok_or(ConGenError::Todo)?;
                    stack.last_mut().ok_or(ConGenError::Todo)?.add_part(part);
                    continue;
                }
                '\\' => {
                    escape = true;
                    continue;
                }
                '*' => {
                    let last_list = stack.last_mut().ok_or(ConGenError::Todo)?.as_mut_list()?;
                    let part = last_list.pop().ok_or(ConGenError::Todo)?;
                    let part = GenerationInstruction::Repetition(part, (0, 10), true); // TODO
                    last_list.push(Box::new(part));
                    continue;
                }
                _ => {
                    if reserved.contains_key(&c_str) {
                        GenerationInstruction::Inner(c_str)
                    } else if let Some(id) = sounds_rev.get(&c_str) {
                        GenerationInstruction::Sound(*id)
                    } else {
                        GenerationInstruction::Literal(c_str)
                    }
                }
            }
        } else if !escape && c.is_numeric() {
            count = true;
            num.push(c);
            continue;
        } else {
            if reserved.contains_key(&c_str) {
                GenerationInstruction::Inner(c_str)
            } else if let Some(id) = sounds_rev.get(&c_str) {
                GenerationInstruction::Sound(*id)
            } else {
                GenerationInstruction::Literal(c_str)
            }
        };
        escape = false;
        let last = stack.last_mut().ok_or(ConGenError::Todo)?;
        last.add_part(instruction);
    }
    if count {
        let max_num = num.parse::<usize>()?;
        let last_list = stack.last_mut().ok_or(ConGenError::Todo)?.as_mut_list()?;
        let part = last_list.pop().ok_or(ConGenError::Todo)?;
        let part = GenerationInstruction::Repetition(part, (1, max_num), false);
        last_list.push(Box::new(part));
    }
    if stack.len() > 1 {
        return Err(ConGenError::TodoExplained("longstack".to_string()));
    }
    let result = stack.pop().ok_or(ConGenError::Todo)?;
    Ok(result)
}

fn parse_instruction<U>(
    instruction: &str,
    reserved: &HashMap<String, U>,
    sounds_rev: &HashMap<String, Uuid>,
) -> ConGenResult<(String, GenerationInstruction<String>)> {
    let split = instruction.split("=").collect::<Vec<_>>();
    if split.len() != 2 {
        todo!()
    }
    let name = split[0].to_string();
    Ok((
        name,
        parse_instruction_inner(split[1], reserved, sounds_rev)?,
    ))
}

impl WordGen {
    pub fn fill_categories(&mut self, categories: &Categories) {
        for (n, cat) in categories {
            let cat_name = n.to_string();
            let cat_instruction = GenerationInstruction::Options(
                cat.into_iter()
                    .map(|(id, s)| {
                        self.rep_sounds.insert(s.representation().to_string(), *id);
                        self.sounds.insert(*id, s.clone());
                        Box::new(GenerationInstruction::Sound(*id))
                    })
                    .collect(),
            );

            if let Some(v) = self.categories.get_mut(&cat_name) {
                v.push(cat_instruction);
            } else {
                self.categories.insert(cat_name, vec![cat_instruction]);
            }
        }
    }

    fn fill<U>(
        to_fill: &mut HashMap<String, Vec<GenerationInstruction<String>>>,
        reserved: &HashMap<String, U>,
        sounds: &HashMap<String, Uuid>,
        input: &[String],
    ) -> ConGenResult<()> {
        let r = input
            .into_iter()
            .map(|p| parse_instruction(p, reserved, sounds))
            .collect::<Vec<_>>();
        for result in r {
            if let Ok((name, i)) = result {
                if let Some(v) = to_fill.get_mut(&name) {
                    v.push(i);
                } else {
                    to_fill.insert(name, vec![i]);
                }
            } else {
            }
        }
        Ok(())
    }

    pub fn fill_syllable_parts(&mut self, input: &[String]) -> ConGenResult<()> {
        let r = input
            .into_iter()
            .map(|p| parse_instruction(p, &self.categories, &self.rep_sounds))
            .collect::<Vec<_>>();
        for result in r {
            if let Ok((name, i)) = result {
                if let Some(v) = self.syllable_parts.get_mut(&name) {
                    v.push(i);
                } else {
                    self.syllable_parts.insert(name, vec![i]);
                }
            } else {
            }
        }
        Ok(())
    }

    pub fn fill_syllables(
        &mut self,
        input: &[(String, Option<String>, String, Option<String>)],
    ) -> ConGenResult<()> {
        for (name, onset, nucleus, coda) in input.into_iter().cloned() {
            let instruction = SyllableInstruction {
                onset,
                nucleus,
                coda,
            };
            if let Some(v) = self.syllables.get_mut(&name) {
                v.push(instruction);
            } else {
                self.syllables.insert(name, vec![instruction]);
            }
        }
        Ok(())
    }

    pub fn fill_words(&mut self, parts: &[String]) -> ConGenResult<()> {
        let r = parts
            .iter()
            .map(|p| parse_instruction_inner(p, &self.syllables, &self.rep_sounds))
            .collect::<Vec<_>>();
        self.words = r.into_iter().filter_map(|i| i.ok()).collect();
        Ok(())
    }
}
