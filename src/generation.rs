// TODO: Generate Syllables as Onset Nucleus Coda

use crate::app::Categories;
use crate::sounds::Sound;
use crate::{ConGenError, ConGenResult};
use rand::prelude::SliceRandom;
use rand::Rng;
use rand_distr::{Distribution, Normal};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone)]
pub struct GenerationSettings {
    pub(crate) deviation: Option<f32>,
}

impl Default for GenerationSettings {
    fn default() -> Self {
        Self {
            deviation: Some(1.),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord)]
pub struct Syllable {
    pub(crate) instruction: Option<String>,
    pub(crate) sounds: Vec<Sound>,
}

impl PartialOrd for Syllable {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.sounds.partial_cmp(&other.sounds)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord)]
pub struct Word {
    pub(crate) instruction: Option<String>,
    pub(crate) syllables: Vec<Syllable>,
}

impl PartialOrd for Word {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.syllables.partial_cmp(&other.syllables)
    }
}

impl Display for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.syllables
                .iter()
                .map(|s| {
                    s.sounds
                        .iter()
                        .map(|s| s.representation.clone())
                        .collect::<Vec<_>>()
                        .join("")
                })
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

#[derive(Debug, Clone)]
pub struct GenerationInstructionRoot {
    name: Option<String>,
    pub(crate) instruction: GenerationInstruction,
}

impl Display for GenerationInstructionRoot {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let start = self
            .name
            .as_ref()
            .map(|n| format!("{}=", n))
            .unwrap_or_default();
        write!(f, "{}{}", start, self.instruction)
    }
}

pub fn generate_map(
    instructions: Vec<GenerationInstructionRoot>,
) -> HashMap<String, GenerationInstructionInner> {
    let mut map: HashMap<String, GenerationInstruction> = HashMap::new();
    let mut artificials = HashSet::new();
    for e in instructions.clone() {
        let name = e.name.clone().unwrap_or("?".to_string()); // TODO
        if let Some(g) = map.get_mut(&name) {
            let boxed = Box::new(g.clone());
            if !artificials.contains(&name) {
                artificials.insert(name);
                *g = GenerationInstruction::Options(vec![boxed, Box::new(e.instruction)]);
            } else {
                g.as_mut_list().unwrap().push(boxed);
            }
        } else {
            map.insert(name, e.instruction);
        }
    }
    map.into_iter()
        .map(|(s, e)| {
            (
                s.clone(),
                GenerationInstructionInner::LowerLevel(Box::new(GenerationInstructionRoot {
                    name: Some(s),
                    instruction: e,
                })),
            )
        })
        .collect::<_>()
}

impl GenerationInstructionRoot {
    pub fn shallow_generate<R: Rng>(
        &self,
        rng: &mut R,
        settings: &GenerationSettings,
    ) -> Vec<ShallowGenResult<'_>> {
        self.instruction.shallow_generate(rng, settings)
    }

    pub fn from_ipa_categories(categories: &Categories) -> Vec<Self> {
        let mut result = vec![];
        for (n, cat) in categories {
            result.push(Self {
                name: Some(n.to_string()),
                instruction: GenerationInstruction::Options(
                    cat.iter()
                        .map(|(_, s)| {
                            Box::new(GenerationInstruction::Part(Box::new(
                                GenerationInstructionInner::Sound(s.clone()),
                            )))
                        })
                        .collect(),
                ),
            })
        }
        result
    }

    pub fn parse_all(
        input: &str,
        lookup: &HashMap<String, GenerationInstructionInner>,
    ) -> Vec<Self> {
        let sets: Vec<_> = input.split("\n").map(|i| i.split(';')).flatten().collect();
        sets.into_iter()
            .filter_map(|s| Self::try_parse(s, lookup).ok())
            .collect()
    }

    pub fn try_parse(
        input: &str,
        lookup: &HashMap<String, GenerationInstructionInner>,
    ) -> ConGenResult<Self> {
        let split = input.splitn(2, '=').collect::<Vec<_>>();
        if split.len() > 2 || split.is_empty() {
            return Err(ConGenError::Todo);
        }
        let (name, to_parse) = if split.len() == 2 {
            (Some(split[0].to_string()), split[1])
        } else {
            (None, split[0])
        };
        Ok(Self {
            name,
            instruction: GenerationInstruction::try_parse(to_parse, lookup)?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum GenerationInstruction {
    LimitedRepetition(Box<GenerationInstruction>, usize),
    UnlimitedRepetition(Box<GenerationInstruction>),
    List(Vec<Box<GenerationInstruction>>),
    Options(Vec<Box<GenerationInstruction>>),
    Part(Box<GenerationInstructionInner>),
}

impl Display for GenerationInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                GenerationInstruction::LimitedRepetition(l, c) => {
                    if *c > 1 {
                        format!("{}{}", l, c)
                    } else {
                        format!("({})", l)
                    }
                }
                GenerationInstruction::UnlimitedRepetition(u) => {
                    format!("{}*", u)
                }
                GenerationInstruction::List(l) => {
                    l.iter().map(|v| v.to_string()).collect::<Vec<_>>().join("")
                }
                GenerationInstruction::Options(o) => {
                    format!(
                        "[{}]",
                        o.iter().map(|v| v.to_string()).collect::<Vec<_>>().join("")
                    )
                }
                GenerationInstruction::Part(p) => {
                    p.to_string()
                }
            }
        )
    }
}

const RESERVED: [char; 9] = ['(', ')', '[', ']', '\\', '=', '*', '{', '}'];

impl GenerationInstruction {
    pub fn as_mut_list(&mut self) -> ConGenResult<&mut Vec<Box<GenerationInstruction>>> {
        match self {
            GenerationInstruction::LimitedRepetition(i, count) => {
                if *count == 1 {
                    i.as_mut_list()
                } else {
                    Err(ConGenError::Todo)
                }
            }
            GenerationInstruction::List(l) | GenerationInstruction::Options(l) => Ok(l),
            _ => Err(ConGenError::Todo),
        }
    }

    pub fn add_part(&mut self, part: GenerationInstruction) {
        match self {
            GenerationInstruction::List(l) | GenerationInstruction::Options(l) => {
                l.push(Box::new(part))
            }
            _ => {}
        }
    }

    pub fn add_inner_part(&mut self, part: GenerationInstructionInner) {
        self.add_part(GenerationInstruction::Part(Box::from(part)));
    }

    pub fn try_parse(
        input: &str,
        lookup: &HashMap<String, GenerationInstructionInner>,
    ) -> ConGenResult<Self> {
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
                    let part = GenerationInstruction::LimitedRepetition(part, max_num);
                    last_list.push(Box::new(part));
                    num = String::new();
                }
            }
            let c_str = c.to_string();
            let next = if RESERVED.contains(&c) && !escape {
                match c {
                    '{' => stack.push(GenerationInstruction::List(vec![])),
                    '}' => {
                        if !matches!(
                            stack.last().ok_or(ConGenError::Todo)?,
                            GenerationInstruction::List(_)
                        ) {
                            return Err(ConGenError::Todo);
                        }
                        let part = stack.pop().ok_or(ConGenError::Todo)?;
                        stack.last_mut().ok_or(ConGenError::Todo)?.add_part(part);
                    }
                    '(' => stack.push(GenerationInstruction::List(vec![])),
                    ')' => {
                        if !matches!(
                            stack.last().ok_or(ConGenError::Todo)?,
                            GenerationInstruction::List(_)
                        ) {
                            return Err(ConGenError::Todo);
                        }
                        let part = stack.pop().ok_or(ConGenError::Todo)?;
                        let part = GenerationInstruction::LimitedRepetition(Box::new(part), 1);
                        stack.last_mut().ok_or(ConGenError::Todo)?.add_part(part);
                    }
                    '[' => stack.push(GenerationInstruction::Options(vec![])),
                    ']' => {
                        if !matches!(
                            stack.last().ok_or(ConGenError::Todo)?,
                            GenerationInstruction::Options(_)
                        ) {
                            return Err(ConGenError::Todo);
                        }
                        let part = stack.pop().ok_or(ConGenError::Todo)?;
                        stack.last_mut().ok_or(ConGenError::Todo)?.add_part(part);
                    }
                    '\\' => {
                        escape = true;
                    }
                    '*' => {
                        let last_list = stack.last_mut().ok_or(ConGenError::Todo)?.as_mut_list()?;
                        let part = last_list.pop().ok_or(ConGenError::Todo)?;
                        let part = GenerationInstruction::UnlimitedRepetition(part);
                        last_list.push(Box::new(part));
                    }
                    _ => {}
                }
                continue;
            } else if c.is_numeric() {
                count = true;
                num.push(c);
                continue;
            } else if let Some(val) = lookup.get(&c_str) {
                val.clone()
            } else {
                GenerationInstructionInner::Sound(Sound::custom(c_str))
            };
            escape = false;
            stack
                .last_mut()
                .ok_or(ConGenError::Todo)?
                .add_inner_part(next);
        }

        if count {
            let max_num = num.parse::<usize>()?;
            let last_list = stack.last_mut().ok_or(ConGenError::Todo)?.as_mut_list()?;
            let part = last_list.pop().ok_or(ConGenError::Todo)?;
            let part = GenerationInstruction::LimitedRepetition(part, max_num);
            last_list.push(Box::new(part));
        }
        if stack.len() > 1 {
            return Err(ConGenError::Todo);
        }
        let result = stack.pop().ok_or(ConGenError::Todo)?;
        Ok(result)
    }

    fn generate_repetition<'a, R: Rng>(
        rng: &mut R,
        settings: &GenerationSettings,
        instruction: &'a Box<GenerationInstruction>,
        count: usize,
    ) -> Vec<ShallowGenResult<'a>> {
        (0..count)
            .map(|_| instruction.shallow_generate(rng, settings))
            .flatten()
            .collect::<Vec<_>>()
    }

    pub fn generate_sound<R: Rng>(
        &self,
        rng: &mut R,
        settings: &GenerationSettings,
    ) -> Option<Sound> {
        let shallow = self.shallow_generate(rng, settings);
        for s in shallow {
            match s {
                ShallowGenResult::Sound(s) => {
                    return Some(s.clone());
                }
                ShallowGenResult::Instruction(_) => {}
            }
        }
        None
    }

    pub fn shallow_generate<R: Rng>(
        &self,
        rng: &mut R,
        settings: &GenerationSettings,
    ) -> Vec<ShallowGenResult<'_>> {
        match self {
            GenerationInstruction::LimitedRepetition(i, max) => {
                let count = rng.gen_range(0..(*max + 1));
                Self::generate_repetition(rng, settings, i, count)
            }
            GenerationInstruction::UnlimitedRepetition(i) => {
                let distr = Normal::new(0., 1.).unwrap();
                let num = distr.sample(rng) as f64;
                let count = num.abs() as usize;
                Self::generate_repetition(rng, settings, i, count)
            }
            GenerationInstruction::List(l) => l
                .iter()
                .map(|v| v.shallow_generate(rng, settings))
                .flatten()
                .collect(),
            GenerationInstruction::Options(l) => {
                let choice = if let Some(deviation) = settings.deviation {
                    let deviation = l.len() as f32 * deviation * 0.5;
                    let dist = Normal::new(0., deviation).unwrap();
                    let sample: f32 = dist.sample(rng);
                    let idx = (sample.abs() as usize) % l.len();
                    l.get(idx)
                } else {
                    l.choose(rng)
                }
                .unwrap(); // TODO: Remove unwrap
                choice.shallow_generate(rng, settings)
            }
            GenerationInstruction::Part(p) => {
                vec![p.shallow_generate()]
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum GenerationInstructionInner {
    LowerLevel(Box<GenerationInstructionRoot>),
    Sound(Sound),
}

impl GenerationInstructionInner {
    pub fn shallow_generate(&self) -> ShallowGenResult<'_> {
        use ShallowGenResult::*;
        match self {
            GenerationInstructionInner::LowerLevel(l) => Instruction(l),
            GenerationInstructionInner::Sound(s) => Sound(s),
        }
    }
}

impl Display for GenerationInstructionInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                GenerationInstructionInner::LowerLevel(l) => {
                    l.name.clone().unwrap_or("?".to_string())
                }
                GenerationInstructionInner::Sound(s) => {
                    s.representation.clone()
                }
            }
        )
    }
}

pub enum ShallowGenResult<'a> {
    Sound(&'a Sound),
    Instruction(&'a Box<GenerationInstructionRoot>),
}

pub struct WordGen {
    pub(crate) instructions: Vec<GenerationInstructionRoot>,
}

impl WordGen {
    pub(crate) fn generate<R: Rng>(&self, rng: &mut R, settings: &GenerationSettings) -> Word {
        let i = self.instructions.choose(rng).unwrap(); // TODO: Unwrap
        let shallow = i.shallow_generate(rng, settings);
        let (instructions, syllables): (Vec<String>, Vec<Syllable>) = shallow
            .into_iter()
            .map(|r| match r {
                ShallowGenResult::Sound(s) => (
                    s.representation.clone(),
                    Syllable {
                        instruction: None,
                        sounds: vec![s.clone()],
                    },
                ),
                ShallowGenResult::Instruction(i) => {
                    let shallow = i.shallow_generate(rng, settings);
                    let (instructions, sounds): (Vec<String>, Vec<Sound>) = shallow
                        .into_iter()
                        .filter_map(|v| match v {
                            ShallowGenResult::Sound(s) => {
                                Some((s.representation.clone(), s.clone()))
                            }
                            ShallowGenResult::Instruction(i) => i
                                .instruction
                                .generate_sound(rng, settings)
                                .map(|s| (i.name.clone().unwrap_or("?".to_string()), s)),
                        })
                        .unzip();
                    (
                        i.name.clone().unwrap_or("?".to_string()),
                        Syllable {
                            instruction: Some(instructions.join("")),
                            sounds,
                        },
                    )
                }
            })
            .unzip();
        Word {
            instruction: Some(instructions.join("")),
            syllables,
        }
    }
}
