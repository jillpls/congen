// TODO: Generate Syllables as Onset Nucleus Coda

use crate::app::Categories;
use crate::sounds::Sound;
use crate::word::{Syllable, SyllablePart, Word};
use crate::{ConGenError, ConGenResult};
use rand::prelude::SliceRandom;
use rand::Rng;
use rand_distr::{Distribution, Normal};
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

#[derive(Debug, Clone, Copy)]
pub enum GenerationResultType {
    Sound,
    SyllablePart,
    Syllable,
    Word,
    Unknown
}

#[derive(Debug, Clone)]
pub enum GenerationResult {
    Sound(Sound),
    SyllablePart(SyllablePart),
    Syllable(Syllable),
    Word(Word)
}

impl Display for GenerationResultType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            GenerationResultType::Sound => { "Sound"}
            GenerationResultType::SyllablePart => { "SyllablePart"}
            GenerationResultType::Syllable => { "Syllable"}
            GenerationResultType::Word => { "Word"}
            GenerationResultType::Unknown => { "Unknown"}
        })
    }
}

impl GenerationResultType {
    fn get_lower(&self) -> Self {
        match self {
            GenerationResultType::Sound => { Self::Sound }
            GenerationResultType::SyllablePart => { Self::Sound}
            GenerationResultType::Syllable => { Self::SyllablePart }
            GenerationResultType::Word => { Self::Syllable }
            GenerationResultType::Unknown => { Self::Unknown }
        }
    }
}

#[derive(Debug, Clone)]
pub struct GenerationInstructionRoot {
    pub(crate) name: Option<String>,
    pub(crate) instruction: GenerationInstruction,
    pub(crate) result_type : GenerationResultType
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
    result_type: GenerationResultType
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
                    result_type,
                })),
            )
        })
        .collect::<_>()
}

impl GenerationInstructionRoot {
    pub fn gen_str(&self, depth: usize) -> String {
        let str = self.instruction.gen_str(depth);
        format!("\n{}{}", vec![" "; depth*2].join(""), str)
    }

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
                result_type: GenerationResultType::Sound,
            })
        }
        result
    }

    pub fn parse_all(
        input: &str,
        lookup: &HashMap<String, GenerationInstructionInner>,
        result_type: GenerationResultType
    ) -> Vec<Self> {
        let sets: Vec<_> = input.split("\n").map(|i| i.split(';')).flatten().collect();
        sets.into_iter()
            .filter_map(|s| Self::try_parse(s, lookup, result_type).ok())
            .collect()
    }

    pub(crate) fn parse_all_name_value(input: &Vec<(String, String)>, lookup: &HashMap<String, GenerationInstructionInner>, result_type: GenerationResultType) -> Vec<Self> {
        input.iter().filter_map(|(n, v)| {
            GenerationInstructionRoot::try_parse_name_value(Some(n.clone()), v, lookup, result_type).ok()
        }).collect::<Vec<_>>()
    }

    pub fn try_parse_name_value(name: Option<String>, value: &str, lookup: &HashMap<String, GenerationInstructionInner>, result_type: GenerationResultType) -> ConGenResult<Self> {

        Ok(Self {
            name,
            instruction: GenerationInstruction::try_parse(value, lookup)?,
            result_type
        })
    }

    pub fn try_parse(
        input: &str,
        lookup: &HashMap<String, GenerationInstructionInner>,
        result_type: GenerationResultType
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
        Self::try_parse_name_value(name, to_parse, lookup, result_type)
    }
}

#[derive(Debug, Clone)]
pub enum GenerationInstruction {
    LimitedRepetition(Box<GenerationInstruction>, usize),
    UnlimitedRepetition(Box<GenerationInstruction>),
    List(Vec<Box<GenerationInstruction>>),
    Options(Vec<Box<GenerationInstruction>>),
    Part(Box<GenerationInstructionInner>),
    Syllable(
        Option<Box<GenerationInstructionRoot>>,
        Box<GenerationInstructionRoot>,
        Option<Box<GenerationInstructionRoot>>,
    ),
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
                GenerationInstruction::Syllable(o, n, c) => {
                    format!(
                        "<{}-{}-{}>",
                        o.as_ref().map(|v| v.to_string()).unwrap_or_default(),
                        n,
                        c.as_ref().map(|v| v.to_string()).unwrap_or_default()
                    )
                }
            }
        )
    }
}

const RESERVED: [char; 9] = ['(', ')', '[', ']', '\\', '=', '*', '{', '}'];

impl GenerationInstruction {
    pub fn gen_str(&self, depth: usize) -> String {
        match self {
            GenerationInstruction::Part(p) => {
                p.gen_str(depth)
            }
            _ => { self.to_string() }
        }
    }

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
                _ => {}
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
            GenerationInstruction::Syllable(o, n, c) => {
                vec![ShallowGenResult::Syllable(o.as_ref(), n, c.as_ref())]
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
    pub fn gen_str(&self, depth: usize) -> String {
        match self {
            GenerationInstructionInner::LowerLevel(l) => {
                l.gen_str(depth+1)
            }
            _ => { self.to_string() }
        }
    }
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
                    s.representation().to_string()
                }
            }
        )
    }
}

#[derive(Debug)]
pub enum ShallowGenResult<'a> {
    Sound(&'a Sound),
    Instruction(&'a Box<GenerationInstructionRoot>),
    Syllable(
        Option<&'a Box<GenerationInstructionRoot>>,
        &'a Box<GenerationInstructionRoot>,
        Option<&'a Box<GenerationInstructionRoot>>,
    ),
}

impl Display for ShallowGenResult<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ShallowGenResult::Sound(_) => { "Sound" }
            ShallowGenResult::Instruction(_) => { "Instruction"}
            ShallowGenResult::Syllable(_, _, _) => { "Syllable" }
        })
    }
}

#[derive(Debug, Clone)]
pub struct WordGen {
    pub(crate) instructions: Vec<GenerationInstructionRoot>,
}

impl WordGen {
    fn generate_sound<R: Rng>(rng: &mut R, settings: &GenerationSettings, shallow_gen_result: &ShallowGenResult) -> Sound {
        match shallow_gen_result {
            ShallowGenResult::Sound(s) => { (*s).clone()}
            ShallowGenResult::Instruction(i) => {
                i.instruction.generate_sound(rng, settings).unwrap() // TODO
            }
            ShallowGenResult::Syllable(_, _, _) => {
                todo!()
            }
        }
    }

    fn generate_syllable_part_option<R: Rng>(rng: &mut R, settings: &GenerationSettings, instruction: &Option<&Box<GenerationInstructionRoot>>) -> Option<SyllablePart> {
        instruction.and_then(|i| {
            let part = Self::generate_syllable_part(rng, settings, i);
            if part.sounds.is_empty() {
                None
            } else {
                Some(part)
            }
        })
    }

    fn generate_syllable_part<R: Rng>(rng: &mut R, settings: &GenerationSettings, instruction: &GenerationInstructionRoot) -> SyllablePart {
        let shallow = instruction.shallow_generate(rng, settings);
        let mut sounds = vec![];
        for r in shallow {
            match &r {
                ShallowGenResult::Sound(_) => { sounds.push(Self::generate_sound(rng, settings, &r));}
                ShallowGenResult::Instruction(i) => {
                    match i.result_type {
                        GenerationResultType::Sound => { sounds.push(Self::generate_sound(rng, settings, &r)) }
                        GenerationResultType::SyllablePart => { let part = Self::generate_syllable_part(rng, settings, i);
                            return part;
                        }
                        _ => {todo!() }
                    }
                }
                ShallowGenResult::Syllable(_, _, _) => { todo!() }
            }
        }
        SyllablePart {
            instruction: Some(instruction.to_string()),
            sounds,
        }
    }

    fn generate_syllable<R: Rng>(rng: &mut R, settings: &GenerationSettings, shallow_gen_result: &ShallowGenResult) -> Syllable {
        match shallow_gen_result {
            ShallowGenResult::Sound(s) => {
                Syllable::from_sound((*s).clone())
            }
            ShallowGenResult::Instruction(i) => {
                let shallow = i.shallow_generate(rng, settings);

                match &i.result_type {
                    GenerationResultType::Sound => {
                        let sounds = shallow.into_iter().map(|i|
                        Self::generate_sound(rng, settings, &i)
                        ).collect();
                        Syllable::from_part(SyllablePart { instruction: Some(i.to_string()), sounds })
                    }
                    GenerationResultType::Syllable => {
                        if shallow.len() != 1 {
                            todo!()
                        }
                        Self::generate_syllable(rng, settings, &shallow[0])
                    }
                    _ => { todo!() }
                }

            }
            ShallowGenResult::Syllable(o, n, c) => {
                let onset = Self::generate_syllable_part_option(rng, settings, o);
                let nucleus= Self::generate_syllable_part(rng, settings, n);
                let coda = Self::generate_syllable_part_option(rng, settings, c);
                let instruction = vec![onset.as_ref(), Some(&nucleus), coda.as_ref()]
                    .into_iter()
                    .filter_map(|v| v.and_then(|p| p.instruction.as_ref().map(|v| v.to_string()))).collect::<Vec<_>>().join("-");
                Syllable {
                    instruction: Some(instruction),
                    onset,
                    nucleus,
                    coda,
                    simple: false,
                }
            }

        }
    }

    fn generate_syllables<R: Rng>(rng: &mut R, settings: &GenerationSettings, shallow_gen_results: &[ShallowGenResult]) -> Vec<Syllable> {
        shallow_gen_results.iter().map(|r| {
            Self::generate_syllable(rng, settings, r)
        }).collect()
    }

    pub(crate) fn generate<R: Rng>(&self, rng: &mut R, settings: &GenerationSettings) -> Word {
        let i = self.instructions.choose(rng).unwrap(); // TODO: Unwrap
        let shallow = i.shallow_generate(rng, settings);
        let syllables = Self::generate_syllables(rng, settings, &shallow);
        Word {
            instruction: Some(i.to_string()),
            syllables,
        }
    }
}
