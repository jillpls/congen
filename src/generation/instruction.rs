// Sounds
// Categories
// SyllableParts
// Syllables
// Words

use crate::sounds::Sound;
use crate::word::{Syllable, SyllablePart};
use crate::{ConGenError, ConGenResult};
use rand::prelude::SliceRandom;
use rand::Rng;
use rand_distr::Distribution;
use rand_distr::Normal;
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Copy, Clone)]
pub struct GenerationSettings {
    pub(crate) deviation: Option<f32>,
}

pub type SoundMap = HashMap<Uuid, Sound>;

#[derive(Debug, Clone, Default)]
pub struct SyllableInstruction {
    pub(crate) onset: Option<String>,
    pub(crate) nucleus: String,
    pub(crate) coda: Option<String>,
}

#[derive(Debug, Clone, Default)]
pub struct WordGen {
    pub(crate) sounds: SoundMap,
    pub(crate) rep_sounds: HashMap<String, Uuid>,
    pub(crate) categories: HashMap<String, Vec<GenerationInstruction<Uuid>>>,
    pub(crate) syllable_parts: HashMap<String, Vec<GenerationInstruction<String>>>,
    pub(crate) syllables: HashMap<String, Vec<SyllableInstruction>>,
    pub(crate) words: Vec<GenerationInstruction<String>>,
}

impl WordGen {
    pub fn new() -> Self {
        Self {
            sounds: Default::default(),
            rep_sounds: Default::default(),
            categories: Default::default(),
            syllable_parts: Default::default(),
            syllables: Default::default(),
            words: Default::default(),
        }
    }
}

impl WordGen {
    pub fn generate_word<R: Rng>(
        &self,
        rng: &mut R,
        settings: &GenerationSettings,
    ) -> Option<crate::word::Word> {
        if let Some(w) = choose_with_deviation(&self.words, settings.deviation, rng) {
            let proto = w.generate(rng, settings);
            let r = proto
                .into_iter()
                .filter_map(|r| match r {
                    GenerationResult::Inner(i) => self.generate_syllable(&i, rng, settings),
                    GenerationResult::Sound(s) => self
                        .sounds
                        .get(&s)
                        .cloned()
                        .map(|s| Syllable::from_sound(s)),
                    GenerationResult::CustomSound(c) => Some(Syllable::from_sound(c)),
                })
                .collect::<Vec<_>>();
            Some(crate::word::Word {
                instruction: Some(w.format_str(&self.sounds)),
                syllables: r,
                rewrite_rules: vec![],
            })
        } else {
            None
        }
    }

    fn generate_syllable<R: Rng>(
        &self,
        key: &str,
        rng: &mut R,
        settings: &GenerationSettings,
    ) -> Option<Syllable> {
        if let Some(s) = self
            .syllables
            .get(key)
            .and_then(|s| choose_with_deviation(s, settings.deviation, rng))
        {
            let onset = s
                .onset
                .as_ref()
                .and_then(|o| self.generate_syllable_part(&o, rng, settings)); // TODO: Handle Inner Failure
            let nucleus = self.generate_syllable_part(&s.nucleus, rng, settings)?;
            let coda = s
                .coda
                .as_ref()
                .and_then(|o| self.generate_syllable_part(&o, rng, settings)); // TODO: Handle Inner Failure
            Some(Syllable {
                instruction: Some(format!(
                    "{}{}{}",
                    s.onset.clone().unwrap_or_default(),
                    &s.nucleus,
                    s.coda.clone().unwrap_or_default()
                )),
                onset,
                nucleus,
                coda,
                simple: false,
            })
        } else {
            None
        }
    }

    fn generate_syllable_part<R: Rng>(
        &self,
        key: &str,
        rng: &mut R,
        settings: &GenerationSettings,
    ) -> Option<SyllablePart> {
        self.syllable_parts
            .get(key)
            .and_then(|p| choose_with_deviation(p, settings.deviation, rng))
            .map(|p| {
                let sounds = p
                    .generate(rng, settings)
                    .into_iter()
                    .filter_map(|r| {
                        // TODO: Handle inner None
                        match r {
                            GenerationResult::Inner(i) => self.generate_sound(&i, rng, settings),
                            GenerationResult::Sound(s) => self.sounds.get(&s).cloned(),
                            GenerationResult::CustomSound(s) => Some(s),
                        }
                    })
                    .collect::<Vec<_>>();
                SyllablePart {
                    instruction: Some(p.format_str(&self.sounds)),
                    sounds,
                }
            })
    }

    fn generate_sound<R: Rng>(
        &self,
        key: &str,
        rng: &mut R,
        settings: &GenerationSettings,
    ) -> Option<Sound> {
        self.categories
            .get(key)
            .and_then(|c| choose_with_deviation(c, settings.deviation, rng))
            .map(|c| {
                let mut r = c.generate(rng, settings);
                if r.len() != 1 {
                    None
                } else {
                    let r = r.pop().unwrap();
                    match r {
                        GenerationResult::Inner(id) | GenerationResult::Sound(id) => {
                            self.sounds.get(&id).cloned()
                        }
                        GenerationResult::CustomSound(s) => Some(s),
                    }
                }
            })?
    }
}

fn choose_with_deviation<'a, T, R: Rng>(
    from: &'a [T],
    deviation: Option<f32>,
    rng: &mut R,
) -> Option<&'a T> {
    if let Some(deviation) = deviation {
        let mut dist = Normal::new(0., deviation).unwrap();
        let sample: f32 = dist.sample(rng);
        let idx = (sample.abs() as usize) % from.len();
        from.get(idx)
    } else {
        from.choose(rng)
    }
}

#[derive(Debug, Clone)]
pub enum GenerationInstruction<T> {
    /// inner, (min, max), same
    Repetition(Box<GenerationInstruction<T>>, (usize, usize), bool),
    List(Vec<Box<GenerationInstruction<T>>>),
    Options(Vec<Box<GenerationInstruction<T>>>),
    Inner(T),
    Sound(Uuid),
    Literal(String),
}

#[derive(Clone)]
pub enum GenerationResult<T: Clone> {
    Inner(T),
    Sound(Uuid),
    CustomSound(Sound),
}

impl<T: Clone + std::fmt::Display> GenerationInstruction<T> {
    pub fn format_str(&self, sounds: &SoundMap) -> String {
        format!(
            "{}",
            match self {
                GenerationInstruction::Repetition(r, (min, max), b) => {
                    if *min == 0 {
                        format!("{}*", r.format_str(sounds))
                    } else {
                        let r = format!("{}{}", r.format_str(sounds), max);
                        if *b {
                            format!("{}!", r)
                        } else {
                            r
                        }
                    }
                }
                GenerationInstruction::List(l) => {
                    format!(
                        "{{{}}}",
                        l.iter()
                            .map(|i| i.format_str(sounds))
                            .collect::<Vec<_>>()
                            .join("")
                    )
                }
                GenerationInstruction::Options(l) => {
                    format!(
                        "[{}]",
                        l.iter()
                            .map(|i| i.format_str(sounds))
                            .collect::<Vec<_>>()
                            .join("")
                    )
                }
                GenerationInstruction::Inner(i) => {
                    i.to_string()
                }
                GenerationInstruction::Sound(id) => {
                    sounds
                        .get(id)
                        .map(|s| s.representation().to_string())
                        .unwrap_or("?".to_string())
                }
                GenerationInstruction::Literal(l) => {
                    l.clone()
                }
            }
        )
    }
}

impl<T: Clone> GenerationInstruction<T> {
    pub fn to_first(self) -> Box<Self> {
        match self {
            GenerationInstruction::List(l) | GenerationInstruction::Options(l) => {
                Box::from(l[0].clone())
            }
            _ => Box::new(self),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            GenerationInstruction::List(l) | GenerationInstruction::Options(l) => l.len(),
            _ => 1,
        }
    }

    pub fn generate<R: Rng>(
        &self,
        r: &mut R,
        settings: &GenerationSettings,
    ) -> Vec<GenerationResult<T>> {
        match self {
            GenerationInstruction::Repetition(i, (min, max), same) => {
                let amount = r.gen_range(*min..(max + 1));
                if *same {
                    let res = i.generate(r, settings);
                    vec![res.clone(); amount].into_iter().flatten().collect()
                } else {
                    (0..amount)
                        .map(|_| i.generate(r, settings))
                        .flatten()
                        .collect()
                }
            }
            GenerationInstruction::List(l) => l
                .iter()
                .map(|i| i.generate(r, settings))
                .flatten()
                .collect::<_>(),
            GenerationInstruction::Options(o) => choose_with_deviation(o, settings.deviation, r)
                .map(|res| res.generate(r, settings))
                .unwrap_or_default(),
            GenerationInstruction::Inner(i) => {
                vec![GenerationResult::Inner(i.clone())]
            }
            GenerationInstruction::Sound(s) => {
                vec![GenerationResult::Sound(*s)]
            }
            GenerationInstruction::Literal(s) => {
                vec![GenerationResult::CustomSound(Sound::custom(s.to_string()))]
            }
        }
    }

    pub fn as_mut_list(&mut self) -> ConGenResult<&mut Vec<Box<GenerationInstruction<T>>>> {
        match self {
            GenerationInstruction::Repetition(i, count, _) => {
                if count.1 == 1 {
                    i.as_mut_list()
                } else {
                    Err(ConGenError::Todo)
                }
            }
            GenerationInstruction::List(l) | GenerationInstruction::Options(l) => Ok(l),
            _ => Err(ConGenError::Todo),
        }
    }

    pub fn add_part(&mut self, part: GenerationInstruction<T>) {
        match self {
            GenerationInstruction::List(l) | GenerationInstruction::Options(l) => {
                l.push(Box::new(part))
            }
            _ => {}
        }
    }
}
