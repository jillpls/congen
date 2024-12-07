use crate::rewrite::RewriteRule;
use crate::sounds::Sound;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub struct SyllablePart {
    pub(crate) instruction: Option<String>,
    pub(crate) sounds: Vec<Sound>,
}

impl PartialOrd for SyllablePart {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.sounds.partial_cmp(&other.sounds)
    }
}

impl SyllablePart {
    pub fn from_sound(sound: Sound) -> Self {
        Self {
            instruction: None,
            sounds: vec![sound],
        }
    }
    pub fn display(&self, rewrite: bool) -> String {
        self.sounds
            .iter()
            .map(|s| s.display(rewrite))
            .collect::<Vec<_>>()
            .join("")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub struct Syllable {
    pub(crate) instruction: Option<String>,
    pub(crate) onset: Option<SyllablePart>,
    pub(crate) nucleus: SyllablePart,
    pub(crate) coda: Option<SyllablePart>,
    pub simple: bool,
}

impl Syllable {
    pub(crate) fn from_part(syllable_part: SyllablePart) -> Self {
        Self {
            instruction: None,
            onset: None,
            nucleus: syllable_part,
            coda: None,
            simple: true,
        }
    }

    pub(crate) fn from_sound(sound: Sound) -> Self {
        Self::from_part(SyllablePart::from_sound(sound))
    }

    pub(crate) fn display(&self, rewrite: bool) -> String {
        format!(
            "{}{}{}",
            self.onset
                .as_ref()
                .map(|v| v.display(rewrite))
                .unwrap_or_default(),
            self.nucleus.display(rewrite),
            self.coda
                .as_ref()
                .map(|v| v.display(rewrite))
                .unwrap_or_default()
        )
    }

    pub(crate) fn sounds(&self) -> Vec<&Sound> {
        let mut result = vec![];
        if let Some(o) = &self.onset {
            result.append(&mut o.sounds.iter().collect::<Vec<_>>());
        }
        result.append(&mut self.nucleus.sounds.iter().collect::<Vec<_>>());
        if let Some(o) = &self.coda {
            result.append(&mut o.sounds.iter().collect::<Vec<_>>());
        }
        result
    }
}

impl PartialOrd for Syllable {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let ord = match (&self.onset, &other.onset) {
            (Some(o), Some(u)) => o.cmp(u),
            (Some(o), None) => o.cmp(&other.nucleus),
            (None, Some(u)) => self.nucleus.cmp(&u),
            _ => Ordering::Equal,
        };
        if ord != Ordering::Equal {
            return Some(ord);
        }
        let ord = self.nucleus.cmp(&other.nucleus);
        if ord != Ordering::Equal {
            return Some(ord);
        }
        Some(match (&self.coda, &other.coda) {
            (Some(_), None) => Ordering::Less,
            (None, Some(_)) => Ordering::Greater,
            _ => Ordering::Equal,
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, Serialize, Deserialize)]
pub struct Word {
    pub(crate) instruction: Option<String>,
    pub(crate) syllables: Vec<Syllable>,
    pub(crate) rewrite_rules: Vec<RewriteRule>,
}

impl PartialOrd for Word {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.to_string().partial_cmp(&other.to_string())
    }
}

impl Word {
    pub fn export(&self) -> Vec<String> {
        vec![
            self.display(false),
            self.display(true),
            self.instruction.clone().unwrap_or_default(),
        ]
    }

    pub fn display(&self, rewrite: bool) -> String {
        let mut r = self
            .syllables
            .iter()
            .map(|s| s.display(rewrite))
            .collect::<Vec<_>>()
            .join("");
        if rewrite {
            for rule in &self.rewrite_rules {
                r = rule.apply_to_str(&r);
            }
        }
        format!("{}", r)
    }
}

impl Display for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display(false))
    }
}
