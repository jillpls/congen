#[derive(Debug, Default, Eq, PartialEq, Clone, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Diphtong {
    pub first: Sound,
    pub second: Sound,
}

#[derive(Debug, Default, Eq, PartialEq, Clone, Hash, Ord, Serialize, Deserialize)]
pub struct Sound {
    pub(crate) representation: String,
    pub(crate) description: SoundKind,
    pub(crate) rewrite: Option<String>,
    pub complexity: usize,
}

impl Sound {
    pub fn representation(&self) -> &str {
        self.representation.as_str()
    }

    pub fn rewrite(&self) -> Option<&String> {
        self.rewrite.as_ref()
    }

    pub fn display(&self, rewrite: bool) -> &str {
        if rewrite {
            if let Some(r) = &self.rewrite {
                return r.as_str();
            }
        }
        self.representation()
    }
}

impl std::cmp::PartialOrd for Sound {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.representation.partial_cmp(&other.representation)
    }
}

impl Sound {
    pub fn diphtong(first: Sound, second: Sound) -> Self {
        let complexity = (first.complexity + second.complexity) / 2;
        Self {
            representation: format!("{}{}", &first.representation, &second.representation),
            description: SoundKind::Diphtong(Box::new(Diphtong { first, second })),
            rewrite: None,
            complexity,
        }
    }
    pub fn is_consonant(&self) -> bool {
        matches!(self.description, SoundKind::Consonant(_))
    }

    pub fn is_vowel(&self) -> bool {
        matches!(self.description, SoundKind::Vowel(_))
    }

    pub fn description_str(&self) -> String {
        match &self.description {
            SoundKind::Vowel(v) => {
                format!("{} vowel", v.to_string())
            }
            SoundKind::Consonant(c) => {
                format!("{} consonant", c.to_string())
            }
            SoundKind::Custom => {
                format!("custom sound ({})", self.representation)
            }
            SoundKind::Diphtong(d) => {
                format!(
                    "diphtong: {} + {}",
                    d.first.description_str(),
                    d.second.description_str()
                )
            }
        }
    }

    pub fn is_attl(&self) -> bool {
        match &self.description {
            SoundKind::Consonant(c) => {
                c.manners.contains(&Manner::TapFlap)
                    || c.manners.contains(&Manner::Trill)
                    || c.manners.contains(&Manner::Approximant)
                    || c.manners.contains(&Manner::Lateral)
            }
            _ => false,
        }
    }

    pub fn is_lateral(&self) -> bool {
        match &self.description {
            SoundKind::Consonant(c) => c.manners.contains(&Manner::Lateral),
            _ => false,
        }
    }

    pub fn is_fricative_affricate(&self) -> bool {
        match &self.description {
            SoundKind::Consonant(c) => {
                c.manners.contains(&Manner::Fricative) || c.manners.contains(&Manner::Affricate)
            }
            _ => false,
        }
    }

    pub fn is_glottal(&self) -> bool {
        match &self.description {
            SoundKind::Consonant(c) => {
                c.place == Place::Glottal || c.place == Place::PharyngealEppiglotal
            }
            _ => false,
        }
    }

    pub fn is_nasal(&self) -> bool {
        match &self.description {
            SoundKind::Consonant(c) => c.manners.contains(&Manner::Nasal),
            _ => false,
        }
    }

    pub fn is_plosive(&self) -> bool {
        match &self.description {
            SoundKind::Consonant(c) => c.manners.contains(&Manner::Plosive),
            _ => false,
        }
    }

    pub fn custom(representation: String) -> Self {
        Self {
            representation,
            description: SoundKind::Custom,
            complexity: 0,
            ..Default::default()
        }
    }

    pub fn is_voiced_consonant(&self) -> bool {
        match &self.description {
            SoundKind::Consonant(c) => c.voice.map(|v| v == Voice::Voiced).unwrap_or(false),
            _ => false,
        }
    }
}

#[derive(Default, Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Hash, Serialize, Deserialize)]
pub enum SoundKind {
    Vowel(Vowel),
    Diphtong(Box<Diphtong>),
    Consonant(Consonant),
    #[default]
    Custom,
}

use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::io;
pub use vowels::*;

pub fn parse_csv_to_map<R: io::Read>(reader: R) -> Vec<HashMap<String, String>> {
    let mut rdr = csv::Reader::from_reader(reader);
    let mut records = vec![];
    let headers = rdr.headers().unwrap().clone();
    for result in rdr.records() {
        let record = result.unwrap();
        let mut map = HashMap::new();
        for (header, value) in headers.iter().zip(record.iter()) {
            map.insert(header.to_string(), value.to_string());
        }
        records.push(map);
    }
    records
}

mod vowels {
    use crate::sounds::{parse_csv_to_map, Sound, SoundKind};
    use crate::ConGenError::GenericParseError;
    use crate::ConGenResult;
    use serde::{Deserialize, Serialize};
    use std::collections::HashMap;
    use std::fmt::{Display, Formatter};
    use std::io;

    pub fn parse_vowels<R: io::Read>(input: R) -> Vec<Sound> {
        let records = parse_csv_to_map(input);
        let sounds = records
            .into_iter()
            .filter_map(|r| Vowel::try_from_map(r).ok())
            .collect::<Vec<_>>();
        sounds
    }

    #[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Hash, Serialize, Deserialize)]
    pub struct Vowel {
        pub(crate) height: Height,
        pub(crate) backness: Backness,
        pub(crate) roundedness: Roundedness,
    }

    impl Vowel {
        pub fn try_from_map(map: HashMap<String, String>) -> ConGenResult<Sound> {
            let roundedness = Roundedness::try_from(
                map.get("roundedness")
                    .ok_or(GenericParseError("No roundedness defined".to_string()))?
                    .as_str(),
            )
            .map_err(|_| GenericParseError("Could not parse roundedness".to_string()))?;
            let height = Height::try_from(
                map.get("height")
                    .ok_or(GenericParseError("No height defined".to_string()))?
                    .as_str(),
            )
            .map_err(|_| GenericParseError("Could not parse height".to_string()))?;

            let backness = Backness::try_from(
                map.get("backness")
                    .ok_or(GenericParseError("No backness defined".to_string()))?
                    .as_str(),
            )
            .map_err(|_| GenericParseError("Could not parse backness".to_string()))?;
            let representation = map
                .get("symbol")
                .ok_or(GenericParseError("No symbol defined".to_string()))?
                .to_owned();
            let complexity = map
                .get("complexity")
                .map(|c| c.parse::<usize>().ok())
                .flatten()
                .unwrap_or(10);
            Ok(Sound {
                representation,
                description: SoundKind::Vowel(Vowel {
                    height,
                    backness,
                    roundedness,
                }),
                complexity,
                ..Default::default()
            })
        }
    }

    impl Display for Vowel {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} {} {}", self.height, self.backness, self.roundedness)
        }
    }

    #[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Hash, Serialize, Deserialize)]
    pub enum Height {
        Close,
        NearClose,
        CloseMid,
        Mid,
        OpenMid,
        NearOpen,
        Open,
    }

    impl TryFrom<&str> for Height {
        type Error = ();

        fn try_from(value: &str) -> Result<Self, Self::Error> {
            use Height::*;
            Ok(match value.to_ascii_lowercase().as_str() {
                "close" => Close,
                "near-close" => NearClose,
                "close-mid" => CloseMid,
                "mid" => Mid,
                "open-mid" => OpenMid,
                "near-open" => NearOpen,
                "open" => Open,
                _ => return Err(()),
            })
        }
    }

    impl Display for Height {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Height::Close => "close",
                    Height::NearClose => "near-close",
                    Height::CloseMid => "close-mid",
                    Height::Mid => "mid",
                    Height::OpenMid => "open-mid",
                    Height::NearOpen => "near-open",
                    Height::Open => "open",
                }
            )
        }
    }

    #[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Hash, Serialize, Deserialize)]
    pub enum Backness {
        Front,
        NearFront,
        Central,
        NearBack,
        Back,
    }

    impl TryFrom<&str> for Backness {
        type Error = ();

        fn try_from(value: &str) -> Result<Self, Self::Error> {
            use Backness::*;
            Ok(match value.to_ascii_lowercase().as_str() {
                "front" => Front,
                "near-front" => NearFront,
                "central" => Central,
                "near-back" => NearBack,
                "back" => Back,
                _ => return Err(()),
            })
        }
    }

    impl Display for Backness {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Backness::Front => "front",
                    Backness::NearFront => "near-front",
                    Backness::Central => "central",
                    Backness::NearBack => "near-back",
                    Backness::Back => "back",
                }
            )
        }
    }

    #[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Hash, Serialize, Deserialize)]
    pub enum Roundedness {
        Unrounded,
        Rounded,
    }

    impl TryFrom<&str> for Roundedness {
        type Error = ();

        fn try_from(value: &str) -> Result<Self, Self::Error> {
            use Roundedness::*;
            Ok(match value.to_ascii_lowercase().as_str() {
                "rounded" => Rounded,
                "unrounded" => Unrounded,
                _ => return Err(()),
            })
        }
    }

    impl Display for Roundedness {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Roundedness::Rounded => "rounded",
                    Roundedness::Unrounded => "unrounded",
                }
            )
        }
    }
}

pub use consonants::*;

mod consonants {
    use crate::sounds::Manner::Ejective;
    use crate::sounds::{parse_csv_to_map, Sound, SoundKind};
    use crate::ConGenError::GenericParseError;
    use crate::{ConGenError, ConGenResult};
    use itertools::Itertools;
    use serde::{Deserialize, Serialize};
    use smallvec::{smallvec, SmallVec};
    use std::cmp::Ordering;
    use std::collections::HashMap;
    use std::fmt::{Display, Formatter};
    use std::io;
    use strum_macros::EnumIter;

    pub fn parse_consonants<R: io::Read>(input: R) -> Vec<Sound> {
        let records = parse_csv_to_map(input);
        let sounds = records
            .into_iter()
            .filter_map(|r| Consonant::try_from_map(r).ok())
            .collect::<Vec<_>>();
        sounds
    }

    #[derive(PartialEq, Eq, Ord, Hash, Clone, Default, Debug, Serialize, Deserialize)]
    pub struct Manners {
        pub(crate) inner: SmallVec<[Manner; 4]>,
    }

    impl Manners {
        pub fn single(manner: Manner) -> Self {
            Self {
                inner: smallvec![manner],
            }
        }

        pub fn contains(&self, manner: &Manner) -> bool {
            self.inner.contains(manner)
        }
    }

    impl Display for Manners {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", fmt_manners(&self.inner))
        }
    }

    impl PartialOrd for Manners {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            if self.inner != other.inner {
                return partial_cmp_manners(&self.inner, &other.inner);
            } else {
                Some(Ordering::Equal)
            }
        }
    }

    #[derive(Debug, Clone, Eq, Hash, PartialEq, Ord, Serialize, Deserialize)]
    pub struct Consonant {
        pub pulmonic: bool,
        pub co_articulated: bool,
        pub uvular: bool,
        pub(crate) voice: Option<Voice>,
        pub(crate) place: Place,
        pub(crate) manners: Manners,
    }

    impl PartialOrd for Consonant {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            if self.manners != other.manners {
                return self.manners.partial_cmp(&other.manners);
            }
            let place_cmp = self.place.cmp(&other.place);
            if place_cmp != Ordering::Equal {
                return Some(place_cmp);
            }
            self.voice.partial_cmp(&other.voice)
        }
    }

    pub fn partial_cmp_manners(
        a: &SmallVec<[Manner; 4]>,
        b: &SmallVec<[Manner; 4]>,
    ) -> Option<Ordering> {
        use Manner::*;
        use Ordering::*;
        match (a.as_ref(), b.as_ref()) {
            (&[Nasal], _) => Some(Less),
            (_, &[Nasal]) => Some(Greater),
            (&[Plosive], _) => Some(Less),
            (_, &[Plosive]) => Some(Greater),
            (&[Sibilant, Affricate], _) => Some(Less),
            (_, &[Sibilant, Affricate]) => Some(Greater),
            (&[NonSibilant, Affricate], _) => Some(Less),
            (_, &[NonSibilant, Affricate]) => Some(Greater),
            (&[Sibilant, Fricative], _) => Some(Less),
            (_, &[Sibilant, Fricative]) => Some(Greater),
            (&[NonSibilant, Fricative], _) => Some(Less),
            (_, &[NonSibilant, Fricative]) => Some(Greater),
            (&[Approximant], _) => Some(Less),
            (_, &[Approximant]) => Some(Greater),
            (&[TapFlap], _) => Some(Less),
            (_, &[TapFlap]) => Some(Greater),
            (&[Trill], _) => Some(Less),
            (_, &[Trill]) => Some(Greater),
            (&[Lateral, Affricate], _) => Some(Less),
            (_, &[Lateral, Affricate]) => Some(Greater),
            (&[Lateral, Fricative], _) => Some(Less),
            (_, &[Lateral, Fricative]) => Some(Greater),
            (&[Lateral, Approximant], _) => Some(Less),
            (_, &[Lateral, Approximant]) => Some(Greater),
            (&[Lateral, TapFlap], _) => Some(Less),
            (_, &[Lateral, TapFlap]) => Some(Greater),
            _ => Some(Equal),
        }
    }

    pub fn contains_order(a: &Manners, b: &Manners, to_check: Manner) -> Option<Ordering> {
        let a = a.contains(&to_check);
        let b = b.contains(&to_check);
        match (a, b) {
            (true, false) => Some(Ordering::Less),
            (false, true) => Some(Ordering::Greater),
            _ => None,
        }
    }

    pub fn cmp_non_pulmonic(a: &Manners, b: &Manners) -> Ordering {
        match (a.contains(&Ejective), b.contains(&Ejective)) {
            (true, false) => return Ordering::Less,
            (false, true) => return Ordering::Greater,
            _ => (),
        }
        match (a.contains(&Manner::Click), b.contains(&Manner::Click)) {
            (true, false) => return Ordering::Less,
            (false, true) => return Ordering::Greater,
            _ => (),
        }

        if let Some(o) = contains_order(a, b, Manner::Stop) {
            return o;
        }
        if let Some(o) = contains_order(b, a, Manner::Lateral) {
            return o;
        }
        if let Some(o) = contains_order(a, b, Manner::Affricate) {
            return o;
        }
        if let Some(o) = contains_order(a, b, Manner::Fricative) {
            return o;
        }
        if let Some(o) = contains_order(a, b, Manner::Tenuis) {
            return o;
        }
        if let Some(o) = contains_order(a, b, Manner::Voiced) {
            return o;
        }
        if let Some(o) = contains_order(a, b, Manner::Nasal) {
            return o;
        }

        Ordering::Equal
    }

    impl Consonant {
        pub fn try_from_map(map: HashMap<String, String>) -> ConGenResult<Sound> {
            let voice = map
                .get("voice")
                .map(|r| Voice::try_from(r.as_str()).ok())
                .flatten();
            let place = Place::try_from(
                map.get("place")
                    .ok_or(ConGenError::GenericParseError(
                        "No place defined".to_string(),
                    ))?
                    .as_str(),
            )
            .map_err(|_| ConGenError::GenericParseError("could not find place".to_string()))?;
            let manners = if let Some(manners) = map.get("manner") {
                let split = manners.split_whitespace().collect::<Vec<_>>();
                split
                    .iter()
                    .filter_map(|v| Manner::try_from(*v).ok())
                    .sorted()
                    .collect::<SmallVec<[Manner; 4]>>()
            } else {
                return Err(ConGenError::GenericParseError(
                    "No manners defined".to_string(),
                ));
            };
            let consonant = Self {
                pulmonic: true,
                co_articulated: false,
                uvular: false,
                voice,
                place,
                manners: Manners { inner: manners },
            };
            let representation = map
                .get("symbol")
                .ok_or(GenericParseError("No symbol defined".to_string()))?
                .to_owned()
                .trim()
                .to_string();
            let complexity = map
                .get("complexity")
                .map(|c| c.parse::<usize>().ok())
                .flatten()
                .unwrap_or(10);
            Ok(Sound {
                representation,
                description: SoundKind::Consonant(consonant),
                complexity,
                ..Default::default()
            })
        }

        pub fn new_base(place: Place, manner: Manner) -> Self {
            Self {
                pulmonic: true,
                co_articulated: false,
                place,
                manners: Manners::single(manner),
                voice: None,
                uvular: false,
            }
        }

        pub fn with_manner(mut self, manner: Manner) -> Self {
            self.manners.inner.push(manner);
            self.manners.inner.sort();
            self
        }

        pub fn with_voice(mut self, voice: Voice) -> Self {
            self.voice = Some(voice);
            self
        }

        pub fn new(place: Place, manner: &[Manner], voice: Option<Voice>) -> Self {
            Self {
                pulmonic: true,
                co_articulated: false,
                place,
                manners: Manners {
                    inner: manner.into(),
                },
                voice,
                uvular: false,
            }
        }
    }

    pub fn fmt_manners(manners: &[Manner]) -> String {
        manners
            .iter()
            .map(|m| m.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    }

    impl std::fmt::Display for Consonant {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let voice = if let Some(v) = self.voice {
                format!("{} ", v)
            } else {
                String::new()
            };
            let place = format!("{} ", self.place);
            write!(f, "{}{}{}", voice, place, &self.manners)
        }
    }

    #[derive(
        Debug, Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Hash, EnumIter, Serialize, Deserialize,
    )]
    pub enum Place {
        Bilabial,
        Labiodental,
        Linguolabial,
        Dental,
        Alveolar,
        Postalveolar,
        Retroflex,
        Palatal,
        Velar,
        Uvular,
        PharyngealEppiglotal,
        Glottal,
        LabialAlveolar,
        LabialRetroflex,
        LabialPalatal,
        LabialVelar,
        LabialUvular,
        PalatalVelar,
        UvularEpiglottal,
        VelarizedAlveolar,
    }

    #[derive(
        Debug, Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Hash, EnumIter, Serialize, Deserialize,
    )]
    pub enum Voice {
        Voiceless,
        Voiced,
    }

    impl TryFrom<&str> for Voice {
        type Error = ();

        fn try_from(value: &str) -> Result<Self, Self::Error> {
            Ok(match value.to_ascii_lowercase().as_str() {
                "voiced" => Self::Voiced,
                "voiceless" => Self::Voiceless,
                _ => return Err(()),
            })
        }
    }

    impl std::fmt::Display for Voice {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Voice::Voiced => {
                        "voiced"
                    }
                    Voice::Voiceless => {
                        "voiceless"
                    }
                }
            )
        }
    }

    impl std::fmt::Display for Place {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Place::Bilabial => {
                        "bilabial"
                    }
                    Place::Labiodental => {
                        "labiodental"
                    }
                    Place::Linguolabial => {
                        "linguolabial"
                    }
                    Place::Dental => {
                        "dental"
                    }
                    Place::Alveolar => {
                        "alveolar"
                    }
                    Place::Postalveolar => {
                        "postalveolar"
                    }
                    Place::Retroflex => {
                        "retroflex"
                    }
                    Place::Palatal => {
                        "palatal"
                    }
                    Place::Velar => {
                        "velar"
                    }
                    Place::Uvular => {
                        "uvular"
                    }
                    Place::PharyngealEppiglotal => {
                        "pharyngeal/epiglottal"
                    }
                    Place::Glottal => {
                        "glottal"
                    }
                    Place::LabialVelar => {
                        "labial-velar"
                    }
                    Place::LabialAlveolar => {
                        "labial-alveolar"
                    }
                    Place::LabialRetroflex => {
                        "labial-retroflex"
                    }
                    Place::LabialPalatal => {
                        "labial-palatal"
                    }
                    Place::LabialUvular => {
                        "labial-uvular"
                    }
                    Place::PalatalVelar => {
                        "palatal-velar"
                    }
                    Place::UvularEpiglottal => {
                        "uvular-epiglottal"
                    }
                    Place::VelarizedAlveolar => {
                        "velarized alveolar"
                    }
                }
            )
        }
    }

    impl TryFrom<&str> for Place {
        type Error = ();

        fn try_from(value: &str) -> Result<Self, Self::Error> {
            Ok(match value.to_ascii_lowercase().as_str() {
                "bilabial" => Self::Bilabial,
                "labiodental" => Self::Labiodental,
                "dental" => Self::Dental,
                "alveolar" => Self::Alveolar,
                "postalveolar" => Self::Postalveolar,
                "retroflex" => Self::Retroflex,
                "palatal" => Self::Palatal,
                "velar" => Self::Velar,
                "uvular" => Self::Uvular,
                "pharyngeal/epiglottal" => Self::PharyngealEppiglotal,
                "glottal" => Self::Glottal,
                "labial-velar" => Self::LabialVelar,
                "labial-uvular" => Self::LabialUvular,
                "labial-retroflex" => Self::LabialRetroflex,
                "labial-palatal" => Self::LabialPalatal,
                "labial-alveolar" => Self::LabialAlveolar,
                "uvular-epiglottal" => Self::UvularEpiglottal,
                "palatal-velar" => Self::PalatalVelar,
                "velarized alveolar" => Self::VelarizedAlveolar,
                _ => return Err(()),
            })
        }
    }

    #[derive(
        Debug, Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Hash, EnumIter, Serialize, Deserialize,
    )]
    pub enum Manner {
        Voiced,
        Lateral,
        Sibilant,
        NonSibilant,
        Nasal,
        Ejective,
        Click,
        Plosive,
        Implosive,
        Affricate,
        Fricative,
        Approximant,
        TapFlap,
        Trill,
        Tenuis,
        Stop,
        Uvular,
    }

    impl std::fmt::Display for Manner {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Manner::Nasal => {
                        "nasal"
                    }
                    Manner::Plosive => {
                        "plosive"
                    }
                    Manner::Affricate => {
                        "affricate"
                    }
                    Manner::Fricative => {
                        "fricative"
                    }
                    Manner::Approximant => {
                        "approximant"
                    }
                    Manner::TapFlap => {
                        "tap/flap"
                    }
                    Manner::Trill => {
                        "trill"
                    }
                    Manner::Lateral => {
                        "lateral"
                    }
                    Manner::Sibilant => {
                        "sibilant"
                    }
                    Manner::NonSibilant => {
                        "non-sibilant"
                    }
                    Manner::Ejective => {
                        "ejective"
                    }
                    Manner::Click => {
                        "click"
                    }
                    Manner::Tenuis => {
                        "tenuis"
                    }
                    Manner::Implosive => {
                        "implosive"
                    }
                    Manner::Stop => {
                        "stop"
                    }
                    Manner::Uvular => {
                        "uvular"
                    }
                    _ => {
                        "voiced"
                    }
                }
            )
        }
    }

    impl TryFrom<&str> for Manner {
        type Error = ();

        fn try_from(value: &str) -> Result<Self, Self::Error> {
            Ok(match value.to_ascii_lowercase().as_str() {
                "nasal" => Manner::Nasal,
                "plosive" => Manner::Plosive,
                "affricate" => Manner::Affricate,
                "fricative" => Manner::Fricative,
                "approximant" => Manner::Approximant,
                "tap/flap" => Manner::TapFlap,
                "trill" => Manner::Trill,
                "lateral" => Manner::Lateral,
                "sibilant" => Manner::Sibilant,
                "non-sibilant" => Manner::NonSibilant,
                "ejective" => Manner::Ejective,
                "click" => Manner::Click,
                "tenuis" => Manner::Tenuis,
                "implosive" => Manner::Implosive,
                "stop" => Manner::Stop,
                "uvular" => Manner::Uvular,
                "voiced" => Manner::Voiced,
                _ => return Err(()),
            })
        }
    }
}
