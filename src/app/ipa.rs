use crate::app::{extract_sound_by_representation, SharedData};
use crate::app::{Categories, SubApp};
use crate::sounds::{cmp_non_pulmonic, parse_consonants, Consonant, Manner, Place, Sound, SoundKind, Diphtong};
use crate::sounds::{
    parse_vowels, partial_cmp_manners, Backness, Height, Manners, Roundedness, Voice,
};
use eframe::emath::Vec2;
use egui::{Context, Ui};
use egui::{Order, ScrollArea};
use egui_extras::{Column, Size, StripBuilder};
use egui_extras::{Strip, TableBuilder};
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::arch::x86_64::__cpuid;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use uuid::Uuid;

#[derive(Serialize, Deserialize)]
pub struct IpaSave {
    pub(crate) app: IpaApp,
}

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
struct SelectedSounds {
    map: HashMap<Uuid, bool>,
    last_changed: VecDeque<Uuid>,
    changed: bool,
}

impl From<&HashMap<Uuid, Sound>> for SelectedSounds {
    fn from(value: &HashMap<Uuid, Sound>) -> Self {
        let map = value
            .iter()
            .map(|(id, s)| (*id, s.complexity <= 1))
            .collect();
        Self {
            map,
            last_changed: vec![].into(),
            changed: false,
        }
    }
}

impl SelectedSounds {
    pub fn is_selected(&self, id: &Uuid) -> bool {
        self.map.get(id).copied().unwrap_or(false)
    }

    pub fn deselected(mut self) -> Self {
        self.deselect_in_place();
        self
    }

    fn deselect_in_place(&mut self) {
        self.map.iter_mut().for_each(|(_,b)| *b = false);
    }

    pub fn only_last_changed(&mut self) {
        self.deselect_in_place();
        for id in &self.last_changed {
            self.map.insert(*id, true);
        }
    }

    pub fn switch(&mut self, id: &Uuid) {
        if let Some(v) = self.map.get_mut(id) {
            *v = !*v;
            self.changed = true;
            if self.last_changed.len() >= 2 {
                self.last_changed.pop_front();
            }
            self.last_changed.push_back(*id);
        }
    }
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct Consonants {
    pulmonic: ConsonantLookups,
    non_pulmonic: ConsonantLookups,
    co_articulated: (Vec<(String, Vec<Uuid>)>, Vec<(String, Vec<Uuid>)>),
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct ConsonantLookups {
    places: Vec<Place>,
    by_manners: HashMap<Manners, HashMap<Place, Vec<Uuid>>>,
    sorted_manners: Vec<Manners>,
    empty_rows: HashMap<Manners, bool>,
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct VowelLookups {
    backness: Vec<Backness>,
    by_height: HashMap<Height, HashMap<Backness, Vec<Uuid>>>,
    sorted_height: Vec<Height>,
    empty_rows: HashMap<Height, bool>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct IpaApp {
    sounds: HashMap<Uuid, Sound>,
    selected: SelectedSounds,
    selected_diphtongs: SelectedSounds,
    diphtongs: HashSet<Uuid>,
    consonants: Consonants,
    vowel_lookups: VowelLookups,
    settings: IpaAppSettings,
    table: String,
    tabs: bool
}

impl Default for IpaApp {
    fn default() -> Self {
        Self {
            sounds: Default::default(),
            selected: Default::default(),
            selected_diphtongs: Default::default(),
            diphtongs: Default::default(),
            consonants: Default::default(),
            vowel_lookups: Default::default(),
            settings: Default::default(),
            table: "Consonants".to_string(),
            tabs: true
        }
    }
}

fn filter_deref_vec<'a, F: FnMut(&(&Uuid, &Sound)) -> bool>(
    it: impl Iterator<Item = &'a Uuid>,
    filter: F,
    sounds: &HashMap<Uuid, Sound>,
) -> Vec<Uuid> {
    it.filter_map(|id| sounds.get(id).map(|v| (id, v)))
        .filter(filter)
        .map(|(v, _)| *v)
        .collect()
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct IpaAppSettings {
    lock_all_tables: bool,
    locked_tables: HashSet<String>,
    editing_tables: HashSet<String>,
    hide_unselected: HashSet<String>,
}

impl IpaApp {
    fn update_categories(&mut self, shared_data: &mut SharedData) {
        let sounds = self
            .selected
            .map
            .iter()
            .filter_map(|(id, b)| {
                if *b && self.sounds.get(id).is_some() {
                    Some(*id)
                } else {
                    None
                }
            })
            .sorted_by(|a, b| self.sounds.get(a).unwrap().cmp(self.sounds.get(b).unwrap()))
            .collect::<Vec<_>>();
        let consonants = filter_deref_vec(sounds.iter(), |(_, s)| s.is_consonant(), &self.sounds);
        let pulmonic = filter_deref_vec(
            consonants.iter(),
            |(_, s)| match &s.description {
                SoundKind::Consonant(c) => c.pulmonic,
                _ => false,
            },
            &self.sounds,
        );
        let non_pulmonic = filter_deref_vec(
            consonants.iter(),
            |(_, s)| match &s.description {
                SoundKind::Consonant(c) => !c.pulmonic,
                _ => false,
            },
            &self.sounds,
        );
        let ejectives = filter_deref_vec(
            consonants.iter(),
            |(_, s)| match &s.description {
                SoundKind::Consonant(c) => c.manners.contains(&Manner::Ejective),
                _ => false,
            },
            &self.sounds,
        );
        let clicks = filter_deref_vec(
            consonants.iter(),
            |(_, s)| match &s.description {
                SoundKind::Consonant(c) => c.manners.contains(&Manner::Click),
                _ => false,
            },
            &self.sounds,
        );
        let implosives = filter_deref_vec(
            consonants.iter(),
            |(_, s)| match &s.description {
                SoundKind::Consonant(c) => c.manners.contains(&Manner::Implosive),
                _ => false,
            },
            &self.sounds,
        );
        let non_glottal = filter_deref_vec(pulmonic.iter(), |(_, s)| !s.is_glottal(), &self.sounds);
        let glottal = filter_deref_vec(pulmonic.iter(), |(_, s)| s.is_glottal(), &self.sounds);
        let nasals = filter_deref_vec(non_glottal.iter(), |(_, s)| s.is_nasal(), &self.sounds);
        let plosives = filter_deref_vec(non_glottal.iter(), |(_, s)| s.is_plosive(), &self.sounds);
        let fricative_affricates = filter_deref_vec(
            non_glottal.iter(),
            |(_, s)| s.is_fricative_affricate() && !s.is_lateral(),
            &self.sounds,
        );
        let attl = filter_deref_vec(non_glottal.iter(), |(_, s)| s.is_attl(), &self.sounds);
        let vowels = filter_deref_vec(sounds.iter(), |(_, s)| s.is_vowel(), &self.sounds);
        let consonants = ("C", pulmonic);
        let glottal = ("H", glottal);
        let nasal = ("N", nasals);
        let plosives = ("P", plosives);
        let fricative_affricates = ("F", fricative_affricates);
        let attl = ("R", attl);
        let ejectives = ("E", ejectives);
        let clicks = ("K", clicks);
        let implosives = ("I", implosives);
        let non_pulmonic = ("Q", non_pulmonic);
        let diphtongs = ("D", self.diphtongs.iter().copied().collect());
        let vowel_diphtongs = ("A", vowels.iter().chain(self.diphtongs.iter()).copied().collect::<_>());
        let vowels = ("V", vowels);
        let list = vec![
            vowels,
            consonants,
            plosives,
            nasal,
            fricative_affricates,
            glottal,
            attl,
            non_pulmonic,
            ejectives,
            clicks,
            implosives,
            vowel_diphtongs,
            diphtongs,
        ];
        shared_data.categories = list
            .into_iter()
            .filter_map(|(str, l)| {
                if l.is_empty() {
                    None
                } else {
                    Some((
                        str.to_string(),
                        l.into_iter()
                            .filter_map(|id| (self.sounds.get(&id).cloned().map(|v| (id, v))))
                            .collect(),
                    ))
                }
            })
            .collect();

    }

    fn import_co_articulated(&mut self) -> (Vec<(String, Vec<Uuid>)>, Vec<(String, Vec<Uuid>)>) {
        let consonants = parse_consonants(crate::raw_data::CO_ARTICULATED_CONSONANTS.as_bytes());
        let consonants = consonants.into_iter().map(|mut c| {
            let desc = match c.description {
                SoundKind::Consonant(mut c) => {
                    c.co_articulated = true;
                    SoundKind::Consonant(c)
                }
                _ => c.description,
            };
            c.description = desc;
            c
        });
        let mut nasal = vec![];
        let mut plosive = vec![];
        let mut fricative = vec![];
        let mut lateral_approximant = vec![];
        let mut implosive = vec![];
        let mut ejective = vec![];
        let mut misc = vec![];
        for c in consonants {
            let id = Uuid::new_v4();
            self.sounds.insert(id, c.clone());
            if let SoundKind::Consonant(c) = c.description {
                if c.manners.contains(&Manner::Nasal) {
                    nasal.push(id);
                } else if c.manners.contains(&Manner::Plosive) {
                    plosive.push(id);
                } else if c.manners.contains(&Manner::Fricative) {
                    fricative.push(id);
                } else if c.manners.contains(&Manner::Lateral)
                    && c.manners.contains(&Manner::Approximant)
                {
                    lateral_approximant.push(id);
                } else if c.manners.contains(&Manner::Implosive) {
                    implosive.push(id);
                } else if c.manners.contains(&Manner::Ejective) {
                    ejective.push(id);
                } else {
                    misc.push(id);
                }
            }
        }
        let left = vec![
            ("nasal".to_string(), nasal),
            ("plosive".to_string(), plosive),
        ];
        let right = vec![
            ("fricative/approximant".to_string(), fricative),
            ("lateral approximant".to_string(), lateral_approximant),
            ("implosive".to_string(), implosive),
            ("ejective".to_string(), ejective),
            ("other".to_string(), misc),
        ];
        (left, right)
    }

    fn import_consonants(&mut self, data: &[u8], pulmonic: bool) -> ConsonantLookups {
        let consonants = parse_consonants(data);
        let consonants = consonants.into_iter().map(|mut c| {
            let desc = match c.description {
                SoundKind::Consonant(mut c) => {
                    c.pulmonic = pulmonic;
                    if !pulmonic {
                        if let Some(index) =
                            c.manners.inner.iter().position(|m| &Manner::Uvular == m)
                        {
                            c.manners.inner.remove(index);
                            c.uvular = true;
                        }
                    }
                    SoundKind::Consonant(c)
                }
                _ => c.description,
            };
            c.description = desc;
            c
        });
        let mut places = HashSet::new();
        let mut by_manners: HashMap<Manners, Vec<Uuid>> = HashMap::new();
        for s in consonants {
            let id = Uuid::new_v4();
            match &s.description {
                SoundKind::Consonant(c) => {
                    places.insert(c.place);
                    if let Some(v) = by_manners.get_mut(&c.manners) {
                        v.push(id);
                        v.sort();
                    } else {
                        by_manners.insert(c.manners.clone(), vec![id]);
                    }
                }
                _ => {}
            }
            self.sounds.insert(id, s.clone());
        }
        let places = places.into_iter().sorted().collect::<Vec<_>>();
        let by_manners = by_manners
            .into_iter()
            .map(|(k, v)| (k, to_by_place(&v, &self.sounds)))
            .collect::<HashMap<_, _>>();
        let sorted_manners = by_manners
            .keys()
            .sorted_by(|a, b| {
                let by_m = partial_cmp_manners(&a.inner, &b.inner).unwrap_or(Ordering::Equal);
                if pulmonic {
                    by_m
                } else {
                    let r = cmp_non_pulmonic(a, b);
                    if r == Ordering::Equal {
                        by_m
                    } else {
                        r
                    }
                }
            })
            .cloned()
            .collect::<_>();
        ConsonantLookups {
            places,
            by_manners,
            sorted_manners,
            empty_rows: Default::default(),
        }
    }

    fn import_vowels(&mut self) -> VowelLookups {
        let vowels: Vec<Sound> = parse_vowels(crate::raw_data::VOWELS.as_bytes());
        let mut backness = HashSet::new();
        let mut by_height: HashMap<Height, Vec<Uuid>> = HashMap::new();
        for s in vowels {
            let id = Uuid::new_v4();
            match &s.description {
                SoundKind::Vowel(v) => {
                    backness.insert(v.backness);
                    if let Some(v) = by_height.get_mut(&v.height) {
                        v.push(id);
                        v.sort();
                    } else {
                        by_height.insert(v.height, vec![id]);
                    }
                }
                _ => {}
            }
            self.sounds.insert(id, s.clone());
        }
        let backness = backness.into_iter().sorted().collect::<Vec<_>>();
        let by_height = by_height
            .into_iter()
            .map(|(k, v)| (k, to_by_backness(&v, &self.sounds)))
            .collect::<HashMap<_, _>>();
        let sorted_height = by_height.keys().copied().sorted().collect::<Vec<_>>();
        VowelLookups {
            backness,
            by_height,
            sorted_height,
            empty_rows: Default::default(),
        }
    }

    fn import_sounds(&mut self) {
        let pulmonic =
            self.import_consonants(crate::raw_data::PULMONIC_CONSONANTS.as_bytes(), true);
        let non_pulmonic =
            self.import_consonants(crate::raw_data::NON_PULMONIC_CONSONANTS.as_bytes(), false);
        let co_articulated = self.import_co_articulated();
        self.consonants = Consonants {
            pulmonic,
            non_pulmonic,
            co_articulated,
        };
        self.vowel_lookups = self.import_vowels();
    }

    pub fn new(shared_data: &mut SharedData) -> Self {
        let mut result = Self::default();
        result.import_sounds();
        result.selected = SelectedSounds::from(&result.sounds);
        result.selected_diphtongs = SelectedSounds::from(&result.sounds).deselected();
        shared_data.sounds = result.sounds.clone();
        shared_data.sound_by_representation = extract_sound_by_representation(&shared_data.sounds);
        result.update_categories(shared_data);
        result.update_empty_rows();
        result
    }
}

fn to_by_backness(input: &[Uuid], sounds: &HashMap<Uuid, Sound>) -> HashMap<Backness, Vec<Uuid>> {
    let mut result: HashMap<Backness, Vec<Uuid>> = HashMap::new();
    for id in input {
        if let Some(s) = sounds.get(id) {
            match &s.description {
                SoundKind::Vowel(v) => {
                    if let Some(v) = result.get_mut(&v.backness) {
                        v.push(*id);
                        v.sort();
                    } else {
                        result.insert(v.backness, vec![*id]);
                    }
                }
                _ => {}
            }
        }
    }
    result
}

fn to_by_place(input: &[Uuid], sounds: &HashMap<Uuid, Sound>) -> HashMap<Place, Vec<Uuid>> {
    let mut result: HashMap<Place, Vec<Uuid>> = HashMap::new();
    for id in input {
        if let Some(s) = sounds.get(id) {
            match &s.description {
                SoundKind::Consonant(c) => {
                    if let Some(v) = result.get_mut(&c.place) {
                        v.push(*id);
                        v.sort();
                    } else {
                        result.insert(c.place.clone(), vec![*id]);
                    }
                }
                _ => {}
            }
        }
    }
    result
}

impl IpaApp {
    pub(crate) fn save(&self) -> IpaSave {
        IpaSave { app: self.clone() }
    }

    fn display_right(sound: &Sound) -> bool {
        match &sound.description {
            SoundKind::Vowel(v) => v.roundedness == Roundedness::Rounded,
            SoundKind::Consonant(c) => {
                if c.pulmonic || c.co_articulated || c.manners.contains(&Manner::Implosive) {
                    c.voice == Some(Voice::Voiced)
                } else {
                    c.uvular
                }
            }
            SoundKind::Custom => false,
            SoundKind::Diphtong(_) => todo!()
        }
    }

    fn build_header<T: ToString>(header: &mut egui_extras::TableRow<'_, '_>, cols: &[T]) {
        header.col(|_| {});
        for c in cols {
            header.col(|ui| {
                ui.separator();
                ui.horizontal_wrapped(|ui| {
                    ui.strong(c.to_string());
                });
            });
        }
    }

    fn display_sound_cell_cell(strip: &mut Strip, id: Option<Uuid>, sounds: &HashMap<Uuid, Sound>, selected : &mut SelectedSounds, settings: &IpaAppSettings, name: &str) {
        strip.cell(|ui| {
            if let Some((id, sound)) =
                id.and_then(|id| sounds.get(&id).map(|s| (id, s)))
            {
                display_sound(ui, &id, sound, selected, settings, name);
            }
        });
    }

    fn display_custom_sound_cell(
        ui: &mut Ui,
        left: Option<Uuid>,
        right: Option<Uuid>,
        sounds: &HashMap<Uuid, Sound>,
        selected: &mut SelectedSounds,
        settings: &IpaAppSettings,
        name: &str,
    ) {
        ui.centered_and_justified(|ui| {
            let _ = StripBuilder::new(ui)
                .size(Size::exact(30.))
                .size(Size::exact(30.))
                .horizontal(|mut strip| {
                    Self::display_sound_cell_cell(&mut strip, left, sounds, selected, settings, name);
                    Self::display_sound_cell_cell(&mut strip, right, sounds, selected, settings, name);
                });
        });
    }

    fn display_sound_cell<C: Eq + Hash>(
        ui: &mut Ui,
        contents: Option<&HashMap<C, Vec<Uuid>>>,
        sounds: &HashMap<Uuid, Sound>,
        selected: &mut SelectedSounds,
        col: &C,
        settings: &IpaAppSettings,
        name: &str,
        additional_filter: Option<&HashSet<Uuid>>,
    ) {
        ui.centered_and_justified(|ui| {
            let _ = StripBuilder::new(ui)
                .size(Size::exact(30.))
                .size(Size::exact(30.))
                .horizontal(|mut strip| {
                    let iter = [false, true].into_iter().map(|v| {
                        contents
                            .and_then(|r| r.get(col))
                            .map(|s| s.iter().filter_map(|id| sounds.get(id).map(|s| (id, s))))
                            .and_then(|mut m| m.find(|(id, s)| Self::display_right(s) == v) )
                            .map(|(id, _)| *id)
                    });
                    let v= if let Some(f) = additional_filter {
                        iter.map(|o| o.and_then(|id| if f.contains(&id) { Some(id) } else { None })).collect::<Vec<_>>()
                    } else {
                        iter.collect::<Vec<_>>()
                    };
                    let left = v[0];
                    let right = v[1];
                    Self::display_sound_cell_cell(&mut strip, left, sounds, selected, settings, name);
                    Self::display_sound_cell_cell(&mut strip, right, sounds, selected, settings, name);

                });
        });
    }

    fn display_sound_row<C: Eq + Hash, R: ToString + Hash + Eq + PartialEq>(
        lookup: &HashMap<R, HashMap<C, Vec<Uuid>>>,
        selected_sounds: &mut SelectedSounds,
        sounds: &HashMap<Uuid, Sound>,
        row: &mut egui_extras::TableRow<'_, '_>,
        cols: &[C],
        row_contents: &R,
        settings: &IpaAppSettings,
        name: &str,
        additional_filter: Option<&HashSet<Uuid>>,
    ) {
        let contents = lookup.get(row_contents);
        row.col(|ui| {
            ui.strong(&row_contents.to_string());
        });
        for (i, col) in cols.iter().enumerate() {
            row.col(|ui| {
                ui.separator();
                Self::display_sound_cell(
                    ui,
                    contents,
                    sounds,
                    selected_sounds,
                    col,
                    settings,
                    name,
                    additional_filter
                );
                if i == cols.len() - 1 {
                    ui.separator();
                }
            });
        }
    }

    fn toggle_table_modifier(
        ui: &mut Ui,
        set: &mut HashSet<String>,
        table_name: &str,
        display_name: &str,
    ) {
        let mut toggle = set.contains(table_name);
        ui.checkbox(&mut toggle, display_name);
        if toggle {
            set.insert(table_name.to_string());
        } else {
            set.remove(table_name);
        }
    }

    fn display_sound_table_header(ui: &mut Ui, settings: &mut IpaAppSettings, name: &str) {
        ui.horizontal_wrapped(|ui| {
            Self::toggle_table_modifier(ui, &mut settings.locked_tables, name, "Lock Table");
            Self::toggle_table_modifier(ui, &mut settings.editing_tables, name, "Edit Table");
            Self::toggle_table_modifier(ui, &mut settings.hide_unselected, name, "Hide Unselected");
        });
    }

    fn display_sound_table<C: Eq + Hash + ToString, R: ToString + Hash + Eq + PartialEq>(
        ui: &mut Ui,
        lookup: &HashMap<R, HashMap<C, Vec<Uuid>>>,
        selected_sounds: &mut SelectedSounds,
        sounds: &HashMap<Uuid, Sound>,
        rows: &Vec<R>,
        empty_rows: &HashMap<R, bool>,
        cols: &[C],
        name: &str,
        settings: &mut IpaAppSettings,
        additional_filter: Option<&HashSet<Uuid>>,
    ) {
        Self::display_sound_table_header(ui, settings, name);
        ui.push_id(name, |ui| {
            ScrollArea::horizontal().show(ui, |ui| {
                let table = TableBuilder::new(ui)
                    .id_salt(name)
                    .striped(true)
                    .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
                    .columns(Column::auto(), cols.len() + 1);
                table
                    .header(20.0, |mut header| {
                        Self::build_header(&mut header, &cols);
                    })
                    .body(|mut body| {
                        for n in rows {
                            if !settings.hide_unselected.contains(name)
                                || !empty_rows.get(n).copied().unwrap_or(true)
                            {
                                body.row(20., |mut row| {
                                    Self::display_sound_row(
                                        &lookup,
                                        selected_sounds,
                                        sounds,
                                        &mut row,
                                        &cols,
                                        n,
                                        settings,
                                        name,
                                        additional_filter
                                    )
                                });
                            }
                        }
                    });
            });
        });
    }

    fn pulmonic_consonant_table(&mut self, ui: &mut Ui) {
        Self::consonant_table(
            ui,
            &self.consonants.pulmonic,
            &mut self.selected,
            &self.sounds,
            &mut self.settings,
            "pulmonic consonants",
        );
    }

    fn non_pulmonic_consonant_table(&mut self, ui: &mut Ui) {
        Self::consonant_table(
            ui,
            &self.consonants.non_pulmonic,
            &mut self.selected,
            &self.sounds,
            &mut self.settings,
            "non-pulmonic consonants",
        );
    }

    // let left = vec![
    //     ("nasal".to_string(), nasal),
    //     ("plosive".to_string(), plosive),
    // ];
    // let right = vec![
    //     ("fricative/approximant".to_string(), fricative),
    //     ("lateral approximant".to_string(), lateral_approximant),
    //     ("implosive".to_string(), implosive),
    //     ("ejective".to_string(), ejective),
    //     ("other".to_string(), misc),
    // ];

    fn display_co_articulated_cell(&mut self, ui: &mut Ui, left: Option<Uuid>, right: Option<Uuid>, label: &str) {
        let _ = StripBuilder::new(ui)
            .size(Size::exact(75.))
            .size(Size::remainder())
            .horizontal(|mut strip| {
                strip.cell(|ui| {
                    Self::display_custom_sound_cell(ui, left, right, &self.sounds, &mut self.selected, &self.settings, "co-articulated");
                });
                strip.cell(|ui| {ui.label(label);} );
            });
    }

    fn co_articulated_consonant_table(&mut self, ui: &mut Ui) {
        let name = "co-articulated";
        Self::display_sound_table_header(ui, &mut self.settings, name);
        ui.push_id(name, |ui| {
            ScrollArea::horizontal().show(ui, |ui| {
                let table = TableBuilder::new(ui)
                    .id_salt(name)
                    .striped(true)
                    .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
                    .columns(Column::auto(), 4);
                table.body(|mut body| {
                    body.row(20., |mut row| {
                        row.col(|ui| {
                            ui.strong("Nasal");
                        });
                        row.col(|ui| {
                            ui.separator();
                            ui.strong("Fricative/approximant");
                        });
                    });

                    fn g_id(l: &IpaApp, col: usize, cat: usize, index: usize) -> Option<Uuid> {
                        Some(if col == 0 {
                            l.consonants.co_articulated.0[cat].1[index]
                        } else {
                            l.consonants.co_articulated.1[cat].1[index]
                        })
                    }

                    body.row(20., |mut row| {
                        row.col(|ui| {
                            self.display_co_articulated_cell(ui, None, g_id(&self, 0,0,0), "labial-alveolar");
                        });
                        row.col(|ui| {
                            ui.separator();
                            self.display_co_articulated_cell(ui, g_id(&self, 1,0,0), g_id(&self, 1,0,1), "labial-palatal");
                        });
                    });

                    body.row(20., |mut row| {
                        row.col(|ui| {
                            self.display_co_articulated_cell(ui, None, g_id(&self, 0,0,1), "labial-retroflex");
                        });
                        row.col(|ui| {
                            ui.separator();
                            self.display_co_articulated_cell(ui, g_id(&self, 1,0,2), g_id(&self, 1,0,3), "labial-velar");
                        });
                    });

                    body.row(20., |mut row| {
                        row.col(|ui| {
                            self.display_co_articulated_cell(ui, None, g_id(&self, 0,0,2), "labial-velar");
                        });
                        row.col(|ui| {
                            ui.separator();
                            self.display_co_articulated_cell(ui, g_id(&self, 1,0,4), None, "Sj-sound");
                        });
                    });

                    body.row(20., |mut row| {
                        row.col(|ui| {
                            ui.strong("Plosive");
                        });
                        row.col(|ui| {
                            ui.separator();
                            ui.strong("Lateral approximant");
                        });
                    });

                    body.row(20., |mut row| {
                        row.col(|ui| {
                            self.display_co_articulated_cell(ui, g_id(&self, 0, 1, 0), g_id(&self, 0,1,1),"labial-alveolar");
                        });
                        row.col(|ui| {
                            ui.separator();
                            self.display_co_articulated_cell(ui, None, g_id(&self, 1,1,0),"velarized alveolar");
                        });
                    });


                    body.row(20., |mut row| {
                        row.col(|ui| {
                            self.display_co_articulated_cell(ui, g_id(&self, 0, 1, 2), g_id(&self, 0,1,3), "labial-retroflex");
                        });
                        row.col(|ui| {
                            ui.separator();
                            ui.strong("Implosive");
                        });
                    });


                    body.row(20., |mut row| {
                        row.col(|ui| {
                            self.display_co_articulated_cell(ui, g_id(&self, 0, 1, 4), g_id(&self, 0,1,5), "labial-velar");
                        });
                        row.col(|ui| {
                            ui.separator();
                            self.display_co_articulated_cell(ui, g_id(&self, 1,2,0), g_id(&self, 1,2,1), "labial-velar");
                        });
                    });

                    body.row(20., |mut row| {
                        row.col(|ui| {
                            self.display_co_articulated_cell(ui, g_id(&self, 0, 1, 6), None, "uvular-epiglottal");
                        });
                        row.col(|ui| {
                            ui.separator();
                            ui.strong("Ejective");
                        });
                    });

                    body.row(20., |mut row| {
                        row.col(|ui| {
                            self.display_co_articulated_cell(ui, g_id(&self, 0, 1, 7), None, "labial-uvular");
                        });
                        row.col(|ui| {
                            ui.separator();
                            self.display_co_articulated_cell(ui,None, g_id(&self, 1,3,0), "labial-alveolar");
                        });
                    });


                })
            });
        });
    }

    fn consonant_table(
        ui: &mut Ui,
        lookup: &ConsonantLookups,
        selected: &mut SelectedSounds,
        sounds: &HashMap<Uuid, Sound>,
        settings: &mut IpaAppSettings,
        name: &str,
    ) {
        let cols = if settings.hide_unselected.contains(name) {
            selected
                .map
                .iter()
                .filter_map(|(id, b)| {
                    if *b {
                        sounds.get(id).and_then(|v| match &v.description {
                            SoundKind::Consonant(c) => {
                                if lookup.places.contains(&c.place) {
                                    Some(c.place)
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        })
                    } else {
                        None
                    }
                })
                .collect::<HashSet<_>>()
                .into_iter()
                .sorted()
                .collect::<Vec<_>>()
        } else {
            lookup.places.clone()
        };
        Self::display_sound_table(
            ui,
            &lookup.by_manners,
            selected,
            sounds,
            &lookup.sorted_manners,
            &lookup.empty_rows,
            &cols,
            name,
            settings,
            None
        );
    }

    fn vowel_table(&mut self, ui: &mut Ui) {
        let cols = if self.settings.hide_unselected.contains("vowels") {
            self.selected
                .map
                .iter()
                .filter_map(|(id, b)| {
                    if *b {
                        self.sounds.get(id).and_then(|v| match &v.description {
                            SoundKind::Vowel(v) => Some(v.backness),
                            _ => None,
                        })
                    } else {
                        None
                    }
                })
                .collect::<HashSet<_>>()
                .into_iter()
                .sorted()
                .collect::<Vec<_>>()
        } else {
            self.vowel_lookups.backness.clone()
        };
        Self::display_sound_table(
            ui,
            &self.vowel_lookups.by_height,
            &mut self.selected,
            &self.sounds,
            &self.vowel_lookups.sorted_height,
            &self.vowel_lookups.empty_rows,
            &cols,
            "vowels",
            &mut self.settings,
            None
        );
    }

    fn diphtong_table(&mut self, ui: &mut Ui, shared_data: &mut SharedData) {
        let cols = if self.settings.hide_unselected.contains("dipthongs") {
            self.selected
                .map
                .iter()
                .filter_map(|(id, b)| {
                    if *b {
                        self.sounds.get(id).and_then(|v| match &v.description {
                            SoundKind::Vowel(v) => Some(v.backness),
                            _ => None,
                        })
                    } else {
                        None
                    }
                })
                .collect::<HashSet<_>>()
                .into_iter()
                .sorted()
                .collect::<Vec<_>>()
        } else {
            self.vowel_lookups.backness.clone()
        };
        Self::display_sound_table(
            ui,
            &self.vowel_lookups.by_height,
            &mut self.selected_diphtongs,
            &self.sounds,
            &self.vowel_lookups.sorted_height,
            &self.vowel_lookups.empty_rows,
            &cols,
            "diphtongs",
            &mut self.settings,
            Some(&self.selected.map.iter().filter_map(|(id, b)| if *b { Some(*id)} else { None }).collect::<HashSet<_>>())
        );
        if self.selected_diphtongs.changed {
            self.selected_diphtongs.only_last_changed();
        }
        ui.horizontal(|ui| {
            ui.label(format!("Currently selected: {}{}",
                             self.selected_diphtongs.last_changed.get(0).and_then(|id|
                             self.sounds.get(id)).map(|s| s.display(false).to_string()).unwrap_or_default(),
                             self.selected_diphtongs.last_changed.get(1).and_then(|id|
                             self.sounds.get(id)).map(|s| s.display(false).to_string()).unwrap_or_default(),
            ));

            if ui.button("Add").clicked() && self.selected_diphtongs.last_changed.len() >= 2 {
                if let Some((s1, s2)) = self.sounds.get(&self.selected_diphtongs.last_changed[0])
                    .and_then(|s| {
                        self.sounds.get(&self.selected_diphtongs.last_changed[1]).map(|s2| (s.clone(), s2.clone()))
                    }) {
                    let id = Uuid::new_v4();
                    self.sounds.insert(id, Sound::diphtong(s1, s2));
                    self.diphtongs.insert(id);
                    self.selected_diphtongs.deselect_in_place();
                    self.selected_diphtongs.last_changed.clear();
                }
            }
        });
        ui.horizontal(|ui| {
            let mut iter = self.diphtongs.clone();
            for (id, d) in iter.into_iter().filter_map(|id| self.sounds.get(&id).map(|s| (id, s))) {
                ui.vertical(|ui| {
                    ui.label(format!("{}", d.display(false)));
                    if ui.button("x").clicked() {
                        self.diphtongs.remove(&id);
                    }
                });
            }
        });
    }

    fn categories(&mut self, ui: &mut Ui, shared_data: &SharedData) {
        for (name, cat) in &shared_data.categories {
            Self::display_category(ui, name, cat);
        }

    }

    fn display_category(ui: &mut Ui, cat_name: &str, contents: &[(Uuid, Sound)]) {
        ui.horizontal_wrapped(|ui| {
            let prev_spacing = ui.spacing().item_spacing;
            ui.spacing_mut().item_spacing = Vec2::new(0., 0.);
            ui.label(&format!("{}=", cat_name));
            let c = contents.iter().map(|(_,s)| {
                s.representation()
            }).collect::<Vec<_>>().join( ",");
            ui.label(c);
            ui.spacing_mut().item_spacing = prev_spacing;
        });
    }

    fn update_empty_rows(&mut self) {
        for m in &self.consonants.pulmonic.sorted_manners {
            if self
                .selected
                .map
                .iter()
                .filter(|(_, b)| **b)
                .filter_map(|(id, _)| self.sounds.get(id))
                .any(|s| match &s.description {
                    SoundKind::Consonant(c) => &c.manners == m,
                    _ => false,
                })
            {
                self.consonants.pulmonic.empty_rows.insert(m.clone(), false);
            } else {
                self.consonants.pulmonic.empty_rows.insert(m.clone(), true);
            }
        }

        for h in &self.vowel_lookups.sorted_height {
            if self
                .selected
                .map
                .iter()
                .filter(|(_, b)| **b)
                .filter_map(|(id, _)| self.sounds.get(id))
                .any(|s| match &s.description {
                    SoundKind::Vowel(v) => &v.height == h,
                    _ => false,
                })
            {
                self.vowel_lookups.empty_rows.insert(h.clone(), false);
            } else {
                self.vowel_lookups.empty_rows.insert(h.clone(), true);
            }
        }
    }
}

impl SubApp for IpaApp {
    fn update(&mut self, ctx: &Context, _frame: &mut eframe::Frame, shared_data: &mut SharedData) {
        self.selected.changed = false;
        egui::SidePanel::right("results")
            .min_width(100.)
            .default_width(200.)
            .show(ctx, |ui| {
                ui.heading("Categories:");
                self.categories(ui, &shared_data);
            });
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.horizontal(|ui| {
                if ui.button("Reset all!").clicked() {
                    *self = Self::new(shared_data);
                    return;
                }
                if ui.button("Clear all!").clicked() {
                    self.selected.deselect_in_place();
                    self.selected_diphtongs.deselect_in_place();
                    self.selected.changed = true;
                    self.diphtongs = HashSet::new();
                }
                ui.checkbox(&mut self.tabs, "As Tabs");
            });
            if self.tabs {
                ui.horizontal_wrapped(|ui| {
                    let name = "Consonants";
                    if ui.selectable_label(self.table.as_str() == name, name).clicked() {
                        self.table = name.to_string();
                    }
                    let name = "Vowels";
                    if ui.selectable_label(self.table.as_str() == name, name).clicked() {
                        self.table = name.to_string();
                    }
                    let name = "Co-articulated";
                    if ui.selectable_label(self.table.as_str() == name, name).clicked() {
                        self.table = name.to_string();
                    }
                    let name = "Non-pulmonic";
                    if ui.selectable_label(self.table.as_str() == name, name).clicked() {
                        self.table = name.to_string();
                    }
                    let name = "Diphtongs";
                    if ui.selectable_label(self.table.as_str() == name, name).clicked() {
                        self.table = name.to_string();
                    }
                });
            }
            ScrollArea::vertical().show(ui, |ui| {
                if self.tabs {
                    match self.table.as_str() {
                        "Vowels" => {
                            self.vowel_table(ui);
                        }
                        "Consonants" => {
                            self.pulmonic_consonant_table(ui);
                        }
                        "Co-articulated" => {
                            self.co_articulated_consonant_table(ui);
                        }
                        "Non-pulmonic" => {
                            self.non_pulmonic_consonant_table(ui);
                        }
                        "Diphtongs" => {
                            self.diphtong_table(ui, shared_data);
                        }
                        _ => {}
                    }
                    ui.add_space(20.);
                } else {
                    ui.heading("Vowels");
                    self.vowel_table(ui);
                    ui.add_space(20.);
                    ui.heading("Consonants");
                    self.pulmonic_consonant_table(ui);
                    ui.add_space(20.);
                    ui.heading("Co-articulated");
                    self.co_articulated_consonant_table(ui);
                    ui.add_space(20.);
                    ui.heading("Non-pulmonic");
                    self.non_pulmonic_consonant_table(ui);
                    ui.add_space(20.);
                    ui.heading("Diphtongs");
                    self.diphtong_table(ui, shared_data);
                    ui.add_space(20.);
                }
            });
        });
        if self.selected.changed || self.selected_diphtongs.changed {
            self.update_empty_rows();
            self.update_categories(shared_data);
        }
    }
}

fn display_sound(
    ui: &mut Ui,
    id: &Uuid,
    sound: &Sound,
    selected: &mut SelectedSounds,
    settings: &IpaAppSettings,
    name: &str,
) {
    ui.centered_and_justified(|ui| {
        display_sound_inner(ui, id, sound, selected, settings, name);
    });
}

fn display_sound_inner(
    ui: &mut Ui,
    id: &Uuid,
    sound: &Sound,
    selected: &mut SelectedSounds,
    settings: &IpaAppSettings,
    name: &str,
) {
    if !settings.editing_tables.contains(name) {
        let on_hover = sound.description_str();
        if selected.is_selected(id) {
            if ui
                .selectable_label(true, sound.representation())
                .on_hover_text(on_hover)
                .clicked()
                && !settings.lock_all_tables
                && !settings.locked_tables.contains(name)
            {
                selected.switch(id);
            }
        } else {
            if !settings.hide_unselected.contains(name) {
                if ui
                    .selectable_label(false, sound.representation())
                    .on_hover_text(on_hover)
                    .clicked()
                    && !settings.lock_all_tables
                    && !settings.locked_tables.contains(name)
                {
                    selected.switch(id);
                }
            }
        }
    } else {
        let mut editable = sound.representation().to_string();
        if !settings.hide_unselected.contains(name) || selected.is_selected(id) {
            let edit = ui.text_edit_singleline(&mut editable);
            if selected.is_selected(id) {
                edit.highlight();
            }
        }
    }
}
