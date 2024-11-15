use crate::app::SubApp;
use crate::app::{extract_sound_by_representation, SharedData};
use crate::sounds::{parse_consonants, Place, Sound, SoundKind};
use crate::sounds::{
    parse_vowels, partial_cmp_manners, Backness, Height, Manners, Roundedness, Voice,
};
use eframe::emath::Vec2;
use egui::ScrollArea;
use egui::{Context, Ui};
use egui_extras::TableBuilder;
use egui_extras::{Column, Size, StripBuilder};
use itertools::Itertools;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use strum::IntoEnumIterator;
use uuid::Uuid;

#[derive(Default)]
struct SelectedSounds {
    map: HashMap<Uuid, bool>,
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
            changed: false,
        }
    }
}

impl SelectedSounds {
    pub fn is_selected(&self, id: &Uuid) -> bool {
        self.map.get(id).copied().unwrap_or(false)
    }

    pub fn switch(&mut self, id: &Uuid) {
        if let Some(v) = self.map.get_mut(id) {
            *v = !*v;
            self.changed = true;
        }
    }
}

#[derive(Default)]
pub struct ConsonantLookups {
    places: Vec<Place>,
    by_manners: HashMap<Manners, HashMap<Place, Vec<Uuid>>>,
    sorted_manners: Vec<Manners>,
    empty_rows: HashMap<Manners, bool>
}

#[derive(Default)]
pub struct VowelLookups {
    backness: Vec<Backness>,
    by_height: HashMap<Height, HashMap<Backness, Vec<Uuid>>>,
    sorted_height: Vec<Height>,
    empty_rows: HashMap<Height, bool>
}

#[derive(Default)]
pub struct IpaApp {
    sounds: HashMap<Uuid, Sound>,
    selected: SelectedSounds,
    consonant_lookups: ConsonantLookups,
    vowel_lookups: VowelLookups,
    settings: IpaAppSettings,
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

#[derive(Default)]
struct IpaAppSettings {
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
        let non_glottal =
            filter_deref_vec(consonants.iter(), |(_, s)| !s.is_glottal(), &self.sounds);
        let glottal = filter_deref_vec(consonants.iter(), |(_, s)| s.is_glottal(), &self.sounds);
        let nasals = filter_deref_vec(non_glottal.iter(), |(_, s)| s.is_nasal(), &self.sounds);
        let plosives = filter_deref_vec(non_glottal.iter(), |(_, s)| s.is_plosive(), &self.sounds);
        let fricative_affricates = filter_deref_vec(
            non_glottal.iter(),
            |(_, s)| s.is_fricative_affricate() && !s.is_lateral(),
            &self.sounds,
        );
        let attl = filter_deref_vec(non_glottal.iter(), |(_, s)| s.is_attl(), &self.sounds);
        let vowels = filter_deref_vec(sounds.iter(), |(_, s)| s.is_vowel(), &self.sounds);
        let consonants = ("C", consonants);
        let glottal = ("H", glottal);
        let nasal = ("N", nasals);
        let plosives = ("P", plosives);
        let fricative_affricates = ("F", fricative_affricates);
        let attl = ("R", attl);
        let vowels = ("V", vowels);
        let list = vec![
            vowels,
            consonants,
            plosives,
            nasal,
            fricative_affricates,
            glottal,
            attl,
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
            .collect()
    }

    fn import_consonants(&mut self) -> ConsonantLookups {
        let mut consonants = parse_consonants(crate::raw_data::PULMONIC_CONSONANTS.as_bytes()); // TODO: inline for web
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
            .sorted_by(|a, b| partial_cmp_manners(&a.inner, &b.inner).unwrap_or(Ordering::Equal))
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
        self.consonant_lookups = self.import_consonants();
        self.vowel_lookups = self.import_vowels();
    }

    pub fn new(shared_data: &mut SharedData) -> Self {
        let mut result = Self::default();
        result.import_sounds();
        result.selected = SelectedSounds::from(&result.sounds);
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
    fn display_right(sound: &Sound) -> bool {
        match &sound.description {
            SoundKind::Vowel(v) => v.roundedness == Roundedness::Rounded,
            SoundKind::Consonant(c) => c.voice == Some(Voice::Voiced),
            SoundKind::Custom => false,
        }
    }

    fn build_header<T: ToString>(header: &mut egui_extras::TableRow<'_, '_>, cols: &[T]) {
        header.col(|ui| {});
        for c in cols {
            header.col(|ui| {
                ui.separator();
                ui.horizontal_wrapped(|ui| {
                    ui.strong(c.to_string());
                });
            });
        }
    }

    fn display_sound_cell<C: Eq + Hash>(
        ui: &mut Ui,
        contents: Option<&HashMap<C, Vec<Uuid>>>,
        sounds: &HashMap<Uuid, Sound>,
        selected: &mut SelectedSounds,
        col: &C,
        settings: &IpaAppSettings,
        name: &str,
    ) {
        ui.centered_and_justified(|ui| {
            let _ = StripBuilder::new(ui)
                .size(Size::exact(30.))
                .size(Size::exact(30.))
                .horizontal(|mut strip| {
                    for v in [false, true] {
                        let mut displayed = false;
                        if let Some(r) = &contents {
                            if let Some(s) = r.get(col) {
                                for id in s {
                                    if let Some(sound) = sounds.get(id) {
                                        if (Self::display_right(sound) == v) && !displayed {
                                            strip.cell(|ui| {
                                                display_sound(
                                                    ui, id, sound, selected, settings, name,
                                                );
                                            });
                                            displayed = true;
                                        }
                                    }
                                }
                            }
                        }
                        if !displayed {
                            strip.cell(|ui| {});
                        }
                    }
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
    ) {
        Self::display_sound_table_header(ui, settings, name);
            ui.push_id(name, |ui| {
                ScrollArea::horizontal().show(ui, |ui| {
                let mut table = TableBuilder::new(ui)
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
                            if !settings.hide_unselected.contains(name) || !empty_rows.get(n).copied().unwrap_or(true) {
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
                                    )
                                });
                            }
                        }
                    });
            });
        });
    }

    fn consonant_table(&mut self, ui: &mut Ui) {
        let cols = if self.settings.hide_unselected.contains("consonants") {
            self.selected
                .map
                .iter()
                .filter_map(|(id, b)| {
                    if *b {
                        self.sounds.get(id).and_then(|v| match &v.description {
                            SoundKind::Consonant(c) => Some(c.place),
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
            self.consonant_lookups.places.clone()
        };
        Self::display_sound_table(
            ui,
            &self.consonant_lookups.by_manners,
            &mut self.selected,
            &self.sounds,
            &self.consonant_lookups.sorted_manners,
            &self.consonant_lookups.empty_rows,
            &cols,
            "consonants",
            &mut self.settings,
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
        );
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
            for (_, s) in contents {
                ui.label(&s.representation);
            }
            ui.spacing_mut().item_spacing = prev_spacing;
        });
    }

    fn update_empty_rows(&mut self) {
        for m in &self.consonant_lookups.sorted_manners {
            if self.selected.map.iter().filter(|(_,b)| **b).filter_map(|(id, _)| self.sounds.get(id)).any(
                |s| match &s.description {
                    SoundKind::Consonant(c) => { &c.manners == m }
                    _ => { false }
                }
            ) {
                self.consonant_lookups.empty_rows.insert(m.clone(), false);
            } else {
                self.consonant_lookups.empty_rows.insert(m.clone(), true);
            }
        }

        for h in &self.vowel_lookups.sorted_height {
            if self.selected.map.iter().filter(|(_,b)| **b).filter_map(|(id, _)| self.sounds.get(id)).any(
                |s| match &s.description {
                    SoundKind::Vowel(v) => { &v.height == h }
                    _ => { false }
                }
            ) {
                self.vowel_lookups.empty_rows.insert(h.clone(), false);
            } else {
                self.vowel_lookups.empty_rows.insert(h.clone(), true);
            }
        }
    }
}

impl SubApp for IpaApp {
    fn update(&mut self, ctx: &Context, frame: &mut eframe::Frame, shared_data: &mut SharedData) {
        self.selected.changed = false;
        egui::SidePanel::right("results")
            .min_width(100.)
            .default_width(200.)
            .show(ctx, |ui| {
                ui.heading("Categories:");
                self.categories(ui, &shared_data);
            });
        egui::CentralPanel::default().show(ctx, |ui| {
            ScrollArea::vertical().show(ui, |ui| {
                ui.heading("Consonants");
                self.consonant_table(ui);
                ui.add_space(20.);
                ui.heading("Vowels");
                self.vowel_table(ui);
            });
        });
        if self.selected.changed {
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
                .selectable_label(true, &sound.representation)
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
                    .selectable_label(false, &sound.representation)
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
        let mut editable = sound.representation.clone();
        if !settings.hide_unselected.contains(name) || selected.is_selected(id) {
            let mut edit = ui.text_edit_singleline(&mut editable);
            if selected.is_selected(id) {
                edit.highlight();
            }
        }
    }
}
