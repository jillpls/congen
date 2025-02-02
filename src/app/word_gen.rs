pub(crate) mod advanced;
mod instructions;
pub(crate) mod save;
pub(crate) mod settings;
pub(crate) mod simple;

use crate::app::word_gen::instructions::InstructionData;
use crate::app::word_gen::settings::WordGenSettings;
use crate::app::{Categories, SharedData, SubApp};
use crate::generation::instruction::GenerationSettings;
use crate::rewrite::RewriteRuleCollection;
use crate::sounds::{Sound, SoundKind};
use crate::word::{Syllable, Word};
use csv::WriterBuilder;
use egui::{Button, ScrollArea, TextEdit, Ui, Vec2};
use egui_dropdown::DropDownBox;
use egui_file::FileDialog;
use log::{debug, info};
use rand::thread_rng;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Formatter;
use std::time::Instant;
use uuid::Uuid;

#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct OncInput {
    pub name_str: String,
    pub value_str: String,
    pub added: Vec<(String, String)>,
}

impl OncInput {
    pub fn default_coda() -> Self {
        Self {
            name_str: "".to_string(),
            value_str: "".to_string(),
            added: vec![("C".to_string(), "(C)".to_string())],
        }
    }

    pub fn default_nucleus() -> Self {
        Self {
            name_str: "".to_string(),
            value_str: "".to_string(),
            added: vec![("N".to_string(), "V".to_string())],
        }
    }
    pub fn default_onset() -> Self {
        Self {
            name_str: "".to_string(),
            value_str: "".to_string(),
            added: vec![("O".to_string(), "(C)".to_string())],
        }
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
enum OutputStyle {
    WordExplanation,
    #[default]
    SyllableExplanation,
    SoundExplanation,
    Raw,
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct WordDisplaySettings {
    output_style: OutputStyle,
    rewrite: bool,
}

/// We derive Deserialize/Serialize so we can persist app state on shutdown.
pub struct WordGenApp {
    pub(crate) settings: WordGenSettings,
    // Example stuff:
    instruction_data: InstructionData,
    output: Option<Vec<Word>>,
    open_help: bool,
    deviation_items: Vec<String>,
    deviation_buf: String,
    import_generation_file_dialog: Option<FileDialog>,
    export_generation_file_dialog: Option<FileDialog>,
    #[allow(unused)]
    import_generation_text_dialog: Option<String>,
}

#[derive(Default, Copy, Clone, Serialize, Deserialize, PartialEq, Eq, Debug)]
pub enum Sorting {
    None,
    #[default]
    Alphabetic,
    AlphabeticRev,
    SyllableCount,
    SoundCount,
}

impl std::fmt::Display for Sorting {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Sorting::None => {
                    "none"
                }
                Sorting::Alphabetic => {
                    "alphabetic"
                }
                Sorting::AlphabeticRev => {
                    "reverse alphabetic"
                }
                Sorting::SyllableCount => {
                    "length"
                }
                Sorting::SoundCount => {
                    "reverse length"
                }
            }
        )
    }
}

impl Default for WordGenApp {
    fn default() -> Self {
        Self {
            // Example stuff:
            settings: Default::default(),
            instruction_data: InstructionData::default(),
            output: None,
            open_help: false,
            deviation_items: vec![
                "none".to_string(),
                "very low".to_string(),
                "low".to_string(),
                "medium".to_string(),
                "high".to_string(),
            ],
            deviation_buf: "low".to_string(),
            import_generation_file_dialog: None,
            export_generation_file_dialog: None,
            import_generation_text_dialog: None,
        }
    }
}

fn categories_to_str(categories: &Vec<(String, Vec<(Uuid, Sound)>)>) -> String {
    categories
        .iter()
        .map(|(n, v)| {
            format!(
                "{}={}",
                n,
                v.iter()
                    .map(|(_, s)| s.representation().to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn category_from_str(
    sounds: &HashMap<Uuid, Sound>,
    lookup: &HashMap<String, Uuid>,
    input: &str,
    split_whitespace: bool,
) -> (String, Vec<(Uuid, Sound)>) {
    // TODO: Allow advanced syntax
    let split = input.splitn(2, "=").collect::<Vec<_>>();
    let (name, content) = if split.len() == 2 {
        (split[0].to_string(), split[1])
    } else {
        ("?".to_string(), split[0])
    };
    let symbols = if split_whitespace {
        content
            .split(",")
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
    } else {
        content
            .chars()
            .filter(|c| !c.is_whitespace())
            .map(|c| c.to_string())
            .collect()
    };

    let result = symbols
        .into_iter()
        .map(|s| {
            let (id, sound) = lookup
                .get(&s)
                .and_then(|id| sounds.get(id).map(|s| (*id, s.clone())))
                .unwrap_or((
                    Uuid::new_v4(),
                    Sound {
                        representation: s.clone(),
                        description: SoundKind::Custom,
                        complexity: 10,
                        ..Default::default()
                    },
                ));
            (id, sound)
        })
        .collect();

    (name, result)
}

fn try_categories_from_str(
    sounds: &HashMap<Uuid, Sound>,
    lookup: &HashMap<String, Uuid>,
    input: &str,
    split_whitespace: bool,
) -> Option<Categories> {
    let mut result = vec![];
    let main_split = input
        .split("\n")
        .map(|v| v.split(';'))
        .flatten()
        .collect::<Vec<_>>();
    for cat in main_split {
        result.push(category_from_str(sounds, lookup, cat, split_whitespace));
    }
    Some(result)
}

impl WordGenApp {
    fn generate_word_gen(
        &mut self,
        shared_data: &mut SharedData,
    ) -> crate::generation::instruction::WordGen {
        let mut word_gen = crate::generation::instruction::WordGen::new();
        if let Some(c) = &self.instruction_data.base.categories {
            word_gen.fill_categories(c);
            let _ = word_gen.fill_syllable_parts(
                &self
                    .instruction_data
                    .advanced
                    .onset_input
                    .added
                    .iter()
                    .map(|(n, v)| format!("{}={}", n, v))
                    .collect::<Vec<_>>(),
            );
            let _ = word_gen.fill_syllable_parts(
                &self
                    .instruction_data
                    .advanced
                    .nucleus_input
                    .added
                    .iter()
                    .map(|(n, v)| format!("{}={}", n, v))
                    .collect::<Vec<_>>(),
            );

            let _ = word_gen.fill_syllable_parts(
                &self
                    .instruction_data
                    .advanced
                    .coda_input
                    .added
                    .iter()
                    .map(|(n, v)| format!("{}={}", n, v))
                    .collect::<Vec<_>>(),
            );
            let _ = word_gen.fill_syllables(&self.instruction_data.advanced.syllables_input.added);
            let _ = word_gen.fill_words(
                &self
                    .instruction_data
                    .base
                    .words_str
                    .split('\n')
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .collect::<Vec<_>>(),
            );
        }
        word_gen
    }

    fn generate_words(&mut self, shared_data: &mut SharedData) {
        let word_gen = self.generate_word_gen(shared_data);
        let time = std::time::Instant::now();
        let settings = GenerationSettings {
            deviation: self.settings.variance,
        };
        let amount = self.settings.amount;
        let settings = settings;
        println!("{:?}", time.elapsed());
        let time = std::time::Instant::now();
        let mut output = (0..amount)
            .filter_map(|_| word_gen.generate_word(&mut thread_rng(), &settings))
            .collect::<Vec<_>>();
        println!("Post Gen: {:?}", time.elapsed());
        let time = std::time::Instant::now();

        match self.settings.sorting {
            Sorting::None => {}
            Sorting::Alphabetic => {
                output.sort();
            }
            Sorting::AlphabeticRev => {
                output.sort();
                output.reverse();
            }
            Sorting::SyllableCount => {
                output.sort_by(|a, b| a.syllables.len().cmp(&b.syllables.len()))
            }
            Sorting::SoundCount => {
                fn count_sounds(word: &Word) -> usize {
                    word.syllables.iter().map(|s| s.sounds().len()).sum()
                }
                output.sort_by(|a, b| count_sounds(a).cmp(&count_sounds(b)));
            }
        }
        output.iter_mut().for_each(|w| {
            self.instruction_data
                .base
                .rewrite_rules
                .apply_to_word(w, shared_data)
        });
        self.output = Some(output);
    }
}

impl WordGenApp {
    #[cfg(target_arch = "wasm32")]
    fn import_instructions(&mut self, ctx: &egui::Context, ui: &mut Ui) {
        if let Some(i) = super::file_handling::import_data::<InstructionData>(
            ctx,
            ui,
            &mut self.import_generation_text_dialog,
        ) {
            self.instruction_data = i;
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn import_instructions(&mut self, ctx: &egui::Context, ui: &mut Ui) {
        if let Some(i) = super::file_handling::import_data::<InstructionData>(
            ctx,
            ui,
            &mut self.import_generation_file_dialog,
        ) {
            self.instruction_data = i;
        }
    }

    #[cfg(target_arch = "wasm32")]
    fn export_instructions(&mut self, ctx: &egui::Context, ui: &mut Ui) {
        super::file_handling::export_data(ui, "generation.json", &self.instruction_data);
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn export_instructions(&mut self, ctx: &egui::Context, ui: &mut Ui) {
        super::file_handling::export_data(
            ctx,
            ui,
            &mut self.export_generation_file_dialog,
            "generation.json",
            &self.instruction_data,
        );
    }
}

impl SubApp for WordGenApp {
    /// Called each time the UI needs repainting, which may be many times per second.
    fn update(
        &mut self,
        ctx: &egui::Context,
        frame: &mut eframe::Frame,
        shared_data: &mut SharedData,
    ) {
        // Put your widgets into a `SidePanel`, `TopBottomPanel`, `CentralPanel`, `Window` or `Area`.
        // For inspiration and more examples, go to https://emilk.github.io/egui
        help_window(ctx, &mut self.open_help);

        egui::SidePanel::left("left_panel")
            .resizable(true)
            .default_width(150.)
            .show(ctx, |ui| {
                ui.heading("Input:");
                ScrollArea::vertical().show(ui, |ui| {
                    ui.horizontal(|ui| {
                        ui.selectable_value(
                            &mut self.instruction_data.is_advanced,
                            false,
                            "Simple",
                        );
                        ui.selectable_value(
                            &mut self.instruction_data.is_advanced,
                            true,
                            "Advanced",
                        );
                        self.export_instructions(ctx, ui);
                        self.import_instructions(ctx, ui);
                    });
                    ui.separator();
                    if self.instruction_data.is_advanced {
                        self.advanced_input(ui, shared_data);
                    } else {
                        self.simple_input(ui, shared_data);
                    }

                    ui.separator();

                    ui.vertical(|ui| {
                        ui.label("Rewrite Rules:");
                        if ui
                            .text_edit_multiline(&mut self.instruction_data.base.rewrites_str)
                            .changed()
                        {
                            if let Ok(rules) = RewriteRuleCollection::try_parse(
                                &self.instruction_data.base.rewrites_str,
                            ) {
                                self.instruction_data.base.rewrite_rules = rules;
                            }
                        }
                    });
                });
            });

        egui::SidePanel::left("buttons")
            .resizable(true)
            .default_width(150.)
            .show(ctx, |ui| {
                if ui.button("Help").clicked() {
                    self.open_help = true;
                }
                ui.label("Word amount:");
                ui.add(egui::DragValue::new(&mut self.settings.amount));
                ui.end_row();

                ui.horizontal_wrapped(|ui| {
                    ui.label("Sorting: ");
                    egui::ComboBox::from_id_salt("xx")
                        .selected_text(format!("{}", self.settings.sorting))
                        .show_ui(ui, |ui| {
                            ui.selectable_value(&mut self.settings.sorting, Sorting::None, "none");
                            ui.selectable_value(
                                &mut self.settings.sorting,
                                Sorting::Alphabetic,
                                "alphabetic",
                            );
                            ui.selectable_value(
                                &mut self.settings.sorting,
                                Sorting::AlphabeticRev,
                                "reverse alphabetic",
                            );
                            ui.selectable_value(
                                &mut self.settings.sorting,
                                Sorting::SyllableCount,
                                "length",
                            );
                            ui.selectable_value(
                                &mut self.settings.sorting,
                                Sorting::SoundCount,
                                "reverse length",
                            );
                        });
                });
                let prev = self.deviation_buf.clone();
                ui.horizontal_wrapped(|ui| {
                    ui.label("Deviation:");
                    ui.add(
                        DropDownBox::from_iter(
                            &self.deviation_items,
                            "test_dropbox",
                            &mut self.deviation_buf,
                            |ui, text| ui.selectable_label(false, text),
                        )
                        .filter_by_input(false)
                        .desired_width(80.),
                    );
                });
                let mut update = self.deviation_buf != prev;
                match self.deviation_buf.as_str() {
                    "none" => self.settings.variance = None,
                    "very low" => self.settings.variance = Some(0.5),
                    "low" => self.settings.variance = Some(1.),
                    "medium" => self.settings.variance = Some(2.),
                    "high" => self.settings.variance = Some(5.),
                    _ => {
                        if let Ok(f) = self.deviation_buf.parse::<f32>() {
                            if f > 0.01 && f <= 10. {
                                self.settings.variance = Some(f);
                            } else {
                                update = false;
                            }
                        } else {
                            update = false;
                        }
                    }
                }
                if update {
                    self.deviation_buf = if let Some(v) = self.settings.variance {
                        format!("{}", v)
                    } else {
                        "none".to_string()
                    };
                }
                ui.separator();
                ui.horizontal_wrapped(|ui| {
                    if ui.add(Button::new("Generate Words!")).clicked() {
                        self.generate_words(shared_data);
                    }
                    ui.checkbox(&mut self.settings.word_display.rewrite, "Rewrite Output");
                    if ui.button("Export").clicked() {
                        if let Some(s) = self.export() {
                            // TODO
                        }
                    }
                });
            });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Output:");
            ui.small("hover for syllable explanation");
            ui.horizontal(|ui| {
                ui.label("Output Separation:");
                ui.selectable_value(
                    &mut self.settings.word_display.output_style,
                    OutputStyle::WordExplanation,
                    "Word",
                );
                ui.selectable_value(
                    &mut self.settings.word_display.output_style,
                    OutputStyle::SyllableExplanation,
                    "Syllable",
                );
                ui.selectable_value(
                    &mut self.settings.word_display.output_style,
                    OutputStyle::SoundExplanation,
                    "Sound",
                );
                ui.selectable_value(
                    &mut self.settings.word_display.output_style,
                    OutputStyle::Raw,
                    "Raw",
                );
            });
            ui.end_row();
            if let Some(output) = &self.output {
                ScrollArea::vertical().max_width(1000.).show(ui, |ui| {
                    let prev_spacing = ui.spacing().clone();
                    for word in output {
                        ui.horizontal(|ui| {
                            ui.spacing_mut().item_spacing = Vec2::new(2., 0.);
                            display_word(ui, word, &self.settings.word_display);
                        });
                    }
                    *ui.spacing_mut() = prev_spacing;
                });
            }
        });
    }
}

impl WordGenApp {
    fn export(&self) -> Option<String> {
        if let Some(o) = &self.output {
            let result = o.iter().map(|w| w.export()).collect::<Vec<_>>();
            let mut wtr = WriterBuilder::new().from_writer(vec![]);
            result.iter().for_each(|v| wtr.write_record(v).unwrap());
            Some(String::from_utf8(wtr.into_inner().unwrap()).unwrap())
        } else {
            None
        }
    }

    fn show_onc_input(ui: &mut Ui, input: &mut OncInput, _settings: &WordGenSettings) {
        input.added.retain(|(n, v)| {
            ui.horizontal(|ui| {
                ui.label(&format!("{}={}", n, v));
                !ui.add(Button::new("x").small()).clicked()
            })
            .inner
        });
        ui.vertical(|ui| {
            ui.horizontal(|ui| {
                let text_edit = TextEdit::singleline(&mut input.name_str).desired_width(10.);
                ui.add(text_edit);
                let text_edit = TextEdit::singleline(&mut input.value_str).desired_width(80.);
                ui.add(text_edit);
            });
            if ui.add(Button::new("Add").small()).clicked() {
                if !input.value_str.trim().is_empty()
                    && !input.name_str.trim().is_empty()
                    && input.name_str.trim().len() == 1
                {
                    input
                        .added
                        .push((input.name_str.clone(), input.value_str.clone()));
                    input.name_str = String::new();
                    input.value_str = String::new();
                }
            }
        });
    }
    fn categories(&mut self, ui: &mut Ui, shared_data: &SharedData) {
        ui.vertical(|ui| {
            ui.label("Categories: ");
            ui.horizontal_wrapped(|ui| {
                if ui.button("Import from Ipa").clicked() {
                    self.instruction_data.base.categories_str =
                        categories_to_str(&shared_data.categories);
                    self.instruction_data.base.categories = Some(shared_data.categories.clone());
                }
                if ui.button("Clear").clicked() {
                    self.instruction_data.base.categories_str = String::new();
                }
            });
            if ui
                .text_edit_multiline(&mut self.instruction_data.base.categories_str)
                .changed()
            {
                self.instruction_data.base.categories = try_categories_from_str(
                    &shared_data.sounds,
                    &shared_data.sound_by_representation,
                    &self.instruction_data.base.categories_str,
                    true,
                );
            }
        });
    }

    fn words(&mut self, ui: &mut Ui, _shared_data: &SharedData) {
        ui.vertical(|ui| {
            ui.label("Words: ");
            ui.text_edit_multiline(&mut self.instruction_data.base.words_str);
        });
    }
}

impl WordGenApp {
    pub fn new(shared_data: &mut SharedData) -> Self {
        let mut result = Self::default();
        result.instruction_data.base.categories = try_categories_from_str(
            &shared_data.sounds,
            &shared_data.sound_by_representation,
            &result.instruction_data.base.categories_str,
            true,
        );
        result
    }
}

fn display_word(ui: &mut egui::Ui, word: &Word, settings: &WordDisplaySettings) {
    ui.horizontal(|ui| match settings.output_style {
        OutputStyle::WordExplanation => {
            ui.label(format!("{}", word.display(settings.rewrite)))
                .on_hover_text(word.instruction.clone().unwrap_or_default());
        }
        OutputStyle::SyllableExplanation => {
            ui.spacing_mut().item_spacing = Vec2::new(0., 0.);
            for (i, syllable) in word.syllables.iter().enumerate() {
                display_syllable(ui, syllable, settings);
                if i < word.syllables.len() - 1 {
                    ui.separator();
                }
            }
        }
        OutputStyle::SoundExplanation => {
            word.syllables.iter().for_each(|syll| {
                syll.sounds().iter().for_each(|sound| {
                    ui.label(sound.display(settings.rewrite))
                        .on_hover_text(sound.description_str());
                })
            });
        }
        OutputStyle::Raw => {
            ui.label(format!("{}", word.display(settings.rewrite)));
        }
    });
}

fn display_syllable(ui: &mut egui::Ui, syllable: &Syllable, settings: &WordDisplaySettings) {
    let str = syllable.display(settings.rewrite);
    ui.label(str).on_hover_text(
        syllable
            .instruction
            .as_ref()
            .map(|v| v.to_string())
            .unwrap_or_default(),
    );
}

fn help_window(ctx: &egui::Context, open: &mut bool) {
    egui::Window::new("Help").resizable(true).open(open).show(ctx, |ui| {
        ui.heading("How to Use:");
        ScrollArea::vertical().auto_shrink(false).show(ui, |ui| {
            ui.strong("General Syntax");
            ui.label("The Syntax for categories, words and syllables is the same:");
            ui.code("[Name]=[Contents]");
            ui.horizontal_wrapped(|ui| {
                ui.label("Names can only be single characters.");
                ui.label("Contents are seen as a list of elements. Elements can either be literals, or reference a \"lower level\" definition (categories for syllables, syllables for words)");
            });
            ui.label("E.g. the syllable definition");
            ui.code("S=CVt");
            ui.label("with categories:");
            ui.code("C=ptk");
            ui.code("V=aeiou");
            ui.label("will output a syllable with a random consonant from ptk, a random vowel from aeiou and then always a t, so for example \"pet\" or \"kit\"");
            ui.label("For compatibility with existing generators the contents of the categories input is always interpreted as a choice unless specified otherwise (see below)");
            ui.separator();
            ui.strong("Advanced Syntax");
            ui.label("• (E) - option - E is optional and will only be inserted with 50% chance");
            ui.label("• [ABC] - choice - choose one of A,B,C at random");
            ui.label("• {ABC} - list - treat \"ABC\" as a list (e.g. inside [] or for categories)");
            ui.label("• A* - unlimited repetition - A can be repeated any number of times");
            ui.label("• An - limited repetition - A can be repeated 0 to n (where n is an integer) times.");
        });
    });
}
