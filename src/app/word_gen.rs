use crate::app::{Categories, SharedData, SubApp};
use crate::generation::{
    generate_map, GenerationInstruction, GenerationInstructionRoot, GenerationSettings, Syllable,
    Word, WordGen,
};
use crate::sounds::{Sound, SoundKind};
use egui::{ScrollArea, Vec2};
use egui_dropdown::DropDownBox;
use rand::thread_rng;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Formatter;
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WordGenSettings {
    word_display: WordDisplaySettings,
    sorting: Sorting,
    amount: usize,
    variance: f32
}

impl Default for WordGenSettings {
    fn default() -> Self {
        Self {
            word_display: Default::default(),
            sorting: Default::default(),
            amount: 20,
            variance: 1.0,
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
}

/// We derive Deserialize/Serialize so we can persist app state on shutdown.
pub struct WordGenApp {
    pub(crate) settings: WordGenSettings,
    // Example stuff:
    label: String,
    categories_str: String,
    syllables_str: String,
    words_str: String,
    rewrites_str: String,
    output: Option<Vec<Word>>,
    open_help: bool,
    deviation_items: Vec<String>,
    deviation_buf: String,
    categories: Option<Categories>,
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
            label: "Hello World!".to_owned(),
            categories_str: "C=ptk\nV=aeiou".to_string(),
            syllables_str: "A=(C)V(C)".to_string(),
            words_str: "AA3".to_string(),
            rewrites_str: "".to_string(),
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
            categories: None,
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
                    .map(|(_, s)| &s.representation)
                    .cloned()
                    .collect::<Vec<_>>()
                    .join("")
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
            .split_whitespace()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
    } else {
        content.chars().filter(|c| !c.is_whitespace()).map(|c| c.to_string()).collect()
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

impl SubApp for WordGenApp {
    /// Called each time the UI needs repainting, which may be many times per second.
    fn update(
        &mut self,
        ctx: &egui::Context,
        _frame: &mut eframe::Frame,
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
                    ui.vertical(|ui| {
                        ui.label("Categories: ");
                        ui.horizontal_wrapped(|ui| {
                            if ui.button("Import from Ipa").clicked() {
                                self.categories_str = categories_to_str(&shared_data.categories);
                                self.categories = Some(shared_data.categories.clone());
                            }
                            if ui.button("Clear").clicked() {
                                self.categories_str = String::new();
                            }
                        });
                        if ui.text_edit_multiline(&mut self.categories_str).changed() {
                            self.categories = try_categories_from_str(
                                &shared_data.sounds,
                                &shared_data.sound_by_representation,
                                &self.categories_str,
                                false,
                            );
                        }
                    });

                    ui.vertical(|ui| {
                        ui.label("Syllables: ");
                        ui.text_edit_multiline(&mut self.syllables_str);
                    });

                    ui.vertical(|ui| {
                        ui.label("Words: ");
                        ui.text_edit_multiline(&mut self.words_str);
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
                if ui.button("Generate Words!").clicked() {
                    let categories = match &self.categories {
                        None => {
                            let mut r = GenerationInstructionRoot::parse_all(
                                &self.categories_str,
                                &HashMap::new(),
                            );
                            r.iter_mut().for_each(|c| match &c.instruction {
                                GenerationInstruction::List(l) => {
                                    c.instruction = GenerationInstruction::Options(l.clone())
                                }
                                _ => {}
                            });
                            r
                        }

                        Some(c) => GenerationInstructionRoot::from_ipa_categories(&c),
                    };
                    let categories_map = generate_map(categories.clone());
                    let syllables =
                        GenerationInstructionRoot::parse_all(&self.syllables_str, &categories_map);
                    let syllables_map = generate_map(syllables.clone());
                    let words =
                        GenerationInstructionRoot::parse_all(&self.words_str, &syllables_map);
                    let word_gen = WordGen {
                        instructions: words,
                    };
                    let settings = GenerationSettings {
                        deviation: if self.settings.variance >= 10. {
                            None
                        } else {
                            Some(self.settings.variance)
                        },
                    };
                    let mut output = (0..self.settings.amount)
                        .map(|_| word_gen.generate(&mut thread_rng(), &settings))
                        .collect::<Vec<_>>();
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
                            output.sort_by(|a,b| a.syllables.len().cmp(&b.syllables.len()))
                        }
                        Sorting::SoundCount => {
                            fn count_sounds(word: &Word) -> usize {
                                word.syllables.iter().map(|s| s.sounds.len()).sum()
                            }
                            output.sort_by(|a, b| count_sounds(a).cmp(&count_sounds(b)));
                        }
                    }
                    self.output = Some(output);
                }

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
                            ui.selectable_value(&mut self.settings.sorting, Sorting::SyllableCount, "length");
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
                    "none" => self.settings.variance = 10.,
                    "very low" => self.settings.variance = 0.5,
                    "low" => self.settings.variance = 1.,
                    "medium" => self.settings.variance = 2.,
                    "high" => self.settings.variance = 5.,
                    _ => {
                        if let Ok(f) = self.deviation_buf.parse::<f32>() {
                            if f > 0.01 && f <= 10. {
                                self.settings.variance = f;
                            } else {
                                update = false;
                            }
                        } else {
                            update = false;
                        }
                    }
                }
                if update {
                    self.deviation_buf = if self.settings.variance < 10. {
                        format!("{}", self.settings.variance)
                    } else {
                        "none".to_string()
                    };
                }
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
    pub fn new(shared_data: &mut SharedData) -> Self {
        let mut result = Self::default();
        result.categories = try_categories_from_str(
            &shared_data.sounds,
            &shared_data.sound_by_representation,
            &result.categories_str,
            false,
        );
        result
    }
}

fn display_word(ui: &mut egui::Ui, word: &Word, settings: &WordDisplaySettings) {
    ui.horizontal(|ui| match settings.output_style {
        OutputStyle::WordExplanation => {
            ui.label(format!("{}", word))
                .on_hover_text(word.instruction.clone().unwrap_or_default());
        }
        OutputStyle::SyllableExplanation => {
            ui.spacing_mut().item_spacing = Vec2::new(0., 0.);
            for (i, syllable) in word.syllables.iter().enumerate() {
                display_syllable(ui, syllable, i + 1);
                if i < word.syllables.len() - 1 {
                    ui.separator();
                }
            }
        }
        OutputStyle::SoundExplanation => {
            word.syllables.iter().for_each(|s| {
                s.sounds.iter().for_each(|s| {
                    ui.label(&s.representation)
                        .on_hover_text(s.description_str());
                })
            });
        }
        OutputStyle::Raw => {
            ui.label(format!("{}", word));
        }
    });
}

fn display_syllable(ui: &mut egui::Ui, syllable: &Syllable, i: usize) {
    let str = syllable
        .sounds
        .iter()
        .map(|s| s.representation.clone())
        .collect::<Vec<_>>()
        .join("");
    ui.label(str).on_hover_text(
        syllable
            .instruction
            .as_ref()
            .and_then(|v| v.rsplit("=").next())
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

fn paragraph(ui: &mut egui::Ui, title: &str, text: &str) {
    ui.strong(title);
    ui.label(text);
    ui.separator();
}
