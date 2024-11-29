use crate::app::{Categories, SharedData, SubApp};
use crate::generation::{generate_map, GenerationInstruction, GenerationInstructionRoot, GenerationResultType, GenerationSettings, WordGen};
use crate::rewrite::RewriteRuleCollection;
use crate::sounds::{Sound, SoundKind};
use crate::word::{Syllable, Word};
use egui::{Button, ScrollArea, TextEdit, Ui, Vec2};
use egui_dropdown::DropDownBox;
use rand::thread_rng;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Formatter;
use std::thread;
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WordGenSettings {
    word_display: WordDisplaySettings,
    sorting: Sorting,
    amount: usize,
    variance: f32,
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
pub struct OncInput {
    pub name_str: String,
    pub value_str: String,
    pub added: Vec<(String, String)>
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

#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SyllablesInput {
    pub name_str: String,
    pub onset_str: String,
    pub nucleus_str: String,
    pub coda_str: String,
    pub added: Vec<(String, Option<String>, String, Option<String>)>
}


impl SyllablesInput {
    pub fn default_simple() -> Self {
       Self {
           name_str: "".to_string(),
           onset_str: "".to_string(),
           nucleus_str: "".to_string(),
           coda_str: "".to_string(),
           added: vec![("A".to_string(), Some("O".to_string()), "N".to_string(), Some("C".to_string()))],
       }
    }
}

impl SyllablesInput {
    pub fn reset(&mut self) {
        self.name_str = String::new();
        self.onset_str = String::new();
        self.nucleus_str = String::new();
        self.coda_str = String::new();
    }
}

/// We derive Deserialize/Serialize so we can persist app state on shutdown.
pub struct WordGenApp {
    pub(crate) settings: WordGenSettings,
    // Example stuff:
    advanced: bool,
    label: String,
    categories_str: String,
    syllables_str: String,
    onset_input: OncInput,
    nucleus_input: OncInput,
    coda_input: OncInput,
    syllables_input: SyllablesInput,
    words_str: String,
    rewrites_str: String,
    rewrite_rules: RewriteRuleCollection,
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
            advanced: false,
            label: "Hello World!".to_owned(),
            categories_str: "C=ptk\nV=aeiou".to_string(),
            syllables_str: "A=(C)V(C)".to_string(),
            onset_input: OncInput::default_onset(),
            nucleus_input: OncInput::default_nucleus(),
            coda_input: OncInput::default_coda(),
            syllables_input: SyllablesInput::default_simple(),
            words_str: "AA3".to_string(),
            rewrites_str: "".to_string(),
            rewrite_rules: Default::default(),
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
                    .map(|(_, s)| s.representation().to_string())
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
    fn generate_word_gen(&mut self, shared_data: &mut SharedData) -> WordGen {
        let categories = match &self.categories {
            None => {
                let mut r = GenerationInstructionRoot::parse_all(
                    &self.categories_str,
                    &HashMap::new(),
                    GenerationResultType::Sound
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
        let categories_map = generate_map(categories.clone(), GenerationResultType::Sound);
        let syllables = if self.advanced {
            let onset = GenerationInstructionRoot::parse_all_name_value(&self.onset_input.added, &categories_map, GenerationResultType::Unknown);
            let onset_map = generate_map(onset.clone(), GenerationResultType::SyllablePart);
            let nucleus = GenerationInstructionRoot::parse_all_name_value(&self.nucleus_input.added, &categories_map, GenerationResultType::Unknown);
            let nucleus_map = generate_map(nucleus.clone(), GenerationResultType::SyllablePart);
            let coda = GenerationInstructionRoot::parse_all_name_value(&self.coda_input.added, &categories_map, GenerationResultType::Unknown);
            let coda_map = generate_map(coda.clone(), GenerationResultType::SyllablePart);
            println!("{} - {} - {}", onset.len(), nucleus.len(), coda.len());
            println!("{} - {} - {}", onset_map.len(), nucleus_map.len(), coda_map.len());
            println!("{:?}", self.syllables_input.added.len());
            self.syllables_input.added.clone().into_iter().map(|(name, o, n, c)| {
                let onset = o.and_then(|name|  onset_map.get(&name).map(|i| {
                    Box::new(GenerationInstructionRoot {
                        name: Some(name),
                        instruction: GenerationInstruction::Part(Box::new(i.clone())),
                        result_type: GenerationResultType::SyllablePart,
                    })
                }));
                let coda = c.and_then(|name|  coda_map.get(&name).map(|i| {
                    Box::new(GenerationInstructionRoot {
                        name: Some(name),
                        instruction: GenerationInstruction::Part(Box::new(i.clone())),
                        result_type: GenerationResultType::SyllablePart,
                    })
                }));
                let nucleus_instruction = nucleus_map.get(&n).unwrap();
                let nucleus = Box::new(
                    GenerationInstructionRoot {
                        name: Some(n),
                        instruction: GenerationInstruction::Part(Box::new(nucleus_instruction.clone())),
                        result_type: GenerationResultType::SyllablePart,
                    }
                );
                GenerationInstructionRoot {
                    name : Some(name.clone()),
                    instruction: GenerationInstruction::Syllable(onset, nucleus, coda),
                    result_type: GenerationResultType::Syllable,
                }
            }).collect::<Vec<_>>()
        } else {
            GenerationInstructionRoot::parse_all(&self.syllables_str, &categories_map, GenerationResultType::Syllable)
        };
        println!("{}", syllables.len());
        let syllables_map = generate_map(syllables.clone(), GenerationResultType::Syllable);
        let words =
            GenerationInstructionRoot::parse_all(&self.words_str, &syllables_map, GenerationResultType::Word);
        WordGen {
            instructions: words,
        }
    }

    fn generate_words(&mut self, shared_data: &mut SharedData) {
        let word_gen = self.generate_word_gen(shared_data);
        let settings = GenerationSettings {
            deviation: if self.settings.variance >= 10. {
                None
            } else {
                Some(self.settings.variance)
            },
        };
        let amount = self.settings.amount;
        let inner_word_gen = word_gen.clone();
        let handle = thread::spawn(move || {
            let settings= settings;
            let word_gen = inner_word_gen;
            (0..amount)
                .map(|_| word_gen.generate(&mut thread_rng(), &settings))
                .collect::<Vec<_>>()
        });

        let mut output = handle.join().unwrap();

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
        output
            .iter_mut()
            .for_each(|w| self.rewrite_rules.apply_to_word(w));
        self.output = Some(output);
    }
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
                ui.horizontal(|ui| {
                    ui.selectable_value(&mut self.advanced, false, "Simple");
                    ui.selectable_value(&mut self.advanced, true, "Advanced");
                });
                ui.separator();
                if self.advanced {
                    self.advanced_input(ui, shared_data);
                } else {
                    self.simple_input(ui, shared_data);
                }

                ui.separator();

                ui.vertical(|ui| {
                    ui.label("Rewrite Rules:");
                    if ui.text_edit_multiline(&mut self.rewrites_str).changed() {
                        if let Ok(rules) = RewriteRuleCollection::try_parse(&self.rewrites_str)
                        {
                            self.rewrite_rules = rules;
                        }
                    }
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

                if ui.add(Button::new("Generate Words!")).clicked() {
                    self.generate_words(shared_data);
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
                ui.separator();
                ui.checkbox(&mut self.settings.word_display.rewrite, "Rewrite Output");
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

    fn show_onc_input(ui: &mut Ui, input: &mut OncInput, settings: &WordGenSettings) {
        input.added.retain(|(n, v)| {
            ui.horizontal(|ui| {
                ui.label(&format!("{}={}",n,v));
                !ui.add(Button::new("x").small()).clicked()
            }).inner
        });
        ui.vertical(|ui| {
            ui.horizontal(|ui| {
                let text_edit = TextEdit::singleline(&mut input.name_str).desired_width(10.);
                ui.add(text_edit);
                let text_edit = TextEdit::singleline(&mut input.value_str).desired_width(80.);
                ui.add(text_edit);
            });
            if ui.add(Button::new("Add").small()).clicked() {
                if !input.value_str.trim().is_empty() && !input.name_str.trim().is_empty() && input.name_str.trim().len() == 1 {
                    input.added.push((input.name_str.clone(), input.value_str.clone()));
                    input.name_str = String::new();
                    input.value_str = String::new();
                }
            }
        });
    }
    fn advanced_input(&mut self, ui: &mut Ui, shared_data: &SharedData) {
        self.categories(ui, shared_data);
        ui.horizontal_wrapped(|ui| {
            ui.vertical(|ui| {
                ui.label("Onset");
                Self::show_onc_input(ui, &mut self.onset_input, &self.settings);
            });

            ui.separator();

            ui.vertical(|ui| {
                ui.label("Nucleus");
                Self::show_onc_input(ui, &mut self.nucleus_input, &self.settings);
            });
            ui.separator();

            ui.vertical(|ui| {
                ui.label("Coda");
                Self::show_onc_input(ui, &mut self.coda_input, &self.settings);
            });
        });
        ui.vertical(|ui| {
            ui.label("Syllables");
            self.syllables_input.added.retain(|(name,o,n,c)| {
                let label = format!("{}={}{}{}", name, o.clone().unwrap_or_default(), n, c.clone().unwrap_or_default());
                ui.horizontal(|ui| {
                    ui.label(&label);
                    !ui.add(Button::new("x").small()).clicked()
                }).inner
            });
            ui.horizontal(|ui| {
                ui.add(TextEdit::singleline(&mut self.syllables_input.name_str).desired_width(10.)).on_hover_text("Name");
                ui.add(TextEdit::singleline(&mut self.syllables_input.onset_str).desired_width(10.)).on_hover_text("Onset");
                ui.add(TextEdit::singleline(&mut self.syllables_input.nucleus_str).desired_width(10.)).on_hover_text("Nucleus");
                ui.add(TextEdit::singleline(&mut self.syllables_input.coda_str).desired_width(10.)).on_hover_text("Coda");
            });
            if ui.button("Add").clicked() {
                if self.syllables_input.name_str.trim().is_empty() {}
                else if self.syllables_input.nucleus_str.trim().is_empty() {}
                else if self.syllables_input.name_str.trim().len() > 1 || self.syllables_input.nucleus_str.trim().len() > 1 || self.syllables_input.onset_str.trim().len() > 1 || self.syllables_input.coda_str.trim().len() > 1 {}
                else {
                    let onset = if self.syllables_input.onset_str.trim().is_empty() {
                        None
                    } else {
                        Some(self.syllables_input.onset_str.clone())
                    };

                    let coda = if self.syllables_input.coda_str.trim().is_empty() {
                        None
                    } else {
                        Some(self.syllables_input.coda_str.clone())
                    };
                    self.syllables_input.added.push((self.syllables_input.name_str.clone(), onset, self.syllables_input.nucleus_str.clone(), coda));
                    self.syllables_input.reset();
                }
            }
        });
        self.words(ui, shared_data);
    }

    fn categories(&mut self, ui: &mut Ui, shared_data: &SharedData) {
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
    }

    fn words(&mut self, ui: &mut Ui, shared_data: &SharedData) {
        ui.vertical(|ui| {
            ui.label("Words: ");
            ui.text_edit_multiline(&mut self.words_str);
        });
    }
    fn simple_input(&mut self, ui: &mut Ui, shared_data: &SharedData) {
        ScrollArea::vertical().show(ui, |ui| {
            self.categories(ui, shared_data);

            ui.vertical(|ui| {
                ui.label("Syllables: ");
                ui.text_edit_multiline(&mut self.syllables_str);
            });

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
            word.syllables.iter().for_each(|s| {
                s.sounds().iter().for_each(|s| {
                    ui.label(s.display(settings.rewrite))
                        .on_hover_text(s.description_str());
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

fn paragraph(ui: &mut egui::Ui, title: &str, text: &str) {
    ui.strong(title);
    ui.label(text);
    ui.separator();
}
