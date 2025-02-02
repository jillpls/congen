use crate::app::{SharedData, SubApp};
use crate::sound_change::{try_parse, SoundChange, SoundChangeRule};
use egui::{CentralPanel, Context, SidePanel};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

#[derive(Serialize, Deserialize)]
pub struct SoundChangeSave {
    pub(crate) app: SoundChangeApp,
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct SoundChangeApp {
    rules_str: String,
    changes: Vec<SoundChange>,
    input_words: String,
    output_words: Vec<String>,
}

impl SoundChangeApp {
    pub fn save(&self) -> SoundChangeSave {
        SoundChangeSave { app: self.clone() }
    }
}

impl SubApp for SoundChangeApp {
    fn update(&mut self, ctx: &Context, _frame: &mut eframe::Frame, shared_data: &mut SharedData) {
        if !self.output_words.is_empty() {
            SidePanel::right("bla").min_width(400.).show(ctx, |ui| {
                ui.strong("Output:");
                for w in &self.output_words {
                    ui.label(w);
                }
            });
        }
        CentralPanel::default().show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.vertical(|ui| {
                    ui.label("Rules:");
                    if ui.text_edit_multiline(&mut self.rules_str).changed {
                        self.changes = self
                            .rules_str
                            .split("\n")
                            .filter_map(|s| try_parse(s, &vec![]).ok())
                            .collect();
                    }
                    ui.label("Words:");
                    ui.text_edit_multiline(&mut self.input_words);
                });
                ui.vertical(|ui| {
                    if ui.button("Apply").clicked() {
                        self.output_words = self
                            .input_words
                            .split("\n")
                            .map(|s| {
                                self.changes.iter().fold(s.to_string(), |prev, next| {
                                    next.apply_to_str(prev, &HashMap::new()).0
                                })
                            })
                            .collect()
                    }
                    if ui.button("Repeat Output").clicked() {
                        self.input_words = self.output_words.join("\n");
                    }
                });

            });
        });
    }
}
