use crate::app::word_gen::WordGenApp;
use crate::app::SharedData;
use egui::{ScrollArea, Ui};
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize)]
pub struct SimpleInstructionData {
    pub syllables_str: String,
}

impl Default for SimpleInstructionData {
    fn default() -> Self {
        Self {
            syllables_str: "A=(C)V(C)".to_string(),
        }
    }
}

impl WordGenApp {
    pub(crate) fn simple_input(&mut self, ui: &mut Ui, shared_data: &SharedData) {
        ScrollArea::vertical().show(ui, |ui| {
            self.categories(ui, shared_data);

            ui.vertical(|ui| {
                ui.label("Syllables: ");
                ui.text_edit_multiline(&mut self.instruction_data.simple.syllables_str);
            });

            self.words(ui, shared_data);
        });
    }
}
