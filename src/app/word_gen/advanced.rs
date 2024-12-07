use crate::app::word_gen::{OncInput, WordGenApp};
use crate::app::SharedData;
use egui::{Button, TextEdit, Ui};
use serde::{Deserialize, Serialize};

pub use syllables_input::*;

mod syllables_input {
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub struct SyllablesInput {
        pub name_str: String,
        pub onset_str: String,
        pub nucleus_str: String,
        pub coda_str: String,
        pub added: Vec<(String, Option<String>, String, Option<String>)>,
    }

    impl SyllablesInput {
        pub fn default_simple() -> Self {
            Self {
                name_str: "".to_string(),
                onset_str: "".to_string(),
                nucleus_str: "".to_string(),
                coda_str: "".to_string(),
                added: vec![(
                    "A".to_string(),
                    Some("O".to_string()),
                    "N".to_string(),
                    Some("C".to_string()),
                )],
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
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AdvancedAppPart {
    pub(crate) onset_input: OncInput,
    pub(crate) nucleus_input: OncInput,
    pub(crate) coda_input: OncInput,
    pub(crate) syllables_input: SyllablesInput,
}

impl Default for AdvancedAppPart {
    fn default() -> Self {
        Self {
            onset_input: OncInput::default_onset(),
            nucleus_input: OncInput::default_nucleus(),
            coda_input: OncInput::default_coda(),
            syllables_input: SyllablesInput::default_simple(),
        }
    }
}

impl WordGenApp {
    pub(crate) fn advanced_input(&mut self, ui: &mut Ui, shared_data: &SharedData) {
        self.categories(ui, shared_data);
        ui.horizontal_wrapped(|ui| {
            ui.vertical(|ui| {
                ui.label("Onset");
                Self::show_onc_input(
                    ui,
                    &mut self.instruction_data.advanced.onset_input,
                    &self.settings,
                );
            });

            ui.separator();

            ui.vertical(|ui| {
                ui.label("Nucleus");
                Self::show_onc_input(
                    ui,
                    &mut self.instruction_data.advanced.nucleus_input,
                    &self.settings,
                );
            });
            ui.separator();

            ui.vertical(|ui| {
                ui.label("Coda");
                Self::show_onc_input(
                    ui,
                    &mut self.instruction_data.advanced.coda_input,
                    &self.settings,
                );
            });
        });
        ui.vertical(|ui| {
            ui.label("Syllables");
            self.instruction_data
                .advanced
                .syllables_input
                .added
                .retain(|(name, o, n, c)| {
                    let label = format!(
                        "{}={}{}{}",
                        name,
                        o.clone().unwrap_or_default(),
                        n,
                        c.clone().unwrap_or_default()
                    );
                    ui.horizontal(|ui| {
                        ui.label(&label);
                        !ui.add(Button::new("x").small()).clicked()
                    })
                    .inner
                });
            ui.horizontal(|ui| {
                ui.add(
                    TextEdit::singleline(
                        &mut self.instruction_data.advanced.syllables_input.name_str,
                    )
                    .desired_width(10.),
                )
                .on_hover_text("Name");
                ui.add(
                    TextEdit::singleline(
                        &mut self.instruction_data.advanced.syllables_input.onset_str,
                    )
                    .desired_width(10.),
                )
                .on_hover_text("Onset");
                ui.add(
                    TextEdit::singleline(
                        &mut self.instruction_data.advanced.syllables_input.nucleus_str,
                    )
                    .desired_width(10.),
                )
                .on_hover_text("Nucleus");
                ui.add(
                    TextEdit::singleline(
                        &mut self.instruction_data.advanced.syllables_input.coda_str,
                    )
                    .desired_width(10.),
                )
                .on_hover_text("Coda");
            });
            if ui.button("Add").clicked() {
                if self
                    .instruction_data
                    .advanced
                    .syllables_input
                    .name_str
                    .trim()
                    .is_empty()
                {
                } else if self
                    .instruction_data
                    .advanced
                    .syllables_input
                    .nucleus_str
                    .trim()
                    .is_empty()
                {
                } else if self
                    .instruction_data
                    .advanced
                    .syllables_input
                    .name_str
                    .trim()
                    .len()
                    > 1
                    || self
                        .instruction_data
                        .advanced
                        .syllables_input
                        .nucleus_str
                        .trim()
                        .len()
                        > 1
                    || self
                        .instruction_data
                        .advanced
                        .syllables_input
                        .onset_str
                        .trim()
                        .len()
                        > 1
                    || self
                        .instruction_data
                        .advanced
                        .syllables_input
                        .coda_str
                        .trim()
                        .len()
                        > 1
                {
                } else {
                    let onset = if self
                        .instruction_data
                        .advanced
                        .syllables_input
                        .onset_str
                        .trim()
                        .is_empty()
                    {
                        None
                    } else {
                        Some(
                            self.instruction_data
                                .advanced
                                .syllables_input
                                .onset_str
                                .clone(),
                        )
                    };

                    let coda = if self
                        .instruction_data
                        .advanced
                        .syllables_input
                        .coda_str
                        .trim()
                        .is_empty()
                    {
                        None
                    } else {
                        Some(
                            self.instruction_data
                                .advanced
                                .syllables_input
                                .coda_str
                                .clone(),
                        )
                    };
                    self.instruction_data.advanced.syllables_input.added.push((
                        self.instruction_data
                            .advanced
                            .syllables_input
                            .name_str
                            .clone(),
                        onset,
                        self.instruction_data
                            .advanced
                            .syllables_input
                            .nucleus_str
                            .clone(),
                        coda,
                    ));
                    self.instruction_data.advanced.syllables_input.reset();
                }
            }
        });
        self.words(ui, shared_data);
    }
}
