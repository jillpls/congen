use crate::app::ipa::IpaApp;
use crate::app::word_gen::WordGenApp;
use crate::sounds::Sound;
use eframe::epaint::FontFamily;
use eframe::{App, Frame, Storage};
use egui::{CentralPanel, Context, FontData, FontDefinitions};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use strum::{EnumIter, IntoEnumIterator};
use uuid::Uuid;
use crate::app::sound_change::SoundChangeApp;

mod header;
mod ipa;
mod shared;
mod sound_change;
mod word_gen;

pub type Categories = Vec<(String, Vec<(Uuid, Sound)>)>;

pub fn extract_sound_by_representation(sounds: &HashMap<Uuid, Sound>) -> HashMap<String, Uuid> {
    let mut result = HashMap::new();
    sounds.iter().for_each(|(k, v)| {
        if !result.contains_key(&v.representation) {
            result.insert(v.representation.clone(), *k);
        }
    });
    result
}

#[derive(Default)]
pub struct SharedData {
    sound_by_representation: HashMap<String, Uuid>,
    sounds: HashMap<Uuid, Sound>,
    categories: Categories,
}

pub trait SubApp {
    fn update(
        &mut self,
        ctx: &egui::Context,
        _: &mut eframe::Frame,
        _: &mut SharedData,
    ) {
        CentralPanel::default().show(ctx, |ui| ui.label("Not implemented."));
    }
}

pub struct WrapperApp {
    tab: TabId,
    ipa_app: IpaApp,
    word_gen_app: WordGenApp,
    sound_change_app: SoundChangeApp,
    settings_open: bool,
    shared_data: SharedData,
}

impl Default for WrapperApp {
    fn default() -> Self {
        let mut shared_data = Default::default();
        Self {
            tab: Default::default(),
            ipa_app: IpaApp::new(&mut shared_data),
            word_gen_app: WordGenApp::new(&mut shared_data),
            sound_change_app: SoundChangeApp::default(),
            settings_open: false,
            shared_data,
        }
    }
}

impl WrapperApp {

    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let mut fonts = FontDefinitions::default();
        fonts.font_data.insert(
            "Noto".to_owned(),
            FontData::from_static(include_bytes!(
                "../assets/NotoSans-VariableFont_wdth,wght.ttf"
            )),
        );
        fonts
            .families
            .get_mut(&FontFamily::Proportional)
            .unwrap()
            .insert(0, "Noto".to_owned());

        cc.egui_ctx.set_fonts(fonts);
        let mut result = Self::default();
        if let Some(storage) = cc.storage {
            let word_gen_settings = eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
            result.word_gen_app.settings = word_gen_settings;
        }
        result
    }

    fn settings_panel(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        let is_open = self.settings_open;
        egui::SidePanel::left("settings_panel")
            .resizable(false)
            .show_animated(ctx, is_open, |ui| {
                ui.add_space(4.);
                ui.vertical_centered(|ui| {
                    ui.heading("⚙ Settings");
                });
            });
    }

    fn header_bar(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("header").show(ctx, |ui| {
            ui.horizontal_wrapped(|ui| {
                egui::widgets::global_theme_preference_switch(ui);
                ui.separator();
                ui.toggle_value(&mut self.settings_open, "⚙ Settings");
                ui.separator();

                for tab in TabId::iter() {
                    if ui
                        .selectable_label(tab == self.tab, &tab.to_string())
                        .clicked()
                    {
                        self.tab = tab
                    }
                }
            });
        });
    }
}

impl App for WrapperApp {
    fn save(&mut self, _storage: &mut dyn Storage) {
        eframe::set_value(_storage, eframe::APP_KEY, &self.word_gen_app.settings)
    }

    fn update(&mut self, ctx: &Context, frame: &mut Frame) {
        self.header_bar(ctx, frame);

        egui::TopBottomPanel::bottom("footer").show(ctx, |ui| {
            ui.with_layout(egui::Layout::bottom_up(egui::Align::LEFT), |ui| {
                powered_by_egui_and_eframe(ui);
                ui.horizontal(|ui| {
                    ui.label("Found a bug or have any suggestions? email me:");
                ui.hyperlink_to("dev@jillplease.de", "mailto:dev@jillplease.de?subject=ConGen") });
                ui.horizontal(|ui| {
                    ui.label("Source:");
                    ui.hyperlink_to("github", "https://github.com/jillpls/congen");
                });
                ui.label(
                    egui::RichText::new("⚠ Work in Progess ⚠")
                        .small()
                        .color(ui.visuals().warn_fg_color),
                )
                    .on_hover_text("Some functionalities are still work in progress and might not do anything");
                egui::warn_if_debug_build(ui);
            });
        });

        self.settings_panel(ctx, frame);

        egui::CentralPanel::default().show(ctx, |ui| {
            match self.tab {
                TabId::Ipa => {
                    self.ipa_app.update(ctx, frame, &mut self.shared_data);
                }
                TabId::WordGen => {
                    self.word_gen_app.update(ctx, frame, &mut self.shared_data);
                }
                TabId::SoundChange => {
                    self.sound_change_app.update(ctx, frame, &mut self.shared_data);
                }
            };

        });
    }
}
#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize, Deserialize, EnumIter, Default)]
pub enum TabId {
    #[default]
    Ipa,
    WordGen,
    SoundChange,
}

impl Display for TabId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TabId::Ipa => {
                    "Ipa"
                }
                TabId::WordGen => {
                    "WordGen"
                }
                TabId::SoundChange => {
                    "SoundChange"
                }
                _ => {
                    "???"
                }
            }
        )
    }
}

pub struct Tab {
    identifier: TabId,
    name: String,
    app: Option<Box<dyn App>>,
}

fn powered_by_egui_and_eframe(ui: &mut egui::Ui) {
    ui.horizontal(|ui| {
        ui.spacing_mut().item_spacing.x = 0.0;
        ui.label("Powered by ");
        ui.hyperlink_to("egui", "https://github.com/emilk/egui");
        ui.label(" and ");
        ui.hyperlink_to(
            "eframe",
            "https://github.com/emilk/egui/tree/master/crates/eframe",
        );
        ui.label(".");
    });
}
