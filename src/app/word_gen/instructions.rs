use crate::app::word_gen::advanced::AdvancedAppPart;
use crate::app::word_gen::simple::SimpleInstructionData;
use crate::app::Categories;
use crate::rewrite::RewriteRuleCollection;
use crate::sounds::Sound;
use eframe::epaint::Vec2;
use egui::{Color32, CursorIcon, Frame, Margin, Stroke, Ui, Widget};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use uuid::Uuid;

#[derive(Clone, Serialize, Deserialize)]
pub struct BaseInstructionData {
    pub categories_str: String,
    pub words_str: String,
    pub rewrites_str: String,
    pub rewrite_rules: RewriteRuleCollection,
    pub categories: Option<Categories>,
    pub to_edit: Option<usize>,
}

impl Default for BaseInstructionData {
    fn default() -> Self {
        Self {
            categories_str: "C=ptk\nV=aeiou".to_string(),
            words_str: "AA3".to_string(),
            rewrites_str: String::new(),
            rewrite_rules: Default::default(),
            categories: None,
            to_edit: Default::default(),
        }
    }
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct InstructionData {
    pub is_advanced: bool,
    pub base: BaseInstructionData,
    pub advanced: AdvancedAppPart,
    pub simple: SimpleInstructionData,
}

pub fn display_category(ui: &mut Ui, category: &(String, Vec<(Uuid, Sound)>)) -> bool {
    ui.horizontal_wrapped(|ui| {
        ui.label(&category.0);
        ui.label("=");
        ui.label(
            category
                .1
                .iter()
                .map(|(_, s)| s.representation())
                .collect::<Vec<_>>()
                .join(","),
        );
        ui.button("Edit").clicked()
    })
    .inner
}

pub fn display_editable_category(ui: &mut Ui, category: &mut (String, Vec<(Uuid, Sound)>)) -> bool {
    let mut to_move = None;
    let mut end_edit = false;
    ui.horizontal_wrapped(|ui| {
        let frame = Frame::default().inner_margin(Margin::symmetric(4., 0.));
        ui.label(&category.0);
        ui.label("=");
        ui.dnd_drop_zone::<usize, _>(frame, |ui| {
            ui.style_mut().spacing.item_spacing = Vec2::new(1., 0.);

            for (idx, (id, sound)) in category.1.iter().enumerate() {
                let id = egui::Id::new(id);
                let mut response = ui
                    .dnd_drag_source(id, idx, |ui| {
                        let frame = Frame::default()
                            .inner_margin(2.)
                            .outer_margin(Margin::symmetric(0., 1.))
                            .fill(Color32::from_rgb(20, 20, 20))
                            .stroke(Stroke::new(0.5, Color32::DARK_GRAY));
                        frame.show(ui, |ui| {
                            let mut label = ui.label(sound.representation());
                            if label.hovered() {
                                label = label.highlight();
                            }
                        })
                    })
                    .response;

                if response.hovered() {
                    response = response.highlight();
                }

                if let (Some(pointer), Some(hovered_payload)) = (
                    ui.input(|i| i.pointer.interact_pos()),
                    response.dnd_hover_payload::<usize>(),
                ) {
                    let rect = response.rect;
                    let stroke = egui::Stroke::new(1.0, Color32::WHITE);
                    let insert_row_idx = if *hovered_payload == idx {
                        ui.painter().vline(rect.center().x, rect.y_range(), stroke);
                        idx
                    } else if pointer.x < rect.center().x {
                        ui.painter().vline(rect.left(), rect.y_range(), stroke);
                        idx
                    } else {
                        ui.painter().vline(rect.right(), rect.y_range(), stroke);
                        idx + 1
                    };

                    if let Some(dragged_payload) = response.dnd_release_payload::<usize>() {
                        to_move = Some((*dragged_payload, insert_row_idx));
                    }
                }
            }
        })
        .0
        .response
        .on_hover_cursor(CursorIcon::PointingHand);
        end_edit = ui.button("✔").clicked();
    });
    if let Some((from, to)) = to_move {
        let item = category.1.remove(from);
        if to < category.1.len() {
            category.1.insert(to, item);
        } else {
            category.1.push(item);
        }
    }
    end_edit
}
