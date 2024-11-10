use crate::app::{Tab, TabId};

pub fn header_bar(ui: &mut egui::Ui, tabs: &Vec<Tab>, selected_tab: &mut TabId) {
    ui.horizontal_wrapped(|ui| {
        bar_contents(ui, tabs, selected_tab);
    });
}

fn bar_contents(ui: &mut egui::Ui, tabs: &Vec<Tab>, selected_tab: &mut TabId) {
    egui::widgets::global_theme_preference_switch(ui);
    ui.separator();
    ui.label("Settings");
    ui.separator();
    for tab in tabs {
        if ui
            .selectable_label(tab.identifier == *selected_tab, &tab.name)
            .clicked()
        {
            *selected_tab = tab.identifier
        }
    }
}
