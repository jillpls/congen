use egui::Ui;
use egui_file::FileDialog;
use log::error;
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::fs::File;
use std::io::{BufReader, BufWriter, Write};

pub struct FileImportExport {
    default_name: String,
    import_dialog: Option<FileDialog>,
    export_dialog: Option<FileDialog>,
    text_container: Option<String>,
}

impl FileImportExport {
    pub fn display<T: Serialize + DeserializeOwned>(
        &mut self,
        ctx: &egui::Context,
        ui: &mut Ui,
        encode: &T,
    ) -> Option<T> {
        let r = self.import(ctx, ui);
        self.export(ctx, ui, encode);
        r
    }

    #[cfg(target_arch = "wasm32")]
    fn import<T: Serialize + DeserializeOwned>(
        &mut self,
        ctx: &egui::Context,
        ui: &mut Ui,
    ) -> Option<T> {
        import_data(ctx, ui, &mut self.text_container)
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn import<T: Serialize + DeserializeOwned>(
        &mut self,
        ctx: &egui::Context,
        ui: &mut Ui,
    ) -> Option<T> {
        import_data(ctx, ui, &mut self.import_dialog)
    }

    #[cfg(target_arch = "wasm32")]
    fn export<T: Serialize + DeserializeOwned>(
        &self,
        ctx: &egui::Context,
        ui: &mut Ui,
        encode: &T,
    ) {
        export_data(ctx, ui, self.default_name.as_str(), encode);
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn export<T: Serialize + DeserializeOwned>(
        &mut self,
        ctx: &egui::Context,
        ui: &mut Ui,
        encode: &T,
    ) {
        export_data(
            ctx,
            ui,
            &mut self.export_dialog,
            self.default_name.as_str(),
            encode,
        );
    }
}

#[cfg(target_arch = "wasm32")]
pub fn import_data<T: DeserializeOwned>(
    ctx: &egui::Context,
    ui: &mut Ui,
    text_container: &mut Option<String>,
) -> Option<T> {
    if ui.button("Import").clicked() {
        *text_container = Some(String::new());
    }
    let mut return_val = None;
    let mut open = true;
    let mut close = false;
    if let Some(t) = text_container {
        egui::Window::new("Import Instructions")
            .title_bar(true)
            .collapsible(false)
            .open(&mut open)
            .show(ctx, |ui| {
                if ui.button("Import").clicked() {
                    if let Ok(v) = serde_json::from_str(t) {
                        return_val = v;
                    }
                    close = true;
                }
                ui.text_edit_multiline(t);
            });
    }
    if !open || close {
        *text_container = None;
    }
    return_val
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn import_data<T: DeserializeOwned>(
    ctx: &egui::Context,
    ui: &mut Ui,
    file_dialog: &mut Option<FileDialog>,
) -> Option<T> {
    if ui.button("Import").clicked() {
        let mut dialog = FileDialog::open_file(None);
        dialog.open();
        *file_dialog = Some(dialog);
    }
    if let Some(d) = file_dialog {
        if d.show(ctx).selected() {
            if let Some(p) = d.path() {
                if let Ok(f) = File::open(p) {
                    let mut reader = BufReader::new(f);
                    if let Ok(i) = serde_json::from_reader::<_, T>(&mut reader) {
                        return Some(i);
                    }
                }
            }
        }
    }
    None
}

#[cfg(target_arch = "wasm32")]
pub fn export_data<T: Serialize>(ui: &mut Ui, file_name: &str, encode: &T) {
    if ui.button("Export").clicked() {
        use wasm_bindgen::JsCast;
        use web_sys;

        let win = web_sys::window().unwrap();
        let doc = win.document().unwrap();

        let link = doc.create_element("a").unwrap();
        if let Ok(s) = serde_json::to_string_pretty(encode) {
            let url_encoded = urlencoding::encode(&s);
            let href = format!("data:text/json,{}", url_encoded);
            link.set_attribute("href", href.as_str());
            link.set_attribute("download", file_name);

            let link: web_sys::HtmlAnchorElement =
                web_sys::HtmlAnchorElement::unchecked_from_js(link.into());
            link.click();
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn export_data<T: Serialize>(
    ctx: &egui::Context,
    ui: &mut Ui,
    file_dialog: &mut Option<FileDialog>,
    file_name: &str,
    encode: &T,
) {
    if ui.button("Export").clicked() {
        let mut dialog = FileDialog::save_file(None).default_filename(file_name);
        dialog.open();
        *file_dialog = Some(dialog)
    };
    if let Some(d) = file_dialog {
        if d.show(ctx).selected() {
            if let Some(p) = d.path() {
                if let Ok(f) = File::create(p) {
                    let mut writer = BufWriter::new(f);
                    if let Err(e) = serde_json::to_writer_pretty(&mut writer, encode) {
                        error!("{}", e);
                    }
                    if let Err(e) = writer.flush() {
                        error!("{}", e);
                    }
                }
            }
        }
    }
}
