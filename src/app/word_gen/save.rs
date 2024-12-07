use crate::app::word_gen::instructions::InstructionData;
use crate::app::word_gen::settings::WordGenSettings;
use crate::app::word_gen::WordGenApp;
use crate::word::Word;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct WordGenSave {
    pub(crate) settings: WordGenSettings,
    pub(crate) output: Option<Vec<Word>>,
    pub(crate) open_help: bool,
    pub(crate) deviation_items: Vec<String>,
    pub(crate) deviation_buf: String,
    pub instruction_data: InstructionData,
}

impl WordGenApp {
    pub fn save(&self) -> WordGenSave {
        WordGenSave {
            settings: self.settings.clone(),
            output: self.output.clone(),
            open_help: false,
            deviation_items: self.deviation_items.clone(),
            deviation_buf: self.deviation_buf.clone(),
            instruction_data: self.instruction_data.clone(),
        }
    }

    pub fn load(save: WordGenSave) -> Self {
        Self {
            settings: save.settings,
            instruction_data: save.instruction_data,
            output: save.output,
            open_help: false,
            deviation_items: save.deviation_items,
            deviation_buf: save.deviation_buf,
            import_generation_file_dialog: None,
            export_generation_file_dialog: None,
            import_generation_text_dialog: None,
        }
    }
}
