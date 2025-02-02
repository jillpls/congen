use crate::app::word_gen::{Sorting, WordDisplaySettings};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WordGenSettings {
    pub(crate) word_display: WordDisplaySettings,
    pub(crate) sorting: Sorting,
    pub(crate) amount: usize,
    pub(crate) variance: Option<f32>,
}

impl Default for WordGenSettings {
    fn default() -> Self {
        Self {
            word_display: Default::default(),
            sorting: Default::default(),
            amount: 20,
            variance: Some(1.),
        }
    }
}
