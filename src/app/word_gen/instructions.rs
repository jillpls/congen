use crate::app::word_gen::advanced::AdvancedAppPart;
use crate::app::word_gen::simple::SimpleInstructionData;
use crate::app::Categories;
use crate::rewrite::RewriteRuleCollection;
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize)]
pub struct BaseInstructionData {
    pub categories_str: String,
    pub words_str: String,
    pub rewrites_str: String,
    pub rewrite_rules: RewriteRuleCollection,
    pub categories: Option<Categories>,
}

impl Default for BaseInstructionData {
    fn default() -> Self {
        Self {
            categories_str: "C=ptk\nV=aeiou".to_string(),
            words_str: "AA3".to_string(),
            rewrites_str: String::new(),
            rewrite_rules: Default::default(),
            categories: None,
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
