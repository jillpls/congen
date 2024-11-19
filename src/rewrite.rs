use crate::sounds::Sound;
use crate::word::{Syllable, SyllablePart, Word};

#[derive(Default)]
pub struct RewriteRuleCollection {
    rules: Vec<RewriteRule>,
}

impl RewriteRuleCollection {
    pub fn apply_to_word(&self, word: &mut Word) {
        for rule in &self.rules {
            rule.apply_to_word(word);
        }
    }

    pub fn try_parse(input: &str) -> Result<Self, ()> {
        let rules = input
            .split("\n")
            .map(|s| s.split(';'))
            .flatten()
            .map(|i| RewriteRule::try_parse(i))
            .collect::<Vec<_>>();
        Ok(Self {
            rules: rules.into_iter().filter_map(|r| r.ok()).collect::<_>(),
        })
    }

    pub fn apply_to_str(&self, input: &str) -> String {
        self.rules
            .iter()
            .fold(input.to_string(), |prev, next| next.apply_to_str(&prev))
    }
}

pub struct RewriteRule {
    pub from: String,
    pub to: String,
}

impl RewriteRule {
    pub fn try_parse(input: &str) -> Result<Self, ()> {
        let split = input.split('|').collect::<Vec<_>>();
        if split.len() != 2 {
            return Err(());
        }
        Ok(Self {
            from: split[0].to_string(),
            to: split[1].to_string(),
        })
    }

    pub fn apply_to_str(&self, input: &str) -> String {
        input.replace(&self.from, &self.to)
    }

    pub fn apply_to_word(&self, word: &mut Word) {
        for syllable in word.syllables.iter_mut() {
            self.apply_to_syllable(syllable);
        }
    }

    pub fn apply_to_syllable(&self, syllable: &mut Syllable) {
        if let Some(o) = syllable.onset.as_mut() {
            self.apply_to_syllable_part(o);
        }

        self.apply_to_syllable_part(&mut syllable.nucleus);

        if let Some(c) = syllable.coda.as_mut() {
            self.apply_to_syllable_part(c);
        }
    }

    pub fn apply_to_syllable_part(&self, syllable_part: &mut SyllablePart) {
        for sound in syllable_part.sounds.iter_mut() {
            self.apply_to_sound(sound);
        }
    }

    pub fn apply_to_sound(&self, sound: &mut Sound) {
        if sound.representation() == self.from.as_str() {
            sound.rewrite = Some(self.to.clone());
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const TH: &str = "θ|th";
    const THT: &str = "th|t";
    const VANISH: &str = "t|";

    #[test]
    fn t_simple_rule() {
        let rule = RewriteRule::try_parse(TH).unwrap();
        assert_eq!(rule.from, "θ");
        assert_eq!(rule.to, "th");
        assert_eq!(rule.apply_to_str("aθthorθ"), "aththorth");
    }

    #[test]
    fn t_vanish_rule() {
        let rule = RewriteRule::try_parse(VANISH).unwrap();
        assert_eq!(rule.apply_to_str("ttht"), "h");
    }

    #[test]
    fn t_rules() {
        let rules = vec![TH, THT, VANISH].join("\n");
        let rules = RewriteRuleCollection::try_parse(&rules).unwrap();
        assert_eq!(rules.rules.len(), 3);
        assert_eq!(rules.apply_to_str("aθthorθ"), "aor");
    }
}
