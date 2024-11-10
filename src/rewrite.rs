pub struct RewriteRuleCollection {
    rules: Vec<RewriteRule>,
}

impl RewriteRuleCollection {
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
