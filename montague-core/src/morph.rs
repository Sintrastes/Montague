//! Morphological segmenter — splits inflected words into stem + morpheme pairs.
//!
//! Produces ambiguous candidate tokenizations that compete in the chart parser,
//! so morphological analysis and whole-word lexicon lookup are treated as equal
//! alternatives.

use crate::types::{AtomType, LambekType};

/// Spelling-change reversal class for recovering stems from inflected forms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SpellingClass {
    /// Reverse e-deletion: try appending `e` to stripped stem.
    EDeletion,
    /// Reverse consonant doubling: try un-doubling final char.
    ConsonantDoubling,
    /// Reverse y→i: replace terminal `i` with `y` at the boundary.
    YToI,
    /// Possessive: strip `'s`, also try without trailing `s`.
    Poss,
}

impl SpellingClass {
    /// Map from STRIPS keyword to class.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "EDeletion" | "e" => Some(SpellingClass::EDeletion),
            "ConsonantDoubling" | "CC" => Some(SpellingClass::ConsonantDoubling),
            "YToI" | "y_i" => Some(SpellingClass::YToI),
            "Poss" | "poss" => Some(SpellingClass::Poss),
            _ => None,
        }
    }
}

/// A known morpheme (suffix) — what the segmenter needs at runtime.
#[derive(Debug, Clone)]
pub struct MorphemeInfo {
    /// The surface form, prefixed with `+` (e.g., `"+s"`, `"+ing"`).
    pub surface: String,
    /// The entity name for type lookup (e.g., `"s_suffix"`).
    pub entity: String,
    /// The Lambek type of the morpheme.
    pub ty: LambekType<AtomType>,
    /// Spelling-change recovery classes for segmenting.
    pub strips: Vec<SpellingClass>,
}

/// Morphological segmenter.
///
/// Given a list of known morphemes, produces all valid stem+suffix segmentations
/// for an input word. The identity segmentation (the word itself) is always
/// included.
pub struct MorphSegmenter {
    morphemes: Vec<MorphemeInfo>,
}

impl MorphSegmenter {
    pub fn new(morphemes: Vec<MorphemeInfo>) -> Self {
        MorphSegmenter { morphemes }
    }

    /// True if any morphemes are registered.
    pub fn has_morphemes(&self) -> bool {
        !self.morphemes.is_empty()
    }

    /// Segment a single word. Returns all possible tokenizations for this word.
    ///
    /// The first entry is always `[word]` (identity). Additional entries are
    /// `[stem, morpheme_surface]` pairs for valid stem+suffix splits.
    ///
    /// `is_known` is a predicate that returns `true` if a candidate stem exists
    /// in the lexicon.
    pub fn segment_word(
        &self,
        word: &str,
        is_known: &dyn Fn(&str) -> bool,
    ) -> Vec<Vec<String>> {
        let mut results = vec![vec![word.to_string()]];

        for morph in &self.morphemes {
            let suffix = match morph.surface.strip_prefix('+') {
                Some(s) => s,
                None => continue,
            };
            if !word.ends_with(suffix) {
                continue;
            }
            for stem in recover_stems(word, suffix, &morph.strips) {
                if stem.is_empty() || stem == word || !is_known(&stem) {
                    continue;
                }
                results.push(vec![stem, morph.surface.clone()]);
            }
        }

        results
    }

    /// Segment a full sentence. Generates the cartesian product of per-word
    /// segmentations, producing all candidate token sequences.
    pub fn segment_sentence(
        &self,
        words: &[String],
        is_known: &dyn Fn(&str) -> bool,
    ) -> Vec<Vec<String>> {
        let per_word: Vec<Vec<Vec<String>>> = words
            .iter()
            .map(|w| self.segment_word(w, is_known))
            .collect();
        cartesian_product(per_word)
    }
}

/// Try to recover candidate stems from a word after stripping a suffix.
fn recover_stems(
    word: &str,
    suffix: &str,
    strips: &[SpellingClass],
) -> Vec<String> {
    let base = match word.strip_suffix(suffix) {
        Some(b) => b.to_string(),
        None => return vec![],
    };
    let mut candidates = vec![base.clone()];

    for class in strips {
        match class {
            SpellingClass::EDeletion => {
                candidates.push(format!("{base}e"));
            }
            SpellingClass::ConsonantDoubling => {
                let chars: Vec<char> = base.chars().collect();
                if chars.len() >= 2 {
                    let last = chars[chars.len() - 1];
                    let prev = chars[chars.len() - 2];
                    if last == prev {
                        let undoubled: String = chars[..chars.len() - 1].iter().collect();
                        candidates.push(undoubled);
                    }
                }
            }
            SpellingClass::YToI => {
                if base.ends_with('i') {
                    let mut chars: Vec<char> = base.chars().collect();
                    let last_idx = chars.len() - 1;
                    chars[last_idx] = 'y';
                    candidates.push(chars.into_iter().collect());
                }
            }
            SpellingClass::Poss => {
                // Strip 's, also try without trailing s
                if base.ends_with('s') && base.len() > 1 {
                    candidates.push(base[..base.len() - 1].to_string());
                }
            }
        }
    }

    candidates
}

/// Compute the cartesian product of a list of option-lists, flattening each
/// combination into a single flat Vec.
fn cartesian_product(lists: Vec<Vec<Vec<String>>>) -> Vec<Vec<String>> {
    lists
        .into_iter()
        .fold(vec![vec![]], |acc, list| {
            acc.into_iter()
                .flat_map(|prefix| {
                    list.iter().map(move |item| {
                        let mut row = prefix.clone();
                        row.extend(item.clone());
                        row
                    })
                })
                .collect()
        })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// A simple known-stem checker: stems must be in this set.
    fn known_set<'a>(stems: &'a [&'a str]) -> impl Fn(&str) -> bool + 'a {
        move |s: &str| stems.contains(&s)
    }

    #[test]
    fn segment_empty_morphemes_is_identity() {
        let seg = MorphSegmenter::new(vec![]);
        let known = |_s: &str| true;
        let results = seg.segment_word("runs", &known);
        assert_eq!(results, vec![vec!["runs"]]);
    }

    #[test]
    fn segment_regular_plural() {
        let seg = MorphSegmenter::new(vec![MorphemeInfo {
            surface: "+s".into(),
            entity: "s_pl_suffix".into(),
            ty: LambekType::Basic("NP".into()),
            strips: vec![SpellingClass::EDeletion],
        }]);
        let known = known_set(&["cat"]);
        let results = seg.segment_word("cats", &known);
        assert_eq!(results.len(), 2);
        assert!(results.contains(&vec!["cats".to_string()]));
        assert!(results.contains(&vec!["cat".to_string(), "+s".to_string()]));
    }

    #[test]
    fn segment_3sg_verb() {
        let seg = MorphSegmenter::new(vec![MorphemeInfo {
            surface: "+s".into(),
            entity: "s_suffix".into(),
            ty: LambekType::Basic("VP".into()),
            strips: vec![],
        }]);
        let known = known_set(&["run"]);
        let results = seg.segment_word("runs", &known);
        assert_eq!(results.len(), 2);
        assert!(results.contains(&vec!["run".to_string(), "+s".to_string()]));
    }

    #[test]
    fn segment_gerund_consonant_doubling() {
        let seg = MorphSegmenter::new(vec![MorphemeInfo {
            surface: "+ing".into(),
            entity: "ing_suffix".into(),
            ty: LambekType::Basic("N".into()),
            strips: vec![SpellingClass::EDeletion, SpellingClass::ConsonantDoubling],
        }]);
        let known = known_set(&["run", "use"]);
        // "running" → "run" + "+ing" (consonant doubling reversal)
        let results = seg.segment_word("running", &known);
        assert!(results.contains(&vec!["run".to_string(), "+ing".to_string()]));
        // "using" → "use" + "+ing" (e-deletion reversal)
        let results2 = seg.segment_word("using", &known);
        assert!(results2.contains(&vec!["use".to_string(), "+ing".to_string()]));
    }

    #[test]
    fn segment_happily_y_i() {
        let seg = MorphSegmenter::new(vec![MorphemeInfo {
            surface: "+ly".into(),
            entity: "ly_suffix".into(),
            ty: LambekType::Basic("Adv".into()),
            strips: vec![SpellingClass::YToI],
        }]);
        let known = known_set(&["happy"]);
        let results = seg.segment_word("happily", &known);
        assert!(results.contains(&vec!["happy".to_string(), "+ly".to_string()]));
    }

    #[test]
    fn segment_used_e_deletion() {
        let seg = MorphSegmenter::new(vec![MorphemeInfo {
            surface: "+ed".into(),
            entity: "ed_suffix".into(),
            ty: LambekType::Basic("VP".into()),
            strips: vec![SpellingClass::EDeletion],
        }]);
        let known = known_set(&["use", "walk"]);
        let results = seg.segment_word("used", &known);
        assert!(results.contains(&vec!["use".to_string(), "+ed".to_string()]));
        // "walked" → "walk" + "+ed" (no spelling change needed)
        let results2 = seg.segment_word("walked", &known);
        assert!(results2.contains(&vec!["walk".to_string(), "+ed".to_string()]));
    }

    #[test]
    fn segment_possessive() {
        let seg = MorphSegmenter::new(vec![MorphemeInfo {
            surface: "+'s".into(),
            entity: "poss_suffix".into(),
            ty: LambekType::Basic("NP".into()),
            strips: vec![SpellingClass::Poss],
        }]);
        let known = known_set(&["Socrates"]);
        // "Socrates'" (possessive with trailing s) → "Socrates" + "+'s"
        let results = seg.segment_word("Socrates'", &known);
        assert!(!results.iter().any(|r| r.len() == 2), "Socrates' ends with ' not 's, so no simple strip");
        // Actually "Socrates'" — the word ends with "'", and "+'s" suffix is "'s"
        // So word "Socrates'" doesn't end with suffix "'s". Let me fix the test.
        let results2 = seg.segment_word("Socrates's", &known);
        assert!(
            results2.contains(&vec!["Socrates".to_string(), "+'s".to_string()]),
            "Socrates's should segment to Socrates + +'s"
        );
    }

    #[test]
    fn segment_string_not_in_lexicon() {
        let seg = MorphSegmenter::new(vec![MorphemeInfo {
            surface: "+ing".into(),
            entity: "ing_suffix".into(),
            ty: LambekType::Basic("N".into()),
            strips: vec![SpellingClass::EDeletion, SpellingClass::ConsonantDoubling],
        }]);
        // "str" is not a known stem, so "string" should only get identity
        let known = known_set(&["run"]);
        let results = seg.segment_word("string", &known);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0], vec!["string"]);
    }

    #[test]
    fn segment_sentence_cartesian_product() {
        let seg = MorphSegmenter::new(vec![
            MorphemeInfo {
                surface: "+s".into(),
                entity: "s_suffix".into(),
                ty: LambekType::Basic("VP".into()),
                strips: vec![],
            },
            MorphemeInfo {
                surface: "+ly".into(),
                entity: "ly_suffix".into(),
                ty: LambekType::Basic("Adv".into()),
                strips: vec![SpellingClass::YToI],
            },
        ]);
        let known = known_set(&["run", "happy"]);
        // "runs" → ["runs"] or ["run", "+s"]
        // "happily" → ["happily"] or ["happy", "+ly"]
        let results = seg.segment_sentence(
            &["runs".into(), "happily".into()],
            &known,
        );
        // 2×2 = 4 candidates
        assert_eq!(results.len(), 4);
        // One should be ["run", "+s", "happy", "+ly"]
        assert!(results.contains(&vec![
            "run".to_string(),
            "+s".to_string(),
            "happy".to_string(),
            "+ly".to_string(),
        ]));
    }
}
