pub use consonants::*;
pub use vowels::*;

mod vowels {
    pub const VOWELS: &str = "\
symbol,height,backness,roundedness,complexity
i,close,front,unrounded,0
y,close,front,rounded,10
ɨ,close,central,unrounded,10
ʉ,close,central,rounded,10
ɯ,close,back,unrounded,10
u,close,back,rounded,0
ɪ,near-close,front,unrounded,10
ʏ,near-close,front,rounded,10
ʊ,near-close,back,unrounded,10
e,close-mid,front,unrounded,1
ø,close-mid,front,rounded,10
ɘ,close-mid,central,unrounded,10
ɵ,close-mid,central,rounded,10
ɤ,close-mid,back,unrounded,10
o,close-mid,back,rounded,1
e̞,mid,front,unrounded,10
ø̞,mid,front,rounded,10
ə,mid,central,unrounded,2
ɵ̞,mid,central,rounded,10
ɤ̞,mid,back,unrounded,10
o̞,mid,back,rounded,10
ɛ,open-mid,front,unrounded,10
œ,open-mid,front,rounded,10
ɜ,open-mid,central,unrounded,10
ɞ,open-mid,central,rounded,10
ʌ,open-mid,back,unrounded,10
ɔ,open-mid,back,rounded,10
æ,near-open,front,unrounded,10
ɐ,near-open,central,unrounded,10
a,open,front,unrounded,0
ɶ,open,front,rounded,10
ä,open,central,unrounded,10
ɑ,open,back,unrounded,10
ɒ,open,back,rounded,10";
}

mod consonants {
    pub const PULMONIC_CONSONANTS: &str = "\
symbol,place,voice,manner,complexity
m̥,bilabial,voiceless,nasal,10
m,bilabial,voiced,nasal,0
p,bilabial,voiceless,plosive,0
b,bilabial,voiced,plosive,0
pɸ,bilabial,voiceless,non-sibilant affricate,10
pß,bilabial,voiced,non-sibilant affricate,10
ɸ,bilabial,voiceless,non-sibilant fricative,10
β,bilabial,voiced,non-sibilant fricative,10
ⱱ̟,bilabial,voiced,tap/flap,10
ʙ̥,bilabial,voiceless,trill,10
ʙ,bilabial,voiced,trill,10
ɱ̊,labiodental,voiceless,nasal,10
ɱ,labiodental,voiced,nasal,10
p̪,labiodental,voiceless,plosive,10
b̪,labiodental,voiced,plosive,10
p̪f,labiodental,voiceless,non-sibilant affricate,10
b̪v,labiodental,voiced,non-sibilant affricate,10
f,labiodental,voiceless,non-sibilant fricative,0
v,labiodental,voiced,non-sibilant fricative,0
ʋ,labiodental,voiced,approximant,10
ⱱ,labiodental,voiced,tap/flap,10
n̼,linguolabial,voiced,nasal,10
t̼,linguolabial,voiceless,plosive,10
d̼,linguolabial,voiced,plosive,10
θ̼,linguolabial,voiceless,non-sibilant fricative,10
ð̼,linguolabial,voiced,non-sibilant fricative,10
ɾ̼,linguolabial,voiced,tap/flap,10
t̪θ,dental,voiceless,non-sibilant affricate,10
d̪ð,dental,voiced,non-sibilant affricate,10
θ̼,dental,voiceless,non-sibilant fricative,10
ð̼,dental,voiced,non-sibilant fricative,10
n̥,alveolar,voiceless,nasal,10
n,alveolar,voiced,nasal,0
t,alveolar,voiceless,plosive,0
d,alveolar,voiced,plosive,0
ts,alveolar,voiceless,sibilant affricate,10
dz,alveolar,voiced,sibilant affricate,10
tɹ̝̊,alveolar,voiceless,non-sibilant affricate,10
dɹ̝,alveolar,voiced,non-sibilant affricate,10
s,alveolar,voiceless,sibilant fricative,0
z,alveolar,voiced,sibilant fricative,0
θ̠,alveolar,voiceless,non-sibilant fricative,10
ð̠,alveolar,voiced,non-sibilant fricative,10
ɹ,alveolar,voiced,approximant,0
ɾ̥,alveolar,voiceless,tap/flap,10
ɾ,alveolar,voiced,tap/flap,2
r̥,alveolar,voiceless,trill,10
r,alveolar,voiced,trill,10
tɬ,alveolar,voiceless,affricate lateral,10
dɮ,alveolar,voiced,affricate lateral,10
ɬ,alveolar,voiceless,fricative lateral,10
ɮ,alveolar,voiced,fricative lateral,10
l,alveolar,voiced,approximant lateral,0
ɺ̥,alveolar,voiceless,tap/flap lateral,10
ɺ,alveolar,voiced,tap/flap lateral,10
t̠ʃ,postalveolar,voiceless,sibilant affricate,10
d̠ʒ,postalveolar,voiced,sibilant affricate,10
t̠ɹ̠̊˔,postalveolar,voiceless,non-sibilant affricate,10
d̠ɹ̠˔,postalveolar,voiced,non-sibilant affricate,10
ʃ,postalveolar,voiceless,sibilant fricative,0
ʒ,postalveolar,voiced,sibilant fricative,10
ɹ̠̊˔,postalveolar,voiceless,non-sibilant fricative,10
ɹ̠˔,postalveolar,voiced,non-sibilant fricative,10
ɳ̊,retroflex,voiceless,nasal,10
ɳ,retroflex,voiced,nasal,10
ʈ,retroflex,voiceless,plosive,10
ɖ,retroflex,voiced,plosive,10
tʂ,retroflex,voiceless,sibilant affricate,10
dʐ,retroflex,voiced,sibilant affricate,10
ʂ,retroflex,voiceless,sibilant fricative,10
ʐ,retroflex,voiced,sibilant fricative,10
ɻ̊˔,retroflex,voiceless,non-sibilant fricative,10
ɻ˔,retroflex,voiced,non-sibilant fricative,10
ɻ,retroflex,voiced,approximant,10
ɽ̊,retroflex,voiceless,tap/flap,10
ɽ,retroflex,voiced,tap/flap,10
ɽ̊r̥,retroflex,voiceless,trill,10
ɽr,retroflex,voiced,trill,10
tꞎ,retroflex,voiceless,affricate lateral,10
ɖ͡ɭ˔,retroflex,voiced,affricate lateral,10
ꞎ,retroflex,voiceless,fricative lateral,10
ɭ˔,retroflex,voiced,fricative lateral,10
ɭ,retroflex,voiced,approximant lateral,10
ɭ̥̆,retroflex,voiceless,tap/flap lateral,10
ɭ̆,retroflex,voiced,tap/flap lateral,10
ɲ̊,palatal,voiceless,nasal,10
ɲ,palatal,voiced,nasal,10
c,palatal,voiceless,plosive,10
ɟ,palatal,voiced,plosive,10
tɕ,palatal,voiceless,sibilant affricate,10
dʑ,palatal,voiced,sibilant affricate,10
cç,palatal,voiceless,non-sibilant affricate,10
ɟʝ,palatal,voiced,non-sibilant affricate,10
ɕ,palatal,voiceless,sibilant fricative,10
ʑ,palatal,voiced,sibilant fricative,10
ç,palatal,voiceless,non-sibilant fricative,10
ʝ,palatal,voiced,non-sibilant fricative,10
j,palatal,voiced,approximant,10
cʎ̥˔,palatal,voiceless,affricate lateral,10
ɟʎ̝,palatal,voiced,affricate lateral,10
ʎ̥˔,palatal,voiceless,fricative lateral,10
ʎ̝,palatal,voiced,fricative lateral,10
ʎ,palatal,voiced,approximant lateral,10
ʎ̆,palatal,voiced,tap/flap lateral,10
ŋ̊,velar,voiceless,nasal,10
ŋ,velar,voiced,nasal,10
k,velar,voiceless,plosive,0
g,velar,voiced,plosive,0
kx,velar,voiceless,non-sibilant affricate,10
ɡɣ,velar,voiced,non-sibilant affricate,10
x,velar,voiceless,non-sibilant fricative,10
ɣ,velar,voiced,non-sibilant fricative,10
ɰ,velar,voiced,approximant,10
kʟ̝̊,velar,voiceless,affricate lateral,10
ɡʟ̝,velar,voiced,affricate lateral,10
ʟ̝̊,velar,voiceless,fricative lateral,10
ʟ̝̊,velar,voiced,fricative lateral,10
ʟ,velar,voiced,approximant lateral,10
ʟ̆,velar,voiced,tap/flap lateral,10
ɴ̥,uvular,voiceless,nasal,10
ɴ,uvular,voiced,nasal,10
q,uvular,voiceless,plosive,10
ɢ,uvular,voiced,plosive,10
qχ,uvular,voiceless,non-sibilant affricate,10
ɢʁ,uvular,voiced,non-sibilant affricate,10
χ,uvular,voiceless,non-sibilant fricative,10
ʁ,uvular,voiced,non-sibilant fricative,10
ɢ̆,uvular,voiced,tap/flap,10
ʀ̥,uvular,voiceless,trill,10
ʀ,uvular,voiced,trill,10
ʟ̠,uvular,voiced,approximant lateral,10
ʡ,pharyngeal/epiglottal,voiceless,plosive,10
ʡʜ,pharyngeal/epiglottal,voiceless,non-sibilant affricate,10
ʡʢ,pharyngeal/epiglottal,voiced,non-sibilant affricate,10
ħ,pharyngeal/epiglottal,voiceless,non-sibilant fricative,10
ʕ,pharyngeal/epiglottal,voiced,non-sibilant fricative,10
ʡ̆,pharyngeal/epiglottal,voiced,tap/flap,10
ʜ,pharyngeal/epiglottal,voiceless,trill,10
ʢ,pharyngeal/epiglottal,voiced,trill,10
ʔ,glottal,voiceless,plosive,10
ʔh,glottal,voiceless,non-sibilant affricate,10
h,glottal,voiceless,non-sibilant fricative,0
ɦ,glottal,voiced,non-sibilant fricative,10
ʔ̞,glottal,voiced,approximant,10";
}

pub mod american_english {
    pub const CATEGORIES: &str = "\
C=mnpbtdkgfvθðszʃʒhwlrj
D=mnpbtdkgfvθðszʃʒrl
V=æ;ɑ;ɔ;ɪ;ɛ;ʌ;ʊ;eɪ;oʊ;aɪ;ɔɪ;aʊ
P=pbkgtd
T=pkt
F=fsθʃv
R=lrw
A=lrwj
N=mn
    ";
    /// Onset:
    /// PR
    /// PF
    /// Consonant other than r w plus j
    /// s[mnfθ]
    /// sT(A)
    /// sNA
    /// Nucleus: V
    /// Coda:
    /// D
    /// l[P{tʃ}{dʒ}
    ///
    pub const SYLLABLES: &str = "\
        [C{P[RF]}{s[mnfθ]}{sT(A)}{sNA}]V
    ";
    pub const VOWELS: &str = "V=æ ɑ ɔ ɪ ɛ ʌ ʊ eɪ oʊ aɪ ɔɪ aʊ";
    pub const END_WOWELS: &str = "U=ə i";
    pub const CONSONANTS: &str = "C=mnpbtdkgfvθðszʃʒhwlrj";
}
