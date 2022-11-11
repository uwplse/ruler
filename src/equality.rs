use std::str::FromStr;

use crate::*;

#[derive(Clone, Serialize, Deserialize)]
#[serde(from = "SerializedEq")]
#[serde(into = "SerializedEq")]
#[serde(bound = "L: SynthLanguage")]
pub struct Equality<L: SynthLanguage> {
    pub name: Arc<str>,
    pub lhs: Pattern<L>,
    pub rhs: Pattern<L>,
    pub rewrite: Rewrite<L, SynthAnalysis>,
}

impl<L: SynthLanguage> Equality<L> {
    fn from_serialized_eq(ser: SerializedEq) -> Self {
        let l_pat: Pattern<L> = ser.lhs.parse().unwrap();
        let r_pat: Pattern<L> = ser.lhs.parse().unwrap();
        let name = format!("{} ==> {}", l_pat, r_pat);
        Self {
            name: name.clone().into(),
            lhs: l_pat.clone(),
            rhs: r_pat.clone(),
            rewrite: Rewrite::new(name, l_pat, r_pat).unwrap(),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
struct SerializedEq {
    lhs: String,
    rhs: String,
}

impl FromStr for SerializedEq {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((l, r)) = s.split_once("=>") {
            Ok(Self {
                lhs: l.into(),
                rhs: r.into(),
            })
        } else {
            Err(format!("Failed to split {}", s))
        }
    }
}

impl<L: SynthLanguage> FromStr for Equality<L> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ser_eq: SerializedEq = s.parse()?;
        Ok(Self::from(ser_eq))
    }
}

impl<L: SynthLanguage + 'static> From<SerializedEq> for Equality<L> {
    fn from(ser: SerializedEq) -> Self {
        Self::from_serialized_eq(ser)
    }
}

impl<L: SynthLanguage> From<Equality<L>> for SerializedEq {
    fn from(eq: Equality<L>) -> Self {
        Self {
            lhs: eq.lhs.to_string(),
            rhs: eq.rhs.to_string(),
        }
    }
}
