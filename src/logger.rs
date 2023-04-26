use std::{
    fs::{self, OpenOptions},
    time::{Duration, Instant},
};

use serde_json::{json, Value};

use crate::{count_lines, enumo::Ruleset, DeriveType, Limits, SynthLanguage};

pub fn add_json_to_file(json: Value) {
    let path = "nightly/data/output.json";
    std::fs::create_dir_all("nightly/data").unwrap_or_else(|e| panic!("Error creating dir: {}", e));

    OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(path)
        .expect("Unable to open or create file");

    let s = fs::read_to_string(path).expect("Unable to read file");

    let mut contents: Vec<Value> = if s.is_empty() {
        vec![]
    } else {
        serde_json::from_str(&s).expect("Unable to parse json")
    };

    contents.push(json);

    std::fs::write(path, serde_json::to_string_pretty(&contents).unwrap())
        .expect("Unable to write to json file");
}

pub fn write_baseline<L: SynthLanguage>(
    ruleset: &Ruleset<L>,
    spec_name: &str,
    baseline: &Ruleset<L>,
    baseline_name: &str,
    time: Duration,
) {
    let skip_derive = vec![
        ("herbie", "rational_replicate"),
        ("herbie", "rational_best"),
        ("halide", "halide"),
        ("halide", "oopsla halide (1 iter)"),
        ("oopsla halide (1 iter)", "halide"),
    ];
    let loc = count_lines(spec_name)
        .map(|x| x.to_string())
        .unwrap_or_else(|| "-".to_string());

    let enumo_derives_baseline = if skip_derive.contains(&(spec_name, baseline_name)) {
        json!({})
    } else {
        json!({
            "lhs": get_derivability(ruleset, baseline, DeriveType::Lhs),
            "lhs_rhs": get_derivability(ruleset, baseline, DeriveType::LhsAndRhs)
        })
    };

    let baseline_derives_enumo = if skip_derive.contains(&(spec_name, baseline_name)) {
        json!({})
    } else {
        json!({
            "lhs": get_derivability(baseline, ruleset, DeriveType::Lhs),
            "lhs_rhs": get_derivability(baseline, ruleset, DeriveType::LhsAndRhs)
        })
    };

    let row = json!({
      "spec_name": spec_name,
      "baseline_name": baseline_name,
      "loc": loc,
      "rules": ruleset.to_str_vec(),
      "time": time.as_secs_f64(),
      "derivability": json!({
        "enumo_derives_baseline": enumo_derives_baseline,
        "baseline_derives_enumo": baseline_derives_enumo
      })
    });

    add_json_to_file(row)
}

pub fn get_derivability<L: SynthLanguage>(
    ruleset: &Ruleset<L>,
    against: &Ruleset<L>,
    derive_type: DeriveType,
) -> Value {
    let start = Instant::now();
    let (can, cannot) = ruleset.derive(derive_type, against, Limits::deriving());
    let elapsed = start.elapsed();

    json!({
        "derive_type": derive_type,
        "can": can.to_str_vec(),
        "cannot": cannot.to_str_vec(),
        "time": elapsed.as_secs_f64()
    })
}

#[cfg(test)]
mod test {
    use serde_json::json;

    use super::*;

    #[test]
    fn test_add() {
        add_json_to_file(json!({
          "foo": [1,2,3],
          "bar": "baz"
        }));
    }
}
