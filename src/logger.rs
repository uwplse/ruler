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

pub fn write_rules<L: SynthLanguage>(ruleset: &Ruleset<L>, spec_name: &str, time: Duration) {
    let loc = count_lines(spec_name)
        .map(|x| x.to_string())
        .unwrap_or_else(|| "-".to_string());
    let row = json!({
      "spec_name": spec_name,
      "loc": loc,
      "rules": ruleset.to_str_vec(),
      "time": time.as_secs_f64(),

    });

    add_json_to_file(row)
}

pub fn write_derivability<L: SynthLanguage>(
    ruleset: &Ruleset<L>,
    ruleset_name: &str,
    against: &Ruleset<L>,
    against_name: &str,
    derive_type: DeriveType,
) {
    let start = Instant::now();
    let (can, cannot) = ruleset.derive(derive_type, against, Limits::deriving());
    let elapsed = start.elapsed();

    let row = json!({
        "ruleset_name": ruleset_name,
        "against_name": against_name,
        "derive_type": derive_type,
        "can": can.to_str_vec(),
        "cannot": cannot.to_str_vec(),
        "time": elapsed.as_secs_f64()
    });

    add_json_to_file(row)
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
