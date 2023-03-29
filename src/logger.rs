use std::{
    fs::{File, OpenOptions},
    io::{BufRead, BufReader, Read, Write},
    time::{Duration, Instant},
};

use serde_json::{json, Value};

use crate::{enumo::Ruleset, DeriveType, Limits, SynthLanguage};

// Writes the ruleset to the json/ subdirectory, to be uploaded to the
// nightly server. The ruleset is written as a json object with a single
// field, "rules".
pub fn write_json_rules<L: SynthLanguage>(ruleset: &Ruleset<L>, filename: &str) {
    let mut filepath = "nightly/json/".to_owned();

    std::fs::create_dir_all(filepath.clone())
        .unwrap_or_else(|e| panic!("Error creating dir: {}", e));

    filepath.push_str(filename);
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(filepath)
        .expect("Unable to open file");
    let rules = json!({
        "rules": ruleset.to_str_vec(),
    })
    .to_string();
    file.write_all(rules.as_bytes())
        .expect("Unable to write to file");
}

pub fn write_json_derivability(filename: String, json: Value) {
    let mut filepath = "nightly/json/derivable_rules/".to_owned();

    std::fs::create_dir_all(filepath.clone())
        .unwrap_or_else(|e| panic!("Error creating dir: {}", e));

    filepath.push_str(&filename);
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(filepath)
        .expect("Unable to open file");

    file.write_all(json.to_string().as_bytes())
        .expect("Unable to write to file");
}

pub fn write_output<L: SynthLanguage>(
    ruleset: &Ruleset<L>,
    baseline: &Ruleset<L>,
    recipe_name: &str,
    baseline_name: &str,
    nightly_file: &str,
    limits: Limits,
    time_rules: Duration,
) {
    // get information about the derivability of our ruleset vs. the baseline ruleset
    let ((forwards_lhs, backwards_lhs), (lhs_f, lhs_b), results_lhs) =
        get_derivability_results(ruleset, DeriveType::Lhs, baseline, limits);
    let ((forwards_lhs_rhs, backwards_lhs_rhs), (lhs_rhs_f, lhs_rhs_b), results_lhs_rhs) =
        get_derivability_results(ruleset, DeriveType::LhsAndRhs, baseline, limits);
    let ((forwards_all, backwards_all), (all_f, all_b), results_all) =
        get_derivability_results(ruleset, DeriveType::AllRules, baseline, limits);

    // get linecount of recipe
    let cnt = count_lines(recipe_name);

    let stats = json!({
        "baseline_name": baseline_name,
        "enumo_spec_name": recipe_name,
        "loc": cnt,
        "num_rules": ruleset.len(),
        "rules": json!({"rules": ruleset.to_str_vec()}),
        "num_baseline": baseline.len(),
        "time": time_rules.as_secs_f64(),
        "enumo_to_baseline_lhs": results_lhs,
        "enumo_to_baseline_lhs_num": forwards_lhs,
        "enumo_to_baseline_lhs_time": lhs_f.as_secs_f64(),
        "enumo_to_baseline_lhs_rhs": results_lhs_rhs,
        "enumo_to_baseline_lhsrhs_num": forwards_lhs_rhs,
        "enumo_to_baseline_lhsrhs_time": lhs_rhs_f.as_secs_f64(),
        "enumo_to_baseline_all": results_all,
        "enumo_to_baseline_all_num": forwards_all,
        "enumo_to_baseline_all_time": all_f.as_secs_f64(),
        "baseline_to_enumo_lhs_num": backwards_lhs,
        "baseline_to_enumo_lhs_time": lhs_b.as_secs_f64(),
        "baseline_to_enumo_lhsrhs_num": backwards_lhs_rhs,
        "baseline_to_enumo_lhsrhs_time": lhs_rhs_b.as_secs_f64(),
        "baseline_to_enumo_all_num": backwards_all,
        "baseline_to_enumo_all_time": all_b.as_secs_f64(),
        "minimization strategy": "compress",
    });

    let nightly_stats = json!({
        "baseline_name": baseline_name,
        "enumo_spec_name": recipe_name,
        "loc": cnt,
        "num_rules": ruleset.len(),
        "num_baseline": baseline.len(),
        "time": time_rules.as_secs_f64(),
        "enumo_derives_baseline (lhs, lhs & rhs, all)":
            format!("{}, {}, {}", forwards_lhs, forwards_lhs_rhs, forwards_all),
        "enumo_derives_baseline_time":
            format!("{}, {}, {}", lhs_f.as_secs_f64(), lhs_rhs_f.as_secs_f64(), all_f.as_secs_f64()),
        "baseline_derives_enumo (lhs, lhs & rhs, all)":
            format!("{}, {}, {}", backwards_lhs, backwards_lhs_rhs, backwards_all),
        "baseline_derives_enumo_time":
            format!("{}, {}, {}", lhs_b.as_secs_f64(), lhs_rhs_b.as_secs_f64(), all_b.as_secs_f64()),
        "minimization strategy": "compress",
    });

    // write to big object JSON file
    add_to_json_file("nightly/json/output.json".to_string(), stats);

    // write to individual derivability results tables for LHS, LHS/RHS, all
    write_json_derivability(
        format!("{}_{}_lhs.json", recipe_name, baseline_name),
        results_lhs,
    );
    write_json_derivability(
        format!("{}_{}_lhs_rhs.json", recipe_name, baseline_name),
        results_lhs_rhs,
    );
    write_json_derivability(
        format!("{}_{}_all.json", recipe_name, baseline_name),
        results_all,
    );

    // write to the table for the individual nightly results
    add_to_json_file(format!("nightly/json/{}", nightly_file), nightly_stats);
}

pub fn add_to_json_file(outfile: String, json: Value) {
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(outfile.clone())
        .expect("Unable to open file");

    let mut file_string = String::new();
    file.read_to_string(&mut file_string)
        .expect("Unable to read file");

    let mut json_arr = vec![];
    if !(file_string.is_empty()) {
        json_arr = serde_json::from_str::<Vec<serde_json::Value>>(&file_string).unwrap();
    }
    json_arr.push(json);

    let mut file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(outfile)
        .expect("Unable to open file");

    file.write_all("[".as_bytes()).expect("write failed");

    for (object, is_last_element) in json_arr
        .iter()
        .enumerate()
        .map(|(i, w)| (w, i == json_arr.len() - 1))
    {
        file.write_all(object.to_string().as_bytes())
            .expect("write failed");
        if !(is_last_element) {
            file.write_all(", ".as_bytes()).expect("write failed");
        }
    }
    file.write_all("]".as_bytes()).expect("write failed");
}

pub fn count_lines(recipe_name: &str) -> usize {
    let mut filepath = "tests/recipes/".to_owned();
    filepath.push_str(recipe_name);
    filepath.push_str(".rs");

    let file = BufReader::new(File::open(filepath).expect("Unable to open file"));
    let mut cnt = 0;

    for _ in file.lines() {
        cnt += 1;
    }
    cnt
}

pub fn get_derivability_results<L: SynthLanguage>(
    ruleset: &Ruleset<L>,
    derive_type: DeriveType,
    baseline: &Ruleset<L>,
    limits: Limits,
) -> ((usize, usize), (Duration, Duration), Value) {
    let start_f = Instant::now();
    let (can_f, cannot_f) = ruleset.derive(derive_type, baseline, limits);
    let time_f = start_f.elapsed();
    let start_b = Instant::now();
    let (can_b, cannot_b) = baseline.derive(derive_type, ruleset, limits);
    let time_b = start_b.elapsed();

    let derivability_results = json!({
        "enumo_derives_baseline_derivable": &can_f.to_str_vec(),
        "enumo_derives_baseline_underivable": &cannot_f.to_str_vec(),
        "baseline_derives_enumo_derivable": &can_b.to_str_vec(),
        "baseline_derives_enumo_underivable": &cannot_b.to_str_vec(),
    });

    (
        (can_f.len(), can_b.len()),
        (time_f, time_b),
        derivability_results,
    )
}
