use std::{
    fs::{File, OpenOptions},
    io::{BufRead, BufReader, Read, Write},
    time::{Duration, Instant},
};

use serde_json::{json, Value};

use crate::{enumo::Ruleset, DeriveType, Limits, SynthLanguage};

pub fn write_output<L: SynthLanguage>(
    ruleset: &Ruleset<L>,
    baseline: &Ruleset<L>,
    recipe_name: &str,
    baseline_name: &str,
    time_rules: Duration,
) {
    // get information about the derivability of our ruleset vs. the baseline ruleset
    let start = Instant::now();
    let ((forwards_lhs, backwards_lhs), (lhs_f, lhs_b), results_lhs) =
        get_derivability_results(ruleset, DeriveType::Lhs, baseline);
    let duration = start.elapsed();
    println!(
        "LHS: {} / {} derivable in {} seconds.",
        forwards_lhs,
        baseline.len(),
        duration.as_secs()
    );
    let start = Instant::now();
    let ((forwards_lhs_rhs, backwards_lhs_rhs), (lhs_rhs_f, lhs_rhs_b), results_lhs_rhs) =
        get_derivability_results(ruleset, DeriveType::LhsAndRhs, baseline);
    let duration = start.elapsed();
    println!(
        "LHS/RHS: {} / {} derivable in {} seconds.",
        forwards_lhs_rhs,
        baseline.len(),
        duration.as_secs()
    );
    let start = Instant::now();
    let ((forwards_all, backwards_all), (all_f, all_b), results_all) =
        get_derivability_results(ruleset, DeriveType::AllRules, baseline);
    let duration = start.elapsed();
    println!(
        "ALL: {} / {} derivable in {} seconds.",
        forwards_all,
        baseline.len(),
        duration.as_secs()
    );
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

    std::fs::create_dir_all("nightly/data").unwrap_or_else(|e| panic!("Error creating dir: {}", e));
    add_to_data_file("nightly/data/output.json".to_string(), stats);
}

pub fn add_to_data_file(outfile: String, json: Value) {
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

    match File::open(filepath) {
        Ok(file) => BufReader::new(file).lines().count(),
        Err(_) => 0,
    }
}

pub fn get_derivability_results<L: SynthLanguage>(
    ruleset: &Ruleset<L>,
    derive_type: DeriveType,
    baseline: &Ruleset<L>,
) -> ((usize, usize), (Duration, Duration), Value) {
    let limits = if let DeriveType::AllRules = derive_type {
        Limits {
            iter: 2,
            node: 100_000,
        }
    } else {
        Limits::deriving()
    };

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
