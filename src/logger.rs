use std::{
    fs::{self, OpenOptions},
    time::{Duration, Instant},
};

use serde_json::{json, Value};

use crate::{count_lines, enumo::Ruleset, DeriveType, Limits, Phase, SynthLanguage};

/**
 * Adds a JSON object to the nightly data
 * The file is an array of JSON objects, so this function
 * parses the file into an array, adds the new JSON objec to
 * the array, and writes it back to the file
 */
fn add_json_to_file(json: Value) {
    let path = "nightly/data/output.json";
    std::fs::create_dir_all("nightly/data").unwrap_or_else(|e| panic!("Error creating dir: {}", e));

    OpenOptions::new()
        .read(true)
        .create(true)
        .append(true)
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

/**
 * Constructs a JSON object that corresponds to a single row of the baseline
 * derivability table (Tables 2 and 3)
 * spec_name: Name of enumo recipe file
 * baseline_name: Baseline to compare against
 * loc: # of lines in enumo recipe
 * rules: array of rules
 * time: time in seconds
 * derivability: JSON object containing dervability in both directions for both derive types
 */
pub fn write_baseline<L: SynthLanguage>(
    ruleset: &Ruleset<L>,
    spec_name: &str,
    baseline: &Ruleset<L>,
    baseline_name: &str,
    time: Duration,
) {
    // Items in this list will *not* run derivability
    // Format is (a, b) where a and b are spec/baseline names
    // and a.derive(b) will *not* run.
    // Note: b.derive(a) will still be computed unless (b, a)
    // is also in this list.
    let skip_derive = [
        ("herbie", "rational_replicate"),
        ("herbie", "rational_best"),
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

    let baseline_derives_enumo = if skip_derive.contains(&(baseline_name, spec_name)) {
        json!({})
    } else {
        json!({
            "lhs": get_derivability(baseline, ruleset, DeriveType::Lhs),
            "lhs_rhs": get_derivability(baseline, ruleset, DeriveType::LhsAndRhs)
        })
    };

    let row = json!({
      "TYPE": "baseline",
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

/**
 * Constructs a JSON object that corresponds to a single row of the bv table (Table 5)
 * domain: one of BV8, BV16, BV32, BV128
 * direct_gen: array of rules + time to generate (directly generated rules for the domain)
 * from_bv4: array of rules + tme to validate (bv4 rules ported to the domain and validated)
 * derivability: JSON object containing dervability for both derive types
 * (using from_bv4 rules to derive direct_gen rules)
 */
pub fn write_bv_derivability<L: SynthLanguage>(
    domain: &str,
    gen_rules: Ruleset<L>,
    gen_time: Duration,
    ported_bv4_rules: Ruleset<L>,
) {
    // Validate bv4 rules for this domain
    let start = Instant::now();
    let (sound_bv4, _) = ported_bv4_rules.partition(|rule| rule.is_valid());
    let validate_time = start.elapsed();

    // Compute derivability
    let start = Instant::now();
    let (can, cannot) = sound_bv4.derive(DeriveType::LhsAndRhs, &gen_rules, Limits::deriving());
    let derive_time = start.elapsed();
    let lhsrhs = json!({
        "can": can.to_str_vec(),
        "cannot": cannot.to_str_vec(),
        "time": derive_time.as_secs_f64()
    });

    let start = Instant::now();
    let (can, cannot) = sound_bv4.derive(DeriveType::Lhs, &gen_rules, Limits::deriving());
    let derive_time = start.elapsed();
    let lhs = json!({
        "can": can.to_str_vec(),
        "cannot": cannot.to_str_vec(),
        "time": derive_time.as_secs_f64()
    });

    add_json_to_file(json!({
        "TYPE": "bv",
        "domain": domain,
        "direct_gen": json!({
            "rules": gen_rules.to_str_vec(),
            "time": gen_time.as_secs_f64()
        }),
        "from_bv4": json!({
            "rules": sound_bv4.to_str_vec(),
            "time": validate_time.as_secs_f64()
        }),
        "derivability": json!({
            "lhs": lhs,
            "lhs_rhs": lhsrhs
        })
    }))
}

/**
 * Constructs a JSON object that corresponds to a single row of the ff table (Table 1)
 * phase1, phase2, phase3 : string indicating what scheduler + rules are used
 * time: time in seconds
 * rules: array of rules
 */
pub fn write_ff_phase<L: SynthLanguage>(
    phase1: Phase<L>,
    phase2: Phase<L>,
    phase3: Phase<L>,
    time: Duration,
    rules: &Ruleset<L>,
) {
    add_json_to_file(json!({
        "TYPE": "ff_phases",
        "phase1": format!("{}", phase1),
        "phase2": format!("{}", phase2),
        "phase3": format!("{}", phase3),
        "time": time.as_secs_f64(),
        "rules": rules.to_str_vec()
    }))
}

/**
 * Uses `ruleset` to derive `against` rules
 * with the specified derive type
 * Returns a JSON object containing the derivability results and time
 */
fn get_derivability<L: SynthLanguage>(
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
