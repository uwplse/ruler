use std::{
    fs::{self, OpenOptions},
    time::{Duration, Instant},
};

use serde_json::{json, Map, Value};

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

/// Stdout of `cmd args...`, trimmed; None if the command fails to run
/// or exits nonzero.
fn command_stdout(cmd: &str, args: &[&str]) -> Option<String> {
    let out = std::process::Command::new(cmd).args(args).output().ok()?;
    if !out.status.success() {
        return None;
    }
    Some(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

/// Provenance half of a run header: wall-clock date and git commit
/// (`-dirty` when the tree has uncommitted changes). Both degrade to
/// "unknown" rather than failing a run.
fn provenance() -> String {
    let date = command_stdout("date", &["+%Y-%m-%d %H:%M:%S %Z"])
        .unwrap_or_else(|| "unknown date".to_string());
    let sha = command_stdout("git", &["rev-parse", "--short", "HEAD"])
        .unwrap_or_else(|| "unknown".to_string());
    let dirty = match command_stdout("git", &["status", "--porcelain"]) {
        Some(s) if !s.is_empty() => "-dirty",
        _ => "",
    };
    format!("{date} | git {sha}{dirty}")
}

/// The evidence log for one run of a case study or baseline: log lines
/// (including per-query LLM stats), derivability results, and raw LLM
/// responses all land in the run's directory, and only a `RunLog` can
/// write them. Constructing one truncates the previous run's log and
/// writes a provenance header (so every log says when and from what
/// code it was produced), and `finish` writes a footer (so a log
/// without one is visibly from an interrupted run). Each run directory
/// must have exactly one writing test; that is what makes truncation
/// safe.
pub struct RunLog {
    dir: String,
    name: String,
    provenance: String,
    started: Instant,
    /// Synthesized artifacts recorded so far (see `record_synthesized`),
    /// written to `results.json` at `finish`.
    synthesized: Map<String, Value>,
}

impl RunLog {
    /// Start a fresh run: truncate `<dir>/log.txt` and stamp it with a
    /// provenance header (date and git commit), so the log always
    /// describes exactly the run that produced the other artifacts in
    /// `dir` (which re-runs overwrite). Construct only after a test's
    /// skip guards and asserts, so a skipped run doesn't wipe the log.
    pub fn start(dir: &str, name: &str) -> Self {
        fs::create_dir_all(dir).unwrap_or_else(|e| panic!("Error creating dir: {}", e));
        let run = Self {
            dir: dir.to_string(),
            name: name.to_string(),
            provenance: provenance(),
            started: Instant::now(),
            synthesized: Map::new(),
        };
        fs::write(run.log_path(), "")
            .unwrap_or_else(|_| panic!("Failed to truncate '{}'", run.log_path()));
        run.line(&format!("=== {name} @ {dir} | {} ===", run.provenance));
        run
    }

    fn log_path(&self) -> String {
        format!("{}/log.txt", self.dir)
    }

    /// Append a line to this run's log, echoing it to stdout so long
    /// runs are observable with --nocapture. (`start` already created
    /// the run directory.)
    pub fn line(&self, line: &str) {
        use std::io::Write;
        let path = self.log_path();
        let mut file = OpenOptions::new()
            .append(true)
            .create(true)
            .open(&path)
            .unwrap_or_else(|_| panic!("Failed to open '{}'", path));
        writeln!(file, "{line}").expect("Unable to write");
        println!("{line}");
    }

    fn raw_file(&self, filename: &str, content: &str) {
        let raw_dir = format!("{}/raw", self.dir);
        fs::create_dir_all(&raw_dir).unwrap_or_else(|e| panic!("Error creating dir: {}", e));
        let path = format!("{raw_dir}/{filename}");
        fs::write(&path, content).unwrap_or_else(|_| panic!("Failed to write '{}'", path));
    }

    /// Record the exact prompt sent for the run's `name` queries in
    /// `<dir>/raw/<name>-prompt.txt`. Reprompts embed rules synthesized
    /// earlier in the same run, so prompt text is run-specific evidence,
    /// not a constant recoverable from the source.
    pub fn raw_prompt(&self, name: &str, prompt: &str) {
        self.raw_file(&format!("{name}-prompt.txt"), prompt);
    }

    /// Record the raw (uncleaned) text of one LLM response in
    /// `<dir>/raw/<name>-<model>-q<attempt>.txt`, where `name`
    /// distinguishes the queries within a run (e.g. "LLM-1").
    pub fn raw_response(&self, name: &str, model: &str, attempt: usize, content: &str) {
        self.raw_file(
            &format!("{name}-{}-q{attempt}.txt", model.replace('/', "-")),
            content,
        );
    }

    /// Compute LhsAndRhs derivability of `against` from `rules` and
    /// record it: a summary line is appended to the run's log, and the
    /// full result is written to
    /// `<dir>/<rules_name>-<against_name>-derive.json` (truncating any
    /// previous run's file, so the json stays valid across re-runs).
    pub fn derivability<L: SynthLanguage>(
        &self,
        rules: &Ruleset<L>,
        rules_name: &str,
        against: &Ruleset<L>,
        against_name: &str,
    ) {
        let start = Instant::now();
        let (can, cannot) = rules.derive(DeriveType::LhsAndRhs, against, Limits::deriving());
        let elapsed = start.elapsed();
        let percent = 100.0 * can.len() as f64 / against.len() as f64;

        self.line(&format!(
            "{rules_name}->{against_name} | {percent:.1}% ({elapsed:.1?})"
        ));

        let v = json!({
            "rules_name": rules_name,
            "against_name": against_name,
            "num_rules": rules.len(),
            "num_against": against.len(),
            "num_can": can.len(),
            "num_cannot": cannot.len(),
            "percent_derivable": percent,
            "time": elapsed.as_secs_f64(),
            "can": can.to_str_vec(),
            "cannot": cannot.to_str_vec()
        });
        let path = format!("{}/{rules_name}-{against_name}-derive.json", self.dir);
        fs::write(&path, serde_json::to_string_pretty(&v).unwrap())
            .unwrap_or_else(|_| panic!("Failed to write '{}'", path));
    }

    /// Record a synthesized artifact (a ruleset or workload):
    /// its final size and the synthesis time the paper reports for it.
    /// Callers accumulate the time across the phases the paper counts
    /// (e.g. LLM queries + validation + minimization). Logged now, and
    /// written to `results.json` at `finish`.
    pub fn record_synthesized(&mut self, name: &str, count: usize, time: Duration) {
        self.line(&format!("{name}: {count} synthesized | total {time:.1?}"));
        self.synthesized.insert(
            name.to_string(),
            json!({"count": count, "time": time.as_secs_f64()}),
        );
    }

    /// End the run: write `<dir>/results.json` (provenance plus every
    /// `synthesized` artifact's size and time — the numbers the paper's
    /// tables cite that derive.json files don't carry) and a footer
    /// line reporting total wall-clock time. Consumes the log, so
    /// nothing can be recorded after it.
    pub fn finish(self) {
        let v = json!({
            "name": self.name,
            "provenance": self.provenance,
            "total_time": self.started.elapsed().as_secs_f64(),
            "synthesized": Value::Object(self.synthesized.clone()),
        });
        let path = format!("{}/results.json", self.dir);
        fs::write(&path, serde_json::to_string_pretty(&v).unwrap())
            .unwrap_or_else(|_| panic!("Failed to write '{}'", path));
        self.line(&format!(
            "=== {} complete | {:.1?} ===",
            self.name,
            self.started.elapsed()
        ));
    }
}

/// Whether to skip computing "a derives b" when writing baseline rows.
pub fn skip_derive(a: &str, b: &str) -> bool {
    // Items in this list will *not* run derivability
    // Format is (a, b) where a and b are spec/baseline names
    // and a.derive(b) will *not* run.
    // Note: b.derive(a) will still be computed unless (b, a)
    // is also in this list.
    let pairs = [
        ("herbie", "rational_replicate"),
        ("herbie", "rational_best"),
    ];

    // Items in this list will not run derivability a.derive(b) for any b.
    // "halide" names both the halide spec row and the Halide compiler
    // baseline; the Halide TRS is not designed for eqsat, so derivability
    // involving it as the deriving side is prohibitively expensive.
    let skip_all = ["halide"];

    skip_all.contains(&a) || pairs.contains(&(a, b))
}

/**
 * Constructs a JSON object that corresponds to a single row of the baseline
 * derivability table (Tables 2 and 3) for a domain synthesized without
 * prior rules. See `write_baseline_with_prior` for the field descriptions.
 */
pub fn write_baseline<L: SynthLanguage>(
    ruleset: &Ruleset<L>,
    spec_name: &str,
    baseline: &Ruleset<L>,
    baseline_name: &str,
    time: Duration,
) {
    write_baseline_with_prior(
        ruleset,
        spec_name,
        baseline,
        baseline_name,
        &Ruleset::default(),
        time,
    )
}

/**
 * Constructs a JSON object that corresponds to a single row of the baseline
 * derivability table (Tables 2 and 3)
 * spec_name: Name of enumo recipe file
 * baseline_name: Baseline to compare against
 * prior: rules the ruleset was synthesized relative to (the arithmetic rules
 *   for the trig and exponential domains). Derivability in *both* directions
 *   unions the prior into the deriving ruleset and never into the target:
 *   the synthesized rules were minimized against the prior, so arithmetic
 *   variants were deliberately dropped from them, and the baseline never
 *   applies its rules without arithmetic either. Pass an empty ruleset for
 *   domains without a prior (or use `write_baseline`).
 * loc: # of lines in enumo recipe
 * rules: array of rules (the prior is not included)
 * prior_rules: array of prior rules
 * time: time in seconds
 * derivability: JSON object containing dervability in both directions for both derive types
 */
pub fn write_baseline_with_prior<L: SynthLanguage>(
    ruleset: &Ruleset<L>,
    spec_name: &str,
    baseline: &Ruleset<L>,
    baseline_name: &str,
    prior: &Ruleset<L>,
    time: Duration,
) {
    let loc = count_lines(spec_name)
        .map(|x| x.to_string())
        .unwrap_or_else(|| "-".to_string());

    let ruleset_with_prior = ruleset.union(prior);
    let baseline_with_prior = baseline.union(prior);

    let enumo_derives_baseline = if skip_derive(spec_name, baseline_name) {
        json!({})
    } else {
        json!({
            "lhs": get_derivability(&ruleset_with_prior, baseline, DeriveType::Lhs),
            "lhs_rhs": get_derivability(&ruleset_with_prior, baseline, DeriveType::LhsAndRhs)
        })
    };

    let baseline_derives_enumo = if skip_derive(baseline_name, spec_name) {
        json!({})
    } else {
        json!({
            "lhs": get_derivability(&baseline_with_prior, ruleset, DeriveType::Lhs),
            "lhs_rhs": get_derivability(&baseline_with_prior, ruleset, DeriveType::LhsAndRhs)
        })
    };

    let row = json!({
      "TYPE": "baseline",
      "spec_name": spec_name,
      "baseline_name": baseline_name,
      "loc": loc,
      "rules": ruleset.to_str_vec(),
      "prior_rules": prior.to_str_vec(),
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
