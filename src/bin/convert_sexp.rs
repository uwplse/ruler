use regex::Regex;
/// A utility to convert cvc rules into a ruler report
use ruler::*;
use std::env;
use std::io::{self, BufRead};
use std::path::Path;
use symbolic_expressions::{parser::parse_str, Sexp};

fn replace(s: &str) -> RecExpr {
    let s = s
        .replace("bvadd", "+")
        .replace("bvsub", "--")
        .replace("bvmul", "*")
        .replace("bvand", "&")
        .replace("bvor", "|")
        .replace("bvneg", "-")
        .replace("bvnot", "~")
        .replace("bvlshr", ">>")
        .replace("bvshl", "<<")
        .replace("#b0000", "0")
        .replace("#b0111", "7")
        .replace("#b1000", "8")
        .replace("true", "255")
        .replace("false", "0")
        .replace("and", "&")
        .replace("xor", "^")
        .replace("or", "|")
        .replace("not", "~");
    assert!(!s.contains('#'));
    s.parse().unwrap()
}

fn convert(s: &str) -> Option<Equality> {
    let sexp = parse_str(s).unwrap();
    let list = sexp.list().unwrap();
    assert!(
        list[0] == Sexp::String("candidate-rewrite".into())
            || list[0] == Sexp::String("rewrite".into())
    );
    let l = list[1].to_string();
    let r = list[2].to_string();
    Equality::new(&replace(&l), &replace(&r))
}

fn main() {
    let _ = env_logger::try_init();
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Provide one cvc log file");
    }

    let filename = Path::new(&args[1]);

    let re = Regex::new(r"(.*)-(.*)vars-(.*)iters.txt").unwrap();
    let base = filename.file_name().unwrap().to_str().unwrap();
    let caps = re.captures(base).unwrap();
    let domain: Domain = caps[1]
        .parse()
        .unwrap_or_else(|_| panic!("couldn't parse domain '{}'", &caps[1]));
    let variables: usize = caps[2].parse().expect("couldn't parse vars");
    let iters: usize = caps[3].parse().expect("couldn't parse iters");
    let params = SynthParams {
        domain,
        variables,
        iters,
        outfile: Some(filename.to_string_lossy().into_owned()),
        seed: 0,
        n_samples: 0,
        constants: vec![],
        rules_to_take: 0,
        chunk_size: None,
    };

    let file = std::fs::File::open(filename).expect("can't open file");
    let reader = io::BufReader::new(file);

    let mut report = Report {
        params,
        time: -1.0,
        eqs: vec![],
    };

    for line in reader.lines() {
        let line = line.unwrap();
        if line.contains("rewrite") {
            if let Some(eq) = convert(&line) {
                report.eqs.push(eq);
            } else {
                eprintln!("Failed to make eq for {}", line);
            }
        } else if line.starts_with("real") {
            let s = line[4..].trim();
            report.time = s.parse().unwrap();
        };
    }

    assert_ne!(report.time, -1.0);
    println!("{}", serde_json::to_string_pretty(&report).unwrap())
}
