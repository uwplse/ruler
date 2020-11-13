use ruler::*;
use std::io::{self, BufRead};
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
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        if let Some(eq) = convert(&line) {
            println!("{}", eq);
        } else {
            eprintln!("Failed to make eq for {}", line);
        }
    }
}
