use ruler::*;
use std::io::{self, BufRead};
use symbolic_expressions::{parser::parse_str, Sexp};

fn replace(s: &str) -> RecExpr {
    let s = s
        .replace("bvadd", "+")
        .replace("bvmul", "*")
        .replace("bvand", "&")
        .replace("bvor", "|")
        .replace("bvneg", "-")
        .replace("bvnot", "~")
        .replace("bvlshr", ">>")
        .replace("bvshl", "<<")
        .replace("#b0000", "0");
    s.parse().unwrap()
}

fn convert(s: &str) -> Equality {
    let sexp = parse_str(s).unwrap();
    let list = sexp.list().unwrap();
    assert_eq!(list[0], Sexp::String("candidate-rewrite".into()));
    let l = list[1].to_string();
    let r = list[2].to_string();
    Equality::new(&replace(&l), &replace(&r)).unwrap()
}

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let eq = convert(&line);
        println!("{}", eq);
    }
}
