use egg::*;
use ruler::*;
use std::fs::File;
use std::io::{Write, BufReader, BufRead, Error};
use std::io::{self};
use std::env;

pub fn parse_rules(line: String) -> (RecExpr<Math>, RecExpr<Math>) {
    let split: Vec<&str> = line
        .split("=>")
        .flat_map(|s| s.split(" <"))
        .filter(|e| *e != "")
        .collect();
    assert_eq!(split.len(), 2);
    let lhs: RecExpr<Math> = split[0].parse().unwrap();
    let rhs: RecExpr<Math> = split[1].parse().unwrap();
    (lhs, rhs)
}

pub fn derive(
    ruler: String,
    other: String,
) -> (
    Vec<(RecExpr<Math>, RecExpr<Math>)>,
    Vec<(RecExpr<Math>, RecExpr<Math>)>,
) {
    let mut derivable = vec![];
    let mut not_derivable = vec![];
    let ruler = File::open(ruler).unwrap();
    let reader = io::BufReader::new(ruler);
    let mut rules: Vec<Equality> = vec![];
    for line in reader.lines() {
        let parsed = parse_rules(line.unwrap());
        let lhs = parsed.0;
        let rhs = parsed.1;
        let rule = Equality::new(&lhs, &rhs);
        rules.push(rule.unwrap());
    }
    let rs = rules.iter().flat_map(|r| &r.rewrites);
    let other = File::open(other).unwrap();
    let reader = io::BufReader::new(other);
    for line in reader.lines() {
        let (l, r) = parse_rules(line.unwrap());
        println!("Testing: {} => {}", l, r);
        let mut egraph = EGraph::<Math, SynthAnalysis>::default();
        egraph.add_expr(&l);
        egraph.add_expr(&r);
        let runner = Runner::default()
            .with_egraph(egraph)
            .with_iter_limit(7)
            .run(rs.clone());
        if runner.egraph.equivs(&l, &r).len() != 0 {
            derivable.push((l, r));
        } else {
            not_derivable.push((l, r));
        }
    }
    (derivable, not_derivable)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        panic!("Provide two rule files");
    }
    let ruler = &args[1];
    let other = &args[2];
    let (derivable, not_derivable) = derive(ruler.to_string(), other.to_string());
    println!("{} rules are derivable from Ruler rules", derivable.len());
    println!("{} rules are not derivable from Ruler rules", not_derivable.len());
    let mut df = File::create("derivable.txt")?;
    let mut ndf = File::create("notderivable.txt")?;
    for d in derivable {
        let wd = format!("{} => {}", d.0.pretty(100), d.1.pretty(100));
        write!(df, "{}\n", wd)?;
    }
    for d in not_derivable {
        let wd = format!("{} => {}", d.0.pretty(100), d.1.pretty(100));
        write!(ndf, "{}\n", wd)?;
    }
    Ok(())
}