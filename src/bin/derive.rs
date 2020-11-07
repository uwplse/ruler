use ruler::*;
use egg::*;
use std::fs::File;
use std::io::{self, BufRead};

pub fn parse_rules(line: String) -> (RecExpr<Math>, RecExpr<Math>) {
    let split: Vec<&str> = line.split("=>").flat_map(|s| s.split(" <")).filter(|e| *e !="").collect();
    assert_eq!(split.len(), 2);
    let lhs: RecExpr<Math> = split[0].parse().unwrap();
    let rhs: RecExpr<Math> = split[1].parse().unwrap();
    (lhs, rhs) 
}

pub fn derive(ruler: String, other: String) -> Vec<(RecExpr<Math>, RecExpr<Math>)> {
    let mut derivable = vec![];
    let ruler = File::open(ruler).unwrap();
    let reader = io::BufReader::new(ruler);
    let mut rules : Vec<Equality> = vec![];
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
        let runner = Runner::default().with_egraph(egraph).with_iter_limit(3).run(rs.clone());
        if runner.egraph.equivs(&l, &r).len() != 0 {
            derivable.push((l, r));
        } 
    }
    derivable 
}

fn main() {
    let derivable = derive("out/ruler.txt".to_string(), "out/bv.txt".to_string());
    println!("{} rules are derivable from Ruler rules", derivable.len());
}