use ruler::llm::Recipe;
use ruler::llm;
use ruler::halide;

// Outlines how to perform Halide rule synthesis.
pub enum ChompyMode {
    HandwrittenRecipes,
    LLMAlphabetSoup,
    LLMRecipes,
}

impl ChompyMode {
    pub fn from_str(s: &str) -> Self {
        match s {
            "handwritten" => Self::HandwrittenRecipes,
            "llm_alphabet_soup" => Self::LLMAlphabetSoup,
            "llm_recipes" => Self::LLMRecipes,
            _ => panic!("Invalid ChompyMode: {}", s),
        }
    }
}

#[tokio::main]
pub async fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        panic!("Usage: chompy <mode> <output>");
    }
    let mode = ChompyMode::from_str(&args[1]);
    match mode {
        ChompyMode::HandwrittenRecipes => {}
        ChompyMode::LLMAlphabetSoup => {
            let default_recipe = Recipe {
                max_size: 5,
                vars: vec!["x".to_string(), "y".to_string()], // Variables to use in the terms
                ops: vec![
                    vec![],
                    vec![],
                    vec![
                        "+".to_string(),
                        "-".to_string(),
                        "*".to_string(),
                        "min".to_string(),
                        "max".to_string(),
                    ],
                ],
                vals: vec!["0".to_string(), "1".to_string()], // Values to use in the terms
            };

            let condition_recipe = Recipe {
                max_size: 3,
                vars: default_recipe.vars.clone(), // Use the same variables as the term recipe. You got to.
                ops: vec![
                    vec![], // No 0-ary operators
                    vec![], // No unary operators
                    vec!["<".to_string(), "<=".to_string(), "!=".to_string()], // Conditional operators
                ],
                vals: vec!["0".to_string()], // Values to use in the conditions.
            };
            let (workload, cond_workload) = llm::generate_alphabet_soup(&default_recipe, Some(&condition_recipe)).await;
            let ruleset = halide::soup_to_rules(&workload, &cond_workload.unwrap(), 5);
            println!("the ruleset is:");
            for r in ruleset.iter() {
                println!("{}", r);
            }

        }
        ChompyMode::LLMRecipes => {}
    }
    println!("Hello, world!");
}
