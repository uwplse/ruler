use ruler::llm::Recipe;
use ruler::llm;
use ruler::halide;
use ruler::halide::Pred;
use ruler::enumo::Ruleset;

use std::str::FromStr;

// Outlines how to perform Halide rule synthesis.
pub enum ChompyMode {
    HandwrittenRecipes,
    LLMAlphabetSoup,
    LLMRecipes,
}

impl FromStr for ChompyMode {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "handwritten" => Ok(Self::HandwrittenRecipes),
            "llm_alphabet_soup" => Ok(Self::LLMAlphabetSoup),
            "llm_recipes" => Ok(Self::LLMRecipes),
            _ => Err("Invalid mode.".to_string()),
        }
    }
}

#[tokio::main]
pub async fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        panic!("Usage: chompy <mode> <output_file_path>");
    }
    let mode = ChompyMode::from_str(&args[1]).unwrap();
    let output_file = &args[2];
    let rules = match mode {
        ChompyMode::HandwrittenRecipes => {
            halide::handwritten_recipes()
        }
        ChompyMode::LLMAlphabetSoup => {
            run_gpt_eval().await
        }
        ChompyMode::LLMRecipes => {
            todo!("Not implemented yet.");
        }
    };
    rules.to_file(output_file);
}


pub async fn run_gpt_eval() -> Ruleset<Pred> {
    let equality_recipe = Recipe {
        max_size: 5,
        vals: vec!["-1".to_string(), "0".to_string(), "1".to_string(), "2".to_string()],
        vars: vec!["a".to_string(), "b".to_string(), "c".to_string()],
        ops: vec![
            vec![],
            vec!["!".to_string()],
            vec!["==".to_string(), "!=".to_string(), "<".to_string(), ">".to_string(), "<=".to_string(), ">=".to_string(), "min".to_string(), "max".to_string()], // Conditional operators
        ],
    };

    let equality_cond_recipe = None;

    let bool_recipe = Recipe {
        max_size: 5,
        vals: vec!["0".to_string(), "1".to_string()],
        vars: vec!["a".to_string(), "b".to_string(), "c".to_string()],
        ops: vec![
            vec![],
            vec!["!".to_string()],
            vec!["&&".to_string(), "||".to_string()],
        ],
    };

    let bool_cond_recipe = None;

    let rat_recipe = Recipe {
        max_size: 5,
        vals: vec!["-1".to_string(), "0".to_string(), "1".to_string(), "2".to_string()],
        vars: vec!["a".to_string(), "b".to_string(), "c".to_string()],
        ops: vec![
            vec![],
            vec!["abs".to_string()],
            vec!["+".to_string(), "-".to_string(), "*".to_string(), "/".to_string()],
        ],
    };

    let rat_cond_recipe = Some(Recipe {
        max_size: 3,
        vals: vec!["0".to_string()],
        vars: rat_recipe.vars.clone(),
        ops: vec![
            vec![],
            vec![],
            vec!["<".to_string(), "<=".to_string(), "!=".to_string()],
        ],
    });

    let recipe_list = vec![
        (equality_recipe, equality_cond_recipe),
        (bool_recipe, bool_cond_recipe),
        (rat_recipe, rat_cond_recipe),
    ];

    let mut prior_ruleset: Ruleset<Pred> = Ruleset::default();

    for (recipe, cond_recipe) in recipe_list {
        let (workload, cond_r) = llm::generate_alphabet_soup(&recipe, cond_recipe.as_ref()).await;
        let ruleset = halide::soup_to_rules(
            &workload,
            cond_r.as_ref(),
            &prior_ruleset,
            recipe.max_size
        );

        prior_ruleset.extend(ruleset);
    }

    prior_ruleset
}
