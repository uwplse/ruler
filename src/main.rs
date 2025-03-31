use ruler::llm::Recipe;
use ruler::llm;
use ruler::halide;
use ruler::halide::Pred;
use ruler::enumo::Ruleset;

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
            // let default_recipe = Recipe {
            //     max_size: 5,
            //     vars: vec!["x".to_string(), "y".to_string()], // Variables to use in the terms
            //     ops: vec![
            //         vec![],
            //         vec![],
            //         vec![
            //             "+".to_string(),
            //             "-".to_string(),
            //             "*".to_string(),
            //             "min".to_string(),
            //             "max".to_string(),
            //         ],
            //     ],
            //     vals: vec!["0".to_string(), "1".to_string()], // Values to use in the terms
            // };

            // let condition_recipe = Recipe {
            //     max_size: 3,
            //     vars: default_recipe.vars.clone(), // Use the same variables as the term recipe. You got to.
            //     ops: vec![
            //         vec![], // No 0-ary operators
            //         vec![], // No unary operators
            //         vec!["<".to_string(), "<=".to_string(), "!=".to_string()], // Conditional operators
            //     ],
            //     vals: vec!["0".to_string()], // Values to use in the conditions.
            // };
            // let (workload, cond_workload) = llm::generate_alphabet_soup(&default_recipe, Some(&condition_recipe)).await;
            // let ruleset = halide::soup_to_rules(&workload, Some(&cond_workload.unwrap()), &Ruleset::default(), 5);
            // println!("the ruleset is:");
            // for r in ruleset.iter() {
            //     println!("{}", r);
            // }
            run_gpt_eval().await; // This will run the GPT evaluation and generate the rules.

        }
        ChompyMode::LLMRecipes => {}
    }
    println!("Hello, world!");
}


// This ports over the recipes from `halide.rs` over to the GPT infrastructure.
pub async fn run_gpt_eval() {
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
            vec!["+".to_string(), "-".to_string(), "*".to_string(), "/".to_string()], // Binary operators
        ],
    };

    let rat_cond_recipe = Some(Recipe {
        max_size: 3,
        vals: vec!["0".to_string()], // Values to use in the conditions.
        vars: rat_recipe.vars.clone(),
        ops: vec![
            vec![],
            vec![],
            vec!["<".to_string(), "<=".to_string(), "!=".to_string()], // Conditional operators
        ],
    });

    let recipe_list = vec![
        (equality_recipe, equality_cond_recipe),
        (bool_recipe, bool_cond_recipe),
        (rat_recipe, rat_cond_recipe),
    ];

    let mut prior_ruleset: Ruleset<Pred> = Ruleset::default();

    for (recipe, cond_recipe) in recipe_list {
        // blegh
        let (workload, cond_r) = llm::generate_alphabet_soup(&recipe, cond_recipe.as_ref()).await;
        let ruleset = halide::soup_to_rules(
            &workload,
            cond_r.as_ref(),
            &prior_ruleset,
            recipe.max_size as usize // The maximum size for the ruleset.
        );

        // Merge the ruleset into the prior ruleset.
        prior_ruleset.extend(ruleset);
    }

    println!("Final ruleset:");
    for r in prior_ruleset.iter() {
        println!("{}", r);
    }
}
