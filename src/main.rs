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

pub fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        panic!("Usage: chompy <mode> <output>");
    }
    let mode = ChompyMode::from_str(&args[1]);
    match mode {
        ChompyMode::HandwrittenRecipes => {}
        ChompyMode::LLMAlphabetSoup => {}
        ChompyMode::LLMRecipes => {}
    }
    println!("Hello, world!");
}
