use reqwest::Client;
use serde_json::json;

use crate::enumo::Workload;

const PROMPT_DE_LA_SOPA_ALFABETO: &str = r#"
You are an expert in generating a list of terms for a given
programming language. The syntax of the programming language
is s-expressions. You will be given a list of variables,
constants, and operations. Your task is to generate a list
of terms using the above variables, constants, and operations.
The terms should be in the s-expression format. These terms
will be used to generate rewrites for a program synthesis task.

Note that you should **not** generate terms that do not include
at least one variable. You should also avoid generating terms
with a size greater than the max_size. Most importantly, you should
not generate terms that include values that are not in the vals list,
variables that are not in the vars list, or operations that are not
in the ops list.

Do not include terms with constant terms which
can be simplified. For example, the expression
(+ 1 (- x 3)) should not be included,
but a term like (+ y (- x y)) should be included.

Your response should only be terms, one after another.
Do not include any other text in your response. Do not
include line numbers or any other formatting. Only
include the terms in your response.

Example:

Example Input:
max_size: 3,
vals: ["0", "1"],
vars: ["x", "y"],
ops: ["+", "-", "*", "min", "max"],

Some example output:
(+ x y)
(- x y)
(* x y)
(min x y)
(min y x)
(min 0 x)
(min x 0)
...

Input:
Here is the recipe for the terms you should generate:
max_size: {max_size}
vals: {vals},
vars: {vars},
ops: {ops},
"#;

struct Recipe {
    max_size: usize,
    vars: Vec<String>,
    ops: Vec<String>,
    vals: Vec<String>,
}

// asks GPT to generate a list of terms which implement some bigass recipe.
pub async fn alphabet_soup(r: &Recipe) -> Result<Workload, reqwest::Error> {
    let client = Client::new();
    let url = "https://api.openai.com/v1/chat/completions";
    let api_key = std::env::var("OPENAI_API_KEY").expect("OPENAI_API_KEY not set");

    let client = Client::new();

    let content = PROMPT_DE_LA_SOPA_ALFABETO
        .replace("{max_size}", &r.max_size.to_string())
        .replace("{vals}", format!("{:?}", r.vals).as_str())
        .replace("{vars}", format!("{:?}", r.vars).as_str())
        .replace("{ops}", format!("{:?}", r.ops).as_str());

    // Define request payload for the Responses API
    let request_body = json!({
        "model": "gpt-4o",  // Correct model
        "messages": [
            {
                "role": "system",
                "content": content,
            },
        ],
        "seed": 0xbeef,});

    println!("SENDING REQUEST TO: {}", url);

    let response = client
        .post("https://api.openai.com/v1/chat/completions") // <-- Using Responses API
        .header("Authorization", format!("Bearer {}", api_key))
        .header("Content-Type", "application/json")
        .json(&request_body)
        .send()
        .await?;

    println!("response status: {}", response.status());
    let response_json: serde_json::Value = response.json().await?;

    println!("the output: {}", response_json["choices"][0]["message"]["content"]);


    Ok(Workload::empty())
}

pub mod tests {
    use super::{alphabet_soup, Recipe};

    #[tokio::test]
    pub async fn test() {
        let recipe = Recipe {
            max_size: 3,
            vars: vec!["x".to_string(), "y".to_string()],
            ops: vec!["+".to_string(), "-".to_string()],
            vals: vec!["0".to_string(), "1".to_string()],
        };
        let alphabet_soup = alphabet_soup(&recipe).await;

    }
}
