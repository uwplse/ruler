use reqwest::Client;
use serde_json::json;

use crate::enumo::Workload;

use std::str::FromStr;

const PROMPT_DE_LA_SOPA_ALFABETO: &str = r#"
You are an expert in generating a list of terms for a given
programming language. The syntax of the programming language
is s-expressions. You will be given a list of variables,
constants, and operations. Your task is to generate a list
of interesting terms using the above variables, constants, and operations.
The operations will be a list of list of strings, where operations[i]
will be the list of strings representing arity-i operations.
The terms should be in the s-expression format.
You should try to generate 50 terms, and you must try to generate
terms at each size from 1 to max_size. Outside of single constants,
make sure each term has at least one variable in it.
Avoid generating terms that only involve constants unless they reduce to a single constant.
First generate all size-1 terms, then size-2 terms, then size-3, and continue up to size-{max_size}.


Your response should only be terms, one after another.
Do not include any other text in your response. Do not
include line numbers or any other formatting. Only
include the terms in your response.

Example:

Example Input:
max_size: 4,
vals: ["0", "1"],
vars: ["x", "y"],
ops: [[], ["abs"], ["+", "-", "*", "min", "max"]],

Some example output:
x

(abs x)

(+ x y)
(- x x)
(* x y)
(min x y)
(min y x)
(min 0 x)
(min x 0)

(min (abs x) y)
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
    ops: Vec<Vec<String>>,
    vals: Vec<String>,
}

// asks GPT to generate a list of terms which implement some bigass recipe.
pub async fn alphabet_soup(r: &Recipe) -> Result<Vec<String>, reqwest::Error> {
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
        "seed": 0xbeef,
    });

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

    let result = response_json["choices"][0]["message"]["content"].as_str().unwrap().lines().map(|s| s.to_string()).collect();
    Ok(result)
}

pub fn soup_to_workload(soup: Vec<String>) -> Result<Workload, Box<dyn std::error::Error>> {
    for r in &soup {
        // TODO: should have check here which asserts that this is a valid expression in Halide.
        match crate::enumo::Sexp::from_str(&r) {
            Ok(_) => {}
            Err(e) => {
                return Err(format!("Error parsing expression: {}", e).into());
            }
        }
    }

    let soup_workload = Workload::new(soup);

    Ok(soup_workload)
}

pub mod tests {
    use super::{alphabet_soup, Recipe, soup_to_workload};

    #[tokio::test]
    pub async fn test() {let recipe = Recipe {
            max_size: 3,
            vars: vec!["x".to_string(), "y".to_string()],
            ops: vec![vec![], vec![], vec!["+".to_string(), "-".to_string()]],
            vals: vec!["-1".to_string(), "0".to_string(), "1".to_string()],
        };
        let alphabet_soup = alphabet_soup(&recipe).await;
        assert!(alphabet_soup.is_ok());
        let workload = super::soup_to_workload(alphabet_soup.unwrap()).unwrap();
        println!("the workload is");
        for t in workload.force() {
            println!("{}", t);
        }

    }
}
