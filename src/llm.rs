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

const PROMPT_DE_LA_SOPA_ALFABETO_CON_CONDICIONES: &str = r#"
Good job! Now let's do it again, but with conditions. Your input will be the same as before:
it will be a list of variables, constants, and operations. However, this time instead of generating
terms that will be useful in rewrite situations, you generate conditions which can be used to
generate equalities between the terms you generated in the previous step.

Like before, you will generate terms in the s-expression format, one after another, up to
max_size. You will also generate conditions which will be used to compare terms.

Your response should only be terms, one after another.
Do not include any other text in your response. Do not
include line numbers or any other formatting. Only
include the terms in your response.

Example Input:
Here are the terms from the last step:
a
(abs a)

Here is the recipe for the terms you should generate:
max_size: 4,
vals: ["0"],
vars: ["a"],
ops: [[], ["abs"], ["<", "<=", "!="]],

Example Output:
(< a 0)
(<= a 0)
(< a (abs a))
...


Input:
Here are the terms from the last step:
{last_step_workload}

Here is the recipe for the terms you should generate:
max_size: {max_size}
vals: {vals},
vars: {vars},
ops: {ops},
"#;

#[derive(Clone)]
pub struct Recipe {
    pub max_size: usize,
    pub vars: Vec<String>,
    pub ops: Vec<Vec<String>>,
    pub vals: Vec<String>,
}

pub async fn generate_alphabet_soup(term_recipe: &Recipe, cond_r: Option<&Recipe>) -> (Workload, Option<Workload>) {
    let client = Client::new();

    let soup = alphabet_soup(&client, &term_recipe).await.unwrap();

    // Convert the generated soup into a workload
    let term_workload = soup_to_workload(soup.clone()).unwrap();

    if let Some(cond_recipe) = cond_r {
        // If a condition recipe is provided, generate conditions based on the previous workload.
        let condition_workload = condition_soup(&client, soup, cond_recipe).await.unwrap();

        // Convert the generated conditions into a workload
        let cond_workload = soup_to_workload(condition_workload).unwrap();

        (term_workload, Some(cond_workload))
    } else {
        (term_workload, None)
    }

}

pub async fn condition_soup(client: &Client, term_workload_as_vec: Vec<String>, r: &Recipe) -> Result<Vec<String>, reqwest::Error> {
    // TODO: @ninehusky -- check that term workload vars are superset of recipe vars.
    let content = PROMPT_DE_LA_SOPA_ALFABETO_CON_CONDICIONES
        .replace("{last_step_workload}",
            // This will be the workload from the previous step, which is a list of terms.
            &term_workload_as_vec.join("\n"))
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

    let response = client
        .post("https://api.openai.com/v1/chat/completions") // <-- Using Responses API
        .header("Authorization", format!("Bearer {}", std::env::var("OPENAI_API_KEY").expect("OPENAI_API_KEY not set")))
        .header("Content-Type", "application/json")
        .json(&request_body)
        .send()
        .await?;

    let response_json: serde_json::Value = response.json().await?;

    let result = response_json["choices"][0]["message"]["content"].as_str().unwrap().lines().map(|s| s.to_string()).collect();

    Ok(result)
}



// asks GPT to generate a list of terms which implement some bigass recipe.
pub async fn alphabet_soup(client: &Client, r: &Recipe) -> Result<Vec<String>, reqwest::Error> {
    let url = "https://api.openai.com/v1/chat/completions";
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
        .header("Authorization", format!("Bearer {}", std::env::var("OPENAI_API_KEY").expect("OPENAI_API_KEY not set")))
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
    use super::{generate_alphabet_soup, Recipe, soup_to_workload};

    #[tokio::test]
    pub async fn test() {
        let recipe = Recipe {
            max_size: 3,
            vars: vec!["x".to_string(), "y".to_string()],
            ops: vec![vec![], vec![], vec!["abs".to_string()], vec![
                "+".to_string(),
                "-".to_string(),
                "*".to_string(),
                "min".to_string(),
                "max".to_string(),
            ]],
            vals: vec!["-1".to_string(), "0".to_string(), "1".to_string(), "2".to_string()],
        };

        let cond_recipe = Recipe {
            max_size: 3,
            vars: recipe.vars.clone(), // Use the same variables as the term recipe
            ops: vec![vec![], vec![], vec!["<".to_string(), "<=".to_string(), "!=".to_string()]],
            vals: vec!["0".to_string()],
        };

        let soup_workloads = generate_alphabet_soup(&recipe, &Some(cond_recipe)).await;

        println!("the workload is");
        for t in soup_workloads.0.force() {
            println!("{}", t);
        }

        println!("the condition workload is");
        for t in soup_workloads.1.clone().unwrap().force() {
            println!("{}", t);
        }

    }
}
