use reqwest::Client;
use serde_json::json;

use crate::enumo::Workload;

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

    // Define request payload for the Responses API
    let request_body = json!({
        "model": "gpt-4o",  // Correct model
        "messages": [
            {
                "role": "system",
                "content": "You are a helpful assistant that generates a list of terms for a given recipe."
            },
            {
                "role": "user",
                "content": format!("Generate a list of terms for the following recipe: max_size: {}, vars: {:?}, ops: {:?}, vals: {:?}. They should all be in the s-expression format.", r.max_size, r.vars, r.ops, r.vals)
            }
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

    println!("the output: {}", response_json["choices"][0]["message"]["content"]);


    Ok(Workload::empty())
}

pub mod tests {
    use super::{alphabet_soup, Recipe};

    #[tokio::test]
    pub async fn test() {
        let recipe = Recipe {
            max_size: 3,
            vars: vec!["x".to_string()],
            ops: vec!["+".to_string(), "-".to_string()],
            vals: vec!["0".to_string(), "1".to_string()],
        };
        let alphabet_soup = alphabet_soup(&recipe).await;

    }
}
