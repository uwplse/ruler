//! Querying LLMs (via OpenRouter) for rule and workload candidates.
//!
//! Requires `OPENROUTER_API_KEY` in the environment (tests and case
//! studies load it from a `.env` file, which is gitignored).

use std::env;
use std::io::Write;

use openai_api_rs::v1::{
    api::OpenAIClient,
    chat_completion::{self, ChatCompletionRequest},
};

/// The models to query. Each `Ruleset::from_llm` / `Workload::from_llm`
/// call queries every model and combines the (deduplicated) results.
pub fn models() -> Vec<String> {
    vec![
        "google/gemini-3.6-flash".to_string(),
        "openai/gpt-5.6-luna".to_string(),
        "anthropic/claude-sonnet-5".to_string(),
    ]
}

/// Send `prompt` to `model` and return the response as cleaned lines:
/// anything after a `;` is treated as a comment and stripped, and blank
/// lines are dropped. The cleaned response is also written to
/// `llm/out/<model>-response.txt` for offline inspection.
/// Returns no lines (with a message on stderr) if the query fails.
pub async fn query(prompt: &str, model: &str) -> Vec<String> {
    println!("Querying {model}");
    let api_key = env::var("OPENROUTER_API_KEY").expect("OPENROUTER_API_KEY not set");
    let mut client = OpenAIClient::builder()
        .with_endpoint("https://openrouter.ai/api/v1")
        .with_api_key(api_key)
        .build()
        .expect("Failed to build OpenRouter client");

    let req = ChatCompletionRequest::new(
        model.to_string(),
        vec![chat_completion::ChatCompletionMessage {
            role: chat_completion::MessageRole::user,
            content: chat_completion::Content::Text(prompt.to_string()),
            name: None,
            tool_calls: None,
            tool_call_id: None,
        }],
    );

    match client.chat_completion(req).await {
        Ok(res) => {
            let content = res.choices.first().and_then(|c| c.message.content.clone());
            let Some(content) = content else {
                eprintln!("Empty response from {model}");
                return vec![];
            };

            let lines: Vec<String> = content
                .lines()
                .map(|line| line.split(';').next().unwrap_or("").trim())
                .filter(|line| !line.is_empty())
                .map(String::from)
                .collect();

            std::fs::create_dir_all("llm/out").expect("Failed to create llm/out");
            let filename = format!("llm/out/{}-response.txt", model.replace('/', "-"));
            let mut file = std::fs::File::create(&filename)
                .unwrap_or_else(|_| panic!("Failed to create '{}'", filename));
            writeln!(file, "{}", lines.join("\n")).expect("Unable to write");

            lines
        }
        Err(e) => {
            eprintln!("Error querying {model}: {e:?}");
            vec![]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_query() {
        // Skip this test in github actions
        if std::env::var("CI").is_ok() && std::env::var("SKIP_RECIPES").is_ok() {
            return;
        }
        dotenv::dotenv().ok();
        // Skip (rather than fail) when no API key is configured locally
        if std::env::var("OPENROUTER_API_KEY").is_err() {
            eprintln!("Skipping test_query: OPENROUTER_API_KEY not set");
            return;
        }

        let prompt =
            "What are the standard Boolean Algebra Axioms? Print one axiom per line, plain text.";
        for model in models() {
            let response = query(prompt, &model).await;
            assert!(!response.is_empty(), "empty response from {}", model);
        }
    }
}
