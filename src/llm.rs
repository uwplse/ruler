//! Querying LLMs (via OpenRouter) for rule and workload candidates.
//!
//! Requires `OPENROUTER_API_KEY` in the environment (tests and case
//! studies load it from a `.env` file, which is gitignored).

use std::env;
use std::time::Instant;

use crate::logger::RunLog;

use openai_api_rs::v1::{
    api::OpenAIClient,
    chat_completion::{self, ChatCompletionRequest},
};

/// Prompt every model in `models()` with `prompt` (twice per model),
/// feeding each cleaned response line to `add`. `add` returns how many
/// items the line contributed to the caller's collection (0 for a
/// duplicate), or `None` if the line could not be parsed (such lines
/// are reported and counted). One stats line per query — response
/// size, yield, and time — is appended to the run's log, so every
/// query's outcome leaves a durable trace even when the caller's
/// stdout is lost; each raw response is recorded under `name` (see
/// `RunLog::raw_response`).
pub async fn query_each(
    prompt: &str,
    log: &RunLog,
    name: &str,
    mut add: impl FnMut(&str) -> Option<usize>,
) {
    log.raw_prompt(name, prompt);
    for model in models() {
        for attempt in 1..=2 {
            let start = Instant::now();
            let lines = query(prompt, &model, attempt, log, name).await;
            let mut new = 0;
            let mut invalid = 0;
            for line in &lines {
                match add(line) {
                    Some(n) => new += n,
                    None => {
                        invalid += 1;
                        eprintln!("Skipping invalid line from {model}: {line}");
                    }
                }
            }
            log.line(&format!(
                "{model} (query {attempt}) | {} lines | {new} new ({invalid} invalid) | {:.1?}",
                lines.len(),
                start.elapsed()
            ));
        }
    }
}

/// The models to query. Each `Ruleset::from_llm` / `Workload::from_llm`
/// call queries every model twice and combines the (deduplicated)
/// results, to reduce variance in model output.
pub fn models() -> Vec<String> {
    vec![
        "google/gemini-3.6-flash".to_string(),
        "openai/gpt-5.6-luna".to_string(),
        "anthropic/claude-sonnet-5".to_string(),
    ]
}

/// Send `prompt` to `model` and return the response as cleaned lines:
/// anything after a `;` is treated as a comment and stripped, and blank
/// lines are dropped. The raw (uncleaned) response is recorded in the
/// run's `raw/` directory (see `RunLog::raw_response`), with `name`
/// distinguishing the queries within the run (e.g. "LLM-1"). Errors
/// (which return no lines) are appended to the run's log; success
/// stats are logged by `query_each`.
pub async fn query(
    prompt: &str,
    model: &str,
    attempt: usize,
    log: &RunLog,
    name: &str,
) -> Vec<String> {
    println!("Querying {model} (query {attempt})");
    let start = Instant::now();
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
                log.line(&format!(
                    "{model} (query {attempt}) | ERROR: empty response | {:.1?}",
                    start.elapsed()
                ));
                return vec![];
            };

            log.raw_response(name, model, attempt, &content);

            let lines: Vec<String> = content
                .lines()
                .map(|line| line.split(';').next().unwrap_or("").trim())
                .filter(|line| !line.is_empty())
                .map(String::from)
                .collect();

            lines
        }
        Err(e) => {
            log.line(&format!(
                "{model} (query {attempt}) | ERROR: {e:?} | {:.1?}",
                start.elapsed()
            ));
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
        let log = RunLog::start("llm/out", "test_query");
        for model in models() {
            let response = query(prompt, &model, 1, &log, "test").await;
            assert!(!response.is_empty(), "empty response from {}", model);
        }
        log.finish();
    }
}
