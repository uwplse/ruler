use std::env;
use std::io::Write;

use openai_api_rs::v1::{
    api::OpenAIClient,
    chat_completion::{self, ChatCompletionRequest},
};

pub fn models() -> Vec<String> {
    vec![
        "google/gemini-flash-1.5".to_string(),
        "anthropic/claude-3.7-sonnet".to_string(),
        // "google/gemini-2.5-pro-preview".to_string(),
        "openai/gpt-4o-mini".to_string(),
    ]
}

pub async fn query(prompt: &str, model: &str) -> Vec<String> {
    let api_key = env::var("OPENROUTER_API_KEY").expect("API_KEY not set");
    let mut client = OpenAIClient::builder()
        .with_endpoint("https://openrouter.ai/api/v1")
        .with_api_key(api_key)
        .build()
        .unwrap();
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

    let res = client.chat_completion(req).await;
    if res.is_ok() {
        let res = res.unwrap().choices[0].message.content.clone().unwrap();

        let lines: Vec<String> = res
            .lines()
            .map(|line| line.split(';').next().unwrap_or("").trim()) // take before `;`, trim whitespace
            .filter(|line| !line.is_empty()) // skip empty lines
            .map(String::from)
            .collect();

        let filename = format!("llm/out/{}-response.txt", model.replace("/", "-"));
        let mut file =
            std::fs::File::create(filename).unwrap_or_else(|_| panic!("Failed to open file"));
        writeln!(file, "{}", lines.join("\n")).expect("unable to write");

        lines
    } else {
        eprintln!("Error: {:?}", res);
        vec![]
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

        let models: Vec<String> = models();
        let prompt = "What are the standard Boolean Algebra Axioms?";
        for model in models {
            let response = query(prompt, &model).await;
            assert!(!response.is_empty());
        }
    }
}
