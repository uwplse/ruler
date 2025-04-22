use std::env;

use openai_api_rs::v1::{
    api::OpenAIClient,
    chat_completion::{self, ChatCompletionRequest},
};

pub async fn query(prompt: &str) -> String {
    let api_key = env::var("OPENROUTER_API_KEY").expect("API_KEY not set");
    let mut client = OpenAIClient::builder()
        .with_endpoint("https://openrouter.ai/api/v1")
        .with_api_key(api_key)
        .build()
        .unwrap();
    let req = ChatCompletionRequest::new(
        // this is the cheapest reasonable model for programming tasks
        // definitely good enough for now, consider trying other models once
        // things are built out more.
        "google/gemini-flash-1.5".to_string(),
        vec![chat_completion::ChatCompletionMessage {
            role: chat_completion::MessageRole::user,
            content: chat_completion::Content::Text(prompt.to_string()),
            name: None,
            tool_calls: None,
            tool_call_id: None,
        }],
    );
    client.chat_completion(req).await.unwrap().choices[0]
        .message
        .content
        .clone()
        .unwrap()
}
