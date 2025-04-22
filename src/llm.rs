use openai_api_rs::v1::{
    api::OpenAIClient,
    chat_completion::{self, ChatCompletionRequest},
};

pub async fn from_llm(grammar: &str) -> String {
    let prompt = format!(
        r#"
    Your task is to aid in rule inference for equality saturation.
    The domain is boolean logic. The grammar is
    
    {}

    Your task is to generate terms from the grammar, from which a set of rewrite rules can be inferred.
    The terms should be generated in a way that they are likely to lead to interesting rewrite rules.
    The terms may use up to three variables: x, y, and z.

    Please generate 1000 terms. Each term should be on its own line.
    Print only the terms, one term per line, no additional text or explanation.
    "#,
        grammar
    );
    let mut client = OpenAIClient::builder()
        .with_endpoint("https://openrouter.ai/api/v1")
        .with_api_key("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")
        .build()
        .unwrap();
    let req = ChatCompletionRequest::new(
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
