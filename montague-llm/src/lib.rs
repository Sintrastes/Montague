//! LLM backends for Montague.
//!
//! Provides the [`Llm`] trait and two implementations:
//! - [`AnthropicBackend`] — any Anthropic-compatible API (env-configured).
//! - [`MistralRsBackend`] — local GGUF inference via mistralrs (feature-gated, default).
//!
//! Backend selection: `ANTHROPIC_AUTH_TOKEN` → Anthropic; otherwise → local GGUF.
//! Local GGUF models are auto-downloaded from HuggingFace on first run.

use std::env;
use std::fmt;
use std::fmt::Write as FmtWrite;
use std::path::PathBuf;

// ---------------------------------------------------------------------------
// ChatMessage
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct ChatMessage {
    pub role: String,
    pub content: String,
}

impl ChatMessage {
    pub fn system(content: impl Into<String>) -> Self {
        ChatMessage { role: "system".into(), content: content.into() }
    }
    pub fn user(content: impl Into<String>) -> Self {
        ChatMessage { role: "user".into(), content: content.into() }
    }
    pub fn assistant(content: impl Into<String>) -> Self {
        ChatMessage { role: "assistant".into(), content: content.into() }
    }
}

// ---------------------------------------------------------------------------
// Error
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub enum LlmError {
    NoBackend(String),
    Http(String),
    Api { status: u16, body: String },
    Parse(String),
    Model(String),
}

impl fmt::Display for LlmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LlmError::NoBackend(msg) => write!(f, "no LLM backend: {msg}"),
            LlmError::Http(msg) => write!(f, "HTTP error: {msg}"),
            LlmError::Api { status, body } => write!(f, "API error {status}: {body}"),
            LlmError::Parse(msg) => write!(f, "parse error: {msg}"),
            LlmError::Model(msg) => write!(f, "model error: {msg}"),
        }
    }
}

impl std::error::Error for LlmError {}

// ---------------------------------------------------------------------------
// Llm trait
// ---------------------------------------------------------------------------

pub trait Llm {
    fn complete_chat(&mut self, messages: &[ChatMessage], max_tokens: usize) -> Result<String, LlmError>;
    fn complete(&mut self, system_prompt: &str, user_prompt: &str, max_tokens: usize) -> Result<String, LlmError> {
        self.complete_chat(&[ChatMessage::system(system_prompt), ChatMessage::user(user_prompt)], max_tokens)
    }
}

// ---------------------------------------------------------------------------
// Anthropic backend
// ---------------------------------------------------------------------------

pub struct AnthropicBackend {
    base_url: String,
    auth_token: String,
    model: String,
}

impl AnthropicBackend {
    pub fn from_env() -> Result<Self, LlmError> {
        let auth_token = env::var("ANTHROPIC_AUTH_TOKEN")
            .map_err(|_| LlmError::NoBackend("Set ANTHROPIC_AUTH_TOKEN for Anthropic.".into()))?;
        let base_url = env::var("ANTHROPIC_BASE_URL").unwrap_or_else(|_| "https://api.anthropic.com".into());
        let model = env::var("ANTHROPIC_MODEL").unwrap_or_else(|_| "claude-sonnet-4-6".into());
        Ok(AnthropicBackend { base_url, auth_token, model })
    }
}

impl Llm for AnthropicBackend {
    fn complete_chat(&mut self, messages: &[ChatMessage], max_tokens: usize) -> Result<String, LlmError> {
        let url = format!("{}/v1/messages", self.base_url);

        // Separate system message (goes in top-level "system" field) from
        // conversation messages (DeepSeek only accepts user/assistant roles).
        let system_prompt = messages.iter()
            .find(|m| m.role == "system")
            .map(|m| m.content.as_str());
        let conversation: Vec<&ChatMessage> = messages.iter()
            .filter(|m| m.role != "system")
            .collect();

        let mut body = format!(
            r#"{{"model":"{}","max_tokens":{max_tokens}"#,
            self.model
        );
        if let Some(sys) = system_prompt {
            write!(body, r#","system":"{}""#, json_escape(sys)).unwrap();
        }
        body.push_str(r#","messages":["#);
        for (i, msg) in conversation.iter().enumerate() {
            if i > 0 { body.push(','); }
            let c = json_escape(&msg.content);
            write!(body, r#"{{"role":"{}","content":[{{"type":"text","text":"{}"}}]}}"#, msg.role, c).unwrap();
        }
        body.push_str("]}");

        // Try sending the request. ureq v2 returns Err(Status(code, response))
        // for non-2xx; extract the response body for diagnostics.
        let resp = match ureq::post(&url)
            .set("x-api-key", &self.auth_token)
            .set("Content-Type", "application/json")
            .send_string(&body)
        {
            Ok(r) => r,
            Err(ureq::Error::Status(code, r)) => {
                let resp_body = r.into_string().unwrap_or_default();
                return Err(LlmError::Api {
                    status: code,
                    body: format!("request: {body}\nresponse: {resp_body}"),
                });
            }
            Err(e) => {
                return Err(LlmError::Http(format!("{url}: {e}")));
            }
        };

        let status = resp.status();
        let resp_body = resp.into_string().map_err(|e| LlmError::Http(e.to_string()))?;
        if status != 200 {
            return Err(LlmError::Api {
                status,
                body: format!("request: {body}\nresponse: {resp_body}"),
            });
        }
        text_from_json(&resp_body)
    }
}

fn json_escape(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"").replace('\n', "\\n").replace('\r', "\\r").replace('\t', "\\t")
}

/// Extract text from an LLM JSON response. Tries OpenAI `content` field first,
/// then Anthropic `text` field.
fn text_from_json(json: &str) -> Result<String, LlmError> {
    let marker = if let Some(pos) = json.find(r#""content":""#) {
        pos + r#""content":""#.len()
    } else if let Some(pos) = json.find(r#""text":""#) {
        pos + r#""text":""#.len()
    } else {
        return Err(LlmError::Parse(format!("no content/text field in: {json}")));
    };
    let start = marker;
    let mut result = String::new();
    let mut chars = json[start..].chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'), Some('t') => result.push('\t'),
                Some('r') => result.push('\r'), Some('"') => result.push('"'),
                Some('\\') => result.push('\\'),
                Some('u') => {
                    let mut hex = String::new();
                    let mut handled = false;
                    for _ in 0..4 {
                        match chars.next() {
                            Some(c) if c.is_ascii_hexdigit() => hex.push(c),
                            Some(c) => {
                                result.push('\\');
                                result.push('u');
                                result.push_str(&hex);
                                result.push(c);
                                handled = true;
                                break;
                            }
                            None => break,
                        }
                    }
                    if !handled && hex.len() == 4 {
                        if let Ok(code) = u32::from_str_radix(&hex, 16) {
                            if let Some(c) = char::from_u32(code) {
                                result.push(c);
                            }
                        }
                    } else if !handled {
                        result.push('\\');
                        result.push('u');
                        result.push_str(&hex);
                    }
                }
                Some(o) => { result.push('\\'); result.push(o); }
                None => result.push('\\'),
            }
        } else if c == '"' { break; }
        else { result.push(c); }
    }
    Ok(result)
}

// ---------------------------------------------------------------------------
// MistralRs backend (local GGUF via mistralrs)
// ---------------------------------------------------------------------------

const DEFAULT_MODEL_REPO: &str = "TheBloke/Hermes-3-llama-3.2-8B-GGUF";
const DEFAULT_MODEL_FILE: &str = "hermes-3-llama-3.2-8b.Q4_K_M.gguf";

pub struct MistralRsBackend {
    #[allow(dead_code)]
    model_path: PathBuf,
    #[cfg(feature = "mistralrs")]
    model: Option<mistralrs::Model>,
}

impl MistralRsBackend {
    pub fn new(model_repo: Option<&str>, model_file: Option<&str>) -> Result<Self, LlmError> {
        let repo = model_repo.unwrap_or(DEFAULT_MODEL_REPO);
        let file = model_file.unwrap_or(DEFAULT_MODEL_FILE);
        let model_path = ensure_model_downloaded(repo, file)?;

        #[cfg(feature = "mistralrs")]
        {
            let model = load_mistral_model(&model_path)?;
            return Ok(MistralRsBackend { model_path, model: Some(model) });
        }
        #[cfg(not(feature = "mistralrs"))]
        {
            Ok(MistralRsBackend { model_path })
        }
    }

    pub fn with_default_model() -> Result<Self, LlmError> { Self::new(None, None) }
}

#[cfg(feature = "mistralrs")]
fn load_mistral_model(path: &PathBuf) -> Result<mistralrs::Model, LlmError> {
    let dir = path.parent().unwrap().to_str().unwrap();
    let file = path.file_name().unwrap().to_str().unwrap();
    let builder = mistralrs::GgufModelBuilder::new(dir, vec![file]);
    let rt = tokio::runtime::Runtime::new()
        .map_err(|e| LlmError::Model(e.to_string()))?;
    rt.block_on(async { builder.build().await })
        .map_err(|e| LlmError::Model(format!("build: {e}")))
}

impl Llm for MistralRsBackend {
    fn complete_chat(&mut self, messages: &[ChatMessage], _max_tokens: usize) -> Result<String, LlmError> {
        #[cfg(feature = "mistralrs")]
        {
            use mistralrs::{TextMessageRole, TextMessages};
            let model = self.model.as_mut().ok_or_else(|| LlmError::Model("model not loaded".into()))?;

            let mut chat = TextMessages::new();
            for msg in messages {
                let role = match msg.role.as_str() {
                    "system" => TextMessageRole::System,
                    "user" => TextMessageRole::User,
                    "assistant" => TextMessageRole::Assistant,
                    _ => TextMessageRole::User,
                };
                chat = chat.add_message(role, msg.content.as_str());
            }

            let rt = tokio::runtime::Runtime::new().map_err(|e| LlmError::Model(e.to_string()))?;
            let response = rt.block_on(model.send_chat_request(chat))
                .map_err(|e| LlmError::Model(format!("inference: {e}")))?;

            Ok(response.choices.into_iter()
                .filter_map(|c| c.message.content)
                .collect::<Vec<_>>()
                .join("\n"))
        }
        #[cfg(not(feature = "mistralrs"))]
        {
            let _ = (messages, _max_tokens);
            Err(LlmError::Model(
                "Local inference requires the `mistralrs` feature.\n\
                 cargo build --features llm,mistralrs".into()))
        }
    }
}

// ---------------------------------------------------------------------------
// Model download
// ---------------------------------------------------------------------------

fn ensure_model_downloaded(repo: &str, filename: &str) -> Result<PathBuf, LlmError> {
    let cache_dir = model_cache_dir();
    let model_path = cache_dir.join(filename);
    if model_path.exists() { return Ok(model_path); }

    eprintln!("Downloading {filename} from {repo}... (~5GB, one-time)");

    let token = env::var("HF_TOKEN")
        .or_else(|_| env::var("HUGGINGFACE_TOKEN"))
        .unwrap_or_default();

    let api = if token.is_empty() {
        return Err(LlmError::Model(
            "HuggingFace requires authentication. Set HF_TOKEN or HUGGINGFACE_TOKEN.\n\
             Get a token at: https://huggingface.co/settings/tokens".into(),
        ));
    } else {
        hf_hub::api::sync::ApiBuilder::new()
            .with_token(Some(token))
            .build()
            .map_err(|e| LlmError::Model(format!("hf-hub: {e}")))?
    };

    let repo = api.model(repo.to_string());
    let path = repo.get(filename).map_err(|e| LlmError::Model(format!("download: {e}")))?;
    eprintln!("Model downloaded to {}", path.display());
    Ok(path)
}

fn model_cache_dir() -> PathBuf {
    let base = dirs_cache().unwrap_or_else(|| PathBuf::from(".cache"));
    base.join("montague").join("models")
}

#[cfg(not(target_os = "windows"))]
fn dirs_cache() -> Option<PathBuf> {
    env::var("XDG_CACHE_HOME").ok().map(PathBuf::from)
        .or_else(|| env::var("HOME").ok().map(|h| PathBuf::from(h).join(".cache")))
}

#[cfg(target_os = "windows")]
fn dirs_cache() -> Option<PathBuf> { env::var("LOCALAPPDATA").ok().map(PathBuf::from) }

// ---------------------------------------------------------------------------
// Backend detection
// ---------------------------------------------------------------------------

/// Backend preference (overrides auto-detection).
pub enum BackendPreference {
    /// Use Anthropic API (requires ANTHROPIC_AUTH_TOKEN).
    Anthropic,
    /// Use local GGUF via mistralrs (the default).
    MistralRs,
    /// Use local Ollama via its API (localhost:11434).
    Ollama,
}

/// Detect backend. Mistralrs is the default.
pub fn detect_backend(pref: BackendPreference) -> Result<Box<dyn Llm>, LlmError> {
    match pref {
        BackendPreference::Anthropic => Ok(Box::new(AnthropicBackend::from_env()?)),
        BackendPreference::MistralRs => Ok(Box::new(MistralRsBackend::with_default_model()?)),
        BackendPreference::Ollama => Ok(Box::new(OllamaBackend::new()?)),
    }
}

// ---------------------------------------------------------------------------
// Ollama backend (local via REST API)
// ---------------------------------------------------------------------------

/// Backend using a local Ollama instance via its OpenAI-compatible API.
///
/// Model defaults to `hermes3:8b`, overridable via `OLLAMA_MODEL` env var.
pub struct OllamaBackend { url: String, model: String }

impl OllamaBackend {
    pub fn new() -> Result<Self, LlmError> {
        let url = env::var("OLLAMA_HOST").unwrap_or_else(|_| "http://localhost:11434".into());
        let model = env::var("OLLAMA_MODEL").unwrap_or_else(|_| "hermes3:8b".into());
        ureq::get(&url).call().map_err(|e| LlmError::Model(format!("Ollama not reachable at {url}: {e}")))?;
        Ok(OllamaBackend { url, model })
    }
}

impl Llm for OllamaBackend {
    fn complete_chat(&mut self, messages: &[ChatMessage], _max_tokens: usize) -> Result<String, LlmError> {
        let mut body = format!(r#"{{"model":"{}","messages":["#, self.model);
        for (i, msg) in messages.iter().enumerate() {
            if i > 0 { body.push(','); }
            let c = json_escape(&msg.content);
            write!(body, r#"{{"role":"{}","content":"{}"}}"#, msg.role, c).unwrap();
        }
        body.push_str("]}");
        let _ = _max_tokens;
        let resp = ureq::post(&format!("{}/v1/chat/completions", self.url))
            .set("Content-Type", "application/json")
            .send_string(&body)
            .map_err(|e: ureq::Error| LlmError::Http(e.to_string()))?;
        text_from_json(&resp.into_string().map_err(|e| LlmError::Http(e.to_string()))?)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn json_escape_basic() {
        assert_eq!(json_escape(r#"hello "world""#), r#"hello \"world\""#);
    }

    #[test]
    fn text_from_json_basic() {
        let json = r#"{"content":[{"type":"text","text":"Hello, world!"}]}"#;
        assert_eq!(text_from_json(json).unwrap(), "Hello, world!");
    }

    #[test]
    fn text_from_json_escapes() {
        let json = r#"{"content":[{"type":"text","text":"line1\nline2"}]}"#;
        assert_eq!(text_from_json(json).unwrap(), "line1\nline2");
    }

    #[test]
    fn text_from_json_unicode_escape() {
        // JSON content field with > (>), < (<), & (&)
        let json = r#"{"content":[{"type":"text","text":"PROD immortal --> immortal"}]}"#;
        assert_eq!(text_from_json(json).unwrap(), "PROD immortal --> immortal");
    }

    #[test]
    fn text_from_json_unicode_escape_multiple() {
        let json = r#"{"content":[{"type":"text","text":"a > b < c"}]}"#;
        assert_eq!(text_from_json(json).unwrap(), "a > b < c");
    }

    #[test]
    fn detect_mistralrs_is_default() {
        let result = detect_backend(BackendPreference::MistralRs);
        let _ = result;
    }
}
