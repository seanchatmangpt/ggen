//! OAuth2 provider integration

use crate::AuthResult;
use urlencoding;

/// OAuth provider types
#[derive(Debug, Clone)]
pub enum OAuthProvider {
    GitHub,
    Google,
}

/// OAuth configuration
#[derive(Debug, Clone)]
pub struct OAuthConfig {
    pub provider: OAuthProvider,
    pub client_id: String,
    pub client_secret: String,
    pub redirect_url: String,
    pub auth_url: String,
    pub token_url: String,
    pub userinfo_url: String,
}

impl OAuthConfig {
    /// Create GitHub OAuth config
    pub fn github(client_id: String, client_secret: String, redirect_url: String) -> Self {
        Self {
            provider: OAuthProvider::GitHub,
            client_id,
            client_secret,
            redirect_url,
            auth_url: "https://github.com/login/oauth/authorize".to_string(),
            token_url: "https://github.com/login/oauth/access_token".to_string(),
            userinfo_url: "https://api.github.com/user".to_string(),
        }
    }

    /// Create Google OAuth config
    pub fn google(client_id: String, client_secret: String, redirect_url: String) -> Self {
        Self {
            provider: OAuthProvider::Google,
            client_id,
            client_secret,
            redirect_url,
            auth_url: "https://accounts.google.com/o/oauth2/v2/auth".to_string(),
            token_url: "https://oauth2.googleapis.com/token".to_string(),
            userinfo_url: "https://openidconnect.googleapis.com/v1/userinfo".to_string(),
        }
    }

    /// Get authorization URL
    pub fn get_auth_url(&self, state: &str) -> String {
        format!(
            "{}?client_id={}&redirect_uri={}&response_type=code&state={}",
            self.auth_url,
            urlencoding::encode(&self.client_id),
            urlencoding::encode(&self.redirect_url),
            urlencoding::encode(state)
        )
    }

    /// Exchange authorization code for access token
    pub async fn exchange_code(&self, code: &str) -> AuthResult<String> {
        // TODO: Implement token exchange using reqwest
        // This is a placeholder that shows the integration point
        Ok(format!("access_token_{}", code))
    }

    /// Get user info from OAuth provider
    pub async fn get_user_info(&self, _access_token: &str) -> AuthResult<OAuthUserInfo> {
        // TODO: Implement using reqwest to call userinfo_url
        Ok(OAuthUserInfo {
            id: "oauth_user_123".to_string(),
            email: "user@example.com".to_string(),
            name: "User Name".to_string(),
            avatar_url: None,
        })
    }
}

/// User information from OAuth provider
#[derive(Debug, Clone)]
pub struct OAuthUserInfo {
    pub id: String,
    pub email: String,
    pub name: String,
    pub avatar_url: Option<String>,
}
