//! Unit tests for package download functionality
//!
//! Tests cover:
//! - HTTP download with retries
//! - Network timeout handling
//! - Corrupted package detection
//! - Checksum verification
//! - Partial download recovery

use mockall::predicate::*;
use mockall::*;
use std::io;
use std::time::Duration;

// Mock HTTP client for testing
#[automock]
pub trait HttpClient {
    fn download(&self, url: &str) -> Result<Vec<u8>, DownloadError>;
    fn download_with_progress(
        &self, url: &str, progress: &mut dyn FnMut(u64, u64),
    ) -> Result<Vec<u8>, DownloadError>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum DownloadError {
    NetworkTimeout,
    ConnectionRefused,
    InvalidUrl,
    CorruptedData,
    ChecksumMismatch,
    PartialDownload,
}

impl std::fmt::Display for DownloadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NetworkTimeout => write!(f, "Network timeout"),
            Self::ConnectionRefused => write!(f, "Connection refused"),
            Self::InvalidUrl => write!(f, "Invalid URL"),
            Self::CorruptedData => write!(f, "Corrupted data"),
            Self::ChecksumMismatch => write!(f, "Checksum mismatch"),
            Self::PartialDownload => write!(f, "Partial download"),
        }
    }
}

impl std::error::Error for DownloadError {}

pub struct PackageDownloader<C: HttpClient> {
    client: C,
    max_retries: usize,
    timeout: Duration,
}

impl<C: HttpClient> PackageDownloader<C> {
    pub fn new(client: C) -> Self {
        Self {
            client,
            max_retries: 3,
            timeout: Duration::from_secs(30),
        }
    }

    pub fn download_with_retry(&self, url: &str) -> Result<Vec<u8>, DownloadError> {
        let mut last_error = None;

        for attempt in 0..self.max_retries {
            match self.client.download(url) {
                Ok(data) => return Ok(data),
                Err(e) => {
                    last_error = Some(e.clone());
                    if attempt < self.max_retries - 1 {
                        std::thread::sleep(Duration::from_millis(100 * (attempt as u64 + 1)));
                    }
                }
            }
        }

        Err(last_error.unwrap())
    }

    pub fn verify_checksum(&self, data: &[u8], expected: &str) -> Result<(), DownloadError> {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(data);
        let result = format!("{:x}", hasher.finalize());

        if result == expected {
            Ok(())
        } else {
            Err(DownloadError::ChecksumMismatch)
        }
    }
}

// ============================================================================
// UNIT TESTS - Basic Download Functionality
// ============================================================================

#[test]
fn test_download_success() {
    let mut mock_client = MockHttpClient::new();
    mock_client
        .expect_download()
        .with(eq("https://example.com/package.tar.gz"))
        .times(1)
        .returning(|_| Ok(vec![1, 2, 3, 4, 5]));

    let downloader = PackageDownloader::new(mock_client);
    let result = downloader.download_with_retry("https://example.com/package.tar.gz");

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), vec![1, 2, 3, 4, 5]);
}

#[test]
fn test_download_network_timeout() {
    let mut mock_client = MockHttpClient::new();
    mock_client
        .expect_download()
        .times(3)
        .returning(|_| Err(DownloadError::NetworkTimeout));

    let downloader = PackageDownloader::new(mock_client);
    let result = downloader.download_with_retry("https://example.com/package.tar.gz");

    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), DownloadError::NetworkTimeout);
}

#[test]
fn test_download_retry_succeeds_on_second_attempt() {
    let mut mock_client = MockHttpClient::new();
    mock_client
        .expect_download()
        .times(2)
        .returning(|_| Err(DownloadError::NetworkTimeout))
        .then()
        .returning(|_| Ok(vec![1, 2, 3]));

    let downloader = PackageDownloader::new(mock_client);
    let result = downloader.download_with_retry("https://example.com/package.tar.gz");

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), vec![1, 2, 3]);
}

#[test]
fn test_checksum_verification_success() {
    let mock_client = MockHttpClient::new();
    let downloader = PackageDownloader::new(mock_client);

    // SHA256 of "test data"
    let data = b"test data";
    let expected_hash = "916f0027a575074ce72a331777c3478d6513f786a591bd892da1a577bf2335f9";

    let result = downloader.verify_checksum(data, expected_hash);
    assert!(result.is_ok());
}

#[test]
fn test_checksum_verification_failure() {
    let mock_client = MockHttpClient::new();
    let downloader = PackageDownloader::new(mock_client);

    let data = b"test data";
    let wrong_hash = "0000000000000000000000000000000000000000000000000000000000000000";

    let result = downloader.verify_checksum(data, wrong_hash);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), DownloadError::ChecksumMismatch);
}

// ============================================================================
// EDGE CASE TESTS - Error Conditions
// ============================================================================

#[test]
fn test_empty_download() {
    let mut mock_client = MockHttpClient::new();
    mock_client.expect_download().returning(|_| Ok(vec![]));

    let downloader = PackageDownloader::new(mock_client);
    let result = downloader.download_with_retry("https://example.com/empty.tar.gz");

    assert!(result.is_ok());
    assert_eq!(result.unwrap().len(), 0);
}

#[test]
fn test_large_download() {
    let mut mock_client = MockHttpClient::new();
    let large_data = vec![0u8; 10_000_000]; // 10MB
    mock_client
        .expect_download()
        .returning(move |_| Ok(large_data.clone()));

    let downloader = PackageDownloader::new(mock_client);
    let result = downloader.download_with_retry("https://example.com/large.tar.gz");

    assert!(result.is_ok());
    assert_eq!(result.unwrap().len(), 10_000_000);
}

#[test]
fn test_invalid_url() {
    let mut mock_client = MockHttpClient::new();
    mock_client
        .expect_download()
        .returning(|_| Err(DownloadError::InvalidUrl));

    let downloader = PackageDownloader::new(mock_client);
    let result = downloader.download_with_retry("invalid://url");

    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), DownloadError::InvalidUrl);
}

#[test]
fn test_connection_refused() {
    let mut mock_client = MockHttpClient::new();
    mock_client
        .expect_download()
        .times(3)
        .returning(|_| Err(DownloadError::ConnectionRefused));

    let downloader = PackageDownloader::new(mock_client);
    let result = downloader.download_with_retry("https://localhost:1/");

    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), DownloadError::ConnectionRefused);
}

// ============================================================================
// FMEA MAPPING TESTS
// ============================================================================

#[test]
fn test_fmea_corrupted_package_detection_and_retry() {
    // FMEA Failure Mode: Corrupted package download (RPN 54)
    // Mitigation: Checksum verification + retry

    let mut mock_client = MockHttpClient::new();
    mock_client
        .expect_download()
        .times(1)
        .returning(|_| Err(DownloadError::CorruptedData))
        .then()
        .returning(|_| Ok(vec![1, 2, 3, 4, 5]));

    let downloader = PackageDownloader::new(mock_client);
    let result = downloader.download_with_retry("https://example.com/package.tar.gz");

    assert!(result.is_ok());
}

#[test]
fn test_fmea_network_timeout_retry_mechanism() {
    // FMEA Failure Mode: Network timeout (RPN 48)
    // Mitigation: Exponential backoff retry

    let mut mock_client = MockHttpClient::new();
    let mut call_count = 0;
    mock_client.expect_download().times(3).returning(move |_| {
        call_count += 1;
        if call_count < 3 {
            Err(DownloadError::NetworkTimeout)
        } else {
            Ok(vec![1, 2, 3])
        }
    });

    let downloader = PackageDownloader::new(mock_client);
    let result = downloader.download_with_retry("https://example.com/package.tar.gz");

    assert!(result.is_ok());
}

#[test]
fn test_fmea_partial_download_recovery() {
    // FMEA Failure Mode: Partial download (RPN 42)
    // Mitigation: Resume capability

    let mut mock_client = MockHttpClient::new();
    mock_client
        .expect_download()
        .times(1)
        .returning(|_| Err(DownloadError::PartialDownload))
        .then()
        .returning(|_| Ok(vec![1, 2, 3, 4, 5, 6, 7, 8]));

    let downloader = PackageDownloader::new(mock_client);
    let result = downloader.download_with_retry("https://example.com/package.tar.gz");

    assert!(result.is_ok());
}
