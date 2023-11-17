use std::time::Duration;

use http::{Request, Response, StatusCode};

use super::Transport;

/// A wrapper around another transport, which may retry requests if they fail.
///
/// Use [`super::HttpsRetryTransport`] for an implementation using `hyper`.
pub struct RetryTransport<C, B, T>
where
    C: ShouldRetry<T, B>,
    B: Clone + Send + Sync,
    T: Transport<B>,
{
    underlying_transport: T,
    phantom_body: std::marker::PhantomData<B>,
    should_retry: C,
}

impl<C, B, T> RetryTransport<C, B, T>
where
    C: ShouldRetry<T, B>,
    B: Clone + Send + Sync,
    T: Transport<B>,
{
    /// Create a new `RetryTransport`, which will retry the request if `should_retry` returns
    /// `true`.
    ///
    /// `should_retry` takes the response, and an attempt number.
    pub fn new(underlying_transport: T, should_retry: C) -> RetryTransport<C, B, T> {
        RetryTransport {
            underlying_transport,
            should_retry,
            phantom_body: std::marker::PhantomData,
        }
    }
}

#[async_trait::async_trait]
impl<C, B, T> Transport<B> for RetryTransport<C, B, T>
where
    C: ShouldRetry<T, B>,
    B: Clone + Send + Sync,
    T: Transport<B>,
{
    type Error = T::Error;
    type ResponseBody = T::ResponseBody;

    async fn request(&self, req: Request<B>) -> Result<Response<T::ResponseBody>, T::Error> {
        // We can't write `for attempt in 0..`; the typechecker doesn't like it :(
        let mut attempt = 0;
        loop {
            let resp = self.underlying_transport.request(clone_request(&req)).await;
            if self.should_retry.should_retry(&resp, attempt).await {
                attempt += 1;
                continue;
            }
            return resp;
        }
    }
}

/// FIXME: Remove when https://github.com/hyperium/http/pull/634 is released
fn clone_request<B: Clone>(req: &Request<B>) -> Request<B> {
    let mut cloned_body = http::request::Builder::new()
        .method(req.method().clone())
        .uri(req.uri().clone())
        .version(req.version().clone());
    for (k, v) in req.headers().iter() {
        cloned_body = cloned_body.header(k, v);
    }
    cloned_body.body(req.body().clone()).unwrap()
}

/// A trait to decide if a request should be retried.
#[async_trait::async_trait]
pub trait ShouldRetry<T: Transport<B>, B>: Send + Sync {
    /// The result of a request is passed, along with the attempt number (initially 0, incremented
    /// each time a request is retried). If `false` is returned, the result is accepted, otherwise
    /// the request is retried.
    ///
    /// `should_retry` is async, and so can sleep to cause a backoff.
    async fn should_retry(
        &self,
        result: &Result<Response<T::ResponseBody>, T::Error>,
        attempts: u16,
    ) -> bool;
}

/// A sensible config, implementing `ShouldRetry`.
#[derive(Clone, Debug)]
pub struct RetryConfig {
    /// The behaviour when a 503 Service Unavailable response is received.
    pub service_unavailable: RetryAfterConfig,
    /// The behaviour when a 429 Too Many Requests response is received.
    pub too_many_requests: RetryAfterConfig,
    /// The behaviour when any other response, with status >=400, is received, and has a
    /// `Retry-After` header.
    pub other_retry_after: RetryAfterConfig,

    /// When none of the other cases are met, the maximum number of retries.
    pub retry_on_error_limit: u16,
    /// When none of the other cases are met, the timeout between retries.
    pub retry_on_error_timeout: Duration,
}

impl RetryConfig {
    pub fn new(
        max_single_wait: Duration,
        default_single_wait: Duration,
        attempt_limit: u16,
    ) -> RetryConfig {
        let retry_after = RetryAfterConfig {
            attempt_limit,
            max_timeout: max_single_wait,
            default_timeout: Some(default_single_wait),
        };
        RetryConfig {
            service_unavailable: retry_after,
            too_many_requests: retry_after,
            other_retry_after: retry_after,

            retry_on_error_limit: attempt_limit,
            retry_on_error_timeout: default_single_wait,
        }
    }
}

/// Part of [`RetryConfig`].
#[derive(Copy, Clone, Debug)]
pub struct RetryAfterConfig {
    pub attempt_limit: u16,
    pub max_timeout: Duration,
    pub default_timeout: Option<Duration>,
}

impl RetryAfterConfig {
    fn parse_retry_after<T>(&self, resp: &Response<T>, attempts: u16) -> Option<Duration> {
        if self.attempt_limit <= attempts {
            return None;
        }
        resp.headers()
            .get("Retry-After")
            .and_then(|secs| {
                secs.to_str()
                    .ok()
                    .and_then(|secs| u16::from_str_radix(secs, 10).ok())
                    .map(|secs| secs as u64)
                    .map(Duration::from_secs)
            })
            .or(self.default_timeout)
            .map(|timeout| timeout.min(self.max_timeout))
    }

    async fn should_retry<T>(&self, resp: &Response<T>, attempts: u16) -> bool {
        match self.parse_retry_after(resp, attempts) {
            Some(retry_after) => {
                tokio::time::sleep(retry_after).await;
                true
            }
            None => false,
        }
    }
}

#[async_trait::async_trait]
impl<T: Transport<B>, B> ShouldRetry<T, B> for RetryConfig {
    async fn should_retry(
        &self,
        result: &Result<Response<T::ResponseBody>, T::Error>,
        attempts: u16,
    ) -> bool {
        match result {
            Ok(resp) if resp.status() == StatusCode::SERVICE_UNAVAILABLE => {
                self.service_unavailable.should_retry(resp, attempts).await
            }
            Ok(resp) if resp.status() == StatusCode::TOO_MANY_REQUESTS => {
                self.too_many_requests.should_retry(resp, attempts).await
            }
            Ok(resp)
                if resp.status().as_u16() >= 400 && resp.headers().contains_key("Retry-After") =>
            {
                self.other_retry_after.should_retry(resp, attempts).await
            }
            Ok(_) => false,

            Err(_) if attempts < self.retry_on_error_limit => {
                tokio::time::sleep(self.retry_on_error_timeout).await;
                true
            }
            Err(_) => false,
        }
    }
}
