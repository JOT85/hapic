use std::fmt::Debug;

use http::{Request, Response};

/// Implemented by response bodies.
#[async_trait::async_trait]
pub trait ResponseBody: Send {
    type Error: Send + Sync + Debug + Into<super::Error>;
    type Bytes: AsRef<[u8]>;

    /// Consume the entire body and return all the bytes.
    async fn read_all(self) -> Result<Self::Bytes, Self::Error>;
}

/// A generic client capable of making HTTP requests.
#[async_trait::async_trait]
pub trait Transport<B>: Send + Sync {
    /// Errors generated from making a HTTP request.
    type Error: Debug + Into<super::Error>;

    /// The type of response body returned by this transport.
    type ResponseBody: ResponseBody;

    /// Make a HTTP request and return the response.
    async fn request(&self, req: Request<B>) -> Result<Response<Self::ResponseBody>, Self::Error>;
}

#[cfg(feature = "hyper")]
#[async_trait::async_trait]
impl<T> ResponseBody for T
where
    T: hyper::body::HttpBody + Send,
    T::Data: Send,
    T::Error: Debug + Send + Sync + Into<super::Error>,
{
    type Bytes = hyper::body::Bytes;
    type Error = T::Error;

    async fn read_all(self) -> Result<hyper::body::Bytes, T::Error> {
        hyper::body::to_bytes(self).await
    }
}

#[cfg(feature = "hyper")]
#[async_trait::async_trait]
impl<C, B> Transport<B> for hyper::Client<C, B>
where
    C: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
    B: hyper::body::HttpBody + Send + 'static,
    B::Data: Send,
    B::Error: Send + Sync + Debug + Into<super::Error> + std::error::Error + 'static,
{
    type Error = hyper::Error;
    type ResponseBody = hyper::Body;

    async fn request(&self, req: Request<B>) -> Result<Response<hyper::Body>, hyper::Error> {
        hyper::Client::request(self, req).await
    }
}

#[cfg(feature = "hyper")]
/// An implementation of `HttpsTransport` using `hyper` and `hyper_tls`.
pub type HttpsTransport<B = hyper::Body> =
    hyper::Client<hyper_tls::HttpsConnector<hyper::client::connect::HttpConnector>, B>;
