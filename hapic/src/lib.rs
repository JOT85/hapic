//! A Rust crate for quickly creating nice-to-use client libraries for HTTP APIs, in particular,
//! there's lots of tooling around HTTP JSON APIs.
//!
//! **This is still a work in progress.**
//!
//! ## Example: defining a JSON API client
//!
//! ### Super Simple
//!
//! We'll start with a simple dummy API, it has the following endpoints:
//!
//! ```text
//! >> POST /add
//! >> { "a": 2, "b": 3 }
//! << { "c": 5 }
//!
//! >> POST /sub
//! >> { "a": 6, "b": 3 }
//! << { "c": 3 }
//!
//! >> POST /factorial
//! >> { "a": 4 }
//! << { "c": 24 }
//! ```
//!
//! We can define a client for this API as such:
//!
//! ```
//! use hapic::json_api;
//! use serde::{Deserialize, Serialize};
//!
//! #[derive(Serialize)]
//! struct Add {
//!     a: u32,
//!     b: u32,
//! }
//!
//! #[derive(Serialize)]
//! struct Sub {
//!     a: u32,
//!     b: u32,
//! }
//!
//! #[derive(Serialize)]
//! struct Factorial {
//!     a: u8,
//! }
//!
//! #[derive(Deserialize, PartialEq, Eq, Debug)]
//! struct Output {
//!     c: u64,
//! }
//!
//! json_api!(
//!     struct TestClient<B, T: Transport<B>>;
//!     trait TestApiCall;
//!
//!     simple {
//!         "/add": Add => Output;
//!         "/sub": Sub => Output;
//!         "/factorial": Factorial => Output;
//!     }
//! );
//! ```
//!
//! Now, the call types (`Add`, `Sub` and `Factorial`) all implement `TestApiCall`.
//!
//! We can make an API call (to `http://localhost:8080`) using:
//!
//! ```
//! # use hapic::json_api;
//! # use serde::{Deserialize, Serialize};
//! #
//! # #[derive(Serialize)]
//! # struct Add {
//! #     a: u32,
//! #     b: u32,
//! # }
//! #
//! # #[derive(Serialize)]
//! # struct Sub {
//! #     a: u32,
//! #     b: u32,
//! # }
//! #
//! # #[derive(Serialize)]
//! # struct Factorial {
//! #     a: u8,
//! # }
//! #
//! # #[derive(Deserialize, PartialEq, Eq, Debug)]
//! # struct Output {
//! #     c: u64,
//! # }
//! #
//! # json_api!(
//! #     struct TestClient<B, T: Transport<B>>;
//! #     trait TestApiCall;
//! #
//! #     simple {
//! #         "/add": Add => Output;
//! #         "/sub": Sub => Output;
//! #         "/factorial": Factorial => Output;
//! #     }
//! # );
//! # async fn example() {
//! let client = TestClient::new("http://localhost:8000".into());
//! let output = client.call(Add { a: 1, b: 2 }).await.unwrap();
//! assert_eq!(output, Output { c: 3 });
//! # }
//! ```
//!
//! ### With Conversions
//!
//! Now suppose we have a single endpoint for all these operations:
//!
//! ```text
//! >> POST /op
//! >> { "a": 2, "b": 3, "operation": "add" }
//! << { "c": 5 }
//!
//! >> POST /op
//! >> { "a": 6, "b": 3, "operation": "sub" }
//! << { "c": 3 }
//!
//! >> POST /op
//! >> { "a": 4, "operation": "factorial" }
//! << { "c": 24 }
//! ```
//!
//! We *could* define the type:
//!
//! ```
//! #[derive(serde::Serialize)]
//! struct Operation {
//!     operation: &'static str,
//!     a: u32,
//!     #[serde(skip_serializing_if = "Option::is_none")]
//!     b: Option<u32>,
//! }
//! ```
//!
//! This this isn't very idiomatic! Firstly, the user can put any string as the operation, but we
//! know our API server only accepts `add`, `sub` or `factorial`. In addition, if `b` is `Some` in a
//! factorial operation, or is `None` in `add` or `sub`, we'll have an invalid call.
//!
//! We want to make this safe, so we can define two types, one for the API call, and another which
//! will be sent as the JSON body.
//!
//! ```rust
//! # use hapic::json_api;
//! #
//! # #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
//! # struct Output {
//! #     c: u64,
//! # }
//! enum Operation {
//!     Add((u32, u32)),
//!     Sub((u32, u32)),
//!     Factorial(u8),
//! }
//!
//! #[derive(serde::Serialize)]
//! struct JsonOperation {
//!     operation: &'static str,
//!     a: u32,
//!     #[serde(skip_serializing_if = "Option::is_none")]
//!     b: Option<u32>,
//! }
//!
//! impl From<Operation> for JsonOperation {
//!     fn from(op: Operation) -> JsonOperation {
//!         match op {
//!             Operation::Add((a, b)) => JsonOperation {
//!                 operation: "add",
//!                 a,
//!                 b: Some(b),
//!             },
//!             Operation::Sub((a, b)) => JsonOperation {
//!                 operation: "sub",
//!                 a,
//!                 b: Some(b),
//!             },
//!             Operation::Factorial(x) => JsonOperation {
//!                 operation: "factorial",
//!                 a: x as u32,
//!                 b: None,
//!             },
//!         }
//!     }
//! }
//!
//! json_api!(
//!     struct TestClient<B, T: Transport<B>>;
//!     trait TestApiCall;
//!
//!     json {
//!         "/op": Operation as JsonOperation => Output as Output;
//!     }
//! );
//! ```
//!
//! Of course, we could have achieved the same thing using a custom serialization implementation,
//! but in many cases (especially for deserializing the output) it's significantly faster to define
//! two types and implement `TryInto` or `Into` between them.
//!
//! Finally, we make another tweak. Out `Output` type is just a single number, so why not return a
//! number as the result.
//!
//! ```rust
//! # enum Operation {
//! #     Add((u32, u32)),
//! #     Sub((u32, u32)),
//! #     Factorial(u8),
//! # }
//! #
//! # #[derive(serde::Serialize)]
//! # struct JsonOperation {
//! #     operation: &'static str,
//! #     a: u32,
//! #     #[serde(skip_serializing_if = "Option::is_none")]
//! #     b: Option<u32>,
//! # }
//! #
//! # impl From<Operation> for JsonOperation {
//! #     fn from(op: Operation) -> JsonOperation {
//! #         match op {
//! #             Operation::Add((a, b)) => JsonOperation {
//! #                 operation: "add",
//! #                 a,
//! #                 b: Some(b),
//! #             },
//! #             Operation::Sub((a, b)) => JsonOperation {
//! #                 operation: "sub",
//! #                 a,
//! #                 b: Some(b),
//! #             },
//! #             Operation::Factorial(x) => JsonOperation {
//! #                 operation: "factorial",
//! #                 a: x as u32,
//! #                 b: None,
//! #             },
//! #         }
//! #     }
//! # }
//! # #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
//! # struct Output {
//! #     c: u64,
//! # }
//! #
//! # use hapic::json_api;
//! impl From<Output> for u64 {
//!     fn from(output: Output) -> u64 {
//!         output.c
//!     }
//! }
//!
//! json_api!(
//!     struct TestClient<B, T: Transport<B>>;
//!     trait TestApiCall;
//!
//!     json {
//!         "/op": Operation as JsonOperation => Output as u64;
//!     }
//! );
//! ```
//!
//! Now we can make API calls really neatly:
//!
//! ```rust
//! # enum Operation {
//! #     Add((u32, u32)),
//! #     Sub((u32, u32)),
//! #     Factorial(u8),
//! # }
//! #
//! # #[derive(serde::Serialize)]
//! # struct JsonOperation {
//! #     operation: &'static str,
//! #     a: u32,
//! #     #[serde(skip_serializing_if = "Option::is_none")]
//! #     b: Option<u32>,
//! # }
//! #
//! # impl From<Operation> for JsonOperation {
//! #     fn from(op: Operation) -> JsonOperation {
//! #         match op {
//! #             Operation::Add((a, b)) => JsonOperation {
//! #                 operation: "add",
//! #                 a,
//! #                 b: Some(b),
//! #             },
//! #             Operation::Sub((a, b)) => JsonOperation {
//! #                 operation: "sub",
//! #                 a,
//! #                 b: Some(b),
//! #             },
//! #             Operation::Factorial(x) => JsonOperation {
//! #                 operation: "factorial",
//! #                 a: x as u32,
//! #                 b: None,
//! #             },
//! #         }
//! #     }
//! # }
//! #
//! # #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
//! # struct Output {
//! #     c: u64,
//! # }
//! #
//! # use hapic::json_api;
//! #
//! # impl From<Output> for u64 {
//! #     fn from(output: Output) -> u64 {
//! #         output.c
//! #     }
//! # }
//! #
//! # json_api!(
//! #     struct TestClient<B, T: Transport<B>>;
//! #     trait TestApiCall;
//! #
//! #     json {
//! #         "/op": Operation as JsonOperation => Output as u64;
//! #     }
//! # );
//! # async fn example() {
//! let client = TestClient::new(std::borrow::Cow::Borrowed("http://localhost:8000"));
//! let output = client.call(Operation::Add((1, 2))).await.unwrap();
//! assert_eq!(output, 3);
//! # }
//! ```
//!
//! ## Real-World Example
//!
//! For a real world example, see `cloudconvert-rs`.
//!
//! ## Using the macros
//!
//! In the simple examples above, we made use of the `json_api!` macro to generate the client and
//! API calls.
//!
//! The `json_api!` macro is just a shorthand way of using the `client!` macro, then the
//! `json_api_call!` macro, both of which have detailed documentation.
//!
//! ## Implementing Traits Directly
//!
//! You can also (and in some cases will want to) implement the traits directly. Here's a summary,
//! from most abstract to least abstract:
//!
//! - [`SimpleApiCall`]: a JSON API call where the input and output types undergo no conversion
//!   (automatically implements [`JsonApiCall`]).
//! - [`JsonApiCall`]: a JSON api call, allowing for conversion of the request and response
//!   (automatically implements [`ApiCall`]).
//! - [`ApiCall`]: a generic API call (automatically implements [`RawApiCall`]).
//! - [`RawApiCall`]: the lowest level trait for API calls, you probably want to implement one of
//!   the others.

use std::borrow::Cow;
use std::marker::PhantomData;

pub use http;
pub use serde_json;

#[cfg(feature = "hyper")]
pub use hyper;
#[cfg(feature = "hyper")]
pub use hyper_tls;

use http::{
    header::{self, HeaderMap, HeaderValue},
    Method, StatusCode,
};
use serde::{Deserialize, Serialize};

pub mod transport;

#[cfg(feature = "hyper")]
/// Macro for generating the client and ApiCall type.
///
/// If you're defining JSON API calls at the same time, you can use [`json_api`] instead.
///
/// For example:
/// ```
/// hapic::client!(
///     pub struct Client<B, T: Transport<B>>;
///     pub trait ApiCall;
/// );
/// ```
///
/// Creates:
/// ```
/// use hapic::RawApiCall;
/// use hapic::transport::Transport;
///
/// pub struct Client<B, T: Transport<B>> {
///     pub client: hapic::Client<B, T>
/// }
///
/// pub trait ApiCall: RawApiCall {}
/// ```
///
/// The `Client` type has, by default, two impls:
///
/// ```
/// use hapic::{Error, RawApiCall};
/// use hapic::transport::HttpsTransport;
///
/// # use hapic::transport::Transport;
/// # pub struct Client<B, T: Transport<B>> {
/// #     pub client: hapic::Client<B, T>
/// # }
///
/// pub trait ApiCall: RawApiCall {}
///
/// impl Client<hyper::Body, HttpsTransport> {
///     /// Create a new client to the provided endpoint, using `hyper` and `HttpsTransport`.
///     pub fn new(endpoint: std::borrow::Cow<'static, str>) -> Self {
///         Client {
///             client: hapic::Client::new_https_client(endpoint),
///         }
///     }
/// }
///
/// impl<B: Send + Sync, T: Transport<B>> Client<B, T> {
///     pub async fn call<C>(
///         &self,
///         api_call: C,
///     ) -> std::result::Result<C::Output, Error>
///     where
///         C: ApiCall + Send,
///         B: From<<C as RawApiCall>::RequestBody>,
///     {
///         RawApiCall::request(api_call, &self.client).await
///     }
/// }
/// ```
#[macro_export]
macro_rules! client {
    (
        $(#[$client_meta:meta])*
        $client_vis:vis struct $Client:ident<B, T: $($(hapic::)?transport::)?Transport<B>>;
        $(#[$trait_meta:meta])*
        $trait_vis:vis trait $ApiCall:ident;
    ) => {
        $(#[$client_meta])*
        $client_vis struct $Client<B, T: $crate::transport::Transport<B>> {
            pub client: $crate::Client<B, T>,
        }

        $(#[$trait_meta])*
        $trait_vis trait $ApiCall: $crate::RawApiCall {}

        impl $Client<$crate::hyper::Body, $crate::transport::HttpsTransport> {
            /// Create a new client to the provided endpoint, using `hyper` and `HttpsTransport`.
            pub fn new(endpoint: std::borrow::Cow<'static, str>) -> Self {
                $Client {
                    client: $crate::Client::new_https_client(endpoint),
                }
            }
        }

        impl<B: Send + Sync, T: $crate::transport::Transport<B>> $Client<B, T> {
            /// Make an API call.
            pub async fn call<C>(
                &self,
                api_call: C,
            ) -> std::result::Result<<C as $crate::RawApiCall>::Output, $crate::Error>
            where
                C: $ApiCall + Send,
                B: From<<C as $crate::RawApiCall>::RequestBody>,
            {
                $crate::RawApiCall::request(api_call, &self.client).await
            }
        }
    };
}

#[cfg(not(feature = "hyper"))]
/// Macro for generating the client and ApiCall type.
///
/// If you're defining JSON API calls at the same time, you can use [`json_api`] instead.
///
/// For example:
/// ```
/// hapic::client!(
///     pub struct Client<B, T: Transport<B>>;
///     pub trait ApiCall;
/// );
/// ```
///
/// Creates:
/// ```
/// use hapic::RawApiCall;
/// use hapic::transport::Transport;
///
/// pub struct Client<B, T: Transport<B>> {
///     pub client: hapic::Client<B, T>
/// }
///
/// pub trait ApiCall: RawApiCall {}
/// ```
///
/// The `Client` type has, by default, two impls:
///
/// ```
/// use hapic::{Error, RawApiCall};
/// use hapic::transport::HttpsTransport;
///
/// # use hapic::transport::Transport;
/// # pub struct Client<B, T: Transport<B>> {
/// #     pub client: hapic::Client<B, T>
/// # }
///
/// pub trait ApiCall: RawApiCall {}
///
/// impl Client<hyper::Body, HttpsTransport> {
///     /// Create a new client to the provided endpoint, using `hyper` and `HttpsTransport`.
///     pub fn new(endpoint: std::borrow::Cow<'static, str>) -> Self {
///         Client {
///             client: hapic::Client::new_https_client(endpoint),
///         }
///     }
/// }
///
/// impl<B: Send + Sync, T: Transport<B>> Client<B, T> {
///     pub async fn call<C>(
///         &self,
///         api_call: C,
///     ) -> std::result::Result<C::Output, Error>
///     where
///         C: ApiCall + Send,
///         B: From<<C as RawApiCall>::RequestBody>,
///     {
///         RawApiCall::request(api_call, &self.client).await
///     }
/// }
/// ```
#[macro_export]
macro_rules! client {
    (
        $(#[$client_meta:meta])*
        $client_vis:vis struct $Client:ident<B, T: $($(hapic::)?transport::)?Transport<B>>;
        $(#[$trait_meta:meta])*
        $trait_vis:vis trait $ApiCall:ident;
    ) => {
        $(#[$client_meta])*
        $client_vis struct $Client<B, T: $crate::transport::Transport<B>> {
            pub client: $crate::Client<B, T>,
        }

        $(#[$trait_meta])*
        $trait_vis trait $ApiCall: $crate::RawApiCall {}

        impl<B: Send + Sync, T: $crate::transport::Transport<B>> $Client<B, T> {
            /// Make an API call.
            pub async fn call<C>(
                &self,
                api_call: C,
            ) -> std::result::Result<<C as $crate::RawApiCall>::Output, $crate::Error>
            where
                C: $ApiCall + Send,
                B: From<<C as $crate::RawApiCall>::RequestBody>,
            {
                $crate::RawApiCall::request(api_call, &self.client).await
            }
        }
    };
}

/// Define a JSON api client and some API calls.
///
/// This is shorthand for the [`client`] macro, followed by the [`json_api_call`] macro.
///
/// See the crate level documentation for examples, and the constituent macros for more detailed
/// documentation.
#[macro_export]
macro_rules! json_api {
    (
        $(#[$client_meta:meta])*
        $client_vis:vis struct $Client:ident<B, T: $($(hapic::)?transport::)?Transport<B>>;
        $(#[$trait_meta:meta])*
        $trait_vis:vis trait $ApiCall:ident;
        $($tt:tt)*
    ) => {
        $crate::client!(
            $(#[$client_meta])*
            $client_vis struct $Client<B, T: Transport<B>>;
            $(#[$trait_meta])*
            $trait_vis trait $ApiCall;
        );
        $crate::json_api_call!(
            ApiCall: $ApiCall;
            $($tt)*
        );
    }
}

/// A macro for quickly implementing the various JSON API call traits.
///
/// # SimpleApiCall
///
/// To implement [`SimpleApiCall`], use:
///
/// ```
/// # use hapic::json_api_call;
/// # #[derive(serde::Serialize)]
/// # struct Foo;
/// # #[derive(serde::Deserialize)]
/// # struct Bar;
///
/// json_api_call!(simple "/foo": Foo => Bar);
/// ```
///
/// This implements [``SimpleApiCall`] for `Foo`, with output type `Bar`. The API call formats the
/// value (of type `Foo`) to a JSON string, makes a post request to `"/foo"` and then parses the
/// results as type `Bar` (expecting a 200-299 response).
///
/// `Foo` must implement [`serde::Serialize`] and `Bar` must implement [`serde::Deserialize`].
///
/// To customise the request method used, you can write:
///
/// ```
/// # use hapic::json_api_call;
/// # #[derive(serde::Serialize)]
/// # struct Foo;
/// # #[derive(serde::Deserialize)]
/// # struct Bar;
///
/// json_api_call!(simple PUT "/foo": Foo => Bar);
/// ```
///
/// Replacing `PUT` with any HTTP method.
///
/// # JsonApiCall
///
/// If you require type conversion of the input and/or output type, you can instead implement
/// [`JsonApiCall`] with:
///
/// ```
/// # use hapic::json_api_call;
/// # struct Foo;
/// # #[derive(serde::Serialize)]
/// # struct FooJson;
/// # #[derive(serde::Deserialize)]
/// # struct BarJson;
/// # struct Bar;
/// # impl From<Foo> for FooJson { fn from(_: Foo) -> FooJson { FooJson } }
/// # impl From<BarJson> for Bar { fn from(_: BarJson) -> Bar { Bar } }
/// json_api_call!(json "/foo": Foo as FooJson => BarJson as Bar);
/// ```
///
/// For this, we require:
/// ```ignore
/// Foo: TryInto<FooJson>
///     where <Foo as TryInto<FooJson>>::Error: Into<hapic::Error>
/// FooJson: Serialize
/// BarJson: Deserialize
/// BarJson: TryInto<Bar>
///     where <BarJson as TryInto<Bar>>::Error: Into<hapic::Error>
/// ```
///
/// # Not using the macros
///
/// Of course, you can implement [`SimpleApiCall`] or [`JsonApiCall`] yourself, however there isn't
/// much advantage, except for adding handling for non-2xx status codes.
///
/// If your API call is more complicated, you probably want to consider implementing [`ApiCall`]
/// (or, if you want almost no abstraction, [`SimpleApiCall`]).
#[macro_export]
macro_rules! json_api_call {
    (simple $(<$($a:lifetime),*>)? ($ApiCall:ty) $method:ident $url:literal : $Input:ty => $Output:ty) => {
        impl$(<$($a),*>)? $ApiCall for $Input {}
        $crate::json_api_call!(simple $(<$($a),*>)? $method $url : $Input => $Output);
    };

    (simple $(<$($a:lifetime),*>)? $method:ident $url:literal : $Input:ty => $Output:ty) => {
        impl$(<$($a),*>)? $crate::SimpleApiCall for $Input {
            type Output = $Output;

            fn method(&self) -> $crate::http::Method {
                $crate::http::Method::$method
            }

            fn uri(&self, endpoint: &str) -> std::string::String {
                let uri = String::with_capacity(endpoint.len() + $url.len());
                uri + endpoint + $url
            }
        }
    };

    (simple $(<$($a:lifetime),*>)? $(($ApiCall:ty))? $url:literal : $Input:ty => $Output:ty) => {
        $crate::json_api_call!(simple$(<$($a),*>)? $(($ApiCall))? POST $url: $Input => $Output);
    };

    (json $(<$($a:lifetime),*>)? ($ApiCall:ty) $method:ident $url:literal : $Input:ty as $JsonInput:ty => $JsonOutput:ty as $Output:ty) => {
        impl$(<$($a),*>)? $ApiCall for $Input {}
        $crate::json_api_call!(
            json$(<$($a),*>)? $method $url : $Input as $JsonInput => $JsonOutput as $Output
        );
    };

    (json $(<$($a:lifetime),*>)? $method:ident $url:literal : $Input:ty as $JsonInput:ty => $JsonOutput:ty as $Output:ty) => {
        impl$(<$($a),*>)? $crate::JsonApiCall for $Input {
            type Output = $Output;
            type JsonResponse = $JsonOutput;
            type JsonRequest = $JsonInput;

            fn method(&self) -> $crate::http::Method {
                $crate::http::Method::$method
            }

            fn uri(&self, endpoint: &str) -> std::string::String {
                let uri = String::with_capacity(endpoint.len() + $url.len());
                uri + endpoint + $url
            }

            fn try_into_request(self) -> std::result::Result<$JsonInput, $crate::Error> {
                Ok(self.try_into()?)
            }

            fn parse_json_response(
                status: $crate::http::StatusCode,
                content_type: Option<$crate::http::HeaderValue>,
                raw_resp: Vec<u8>,
                resp: $crate::serde_json::Result<Self::JsonResponse>,
            ) -> std::result::Result<$Output, $crate::Error> {
                if status.is_success() {
                    Ok(resp?.try_into()?)
                } else {
                    Err($crate::Error::HttpStatusNotSuccess{
                        status,
                        content_type,
                        body: raw_resp,
                    })
                }
            }
        }
    };

    (json $(<$($a:lifetime),*>)? $(($ApiCall:ty))? $url:literal : $Input:ty as $JsonInput:ty => $JsonOutput:ty as $Output:ty) => {
        $crate::json_api_call!(
            json $(<$($a),*>)? $(($ApiCall))? POST $url: $Input as $JsonInput => $JsonOutput as $Output
        );
    };

    (
        ApiCall: $ApiCall:ident;
        $(
            $(
                simple {$(
                    $(<$($a:lifetime),*>)? $($simple_method:ident)? $simple_url:literal : $SimpleInput:ty => $SimpleOutput:ty;
                )*}
            )?
            $(
                json {$(
                    $(<$($b:lifetime),*>)? $($json_method:ident)? $json_url:literal : $Input:ty as $JsonInput:ty => $JsonOutput:ty as $Output:ty;
                )*}
            )?
        );*
    ) => {$(
        $($(
            $crate::json_api_call!(simple $(<$($a),*>)? ($ApiCall) $($simple_method)? $simple_url: $SimpleInput => $SimpleOutput);
        )*)?
        $($(
            $crate::json_api_call!(json $(<$($b),*>)? ($ApiCall) $($json_method)? $json_url: $Input as $JsonInput => $JsonOutput as $Output);
        )*)?
    )*};
}

use transport::{ResponseBody, Transport};

pub struct Client<B, T: Transport<B>> {
    pub transport: T,
    pub phantom_body: PhantomData<B>,
    pub endpoint: Cow<'static, str>,
    pub authorization: Option<HeaderValue>,
    pub extra_headers: HeaderMap,
}

#[cfg(feature = "hyper")]
impl Client<hyper::Body, transport::HttpsTransport> {
    /// Create a new client using [`transport::HttpsTransport`] and [`hyper::Body`].
    pub fn new_https_client(
        endpoint: Cow<'static, str>,
    ) -> Client<hyper::Body, transport::HttpsTransport> {
        Client {
            transport: hyper::Client::builder().build(hyper_tls::HttpsConnector::new()),
            phantom_body: PhantomData,
            endpoint,
            authorization: None,
            extra_headers: HeaderMap::new(),
        }
    }
}

impl<B, T: Transport<B>> Client<B, T> {
    /// Create a new request builder based on this client.
    ///
    /// This should always be used as the base builder, since it includes the authorization and
    /// extra headers, and may include more parameters in the future.
    pub fn http_request_builder(&self) -> http::request::Builder {
        let mut builder = http::Request::builder();
        if let Some(authorization) = self.authorization.as_ref() {
            builder = builder.header(header::AUTHORIZATION, authorization);
        }
        for (header, value) in self.extra_headers.iter() {
            builder = builder.header(header, value);
        }
        builder
    }
}

#[derive(Debug)]
pub enum Error {
    Json(serde_json::Error),
    #[cfg(feature = "hyper")]
    Hyper(hyper::Error),
    HttpStatusNotSuccess {
        status: http::StatusCode,
        content_type: Option<http::HeaderValue>,
        body: Vec<u8>,
    },
    HttpNoBody,
    Other(Cow<'static, str>),
}

macro_rules! error_from {
    ($(impl From<$type:ty> for $Error:ident :: $Variant:ident);* $(;)?) => {
        $(
            impl From<$type> for $Error {
                fn from(err: $type) -> $Error {
                    $Error::$Variant(err)
                }
            }
        )*
    };
}

error_from!(
    impl From<serde_json::Error> for Error::Json;
);

#[cfg(feature = "hyper")]
error_from!(
    impl From<hyper::Error> for Error::Hyper;
);

impl From<std::convert::Infallible> for Error {
    fn from(_: std::convert::Infallible) -> Error {
        unreachable!()
    }
}

/// A JSON API call where the input and output types undergo no conversion.
///
/// This can be implemented manually, or by using:
/// `json_api_call!(simple "/endpoint": InputType => OutputType)`, or, to specify a custom method:
/// `json_api_call!(simple PATCH "/endpoint": InputType => OutputType)`.
///
/// Implementers of `SimpleApiCall`, automatically implement [`JsonApiCall`], therefore [`ApiCall`]
/// and therefore [`RawApiCall`].
pub trait SimpleApiCall: Serialize + Sized + Send {
    /// The output type of the call.
    type Output: for<'a> Deserialize<'a>;

    /// The HTTP request method.
    fn method(&self) -> Method {
        Method::POST
    }

    /// Generate the URI to make the HTTP POST request to.
    fn uri(&self, endpoint: &str) -> String;

    /// A hook used to modify the request before it's finalised. This is called directly before the
    /// `body` method on the builder is called.
    fn modify_request(&self, request_builder: http::request::Builder) -> http::request::Builder {
        request_builder
    }
}

impl<T: SimpleApiCall> JsonApiCall for T {
    type Output = <Self as SimpleApiCall>::Output;
    type JsonResponse = <Self as SimpleApiCall>::Output;

    type JsonRequest = Self;

    fn method(&self) -> Method {
        SimpleApiCall::method(self)
    }

    fn uri(&self, endpoint: &str) -> String {
        SimpleApiCall::uri(self, endpoint)
    }

    fn modify_request(&self, request_builder: http::request::Builder) -> http::request::Builder {
        SimpleApiCall::modify_request(self, request_builder)
    }

    fn try_into_request(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn parse_json_response(
        status: StatusCode,
        content_type: Option<http::HeaderValue>,
        raw_resp: Vec<u8>,
        resp: serde_json::Result<Self::Output>,
    ) -> Result<Self::Output, Error> {
        if status.is_success() {
            Ok(resp?)
        } else {
            Err(Error::HttpStatusNotSuccess {
                status,
                content_type,
                body: raw_resp,
            })
        }
    }
}

/// A JSON API call. (See also: [`SimpleApiCall`])
///
/// This can be implemented manually, or by using the [`json_api_call`] macro.
///
/// Implementers of `JsonApiCall`, automatically implement [`ApiCall`] and therefore [`RawApiCall`].
pub trait JsonApiCall: Sized + Send {
    /// The output type of the call.
    type Output;

    /// The response type of the call, as is returned by the server.
    ///
    /// This is passed through [`Self::parse_json_response`] to generate the `Output` value.
    type JsonResponse: for<'a> Deserialize<'a>;

    /// The request that is sent to the server. `self` is converted to this type, using
    /// [`Self::try_into_request`] before making the API call.
    type JsonRequest: Serialize;

    /// The HTTP request method.
    fn method(&self) -> Method {
        Method::POST
    }

    /// Generate the URI to make the HTTP POST request to.
    fn uri(&self, endpoint: &str) -> String;

    /// A hook used to modify the request before it's finalised. This is called directly before the
    /// `body` method on the builder is called.
    fn modify_request(&self, request_builder: http::request::Builder) -> http::request::Builder {
        request_builder
    }

    /// Convert `self` to a [`Self::JsonRequest`]. This is called before making the HTTP POST
    /// request.
    fn try_into_request(self) -> Result<Self::JsonRequest, Error>;

    /// Convert a [`Self::JsonResponse`], as received from the server, into [`Self::Output`].
    fn parse_json_response(
        status: StatusCode,
        content_type: Option<http::HeaderValue>,
        raw_resp: Vec<u8>,
        resp: serde_json::Result<Self::JsonResponse>,
    ) -> Result<Self::Output, Error>;
}

#[async_trait::async_trait]
impl<T: JsonApiCall> ApiCall for T {
    type Output = <Self as JsonApiCall>::Output;
    type RequestBody = Vec<u8>;

    fn method(&self) -> Method {
        JsonApiCall::method(self)
    }

    fn uri(&self, endpoint: &str) -> String {
        JsonApiCall::uri(self, endpoint)
    }

    fn modify_request(&self, request_builder: http::request::Builder) -> http::request::Builder {
        JsonApiCall::modify_request(
            self,
            request_builder.header(http::header::CONTENT_TYPE, "application/json"),
        )
    }

    fn request_body(self) -> Result<Self::RequestBody, Error> {
        let req = self.try_into_request()?;
        Ok(serde_json::to_vec(&req)?)
    }

    async fn response<B: ResponseBody>(resp: http::Response<B>) -> Result<Self::Output, Error> {
        let status = resp.status();
        let content_type = resp.headers().get("Content-Type").cloned();
        let body_bytes = resp
            .into_body()
            .read_all()
            .await
            .map_err(|err| err.into())?;
        let resp: serde_json::Result<<Self as JsonApiCall>::JsonResponse> =
            serde_json::from_slice(body_bytes.as_ref());
        <Self as JsonApiCall>::parse_json_response(status, content_type, body_bytes.into(), resp)
    }
}

/// A generic API call. For a lower level interface, see [`RawApiCall`]. For JSON APIs, see
/// [`JsonApiCall`] and [`SimpleApiCall`].
#[async_trait::async_trait]
pub trait ApiCall: Sized + Send {
    /// The type of the request body.
    type RequestBody;

    /// The output type of the call.
    type Output;

    /// The HTTP request method.
    fn method(&self) -> Method {
        Method::POST
    }

    /// Generate the URI to make the HTTP POST request to.
    fn uri(&self, endpoint: &str) -> String;

    /// A hook used to modify the request before it's finalised. This is called directly before the
    /// `body` method on the builder is called.
    fn modify_request(&self, request_builder: http::request::Builder) -> http::request::Builder {
        request_builder
    }

    /// Convert `self` to a [`Self::RequestBody`]. This is called before making the HTTP POST
    /// request.
    fn request_body(self) -> Result<Self::RequestBody, Error>;

    /// Convert a [`http::Response`], as received from the server, into [`Self::Output`].
    async fn response<B: ResponseBody>(resp: http::Response<B>) -> Result<Self::Output, Error>;
}

#[async_trait::async_trait]
impl<C: ApiCall> RawApiCall for C {
    type Output = <C as ApiCall>::Output;

    type RequestBody = <C as ApiCall>::RequestBody;

    async fn request<B: From<Self::RequestBody> + Send, T: Transport<B>>(
        self,
        client: &Client<B, T>,
    ) -> Result<Self::Output, Error>
    where
        B: From<Self::RequestBody> + Sync,
    {
        let method = self.method();
        let uri = self.uri(&client.endpoint);
        let request = self
            .modify_request(client.http_request_builder().method(method).uri(uri))
            .body::<B>(self.request_body()?.into())
            .unwrap();
        let resp = client
            .transport
            .request(request)
            .await
            .map_err(|err| err.into())?;
        Self::response(resp).await
    }
}

/// The lowest level trait for API calls. You probably shouldn't implement it directly, instead
/// preferring [`ApiCall`], or, for JSON APIs, [`JsonApiCall`] and [`SimpleApiCall`].
///
/// If you do implement it directly, make sure you properly read the docs for the `request` method!
#[async_trait::async_trait]
pub trait RawApiCall: Sized {
    /// The type of the request body.
    type RequestBody;

    /// The output type of the call.
    type Output;

    /// Make a request using a client.
    ///
    /// It's super important to use the `client` value correctly. You should:
    /// - Use `client.endpoint` as the base of the URI.
    /// - Use `client.http_request_builder` as the base request builder, since this is garenteed to
    ///   use the other properties of the client.
    /// - Use `client.transport` to make the request.
    async fn request<B: From<Self::RequestBody> + Send, T: Transport<B>>(
        self,
        client: &Client<B, T>,
    ) -> Result<Self::Output, Error>
    where
        B: From<Self::RequestBody> + Sync;
}

#[cfg(test)]
mod test;
