use std::ops::Deref;
use std::sync::atomic::{AtomicU8, Ordering};

use rocket::{post, serde::json::Json};
use serde::{Deserialize, Serialize};

static COUNTER: AtomicU8 = AtomicU8::new(0);

#[derive(Deserialize)]
struct UnitaryOpArgs {
    a: u32,
}

#[derive(Deserialize)]
struct BinaryOpArgs {
    a: u32,
    b: u32,
}

#[derive(Deserialize)]
struct Operation {
    operation: String,
    a: u32,
    #[serde(default)]
    b: u32,
}

#[derive(Serialize)]
struct Output {
    c: u64,
}

#[post("/op", data = "<op>")]
fn op(op: Json<Operation>) -> Json<Output> {
    let c = match op.operation.deref() {
        "add" => op.a as u64 + op.b as u64,
        "sub" => op.a as u64 - op.b as u64,
        "factorial" => factorial_impl(op.a as u64),
        _ => todo!(),
    };
    Json(Output { c })
}

#[post("/add", data = "<args>")]
fn add(args: Json<BinaryOpArgs>) -> Json<Output> {
    Json(Output {
        c: (args.a as u64) + (args.b as u64),
    })
}

/// And endpoint to add two numbers, that only works 25% of the time. Otherwise returning
/// ServiceUnavailable.
#[post("/add/dodgy", data = "<args>")]
fn add_dodgy(args: Json<BinaryOpArgs>) -> Result<Json<Output>, rocket::http::Status> {
    if COUNTER.fetch_add(1, Ordering::Relaxed) % 4 == 0 {
        Ok(Json(Output {
            c: (args.a as u64) + (args.b as u64),
        }))
    } else {
        Err(rocket::http::Status::ServiceUnavailable)
    }
}

#[post("/503", data = "<_args>")]
fn always_503(_args: Json<BinaryOpArgs>) -> rocket::http::Status {
    rocket::http::Status::ServiceUnavailable
}

#[post("/sub", data = "<args>")]
fn sub(args: Json<BinaryOpArgs>) -> Json<Output> {
    Json(Output {
        c: (args.a as u64) - (args.b as u64),
    })
}

fn factorial_impl(x: u64) -> u64 {
    if x == 0 || x == 1 {
        1
    } else {
        x * factorial_impl(x - 1)
    }
}

#[post("/factorial", data = "<args>")]
fn factorial(args: Json<UnitaryOpArgs>) -> Json<Output> {
    Json(Output {
        c: factorial_impl(args.a as u64),
    })
}

#[rocket::launch]
async fn rocket() -> _ {
    rocket::build().mount(
        "/",
        rocket::routes![add, add_dodgy, sub, factorial, op, always_503],
    )
}
