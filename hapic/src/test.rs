use serde::{Deserialize, Serialize};

#[derive(Serialize)]
struct Add {
    a: u32,
    b: u32,
}

#[derive(Serialize)]
struct Sub {
    a: u32,
    b: u32,
}

#[derive(Serialize)]
struct Factorial {
    a: u8,
}

#[derive(Deserialize, PartialEq, Eq, Debug)]
struct Output {
    c: u64,
}

impl From<Output> for u64 {
    fn from(output: Output) -> u64 {
        output.c
    }
}

enum Operation {
    Add((u32, u32)),
    Sub((u32, u32)),
    Factorial(u8),
}

#[derive(Serialize)]
struct JsonOperation {
    operation: &'static str,
    a: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    b: Option<u32>,
}

impl From<Operation> for JsonOperation {
    fn from(op: Operation) -> JsonOperation {
        match op {
            Operation::Add((a, b)) => JsonOperation {
                operation: "add",
                a,
                b: Some(b),
            },
            Operation::Sub((a, b)) => JsonOperation {
                operation: "sub",
                a,
                b: Some(b),
            },
            Operation::Factorial(x) => JsonOperation {
                operation: "factorial",
                a: x as u32,
                b: None,
            },
        }
    }
}

json_api!(
    struct TestClient<B, T: Transport<B>>;
    trait TestApiCall;

    simple {
        "/add": Add => Output;
        "/sub": Sub => Output;
        "/factorial": Factorial => Output;
    }

    json {
        "/op": Operation as JsonOperation => Output as Output;
    }
);

enum AnotherOperation {
    Add((u32, u32)),
    Sub((u32, u32)),
    Factorial(u8),
}

impl From<AnotherOperation> for Operation {
    fn from(op: AnotherOperation) -> Operation {
        match op {
            AnotherOperation::Add(args) => Operation::Add(args),
            AnotherOperation::Sub(args) => Operation::Sub(args),
            AnotherOperation::Factorial(x) => Operation::Factorial(x),
        }
    }
}

impl From<AnotherOperation> for JsonOperation {
    fn from(op: AnotherOperation) -> JsonOperation {
        Operation::from(op).into()
    }
}

json_api!(
    struct AnotherTestClient<B, T: Transport<B>>;
    trait AnotherTestApiCall;

    json {
        "/op": AnotherOperation as JsonOperation => Output as u64;
    }
);

#[tokio::test]
async fn test() {
    let client = TestClient::new(std::borrow::Cow::Borrowed("http://localhost:8000"));
    assert_eq!(
        client.call(Add { a: 1, b: 2 }).await.unwrap(),
        Output { c: 3 }
    );
    assert_eq!(
        client.call(Sub { a: 3, b: 2 }).await.unwrap(),
        Output { c: 1 }
    );
    assert_eq!(
        client.call(Factorial { a: 4 }).await.unwrap(),
        Output { c: 24 }
    );
    assert_eq!(
        client.call(Operation::Add((5, 6))).await.unwrap(),
        Output { c: 11 }
    );
    assert_eq!(
        client.call(Operation::Sub((7, 5))).await.unwrap(),
        Output { c: 2 }
    );
    assert_eq!(
        client.call(Operation::Factorial(5)).await.unwrap(),
        Output { c: 120 }
    );

    let another_client =
        AnotherTestClient::new(std::borrow::Cow::Borrowed("http://localhost:8000"));
    let output = another_client
        .call(AnotherOperation::Add((1, 2)))
        .await
        .unwrap();
    assert_eq!(output, 3);
    let output = another_client
        .call(AnotherOperation::Sub((3, 2)))
        .await
        .unwrap();
    assert_eq!(output, 1);
    let output = another_client
        .call(AnotherOperation::Factorial(3))
        .await
        .unwrap();
    assert_eq!(output, 6);
}
