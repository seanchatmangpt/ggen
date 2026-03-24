use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use ggen_receipt::{generate_keypair, hash_data, Receipt, ReceiptChain};

fn bench_keypair_generation(c: &mut Criterion) {
    c.bench_function("keypair_generation", |b| {
        b.iter(|| {
            black_box(generate_keypair());
        });
    });
}

fn bench_receipt_creation(c: &mut Criterion) {
    c.bench_function("receipt_creation", |b| {
        b.iter(|| {
            black_box(Receipt::new(
                "test-operation".to_string(),
                vec!["input1".to_string()],
                vec!["output1".to_string()],
                None,
            ));
        });
    });
}

fn bench_receipt_signing(c: &mut Criterion) {
    let (signing_key, _) = generate_keypair();

    c.bench_function("receipt_signing", |b| {
        b.iter(|| {
            let receipt = Receipt::new(
                "test-operation".to_string(),
                vec!["input1".to_string()],
                vec!["output1".to_string()],
                None,
            );
            black_box(receipt.sign(&signing_key).unwrap());
        });
    });
}

fn bench_receipt_verification(c: &mut Criterion) {
    let (signing_key, verifying_key) = generate_keypair();
    let receipt = Receipt::new(
        "test-operation".to_string(),
        vec!["input1".to_string()],
        vec!["output1".to_string()],
        None,
    )
    .sign(&signing_key)
    .unwrap();

    c.bench_function("receipt_verification", |b| {
        b.iter(|| {
            black_box(receipt.verify(&verifying_key).unwrap());
        });
    });
}

fn bench_receipt_hashing(c: &mut Criterion) {
    let (signing_key, _) = generate_keypair();
    let receipt = Receipt::new(
        "test-operation".to_string(),
        vec!["input1".to_string()],
        vec!["output1".to_string()],
        None,
    )
    .sign(&signing_key)
    .unwrap();

    c.bench_function("receipt_hashing", |b| {
        b.iter(|| {
            black_box(receipt.hash().unwrap());
        });
    });
}

fn bench_receipt_chaining(c: &mut Criterion) {
    let (signing_key, _) = generate_keypair();
    let genesis = Receipt::new(
        "genesis".to_string(),
        vec![],
        vec![],
        None,
    )
    .sign(&signing_key)
    .unwrap();

    c.bench_function("receipt_chaining", |b| {
        b.iter(|| {
            let receipt = Receipt::new(
                "chained-op".to_string(),
                vec!["input1".to_string()],
                vec!["output1".to_string()],
                None,
            )
            .chain(&genesis)
            .unwrap()
            .sign(&signing_key)
            .unwrap();
            black_box(receipt);
        });
    });
}

fn bench_chain_building(c: &mut Criterion) {
    let mut group = c.benchmark_group("chain_building");

    for size in [10, 50, 100, 500].iter() {
        group.throughput(Throughput::Elements(*size));
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &size| {
            b.iter(|| {
                let (signing_key, _) = generate_keypair();
                let genesis = Receipt::new(
                    "genesis".to_string(),
                    vec![],
                    vec![],
                    None,
                )
                .sign(&signing_key)
                .unwrap();

                let mut chain = ReceiptChain::from_genesis(genesis.clone()).unwrap();
                let mut prev = genesis;

                for i in 1..size {
                    let receipt = Receipt::new(
                        format!("op-{}", i),
                        vec![],
                        vec![],
                        None,
                    )
                    .chain(&prev)
                    .unwrap()
                    .sign(&signing_key)
                    .unwrap();

                    chain.append(receipt.clone()).unwrap();
                    prev = receipt;
                }

                black_box(chain);
            });
        });
    }
    group.finish();
}

fn bench_chain_verification(c: &mut Criterion) {
    let mut group = c.benchmark_group("chain_verification");

    for size in [10, 50, 100].iter() {
        let (signing_key, verifying_key) = generate_keypair();
        let genesis = Receipt::new(
            "genesis".to_string(),
            vec![],
            vec![],
            None,
        )
        .sign(&signing_key)
        .unwrap();

        let mut chain = ReceiptChain::from_genesis(genesis.clone()).unwrap();
        let mut prev = genesis;

        for i in 1..*size {
            let receipt = Receipt::new(
                format!("op-{}", i),
                vec![],
                vec![],
                None,
            )
            .chain(&prev)
            .unwrap()
            .sign(&signing_key)
            .unwrap();

            chain.append(receipt.clone()).unwrap();
            prev = receipt;
        }

        group.throughput(Throughput::Elements(*size));
        group.bench_with_input(BenchmarkId::from_parameter(size), &chain, |b, chain| {
            b.iter(|| {
                black_box(chain.verify(&verifying_key).unwrap());
            });
        });
    }
    group.finish();
}

fn bench_data_hashing(c: &mut Criterion) {
    let mut group = c.benchmark_group("data_hashing");

    for size in [1024, 10_240, 102_400].iter() {
        let data = vec![0u8; *size];
        group.throughput(Throughput::Bytes(*size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &data, |b, data| {
            b.iter(|| {
                black_box(hash_data(data));
            });
        });
    }
    group.finish();
}

criterion_group!(
    benches,
    bench_keypair_generation,
    bench_receipt_creation,
    bench_receipt_signing,
    bench_receipt_verification,
    bench_receipt_hashing,
    bench_receipt_chaining,
    bench_chain_building,
    bench_chain_verification,
    bench_data_hashing,
);
criterion_main!(benches);
