// NEGATIVE (historical FP class): #[tokio::test] (last segment `test`) with
// await points and a real assertion. Expected verdict: [] (clean).
async fn fetch_len() -> usize {
    let data: Vec<u8> = vec![1, 2, 3];
    data.len()
}

#[tokio::test]
async fn async_test_with_real_assert() {
    let n = fetch_len().await;
    assert_eq!(n, 3);
}
