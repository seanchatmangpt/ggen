use cucumber::{given, then, when, World};

#[derive(Debug, Default, World)]
pub struct TestWorld {
    pub value: i32,
}

#[given("I have a value")]
fn given_value(world: &mut TestWorld) {
    world.value = 42;
}

#[when("I increment it")]
fn when_increment(world: &mut TestWorld) {
    world.value += 1;
}

#[then("it should be {int}")]
fn then_value(world: &mut TestWorld, expected: i32) {
    assert_eq!(world.value, expected);
}

#[tokio::test]
async fn test_cucumber_api() {
    TestWorld::cucumber()
        .run("tests/features")
        .await;
}
