use cleanroom_bdd::CleanroomBDDWorld;
use cucumber::World;

#[tokio::test]
async fn test_basic_bdd() {
    CleanroomBDDWorld::cucumber()
        .run("tests/features/00_basic.feature")
        .await;
}

#[tokio::test]
async fn test_all_features() {
    CleanroomBDDWorld::cucumber()
        .run("tests/features/00_basic.feature")
        .await;
}
