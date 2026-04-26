use mcpp_core::autonomics::{MetaController, MetaControllerConfig};
use mcpp_core::Receipt;

#[test]
fn test_second_order_autonomics_loop() {
    // 1. Setup First-Order Parameters
    let initial_config = MetaControllerConfig {
        jidoka_sensitivity: 0.5,
        heijunka_leveling_factor: 0.5,
        adaptation_threshold: 0.2,
    };
    let controller = MetaController::new(initial_config.clone());

    // 2. Simulate Operational Failure (High failure rate in receipts)
    let receipt_history: Vec<Receipt> = ["failed", "failed", "success"]
        .iter()
        .enumerate()
        .map(|(i, s)| {
            let mut r = Receipt::pending(format!("op{}", i + 1));
            r.status = (*s).into();
            r
        })
        .collect();
    // 66% failure rate > 20% threshold

    // 3. Trigger Second-Order Adaptation
    let optimized_config = controller.evaluate_and_adapt(&receipt_history);

    // 4. Validate Re-optimization (with Dampening)
    assert!(
        optimized_config.is_some(),
        "Meta-controller should have triggered adaptation"
    );
    let new_config = optimized_config.unwrap();

    // Expected increase: (0.5 * 0.1) * 0.5 = 0.025 -> 0.525
    assert!(
        new_config.jidoka_sensitivity > 0.5,
        "Sensitivity should increase"
    );
    assert!(
        new_config.jidoka_sensitivity < 0.55,
        "Sensitivity increase should be dampened"
    );

    assert!(
        new_config.heijunka_leveling_factor < initial_config.heijunka_leveling_factor,
        "Heijunka leveling factor should decrease to smooth workload"
    );

    println!("✅ Second-order autonomic loop validated: Policy re-optimized with dampening factor");
}

#[test]
fn test_meta_loop_stability() {
    // Test that if system is stable, no adaptation occurs
    let config = MetaControllerConfig {
        jidoka_sensitivity: 0.5,
        heijunka_leveling_factor: 0.5,
        adaptation_threshold: 0.2,
    };
    let controller = MetaController::new(config);

    let stable_history: Vec<Receipt> = ["success", "success"]
        .iter()
        .enumerate()
        .map(|(i, s)| {
            let mut r = Receipt::pending(format!("op{}", i + 1));
            r.status = (*s).into();
            r
        })
        .collect();

    let result = controller.evaluate_and_adapt(&stable_history);
    assert!(
        result.is_none(),
        "Stable system should not trigger policy adaptation"
    );

    println!("✅ Meta-loop stability validated: No unnecessary adaptations");
}
