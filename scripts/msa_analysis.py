#!/usr/bin/env python3
"""
MSA Gage R&R Analysis Script
Calculates repeatability, reproducibility, and Gage R&R percentages
Process Capability Analysis (Cp/Cpk)

DfLSS Measure Phase: Measurement Systems Analysis
Feature 004: Test Quality Audit and Performance Optimization
"""

import pandas as pd
import numpy as np
from pathlib import Path
import sys
import json
from typing import Dict, List, Tuple

# Specification limits (from Makefile.toml SLOs)
SPEC_LIMITS = {
    'unit::version_resolution': {'LSL': 0, 'USL': 1000, 'Target': 500},
    'unit::registry_client': {'LSL': 0, 'USL': 1000, 'Target': 500},
    'integration::end_to_end_flow': {'LSL': 0, 'USL': 10000, 'Target': 5000},
    'security::signature_verification': {'LSL': 0, 'USL': 1000, 'Target': 500},
    'security::dos_resistance': {'LSL': 0, 'USL': 1000, 'Target': 500},
    'chicago_tdd_smoke_test': {'LSL': 0, 'USL': 1000, 'Target': 500},
    'aci::mod': {'LSL': 0, 'USL': 5000, 'Target': 2500},
    'aci::skill_invocation_tests': {'LSL': 0, 'USL': 5000, 'Target': 2500},
    'aci::timeout_enforcement_tests': {'LSL': 0, 'USL': 5000, 'Target': 2500},
    'aci::tool_selection_tests': {'LSL': 0, 'USL': 5000, 'Target': 2500},
}


def calculate_gage_rr(df: pd.DataFrame) -> pd.DataFrame:
    """
    Calculate Gage R&R from MSA data

    Returns DataFrame with:
    - EV (Equipment Variation - Repeatability)
    - AV (Appraiser Variation - Reproducibility)
    - PV (Part Variation)
    - Total Variation
    - Gage R&R percentage
    - Acceptance criteria
    """
    results = []

    for part in df['Part'].unique():
        part_data = df[df['Part'] == part]

        # Skip if insufficient data
        if len(part_data) < 9:  # Need at least 3 operators × 3 trials
            print(f"⚠️  Skipping {part}: Insufficient data ({len(part_data)} measurements)")
            continue

        # 1. REPEATABILITY (Equipment Variation - EV)
        # Within-operator variation (how consistent is the same operator?)
        ev_variances = []
        for operator in part_data['Operator'].unique():
            op_data = part_data[part_data['Operator'] == operator]['Duration_ms']
            if len(op_data) > 1:
                ev_variances.append(op_data.var())

        if not ev_variances:
            continue

        ev_variance = np.mean(ev_variances)
        ev_std = np.sqrt(ev_variance)

        # 2. REPRODUCIBILITY (Appraiser Variation - AV)
        # Between-operator variation (how different are operators from each other?)
        operator_means = part_data.groupby('Operator')['Duration_ms'].mean()

        if len(operator_means) > 1:
            av_variance = operator_means.var()
            # Adjust for repeatability within operators
            n_trials = part_data.groupby('Operator').size().mean()
            av_variance = max(0, av_variance - ev_variance / n_trials)
        else:
            av_variance = 0

        av_std = np.sqrt(av_variance)

        # 3. PART VARIATION (PV)
        # Total variation in the measurements
        pv_variance = part_data['Duration_ms'].var()
        # Adjust for measurement system variation
        pv_variance = max(0, pv_variance - ev_variance - av_variance)
        pv_std = np.sqrt(pv_variance)

        # 4. TOTAL VARIATION
        total_variance = ev_variance + av_variance + pv_variance
        total_std = np.sqrt(total_variance)

        # 5. GAGE R&R
        gage_rr_variance = ev_variance + av_variance
        gage_rr_std = np.sqrt(gage_rr_variance)

        # Gage R&R as percentage of total variation
        if total_std > 0:
            gage_rr_percent = (gage_rr_std / total_std) * 100
        else:
            gage_rr_percent = 0

        # 6. ACCEPTANCE CRITERIA
        if gage_rr_percent < 10:
            acceptance = "✅ ACCEPTABLE"
            recommendation = "Measurement system is excellent"
        elif gage_rr_percent < 30:
            acceptance = "⚠️ MARGINAL"
            recommendation = "Acceptable for some applications, improvements recommended"
        else:
            acceptance = "🔴 UNACCEPTABLE"
            recommendation = "Cannot make reliable decisions - MUST IMPROVE measurement system"

        # 7. CONTRIBUTION BREAKDOWN
        if total_variance > 0:
            ev_percent = (ev_variance / total_variance) * 100
            av_percent = (av_variance / total_variance) * 100
            pv_percent = (pv_variance / total_variance) * 100
        else:
            ev_percent = av_percent = pv_percent = 0

        results.append({
            'Part': part,
            'Measurements': len(part_data),
            'EV_std_ms': round(ev_std, 2),
            'EV_percent': round(ev_percent, 1),
            'AV_std_ms': round(av_std, 2),
            'AV_percent': round(av_percent, 1),
            'PV_std_ms': round(pv_std, 2),
            'PV_percent': round(pv_percent, 1),
            'Total_std_ms': round(total_std, 2),
            'GageRR_percent': round(gage_rr_percent, 1),
            'Acceptance': acceptance,
            'Recommendation': recommendation
        })

    return pd.DataFrame(results)


def calculate_process_capability(df: pd.DataFrame, spec_limits: Dict) -> pd.DataFrame:
    """
    Calculate Cp and Cpk for each test category

    Cp = (USL - LSL) / (6 × σ)  [Potential capability]
    Cpk = min((USL - μ)/(3σ), (μ - LSL)/(3σ))  [Actual capability]

    Interpretation:
    - Cp, Cpk > 1.33: Capable (6σ quality, 3.4 DPMO)
    - Cp, Cpk = 1.0-1.33: Marginally capable (3σ quality, 2700 DPMO)
    - Cp, Cpk < 1.0: Not capable
    """
    results = []

    for part, limits in spec_limits.items():
        part_data = df[df['Part'] == part]['Duration_ms']

        if len(part_data) == 0:
            continue

        # Calculate statistics
        mean = part_data.mean()
        std = part_data.std()

        USL = limits['USL']
        LSL = limits['LSL']
        Target = limits.get('Target', (USL + LSL) / 2)

        if std == 0:
            print(f"⚠️  {part}: Zero standard deviation, skipping capability analysis")
            continue

        # 1. Cp (Potential Capability)
        # Assumes process is centered
        cp = (USL - LSL) / (6 * std)

        # 2. Cpk (Actual Capability)
        # Accounts for process centering
        cpu = (USL - mean) / (3 * std)  # Upper capability
        cpl = (mean - LSL) / (3 * std)  # Lower capability
        cpk = min(cpu, cpl)

        # 3. Cpm (Capability relative to target)
        # Penalizes off-target performance
        tau = np.sqrt(std**2 + (mean - Target)**2)
        cpm = (USL - LSL) / (6 * tau) if tau > 0 else 0

        # 4. Process Performance (long-term)
        # Uses overall standard deviation
        pp = cp  # Same as Cp for this dataset
        ppk = cpk  # Same as Cpk for this dataset

        # 5. Interpretation
        if cp > 1.33 and cpk > 1.33:
            capability = "✅ CAPABLE (6σ)"
            sigma_level = 6
        elif cp > 1.0 and cpk > 1.0:
            capability = "⚠️ MARGINAL (3σ)"
            sigma_level = 3
        else:
            capability = "🔴 NOT CAPABLE"
            sigma_level = 0

        # 6. Centering
        if abs(cp - cpk) < 0.1:
            centering = "Centered"
        elif cpu < cpl:
            centering = "Off-center (high)"
        else:
            centering = "Off-center (low)"

        # 7. DPMO (Defects Per Million Opportunities)
        # Approximate from Cpk
        if cpk >= 2.0:
            dpmo = 0.002  # 6σ
        elif cpk >= 1.33:
            dpmo = 3.4  # 5σ
        elif cpk >= 1.0:
            dpmo = 2700  # 3σ
        else:
            dpmo = 66800  # <3σ

        results.append({
            'Part': part,
            'Mean_ms': round(mean, 2),
            'Std_ms': round(std, 2),
            'LSL': LSL,
            'USL': USL,
            'Target': Target,
            'Cp': round(cp, 2),
            'Cpk': round(cpk, 2),
            'Cpm': round(cpm, 2),
            'Sigma_Level': sigma_level,
            'DPMO': dpmo,
            'Centering': centering,
            'Capability': capability
        })

    return pd.DataFrame(results)


def generate_summary_report(
    gage_rr_df: pd.DataFrame,
    capability_df: pd.DataFrame,
    output_file: Path
) -> None:
    """Generate JSON summary report for dashboards"""

    summary = {
        'timestamp': pd.Timestamp.now().isoformat(),
        'msa_summary': {
            'total_parts': len(gage_rr_df),
            'acceptable': len(gage_rr_df[gage_rr_df['Acceptance'].str.contains('ACCEPTABLE')]),
            'marginal': len(gage_rr_df[gage_rr_df['Acceptance'].str.contains('MARGINAL')]),
            'unacceptable': len(gage_rr_df[gage_rr_df['Acceptance'].str.contains('UNACCEPTABLE')]),
            'average_gage_rr': round(gage_rr_df['GageRR_percent'].mean(), 1),
            'parts': gage_rr_df.to_dict('records')
        },
        'capability_summary': {
            'total_parts': len(capability_df),
            'capable_6sigma': len(capability_df[capability_df['Capability'].str.contains('CAPABLE')]),
            'marginal_3sigma': len(capability_df[capability_df['Capability'].str.contains('MARGINAL')]),
            'not_capable': len(capability_df[capability_df['Capability'].str.contains('NOT CAPABLE')]),
            'average_cp': round(capability_df['Cp'].mean(), 2),
            'average_cpk': round(capability_df['Cpk'].mean(), 2),
            'parts': capability_df.to_dict('records')
        }
    }

    with open(output_file, 'w') as f:
        json.dump(summary, f, indent=2)

    print(f"\n✅ Summary report saved to: {output_file}")


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 msa_analysis.py <data_file.csv>")
        print("Example: python3 scripts/msa_analysis.py specs/004-optimize-test-concurrency/dflss/msa_data/msa_raw_data_*.csv")
        sys.exit(1)

    data_file = Path(sys.argv[1])

    if not data_file.exists():
        print(f"❌ Error: Data file not found: {data_file}")
        sys.exit(1)

    # Load data
    print(f"📊 Loading MSA data from: {data_file}")
    df = pd.read_csv(data_file)

    # Filter out non-PASS measurements for capability analysis
    df_passed = df[df['Status'] == 'PASS'].copy()

    print(f"   Total measurements: {len(df)}")
    print(f"   Passed measurements: {len(df_passed)}")
    print(f"   Failed/Timeout: {len(df) - len(df_passed)}")

    if len(df_passed) < 9:
        print("❌ Error: Insufficient PASS measurements for MSA analysis")
        print("   Need at least 3 operators × 3 trials = 9 measurements")
        sys.exit(1)

    # ========================================================================
    # GAGE R&R ANALYSIS
    # ========================================================================

    print("\n" + "=" * 80)
    print("GAGE R&R ANALYSIS (Measurement Systems Analysis)")
    print("=" * 80)
    print("\nAcceptance Criteria:")
    print("  ✅ Gage R&R < 10%: ACCEPTABLE (excellent measurement system)")
    print("  ⚠️  Gage R&R 10-30%: MARGINAL (acceptable for some applications)")
    print("  🔴 Gage R&R > 30%: UNACCEPTABLE (cannot make reliable decisions)")
    print("\n" + "-" * 80)

    gage_rr_results = calculate_gage_rr(df_passed)

    if len(gage_rr_results) == 0:
        print("❌ No valid Gage R&R results calculated")
        sys.exit(1)

    # Display results
    pd.set_option('display.max_columns', None)
    pd.set_option('display.width', 120)
    print(gage_rr_results.to_string(index=False))

    # ========================================================================
    # PROCESS CAPABILITY ANALYSIS
    # ========================================================================

    print("\n" + "=" * 80)
    print("PROCESS CAPABILITY ANALYSIS (Cp/Cpk)")
    print("=" * 80)
    print("\nCapability Criteria:")
    print("  ✅ Cp, Cpk ≥ 1.33: CAPABLE (6σ quality, 3.4 DPMO)")
    print("  ⚠️  Cp, Cpk 1.0-1.33: MARGINAL (3σ quality, 2700 DPMO)")
    print("  🔴 Cp, Cpk < 1.0: NOT CAPABLE (>66800 DPMO)")
    print("\n" + "-" * 80)

    capability_results = calculate_process_capability(df_passed, SPEC_LIMITS)

    if len(capability_results) == 0:
        print("❌ No valid capability results calculated")
        sys.exit(1)

    print(capability_results.to_string(index=False))

    # ========================================================================
    # SUMMARY STATISTICS
    # ========================================================================

    print("\n" + "=" * 80)
    print("SUMMARY STATISTICS")
    print("=" * 80)

    acceptable_msa = len(gage_rr_results[gage_rr_results['Acceptance'].str.contains('ACCEPTABLE')])
    marginal_msa = len(gage_rr_results[gage_rr_results['Acceptance'].str.contains('MARGINAL')])
    unacceptable_msa = len(gage_rr_results[gage_rr_results['Acceptance'].str.contains('UNACCEPTABLE')])

    print(f"\nMeasurement System Analysis:")
    print(f"  ✅ Acceptable: {acceptable_msa}/{len(gage_rr_results)} ({acceptable_msa/len(gage_rr_results)*100:.1f}%)")
    print(f"  ⚠️  Marginal: {marginal_msa}/{len(gage_rr_results)} ({marginal_msa/len(gage_rr_results)*100:.1f}%)")
    print(f"  🔴 Unacceptable: {unacceptable_msa}/{len(gage_rr_results)} ({unacceptable_msa/len(gage_rr_results)*100:.1f}%)")
    print(f"  📊 Average Gage R&R: {gage_rr_results['GageRR_percent'].mean():.1f}%")

    capable_6sigma = len(capability_results[capability_results['Capability'].str.contains('CAPABLE')])
    marginal_3sigma = len(capability_results[capability_results['Capability'].str.contains('MARGINAL')])
    not_capable = len(capability_results[capability_results['Capability'].str.contains('NOT CAPABLE')])

    print(f"\nProcess Capability:")
    print(f"  ✅ Capable (6σ): {capable_6sigma}/{len(capability_results)} ({capable_6sigma/len(capability_results)*100:.1f}%)")
    print(f"  ⚠️  Marginal (3σ): {marginal_3sigma}/{len(capability_results)} ({marginal_3sigma/len(capability_results)*100:.1f}%)")
    print(f"  🔴 Not Capable: {not_capable}/{len(capability_results)} ({not_capable/len(capability_results)*100:.1f}%)")
    print(f"  📊 Average Cp: {capability_results['Cp'].mean():.2f}")
    print(f"  📊 Average Cpk: {capability_results['Cpk'].mean():.2f}")

    # ========================================================================
    # IMPROVEMENT RECOMMENDATIONS
    # ========================================================================

    print("\n" + "=" * 80)
    print("IMPROVEMENT RECOMMENDATIONS")
    print("=" * 80)

    if unacceptable_msa > 0:
        print("\n🔴 CRITICAL: Unacceptable Measurement Systems Detected")
        print("   Cannot make reliable decisions with Gage R&R > 30%")
        print("\n   Recommended Actions:")
        print("   1. Stabilize repeatability (use fixed seeds, mock external deps)")
        print("   2. Stabilize reproducibility (Docker/testcontainers, pre-warm cache)")
        print("   3. Use single-threaded execution (--test-threads=1)")
        print("   4. Add timeouts to prevent hangs")
        print("   5. Consider widening specification limits (if justified)")

    if not_capable > 0:
        print("\n🔴 CRITICAL: Process Not Capable")
        print("   Process cannot consistently meet specification limits")
        print("\n   Recommended Actions:")
        print("   1. Reduce variation (optimize test code, faster infrastructure)")
        print("   2. Center process (identify and fix slow tests)")
        print("   3. Parallelize independent tests")
        print("   4. Reassess specification limits (verify business needs)")

    if acceptable_msa == len(gage_rr_results) and capable_6sigma == len(capability_results):
        print("\n✅ EXCELLENT: All measurement systems acceptable and processes capable!")
        print("   Ready to proceed with test optimization and performance improvements")

    # ========================================================================
    # SAVE SUMMARY REPORT
    # ========================================================================

    summary_file = data_file.parent / "msa_summary_report.json"
    generate_summary_report(gage_rr_results, capability_results, summary_file)

    print("\n" + "=" * 80)
    print("ANALYSIS COMPLETE")
    print("=" * 80)
    print(f"\nData file: {data_file}")
    print(f"Summary report: {summary_file}")
    print("\nNext steps:")
    print("  1. Review Gage R&R results (focus on UNACCEPTABLE items)")
    print("  2. Review capability results (focus on NOT CAPABLE items)")
    print("  3. Implement improvement recommendations")
    print("  4. Re-run MSA after improvements to verify effectiveness")


if __name__ == "__main__":
    main()
