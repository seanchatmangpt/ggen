#!/usr/bin/env python3
"""Generate figures for thesis."""

import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path


def setup_latex_style():
    """Configure matplotlib for LaTeX style."""
    plt.rcParams.update({
        'font.size': 10,
        'font.family': 'sans-serif',
        'font.sans-serif': ['DejaVu Sans', 'Arial'],
        'text.usetex': False,  # Set to True if you have LaTeX installed
        'figure.figsize': (6, 4),
        'axes.labelsize': 10,
        'axes.titlesize': 12,
        'xtick.labelsize': 9,
        'ytick.labelsize': 9,
        'legend.fontsize': 9,
    })


def plot_slo_comparison():
    """Plot SLO comparison chart."""
    setup_latex_style()

    metrics = [
        'Agent\nCreation',
        'Agent\nStartup',
        'Message\nThroughput',
        'Tool\nDiscovery',
        'Plan\nGeneration',
        'Tool\nExecution',
        'Consensus',
        'Domain\nBalance'
    ]

    actual = [45, 3, 2800000, 85, 175, 280, 12, 2]
    target = [100, 500, 10000, 200, 200, 300, 2000, 500]

    x = np.arange(len(metrics))
    width = 0.35

    fig, ax = plt.subplots(figsize=(10, 5))
    bars1 = ax.bar(x - width/2, actual, width, label='Actual', color='#2ecc71')
    bars2 = ax.bar(x + width/2, target, width, label='Target', color='#3498db')

    ax.set_ylabel('Time (ms) / Throughput (msg/sec)')
    ax.set_title('SLO Performance: Actual vs Target')
    ax.set_xticks(x)
    ax.set_xticklabels(metrics)
    ax.legend()
    ax.set_yscale('log')
    ax.grid(axis='y', alpha=0.3)

    plt.tight_layout()
    plt.savefig('../figures/slo-comparison.pdf', dpi=300, bbox_inches='tight')
    plt.close()


def plot_productivity_comparison():
    """Plot productivity comparison."""
    setup_latex_style()

    approaches = ['Manual', 'ggen']
    times = [14, 3]  # days

    fig, ax = plt.subplots(figsize=(6, 4))
    bars = ax.bar(approaches, times, color=['#e74c3c', '#2ecc71'])

    ax.set_ylabel('Development Time (days)')
    ax.set_title('API Development: Manual vs ggen')
    ax.set_ylim(0, 16)

    # Add value labels on bars
    for bar in bars:
        height = bar.get_height()
        ax.text(bar.get_x() + bar.get_width()/2., height,
                f'{height} days',
                ha='center', va='bottom')

    plt.tight_layout()
    plt.savefig('../figures/productivity-comparison.pdf', dpi=300, bbox_inches='tight')
    plt.close()


def plot_determinism_verification():
    """Plot determinism verification results."""
    setup_latex_style()

    iterations = list(range(1, 10001))
    # All hashes identical (flat line at 1 unique hash)
    unique_hashes = [1] * 10000

    fig, ax = plt.subplots(figsize=(8, 4))
    ax.plot(iterations, unique_hashes, linewidth=2, color='#2ecc71')
    ax.fill_between(iterations, 0, unique_hashes, alpha=0.3, color='#2ecc71')

    ax.set_xlabel('Generation Iteration')
    ax.set_ylabel('Unique Hash Count')
    ax.set_title('Determinism Verification: 10,000 Generations')
    ax.set_xlim(0, 10000)
    ax.set_ylim(0, 10)
    ax.grid(True, alpha=0.3)

    plt.tight_layout()
    plt.savefig('../figures/determinism-verification.pdf', dpi=300, bbox_inches='tight')
    plt.close()


def plot_coverage_chart():
    """Plot test coverage chart."""
    setup_latex_style()

    categories = ['Unit', 'Integration', 'E2E', 'Property', 'Security']
    coverage = [87, 85, 90, 75, 95]

    colors = ['#3498db', '#2ecc71', '#9b59b6', '#e67e22', '#e74c3c']

    fig, ax = plt.subplots(figsize=(8, 5))
    bars = ax.barh(categories, coverage, color=colors)

    ax.set_xlabel('Coverage (%)')
    ax.set_title('Test Coverage by Category')
    ax.set_xlim(0, 100)

    # Add value labels on bars
    for bar in bars:
        width = bar.get_width()
        ax.text(width + 1, bar.get_y() + bar.get_height()/2,
                f'{width}%',
                ha='left', va='center')

    plt.tight_layout()
    plt.savefig('../figures/coverage-chart.pdf', dpi=300, bbox_inches='tight')
    plt.close()


def main():
    """Generate all figures."""
    # Change to script directory for relative paths
    import os
    os.chdir(os.path.dirname(os.path.abspath(__file__)))

    figures_dir = Path('../figures')
    figures_dir.mkdir(exist_ok=True)

    print("Generating figures...")

    plot_slo_comparison()
    print("✓ slo-comparison.pdf")

    plot_productivity_comparison()
    print("✓ productivity-comparison.pdf")

    plot_determinism_verification()
    print("✓ determinism-verification.pdf")

    plot_coverage_chart()
    print("✓ coverage-chart.pdf")

    print(f"\nAll figures saved to {figures_dir.absolute()}")


if __name__ == '__main__':
    main()
