// Risk Management TypeScript Implementation
export interface Portfolio {
  id: string;
  positions: Position[];
  marketValue: number;
}

export interface Position {
  instrument: string;
  quantity: number;
  marketValue: number;
}

export class VaRCalculator {
  static historicalVaR(returns: number[], confidenceLevel: number): number {
    const sorted = [...returns].sort((a, b) => a - b);
    const idx = Math.floor((1 - confidenceLevel) * sorted.length);
    return -sorted[idx];
  }

  static parametricVaR(mean: number, stdDev: number, confidenceLevel: number): number {
    const zScore = confidenceLevel >= 0.99 ? 2.33 : confidenceLevel >= 0.95 ? 1.65 : 1.28;
    return -(mean - zScore * stdDev);
  }
}

export class GreeksCalculator {
  static delta(spot: number, strike: number, vol: number, time: number, rate: number): number {
    const d1 = (Math.log(spot / strike) + (rate + 0.5 * vol ** 2) * time) / (vol * Math.sqrt(time));
    return this.normalCDF(d1);
  }

  private static normalCDF(x: number): number {
    return 0.5 * (1 + this.erf(x / Math.sqrt(2)));
  }

  private static erf(x: number): number {
    const sign = x >= 0 ? 1 : -1;
    x = Math.abs(x);
    const a1 = 0.254829592, a2 = -0.284496736, a3 = 1.421413741;
    const a4 = -1.453152027, a5 = 1.061405429, p = 0.3275911;
    const t = 1.0 / (1.0 + p * x);
    const y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * Math.exp(-x * x);
    return sign * y;
  }
}
