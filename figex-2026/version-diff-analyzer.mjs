/**
 * Analyzes the version difference between two semantic version strings.
 * @param {string} v1 - The base version string (e.g., "1.2.3")
 * @param {string} v2 - The target version string (e.g., "1.3.0")
 * @returns {{major: number, minor: number, patch: number}} The difference in version components
 * @throws {Error} If either version string is invalid
 * @example
 * const diff = analyzeVersionDiff("1.2.3", "1.3.0")
 * // diff => { major: 0, minor: 1, patch: 0 }
 */
export function analyzeVersionDiff(v1, v2) {
  const parseVersion = (version) => {
    const match = version.match(/^(\d+)\.(\d+)\.(\d+)$/);
    if (!match) throw new Error(`Invalid version format: ${version}`);
    return [parseInt(match[1]), parseInt(match[2]), parseInt(match[3])];
  };

  const [major1, minor1, patch1] = parseVersion(v1);
  const [major2, minor2, patch2] = parseVersion(v2);

  return {
    major: major2 - major1,
    minor: minor2 - minor1,
    patch: patch2 - patch1,
  };
}

/**
 * Categorizes changes between two semantic versions into patch, minor, or major.
 * @param {{major: number, minor: number, patch: number}} diff - The version difference
 * @returns {"patch" | "minor" | "major"} The category of change
 * @throws {Error} If the diff is invalid
 * @example
 * const category = categorizeChanges({ major: 0, minor: 1, patch: 0 })
 * // category => "minor"
 */
export function categorizeChanges(diff) {
  if (diff.major > 0) return "major";
  if (diff.minor > 0) return "minor";
  if (diff.patch > 0) return "patch";
  throw new Error("No changes detected");
}

/**
 * Assesses the impact of changes between two semantic versions.
 * @param {{major: number, minor: number, patch: number}} diff - The version difference
 * @returns {{impact: "none" | "low" | "medium" | "high", description: string}} The impact assessment
 * @throws {Error} If the diff is invalid
 * @example
 * const impact = assessImpact({ major: 0, minor: 1, patch: 0 })
 * // impact => { impact: "medium", description: "Breaking changes in minor version" }
 */
export function assessImpact(diff) {
  if (diff.major > 0) return { impact: "high", description: "Major version change: significant breaking changes" };
  if (diff.minor > 0) return { impact: "medium", description: "Minor version change: breaking changes in new features" };
  if (diff.patch > 0) return { impact: "low", description: "Patch version change: non-breaking bug fixes" };
  throw new Error("No changes detected");
}

/**
 * Recommends a semantic version based on the analyzed diff and impact.
 * @param {{major: number, minor: number, patch: number}} diff - The version difference
 * @param {{impact: "none" | "low" | "medium" | "high", description: string}} impact - The impact assessment
 * @returns {{recommendedVersion: string, changelog: string}} The recommended version and changelog
 * @throws {Error} If the diff or impact is invalid
 * @example
 * const recommendation = recommendVersion({ major: 0, minor: 1, patch: 0 }, { impact: "medium", description: "Breaking changes in minor version" })
 * // recommendation => { recommendedVersion: "1.1.0", changelog: "BREAKING: New features introduced in minor version" }
 */
export function recommendVersion(diff, impact) {
  if (diff.major > 0) {
    return {
      recommendedVersion: `${diff.major + 1}.0.0`,
      changelog: "MAJOR: Significant breaking changes. All dependent systems may require updates.",
    };
  }

  if (diff.minor > 0) {
    return {
      recommendedVersion: `${diff.minor + 1}.0.0`,
      changelog: "MINOR: Breaking changes in new features. Dependent systems may need updates.",
    };
  }

  if (diff.patch > 0) {
    return {
      recommendedVersion: `1.${diff.patch + 1}.0`,
      changelog: "PATCH: Non-breaking bug fixes. No changes to existing functionality.",
    };
  }

  throw new Error("No changes detected");
}