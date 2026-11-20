import type { NextConfig } from 'next';

const nextConfig: NextConfig = {
  reactStrictMode: true,
  experimental: { optimizePackageImports: ['@deck.gl/layers', '@deck.gl/geo-layers'] },
};

export default nextConfig;
