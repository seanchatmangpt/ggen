// Product Recommendations - UI Widgets (TypeScript/React)
// Reusable recommendation components for e-commerce sites

import React, { useState, useEffect } from 'react';

interface Product {
  productId: string;
  productName: string;
  price: number;
  imageUrl?: string;
  rating?: number;
}

interface RecommendationProps {
  userId: string;
  context: 'homepage' | 'product-page' | 'cart' | 'checkout';
  currentProductId?: string;
  limit?: number;
}

// API client for recommendations
class RecommendationAPI {
  private baseUrl: string;

  constructor(baseUrl: string = '/api/recommendations') {
    this.baseUrl = baseUrl;
  }

  async getPersonalizedRecs(userId: string, limit: number = 10): Promise<Product[]> {
    const response = await fetch(`${this.baseUrl}/personalized?user_id=${userId}&limit=${limit}`);
    if (!response.ok) throw new Error('Failed to fetch recommendations');
    return response.json();
  }

  async getBundleRecs(productId: string, limit: number = 5): Promise<Product[]> {
    const response = await fetch(`${this.baseUrl}/bundle?product_id=${productId}&limit=${limit}`);
    if (!response.ok) throw new Error('Failed to fetch bundle recommendations');
    return response.json();
  }

  async trackBehavior(userId: string, productId: string, behaviorType: string): Promise<void> {
    await fetch(`${this.baseUrl}/track`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ userId, productId, behaviorType }),
    });
  }
}

// Main Recommendation Widget
export const RecommendationWidget: React.FC<RecommendationProps> = ({
  userId,
  context,
  currentProductId,
  limit = 6,
}) => {
  const [recommendations, setRecommendations] = useState<Product[]>([]);
  const [loading, setLoading] = useState(true);

  const api = new RecommendationAPI();

  useEffect(() => {
    loadRecommendations();
  }, [userId, currentProductId]);

  const loadRecommendations = async () => {
    try {
      setLoading(true);

      let products: Product[];
      if (context === 'product-page' && currentProductId) {
        products = await api.getBundleRecs(currentProductId, limit);
      } else {
        products = await api.getPersonalizedRecs(userId, limit);
      }

      setRecommendations(products);
    } catch (error) {
      console.error('Failed to load recommendations:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleProductClick = async (productId: string) => {
    await api.trackBehavior(userId, productId, 'click');
  };

  if (loading) {
    return <div className="recommendations-loading">Loading recommendations...</div>;
  }

  if (recommendations.length === 0) {
    return null;
  }

  const title = context === 'product-page'
    ? 'Frequently Bought Together'
    : 'Recommended For You';

  return (
    <div className="recommendation-widget">
      <h2>{title}</h2>

      <div className="product-carousel">
        {recommendations.map(product => (
          <div
            key={product.productId}
            className="product-card"
            onClick={() => handleProductClick(product.productId)}
          >
            {product.imageUrl && (
              <img src={product.imageUrl} alt={product.productName} />
            )}
            <h3>{product.productName}</h3>
            <div className="product-price">${product.price.toFixed(2)}</div>
            {product.rating && (
              <div className="product-rating">
                {'★'.repeat(Math.round(product.rating))}
                {'☆'.repeat(5 - Math.round(product.rating))}
              </div>
            )}
            <button className="add-to-cart">Add to Cart</button>
          </div>
        ))}
      </div>
    </div>
  );
};

// Trending Products Widget
export const TrendingProductsWidget: React.FC<{ limit?: number }> = ({ limit = 6 }) => {
  const [products, setProducts] = useState<Product[]>([]);

  useEffect(() => {
    loadTrending();
  }, []);

  const loadTrending = async () => {
    const api = new RecommendationAPI();
    const response = await fetch('/api/recommendations/trending?limit=' + limit);
    if (response.ok) {
      setProducts(await response.json());
    }
  };

  return (
    <div className="trending-widget">
      <h2>Trending Now</h2>
      <div className="product-grid">
        {products.map(product => (
          <div key={product.productId} className="trending-product">
            <img src={product.imageUrl} alt={product.productName} />
            <h4>{product.productName}</h4>
            <span className="price">${product.price.toFixed(2)}</span>
          </div>
        ))}
      </div>
    </div>
  );
};

export default RecommendationWidget;
