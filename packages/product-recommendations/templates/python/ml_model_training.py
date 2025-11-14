"""
Product Recommendations - ML Model Training (Python)
Train collaborative filtering models for batch recommendations
"""

import numpy as np
from typing import List, Dict, Tuple


class MatrixFactorizationModel:
    """
    Simple matrix factorization for collaborative filtering
    Uses Alternating Least Squares (ALS) algorithm
    """

    def __init__(self, n_factors: int = 20, learning_rate: float = 0.01, n_epochs: int = 20):
        self.n_factors = n_factors
        self.learning_rate = learning_rate
        self.n_epochs = n_epochs
        self.user_factors = None
        self.item_factors = None
        self.user_id_map = {}
        self.item_id_map = {}

    def fit(self, interactions: List[Tuple[str, str, float]]):
        """
        Train the model on user-item interactions
        interactions: List of (user_id, item_id, rating)
        """
        # Create ID mappings
        user_ids = set(u for u, _, _ in interactions)
        item_ids = set(i for _, i, _ in interactions)

        self.user_id_map = {uid: idx for idx, uid in enumerate(user_ids)}
        self.item_id_map = {iid: idx for idx, iid in enumerate(item_ids)}

        n_users = len(user_ids)
        n_items = len(item_ids)

        # Initialize factor matrices
        self.user_factors = np.random.normal(0, 0.1, (n_users, self.n_factors))
        self.item_factors = np.random.normal(0, 0.1, (n_items, self.n_factors))

        # Create interaction matrix
        interaction_matrix = np.zeros((n_users, n_items))
        for user_id, item_id, rating in interactions:
            u_idx = self.user_id_map[user_id]
            i_idx = self.item_id_map[item_id]
            interaction_matrix[u_idx, i_idx] = rating

        # Train with SGD
        for epoch in range(self.n_epochs):
            for user_id, item_id, rating in interactions:
                u_idx = self.user_id_map[user_id]
                i_idx = self.item_id_map[item_id]

                # Predict rating
                prediction = np.dot(self.user_factors[u_idx], self.item_factors[i_idx])
                error = rating - prediction

                # Update factors
                user_factor_update = error * self.item_factors[i_idx]
                item_factor_update = error * self.user_factors[u_idx]

                self.user_factors[u_idx] += self.learning_rate * user_factor_update
                self.item_factors[i_idx] += self.learning_rate * item_factor_update

            # Print progress
            if (epoch + 1) % 5 == 0:
                rmse = self._calculate_rmse(interactions)
                print(f"Epoch {epoch + 1}/{self.n_epochs}, RMSE: {rmse:.4f}")

    def predict(self, user_id: str, item_id: str) -> float:
        """Predict rating for user-item pair"""
        if user_id not in self.user_id_map or item_id not in self.item_id_map:
            return 0.0

        u_idx = self.user_id_map[user_id]
        i_idx = self.item_id_map[item_id]

        return np.dot(self.user_factors[u_idx], self.item_factors[i_idx])

    def recommend_for_user(self, user_id: str, n: int = 10, exclude_items: set = None) -> List[Tuple[str, float]]:
        """Generate top-N recommendations for a user"""
        if user_id not in self.user_id_map:
            return []

        u_idx = self.user_id_map[user_id]
        exclude_items = exclude_items or set()

        # Calculate scores for all items
        scores = []
        for item_id, i_idx in self.item_id_map.items():
            if item_id not in exclude_items:
                score = np.dot(self.user_factors[u_idx], self.item_factors[i_idx])
                scores.append((item_id, score))

        # Sort by score and return top N
        scores.sort(key=lambda x: x[1], reverse=True)
        return scores[:n]

    def _calculate_rmse(self, interactions: List[Tuple[str, str, float]]) -> float:
        """Calculate root mean squared error"""
        squared_errors = []
        for user_id, item_id, rating in interactions:
            prediction = self.predict(user_id, item_id)
            squared_errors.append((rating - prediction) ** 2)

        return np.sqrt(np.mean(squared_errors))


# Example usage
if __name__ == '__main__':
    # Mock training data: (user_id, product_id, rating)
    interactions = [
        ('user_1', 'product_a', 5.0),
        ('user_1', 'product_b', 4.0),
        ('user_2', 'product_a', 4.0),
        ('user_2', 'product_c', 5.0),
        ('user_3', 'product_b', 3.0),
        ('user_3', 'product_c', 5.0),
        ('user_4', 'product_a', 5.0),
        ('user_4', 'product_d', 4.0),
    ]

    # Train model
    model = MatrixFactorizationModel(n_factors=10, n_epochs=50)
    model.fit(interactions)

    # Generate recommendations
    recommendations = model.recommend_for_user('user_1', n=3, exclude_items={'product_a', 'product_b'})
    print("\nRecommendations for user_1:")
    for product_id, score in recommendations:
        print(f"  {product_id}: {score:.2f}")
