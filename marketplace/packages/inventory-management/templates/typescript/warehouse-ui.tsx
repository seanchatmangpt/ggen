// Inventory Management - Warehouse Management UI (TypeScript/React)
// Dashboard for stock levels, movements, and reorder management

import React, { useState, useEffect } from 'react';

interface StockLevel {
  productId: string;
  productName: string;
  locationId: string;
  locationName: string;
  quantityOnHand: number;
  quantityReserved: number;
  quantityAvailable: number;
  reorderPoint: number;
  currentValue: number;
}

interface StockMovement {
  movementId: string;
  productId: string;
  productName: string;
  movementType: 'Receive' | 'Sell' | 'Transfer' | 'Adjust' | 'Return' | 'Scrap';
  quantity: number;
  fromLocation?: string;
  toLocation?: string;
  timestamp: Date;
  reason?: string;
}

interface ReorderSuggestion {
  productId: string;
  productName: string;
  currentStock: number;
  reorderPoint: number;
  suggestedQty: number;
  leadTimeDays: number;
}

// Stock Levels Dashboard
export const StockLevelsDashboard: React.FC = () => {
  const [stockLevels, setStockLevels] = useState<StockLevel[]>([]);
  const [filterLocation, setFilterLocation] = useState<string>('all');
  const [showLowStock, setShowLowStock] = useState(false);

  useEffect(() => {
    loadStockLevels();
  }, [filterLocation]);

  const loadStockLevels = async () => {
    // Mock API call
    const data: StockLevel[] = [
      {
        productId: 'SKU-12345',
        productName: 'Widget Pro',
        locationId: 'WH-1',
        locationName: 'Main Warehouse',
        quantityOnHand: 150,
        quantityReserved: 30,
        quantityAvailable: 120,
        reorderPoint: 50,
        currentValue: 1500.00,
      },
      {
        productId: 'SKU-67890',
        productName: 'Gadget Ultra',
        locationId: 'WH-1',
        locationName: 'Main Warehouse',
        quantityOnHand: 25,
        quantityReserved: 10,
        quantityAvailable: 15,
        reorderPoint: 40,
        currentValue: 750.00,
      },
    ];
    setStockLevels(data);
  };

  const filteredStock = stockLevels.filter(stock => {
    const locationMatch = filterLocation === 'all' || stock.locationId === filterLocation;
    const lowStockMatch = !showLowStock || stock.quantityAvailable <= stock.reorderPoint;
    return locationMatch && lowStockMatch;
  });

  return (
    <div className="stock-dashboard">
      <h1>Inventory Stock Levels</h1>

      <div className="filters">
        <select value={filterLocation} onChange={e => setFilterLocation(e.target.value)}>
          <option value="all">All Locations</option>
          <option value="WH-1">Main Warehouse</option>
          <option value="WH-2">Distribution Center</option>
          <option value="STORE-1">Retail Store #1</option>
        </select>

        <label>
          <input
            type="checkbox"
            checked={showLowStock}
            onChange={e => setShowLowStock(e.target.checked)}
          />
          Show Low Stock Only
        </label>
      </div>

      <table className="stock-table">
        <thead>
          <tr>
            <th>SKU</th>
            <th>Product</th>
            <th>Location</th>
            <th>On Hand</th>
            <th>Reserved</th>
            <th>Available</th>
            <th>Reorder Point</th>
            <th>Value</th>
            <th>Status</th>
          </tr>
        </thead>
        <tbody>
          {filteredStock.map(stock => (
            <tr key={`${stock.productId}-${stock.locationId}`}>
              <td>{stock.productId}</td>
              <td>{stock.productName}</td>
              <td>{stock.locationName}</td>
              <td>{stock.quantityOnHand}</td>
              <td>{stock.quantityReserved}</td>
              <td className={stock.quantityAvailable <= stock.reorderPoint ? 'low-stock' : ''}>
                {stock.quantityAvailable}
              </td>
              <td>{stock.reorderPoint}</td>
              <td>${stock.currentValue.toFixed(2)}</td>
              <td>
                {stock.quantityAvailable <= stock.reorderPoint ? (
                  <span className="badge badge-warning">REORDER NEEDED</span>
                ) : (
                  <span className="badge badge-success">GOOD</span>
                )}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

// Stock Movements Log
export const StockMovementsLog: React.FC = () => {
  const [movements, setMovements] = useState<StockMovement[]>([]);
  const [filterType, setFilterType] = useState<string>('all');

  useEffect(() => {
    loadMovements();
  }, []);

  const loadMovements = async () => {
    // Mock data
    const data: StockMovement[] = [
      {
        movementId: 'MOV-1001',
        productId: 'SKU-12345',
        productName: 'Widget Pro',
        movementType: 'Receive',
        quantity: 100,
        toLocation: 'Main Warehouse',
        timestamp: new Date('2025-11-08T10:00:00Z'),
        reason: 'Purchase Order #5001',
      },
      {
        movementId: 'MOV-1002',
        productId: 'SKU-12345',
        productName: 'Widget Pro',
        movementType: 'Sell',
        quantity: -30,
        fromLocation: 'Main Warehouse',
        timestamp: new Date('2025-11-08T14:30:00Z'),
        reason: 'Sales Order #7023',
      },
    ];
    setMovements(data);
  };

  const filteredMovements = filterType === 'all'
    ? movements
    : movements.filter(m => m.movementType === filterType);

  return (
    <div className="movements-log">
      <h1>Stock Movements</h1>

      <div className="filters">
        <select value={filterType} onChange={e => setFilterType(e.target.value)}>
          <option value="all">All Movements</option>
          <option value="Receive">Receive</option>
          <option value="Sell">Sell</option>
          <option value="Transfer">Transfer</option>
          <option value="Adjust">Adjustment</option>
        </select>
      </div>

      <table className="movements-table">
        <thead>
          <tr>
            <th>Movement ID</th>
            <th>Product</th>
            <th>Type</th>
            <th>Quantity</th>
            <th>From</th>
            <th>To</th>
            <th>Date</th>
            <th>Reason</th>
          </tr>
        </thead>
        <tbody>
          {filteredMovements.map(movement => (
            <tr key={movement.movementId}>
              <td>{movement.movementId}</td>
              <td>{movement.productName}</td>
              <td>
                <span className={`badge movement-${movement.movementType.toLowerCase()}`}>
                  {movement.movementType}
                </span>
              </td>
              <td className={movement.quantity < 0 ? 'negative' : 'positive'}>
                {movement.quantity > 0 ? '+' : ''}{movement.quantity}
              </td>
              <td>{movement.fromLocation || '-'}</td>
              <td>{movement.toLocation || '-'}</td>
              <td>{new Date(movement.timestamp).toLocaleString()}</td>
              <td>{movement.reason}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

// Reorder Management
export const ReorderManagement: React.FC = () => {
  const [suggestions, setSuggestions] = useState<ReorderSuggestion[]>([]);

  useEffect(() => {
    loadReorderSuggestions();
  }, []);

  const loadReorderSuggestions = async () => {
    const data: ReorderSuggestion[] = [
      {
        productId: 'SKU-67890',
        productName: 'Gadget Ultra',
        currentStock: 15,
        reorderPoint: 40,
        suggestedQty: 100,
        leadTimeDays: 7,
      },
    ];
    setSuggestions(data);
  };

  const handleCreatePO = (productId: string, quantity: number) => {
    alert(`Creating Purchase Order for ${quantity} units of ${productId}`);
    // TODO: API call to create PO
  };

  return (
    <div className="reorder-management">
      <h1>Reorder Management</h1>

      {suggestions.length === 0 ? (
        <div className="no-suggestions">
          All stock levels are healthy. No reorders needed.
        </div>
      ) : (
        <table className="reorder-table">
          <thead>
            <tr>
              <th>Product</th>
              <th>Current Stock</th>
              <th>Reorder Point</th>
              <th>Suggested Qty</th>
              <th>Lead Time</th>
              <th>Action</th>
            </tr>
          </thead>
          <tbody>
            {suggestions.map(suggestion => (
              <tr key={suggestion.productId}>
                <td>{suggestion.productName}</td>
                <td className="low-stock">{suggestion.currentStock}</td>
                <td>{suggestion.reorderPoint}</td>
                <td>{suggestion.suggestedQty}</td>
                <td>{suggestion.leadTimeDays} days</td>
                <td>
                  <button
                    onClick={() => handleCreatePO(suggestion.productId, suggestion.suggestedQty)}
                  >
                    Create PO
                  </button>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      )}
    </div>
  );
};

export default StockLevelsDashboard;
