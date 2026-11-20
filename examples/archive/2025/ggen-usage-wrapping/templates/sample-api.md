---
name: rest-api-endpoint
description: REST API endpoint template with CRUD operations
version: 1.0.0
author: ggen-examples
tags: [api, rest, crud, typescript]
---
// {{ resource_name | capitalize }} API Endpoint

import { Router, Request, Response } from 'express';
import { {{ resource_name | capitalize }}Service } from '../services/{{ resource_name }}.service';
import { validate{{ resource_name | capitalize }} } from '../validators/{{ resource_name }}.validator';

const router = Router();
const {{ resource_name }}Service = new {{ resource_name | capitalize }}Service();

/**
 * GET /{{ resource_name }}
 * Retrieve all {{ resource_name }} items
 */
router.get('/{{ resource_name }}', async (req: Request, res: Response) => {
  try {
    const items = await {{ resource_name }}Service.findAll();
    res.json({
      success: true,
      data: items,
      count: items.length
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: 'Failed to retrieve {{ resource_name }} items'
    });
  }
});

/**
 * GET /{{ resource_name }}/:id
 * Retrieve a specific {{ resource_name }} item by ID
 */
router.get('/{{ resource_name }}/:id', async (req: Request, res: Response) => {
  try {
    const { id } = req.params;
    const item = await {{ resource_name }}Service.findById(id);

    if (!item) {
      return res.status(404).json({
        success: false,
        error: '{{ resource_name | capitalize }} not found'
      });
    }

    res.json({
      success: true,
      data: item
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: 'Failed to retrieve {{ resource_name }} item'
    });
  }
});

/**
 * POST /{{ resource_name }}
 * Create a new {{ resource_name }} item
 */
router.post('/{{ resource_name }}', validate{{ resource_name | capitalize }}, async (req: Request, res: Response) => {
  try {
    const newItem = await {{ resource_name }}Service.create(req.body);

    res.status(201).json({
      success: true,
      data: newItem,
      message: '{{ resource_name | capitalize }} created successfully'
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: 'Failed to create {{ resource_name }} item'
    });
  }
});

/**
 * PUT /{{ resource_name }}/:id
 * Update an existing {{ resource_name }} item
 */
router.put('/{{ resource_name }}/:id', validate{{ resource_name | capitalize }}, async (req: Request, res: Response) => {
  try {
    const { id } = req.params;
    const updatedItem = await {{ resource_name }}Service.update(id, req.body);

    if (!updatedItem) {
      return res.status(404).json({
        success: false,
        error: '{{ resource_name | capitalize }} not found'
      });
    }

    res.json({
      success: true,
      data: updatedItem,
      message: '{{ resource_name | capitalize }} updated successfully'
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: 'Failed to update {{ resource_name }} item'
    });
  }
});

/**
 * DELETE /{{ resource_name }}/:id
 * Delete a {{ resource_name }} item
 */
router.delete('/{{ resource_name }}/:id', async (req: Request, res: Response) => {
  try {
    const { id } = req.params;
    const deleted = await {{ resource_name }}Service.delete(id);

    if (!deleted) {
      return res.status(404).json({
        success: false,
        error: '{{ resource_name | capitalize }} not found'
      });
    }

    res.json({
      success: true,
      message: '{{ resource_name | capitalize }} deleted successfully'
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      error: 'Failed to delete {{ resource_name }} item'
    });
  }
});

export default router;
