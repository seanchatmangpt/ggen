import { expect, test } from '@playwright/test';

test('renders deck.gl market and settles PCQ through the real UI', async ({ page }) => {
  await page.goto('/');
  await expect(page.getByRole('heading', { name: 'PCQ Atlas Marketplace' })).toBeVisible();
  await expect(page.locator('canvas')).toBeVisible();
  await expect(page.getByText('BALANCED')).toBeVisible();
  await page.getByRole('button', { name: 'Acquire' }).first().click();
  await expect(page.getByTestId('settlement-receipt')).toBeVisible();
});
