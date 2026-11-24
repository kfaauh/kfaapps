// Generate a synthetic publication with MONITOR_TEST_RDS=/absolute/demo.rds Rscript monitorering/tests/analysis_routes.R.
// Serve it with MONITOR_PUBLICATION=/absolute/demo.rds and run this script with Playwright.
const { chromium } = require('playwright');
const assert = require('node:assert/strict');
const fs = require('node:fs');

async function download(page, selector) {
  const pending = page.waitForEvent('download');
  await page.locator(selector).click();
  return fs.readFileSync(await (await pending).path(), 'utf8');
}

async function selectProduct(page, query) {
  const search = page.locator('#tariff_product + .selectize-control input');
  await search.fill(query);
  await page.locator('.selectize-dropdown [data-value="000001"]:visible').click();
}

(async () => {
  const browser = await chromium.launch({
    headless: true,
    executablePath: process.env.CHROMIUM_PATH || undefined,
    args: ['--no-sandbox', '--disable-dev-shm-usage']
  });
  try {
    const page = await browser.newPage({ viewport: { width: 1440, height: 1050 }, acceptDownloads: true });
    page.setDefaultTimeout(9000);
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(process.env.MONITOR_TEST_URL || 'http://127.0.0.1:3838/');
    const tab = name => page.locator('#view .nav-link').getByText(name, { exact: true });
    const inventory = page.locator('#inventory');
    await inventory.getByRole('button', { name: /A10/ }).waitFor();
    assert.equal(await inventory.locator('.coverage-grid tr').count(), 5);
    assert.equal(await inventory.getByRole('row', { name: /A10/ }).locator('.data-empty').count(), 1);
    await inventory.getByRole('button', { name: /A10/ }).click();
    await page.waitForFunction(() => document.querySelector('#to').value === '2026-04');
    await page.locator('#to').selectOption('2026-03');
    await page.locator('#overview').getByText('480', { exact: true }).waitFor();
    await page.locator('#year_comparison').getByText('100,00', { exact: true }).waitFor();
    await page.locator('#region').selectOption('MJ');
    await page.locator('#overview').getByText('48', { exact: true }).waitFor();
    const consumption = await download(page, '#export');
    assert.match(consumption, /regionalt_tilskud_kr/);
    assert.match(consumption, /"MJ"/);
    assert.doesNotMatch(consumption, /"DK"/);
    await tab('Fremskrivning').click();
    await page.locator('#forecast_summary').getByText('1.920 kr.', { exact: true }).waitFor();
    await page.locator('#estimate_series img').waitFor();
    await page.getByText('Avanceret: Beregning med lægemiddelpriser', { exact: true }).click();
    await page.locator('#scenario').getByText('96 kr.', { exact: true }).waitFor();

    await tab('Overblik').click();
    await page.locator('#inventory_region').selectOption('MJ');
    await inventory.getByRole('button', { name: /N06/ }).click();
    await page.waitForFunction(() => document.querySelector('#to').value === '2026-09');
    await page.locator('#from').selectOption('2026-08');
    await page.locator('#to').selectOption('2026-10');
    await tab('Overblik').click();
    await page.locator('#selection_status').getByText('Intet udtræk: 2026-10').waitFor();
    await tab('Fremskrivning').click();
    await page.locator('#forecast_summary').getByText('Annualisering med manglende måneder').waitFor();
    await page.locator('#forecast_summary').getByText('5.400 kr.', { exact: true }).waitFor();
    await tab('Datastatus').click();
    await page.locator('#coverage').getByText('Intet indlæst udtræk').waitFor();
    await page.getByText('Import- og revisionshistorik', { exact: true }).click();
    await page.locator('#extracts table tbody').waitFor();

    await tab('Takstpriser').click();
    await selectProduct(page, 'A10BA02');
    await page.locator('#prices').getByText('01.12.2025 14:00:00', { exact: true }).waitFor();
    await page.locator('#prices').getByText('03.09.2026 01:30:00', { exact: true }).waitFor();
    await page.locator('#tariff_history_ui').getByText('65,00 kr.', { exact: true }).waitFor();
    await page.locator('#tariff_history_ui').getByText(/\+5,00 kr. siden/).waitFor();
    await page.locator('#price_plot img').waitFor();
    // Locate the rightmost blue point in the rendered plot, then move the real pointer.
    const image = page.locator('#price_plot img');
    await image.scrollIntoViewIfNeeded();
    await page.waitForFunction(() => document.querySelector('#price_plot img')?.naturalWidth > 0);
    const position = await image.evaluate(img => {
      const canvas = document.createElement('canvas');
      canvas.width = img.naturalWidth;
      canvas.height = img.naturalHeight;
      const context = canvas.getContext('2d');
      context.drawImage(img, 0, 0);
      const { width, height } = canvas;
      const pixels = context.getImageData(0, 0, width, height).data;
      const box = img.getBoundingClientRect();
      for (let x = width - 1; x > width / 2; x--) {
        for (let y = 0; y < height; y++) {
          const index = (y * width + x) * 4;
          if (pixels[index] === 0 && pixels[index + 1] === 51 && pixels[index + 2] === 160)
            return { x: box.left + x * box.width / width, y: box.top + y * box.height / height };
        }
      }
      throw new Error('The latest dispensing-price point was not found in the plot.');
    });
    await page.mouse.move(position.x, position.y);
    await page.locator('#price_point').getByText(/06.09.2026.*Varenummer 000001/).waitFor();
    const prices = await download(page, '#price_export');
    assert.match(prices, /"000001"/);
    assert.match(prices, /03\.09\.2026 01:30:00/);
    await page.getByText('Sammenligning af takstudtræk', { exact: true }).click();
    await page.locator('#changes').getByText('Første komplette udtræk').waitFor();

    const mobile = await browser.newPage({ viewport: { width: 390, height: 900 }, isMobile: true, hasTouch: true });
    mobile.setDefaultTimeout(9000);
    mobile.on('pageerror', error => errors.push(error.message));
    await mobile.goto(process.env.MONITOR_TEST_URL || 'http://127.0.0.1:3838/');
    await mobile.locator('#inventory .coverage-grid').waitFor();
    await mobile.locator('[aria-controls="filters_panel"]').click();
    await mobile.locator('#region').selectOption('MJ');
    await mobile.locator('[aria-controls="filters_panel"]').click();
    for (const [view, width] of [[page, 1440], [mobile, 390]]) {
      const nav = view.locator('#view .nav-link');
      for (const label of ['Overblik', 'Forbrug og tilskud', 'Takstpriser', 'Fremskrivning', 'Datastatus']) {
        await nav.getByText(label, { exact: true }).click();
        if (label === 'Takstpriser' && width === 390) {
          await selectProduct(view, 'Eksempelmedicin');
          await view.locator('#prices table[id]').waitFor();
        }
        assert(await view.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1),
          `${label} overflows the viewport at ${width}px`);
        assert.equal(await view.locator('.shiny-output-error:visible').count(), 0);
      }
      if (process.env.MONITOR_TEST_SCREENSHOT) {
        await nav.getByText('Overblik', { exact: true }).click();
        await view.screenshot({ path: `${process.env.MONITOR_TEST_SCREENSHOT}-${width}.png`, fullPage: true });
      }
    }
    assert.deepEqual(errors, []);
    console.log('PASS: desktop/mobile Shiny demo, price hover, and both CSV downloads.');
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exit(1); });
