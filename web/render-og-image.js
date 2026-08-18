/* Renders og-image.html to og-image.png (the social card).
 *
 * Usage, from the repository root:
 *
 *   npm install playwright
 *   npx playwright install chromium
 *   node web/render-og-image.js
 *
 * Then commit the regenerated web/og-image.png. Run this whenever
 * og-image.html changes, or when a style change should be reflected in the
 * card — nothing rebuilds it automatically, since CI has no browser.
 *
 * The card is rendered at 2x for crisp text on retina displays: a 1200x630
 * layout captured as a 2400x1260 PNG. Those dimensions are also declared in
 * index.html's og:image:width / og:image:height meta tags, so keep the three
 * in sync if you change them.
 */
'use strict';

const path = require('path');
const { chromium } = require('playwright');

const SOURCE = path.join(__dirname, 'og-image.html');
const OUTPUT = path.join(__dirname, 'og-image.png');

(async () => {
  const browser = await chromium.launch();
  const page = await browser.newPage({
    viewport: { width: 1200, height: 630 },
    deviceScaleFactor: 2,
  });
  await page.goto('file://' + SOURCE);
  // Give fonts a moment to settle before capturing.
  await page.waitForTimeout(200);
  await page.screenshot({ path: OUTPUT });
  await browser.close();
  console.log('wrote ' + OUTPUT + ' (2400x1260)');
})();
