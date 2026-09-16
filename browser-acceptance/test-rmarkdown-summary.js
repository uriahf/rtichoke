const { chromium } = require('playwright');
const http = require('http');
const fs = require('fs');
const path = require('path');

const REPORT_PATH = process.env.REPORT_PATH || path.join(__dirname, 'rmarkdown-summary.html');
const PORT = process.env.PORT ? parseInt(process.env.PORT, 10) : 8769;
const HTTP_URL = `http://127.0.0.1:${PORT}/rmarkdown-summary.html`;
const FILE_URL = `file://${path.resolve(REPORT_PATH)}`;

function startServer() {
  return new Promise((resolve, reject) => {
    const server = http.createServer((req, res) => {
      if (req.url === '/' || req.url.includes('rmarkdown-summary.html')) {
        res.writeHead(200, { 'Content-Type': 'text/html' });
        res.end(fs.readFileSync(REPORT_PATH));
      } else {
        res.writeHead(404);
        res.end('Not found');
      }
    });
    server.listen(PORT, '127.0.0.1', () => resolve(server));
    server.on('error', (err) => {
      if (err.code === 'EADDRINUSE') {
        // Server already running externally on this port
        resolve(null);
      } else {
        reject(err);
      }
    });
  });
}

async function runAcceptanceForUrl(browser, url, label) {
  console.log(`\n========================================`);
  console.log(`Running Acceptance Tests for: ${label}`);
  console.log(`URL: ${url}`);
  console.log(`========================================`);

  const page = await browser.newPage({ viewport: { width: 1200, height: 900 } });

  // Polyfill window.GLOBAL for legacy Plotly TypedArray if needed
  await page.addInitScript(() => {
    window.GLOBAL = window.GLOBAL || {};
    window.GLOBAL.WebModule = window.GLOBAL.WebModule || { exports: (b, c) => c(window) };
  });

  const consoleErrors = [];
  const pageErrors = [];
  const failedRequests = [];

  page.on('console', msg => {
    if (msg.type() === 'error') {
      consoleErrors.push(msg.text());
    }
  });

  page.on('pageerror', err => {
    pageErrors.push(err.message);
  });

  page.on('requestfailed', req => {
    const reqUrl = req.url();
    // Ignore only favicon.ico missing request if any
    if (reqUrl.endsWith('favicon.ico')) return;
    failedRequests.push(`${req.method()} ${reqUrl} - ${req.failure()?.errorText}`);
  });

  await page.goto(url, { waitUntil: 'load' });
  await page.waitForSelector('#rtichoke-viz-1', { state: 'attached' });
  await page.waitForSelector('#rtichoke-viz-2', { state: 'attached' });

  // 1. Root elements and distinct IDs
  const id1 = await page.$eval('#rtichoke-viz-1', el => el.id);
  const id2 = await page.$eval('#rtichoke-viz-2', el => el.id);
  console.log(`✓ Component 1 ID: "${id1}", Component 2 ID: "${id2}"`);
  if (id1 === id2) throw new Error('Component IDs must be distinct!');

  // Check Threshold SVG in active tab
  await page.waitForSelector('#rtichoke-viz-1 svg', { state: 'visible' });
  const bbox1 = await page.$eval('#rtichoke-viz-1 svg', svg => {
    const r = svg.getBoundingClientRect();
    return { width: r.width, height: r.height };
  });
  console.log(`✓ Threshold SVG bounding box: ${bbox1.width}x${bbox1.height}px`);
  if (bbox1.width <= 0 || bbox1.height <= 0) throw new Error('Threshold SVG has zero or negative dimensions!');

  // Switch to PPCR Tab
  const ppcrTabLink = page.locator('a[href="#by-predicted-positives-condition-rate-ppcr"], a:has-text("By Predicted Positives Condition Rate")').first();
  await ppcrTabLink.click();
  await page.waitForSelector('#rtichoke-viz-2 svg', { state: 'visible' });

  const bbox2 = await page.$eval('#rtichoke-viz-2 svg', svg => {
    const r = svg.getBoundingClientRect();
    return { width: r.width, height: r.height };
  });
  console.log(`✓ PPCR SVG bounding box: ${bbox2.width}x${bbox2.height}px`);
  if (bbox2.width <= 0 || bbox2.height <= 0) throw new Error('PPCR SVG has zero or negative dimensions!');

  // Container width clipping check
  const containerWidth = await page.$eval('.main-container', el => el.clientWidth);
  console.log(`✓ Main container width: ${containerWidth}px`);
  if (bbox1.width > containerWidth + 20) throw new Error('Threshold component overflows container width!');

  // Existing Plotly and reactable content check
  const hasReactable = await page.$eval('.reactable', el => !!el).catch(() => false);
  const plotlyCount = await page.$$eval('.js-plotly-plot', plots => plots.length);
  console.log(`✓ Reactable present: ${hasReactable}, Plotly charts count: ${plotlyCount}`);
  if (plotlyCount < 1) throw new Error('Existing Plotly charts missing from report!');

  // Switch back to Threshold tab to test interactions and tab switching
  const threshTabLink = page.locator('a[href="#by-probability-threshold"], a:has-text("By Probability Threshold")').first();
  await threshTabLink.click();
  await page.waitForSelector('#rtichoke-viz-1 svg', { state: 'visible' });

  // Function to extract operating point line marker X coordinate
  const getOpLineX = async (selector) => {
    return page.evaluate((sel) => {
      const el = document.querySelector(sel);
      if (!el) return null;
      const lines = Array.from(el.querySelectorAll('line'));
      if (lines.length >= 3) return lines[2].getAttribute('x1');
      return lines[0]?.getAttribute('x1');
    }, selector);
  };

  // Function to extract histogram bar geometries
  const getBarGeometries = async (selector) => {
    return page.evaluate((sel) => {
      const el = document.querySelector(sel);
      if (!el) return [];
      return Array.from(el.querySelectorAll('path'))
        .map(p => p.getAttribute('d'))
        .filter(d => d && (d.includes('M') || d.includes('L')));
    }, selector);
  };

  // Threshold component interaction & isolation
  const initialBarPaths1 = await getBarGeometries('#rtichoke-viz-1');
  const initialMarker1 = await getOpLineX('#rtichoke-viz-1');

  // Check initial marker for PPCR while in tab 2
  await ppcrTabLink.click();
  await page.waitForSelector('#rtichoke-viz-2 svg', { state: 'visible' });
  const initialMarker2 = await getOpLineX('#rtichoke-viz-2');

  // Switch back to Threshold tab and move slider
  await threshTabLink.click();
  await page.waitForSelector('#rtichoke-viz-1 svg', { state: 'visible' });

  // Change Threshold slider from 50 to 20
  await page.locator('#rtichoke-viz-1 input[type="range"]').fill("20");
  await page.waitForTimeout(200);

  const updatedMarker1 = await getOpLineX('#rtichoke-viz-1');
  const updatedBarPaths1 = await getBarGeometries('#rtichoke-viz-1');

  console.log(`✓ Threshold operating point line X before: ${initialMarker1}, after moving slider to 20: ${updatedMarker1}`);
  if (initialMarker1 === updatedMarker1) throw new Error('Threshold operating point line X did not change after moving threshold slider!');

  // Histogram bar path/rect geometry remains identical
  if (JSON.stringify(initialBarPaths1) !== JSON.stringify(updatedBarPaths1)) {
    throw new Error('Threshold histogram bar geometry changed unexpectedly on slider move!');
  }
  console.log(`✓ Threshold histogram bar geometry remained completely stable.`);

  // Verify PPCR marker unchanged after threshold slider move
  await ppcrTabLink.click();
  await page.waitForSelector('#rtichoke-viz-2 svg', { state: 'visible' });
  const untouchedMarker2 = await getOpLineX('#rtichoke-viz-2');

  if (initialMarker2 !== untouchedMarker2) {
    throw new Error('Moving threshold slider altered the PPCR component!');
  }
  console.log(`✓ PPCR component was isolated and unaffected by threshold slider move.`);

  // PPCR component interaction & isolation
  const initialBarPaths2 = await getBarGeometries('#rtichoke-viz-2');

  // Change PPCR slider from 50 to 80
  await page.locator('#rtichoke-viz-2 input[type="range"]').fill("80");
  await page.waitForTimeout(200);

  const updatedMarker2 = await getOpLineX('#rtichoke-viz-2');
  const updatedBarPaths2 = await getBarGeometries('#rtichoke-viz-2');

  console.log(`✓ PPCR operating point line X before: ${initialMarker2}, after moving slider to 80: ${updatedMarker2}`);
  if (initialMarker2 === updatedMarker2) throw new Error('PPCR operating point line X did not change after moving PPCR slider!');

  // Canonical rank-bin histogram bar geometry remains identical
  if (JSON.stringify(initialBarPaths2) !== JSON.stringify(updatedBarPaths2)) {
    throw new Error('PPCR rank-bin histogram geometry changed unexpectedly on slider move!');
  }
  console.log(`✓ PPCR rank-bin histogram bar geometry remained completely stable.`);

  // Verify Threshold marker unchanged after PPCR slider move
  await threshTabLink.click();
  await page.waitForSelector('#rtichoke-viz-1 svg', { state: 'visible' });
  const untouchedMarker1AfterPPCR = await getOpLineX('#rtichoke-viz-1');

  if (updatedMarker1 !== untouchedMarker1AfterPPCR) {
    throw new Error('Moving PPCR slider altered the Threshold component!');
  }
  console.log(`✓ Threshold component was isolated and unaffected by PPCR slider move.`);

  // Take screenshot for proof artifact
  const sanitizeLabel = label.toLowerCase().replace(/[^a-z0-9]/g, '-');
  const screenshotPath = path.join(__dirname, `rmarkdown-summary-${sanitizeLabel}.png`);
  await page.screenshot({ path: screenshotPath, fullPage: false });
  console.log(`✓ Saved artifact screenshot: ${screenshotPath}`);

  // Console & network cleanliness check
  console.log(`✓ Console errors count: ${consoleErrors.length}`);
  console.log(`✓ Page errors count: ${pageErrors.length}`);
  console.log(`✓ Failed requests count: ${failedRequests.length}`);

  if (consoleErrors.length > 0) {
    console.error('Console errors:', consoleErrors);
    throw new Error(`Browser console contains ${consoleErrors.length} error(s)!`);
  }
  if (pageErrors.length > 0) {
    console.error('Page errors:', pageErrors);
    throw new Error(`Browser page contains ${pageErrors.length} uncaught exception(s)!`);
  }
  if (failedRequests.length > 0) {
    console.error('Failed requests:', failedRequests);
    throw new Error(`Browser page contains ${failedRequests.length} failed asset/network request(s)!`);
  }

  await page.close();
  console.log(`ALL ACCEPTANCE CHECKS PASSED FOR ${label}!`);
}

async function main() {
  const server = await startServer();
  const browser = await chromium.launch({ headless: true });

  try {
    // Test http://127.0.0.1 protocol
    await runAcceptanceForUrl(browser, HTTP_URL, 'HTTP Protocol');

    // Test file:// protocol
    await runAcceptanceForUrl(browser, FILE_URL, 'file Protocol');

    console.log('\n========================================');
    console.log('REAL-BROWSER ACCEPTANCE SUITE PASSED SUCCESSFULLY!');
    console.log('========================================\n');
  } finally {
    await browser.close();
    if (server) server.close();
  }
}

main().catch(err => {
  console.error('\nACCEPTANCE SUITE FAILED:', err);
  process.exit(1);
});
