// Optional real Chromium UI test; npm install playwright; npx playwright install chromium.
// Entire report and query endpoint are mocked; no live credentials or data requests.
const {chromium}=require('playwright');
const assert=require('node:assert/strict');
const fs=require('node:fs');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.CHROMIUM_PATH||undefined,args:['--no-sandbox','--disable-gpu','--disable-dev-shm-usage']});
 try {
  const context=await browser.newContext({acceptDownloads:true});const page=await context.newPage();
  const p=require('../browser/monitorering.js');
  const rows=fs.readFileSync(__dirname+'/fixtures/monitorering_forbrug_N06_2025-01.csv','utf8').replace(/^\uFEFF/,'').trim().split(/\r?\n/).slice(1).map(s=>s.split(';'));
  const keys=['G0','G1','G2','G3','G4','M0'];
  const response={results:[{result:{data:{descriptor:{Select:p.EXPECTED.map((Name,i)=>({Name,Value:keys[i]}))},dsr:{DS:[{PH:[{DM0:rows.map((r,i)=>({...(i===0?{S:keys.map(N=>({N}))}:{}),C:[r[0]+' - test',r[1],2025,'Jan 2025',r[3]==='ddd'?'Mængdeforbrug':'Regionalt tilskud',+r[4]]}))}]}]}}}}]};
  await page.route('**/*',route=>{
   if(route.request().url().includes('/explore/querydata'))return route.fulfill({contentType:'application/json',body:JSON.stringify(response),headers:{'access-control-allow-origin':'*'}});
   return route.fulfill({contentType:'text/html',body:'<html><body><h1>Synthetic report fixture</h1></body></html>'});
  });
  await page.goto('https://app.powerbi.com/synthetic-test');
  await page.addScriptTag({content:fs.readFileSync(__dirname+'/../browser/monitorering.js','utf8')});
  await page.evaluate(()=>Monitorering.panel());
  await page.getByRole('button',{name:'Start udtræk',exact:true}).click();
  await page.waitForFunction(()=>!!window.monitoreringCollector);
  await page.evaluate(()=>fetch('https://wabi-west-europe-g-primary-redirect.analysis.windows.net/explore/querydata',{
   method:'POST',headers:{authorization:'Bearer SYNTHETIC-TEST-ONLY'}}));
  await page.getByRole('button',{name:'Gem CSV',exact:true}).waitFor();
  const csvDownload=page.waitForEvent('download');await page.getByRole('button',{name:'Gem CSV',exact:true}).click();
  const csv=fs.readFileSync(await (await csvDownload).path(),'utf8');
  const manifestDownload=page.waitForEvent('download');await page.getByRole('button',{name:'Gem manifest',exact:true}).click();
  const manifest=JSON.parse(fs.readFileSync(await (await manifestDownload).path(),'utf8'));
  assert.equal(manifest.total_ddd,25198980);assert.equal(manifest.total_regionalt_tilskud_kr,80632838);
  assert.equal(require('node:crypto').createHash('sha256').update(csv).digest('hex'),manifest.csv_sha256);
  const checkpointDownload=page.waitForEvent('download');await page.getByRole('button',{name:'Gem checkpoint',exact:true}).click();
  const cp=fs.readFileSync(await (await checkpointDownload).path(),'utf8');
  assert(!cp.includes('SYNTHETIC'));assert(!JSON.stringify(manifest).includes('SYNTHETIC'));
  assert.equal(JSON.parse(cp).completed.length,1);assert.equal(JSON.parse(cp).pending.length,0);
  await page.waitForFunction(()=>!window.monitoreringCollector);
  console.log('PASS: Chromium panel, token observation, real-fixture DSR, manual file downloads/checksum, checkpoint and secret exclusion.');
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
