// Run against a local app with Test 8 published; no production access.
const {chromium}=require('playwright');const assert=require('node:assert/strict');
(async()=>{const b=await chromium.launch({headless:true,executablePath:process.env.CHROMIUM_PATH||undefined,args:['--no-sandbox','--disable-gpu','--disable-dev-shm-usage']});
 try{const p=await b.newPage({viewport:{width:1440,height:1100},acceptDownloads:true});const errors=[];p.on('pageerror',e=>errors.push(e.message));
  await p.goto(process.env.MONITOR_TEST_URL||'http://127.0.0.1:3838/');
  await p.getByText('Forespørgsel indlæst · population ikke verificeret',{exact:true}).waitFor();
  await p.getByText('Forbrug og tilskud',{exact:true}).click();
  await p.locator('#monthly').getByText('25.198.980,00',{exact:true}).waitFor();
  await p.locator('#monthly').getByText('80.632.838,00',{exact:true}).waitFor();
  await p.locator('#to').selectOption('2025-02');
  await p.locator('#monthly').getByText('Ingen indlæst afgrænsning',{exact:true}).waitFor();
  for(const tab of ['Takster og kobling','Fremskrivning og scenarie','Datastatus']){
   await p.getByText(tab,{exact:true}).click();await p.waitForTimeout(250);
  }
  await p.getByText('Fremskrivning og scenarie',{exact:true}).click();
  assert.match(await p.locator('#forecast').innerText(),/Fremskrivning|Utilstrækkeligt/);
  assert.equal(await p.locator('.shiny-output-error').count(),0);assert.deepEqual(errors,[]);
  const download=p.waitForEvent('download');await p.locator('#export').click();
  const file=await (await download).path();const csv=require('node:fs').readFileSync(file,'utf8');
  assert(csv.includes('regionalt_tilskud_kr'));assert(csv.includes('019307'));
  if(process.env.MONITOR_TEST_SCREENSHOT)await p.screenshot({path:process.env.MONITOR_TEST_SCREENSHOT,fullPage:true});
  console.log('PASS: real Shiny HTTP/browser startup, all tabs, Test 8 totals, missing-month state and CSV export.');
 }finally{await b.close();}
})().catch(e=>{console.error(e);process.exit(1)});
