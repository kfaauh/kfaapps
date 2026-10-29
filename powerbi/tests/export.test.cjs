// No network: test the exact generator template with synthetic responses with the observed Power BI schema.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const root = path.resolve(__dirname, '..');
const fixture = JSON.parse(fs.readFileSync(path.join(__dirname, 'fixtures/synthetic_response.json')));
const template = fs.readFileSync(path.join(root, 'export_template.js'), 'utf8');
const meta = {atc:'C09CA01', region:'Region Midtjylland', year:2025, opgoerelse:'Mængdeforbrug', normalisering:'Ingen valg'};
function render(level='atc5') {
  let source = template;
  for (const [key,value] of Object.entries({ATC_JS:['C09CA01'], METRICS_JS:['Mængdeforbrug'], REGIONS_JS:['Region Midtjylland'], YEARS_JS:[2025], SPLIT_JS:true, OUTPUT_LEVEL_JS:level})) {
    source=source.replaceAll(`__${key}__`,JSON.stringify(value));
  }
  return source;
}
async function api(level) {
  const context = {};
  await vm.runInNewContext(render(level).replace('// RUN_EXPORT','globalThis.api={decodeResponse,parseResponseToRows,buildPayload,rowsToCsv};return;'),context);
  return context.api;
}
async function runBatch(mode='success', options={}) {
  let calls=0, clicks=[], blobs=[], logs=[];
  const context={
    location:{origin:options.origin||'https://app.powerbi.com'}, window:{},
    Blob, AbortController,
    URL:{createObjectURL:b=>{blobs.push(b);return 'blob:'+blobs.length},revokeObjectURL:()=>{}},
    document:{body:{appendChild:()=>{}},createElement:()=>({click(){clicks.push(this.download)},remove(){}})},
    console:{log:(...x)=>logs.push(x),warn:(...x)=>logs.push(x),error:(...x)=>logs.push(x),table:x=>logs.push(x)},
    setTimeout:(fn,ms)=>{if(ms<1000)fn();return 1},clearTimeout:()=>{},
    fetch:async (url,req)=>{
      calls++;
      assert.equal(new URL(url).hostname,'wabi-west-europe-g-primary-redirect.analysis.windows.net');
      assert.equal(req.headers.authorization,'Bearer TEST_ONLY');
      assert.ok(!('x-powerbi-resourcekey' in req.headers));
      assert.equal(req.credentials,'omit');
      assert.equal(JSON.parse(req.body).modelId,1212197);
      if(mode==='network')throw new TypeError('Failed to fetch');
      if(mode==='http')return {ok:false,status:401};
      let data=structuredClone(fixture);
      if(mode==='api')data={error:{code:'InvalidQuery'}};
      if(mode==='empty')data.results[0].result.data.dsr.DS[0].PH[0].DM0=[];
      if(mode==='second-fails' && calls===2)return {ok:false,status:403};
      if(mode==='stop')context.window.medicinsalgExport.stop();
      return {ok:true,status:200,json:async()=>data};
    }
  };
  let source=render().replace('const authorization = "INDSÆT_AUTHORIZATION_HER"','const authorization = "Bearer TEST_ONLY"');
  if(options.placeholder)source=render();
  if(options.smallChunks)source=source.replace('const maxRowsPerCsv = 50000','const maxRowsPerCsv = 17');
  if(options.alreadyRunning)context.window.medicinsalgExport={stop(){}};
  await vm.runInNewContext(source,context);
  assert.ok(!JSON.stringify(logs).includes('Bearer TEST_ONLY'));
  assert.equal(context.window.medicinsalgExport,undefined);
  const summary=JSON.parse(await blobs.at(-1).text());
  assert.ok(!JSON.stringify(summary).includes('TEST_ONLY'));
  return {calls,clicks,blobs,summary};
}
(async()=>{
  const a=await api('atc5');
  const rows=a.decodeResponse(fixture);
  assert.equal(rows.length,60);
  assert.deepEqual(Array.from(rows[0]),['C09CA01 (Losartan)',2025,'Jan 2025','Almen læge','Region Midtjylland',1000]);
  assert.equal(rows.at(-1)[5],1590);
  assert.equal(rows.reduce((n,r)=>n+r[5],0),76670);
  assert.equal(new Set(rows.map(r=>r[2])).size,12);
  const compressed=fixture.results[0].result.data.dsr.DS[0].PH[0].DM0;
  compressed.forEach((r,i)=>assert.equal(rows[i][5],Number(String(i?r.C.at(-1):r.C[5]).replace(/D$/,''))));
  const mapped=a.parseResponseToRows(fixture,meta);
  assert.equal(mapped[0]['Måned'],'Jan 2025');
  assert.equal(mapped[0]['Ydertype'],'Almen læge');
  assert.equal(mapped[0].value,1000);
  const nullable=structuredClone(fixture), nr=nullable.results[0].result.data.dsr.DS[0].PH[0].DM0;
  nr[1].N=32;nr[1].C=[1];nr[2].C=[2,'0D'];
  const decoded=a.decodeResponse(nullable);
  assert.equal(decoded[1][5],null);assert.equal(decoded[2][5],0);
  assert.match(a.rowsToCsv(a.parseResponseToRows(nullable,meta)),/Ingen valg/);
  for(const kind of ['truncated','index','cells']){
    const bad=structuredClone(fixture), ds=bad.results[0].result.data.dsr.DS[0];
    if(kind==='truncated')ds.RT=['next'];
    if(kind==='index')ds.PH[0].DM0[0].C[0]=100;
    if(kind==='cells')ds.PH[0].DM0[1].C.push(99);
    assert.throws(()=>a.decodeResponse(bad));
  }
  const v=await api('varenummer');
  const product=structuredClone(fixture),data=product.results[0].result.data;
  data.descriptor.Select.splice(1,0,{Name:'DimLaegemiddel.Varenummer',Value:'G5'},{Name:'DimLaegemiddel.Laegemiddeltekst_samlet_unavne',Value:'G6'});
  const ds=data.dsr.DS[0],row=ds.PH[0].DM0[0];
  ds.PH[0].DM0=[row];
  row.S.splice(1,0,{N:'G5',T:1,DN:'D6'},{N:'G6',T:1,DN:'D7'});row.C.splice(1,0,0,0);
  ds.ValueDicts.D6=['012345'];ds.ValueDicts.D7=['012345 | Testprodukt | Tabletter | 50 mg | 100 stk'];
  const pr=v.parseResponseToRows(product,meta)[0];assert.equal(pr.Varenummer,'012345');assert.equal(pr['Navn (Præparat)'],'Testprodukt');assert.equal(pr.value,1000);
  ds.ValueDicts.D7=['bad'];assert.throws(()=>v.parseResponseToRows(product,meta),/produkttekst/);
  for(const level of ['atc5','varenummer']){
    const p=(await api(level)).buildPayload({atcValue:'C09CA01',metricValues:['Mængdeforbrug'],regionValues:['Region Midtjylland'],yearValues:[2025],normalisationValue:'Antal pr. 1.000'});
    assert.equal(p.modelId,1212197);
    const q=p.queries[0].Query.Commands[0].SemanticQueryDataShapeCommand;
    assert.equal(q.Query.Select.length,level==='atc5'?6:8);
    assert.ok(q.Query.From.some(x=>x.Entity==='Antal pr  1 000'));
  }
  const good=await runBatch('success',{smallChunks:true});assert.equal(good.calls,2);assert.equal(good.summary.complete,true);assert.equal(good.summary.files.reduce((n,x)=>n+x.rows,0),120);assert.equal(good.clicks.length,9);
  const empty=await runBatch('empty');assert.equal(empty.summary.complete,true);assert.equal(empty.summary.requests.filter(x=>x.parsedRows===0).length,2);assert.equal(empty.clicks.length,1);
  for(const mode of ['network','http','api','second-fails','stop']){
    const bad=await runBatch(mode);assert.equal(bad.summary.complete,false);assert.equal(bad.clicks.length,1);assert.equal(bad.summary.files.length,0);assert.equal(bad.calls,mode==='second-fails'?2:1);
  }
  await assert.rejects(()=>runBatch('success',{origin:'https://sundhedsdatabank.dk'}),/frame/);
  await assert.rejects(()=>runBatch('success',{placeholder:true}),/Authorization/);
  await assert.rejects(()=>runBatch('success',{alreadyRunning:true}),/allerede/);
  console.log('PASS: 60 synthetic rows, every DDD value, null/zero/reuse/dictionaries, product fixture, request generation, chunking, empty responses, failure/abort guards, no credentials in output.');
})().catch(e=>{console.error(e);process.exit(1)});
