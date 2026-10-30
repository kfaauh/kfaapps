'use strict';
const {test}=require('node:test');
const assert=require('node:assert/strict');
const fs=require('node:fs');
const p=require('../browser/monitorering.js');
const task={prefix:'N06',period:'2025-01'};
const fixture=fs.readFileSync(__dirname+'/fixtures/monitorering_forbrug_N06_2025-01.csv','utf8').replace(/^\uFEFF/,'').trim().split(/\r?\n/).slice(1).map(s=>s.split(';'));
function response(rows=fixture) {
 const keys=['G7','G2','G5','G3','G0','M4'];
 return {results:[{result:{data:{descriptor:{Select:p.EXPECTED.map((Name,i)=>({Name,Value:keys[i]}))},dsr:{DS:[{ValueDicts:{D0:['N06BA04 - methylphenidat']},PH:[{DM0:rows.map((r,i)=>({...(i===0?{S:keys.map(N=>({N}))}:{}),C:[r[0]+' - text',r[1],2025,'Jan 2025',r[3]==='ddd'?'Mængdeforbrug':'Regionalt tilskud',+r[4]]}))}]}]}}}}]};
}
test('validated query retains two metrics, five regions, filters and grouping',()=>{
 const body=p.buildPayload(task),command=body.queries[0].Query.Commands[0].SemanticQueryDataShapeCommand;
 assert.equal(command.Query.Select.length,6);assert.equal(command.Query.Where.length,9);
 const s=JSON.stringify(body);for(const t of ['Flag_T3','Ingen valg','Volume_tekst_grp','Not','0D',...p.REGIONS]) assert(s.includes(t));
 for(const prefix of ['N','N06','N06B','N06BA','N06BA04']) assert(p.buildPayload({...task,prefix}));
});
test('real Test 8 records survive DSR decoding and reproduce observed totals',async()=>{
 const rows=p.recordsFromResponse(response(),task);
 const out=await p.makeExport(rows,task);
 assert.equal(out.manifest.row_count,1143);assert.equal(out.manifest.atc5_count,29);
 assert.equal(out.manifest.total_ddd,25198980);assert.equal(out.manifest.total_regionalt_tilskud_kr,80632838);
 assert.deepEqual(out.manifest.metric_row_counts,{ddd:576,regionalt_tilskud_kr:567});
 assert.equal(rows.filter(r=>r.atc5==='N06BA04'&&r.maaling==='ddd').reduce((s,r)=>s+r.vaerdi,0),3340703);
 assert(rows.some(r=>r.varenummer==='019307'));assert.match(out.manifest.csv_sha256,/^[0-9a-f]{64}$/);
});
test('dictionary, reuse and null masks decode independently',()=>{
 const r=response(fixture.slice(0,1)),ds=r.results[0].result.data.dsr.DS[0],g=ds.PH[0];
 g.DM0[0].S[0].DN='D0';g.DM0[0].C[0]=0;
 g.DM0.push({R:31,C:[12]});
 const values=p.decodeResponse(r);assert.equal(values[0][0],'N06BA04 - methylphenidat');assert.equal(values[1][0],values[0][0]);assert.equal(values[1][5],12);
 g.DM0.push({R:31,N:32,C:[]});assert.equal(p.decodeResponse(r)[2][5],null);
 g.DM0.push({R:1,N:1,C:[]});assert.throws(()=>p.decodeResponse(r),/Conflicting/);
});
test('strict failures for duplicate, null, zero, bad descriptor, restart, row limit and model errors',()=>{
 assert.throws(()=>p.recordsFromResponse(response([fixture[0],fixture[0]]),task),/Duplicate/);
 let r=response(fixture.slice(0,1));r.results[0].result.data.dsr.DS[0].PH[0].DM0[0].C[5]=null;
 assert.throws(()=>p.recordsFromResponse(r,task),/measure/);
 r=response(fixture.slice(0,1));r.results[0].result.data.dsr.DS[0].PH[0].DM0[0].C[5]=0;
 assert.throws(()=>p.recordsFromResponse(r,task),/measure/);
 r=response();r.results[0].result.data.descriptor.Select.reverse();assert.throws(()=>p.decodeResponse(r),/descriptor/);
 r=response();r.results[0].result.data.dsr.DS[0].RT=['opaque'];assert.throws(()=>p.decodeResponse(r),p.SplitRequired);
 assert.throws(()=>p.decodeResponse(response(fixture.slice(0,2)),2),p.SplitRequired);
 assert.throws(()=>p.decodeResponse({error:{code:'failure'}}),/error/);
});
test('plans disallow overlap, cross year safely and partition all next characters',()=>{
 assert.throws(()=>p.plan(['N06','N06BA'],'2025-01','2025-02'),/Overlapping/);
 assert.equal(p.plan(['N06'],'2024-12','2025-02').length,3);
 assert.deepEqual(p.children('N06').slice(0,2),['N06A','N06B']);assert.equal(p.children('N').length,100);assert.equal(p.children('N06BA').length,100);assert.equal(p.children('N06BA04').length,0);
});
test('sequential retries, bounded requests and no export after invalid response',async()=>{
 let calls=0,emitted=0,checkpoint;
 const result=await p.collect([task],{wait:async()=>{},checkpoint:s=>checkpoint=s,
  request:async()=>{if(++calls===1)throw Object.assign(Error('busy'),{status:429});return {body:response()};},
  emit:async()=>{emitted++;}});
 assert.equal(calls,2);assert.equal(emitted,1);assert.equal(result.pending.length,0);assert.equal(checkpoint.completed.length,1);
 emitted=0;await assert.rejects(p.collect([task],{wait:async()=>{},request:async()=>({body:{error:'bad'}}),emit:async()=>emitted++}),/error/);
 assert.equal(emitted,0);
 calls=0;await assert.rejects(p.collect([task],{wait:async()=>{},maxRequests:1,request:async()=>{calls++;throw Object.assign(Error('busy'),{status:503});},emit:async()=>{}}),/budget/);assert.equal(calls,1);
});
test('RestartToken abandons parent and retries disjoint children; stop keeps pending',async()=>{
 let state,calls=[];
 await assert.rejects(p.collect([task],{wait:async()=>{},maxRequests:2,checkpoint:s=>state=s,request:async t=>{
  calls.push(t.prefix);const r=response([]);if(t.prefix==='N06')r.results[0].result.data.dsr.DS[0].RT=['x'];return {body:r};
 },emit:async()=>{}}),/budget/);
 assert.deepEqual(calls,['N06','N06A']);assert(!state.completed.some(t=>t.prefix==='N06'));assert.equal(state.pending[0].prefix,'N06B');
 const c=new AbortController();c.abort();await assert.rejects(p.collect([task],{signal:c.signal,request:async()=>{},emit:async()=>{}}),/Stopped/);
});
test('401 never retries and never emits; an empty complete result remains explicit',async()=>{
 let calls=0;await assert.rejects(p.collect([task],{wait:async()=>{},request:async()=>{calls++;throw Object.assign(Error('expired'),{status:401});},emit:async()=>assert.fail()}),/expired/);assert.equal(calls,1);
 const out=await p.makeExport(p.recordsFromResponse(response([]),task),task);assert.equal(out.manifest.allow_empty,true);assert.equal(out.manifest.row_count,0);
});
