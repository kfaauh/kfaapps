'use strict';
const fs=require('node:fs'),path=require('node:path');
const {makeExport}=require('../browser/monitorering.js');
(async()=>{
 const dir=process.argv[2];if(!dir)throw Error('Supply test output directory');fs.mkdirSync(dir,{recursive:true});
 const text=fs.readFileSync(path.join(__dirname,'fixtures/monitorering_forbrug_N06_2025-01.csv'),'utf8').replace(/^\uFEFF/,'').trim();
 const rows=text.split(/\r?\n/).slice(1).map(line=>{const [atc5,varenummer,aar_maaned,maaling,v]=line.split(';');return {atc5,varenummer,aar_maaned,maaling,vaerdi:Number(v)};});
 const out=await makeExport(rows,{prefix:'N06',period:'2025-01'},{duration_ms:11});
 fs.writeFileSync(path.join(dir,'monitorering_forbrug_N06_2025-01.csv'),out.csv);
 fs.writeFileSync(path.join(dir,'monitorering_manifest_N06_2025-01.json'),JSON.stringify(out.manifest,null,2));
})().catch(e=>{console.error(e.message);process.exitCode=1;});
