/* Monitorering compact exporter. No build or external dependency. */
(function(root) {
  "use strict";
  const REGIONS = ["Region Hovedstaden","Region Sjælland","Region Syddanmark","Region Midtjylland","Region Nordjylland"];
  const METRICS = ["Mængdeforbrug","Regionalt tilskud"];
  const EXPECTED = ["DimLaegemiddel.ATC5_Kode_Tekst","DimLaegemiddel.Varenummer","DimDato.År_ID","DimDato.MånedÅr","Opgørelse.CalculationItemColumn 1","# Measures.SelectMeasure1"];
  const DEFAULT = {
    api:"https://wabi-west-europe-g-primary-redirect.analysis.windows.net/explore/querydata",
    dataset:"1bebf468-3f1e-4596-8d94-c43c4cce0759",
    report:"870dd64d-7c9e-4e21-9711-270d8d9acb54", model:1212197,limit:30000
  };
  const MONTHS = ["Jan","Feb","Mar","Apr","Maj","Jun","Jul","Aug","Sep","Okt","Nov","Dec"];
  const prefixOK = p => typeof p==="string" && /^[A-Z]([0-9]{2}([A-Z]([A-Z]([0-9]{2})?)?)?)?$/.test(p);
  const periodOK = p => typeof p==="string" && /^\d{4}-(0[1-9]|1[0-2])$/.test(p);
  function config(task, options={}) {
    if(!prefixOK(task.prefix)||!periodOK(task.period)) throw Error("Invalid ATC prefix or period");
    const year=Number(task.period.slice(0,4));
    const labels=options.monthLabels||MONTHS;
    if(labels.length!==12||labels.some(s=>typeof s!=="string"||!s)) throw Error("Expected twelve month labels");
    return {...DEFAULT,...options,...task,year,month:`${labels[Number(task.period.slice(5))-1]} ${year}`};
  }
  function buildPayload(task,options={}) {
    const CFG=config(task,options);
  function literal(v) {
    return typeof v === "number" ? `${v}L` :
      "'" + String(v).replaceAll("\\", "\\\\")
        .replaceAll("'", "\\'") + "'";
  }

  function field(src, prop) {
    return {
      Column: {
        Expression: { SourceRef: { Source: src } },
        Property: prop
      }
    };
  }

  function selectCol(src, prop, name) {
    return { ...field(src, prop), Name: name };
  }

  function measure() {
    return {
      Measure: {
        Expression: { SourceRef: { Source: "#" } },
        Property: "SelectMeasure1"
      }
    };
  }

  function filterIn(src, prop, vals) {
    return {
      Condition: {
        In: {
          Expressions: [field(src, prop)],
          Values: vals.map(v => [
            { Literal: { Value: literal(v) } }
          ])
        }
      }
    };
  }

  function prefixFilter() {
    return {
      Condition: {
        StartsWith: {
          Left: field("lm", "ATC5_Kode_Tekst"),
          Right: { Literal: { Value: literal(CFG.prefix) } }
        }
      }
    };
  }

  function nonzeroFilter() {
    return {
      Condition: {
        Not: {
          Expression: {
            In: {
              Expressions: [measure()],
              Values: [[{ Literal: { Value: "0D" } }]]
            }
          }
        }
      }
    };
  }

  function payload() {
    const selections = [
      selectCol("lm", "ATC5_Kode_Tekst",
        "DimLaegemiddel.ATC5_Kode_Tekst"),
      selectCol("lm", "Varenummer",
        "DimLaegemiddel.Varenummer"),
      selectCol("dato", "År_ID", "DimDato.År_ID"),
      selectCol("dato", "MånedÅr", "DimDato.MånedÅr"),
      selectCol("opg", "CalculationItemColumn 1",
        "Opgørelse.CalculationItemColumn 1"),
      {
        ...measure(),
        Name: "# Measures.SelectMeasure1"
      }
    ];

    const order = [
      ["lm", "ATC5_Kode_Tekst"],
      ["lm", "Varenummer"],
      ["dato", "MånedÅr"],
      ["opg", "CalculationItemColumn 1"]
    ].map(([src, prop]) => ({
      Direction: 1,
      Expression: field(src, prop)
    }));

    return {
      version: "1.0.0",
      queries: [{
        Query: {
          Commands: [{
            SemanticQueryDataShapeCommand: {
              Query: {
                Version: 2,
                From: [
                  { Name: "#", Entity: "# Measures", Type: 0 },
                  { Name: "lm", Entity: "DimLaegemiddel", Type: 0 },
                  { Name: "dato", Entity: "DimDato", Type: 0 },
                  { Name: "opg", Entity: "Opgørelse", Type: 0 },
                  { Name: "kron", Entity: "Kroniske sygdomme", Type: 0 },
                  { Name: "geo", Entity: "DimBorgerDemografi", Type: 0 },
                  { Name: "vol", Entity: "DimVolume", Type: 0 }
                ],
                Select: selections,
                Where: [
                  filterIn("vol", "Volume_tekst_grp", ["DDD"]),
                  filterIn("opg", "CalculationItemColumn 1", METRICS),
                  filterIn("dato", "År_ID", [CFG.year]),
                  filterIn("dato", "MånedÅr", [CFG.month]),
                  filterIn("kron", "CalculationItemColumn 1",
                    ["Ingen valg"]),
                  filterIn("geo", "Bopælsregion", REGIONS),
                  prefixFilter(),
                  filterIn("lm", "Flag_T3", ["Ja"]),
                  nonzeroFilter()
                ],
                OrderBy: order
              },
              Binding: {
                Primary: {
                  Groupings: [{
                    Projections: [0, 1, 2, 3, 4, 5]
                  }]
                },
                DataReduction: {
                  DataVolume: 4,
                  Primary: {
                    Window: { Count: CFG.limit }
                  }
                },
                Version: 1
              },
              ExecutionMetricsKind: 1
            }
          }]
        },
        QueryId: "",
        ApplicationContext: {
          DatasetId: CFG.dataset,
          Sources: [{ ReportId: CFG.report }]
        }
      }],
      cancelQueries: [],
      modelId: CFG.model,
      userPreferredLocale: "da-DK"
    };
  }

    return payload();
  }
  function captureAuthorization(signal, url) {
    let resolveToken, rejectToken, settled = false, timer;
    const promise = new Promise((resolve, reject) => { resolveToken = resolve; rejectToken = reject; });
    const originalFetch = window.fetch;
    const proto = window.XMLHttpRequest?.prototype;
    const originalOpen = proto?.open, originalHeader = proto?.setRequestHeader;
    const requestUrls = new WeakMap();
    function matches(value) {
      try {
        const target = new URL(String(value), location.href), expected = new URL(url);
        return target.origin === expected.origin && target.pathname === expected.pathname;
      } catch { return false; }
    }
    function cleanup() {
      clearTimeout(timer);
      signal.removeEventListener("abort", abort);
      if (window.fetch === observedFetch) window.fetch = originalFetch;
      if (proto?.open === observedOpen) proto.open = originalOpen;
      if (proto?.setRequestHeader === observedHeader) proto.setRequestHeader = originalHeader;
    }
    function provide(value) {
      if (settled || typeof value !== "string" || !/^(EmbedToken|Bearer)\s+\S+$/i.test(value.trim())) return false;
      settled = true;
      cleanup();
      resolveToken(value.trim());
      return true;
    }
    function abort() {
      if (settled) return;
      settled = true; cleanup(); rejectToken(new Error("Udtrækket blev stoppet af brugeren."));
    }
    function observedFetch(input, init) {
      // Token observeres; rapportens forespørgsel videresendes helt uændret.
      try {
        if (matches(typeof input === "string" || input instanceof URL ? input : input.url)) {
          const headers = new Headers(init?.headers ?? input?.headers);
          provide(headers.get("authorization"));
        }
      } catch { /* Rapportens eget kald skal altid fortsætte. */ }
      return originalFetch.apply(this, arguments);
    }
    function observedOpen(method, target) {
      const result = originalOpen.apply(this, arguments);
      requestUrls.set(this, target);
      return result;
    }
    function observedHeader(name, value) {
      const result = originalHeader.apply(this, arguments);
      if (String(name).toLowerCase() === "authorization" && matches(requestUrls.get(this))) provide(value);
      return result;
    }
    window.fetch = observedFetch;
    if (proto) { proto.open = observedOpen; proto.setRequestHeader = observedHeader; }
    signal.addEventListener("abort", abort, { once: true });
    timer = setTimeout(() => {
      if (settled) return;
      settled = true; cleanup();
      rejectToken(new Error("Intet adgangstoken opsamlet inden 2 minutter. Kør koden igen og skift et filter, eller brug den manuelle authorize-kommando."));
    }, 120000);
    if (signal.aborted) abort();
    return { promise, provide, cleanup };
  }

  class SplitRequired extends Error {}
  function inspectResponse(x) {
    let continuation=false; const errors=[];
    function walk(obj) {
      if(!obj||typeof obj!=="object") return;
      for(const [k,v] of Object.entries(obj)) {
        if(/^(RT|RestartToken|ContinuationToken)$/i.test(k) && v!=null && v!=="" && v!==false) continuation=true;
        if(/^(errors?|exception|errorCode)$/i.test(k) && v!=null && v!=="" && !(Array.isArray(v)&&!v.length)) errors.push(k);
        walk(v);
      }
    }
    walk(x); return {continuation,errors};
  }
  function decodeResponse(response, limit=30000) {
    const state=inspectResponse(response);
    if(state.errors.length) throw Error("PowerBI error response: "+state.errors.join(","));
    if(state.continuation) throw new SplitRequired("Continuation requires disjoint subdivision");
    if(response.results?.length!==1) throw Error("Expected exactly one query result");
    const data=response.results[0]?.result?.data;
    const selects=data?.descriptor?.Select;
    if(!Array.isArray(selects)||JSON.stringify(selects.filter(Boolean).map(x=>x.Name))!==JSON.stringify(EXPECTED))
      throw Error("Unexpected query descriptor");
    const keys=selects.map(s=>s.Value);
    if(keys.some(k=>typeof k!=="string")||new Set(keys).size!==6) throw Error("Missing descriptor value keys");
    if(!Array.isArray(data?.dsr?.DS)||!data.dsr.DS.length) throw Error("Missing DSR dataset");
    const output=[];
    for(const ds of data.dsr.DS) {
      if(!Array.isArray(ds.PH)) throw Error("Missing DSR partitions");
      const dicts=ds.ValueDicts||{};
      for(const ph of ds.PH) {
        if(!Array.isArray(ph.DM0)) throw Error("Unknown DSR grouping");
        if(ph.DM0.length>=limit) throw new SplitRequired("Row limit reached");
        let schema=null,previous=null;
        for(const row of ph.DM0) {
          if(row.S) schema=row.S;
          if(!Array.isArray(schema)||schema.length>31||schema.length===0) throw Error("Unknown DSR schema");
          for(const mask of [row.R??0,row.N??0]) if(!Number.isInteger(mask)||mask<0||mask>=2**schema.length) throw Error("Invalid DSR mask bits");
          const cells=row.C||[], values={}; let j=0;
          for(let i=0;i<schema.length;i++) {
            const f=schema[i],bit=2**i,reuse=(row.R||0)&bit,blank=(row.N||0)&bit;
            if(reuse&&blank) throw Error("Conflicting DSR masks");
            if(reuse) {
              if(!previous||!Object.hasOwn(previous,f.N)) throw Error("Invalid repeat mask");
              values[f.N]=previous[f.N];
            } else if(blank) values[f.N]=null;
            else {
              if(j>=cells.length) throw Error("Missing DSR cells");
              let v=cells[j++];
              if(f.DN && typeof v!=="string" && v!=null) {
                const dict=dicts[f.DN];
                if(Array.isArray(dict)&&dict.length) {
                  if(!Number.isInteger(v)||v<0||v>=dict.length) throw Error("Invalid dictionary index");
                  v=dict[v];
                } else if(f.T===1 && Array.isArray(dict)&&dict.length===0 && Number.isFinite(v)) v=String(v);
                else if(f.T===1) throw Error("Missing text dictionary");
              }
              values[f.N]=v;
            }
          }
          if(j!==cells.length) throw Error("Extra DSR cells");
          previous=values;
          output.push(keys.map(k=>{
            if(!Object.hasOwn(values,k)) throw Error("Descriptor/schema mismatch");
            return values[k];
          }));
        }
      }
    }
    if(output.length>=limit) throw new SplitRequired("Combined row limit reached");
    return output;
  }
  function recordsFromResponse(response,task,options={}) {
    const cfg=config(task,options), seen=new Set();
    return decodeResponse(response,cfg.limit).map(r=>{
      const atc5=String(r[0]??"").slice(0,7);
      const rawProduct=String(r[1]??"");
      if(!/^\d{1,6}$/.test(rawProduct)) throw Error("Invalid product number");
      const varenummer=rawProduct.padStart(6,"0");
      const rawValue=r[5]==null?"":String(r[5]).replace(/[DL]$/,"");
      const value=Number(rawValue);
      if(!/^[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}$/.test(atc5)||!atc5.startsWith(cfg.prefix)) throw Error("ATC outside scope");
      if(Number(r[2])!==cfg.year||r[3]!==cfg.month) throw Error("Month outside scope");
      if(!METRICS.includes(r[4])) throw Error("Unexpected metric");
      if(!rawValue.trim()||!Number.isFinite(value)||value===0||Math.abs(value)>=1e20) throw Error("Invalid nonzero measure");
      const maaling=r[4]===METRICS[0]?"ddd":"regionalt_tilskud_kr";
      const key=[atc5,varenummer,task.period,maaling].join("|");
      if(seen.has(key)) throw Error("Duplicate metric key"); seen.add(key);
      return {atc5,varenummer,aar_maaned:task.period,maaling,vaerdi:value};
    });
  }
  function rowsToCsv(rows) {
    const keys=["atc5","varenummer","aar_maaned","maaling","vaerdi"];
    const number = v => {
      const text=Number(v).toLocaleString("en-US",{useGrouping:false,maximumFractionDigits:8});
      if(Number(text)!==v || (Number.isInteger(v)&&!Number.isSafeInteger(v))) throw Error("Measure exceeds lossless CSV precision");
      return text;
    };
    return "\uFEFF"+[keys.join(";"),...rows.map(r=>keys.map(k=>k==="vaerdi"?number(r[k]):r[k]).join(";"))].join("\r\n");
  }
  async function hashText(s) {
    const bytes=new TextEncoder().encode(s);
    return [...new Uint8Array(await crypto.subtle.digest("SHA-256",bytes))].map(x=>x.toString(16).padStart(2,"0")).join("");
  }
  async function makeExport(rows,task,performanceMeta={}) {
    const csv=rowsToCsv(rows),counts={ddd:0,regionalt_tilskud_kr:0},totals={ddd:0,regionalt_tilskud_kr:0};
    for(const r of rows) {counts[r.maaling]++; totals[r.maaling]+=r.vaerdi;}
    const manifest={schema_version:2,source:"Sundhedsdatabanken PowerBI Medicinsalg",extracted_at:new Date().toISOString(),
      atc_prefix:task.prefix,period:task.period,region_filter:REGIONS,metrics:Object.keys(counts),zero_filter:true,
      query_contract:"compact-national-v1",complete_query_response:true,population_coverage:"unverified",
      omitted_zero_or_blank_values:"Fravær er ikke automatisk dokumenteret nul",has_restart_token:false,
      row_count:rows.length,atc5_count:new Set(rows.map(r=>r.atc5)).size,metric_row_counts:counts,
      total_ddd:totals.ddd,total_regionalt_tilskud_kr:totals.regionalt_tilskud_kr,
      http:200,duration_ms:performanceMeta.duration_ms??0,response_bytes:performanceMeta.response_bytes??0,
      csv_sha256:await hashText(csv),allow_empty:rows.length===0,
      query_sha256:performanceMeta.query_sha256??await hashText(JSON.stringify(buildPayload(task))),
      query_model:performanceMeta.query_model??{model:DEFAULT.model,dataset:DEFAULT.dataset,report:DEFAULT.report}};
    return {csv,manifest};
  }
  function children(prefix) {
    if(!prefixOK(prefix)||prefix.length===7) return [];
    const suffix=prefix.length===1||prefix.length===5
      ? Array.from({length:100},(_,i)=>String(i).padStart(2,"0"))
      : Array.from({length:26},(_,i)=>String.fromCharCode(65+i));
    return suffix.map(s=>prefix+s);
  }
  function plan(prefixes,from,to) {
    if(!Array.isArray(prefixes)||!prefixes.length||prefixes.some(p=>!prefixOK(p))||!periodOK(from)||!periodOK(to)||from>to)
      throw Error("Invalid collection plan");
    if(prefixes.some((p,i)=>prefixes.some((q,j)=>i!==j&&(p.startsWith(q)||q.startsWith(p))))) throw Error("Overlapping ATC prefixes");
    const tasks=[]; let cursor=from;
    while(cursor<=to) {
      for(const prefix of prefixes) tasks.push({prefix,period:cursor});
      const [y,m]=cursor.split("-").map(Number);
      cursor=m===12?`${y+1}-01`:`${y}-${String(m+1).padStart(2,"0")}`;
      if(tasks.length>5000) throw Error("Plan too large; collect smaller batches");
    }
    return tasks;
  }
  function download(name,content,type) {
    const blob=new Blob([content],{type}), url=URL.createObjectURL(blob),a=document.createElement("a");
    a.href=url;a.download=name;document.body.appendChild(a);a.click();a.remove();setTimeout(()=>URL.revokeObjectURL(url),30000);
  }
  const taskKey=t=>`${t.prefix}_${t.period}`;
  // Scheduler is transport-independent and testable. Checkpoint contains no tokens or response bodies.
  async function collect(tasks,options) {
    const {request,emit,checkpoint=()=>{},signal}=options;
    const wait=options.wait||((ms)=>new Promise((resolve,reject)=>{
      const done=()=>{signal?.removeEventListener("abort",abort);resolve();};
      const timer=setTimeout(done,ms);
      const abort=()=>{clearTimeout(timer);reject(Error("Stopped"));};
      signal?.addEventListener("abort",abort,{once:true}); if(signal?.aborted) abort();
    }));
    const state={version:1,query_contract:"compact-national-v1",pending:structuredClone(tasks),completed:[],failures:[],requests:0};
    const interval=Math.max(2000,options.pauseMs??2500), retries=Math.min(3,options.retries??2), budget=options.maxRequests??60;
    if(tasks.length>5000) throw Error("Plan too large");
    while(state.pending.length) {
      if(signal?.aborted) throw Error("Stopped");
      const task=state.pending[0]; let success=false;
      for(let attempt=0;attempt<=retries;attempt++) {
        if(state.requests>=budget) {checkpoint(structuredClone(state));throw Error("Request budget exhausted; resume from checkpoint");}
        state.requests++;checkpoint(structuredClone(state));
        try {
          await wait(interval);
          const result=await request(task,signal);
          const rows=recordsFromResponse(result.body,task,options);
          await emit(await makeExport(rows,task,result),task);
          state.completed.push(task);state.pending.shift();success=true;
          checkpoint(structuredClone(state));break;
        } catch(error) {
          if(error instanceof SplitRequired) {
            const parts=children(task.prefix);
            if(!parts.length) {checkpoint(structuredClone(state));throw Error("ATC5/month truncated; no validated finer partition");}
            state.pending.splice(0,1,...parts.map(prefix=>({...task,prefix})));
            state.failures.push({task,reason:"subdivided",attempt});success=true;
            checkpoint(structuredClone(state));break;
          }
          const status=error.status;
          // Only transient transport failures can retry. Schema/data/auth failures stop immediately.
          const transient=error.retryable===true || status===429 || (status>=500&&status<=599);
          state.failures.push({task,attempt,status:status??null,reason:status?`HTTP ${status}`:"query_or_transport_failure"});
          checkpoint(structuredClone(state));
          if(signal?.aborted||!transient||attempt===retries) throw error;
          if(error.retryAfterMs>60000) throw Error("Retry-After exceeds one minute; resume later from checkpoint");
          await wait(Math.max(interval*2**(attempt+1),error.retryAfterMs||0));
        }
      }
      if(!success) throw Error("Incomplete task");
    }
    return state;
  }
  async function start(options={}) {
    if(location.origin!=="https://app.powerbi.com") throw Error("Select app.powerbi.com console frame");
    if(root.monitoreringCollector) throw Error("Collector already active");
    const resumed=options.resume;
    if(resumed && (resumed.version!==1||resumed.query_contract!=="compact-national-v1"||!Array.isArray(resumed.pending)))
      throw Error("Invalid checkpoint");
    const tasks=resumed?resumed.pending:plan(options.prefixes||["N06"],options.from||"2025-01",options.to||options.from||"2025-01");
    // Revalidate resume scopes and dates; exclude overlap within each month.
    const byMonth=new Map();
    for(const t of tasks) {config(t,options);byMonth.set(t.period,[...(byMonth.get(t.period)||[]),t.prefix]);}
    for(const [p,prefixes] of byMonth) plan(prefixes,p,p);
    const cfg=config(tasks[0]||{prefix:"N06",period:"2025-01"},options);
    const endpoint=new URL(cfg.api);
    if(endpoint.protocol!=="https:"||!endpoint.hostname.endsWith(".analysis.windows.net")||endpoint.pathname!=="/explore/querydata")
      throw Error("Unexpected PowerBI endpoint");
    const controller=new AbortController(),capture=captureAuthorization(controller.signal,cfg.api);
    let authorization="", latest={version:1,query_contract:"compact-national-v1",pending:tasks,completed:[],failures:[],requests:0};
    const id=new Date().toISOString().replace(/[^0-9]/g,"");
    const save=()=>download(`monitorering_checkpoint_${id}.json`,JSON.stringify(latest,null,2),"application/json");
    root.monitoreringCollector={stop(){controller.abort();},authorize:capture.provide,checkpoint:save};
    console.log("Skift et rapportfilter for tokenopsamling. Manuel fallback: monitoreringCollector.authorize(header). Stop: monitoreringCollector.stop().");
    try {
      authorization=await capture.promise;
      return await collect(tasks,{...options,signal:controller.signal,
        checkpoint(s){latest=s;},
        async request(task,signal) {
          const local=new AbortController();const abort=()=>local.abort();signal.addEventListener("abort",abort,{once:true});
          const timer=setTimeout(abort,60000),started=performance.now();
          try {
            const query=JSON.stringify(buildPayload(task,options));
            const r=await fetch(cfg.api,{method:"POST",credentials:"omit",mode:"cors",headers:{accept:"application/json","content-type":"application/json;charset=UTF-8",authorization},
              body:query,signal:local.signal});
            if(!r.ok) {
              const error=Object.assign(Error(`HTTP ${r.status}`),{status:r.status});
              const retry=r.headers.get("retry-after");
              if(retry) error.retryAfterMs=/^\d+$/.test(retry)?Number(retry)*1000:Math.max(0,Date.parse(retry)-Date.now());
              throw error;
            }
            const text=await r.text();
            return {body:JSON.parse(text),query_sha256:await hashText(query),query_model:{model:cfg.model,dataset:cfg.dataset,report:cfg.report},duration_ms:Math.round(performance.now()-started),response_bytes:new TextEncoder().encode(text).length};
          } catch(e) {
            if(e instanceof TypeError||e.name==="AbortError"&&!signal.aborted) e.retryable=true;
            throw e;
          } finally {clearTimeout(timer);signal.removeEventListener("abort",abort);}
        },
        async emit(result,task) {
          const key=taskKey(task);
          download(`monitorering_forbrug_${key}.csv`,result.csv,"text/csv;charset=utf-8");
          download(`monitorering_manifest_${key}.json`,JSON.stringify(result.manifest,null,2),"application/json");
          console.log("Validated download offered:",key,result.manifest.row_count);
        }
      });
    } finally {
      capture.cleanup();authorization="";save();delete root.monitoreringCollector;
      console.log("Checkpoint download offered. Verify that every CSV/manifest pair reached disk before resuming.");
    }
  }
  const api={buildPayload,inspectResponse,decodeResponse,recordsFromResponse,rowsToCsv,makeExport,children,plan,collect,start,SplitRequired,REGIONS,EXPECTED,config};
  if(typeof module!=="undefined"&&module.exports) module.exports=api;
  else {root.Monitorering=api;console.log('Monitorering loaded. Start a small batch with Monitorering.start({prefixes:["N06"],from:"2025-01",to:"2025-01"}).');}
})(typeof window!=="undefined"?window:globalThis);
