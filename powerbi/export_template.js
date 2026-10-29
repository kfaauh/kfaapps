
// Power BI Medicinsalg i primærsektoren - direkte CSV-export
// Kør i Chrome DevTools Console med app.powerbi.com valgt som frame.
// Output: CSV-filer og en oversigt over alle kald, inklusive tomme resultater.
// Stop en kørende eksport med medicinsalgExport.stop().

(async () => {
  // Token opsamles kun i denne browsersession fra rapportens næste datakald.
  let authorization = "";
  const url = "https://wabi-west-europe-g-primary-redirect.analysis.windows.net/explore/querydata";

  const selectedAtc = __ATC_JS__;
  const selectedMetrics = __METRICS_JS__;
  const selectedRegions = __REGIONS_JS__;
  const selectedYears = __YEARS_JS__;
  const splitByYear = __SPLIT_JS__;
  const outputLevel = __OUTPUT_LEVEL_JS__;
  const isVarenummerLevel = outputLevel === "varenummer";

  // Hold hver fil moderat i størrelse, så de kan uploades enkeltvis bagefter, hvis ønsket.
  const maxRowsPerCsv = 50000;

  const metricMap = {
    "Mængdeforbrug": "Antal DDD",
    "Regionalt tilskud": "Tilskudsbeløb - Regionalt",
    "Omsætning": "Ekspeditionsbeløb"
  };

  const normalisationOptions = [
    { value: "Ingen valg", suffix: "" },
    { value: "Antal pr. 1.000", suffix: " pr. 1.000 borgere" }
  ];

  function outputMetricName(opgoerelse, normalisering) {
    const base = metricMap[opgoerelse] || opgoerelse;
    const opt = normalisationOptions.find(x => x.value === normalisering);
    return base + (opt ? opt.suffix : "");
  }

  const headers = [
    "ATC-kode",
    "ATC-tekst",
    "Varenummer",
    "Navn (Præparat)",
    "Form",
    "Styrke",
    "Pakningsstørrelse",
    "År",
    "Måned",
    "Ydertype",
    "Bopælsregion",
    "nøgletal_output",
    "value",
    "atc_filter",
    "region_filter",
    "year_filter",
    "opgoerelse",
    "normalisering",
    "opgoerelsesniveau",
    "status",
    "rowCountMarker",
    "hasRestartToken",
    "export_error",
    "export_id"
  ];

  function literalValue(v) {
    if (typeof v === "number") return `${v}L`;
    const s = String(v).replaceAll("\\", "\\\\").replaceAll("'", "\\'");
    return `'${s}'`;
  }

  function inFilter(source, property, values) {
    return {
      Condition: {
        In: {
          Expressions: [{ Column: { Expression: { SourceRef: { Source: source } }, Property: property } }],
          Values: values.map(v => [{ Literal: { Value: literalValue(v) } }])
        }
      }
    };
  }

  function startsWithFilter(source, property, value) {
    return {
      Condition: {
        StartsWith: {
          Left: { Column: { Expression: { SourceRef: { Source: source } }, Property: property } },
          Right: { Literal: { Value: literalValue(value) } }
        }
      }
    };
  }

  function atcTextFilter(source, property, value) {
    const s = String(value || "").trim();
    // Fuld Power BI-værdi: eksakt match. Ren ATC-kode: prefix-match på ATC5_Kode_Tekst.
    if (s.includes("(") && s.includes(")")) return inFilter(source, property, [s]);
    return startsWithFilter(source, property, s.toUpperCase());
  }

  function buildPayload({ atcValue, metricValues, regionValues, yearValues, normalisationValue }) {
    const isPr1000 = normalisationValue === "Antal pr. 1.000";

    // Vigtigt: "Antal pr. 1.000" er en separat calculation-group/table i Power BI.
    // Hvis tabellen "Antal pr  1 000" medtages/filtreres ved totalværdier, kan de absolutte
    // nøgletal ende som 0. Derfor bruges source "a" kun ved pr. 1.000-kald.
    const fromItems = [
      { Name: "#", Entity: "# Measures", Type: 0 },
      { Name: "lm", Entity: "DimLaegemiddel", Type: 0 },
      { Name: "dato", Entity: "DimDato", Type: 0 },
      { Name: "ud", Entity: "DimUdstederType", Type: 0 },
      { Name: "opg", Entity: "Opgørelse", Type: 0 },
      { Name: "kron", Entity: "Kroniske sygdomme", Type: 0 },
      { Name: "geo", Entity: "DimBorgerDemografi", Type: 0 },
      { Name: "vol", Entity: "DimVolume", Type: 0 }
    ];

    const whereItems = [
      inFilter("vol", "Volume_tekst_grp", ["DDD"]),
      inFilter("opg", "CalculationItemColumn 1", metricValues),
      inFilter("dato", "År_ID", yearValues),
      inFilter("kron", "CalculationItemColumn 1", ["Ingen valg"]),
      inFilter("geo", "Bopælsregion", regionValues),
      atcTextFilter("lm", "ATC5_Kode_Tekst", atcValue),
      inFilter("lm", "Flag_T3", ["Ja"])
    ];

    if (isPr1000) {
      fromItems.push({ Name: "a", Entity: "Antal pr  1 000", Type: 0 });
      whereItems.splice(3, 0, inFilter("a", "CalculationItemColumn 1", ["Antal pr. 1.000"]));
    }

    return {
      version: "1.0.0",
      queries: [{
        Query: {
          Commands: [{
            SemanticQueryDataShapeCommand: {
              Query: {
                Version: 2,
                From: fromItems,
                Select: (() => {
                  const selectItems = [
                    { Column: { Expression: { SourceRef: { Source: "lm" } }, Property: "ATC5_Kode_Tekst" }, Name: "DimLaegemiddel.ATC5_Kode_Tekst" }
                  ];
                  if (isVarenummerLevel) {
                    selectItems.push(
                      { Column: { Expression: { SourceRef: { Source: "lm" } }, Property: "Varenummer" }, Name: "DimLaegemiddel.Varenummer", NativeReferenceName: "Varenummer" },
                      { Column: { Expression: { SourceRef: { Source: "lm" } }, Property: "Laegemiddeltekst_samlet_unavne" }, Name: "DimLaegemiddel.Laegemiddeltekst_samlet_unavne", NativeReferenceName: "Laegemiddeltekst_samlet_unavne" }
                    );
                  }
                  selectItems.push(
                    { Column: { Expression: { SourceRef: { Source: "dato" } }, Property: "År_ID" }, Name: "DimDato.År_ID" },
                    { Column: { Expression: { SourceRef: { Source: "dato" } }, Property: "MånedÅr" }, Name: "DimDato.MånedÅr" },
                    { Column: { Expression: { SourceRef: { Source: "ud" } }, Property: "Udsteder_type" }, Name: "DimUdstederType.Udsteder_type" },
                    { Column: { Expression: { SourceRef: { Source: "geo" } }, Property: "Bopælsregion" }, Name: "DimBorgerDemografi.Bopælsregion" },
                    { Measure: { Expression: { SourceRef: { Source: "#" } }, Property: "SelectMeasure1" }, Name: "# Measures.SelectMeasure1" }
                  );
                  return selectItems;
                })(),
                Where: whereItems,
                OrderBy: (() => {
                  const orderItems = [
                    { Direction: 1, Expression: { Column: { Expression: { SourceRef: { Source: "lm" } }, Property: "ATC5_Kode_Tekst" } } }
                  ];
                  if (isVarenummerLevel) {
                    orderItems.push(
                      { Direction: 1, Expression: { Column: { Expression: { SourceRef: { Source: "lm" } }, Property: "Varenummer" } } },
                      { Direction: 1, Expression: { Column: { Expression: { SourceRef: { Source: "lm" } }, Property: "Laegemiddeltekst_samlet_unavne" } } }
                    );
                  }
                  orderItems.push(
                    { Direction: 1, Expression: { Column: { Expression: { SourceRef: { Source: "dato" } }, Property: "År_ID" } } },
                    { Direction: 1, Expression: { Column: { Expression: { SourceRef: { Source: "dato" } }, Property: "MånedÅr" } } },
                    { Direction: 1, Expression: { Column: { Expression: { SourceRef: { Source: "ud" } }, Property: "Udsteder_type" } } },
                    { Direction: 1, Expression: { Column: { Expression: { SourceRef: { Source: "geo" } }, Property: "Bopælsregion" } } }
                  );
                  return orderItems;
                })()
              },
              Binding: {
                Primary: { Groupings: [{ Projections: isVarenummerLevel ? [0, 1, 2, 3, 4, 5, 6, 7] : [0, 1, 2, 3, 4, 5] }] },
                DataReduction: { DataVolume: 4, Primary: { Window: { Count: 30000 } } },
                Version: 1
              },
              ExecutionMetricsKind: 1
            }
          }]
        },
        QueryId: "",
        ApplicationContext: {
          DatasetId: "1bebf468-3f1e-4596-8d94-c43c4cce0759",
          Sources: [{ ReportId: "870dd64d-7c9e-4e21-9711-270d8d9acb54" }]
        }
      }],
      cancelQueries: [],
      modelId: 1212197,
      userPreferredLocale: "da-DK"
    };
  }

  function decodeResponse(root) {
    function check(x) {
      if(!x || typeof x!=="object")return;
      for(const [k,v] of Object.entries(x)){
        if(/^errors?$/i.test(k) && v!=null && !(Array.isArray(v)&&!v.length))throw Error("Power BI-fejl: "+JSON.stringify(v));
        if(k==="RT" && v!=null)throw Error("Svaret indeholder RestartToken. Udtrækket kan være afkortet.");
        check(v);
      }
    }
    check(root);
    if(!Array.isArray(root.results) || !root.results.length)throw Error("Ukendt svarformat.");
    const output=[];
    for(const result of root.results){
      const data=result.result?.data;
      if(!data?.descriptor?.Select || !Array.isArray(data.dsr?.DS))throw Error("Dataschema mangler.");
      const selects=data.descriptor.Select;
      const fields = ["DimLaegemiddel.ATC5_Kode_Tekst"];
      if (isVarenummerLevel) fields.push("DimLaegemiddel.Varenummer", "DimLaegemiddel.Laegemiddeltekst_samlet_unavne");
      fields.push("DimDato.År_ID", "DimDato.MånedÅr", "DimUdstederType.Udsteder_type", "DimBorgerDemografi.Bopælsregion", "# Measures.SelectMeasure1");
      const keys=fields.map(name=>{
        const key=selects.find(s=>s.Name===name)?.Value;
        if(!key)throw Error("Kolonne mangler: "+name);
        return key;
      });
      for(const ds of data.dsr.DS){
        if(!Array.isArray(ds.PH))throw Error("Ukendt grupperingsformat.");
        const dicts=ds.ValueDicts||{};
        for(const group of ds.PH){
          if(!Array.isArray(group.DM0))throw Error("Forventede flade DM0-rækker.");
          if(group.DM0.length>=30000)throw Error("Rækkegrænsen er nået; resultatet kan være afkortet.");
          let schema=null,previous=null;
          for(const row of group.DM0){
            if(row.S)schema=row.S;
            if(!schema || schema.length>31)throw Error("Ukendt rækkeskema.");
            const cells=row.C||[], values={};let j=0;
            for(let i=0;i<schema.length;i++){
              const s=schema[i], reuse=(row.R||0)&(1<<i), blank=(row.N||0)&(1<<i);
              if(reuse && blank)throw Error("Modstridende null-/gentagelsesmaske.");
              if(reuse){
                if(!previous || !Object.hasOwn(previous,s.N))throw Error("Manglende foregående værdi.");
                values[s.N]=previous[s.N];continue;
              }
              if(blank){values[s.N]=null;continue;}
              if(j>=cells.length)throw Error("For få celler i rækken.");
              let v=cells[j++];
              if(s.DN){
                const dict=dicts[s.DN];
                // Power BI kan sende tekst direkte, også med DN og en tom ordbog.
                // Kun numeriske celler i en ikke-tom ordbog er indeks.
                if (typeof v === "string" || v == null) {
                  // Direkte tekst, fx et varenummer med foranstillede nuller.
                } else if (Array.isArray(dict) && dict.length) {
                  if (!Number.isInteger(v) || v < 0 || v >= dict.length) throw Error("Ugyldigt ordbogsindeks: " + s.N);
                  v = dict[v];
                } else if (s.T === 1 && Array.isArray(dict) && !dict.length && Number.isFinite(v)) {
                  v = String(v);
                } else if (s.T === 1) {
                  throw Error("Tekstordbog mangler eller er ugyldig: " + s.N);
                }
              }
              values[s.N]=v;
            }
            if(j!==cells.length)throw Error("Ekstra celler i rækken.");
            previous=values;
            const out=keys.map(k=>{
              if(!Object.hasOwn(values,k))throw Error("Værdi mangler: "+k);
              return values[k];
            });
            if(out[out.length - 1]!=null){
              const raw=String(out[out.length - 1]).replace(/[DL]$/,"");
              if(!raw.trim() || !Number.isFinite(Number(raw)))throw Error("Ugyldig måleværdi: "+raw);
              out[out.length - 1]=Number(raw);
            }
            output.push(out);
          }
        }
      }
    }
    return output;
  }

  function splitAtc(value) {
    const match = String(value ?? "").trim().match(/^([A-Z][0-9]{2}[A-Z]{2}[0-9]{2})\s*(?:\((.+)\)|[-–—]\s*(.+))$/);
    if (!match) throw Error("Ukendt ATC-kode og tekst i svaret.");
    return [match[1], (match[2] || match[3]).trim()];
  }

  function parseResponseToRows(response, meta) {
    return decodeResponse(response).map(values => {
      const offset = isVarenummerLevel ? 2 : 0;
      let product = ["", "", "", "", ""];
      if (isVarenummerLevel) {
        const pieces = String(values[2] ?? "").split(/\s*\|\s*/);
        if (pieces.length < 5) throw Error("Ukendt produkttekst; ingen produktdata eksporteres.");
        product = [String(values[1] ?? "").padStart(6, "0"), pieces[1], pieces[2], pieces[3], pieces.slice(4).join(" | ")];
        if (!/^\d{6}$/.test(product[0]) || product.slice(1).some(x => !x.trim())) {
          throw Error("Ufuldstændige produktfelter; udtrækket er stoppet.");
        }
        if (/^\d+$/.test(pieces[0]) && pieces[0].padStart(6, "0") !== product[0]) {
          throw Error("Varenummer og produkttekst stemmer ikke overens.");
        }
      }
      return Object.fromEntries(headers.map((key, i) => [key, [
        ...splitAtc(values[0]),
        ...product, values[1 + offset], values[2 + offset], values[3 + offset], values[4 + offset],
        outputMetricName(meta.opgoerelse, meta.normalisering), values[5 + offset],
        meta.atc, meta.region, Array.isArray(meta.year) ? meta.year.join(", ") : meta.year,
        meta.opgoerelse, meta.normalisering, outputLevel, 200, 0, false, "", exportId
      ][i]]));
    });
  }

  function csvEscape(v) {
    if (v == null) return "";
    if (typeof v === "number") v = String(v).replace(".", ",");
    else v = String(v);
    if (/[";\r\n]/.test(v)) v = '"' + v.replaceAll('"', '""') + '"';
    return v;
  }

  function rowsToCsv(rows) {
    const lines = [headers.map(csvEscape).join(";")];
    for (const r of rows) lines.push(headers.map(h => csvEscape(r[h])).join(";"));
    return "\ufeff" + lines.join("\r\n");
  }


  // Blobber dannes løbende, men CSV-filer downloades først, når alle kald er lykkedes.
  function downloadBlob(filename, blob) {
    const link = document.createElement("a");
    const objectUrl = URL.createObjectURL(blob);
    link.href = objectUrl;
    link.download = filename;
    document.body.appendChild(link);
    link.click();
    link.remove();
    setTimeout(() => URL.revokeObjectURL(objectUrl), 60000);
  }

  async function runOne(meta, controller) {
    const yearValues = Array.isArray(meta.year) ? meta.year : [meta.year];
    const payload = buildPayload({ atcValue: meta.atc, metricValues: [meta.opgoerelse], regionValues: [meta.region], yearValues, normalisationValue: meta.normalisering });
    const timeout = setTimeout(() => controller.abort(), 60000);
    try {
      const response = await fetch(url, {
        method: "POST", mode: "cors", credentials: "omit",
        headers: { accept: "application/json", "content-type": "application/json;charset=UTF-8", authorization: authorization.trim() },
        body: JSON.stringify(payload), signal: controller.signal
      });
      if (!response.ok) throw Error(`HTTP ${response.status}. Ved 401/403: kopiér en ny Authorization-værdi fra rapporten.`);
      const rows = parseResponseToRows(await response.json(), meta);
      rows.forEach(row => { row.rowCountMarker = rows.length; });
      return { rows, meta: { ...meta, opgoerelsesniveau: outputLevel, status: 200, rowCountMarker: rows.length, hasRestartToken: false, parsedRows: rows.length, error: null } };
    } finally {
      clearTimeout(timeout);
    }
  }

  function captureAuthorization(signal) {
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

  const exportId = new Date().toISOString().replace(/[-:.]/g, "") + "_" + Math.random().toString(36).slice(2, 8);

  // Testene indlæser funktionerne ovenfor uden at køre netværkskald.
  // RUN_EXPORT
  if (location.origin !== "https://app.powerbi.com") throw Error("Vælg app.powerbi.com i Consoles frame-menu, og kør koden igen.");
  if (window.medicinsalgExport) throw Error("Et udtræk kører allerede. Stop det med medicinsalgExport.stop().");

  const total = selectedAtc.length * selectedRegions.length * selectedMetrics.length * normalisationOptions.length * (splitByYear ? selectedYears.length : 1);
  const summary = { version: 2, exportId, complete: false, expectedQueries: total, completedQueries: 0, requests: [], files: [] };
  const pendingFiles = [];
  let buffer = [], activeController = null, stopped = false, current = null;
  const authorizationController = new AbortController();
  const tokenCapture = captureAuthorization(authorizationController.signal);
  window.medicinsalgExport = {
    stop() { stopped = true; authorizationController.abort(); activeController?.abort(); },
    authorize(value) { return tokenCapture.provide(value); }
  };

  function stage(force) {
    while (buffer.length >= maxRowsPerCsv || (force && buffer.length)) {
      const rows = buffer.splice(0, maxRowsPerCsv);
      const filename = `medicinsalg_${exportId}_part_${String(pendingFiles.length + 1).padStart(3, "0")}.csv`;
      pendingFiles.push({ filename, blob: new Blob([rowsToCsv(rows)], { type: "text/csv;charset=utf-8" }) });
      summary.files.push({ filename, rows: rows.length });
    }
  }

  try {
    console.log("Skift nu et filter i Power BI-rapporten. Udtrækket starter, når rapportens adgangstoken er opsamlet.");
    console.log('Hvis det ikke lykkes: kopiér Authorization fra Network og kør medicinsalgExport.authorize("INDSÆT_HELE_HEADEREN") i Console.');
    authorization = await tokenCapture.promise;
    if (stopped) throw Error("Udtrækket blev stoppet af brugeren.");
    console.log("Adgangstoken opsamlet. Starter udtrækket.");
    const yearGroups = splitByYear ? selectedYears : [selectedYears];
    for (const atc of selectedAtc) for (const region of selectedRegions) for (const opgoerelse of selectedMetrics) for (const normalisering of normalisationOptions.map(x => x.value)) for (const year of yearGroups) {
      if (stopped) throw Error("Udtrækket blev stoppet af brugeren.");
      current = { atc, region, year, opgoerelse, normalisering };
      console.log(`[${summary.completedQueries + 1}/${total}] Henter`, current);
      activeController = new AbortController();
      const result = await runOne(current, activeController);
      summary.requests.push(result.meta);
      summary.completedQueries++;
      for (const row of result.rows) buffer.push(row);
      stage(false);
      if (!result.rows.length) console.warn("Ingen data for dette valg (ikke det samme som nul):", current);
      await new Promise(resolve => setTimeout(resolve, 200));
    }
    if (stopped) throw Error("Udtrækket blev stoppet af brugeren.");
    stage(true);
    summary.complete = true;
    for (const file of pendingFiles) {
      downloadBlob(file.filename, file.blob);
      await new Promise(resolve => setTimeout(resolve, 250));
    }
    const empty = summary.requests.filter(x => x.parsedRows === 0);
    console.log(`Færdig: ${total} kald, ${summary.files.reduce((n, f) => n + f.rows, 0)} rækker, ${empty.length} tomme deludtræk.`, summary);
    if (empty.length) console.table(empty);
  } catch (error) {
    summary.error = error.name === "AbortError" ? (stopped ? "Udtrækket blev stoppet." : "Tidsgrænsen på 60 sekunder blev overskredet.") : error.message;
    summary.failedQuery = current;
    // Fjern planlagte filer fra oversigten, da de aldrig blev downloadet.
    summary.files = [];
    console.error("STOP. Ingen CSV-filer er eksporteret. Ret fejlen og start igen:", summary.error, current);
  } finally {
    tokenCapture.cleanup();
    authorization = "";
    delete window.medicinsalgExport;
    downloadBlob(`medicinsalg_${exportId}_summary.json`, new Blob([JSON.stringify(summary, null, 2)], { type: "application/json;charset=utf-8" }));
  }
})();
