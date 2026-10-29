# Medicinsalg – enkeltfil til lokal brug og Shiny Server (R >= 4.0)
# Start lokalt fra RStudios Console: shiny::runApp(file.choose(), launch.browser = TRUE)
# Vælg denne R-fil i dialogen. Stop med Esc.
# Ved installation på Shiny Server: navngiv filen app.R i den eksisterende appmappe.
# Ingen ekstra projektfiler kræves. Pakkerne nedenfor skal være installeret.

# app.R
# Shiny-app: Power BI Medicinsalg – CSV-eksport og tabel/Excel
#
# Formål:
# 1) Generér JavaScript-kode, som kan køres i Chrome DevTools Console på den åbne Power BI-rapport.
# 2) Browserkoden bruger Authorization lokalt, afkoder Power BI-svar og downloader CSV.
# 3) Appen importerer CSV til en bred tabel/Excel; rå Power BI-JSON parses ikke i R.
#
# Pakker:
# install.packages(c(
#   "shiny", "bslib", "jsonlite", "dplyr", "tidyr", "stringr",
#   "purrr", "readr", "DT", "writexl"
# ))

library(shiny)
library(bslib)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(DT)
library(writexl)
library(readxl)

# Tillad flere CSV-dele og den tilhørende eksportoversigt.
options(shiny.maxRequestSize = 200 * 1024^2)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

normalise_atc_input <- function(x) {
  if (is.null(x) || !nzchar(x)) return(character(0))
  tokens <- unique(str_trim(unlist(str_split(x, "[\\r\\n,;]+"), use.names = FALSE)))
  tokens <- tokens[tokens != ""]
  valid <- str_detect(str_to_upper(tokens), "^[A-Z][0-9]{2}[A-Z]{1,2}[0-9]{0,4}(\\s*\\(.+\\))?$")
  if (any(!valid)) stop("Ugyldig ATC-værdi: ", paste(tokens[!valid], collapse = ", "))
  vapply(tokens, function(token) {
    code <- str_extract(token, "^[A-Za-z][0-9]{2}[A-Za-z]{1,2}[0-9]{0,4}")
    paste0(toupper(code), substring(token, nchar(code) + 1L))
  }, character(1), USE.NAMES = FALSE)
}

clean_atc_code_for_mapping <- function(x) {
  x |>
    str_trim() |>
    str_extract("^[A-Za-z][0-9]{2}[A-Za-z]{1,2}[0-9]{0,4}") |>
    toupper()
}

read_mapping_file <- function(path, filename = path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  
  ext <- tolower(tools::file_ext(filename))
  
  dat <- switch(
    ext,
    "csv" = readr::read_csv(path, show_col_types = FALSE, locale = locale(encoding = "UTF-8")),
    "txt" = readr::read_delim(path, delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8")),
    "tsv" = readr::read_tsv(path, show_col_types = FALSE, locale = locale(encoding = "UTF-8")),
    stop("Mapping-filen skal være csv, txt eller tsv.")
  )
  
  nms <- names(dat)
  nms_low <- tolower(nms)
  
  atc_col <- nms[match(TRUE, nms_low %in% c("atc", "atc_kode", "atckode", "atc_code"))]
  pbi_col <- nms[match(TRUE, nms_low %in% c("atc5_kode_tekst", "powerbi_atc", "powerbi", "atc5", "atc5tekst"))]
  
  if (is.na(atc_col) || is.na(pbi_col)) {
    stop("Mapping-filen skal indeholde kolonnerne ATC og ATC5_Kode_Tekst.")
  }
  
  dat |>
    transmute(
      ATC = toupper(str_trim(as.character(.data[[atc_col]]))),
      ATC5_Kode_Tekst = str_trim(as.character(.data[[pbi_col]]))
    ) |>
    filter(!is.na(ATC), ATC != "", !is.na(ATC5_Kode_Tekst), ATC5_Kode_Tekst != "") |>
    distinct()
}

apply_atc_mapping <- function(atc_values, mapping = NULL) {
  if (length(atc_values) == 0) return(character(0))
  
  already_powerbi <- str_detect(atc_values, "\\(.+\\)")
  raw_codes <- clean_atc_code_for_mapping(atc_values)
  
  if (is.null(mapping) || nrow(mapping) == 0) {
    return(unique(ifelse(already_powerbi, atc_values, raw_codes)))
  }
  
  mapped <- tibble(
    input = atc_values,
    ATC = raw_codes,
    already_powerbi = already_powerbi
  ) |>
    left_join(mapping, by = "ATC") |>
    mutate(
      powerbi_atc = case_when(
        already_powerbi ~ input,
        !is.na(ATC5_Kode_Tekst) & ATC5_Kode_Tekst != "" ~ ATC5_Kode_Tekst,
        TRUE ~ ATC
      )
    ) |>
    pull(powerbi_atc)
  
  unique(mapped[!is.na(mapped) & mapped != ""])
}

js_string_array <- function(x) {
  jsonlite::toJSON(as.character(x), auto_unbox = FALSE)
}

js_numeric_array <- function(x) {
  paste0("[", paste(as.integer(x), collapse = ", "), "]")
}

.medicinsalg_js_template <- r"---(
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
})();)---"

generate_js <- function(atc_powerbi, metrics, regions, years, split_by_year = TRUE, output_level = "atc5") {
  output_level <- match.arg(output_level, c("atc5", "varenummer"))
  template <- .medicinsalg_js_template
  replacements <- list(
    "__ATC_JS__" = js_string_array(atc_powerbi),
    "__METRICS_JS__" = js_string_array(metrics),
    "__REGIONS_JS__" = js_string_array(regions),
    "__YEARS_JS__" = js_numeric_array(years),
    "__SPLIT_JS__" = if (isTRUE(split_by_year)) "true" else "false",
    "__OUTPUT_LEVEL_JS__" = jsonlite::toJSON(output_level, auto_unbox = TRUE)
  )
  for (key in names(replacements)) template <- gsub(key, replacements[[key]], template, fixed = TRUE)
  template
}

metric_map <- c(
  "Mængdeforbrug" = "Antal DDD",
  "Regionalt tilskud" = "Tilskudsbeløb - Regionalt",
  "Omsætning" = "Ekspeditionsbeløb"
)
metric_cols <- c(unname(metric_map), paste0(unname(metric_map), " pr. 1.000 borgere"))
month_names_da <- c("Januar", "Februar", "Marts", "April", "Maj", "Juni", "Juli", "August", "September", "Oktober", "November", "December")

normalise_year_month_columns <- function(dat) {
  month_raw <- str_trim(as.character(dat$`Måned`))
  year_raw <- str_trim(as.character(dat$`År`))
  embedded_year <- str_extract(month_raw, "[12][0-9]{3}$")
  if (any(!is.na(embedded_year) & embedded_year != year_raw, na.rm = TRUE)) {
    stop("År og årstallet i Måned stemmer ikke overens.")
  }
  month_text <- str_to_lower(str_trim(str_remove(month_raw, "\\s+[12][0-9]{3}$")))
  month_no <- match(month_text, str_to_lower(month_names_da))
  short_no <- match(month_text, c("jan", "feb", "mar", "apr", "maj", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))
  month_no <- coalesce(month_no, short_no)
  if (any(is.na(month_no)) || any(is.na(year_raw) | !str_detect(year_raw, "^[12][0-9]{3}$"))) {
    stop("Ukendt år/måned. Brug et nyt CSV-udtræk med læsbare månedsnavne; rå ordbogsindeks kan ikke tolkes sikkert.")
  }
  dat$`År` <- as.integer(year_raw)
  dat$`Måned` <- month_names_da[month_no]
  dat
}

read_export_csv <- function(path) {
  # Bevar identifikatorer, herunder foranstillede nuller i varenumre.
  dat <- readr::read_delim(path, delim = ";", col_types = cols(.default = col_character()),
    locale = locale(encoding = "UTF-8"), na = "", trim_ws = FALSE, show_col_types = FALSE)
  if (nrow(readr::problems(dat))) stop("CSV-filen har ugyldige felter eller forskudte kolonner.")
  dat
}

normalise_atc_columns <- function(dat) {
  legacy <- "ATC, Niveau 5, kode & tekst"
  if (legacy %in% names(dat)) {
    values <- str_trim(as.character(dat[[legacy]]))
    code <- str_extract(values, "^[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}")
    label <- str_trim(str_remove(values, "^[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}\\s*"))
    label <- str_trim(str_remove(label, "^[-–—]\\s*"))
    label <- str_replace(label, "^\\((.*)\\)$", "\\1")
    if (!"ATC-kode" %in% names(dat)) dat[["ATC-kode"]] <- code
    if (!"ATC-tekst" %in% names(dat)) dat[["ATC-tekst"]] <- label
    if (any(is.na(code) | is.na(dat[["ATC-kode"]]) | code != dat[["ATC-kode"]])) stop("ATC-kolonnerne stemmer ikke overens.")
    dat[[legacy]] <- NULL
  }
  dat
}

finalise_parsed_long_export <- function(long2) {
  long2 <- normalise_atc_columns(long2)
  required <- c("ATC-kode", "ATC-tekst", "År", "Måned", "Ydertype", "Bopælsregion", "nøgletal_output", "value")
  missing <- setdiff(required, names(long2))
  if (length(missing)) stop("CSV mangler kolonner: ", paste(missing, collapse = ", "))
  if (!nrow(long2)) return(tibble())

  if ("status" %in% names(long2) && any(is.na(long2$status) | as.character(long2$status) != "200")) stop("CSV indeholder mislykkede eller ukendte HTTP-kald.")
  if ("export_error" %in% names(long2) && any(!is.na(long2$export_error) & nzchar(str_trim(long2$export_error)))) stop("CSV indeholder eksportfejl.")
  if ("hasRestartToken" %in% names(long2) && any(tolower(as.character(long2$hasRestartToken)) %in% c("true", "1"))) stop("CSV indeholder afkortede svar (RestartToken).")
  if ("rowCountMarker" %in% names(long2) && any(suppressWarnings(as.numeric(long2$rowCountMarker)) >= 30000, na.rm = TRUE)) stop("CSV har nået rækkegrænsen. Hent et mindre deludtræk.")

  if (!is.numeric(long2$value)) {
    raw_value <- str_trim(as.character(long2$value))
    converted <- suppressWarnings(readr::parse_double(raw_value, locale = locale(decimal_mark = ","), na = c("", "NA")))
    invalid <- !is.na(raw_value) & !raw_value %in% c("", "NA") & (is.na(converted) | !is.finite(converted))
    if (any(invalid)) stop("Ugyldig numerisk værdi i CSV. Forventet decimalkomma uden tusindtalsseparator.")
    long2$value <- converted
  }
  if (any(!is.na(long2$value) & !is.finite(long2$value))) stop("CSV indeholder ikke-endelige tal.")
  long2 <- normalise_year_month_columns(long2)
  base_keys <- c("ATC-kode", "ATC-tekst", "År", "Måned", "Ydertype", "Bopælsregion")
  product_keys <- c("Varenummer", "Navn (Præparat)", "Form", "Styrke", "Pakningsstørrelse")
  for (nm in product_keys) {
    if (!nm %in% names(long2)) long2[[nm]] <- NA_character_
    long2[[nm]] <- na_if(str_trim(as.character(long2[[nm]])), "")
  }
  if (!"opgoerelsesniveau" %in% names(long2)) long2$opgoerelsesniveau <- NA_character_
  levels <- unique(na.omit(na_if(long2$opgoerelsesniveau, "")))
  if (length(levels) > 1 || any(!levels %in% c("atc5", "varenummer"))) stop("Upload kun ét opgørelsesniveau ad gangen.")
  has_product <- any(!is.na(long2$Varenummer))
  is_product <- identical(levels, "varenummer") || has_product
  if (identical(levels, "atc5") && has_product) stop("ATC5-udtrækket indeholder uventede produktfelter.")
  if (is_product) {
    if (any(!str_detect(coalesce(long2$Varenummer, ""), "^\\d{6}$")) || any(is.na(as.matrix(long2[product_keys])))) stop("Ufuldstændige produktfelter. Ingen rækker er fjernet automatisk.")
  }
  keys <- if (is_product) c(base_keys[1:2], product_keys, base_keys[-c(1, 2)]) else base_keys
  if (any(is.na(as.matrix(long2[base_keys]))) || any(str_trim(as.character(unlist(long2[base_keys]))) == "")) stop("En eller flere datanøgler mangler.")
  if (any(str_detect(long2$Ydertype, "^\\d+$")) || any(!str_detect(long2[[base_keys[1]]], "^[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}$"))) stop("CSV indeholder uafkodede ATC-/ydertypeværdier. Hent data med den nye generator.")
  if (any(is.na(long2$nøgletal_output) | !long2$nøgletal_output %in% metric_cols)) stop("CSV indeholder ukendte nøgletal.")
  if (anyDuplicated(long2[c(keys, "nøgletal_output")])) stop("Overlappende datarækker: upload ikke samme udtræk eller overlappende CSV-filer flere gange. Pr. 1.000-værdier må ikke summeres.")

  # Hver datanøgle har højst én værdi per nøgletal. Manglende værdier forbliver NA.
  out <- long2 |>
    select(all_of(c(keys, "nøgletal_output", "value"))) |>
    pivot_wider(names_from = nøgletal_output, values_from = value)
  for (nm in metric_cols) if (!nm %in% names(out)) out[[nm]] <- NA_real_
  out |>
    mutate(.month_order = match(`Måned`, month_names_da)) |>
    arrange(.data[[base_keys[1]]], `År`, .month_order, across(all_of(setdiff(keys, c(base_keys[1], "År", "Måned"))))) |>
    select(-.month_order) |>
    relocate(all_of(keys), all_of(metric_cols))
}

csv_metadata <- function(dat) {
  mapping <- c(atc = "atc_filter", region = "region_filter", year = "year_filter", opgoerelse = "opgoerelse", normalisering = "normalisering", opgoerelsesniveau = "opgoerelsesniveau", status = "status", rowCountMarker = "rowCountMarker", hasRestartToken = "hasRestartToken", error = "export_error")
  for (nm in unname(mapping)) if (!nm %in% names(dat)) dat[[nm]] <- NA_character_
  out <- dat |> select(all_of(unname(mapping))) |> distinct()
  names(out) <- names(mapping)
  out
}

read_export_summary <- function(path) {
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(as.integer(x$version), 2L) || !isTRUE(x$complete)) stop("Oversigten er ikke fra et fuldført udtræk med den nye generator.")
  requests <- x$requests %||% list()
  if (!length(x$exportId) || length(requests) != x$expectedQueries || x$completedQueries != x$expectedQueries) stop("Oversigten har manglende forespørgsler.")
  meta <- map_dfr(requests, function(r) tibble(
    atc = as.character(r$atc), region = as.character(r$region), year = paste(unlist(r$year), collapse = ", "),
    opgoerelse = as.character(r$opgoerelse), normalisering = as.character(r$normalisering),
    opgoerelsesniveau = as.character(r$opgoerelsesniveau), status = as.integer(r$status),
    rowCountMarker = as.integer(r$rowCountMarker), hasRestartToken = isTRUE(r$hasRestartToken),
    parsedRows = as.integer(r$parsedRows), error = r$error %||% NA_character_
  ))
  if (nrow(meta) != x$expectedQueries || any(is.na(meta$status) | meta$status != 200 | meta$hasRestartToken | !is.na(meta$error)) || any(is.na(meta$parsedRows) | meta$parsedRows < 0)) stop("Oversigten indeholder fejl eller manglende metadata.")
  expected_rows <- sum(meta$parsedRows)
  file_rows <- sum(vapply(x$files %||% list(), function(f) as.numeric(f$rows), numeric(1)))
  if (expected_rows != file_rows) stop("Rækkeantal i oversigten stemmer ikke overens.")
  list(export_id = x$exportId, rows = expected_rows, meta = meta)
}

validate_export_summaries <- function(summaries, long) {
  if (!length(summaries)) return(invisible(NULL))
  ids <- vapply(summaries, function(s) as.character(s$export_id), character(1))
  if (anyDuplicated(ids)) stop("Samme eksportoversigt er uploadet flere gange.")
  if (nrow(long)) {
    if (!"export_id" %in% names(long) || any(is.na(long$export_id) | !long$export_id %in% ids)) stop("CSV og oversigt hører ikke sammen, eller der mangler en oversigt. CSV fra den tidligere test kan importeres uden oversigt.")
  }
  for (s in summaries) {
    found <- if (nrow(long)) sum(long$export_id == s$export_id) else 0L
    if (found != s$rows) stop("Der mangler CSV-dele, eller der er ekstra rækker for udtræk ", s$export_id, ".")
  }
  invisible(NULL)
}


# Færdige Excel-udtræk og Taksten: bevar nul, NA og varenumre inkl. foranstillede nuller.
parse_amount <- function(x, column = "nøgletal") {
  raw <- trimws(as.character(x))
  raw[raw %in% c("", "NA", "NaN")] <- NA_character_
  raw <- gsub("[[:space:]\u00a0]", "", raw, perl = TRUE)
  both <- !is.na(raw) & grepl(",", raw, fixed = TRUE) & grepl(".", raw, fixed = TRUE)
  raw[both] <- gsub(".", "", raw[both], fixed = TRUE)
  raw <- gsub(",", ".", raw, fixed = TRUE)
  out <- suppressWarnings(as.numeric(raw))
  if (any(!is.na(raw) & (is.na(out) | !is.finite(out)))) stop("Ugyldigt tal i ", column, ".")
  out
}

normalise_product_number <- function(x) {
  v <- trimws(as.character(x))
  v <- sub("\\.0+$", "", v)
  good <- !is.na(v) & grepl("^[0-9]{1,6}$", v)
  v[good] <- stringr::str_pad(v[good], 6, pad = "0")
  v
}

normalise_wide_export <- function(dat) {
  dat <- tibble::as_tibble(dat)
  names(dat) <- trimws(names(dat))
  dat <- normalise_atc_columns(dat)
  keys <- c("ATC-kode", "ATC-tekst", "År", "Måned", "Ydertype", "Bopælsregion")
  if (length(setdiff(keys, names(dat)))) stop("Excel mangler: ", paste(setdiff(keys, names(dat)), collapse = ", "))
  if (!nrow(dat) || !any(metric_cols %in% names(dat))) stop("Ingen salgstal eller nøgletal i Excel.")
  dat <- normalise_year_month_columns(dat)
  dat[["ATC-kode"]] <- toupper(trimws(as.character(dat[["ATC-kode"]])))
  for (nm in c("ATC-tekst", "Ydertype", "Bopælsregion")) dat[[nm]] <- trimws(as.character(dat[[nm]]))
  if (any(!stats::complete.cases(dat[keys])) ||
      any(vapply(dat[keys], function(x) any(!nzchar(trimws(as.character(x)))), logical(1))) ||
      any(!grepl("^[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}$", dat[["ATC-kode"]]))) {
    stop("Ugyldige eller manglende datanøgler.")
  }
  product <- c("Varenummer", "Navn (Præparat)", "Form", "Styrke", "Pakningsstørrelse")
  has_product <- "Varenummer" %in% names(dat) &&
    any(!is.na(dat[["Varenummer"]]) & nzchar(trimws(as.character(dat[["Varenummer"]]))))
  if (has_product) {
    if (length(setdiff(product, names(dat)))) stop("Ufuldstændige produktkolonner.")
    dat[["Varenummer"]] <- normalise_product_number(dat[["Varenummer"]])
    for (nm in setdiff(product, "Varenummer")) dat[[nm]] <- trimws(as.character(dat[[nm]]))
    if (any(!grepl("^[0-9]{6}$", dat[["Varenummer"]])) ||
        any(!stats::complete.cases(dat[product])) ||
        any(vapply(dat[product], function(x) any(!nzchar(x)), logical(1)))) {
      stop("Ugyldige varenumre eller manglende produktfelter.")
    }
  } else {
    dat <- dplyr::select(dat, -dplyr::any_of(product))
  }
  for (nm in metric_cols) {
    dat[[nm]] <- if (nm %in% names(dat)) parse_amount(dat[[nm]], nm) else rep(NA_real_, nrow(dat))
  }
  row_keys <- if (has_product) c(keys[1:2], product, keys[-c(1, 2)]) else keys
  if (anyDuplicated(dat[row_keys])) stop("Overlappende datarækker. Samme vare, periode, region og ydertype må kun forekomme én gang.")
  dat[[".month_order"]] <- match(dat[["Måned"]], month_names_da)
  dat <- dplyr::arrange(dat, .data[["ATC-kode"]], .data[["År"]], .data[[".month_order"]], .data[["Bopælsregion"]])
  dplyr::select(dat, -dplyr::all_of(".month_order"), dplyr::all_of(row_keys), dplyr::all_of(metric_cols))
}

read_sales_xlsx <- function(path) {
  for (sheet in readxl::excel_sheets(path)) {
    nms <- tryCatch(names(readxl::read_excel(path, sheet = sheet, n_max = 0)), error = function(e) character())
    if (all(c("År", "Måned", "Ydertype", "Bopælsregion") %in% nms) &&
        ("ATC-kode" %in% nms || "ATC, Niveau 5, kode & tekst" %in% nms) &&
        any(metric_cols %in% nms)) {
      return(normalise_wide_export(readxl::read_excel(path, sheet = sheet,
        col_types = "text", .name_repair = "minimal")))
    }
  }
  stop("Ingen medicinsalgstabel fundet i Excel-filen.")
}

takst_column <- function(columns, choices) {
  idx <- match(tolower(choices), tolower(trimws(columns)))
  idx <- idx[!is.na(idx)]
  if (length(idx)) columns[idx[1]] else NA_character_
}
read_takst_xlsx <- function(path) {
  dat <- NULL
  for (sheet in readxl::excel_sheets(path)) {
    for (skip in 0:5) {
      nms <- tryCatch(names(readxl::read_excel(path, sheet = sheet, skip = skip, n_max = 0)), error = function(e) character())
      if (!is.na(takst_column(nms, c("Varenr.", "Varenummer", "Varenr", "Vare nr.")))) {
        dat <- readxl::read_excel(path, sheet = sheet, skip = skip,
          col_types = "text", .name_repair = "minimal")
        break
      }
    }
    if (!is.null(dat)) break
  }
  if (is.null(dat)) stop("Taksten mangler Varenr. / Varenummer.")
  names(dat) <- trimws(names(dat))
  nms <- names(dat)
  get <- function(options) {
    nm <- takst_column(nms, options)
    if (is.na(nm)) rep(NA_character_, nrow(dat)) else as.character(dat[[nm]])
  }
  prices <- c("ESP (kr.)", "TSP (kr.)", "Pris pr. DDD (kr.)")
  if (all(vapply(c(prices, "Substitutionsgrp."), function(x) is.na(takst_column(nms, x)), logical(1)))) {
    stop("Taksten mangler genkendelige pris-/substitutionskolonner.")
  }
  out <- tibble::tibble(
    "Varenummer" = normalise_product_number(get(c("Varenr.", "Varenummer", "Varenr", "Vare nr."))),
    "Takst · ATC-kode" = toupper(trimws(get(c("ATC-kode", "ATC")))),
    "Takst · ESP (kr.)" = parse_amount(get(c("ESP (kr.)", "ESP", "Pris")), "ESP"),
    "Takst · TSP (kr.)" = parse_amount(get(c("TSP (kr.)", "TSP")), "TSP"),
    "Takst · Pris pr. DDD (kr.)" = parse_amount(get(c("Pris pr. DDD (kr.)", "Pris_pr_DDD")), "Pris pr. DDD"),
    "Takst · Substitutionsgruppe" = get(c("Substitutionsgrp.", "Tilskudssubstitutionsgruppe", "Substitutionsgruppe"))
  )
  out <- dplyr::filter(out, grepl("^[0-9]{6}$", .data[["Varenummer"]]))
  out <- dplyr::distinct(out)
  if (!nrow(out)) stop("Ingen gyldige varenumre i Taksten.")
  if (anyDuplicated(out[["Varenummer"]])) stop("Flere forskellige Takst-opslag for samme varenummer.")
  dplyr::mutate(out, .takst_match = TRUE)
}
enrich_sales_with_takst <- function(sales, takst) {
  if (!nrow(sales) || is.null(takst) || !"Varenummer" %in% names(sales)) return(sales)
  out <- dplyr::left_join(sales, takst, by = "Varenummer")
  out[["Takstmatch"]] <- !is.na(out[[".takst_match"]])
  out[["ATC-afvigelse"]] <- out[["Takstmatch"]] &
    !is.na(out[["Takst · ATC-kode"]]) &
    out[["Takst · ATC-kode"]] != out[["ATC-kode"]]
  dplyr::select(out, -dplyr::all_of(".takst_match"))
}
safe_total <- function(x) if (!length(x) || all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
format_metric <- function(x) if (is.na(x)) "—" else format(round(x), big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE)

# Samme farver, systemskrifter og kort som homepage og lister.
kfa_theme <- bs_theme(
  version = 5, bg = "#F6F8FB", fg = "#1F2933", primary = "#0033A0", secondary = "#667085",
  base_font = "-apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif",
  heading_font = "-apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif"
)

ui <- fluidPage(
  theme = kfa_theme,
  tags$head(
    tags$title("Medicinsalg · Power BI | KFA apps"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body { background:#F6F8FB; color:#1F2933; }
      .container-fluid { padding:0; }
      .kfa-shell { width:100%; max-width:1440px; margin:0 auto; padding:28px 28px 60px; }
      .kfa-topbar { display:flex; justify-content:space-between; align-items:center; margin-bottom:30px; }
      .home-link { color:#667085; text-decoration:none; font-size:.88rem; font-weight:550; }
      .home-link:hover { color:#0033A0; }
      .kfa-label { color:#98A2B3; font-size:.78rem; font-weight:600; letter-spacing:.04em; text-transform:uppercase; }
      .app-heading { margin-bottom:30px; }
      .app-heading h1 { margin:0; color:#182230; font-size:clamp(2rem,4vw,2.75rem); font-weight:700; letter-spacing:-.035em; }
      .app-heading p { margin:10px 0 0; max-width:760px; color:#667085; line-height:1.6; }
      .bslib-sidebar-layout { border:0; background:transparent; gap:24px; overflow:visible; }
      .bslib-sidebar-layout > .sidebar { background:#FFF; border:1px solid #E3E8EF; border-radius:14px; box-shadow:0 5px 18px rgba(16,24,40,.035); }
      .bslib-sidebar-layout > .main { padding:0; overflow:visible; }
      .card { background:#FFF; border:1px solid #E3E8EF; border-radius:14px; box-shadow:0 5px 18px rgba(16,24,40,.035); }
      .card-header { background:#FFF; color:#182230; border-bottom:1px solid #E3E8EF; font-weight:650; padding:18px 22px; }
      .card-body { padding:20px 22px; }
      .btn { border-radius:9px; font-weight:550; }
      .btn-primary { background:#0033A0; border-color:#0033A0; }
      .btn-primary:hover { background:#002774; border-color:#002774; }
      .btn-default { background:#FFF; color:#344054; border:1px solid #D0D5DD; }
      .btn-outline-secondary { color:#344054; border-color:#D0D5DD; background:#FFF; }
      .btn-outline-secondary:hover { color:#0033A0; border-color:#9CB6E8; background:#EDF3FF; }
      .form-control { border-color:#D0D5DD; border-radius:9px; box-shadow:none; }
      .form-control:focus { border-color:#7295DA; box-shadow:0 0 0 3px rgba(0,51,160,.10); }
      h5, label, .control-label { color:#182230; }
      h5 { font-size:1rem; font-weight:650; }
      code { color:#0033A0; }
      .alert { border-radius:10px; font-size:.9rem; }
      .alert-info { color:#344054; background:#EDF3FF; border-color:#DCE7FD; }
      .kfa-footer { margin-top:28px; color:#7A8494; font-size:.82rem; }
      .app-heading { background:#FFF; border:1px solid #E3E8EF; border-radius:18px; padding:29px 34px; box-shadow:0 8px 28px rgba(16,24,40,.045); }
      .kfa-kpi-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:12px; margin-bottom:15px; }
      .kfa-kpi { background:#FFF; border:1px solid #E3E8EF; border-radius:13px; padding:17px 20px; }
      .kfa-kpi-title { color:#667085; font-size:.78rem; font-weight:600; }
      .kfa-kpi-number { color:#182230; font-size:1.55rem; font-weight:700; margin-top:4px; font-variant-numeric:tabular-nums; }
      .kfa-control-row { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:12px; }
      .kfa-control-row .form-group { margin-bottom:5px; }
      .kfa-help { color:#667085; font-size:.81rem; line-height:1.5; margin-top:8px; }
      @media(max-width:920px) { .kfa-kpi-grid,.kfa-control-row { grid-template-columns:1fr; } }
      @media(max-width:760px) { .app-heading { padding:23px 20px; } }
      @media(max-width:760px) { .kfa-shell {padding:22px 16px 40px;} .kfa-topbar {margin-bottom:22px;} }
"))
  ),
  div(class = "kfa-shell",
    div(class = "kfa-topbar",
      tags$a(class = "home-link", href = "https://kfaapps.au.dk/", "← Alle apps"),
      tags$span(class = "kfa-label", "Klinisk Farmakologi")
    ),
    div(class = "app-heading", h1("Medicinsalg · Power BI"),
      p("Generér Power BI-udtræk, åbn færdige Excel-filer og analysér medicinsalget. På varenummerniveau kan du tilføje Taksten.")
    ),
    layout_sidebar(fillable = FALSE,
  sidebar = sidebar(
    width = 360,
    
    h5("1. Dataudtræk"),
    
    textAreaInput(
      "atc",
      "ATC-koder eller Power BI ATC5-værdier",
      value = paste("C09CA01", "N05CJ03", "R05DB29", sep = "\n"),
      rows = 7,
      placeholder = "Én ATC-kode per linje, fx:\nA02BC02\nB01AF01\nN06BA04\n\nEller fuld Power BI-værdi:\nA02BC02 (Pantoprazol)"
    ),
    
    radioButtons(
      "output_level",
      "Opgørelsesniveau",
      choices = c(
        "ATC5-niveau" = "atc5",
        "Varenummerniveau" = "varenummer"
      ),
      selected = "atc5"
    ),
    
    fileInput(
      "mapping_file",
      "Valgfri mapping-tabel: ATC → ATC5_Kode_Tekst",
      accept = c(".csv", ".txt", ".tsv")
    ),
    
    div(
      class = "alert alert-info py-2",
      strong("Nøgletal: "),
      "Alle tre nøgletal medtages altid — både som absolutte værdier og pr. 1.000 borgere."
    ),
    
    checkboxGroupInput(
      "regions",
      "Region",
      choices = c(
        "Region Hovedstaden",
        "Region Sjælland",
        "Region Syddanmark",
        "Region Midtjylland",
        "Region Nordjylland"
      ),
      selected = c(
        "Region Hovedstaden",
        "Region Sjælland",
        "Region Syddanmark",
        "Region Midtjylland",
        "Region Nordjylland"
      )
    ),
    
    sliderInput(
      "years_range",
      "Periode (år)",
      min = 2020,
      max = as.integer(format(Sys.Date(), "%Y")),
      value = c(max(2023, as.integer(format(Sys.Date(), "%Y")) - 3), as.integer(format(Sys.Date(), "%Y"))),
      step = 1,
      sep = "",
      ticks = TRUE
    ),
    
    hr(),
    
    h5("2. Åbn udtræksfilen"),
    
    fileInput(
      "parsed_csv_files",
      "Vælg færdig Excel (.xlsx) eller rå eksport (.csv)",
      accept = c(".csv", ".xlsx"),
      multiple = TRUE
    ),
    
    p(class = "text-muted small", "Vælg færdige Excel-filer eller samtlige rå CSV-dele. Upload ikke både CSV og Excel fra samme udtræk."),
    fileInput("summary_files", "Valgfrit: eksportoversigt (*_summary.json)", accept = c(".json"), multiple = TRUE),
    
    actionButton("import_btn", "Indlæs og kontrollér data", class = "btn-primary"),

    hr(),
    h5("3. Valgfrit: Taksten"),
    fileInput("takst_file", "Tilføj Taksten (.xlsx)", accept = ".xlsx"),
    p(class = "kfa-help", "På varenummerniveau matches Taksten på varenummer. Aktuelle priser må ikke fortolkes som historiske salgspriser."),
    
    hr(),
    
    downloadButton("download_js", "Download JavaScript"),
    downloadButton("download_csv", "Download CSV"),
    downloadButton("download_xlsx", "Download Excel")
  ),
  
  layout_columns(
    col_widths = c(12),
    
    card(
      card_header("Sådan bruges generatoren"),
      div(
        tags$ol(
          tags$li(tags$a(href = "https://sundhedsdatabank.dk/medicin/medicinsalg-og-udgifter-menu/medicinsalg", target = "_blank", rel = "noopener", "Åbn rapporten Medicinsalg i Sundhedsdatabanken.")),
          tags$li("Tryk F12 → Console, og vælg app.powerbi.com i frame-menuen øverst (ofte står der top)."),
          tags$li("Kopiér koden nedenfor til Console, og kør hele koden."),
          tags$li("Ændr nu ét filter i Power BI-rapporten. Koden forsøger automatisk at opsamle rapportens adgangstoken og starter derefter udtrækket."),
          tags$li("Vent til Console skriver Færdig. Browseren downloader en udtræksfil (.csv, som kan åbnes i Excel) og en eksportoversigt. Store udtræk kan give flere datafiler."),
          tags$li("Åbn en færdig Excel-fil eller alle browserens CSV-dele, klik Indlæs og kontrollér data, og tilføj eventuelt Taksten.")
        ),
        p("Adgangstokenet udløber og kan derfor ikke gemmes permanent i appen. Det opsamles kun i Power BI-fanen og gemmes ikke i filer. Stop udtrækket med ", tags$code("medicinsalgExport.stop()"), "."),
        tags$details(tags$summary("Hvis automatisk opsamling ikke virker"),
          p("I Network: filtrér på querydata, ændr et filter, og vælg et kald med status 200. Kopiér hele Authorization-værdien under Request Headers. Mens udtrækskoden venter, kør denne kommando i Console med værdien indsat:"),
          tags$pre('medicinsalgExport.authorize("INDSÆT_HELE_AUTHORIZATION_HEADEREN")'),
          p("Ved 401/403 er tokenet muligvis udløbet. Kør udtrækskoden igen, og skift et filter. Indsæt ikke tokenet i Shiny-appen.")
        ),
        p("Ingen datafiler downloades før alle deludtræk er færdige. Eksportoversigten viser også tomme deludtræk; tomme svar omdannes ikke til nul.")
      )
    ),
    
    card(
      card_header(
        div(
          "Genereret JavaScript",
          tags$button(
            id = "copy_js_btn",
            type = "button",
            class = "btn btn-sm btn-outline-secondary float-end",
            "Kopiér kode"
          )
        )
      ),
      tags$textarea(
        id = "js_code_area",
        style = "width:100%; min-height:430px; font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', monospace; font-size: 12px;",
        readonly = NA
      ),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('set_js_code', function(message) {
          const area = document.getElementById('js_code_area');
          if (area) area.value = message;
        });

        document.addEventListener('DOMContentLoaded', function() {
          const btn = document.getElementById('copy_js_btn');
          btn.addEventListener('click', async function() {
            const txt = document.getElementById('js_code_area').value;
            try {
              await navigator.clipboard.writeText(txt);
              btn.innerText = 'Kopieret!';
              setTimeout(() => btn.innerText = 'Kopiér kode', 1200);
            } catch(e) {
              const area = document.getElementById('js_code_area');
              area.select();
              document.execCommand('copy');
              btn.innerText = 'Kopieret!';
              setTimeout(() => btn.innerText = 'Kopiér kode', 1200);
            }
          });
        });
      "))
    ),
    
    uiOutput("validation_message"),
    uiOutput("import_status"),
    uiOutput("import_notice"),
    
    card(
      card_header("Oversigt over deludtræk"),
      DTOutput("metadata_table")
    ),
    
    card(
      card_header("Analyse og datakvalitet"),
      uiOutput("kpi_cards"),
      div(class = "kfa-control-row",
        selectInput("view_atc", "ATC-kode", choices = c("Alle" = "")),
        selectInput("view_region", "Bopælsregion", choices = c("Alle" = "")),
        selectInput("view_year", "År", choices = c("Alle" = ""))
      ),
      selectInput("view_metric", "Nøgletal i udviklingskurve",
        choices = stats::setNames(unname(metric_map), unname(metric_map)), selected = "Antal DDD"),
      plotOutput("sales_trend", height = "280px"),
      uiOutput("takst_status"),
      p(class = "kfa-help", "Udviklingskurven summerer kun absolutte målinger. Pr. 1.000 borgere må ikke summeres på tværs af regioner."),
      uiOutput("quality_message")
    ),
    card(
      card_header("Samlet tabel · filtrér og eksportér"),
      DTOutput("parsed_table")
    )
  )
    ),
    div(class = "kfa-footer", "KFA apps · Klinisk Farmakologisk Afdeling, AUH")
  )
)

server <- function(input, output, session) {
  mapping_data <- reactive({
    req(input$mapping_file)
    # Shiny datapath har ikke nødvendigvis filtypen fra det uploadede filnavn.
    read_mapping_file(input$mapping_file$datapath, input$mapping_file$name)
  })
  selected_years <- reactive({
    rng <- suppressWarnings(as.integer(input$years_range))
    rng <- rng[!is.na(rng)]
    if (length(rng) < 2) return(integer(0))
    seq.int(min(rng), max(rng))
  })
  atc_powerbi <- reactive({
    apply_atc_mapping(normalise_atc_input(input$atc), if (!is.null(input$mapping_file)) mapping_data() else NULL)
  })
  generated_js <- reactive({
    shiny::validate(
      shiny::need(length(atc_powerbi()) > 0, "Indtast mindst én ATC-kode."),
      shiny::need(length(input$regions) > 0, "Vælg mindst én region."),
      shiny::need(length(selected_years()) > 0, "Vælg mindst ét år.")
    )
    generate_js(atc_powerbi(), names(metric_map), input$regions, selected_years(), TRUE, input$output_level %||% "atc5")
  })
  output$validation_message <- renderUI({
    div(class = "alert alert-secondary",
      strong("ATC-værdier sendt til Power BI-filteret: "), tags$code(paste(atc_powerbi(), collapse = "; ")), br(),
      strong("Opgørelsesniveau: "), if ((input$output_level %||% "atc5") == "varenummer") "Varenummerniveau (kontrollér første udtræk mod rapporten)" else "ATC5-niveau")
  })
  observe({ session$sendCustomMessage("set_js_code", generated_js()) })
  output$download_js <- downloadHandler(
    filename = function() paste0("powerbi_medicinsalg_export_", format(Sys.Date(), "%Y%m%d"), ".js"),
    content = function(file) writeLines(generated_js(), file, useBytes = TRUE)
  )
  metadata_val <- reactiveVal(tibble())
  parsed_data_val <- reactiveVal(tibble())
  import_status_val <- reactiveVal(NULL)
  import_notice_val <- reactiveVal(NULL)

  observeEvent(input$import_btn, {
    uploaded <- input$parsed_csv_files
    has_files <- !is.null(uploaded) && nrow(uploaded) > 0
    has_summary <- !is.null(input$summary_files) && nrow(input$summary_files) > 0
    req(has_files || has_summary)
    metadata_val(tibble()); parsed_data_val(tibble()); import_status_val(NULL); import_notice_val(NULL)
    tryCatch({
      longs <- list(); wides <- list()
      if (has_files) for (i in seq_len(nrow(uploaded))) {
        ext <- tolower(tools::file_ext(uploaded$name[i]))
        if (ext == "csv") longs[[length(longs) + 1L]] <- read_export_csv(uploaded$datapath[i])
        else if (ext == "xlsx") wides[[length(wides) + 1L]] <- read_sales_xlsx(uploaded$datapath[i])
        else stop("Ukendt filtype: ", uploaded$name[i])
      }
      long <- if (length(longs)) dplyr::bind_rows(longs) else tibble()
      summaries <- if (has_summary) purrr::map(input$summary_files$datapath, read_export_summary) else list()
      if (has_summary && !length(longs)) stop("Eksportoversigt kræver tilhørende rå CSV-filer.")
      validate_export_summaries(summaries, long)
      if (length(longs)) wides <- c(wides, list(finalise_parsed_long_export(long)))
      if (!length(wides)) stop("Ingen salgstal indlæst.")
      modes <- vapply(wides, function(x) "Varenummer" %in% names(x), logical(1))
      if (length(unique(modes)) > 1L) stop("ATC5- og varenummerdata skal indlæses hver for sig.")
      parsed <- normalise_wide_export(dplyr::bind_rows(wides))
      meta <- if (has_summary) dplyr::bind_rows(purrr::map(summaries, "meta")) else if (length(longs)) csv_metadata(long) else tibble()
      metadata_val(meta); parsed_data_val(parsed)
      import_status_val(list(ok = TRUE, message = paste0("Importeret ", nrow(parsed), " rækker fra ", nrow(uploaded), " fil(er). Niveau: ", if (modes[1]) "varenummer" else "ATC5", ". Manglende målinger og nulværdier er bevaret.")))
      notices <- character()
      if (length(longs) && !has_summary) notices <- c(notices, "Ingen eksportoversigt: tomme Power BI-kald og manglende CSV-dele kan ikke kontrolleres.")
      if (has_summary && any(meta$parsedRows == 0)) notices <- c(notices, paste0(sum(meta$parsedRows == 0), " deludtræk gav ingen rækker; dette er ikke det samme som nul forbrug."))
      dates <- as.Date(sprintf("%04d-%02d-01", parsed[["År"]], match(parsed[["Måned"]], month_names_da)))
      notices <- c(notices, paste0("Observeret periode: ", format(min(dates), "%m/%Y"), "–", format(max(dates), "%m/%Y"), ". Dækningen kan variere mellem varer og regioner."))
      absent <- metric_cols[colSums(!is.na(parsed[metric_cols])) == 0]
      if (length(absent)) notices <- c(notices, paste0("Ingen målinger for: ", paste(absent, collapse = "; "), "."))
      import_notice_val(notices)
    }, error = function(e) {
      metadata_val(tibble()); parsed_data_val(tibble())
      import_status_val(list(ok = FALSE, message = paste("Importfejl:", conditionMessage(e))))
    })
  })
  output$import_status <- renderUI({
    s <- import_status_val(); if (is.null(s)) return(NULL)
    div(class = if (isTRUE(s$ok)) "alert alert-success" else "alert alert-danger", s$message)
  })
  output$import_notice <- renderUI({
    notices <- import_notice_val(); if (!length(notices)) return(NULL)
    div(class = "alert alert-info", lapply(notices, tags$p))
  })
  takst_result <- reactive({
    if (is.null(input$takst_file)) return(list(data = NULL, error = NULL))
    tryCatch(list(data = read_takst_xlsx(input$takst_file$datapath), error = NULL),
      error = function(e) list(data = NULL, error = conditionMessage(e)))
  })
  enriched_data <- reactive({
    dat <- parsed_data_val()
    if (!nrow(dat)) return(dat)
    enrich_sales_with_takst(dat, takst_result()$data)
  })
  observeEvent(parsed_data_val(), {
    dat <- parsed_data_val()
    if (!nrow(dat)) return()
    atcs <- sort(unique(dat[["ATC-kode"]]))
    regions <- sort(unique(dat[["Bopælsregion"]]))
    years <- sort(unique(dat[["År"]]))
    updateSelectInput(session, "view_atc", choices = c("Alle" = "", stats::setNames(atcs, atcs)), selected = "")
    updateSelectInput(session, "view_region", choices = c("Alle" = "", stats::setNames(regions, regions)), selected = "")
    updateSelectInput(session, "view_year", choices = c("Alle" = "", stats::setNames(as.character(years), as.character(years))), selected = "")
  })
  filtered_data <- reactive({
    dat <- enriched_data()
    if (!nrow(dat)) return(dat)
    if (nzchar(input$view_atc %||% "")) dat <- dplyr::filter(dat, .data[["ATC-kode"]] == input$view_atc)
    if (nzchar(input$view_region %||% "")) dat <- dplyr::filter(dat, .data[["Bopælsregion"]] == input$view_region)
    if (nzchar(input$view_year %||% "")) dat <- dplyr::filter(dat, .data[["År"]] == as.integer(input$view_year))
    dat
  })
  output$kpi_cards <- renderUI({
    dat <- filtered_data()
    if (!nrow(dat)) return(NULL)
    totals <- vapply(unname(metric_map), function(m) safe_total(dat[[m]]), numeric(1))
    labels <- c("DDD i udvalget", "Regionalt tilskud · kr.", "Ekspeditionsbeløb · kr.")
    div(class = "kfa-kpi-grid", lapply(seq_along(labels), function(i)
      div(class = "kfa-kpi", div(class = "kfa-kpi-title", labels[i]),
        div(class = "kfa-kpi-number", format_metric(totals[i])))))
  })
  output$sales_trend <- renderPlot({
    dat <- filtered_data()
    if (!nrow(dat)) return(invisible(NULL))
    m <- input$view_metric %||% "Antal DDD"
    shiny::validate(shiny::need(m %in% unname(metric_map), "Vælg et nøgletal."))
    date <- as.Date(sprintf("%04d-%02d-01", dat[["År"]], match(dat[["Måned"]], month_names_da)))
    grouped <- data.frame(date = date, value = dat[[m]])
    grouped <- grouped[!is.na(grouped$value), , drop = FALSE]
    if (!nrow(grouped)) {
      graphics::plot.new(); graphics::text(0.5, 0.5, "Ingen målinger for det valgte nøgletal.")
      return(invisible(NULL))
    }
    summed <- stats::aggregate(value ~ date, grouped, sum)
    summed <- summed[order(summed$date), , drop = FALSE]
    graphics::plot(summed$date, summed$value, type = "o", pch = 16,
      col = "#0033A0", lwd = 2, cex = .65, xlab = "Måned", ylab = m,
      main = "", las = 1, panel.first = graphics::grid(col = "#E3E8EF"))
  })
  output$takst_status <- renderUI({
    dat <- parsed_data_val()
    if (!nrow(dat) || is.null(input$takst_file)) return(NULL)
    if (!is.null(takst_result()$error))
      return(div(class = "alert alert-danger", paste("Takstimport:", takst_result()$error)))
    if (!"Varenummer" %in% names(dat))
      return(div(class = "alert alert-info", "Taksten kan kun kobles på varenummerniveau."))
    joined <- enriched_data()
    total <- length(unique(joined[["Varenummer"]]))
    hit <- length(unique(joined[["Varenummer"]][joined[["Takstmatch"]]]))
    mismatch <- length(unique(joined[["Varenummer"]][joined[["ATC-afvigelse"]]]))
    div(class = if (mismatch) "alert alert-warning" else "alert alert-info",
      paste0("Taksten: ", hit, " af ", total, " unikke varenumre matchet. ",
        mismatch, " har afvigende ATC-kode. Takstpriserne er ikke historiske salgspriser."))
  })
  output$quality_message <- renderUI({
    dat <- filtered_data()
    if (!nrow(dat)) return(NULL)
    x <- as.matrix(dat[metric_cols])
    empty <- sum(rowSums(!is.na(x)) == 0)
    zeros <- sum(rowSums(!is.na(x)) > 0 & rowSums(replace(x, is.na(x), 0)) == 0)
    div(class = "kfa-help", paste0("Rækker: ", format(nrow(dat), big.mark = "."),
      " · Uden nøgletal: ", empty, " · Kun registrerede nulværdier: ", zeros, "."))
  })
  output$metadata_table <- renderDT({
    req(nrow(metadata_val()) > 0)
    datatable(metadata_val(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$parsed_table <- renderDT({
    req(nrow(parsed_data_val()) > 0)
    datatable(filtered_data(), options = list(pageLength = 25, scrollX = TRUE, order = list()), rownames = FALSE)
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("medicinsalg_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      dat <- filtered_data(); if (!nrow(dat)) stop("Importér data først.")
      readr::write_excel_csv2(dat, file, na = "")
    }
  )
  output$download_xlsx <- downloadHandler(
    filename = function() paste0("medicinsalg_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      dat <- parsed_data_val(); if (!nrow(dat)) stop("Importér CSV-data først.")
      out <- list("Medicinsalg" = dat); if (nrow(metadata_val())) out[["Udtræk"]] <- metadata_val(); writexl::write_xlsx(out, path = file)
    }
  )
}

shinyApp(ui, server)
