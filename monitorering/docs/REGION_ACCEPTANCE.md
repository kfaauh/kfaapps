# Bopælsregioner i Monitorering

## Liveaccept

Den grupperede forespørgsel returnerede observationer af begge målinger for alle fem bopælsregioner i liveprøven den 18. november 2026. Afgrænsningen var methylphenidat (N06BA04), januar 2025.

| Måling | Hele Danmark | Sum af fem regioner | Regionssum minus landstal | Varenumre med forskel blandt varenumre med observation i alle seks afgrænsninger |
| --- | ---: | ---: | ---: | ---: |
| DDD | 3.340.529 | 3.340.524 | −5 | 12 af 81 |
| Tilskud (kr.) | 32.599.267 | 32.599.272 | +5 | 31 af 78 |

For henholdsvis 20 og 23 varenumre var mindst én regional måling fraværende. Årsagen til de små differencer er uafklaret. Appens nationale serie bruger det nationale udtræk. Regionale observationer påvirker hverken national dækning eller revisionshistorik.

Prøven bruger methylphenidat (N06BA04) i januar 2025. Den kører via den eksisterende Medicinsalg-rapport. Begge varianter bruger samme ATC5, måned, DDD, Flag T3, sygdomsafgrænsning og målinger.

1. Åbn Medicinsalg-rapporten med din sædvanlige adgang. Vælg konsolrammen `app.powerbi.com` i DevTools.
2. Kopiér indholdet af [branchens eksportør](../browser/monitorering.js) ind i konsollen.
3. Start prøven, og skift derefter ét filter i rapporten, så den eksisterende eksportør kan bruge rapportens aktive adgang.

```js
Monitorering.probeRegions().then(result => console.log(JSON.stringify(result, null, 2)))
```

Første kald henter den eksisterende nationale afgrænsning med alle fem regioner. Andet kald beholder samme filter og tilføjer `DimBorgerDemografi.Bopælsregion` som gruppenøgle. Prøven viser målinger, rækkeantal og differencer i konsollen. Send JSON-resultatet eller fejlteksten. Prøven opretter hverken importfiler eller checkpoint.

Hvis det nationale kald lykkes, men det grupperede kald bliver afvist, returnerer en uventet struktur, mangler en region eller bliver afkortet, kan du køre varianten med ét filter pr. region. Skift igen ét rapportfilter efter start. Varianten kører seks kald, inklusive en national kontrol.

```js
Monitorering.probeRegions({ separate: true }).then(result => console.log(JSON.stringify(result, null, 2)))
```

Kontrollér følgende:

- Alle fem forventede regionsnavne optræder med observationer af begge målinger. Prøven afviser ukendte regioner, duplikerede målingsnøgler og tegn på afkortning.
- Sammenlign `regionSum` med `nationalTotal` for DDD og tilskud. `difference` er regionssummen minus landstallet. `productDifferences` tæller kun varenumre med observation i alle seks afgrænsninger. `incompleteProducts` tæller øvrige varenumre. `null` angiver, at den pågældende sum eller differens ikke kunne beregnes.
- Det arkiverede nationale Test 8-udtræk havde for denne afgrænsning 101 DDD-målinger, 101 tilskudsmålinger, 3.340.703 DDD og 32.601.464 kr. En eventuel revision af kilden vurderes særskilt fra differencen mellem samtidige kald. Registrér uforklarede differencer, også hvis de er små.

## Datakontrakt og analyse

| Led | Ændring |
| --- | --- |
| Eksport og import | Version 3 med eksplicit region på rækkerne, deklareret geografisk forespørgselsscope og kontroller pr. måling og region. Version 1 og 2 importeres fortsat som Hele Danmark. Kildens regionale navne mappes til faste koder. |
| DuckDB og SQL | Skema 3 knytter eksisterende udtræk og observationer transaktionelt til `DK` og bevarer id’er, rækkefølge, tidsstempler, arkiver og attestationer. Revision, ATC-aggregater og priskobling findes i visninger med endelsen `_by_region`. De hidtidige visninger returnerer nationale observationer. Et nationalt udtræk og regionale udtræk kan eksistere for samme periode og ATC uden at påvirke hinandens revisioner. |
| Dækning og afstemning | Dækning registreres pr. region, måned og ATC-afgrænsning. Et teknisk komplet svar kan have nul rækker i en region. Det registreres som forespørgselsdækning med ukendte målinger. Nationale og regionale summer kan afstemmes med liveprøven ved identiske filtre. |
| Publicering og Shiny | RDS-version 2 publiceres atomisk. Shiny-læseren læser også version 1 som Hele Danmark. Ét regionsvalg med Hele Danmark som standard styrer forbrug, tilskud, sammenligninger, dæknings- og revisionsvisning, prisscenarier, prognoser og CSV. Selve takstopslaget er fælles. |
| Regression og drift | Regressionstestene dækker migration, import og genimport, afstemning, dækningshuller, prognoser, publicering og filteret i Shiny. Mål publiceringens størrelse og køretid i piloten før bredt backfill. |

## Lille eksport- og importpilot

Indlæs den opdaterede eksportør i rapportens konsol. Kør nedenstående og skift derefter ét rapportfilter. Vælg alternativt **Fem bopælsregioner** under **Geografi** i `Monitorering.panel()`. Ét grupperet kald giver et CSV/manifest-par med endelsen `_regioner`. National eksport er fortsat standard.

```js
Monitorering.start({ prefixes: ["N06BA04"], from: "2025-01", to: "2025-01", regional: true })
```

| Kode | Geografi |
| --- | --- |
| `DK` | Hele Danmark |
| `H` | Region Hovedstaden |
| `SJ` | Region Sjælland |
| `SD` | Region Syddanmark |
| `MJ` | Region Midtjylland |
| `NJ` | Region Nordjylland |

Kør følgende fra branchens checkout med `MONITOR_RUNTIME` sat til en testmappe med en ny eller gendannet testdatabase. `MONITOR_DB`, `MONITOR_ARCHIVE` og `MONITOR_LOG` sættes eksplicit for at holde testkørslen på samme afgrænsning. Sæt `PILOT_INBOX` til en mappe med det regionale filpar og eventuelt det nationale referencepar.

```bash
export MONITOR_RUNTIME="$(mktemp -d)"
export MONITOR_DB="$MONITOR_RUNTIME/monitorering.duckdb"
export MONITOR_ARCHIVE="$MONITOR_RUNTIME/archive"
export MONITOR_LOG="$MONITOR_RUNTIME/events.jsonl"

Rscript monitorering/scripts/monitor.R init
Rscript monitorering/scripts/import_and_publish.R "${PILOT_INBOX:?Angiv mappen med pilotens filpar}" "$MONITOR_RUNTIME/analysis.rds"
Rscript monitorering/scripts/monitor.R coverage N06BA04 2025-01 2025-01 MJ
Rscript monitorering/scripts/publication_metrics.R "$MONITOR_RUNTIME/analysis.rds" N06BA04 MJ
MONITOR_PUBLICATION="$MONITOR_RUNTIME/analysis.rds" Rscript -e 'shiny::runApp("monitorering-app")'
```

Kontrollér, at Region Midtjylland viser de regionale pilotværdier, at Hele Danmark viser det nationale referencepar, hvis det er importeret, og at februar fremstår uden målinger. Ét enkelt månedspunkt giver ingen prognose. Ved senere godkendt udrulning skal den opdaterede Shiny-læser være på plads, før første publicering i RDS-version 2. Bevar en backup af databasen og den aktive publicering før migration.
