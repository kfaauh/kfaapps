# Medicinsalg · Power BI

Shiny-app til at generere browserudtræk fra Medicinsalg og samle data til CSV/Excel.

## Installation og lokal afprøvning

**Appen kræver kun `app.R`.** JavaScript er indbygget i filen. Mappestrukturen på Shiny-serveren skal derfor ikke ændres: behold `powerbi/app.R`. Der skal ikke ligge CSV-, JSON- eller CJS-filer på serveren for at starte appen.

R-pakker: `shiny`, `bslib`, `jsonlite`, `dplyr`, `tidyr`, `stringr`, `purrr`, `readr`, `DT`, `writexl`, `readxl`. R skal være mindst 4.0.

```r
install.packages(c("shiny", "bslib", "jsonlite", "dplyr", "tidyr",
                   "stringr", "purrr", "readr", "DT", "writexl"))
shiny::runApp(file.choose(), launch.browser = TRUE)
```

Vælg den downloadede R-fil i dialogen. Samme fil kan installeres som `app.R` på Shiny Server; filen slutter med et Shiny-appobjekt og starter ikke en indlejret `runApp()`-proces.


## Færdige Excel-filer og Taksten (2026-10-29)

- Appen kan indlæse både rå semikolonseparerede browser-CSV'er og færdige XLSX-tabeller med 12 (ATC5) eller 17 (varenummer) kolonner. Den forsøger at vælge salgstabellen frem for metadataarket \`Udtræk\`.
- De to niveauer må ikke blandes i samme import. Overlappende observationer afvises; der aggregeres aldrig stiltiende på identiske nøgler.
- Varenummerformatet bevares som seks tegn, herunder foranstillede nuller.
- Med en valgfri upload af Taksten (.xlsx) kobles ESP, TSP, pris pr. DDD og substitutionsgruppe, i det omfang standardkolonnerne findes. Dækning og eventuelle ATC-afvigelser vises. En Takst-pris er prisen fra det uploadede Takst-øjebliksbillede og **må ikke** bruges som et estimat for historisk ekspeditionsbeløb.
- KPI-kort, ATC-/regions-/årsfiltre og månedlig udviklingskurve i absolutte nøgletal vises efter import. Eksporteret Excel og CSV følger aktive filtre.
- \`readxl\` kræves nu på Shiny-serveren. Bibliotekerne til Power BI-generatoren og JavaScript-templaten er uændrede.
- Kontroller særskilt mod de aktuelle eksempelfiler: \`medicinsalg_20261029_eksempel100.xlsx\` og \`medicinsalg_20261029_eksempelatc5.xlsx\`. Testsættet indeholder syntetiske Excel- og Takst-fixtures, men kræver et R-miljø for at køre.

## Arbejdsgang

1. Vælg ATC-koder, år, regioner og niveau i appen.
2. Åbn Medicinsalg i Sundhedsdatabanken. Vælg `app.powerbi.com` som frame i DevTools Console.
3. Kør den genererede kode. Ændr derefter ét filter i rapporten. Koden opsamler Authorization fra rapportens næste querydata-kald via fetch eller XMLHttpRequest. Tokenet lever kun i udtrækkets hukommelse; det gemmes ikke i filer eller sendes til Shiny. Observerne fjernes efter opsamling, afbrydelse eller timeout.
4. Hvis automatisk opsamling ikke virker, kopier Authorization fra et fungerende querydata-kald i Network og kør `medicinsalgExport.authorize("HELE_HEADEREN")` i Console, mens koden venter. Ventetiden er 2 minutter. Ved 401/403 køres koden igen med et aktuelt token. Power BI-tokens udløber og kan ikke hardcodes permanent.
5. Vent på Færdig. Browseren downloader CSV samt en oversigt i JSON. CSV vises ofte med Excel-ikon i Windows. Store udtræk deles ved 50.000 rækker. Ingen CSV downloades, før alle kald er lykkedes.
6. Indlæs datafilen (eller alle dele) i appen. Oversigten er valgfri, men gør det muligt at kontrollere manglende dele og se tomme deludtræk. Hent en samlet Excel-fil med nøgletallene i separate kolonner.

Stop med `medicinsalgExport.stop()`. Alle kald udføres sekventielt, med 60 sekunders timeout per kald. API-fejl, RestartToken og rækkegrænse stopper eksporten. Tomme svar er ikke nulværdier.

## Data og udseende

- ATC-kode og ATC-tekst eksporteres i to kolonner. Tidligere CSV med den kombinerede ATC-kolonne kan stadig indlæses.
- Varer bevarer deres sekscifrede varenummer, inklusive foranstillede nuller. Dekoderen håndterer både ordbogsindeks og direkte tekstceller, også når schemaet angiver en tom ordbog.
- Manglende værdier og reelle nulværdier holdes adskilt. Overlappende data afvises, og værdier pr. 1.000 summeres ikke. Måneder sorteres kronologisk.
- Temaet følger homepage og lister: lys grå baggrund, hvide kort, blå accent, systemskrifter og afrundede kontroller.

## Udvikling og kontrol

`export_template.js` er en læsbar udviklingskopi. Runtime læser den ikke. Ved ændring skal indholdet også opdateres i R-variablen `.medicinsalg_js_template`; R-testen kontrollerer, at kopierne er ens.

`tests/` og `tests/fixtures/` er kun udviklingsfiler og behøver ikke kopieres til serveren. Fixtures indeholder kunstige måleværdier. Der er ikke indsat brugerens Authorization eller medicinsalgsudtræk.

Kør fra `powerbi/`:

```sh
node tests/export.test.cjs
Rscript tests/import.test.R
# Valgfrit lokalt accepttjek med en privat CSV, som ikke committes:
Rscript tests/import.test.R /sti/til/medicinsalg.csv
```

Kontrollerne omfatter direkte tekst i varefelter, ordbøger, null/gentagelsesmasker, ATC-opdeling, tokenopsamling via fetch/XHR, manuel fallback, oprydning ved stop/timeout, fejlstop, import og Excel-skrivning. Varenummerrettelsen og tokenopsamlingen skal desuden afprøves mod den aktuelle rapport i brugerens browser; testene kalder ikke Power BI.
