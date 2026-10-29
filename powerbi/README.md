# Medicinsalg: Power BI-udtræk og CSV-import

Appen genererer JavaScript til Sundhedsdatabankens Power BI-rapport og importerer den færdige CSV til en bred tabel med seks nøgletal og Excel-eksport.

## Kør appen

Behold `app.R` og `export_template.js` i samme mappe. `shiny::runApp("powerbi")` sætter arbejdsmappe korrekt. Pakkerne står øverst i `app.R`; denne revision kræver ingen nye R-pakker.

## Udtræk

1. Vælg ATC-koder, regioner, periode og ATC5-/varenummerniveau i appen.
2. Åbn [Medicinsalg](https://sundhedsdatabank.dk/medicin/medicinsalg-og-udgifter-menu/medicinsalg). Åbn DevTools → Network, filtrér på `querydata`, og ændr et filter i rapporten.
3. Fra et fungerende kald (HTTP 200) kopieres hele `Authorization`-headeren, inklusive præfikset.
4. Vælg `app.powerbi.com` i Console-kontekstmenuen. Kopiér generatorens kode til Console eller en lokal teksteditor, erstat `INDSÆT_AUTHORIZATION_HER`, og kør koden.
5. Vent på `Færdig`. Browseren downloader CSV-dele og en `*_summary.json` med alle forespørgsler, inklusive tomme svar. Tillad flere downloads, hvis browseren spørger.
6. CSV kan bruges direkte. Upload eventuelt alle CSV-dele og oversigten til Shiny for at samle nøgletal i kolonner og downloade Excel.

Authorization indtastes **ikke** i Shiny og gemmes ikke i generatoren på serveren, CSV eller oversigten. Brug en ny værdi fra rapporten ved HTTP 401/403. Stop en kørsel med `medicinsalgExport.stop()`; genindlæsning af rapporten stopper også koden. Én kørsel tillades ad gangen i den valgte frame.

Browserkoden bruger den verificerede Power BI Service-adresse, model `1212197`, dataset `1bebf468-3f1e-4596-8d94-c43c4cce0759` og rapport `870dd64d-7c9e-4e21-9711-270d8d9acb54`. Det er rapportens interne API, ikke en stabil, dokumenteret eksportkontrakt. En senere flytning kan kræve nye værdier i `export_template.js`.

## Parsing og dataintegritet

- Power BI DSR afkodes **kun i browseren**. R-parseren og importen af gamle rå JSON-udtræk er fjernet. JSON-oversigten indeholder kun metadata om udtrækket.
- Afkodningen følger `descriptor.Select`, rækkeskemaet `S`, kolonnens `DN`/`ValueDicts`, gentagelsesmasken `R` og nullmasken `N`. Ekstra formatkolonner som `M1` forbruges, men eksporteres ikke som målinger.
- API-/HTTP-fejl, ukendt skema, ugyldige ordbogsreferencer og afkortning stopper hele kørslen. CSV-filer downloades først efter alle datakald er lykkedes. Ved fejl downloades kun en oversigt markeret `complete: false`.
- CSV-dele har højst 50.000 rækker. De dannes som Blobber under kørslen og beholdes i browserhukommelsen indtil afslutning. Meget store udtræk bør stadig opdeles på flere ATC-valg/perioder.
- Shiny bevarer reelle nuller og manglende værdier hver for sig. Rækker med seks nuller beholdes. Ukendte eller manglende nøgletal bliver ikke til nul.
- CSV-import accepterer dansk decimalkomma, bevarer tekstidentifikatorer og sorterer måneder kronologisk. `Jan 2025` bliver `År = 2025`, `Måned = Januar`. Kildens ydertypebetegnelser bevares.
- Overlappende datanøgler afvises i stedet for at summere dem. Dette forhindrer blandt andet dobbelttælling af værdier pr. 1.000 borgere. ATC5- og varenummerniveau kan ikke blandes i samme import.
- Med en tilhørende oversigt kontrolleres eksport-ID, gennemførte kald og det samlede antal CSV-rækker. Uden oversigt kan tomme kald og manglende CSV-dele ikke identificeres; appen viser denne begrænsning.

## Validering

Kør fra mappen `powerbi`:

```sh
node tests/export.test.cjs
Rscript tests/import.test.R
# Valgfri kontrol mod det konkrete acceptudtræk (ikke en del af repository):
Rscript tests/import.test.R /sti/til/medicinsalg_atc5.csv
```

De medfølgende fixtures bruger **kunstige måleværdier**, som ikke er faktiske medicinsalgstal. Strukturen følger det observerede Power BI-svar: kolonnedefinitioner, tekstordbøger, gentagelsesmasker og en ekstra formateringskolonne. Testene kontrollerer hver af de 60 syntetiske DDD-værdier, nuller, manglende værdier, fejl/afkortning, stop, filopdeling, CSV-import, generatoren og Shiny-importhandlingen.

Et fuldt brugerleveret udtræk er desuden valideret lokalt. Brugerens rå svar og CSV er ikke inkluderet i repository. Det valgfri filargument kontrollerer et lokalt udtræk uden at gemme det i repository.

Varenummerforespørgslen er bevaret fra den tidligere app og dens afkodning er testet med en **syntetisk** fixture. Den er ikke afprøvet mod et levende varenummersvar efter flytningen. Kontrollér første varenummerudtræk mod rapporten.

## Installation på den eksisterende server

Deploy begge runtime-filer (`app.R` og `export_template.js`) samlet til den eksisterende `powerbi`-mappe. Ved Git-deployment skal ændringen først indgå i den branch, serveren henter. Denne revision ændrer ikke serverkonfigurationen og deployer ikke automatisk.
