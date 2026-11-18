# Bopælsregioner i Monitorering

## Liveaccept

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

## Forventede ændringer efter liveaccept

| Led | Ændring |
| --- | --- |
| Eksport og import | Version 3 med eksplicit region på rækkerne, deklareret geografisk forespørgselsscope og kontroller pr. måling og region. Version 1 og 2 importeres fortsat som Hele Danmark. Kildens regionale navne mappes til faste koder. |
| DuckDB og SQL | Migrér eksisterende scopes og observationer transaktionelt til `DK`. Bevar id'er, rækkefølge, tidsstempler, arkiver og attestationer. Udvid aktuel revision, ATC-aggregater og priskoblinger med region. Et nationalt udtræk og regionale udtræk kan eksistere for samme periode og ATC uden at påvirke hinandens revisioner. |
| Dækning og afstemning | Registrér dækning pr. region, måned og ATC-scope. Håndtér komplette tomme svar særskilt fra manglende scopes. Manglende målinger forbliver ukendte. Afstem de fem regioner med det nationale udtræk for samme ATC, måned, måling, filtre og kildetid, både samlet og på varenummerniveau hvor observationerne kan sammenlignes. |
| Publicering og Shiny | Publicér regionsmærkede data atomisk. Gør det muligt for den nye Shiny-læser at læse eksisterende RDS-version 1 som national. Anvend ét regionsvalg med Hele Danmark som standard på forbrug, tilskud, sammenligninger, dæknings- og revisionsvisning, prisscenarier, prognoser og CSV. Selve takstopslaget er fælles. |
| Regression og drift | Test migration og huller i dækning, import og genimport af gamle og nye udtræk, regionssummer, prognoseinput, atomisk publicering og filterets gennemslag i Shiny. Kør en afgrænset pilot og mål publiceringens størrelse og køretid før bredt backfill. |
