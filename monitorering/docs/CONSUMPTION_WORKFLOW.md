# Forbrug og Monitorering – første analyseversion

## Branch og afhængigheder

Arbejdet er baseret på PR #8, commit `4a38c93`, og indeholder desuden PR #7's DSR-rettelse og main med merged PR #9. PR #7 og #8 var åbne ved gennemgangen 30. oktober 2026. Ingen merge af disse PR'er eller ændringer i produktion er foretaget af dette arbejde. Takstjobbet er uændret.

Det tidligere `Monitorering_shiny_app.zip` fra 29. oktober er gennemgået. Det var et selvstændigt Excel-uploadudkast uden DuckDB-import/revisionshistorik. Det afledte DDD pr. pakning fra ESP / afrundet pris pr. DDD og brugte filnavne/mtime som prisdatoer. Desuden kunne `sum(..., na.rm=TRUE)` gøre manglende tilskud til nul i aggregater. Derfor genbruges dets faglige skel mellem faktisk tilskud og prisgrundlag, men hverken import-, pris- eller prognosekoden. Den gamle forecast-branch indeholder ingen separat Monitorering-app.

Eksisterende eksport, revisionsregler og SQL-koblinger genbruges. En komplet nyere afgrænsning erstatter hele den relevante ATC/måned, også observationer der forsvinder. Bredere og smallere udtræk løses efter kildetid; de summeres ikke som nye observationer. Uafhængigt populationsbevis er fortsat en separat handling.

## 1. Lille browserprøve

1. Åbn den sædvanlige Medicinsalg-rapport med din autoriserede adgang. Vælg `app.powerbi.com` som Console-frame.
2. Indsæt `monitorering/browser/monitorering.js` i konsollen. Koden starter ikke netværkskald.
3. Kør `Monitorering.panel()`.
4. Behold N06, januar 2025, og tryk **Start udtræk**. Ændr derefter ét rapportfilter for at opsamle en aktuel header. Panelet kan lukkes efter stoppet kørsel. Ingen header skal kopieres til chat eller fil.
5. Gem både CSV og manifest fra panelet. Knapperne kan bruges igen, hvis et download blokeres. Filerne holdes kun i panelets hukommelse, ikke i browserens permanente lager.
6. Gem checkpointet. Kontroller selv, at alle færdige filpar er på disk, før det bruges til genoptagelse. Checkpoint betyder færdig forespørgsel, ikke bekræftet filskrivning. Ved mistet filpar køres den lille afgrænsning igen.

Forventet oprindelig Test 8-reference: 1.143 rækker, 576 DDD-målinger, 567 tilskudsmålinger, 25.198.980 DDD og 80.632.838 kr. Den reelle kilde kan være revideret siden referenceudtrækket. Kontroller med `check_reference.R test8 CSV`; en difference skal undersøges og registreres, ikke accepteres som afrunding uden dokumentation.

Panelets fremdrift viser requests og udestående/færdige opgaver. Tokenudløb stopper; genoptag checkpoint og opsaml en ny header. Højst 60 requests pr. kørsel og mindst 2 sekunder mellem requests (standard 2,5). Kun midlertidige netværksfejl/429/5xx genforsøges. Rækkegrænse/RestartToken medfører disjunkt opsplitning; en afkortet ATC5/måned stopper. Der er ingen udokumenteret token-pagination. ATC1–5 understøttes af querygeneratoren, men andre afgrænsninger/måneder end den oprindelige januarfixture skal livevalideres gradvist.

## 2. Upload og kontrolleret import

Upload er filoverførsel under din eksisterende SSH-adgang. Der er ingen offentlig skrivefunktion i Shiny. Overfør CSV først og manifest som `.uploading`; omdøb manifestet sidst. Behold filnavnene som i panelet; browserens `(1)`-suffiks kræver konsekvent omdøbning af begge filer.

Efter den udtrykkeligt godkendte installation og kontrol af datafilrettigheder køres på serveren:

```bash
cd /home/au323402/monitorering-kfaapps
export R_LIBS_USER=/home/au323402/monitorering-r-library
export MONITOR_RUNTIME=/home/au323402/monitorering-data
export MONITOR_DB="$MONITOR_RUNTIME/monitorering.duckdb"
export MONITOR_ARCHIVE="$MONITOR_RUNTIME/archive"
export MONITOR_LOG="$MONITOR_RUNTIME/events.jsonl"
/usr/bin/Rscript monitorering/scripts/import_and_publish.R \
  "$MONITOR_RUNTIME/inbox" /var/lib/monitorering-public/analysis.rds
/usr/bin/Rscript monitorering/scripts/monitor.R coverage N06 2025-01 2025-03
```

Forventet: `Published: ...`, januar med `query_complete_population_unverified`, øvrige måneder `missing`, medmindre allerede indlæst. En kopi af samme uændrede filpar ændrer ikke aktuelle tal. Ved fejl: stop og undersøg den konkrete meddelelse. Succesfulde par i samme inbox-job kan være committed i databasen, men jobfejl publicerer intet nyt; tidligere appfil bevares. En afbrudt browserkørsel kan have færdige, selvstændige måned/par, som godt må importeres.

Filoverførsel fra egen computer kan fx bruge `scp -o ProxyJump=BRUGER@ssh.au.dk FIL BRUGER@kfaapps.uni.au.dk:/home/au323402/monitorering-data/inbox/`. Brug din normale godkendte SSH-konfiguration; ingen credentials skal gemmes i scripts. Omdøb på serveren via SSH, når overførslen er færdig. En konkret Windows-sti afhænger af din downloadmappe.

## 3. Installation af læsende app – kræver godkendelse før udførelse

Denne vejledning er til efter kodegennemgang/PR-accept. Kør ikke en bred opdatering af produktionscheckout for at installere den nye app. Arbejdsbranch skal først være hentet til udviklingscheckout; eksisterende takstservice kan bruge dette checkout og skal derfor testes ved en senere godkendt branchopdatering. Bevar installeret service og credentials.

Installér kun de nye UI-pakker i det eksisterende bibliotek, hvis de mangler:

```bash
R_LIBS_USER=/home/au323402/monitorering-r-library /usr/bin/Rscript -e \
 'p <- c("shiny","bslib","DT"); p <- p[!vapply(p, requireNamespace, logical(1), quietly=TRUE)]; if(length(p)) install.packages(p, repos="https://cloud.r-project.org")'
```

Operatør ejer publiceringsmappen; Shiny må læse, men ikke skrive. `setgid` sikrer gruppe på nye midlertidige filer. Ingen læseadgang til hjemmemappens DB er nødvendig:

```bash
sudo install -d -o au323402 -g shiny -m 2750 /var/lib/monitorering-public
sudo install -d -o root -g root -m 0755 /srv/shiny-server/monitorering
sudo install -o root -g root -m 0644 monitorering-app/app.R monitorering-app/analysis.R /srv/shiny-server/monitorering/
```

Standardplaceringen i appen er `/var/lib/monitorering-public/analysis.rds`. `MONITOR_PUBLICATION` kan angive en anden sti, hvis Shiny-processen får variablen. Appen kræver ikke DBI/DuckDB. UI-pakkerne skal findes i Shinys pakkesti, fx den allerede etablerede site-library; en personlig library i en anden brugers hjem bliver ikke automatisk læst. Kontroller før start:

```bash
sudo -u shiny env R_LIBS_USER=/home/au323402/monitorering-r-library /usr/bin/Rscript -e \
 'stopifnot(all(vapply(c("shiny","bslib","DT"),requireNamespace,logical(1),quietly=TRUE))); x <- readRDS("/var/lib/monitorering-public/analysis.rds"); stopifnot(x$version==1L); print(x$published_at)'
```

Åbn `/monitorering/` og bekræft at appen faktisk starter, at N06/januar viser referencesummerne (eller dokumenteret revision), og at februar vises manglende. Kontroller også `/kfaapps/lmgrupper2/` på dens faktiske URL efter installation. Se Shiny-log ved fejl; filtilstedeværelse er ikke en accepttest af appopstart. Eksisterende homepage ændres ikke.

Appen genlæser filen hvert 30. sekund og bevarer seneste validerede objekt ved læsefejl. Ny session uden læsbar fil viser ventestatus. Takstjobbet kan nu genpublicere analysefilen med `MONITOR_PUBLICATION` og eksplicit skriveret i systemd; se `OBSERVATIONS_UPDATE.md`. Indtil det er aktiveret, kræves manuel `monitor.R publish-analysis /var/lib/monitorering-public/analysis.rds`. Datastatus viser derfor **senest indlæste/publicerede** takst og er ikke en kontrol af servicekvitteringen `last-tariff-success.json`.

Rollback af publiceret analysedata, under samme operatør og uden aktiv publicering:

```bash
cp /var/lib/monitorering-public/analysis.rds.previous /var/lib/monitorering-public/analysis.rollback
chmod 0640 /var/lib/monitorering-public/analysis.rollback
mv /var/lib/monitorering-public/analysis.rollback /var/lib/monitorering-public/analysis.rds
```

Dette ændrer kun appens publicerede analysefil, ikke kildehistorikken i databasen.

## Arkitektur og driftsgrænser

- `import_and_publish.R` holder den eksisterende writer-lås under import, validering og publicering. Det daglige takstjob bruger samme lås. Ved kollision fejler jobbet sikkert og skal genkøres, uden at en aktiv lås slettes.
- `publish_analysis()` læser alle analysevisninger inden for én DB-transaktion, skriver én midlertidig RDS ved destinationen, genlæser/validerer den og omdøber atomisk. Filtilstand 0640 sættes eksplicit med `use_umask=FALSE`. Forrige fil gemmes som `.previous`.
- Shiny åbner aldrig den levende DuckDB. Det undgår konflikter mellem en Shiny-læser og den separate R-writer. DuckDBs native flerprocesadgang har andre krav end forbindelser i samme proces: https://duckdb.org/docs/2.0/connect/concurrency.
- Appfilen indeholder kun valgte analysedata og driftsmetadata; ingen rå JSON, arkivstier, manifests eller tokens. Databasen og administrative scripts installeres ikke i appmappen.
- Første version læser analysefilen i hukommelsen og publicerer alle historiske tarifprodukter samt månedlige koblinger. Dette skal kapacitetsmåles før et nationalt flerårigt backfill; der loves ikke skalerbarhed uden måling. Ingen national backfill udføres automatisk.
- Appen summerer observerede værdier. Manglende målinger og måneder bliver ikke nul. Delvise summer markeres med antal manglende målinger og dækningsstatus; diagrammets orange ringe markerer delvise data. Teknisk komplet scope kan stadig have uverificeret population.
- Historisk kobling genbruger den daglige SQL-visning og kræver fuld måneds pris/DDD-dækning og ATC-match. Flere perioder giver én produkt/måned og ingen fordobling af DDD. Arkiver med ukendt gyldighed og observed-only-snapshots tilbagedateres ikke.
- Fremskrivningen kræver mindst januar til tre afsluttede måneder uden scope-huller eller manglende tilskudsmålinger; seneste tre måneders gennemsnit bruges til resten af året. Kun indeværende år og ATC-afgrænsning, ingen produktfremskrivning, sæsonmodel eller statistisk advarsel. Et udsnit stoppende tidligt i året skal fortolkes som fremskrivning fra dette valgte tidspunkt, ikke som opdateret budgetstatus.
- Et separat fremtidigt prisscenarie kræver mindst tre afsluttede, scope-komplette måneder fra januar. Hvert produkt kræver DDD i alle de seneste tre måneder og et egnet pris/DDD-match. Gennemsnittet × resterende måneder × senest observerede pris giver prisgrundlaget for resten af året. Forsvundne eller nye produkter uden tre målinger udelades eksplicit; de bliver ikke nul. Uændret pris og fortsat anvendelse er antagelser.
- Prisgrundlaget er observeret DDD × seneste observerede TSP/DDD på matchede produkter, med række- og DDD-dækning. Det er hverken faktisk tilskud eller en valideret fremtidsprognose. DDD-definition, observationsdato og fortsat prisgyldighed skal vurderes. Automatisk pris-/mængdeeffektdekomponering er udsat, til der findes dokumenterede priser og tilstrækkelig historik; ingen effekt tilskrives uden en defineret metode.

## Forsigtig backfill- og revisionspolitik

1. Reproducer N06/januar 2025 live og sammenlign med fixture; dokumenter differencer. Denne udvikling gennemfører ikke liveindsamlingen.
2. Prøv N06/februar og marts 2025. Kontroller månedsetiketter, kontrolsummer, filpar og samme tal i appen. Derefter ét ATC5/år og et moderat ATC-præfiks/kvartal. Vurder rapportens adgangsvilkår/kapacitet inden skalering.
3. Hent aftalte ATC-grupper måned for måned i afgrænsede batches. Gem checkpoints og optæl dækning; splits er selvstændige, disjunkte delscopes. Kør ikke nationalt backfill som første test.
4. Forslag til fremtidig politik: efter ny måneds udgivelse genhentes de seneste tre afsluttede måneder med samme afgrænsning. Kvartalsvis genkontrol af seneste 12 måneder, hvis kildekapacitet/vilkår tillader det. Tre og tolv måneder er driftsforslag, ikke dokumenterede kildefrister. Juster efter observerede revisioner og udgiverens oplysninger.
5. Sammenlign revisioner og bevar alle versioner. Træk først dækning, derefter aktuelle summer; historiske rå observationer må aldrig summeres direkte som aktuelt forbrug.

## Verifikation i udviklingsmiljøet

30. oktober 2026, Ubuntu 24.04, R 4.3.3 og DuckDB 2.0.0. Der er ikke gennemført test på serverens R 4.6.1 i dette miljø.

- Node-tests for Monitorering og den eksisterende PowerBI-app: bestået. Reelle Test 8-rækker er kørt gennem simuleret DSR og matcher 25.198.980 DDD og 80.632.838 kr.
- R-integration, historik/migration, CLI, backup/restore, writer-lås, fetch-evaluering og takstjob: bestået. Syntetiske pris- og revisionsdata er mærket som sådanne i tests; Test 4–7 er ikke reproduceret uden rå fixtures.
- Ny `publication.R`-test: bestået. Omfatter offentliggørelse under 0077-umask, bevaret tidligere fil efter fejl, særskilt læseproces under åben writer, dækningshuller, prognosebegrænsninger, syntetisk produktfremskrivning, Shiny-opstart/reactives og bevarelse af sidste objekt ved korrupt opdatering.
- `panel.cjs`: bestået i Chromium 153 med simuleret rapport/endepunkt og reel Test 8-fixture. Headeropsamling, gemmeknapper, kontrolsum og hemmelighedsfrie checkpoints afprøvet. Ingen live PowerBI-requests.
- `app_ui.cjs`: lokal HTTP/Shiny-opstart, alle faner, referencesummer, manglende februar og CSV-download er afprøvet i Chromium. Den udpakningsbaserede lokale Ubuntu-R-runtime krævede en lokal jquerylib-versionskorrektion, fordi dpkg-pakkeregisteret ikke kender de udpakkede pakker; denne korrektion er kun i teststarten og ændrer ikke appkoden. Visuel layoutkontrol udført. Screenshots er kun midlertidig QA.

Reproducer de almindelige tests fra repositoryets rod:

```bash
node --test monitorering/tests/export.test.cjs powerbi/tests/export.test.cjs
Rscript monitorering/tests/publication.R
Rscript monitorering/tests/integration.R
Rscript monitorering/tests/tariff_archive.R
Rscript monitorering/tests/fetch_eval.R
Rscript monitorering/tests/tariff_job.R
python3 monitorering/tests/test_tariff_admin.py
bash monitorering/tests/cli.sh
```

De to Chromium-tests kræver en separat Playwright-installation, en kompatibel Chromium og eventuelt `CHROMIUM_PATH`. `panel.cjs` kører uden livekilde. `app_ui.cjs` køres mod en lokalt startet app med Test 8 publiceret (`MONITOR_TEST_URL`, standard `http://127.0.0.1:3838/`). R-tests indgår i CI; browserprøverne er særskilte QA-scripts og installerer intet i produktion.

Manuel accept udestår: nyt N06/januar-udtræk fra den autoriserede rapport; filoverførsel/import i serverens udviklingsmiljø; godkendt appinstallation, kontrol som bruger `shiny`, faktisk HTTP-opstart og kontrol af den eksisterende prisapp. National backfill og automatisk publicering efter takstjobbet er ikke aktiveret.
