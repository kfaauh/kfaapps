# Monitorering — historisk datagrundlag

Implementerbart R/DuckDB-datalag til nationale månedsdata og takstsnapshots. Ingen Shiny-app eller ændring af eksisterende apps. Start med den medfølgende Test 8: N06, januar 2025, 1.143 målingsrækker, 25.198.980 DDD og 80.632.838 kr. i faktisk regionalt tilskud. Koden indeholder ikke adgangstokens eller takstdata.

## 1. Installation på Ubuntu 24.04

Kommandoerne køres fra repositoryets rod. Der foretages ingen installation på serveren, før du selv kører dem. Eksemplet bruger `/opt/kfaapps`; tilpas det til din faktiske checkout. Brug den eksisterende R 4.6.1. Node >=18 bruges kun til offline-tests; browserkoden kræver ingen npm-pakker.

```bash
git fetch origin
git switch monitorering/historical-data-infrastructure
sudo apt-get update
sudo apt-get install -y build-essential libcurl4-openssl-dev libssl-dev libxml2-dev
Rscript monitorering/scripts/install.R
node --test monitorering/tests/export.test.cjs
Rscript monitorering/tests/integration.R
bash monitorering/tests/cli.sh
```

`install.R` bruger en Linux-binærpakke-kilde anbefalet af DuckDB, med CRAN som fallback. Hvis din R-version endnu ikke har en binær DuckDB-pakke, kan kildekompilering tage tid og kræve flere GB RAM. Gem de udskrevne pakkeversioner i dit driftsmiljø. Test ved opgradering og tag backup først; bland ikke DuckDB-versioner på samme fil. Den lokale verificerede kombination står i [STATUS.md](docs/STATUS.md); CI tester også R 4.6.1.

Opret evt. en separat driftskonto og mapper:

```bash
sudo useradd --system --home-dir /var/lib/monitorering --create-home --shell /usr/sbin/nologin monitorering
sudo install -d -o monitorering -g monitorering -m 0750 /var/lib/monitorering/inbox /var/log/monitorering /var/backups/monitorering
sudo install -m 0640 -o root -g monitorering monitorering/config/monitorering.env.example /etc/monitorering.env
```

R-pakker og kode skal være læsbare for driftskontoen. Installer om nødvendigt pakker i en særskilt, læsbar site-library; systemd har `ProtectHome=true`, så din personlige R-library under hjemmemappen er ikke tilgængelig. Kontroller med `sudo -u monitorering Rscript -e 'stopifnot(requireNamespace("duckdb",quietly=TRUE))'`.

Til manuel brug i din shell:

```bash
export MONITOR_RUNTIME=/var/lib/monitorering
export MONITOR_DB=/var/lib/monitorering/monitorering.duckdb
export MONITOR_ARCHIVE=/var/lib/monitorering/archive
export MONITOR_LOG=/var/log/monitorering/events.jsonl
```

Kør med en konto, der ejer mapperne. Uden miljøvariable bruges den git-ignorerede `monitorering/runtime/`. Der bruges ingen hardcodede serverstier i R-koden.

## 2. Første import og kontrol

```bash
Rscript monitorering/scripts/monitor.R init
Rscript monitorering/scripts/monitor.R import-consumption \
  monitorering/tests/fixtures/monitorering_forbrug_N06_2025-01.csv \
  monitorering/tests/fixtures/monitorering_manifest_N06_2025-01.json
Rscript monitorering/scripts/monitor.R validate
Rscript monitorering/scripts/monitor.R coverage N06 2025-01 2025-12
Rscript monitorering/scripts/monitor.R export v_consumption /tmp/monitorering_forbrug.csv
Rscript monitorering/scripts/check_reference.R test8 \
  monitorering/tests/fixtures/monitorering_forbrug_N06_2025-01.csv
```

Gentagen import af samme filpar ændrer ikke databasen. En ny revision beholder tidligere observationer. Filer valideres før en transaktion registrerer dem som fuldførte. Rå CSV/manifest arkiveres uforanderligt med SHA-256. Eksporten nægter at overskrive en eksisterende fil.

`coverage` viser januar som `query_complete_population_unverified`; de øvrige måneder mangler. Dette er bevidst: en godkendt forespørgselsrespons beviser ikke fuld kildepopulationsdækning. `partial_subscopes` betyder, at kun dele af den ønskede ATC-afgrænsning er hentet. Et komplet sæt af alle næste-niveau-præfikser, inkl. validerede tomme svar, kan tilsammen dække en bredere afgrænsning. `attest EXTRACT_ID EVIDENCE_FILE` kan registrere en dokumenteret ekstern kontrol; brug ikke importens egne summer som uafhængigt dækningsbevis.

## 3. Nye forbrugsudtræk i browseren

1. Åbn den eksisterende Medicinsalg-rapport, og vælg `app.powerbi.com` som Console-frame i udviklerværktøjerne.
2. Indsæt hele [browser/monitorering.js](browser/monitorering.js) i konsollen. Koden indlæser et API og starter ikke selv requests.
3. Start en lille, sekventiel batch:

```javascript
await Monitorering.start({prefixes:["N06"],from:"2025-02",to:"2025-03"});
```

4. Ændr ét filter i rapporten for at opsamle en aktuel Authorization-header. Manuel fallback: `monitoreringCollector.authorize(header)` med hele headeren. Den findes kun i hukommelsen og slettes ved afslutning. Brug kun din normale, autoriserede rapportadgang.
5. Tillad flere downloads for rapporten, og kontroller, at hvert CSV/manifest-par er gemt. Det er browseren, der bestemmer, om et tilbudt download faktisk skrives til disk.

Præfikser kan være ATC1–5 (`N`, `N06`, `N06B`, `N06BA`, `N06BA04`). Overlappende præfikser i samme plan afvises. Hver måned hentes separat. Månedstekster antages danske (`Jan`, `Feb`, `Mar`, `Apr`, `Maj`, `Jun`, `Jul`, `Aug`, `Sep`, `Okt`, `Nov`, `Dec`); ændr `monthLabels:[...]`, hvis rapportmodellen viser andre etiketter. Svaret valideres mod præcis den forespurgte måned. Kun januar er valideret mod et leveret live-output i denne udvikling.

Standard: minimum 2,5 sekunders pause, højst 60 requests pr. kørsel, op til to ekstra forsøg ved netværksfejl/429/5xx og 60 sekunders timeout. 401/403, modelændringer og datakvalitetsfejl stopper straks. `Retry-After` respekteres; over ét minut stoppes batchen til senere genoptagelse. Alle kald er sekventielle.

Ved `RT`/`RestartToken`, rækkegrænsen eller en samlet respons på mindst 30.000 rækker kasseres delsvaret. Præfikset erstattes med **alle** næste-niveau-præfikser (26 bogstaver eller 100 tocifrede værdier). Det giver ikke overlap. Ugyldige/tomme undergrupper kræver stadig et valideret tomt svar. Dette kan bruge mange requests: behold budgettet og start smalt. En afkortet ATC5/måned stopper; der implementeres ikke en udokumenteret continuation-token-binding. En model, som afviser en tom undergruppe, stopper sikkert til manuel planlægning.

Stop med `monitoreringCollector.stop()`. Checkpoint tilbydes som download ved afslutning/fejl; undervejs kan du køre `monitoreringCollector.checkpoint()`. Genoptag efter at have kontrolleret downloadede filer:

```javascript
// Indsæt det læste checkpoint-objekt som checkpoint.
await Monitorering.start({resume:checkpoint});
```

Kun `pending` genoptages; checkpointet har ingen tokens. Hvis et tilbudt download ikke blev gemt, tilføj den pågældende opgave til `pending` eller kør den igen. Idempotent serverimport forhindrer dobbelttælling. Et checkpoint dokumenterer fuldførte browseropgaver, ikke en kvittering for filskrivning eller fuld kildepopulationsdækning.

## 4. Automatisk serverimport

Overfør færdige filpar til inbox. Brug midlertidige navne under overførsel, og omdøb manifestet til det endelige navn **sidst**, når CSV-filen er klar:

```bash
Rscript monitorering/scripts/monitor.R import-inbox /var/lib/monitorering/inbox
Rscript monitorering/scripts/monitor.R validate
```

Navne: `monitorering_forbrug_<PREFIX>_<YYYY-MM>.csv` og `monitorering_manifest_<PREFIX>_<YYYY-MM>.json`. En fil med browsertilføjelsen `(1)` skal omdøbes konsekvent; importer altid CSV og det tilsvarende manifest. `import-consumption` kan bruge vilkårlige navne. Flyt ældre inbox-par efter behov; rådata ligger også i arkivet. En fejlbehæftet fil stopper ikke andre filpar, men hele jobbet returnerer status 1. Se JSONL-loggen og ret/udtag den afviste fil før næste kørsel.

Systemd-eksemplet importerer kun lokale filer; det logger ikke ind i PowerBI og foretager ikke eksterne requests:

```bash
sudo install -m 0644 monitorering/systemd/monitorering-import.service /etc/systemd/system/
sudo install -m 0644 monitorering/systemd/monitorering-import.timer /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl start monitorering-import.service
sudo journalctl -u monitorering-import.service --no-pager -n 60
sudo systemctl enable --now monitorering-import.timer
```

Tilpas `/opt/kfaapps` og Rscript-stien i servicefilen først. Alternativ cron under driftskontoen: `30 4 * * * . /etc/monitorering.env; export MONITOR_RUNTIME MONITOR_DB MONITOR_ARCHIVE MONITOR_LOG; cd /opt/kfaapps && /usr/bin/Rscript monitorering/scripts/monitor.R import-inbox /var/lib/monitorering/inbox`. Brug én scheduler.

## 5. Historisering af taksten

Genbrug allerede hentet `Taksten_YYYY-MM-DD.xlsx` (foretrukket), `clean_data2.rds` eller normaliseret semikolon-CSV. Hver fil kræver et sidecar-JSON efter [config/tariff.metadata.example.json](config/tariff.metadata.example.json). Datoen i filnavnet er ikke tilstrækkeligt bevis for gyldighedsperioden. Eksempelfilen må ikke bruges uændret til reelle data.

Obligatorisk metadata: `schema_version:1`, `source`, UTC `observed_at`, faktisk `nominal_from`/`nominal_to`, `validity_kind` og `complete`. Undtagelsen er ældre arkiver med dokumenteret ukendt periode; se afsnittet nederst. Ved `complete:true` kræves `completeness_evidence`. Standard er `validity_kind:"observed_only"`, uden effective-datoer. Til dokumenterede kilder med faktiske effektive priser bruges `"source_interval"` med `effective_from`, `effective_to` og `validity_evidence`; et dagsudtræk kan have samme fra/til-dato. Tilskudspriser kan skifte inden for en takstperiode, så udvid aldrig intervallet på grundlag af prisperiodens datoer alene.

```bash
Rscript monitorering/scripts/monitor.R import-tariff \
  /var/lib/monitorering/prisinput/Taksten_2026-10-30.xlsx \
  /var/lib/monitorering/prisinput/takst.metadata.json
Rscript monitorering/scripts/monitor.R export v_tariff_changes /tmp/takstaendringer.csv
# Sammenlign vilkårlige snapshots med deres ID fra tariff_snapshot:
Rscript monitorering/scripts/monitor.R compare-tariff CURRENT_SNAPSHOT_ID PREVIOUS_SNAPSHOT_ID
```

Normaliseret CSV accepterer `varenummer;atc;name;strength;form;pack;esp;tsp;ddd_per_pack;ddd_provenance` samt valgfrie substitutions-, leverings- og markedsføringsfelter. Rå Excel og clean RDS mappes efter de eksisterende kolonnenavne. Ingen DDD beregnes fra ESP divideret med en afrundet ESP/DDD. For normaliserede positive DDD-værdier kræves `ddd_provenance=source|verified` og `ddd_evidence` i metadata. Ukendte DDD giver manglende TSP/DDD. Originale kildefelter og kvalitetsflag bevares.

Opt-in genbrug af eksisterende fetch (erstatter en planlagt fetch-kørsel; kør ikke begge):

```bash
export MEDPRIS_EMAIL='din-autoriserede-konto'
read -r -s -p 'Medicinpriser password: ' MEDPRIS_PASSWORD
export MEDPRIS_PASSWORD
Rscript monitorering/scripts/archive_existing_fetch.R \
  /var/lib/monitorering/prisinput \
  '/opt/kfaapps/data-scripts/Alle lægemidler.xlsx' \
  /var/lib/monitorering/prisinput/takst.metadata.json
unset MEDPRIS_PASSWORD MEDPRIS_EMAIL
```

Wrapperen genbruger den eksisterende kode én gang og arkiverer workbooken uden ekstra prisrequests. Indlejrede credentials i det gamle script erstattes i hukommelsen af eksplicitte miljøvariable; de genbruges ikke. Hvis grupper springes over, markeres snapshotet ufuldstændigt. Allerede hentede raw-filer kan stadig importeres, hvis wrapperens periodetjek afviser. Wrapperens live-login/fetch skal verificeres på din autoriserede server før planlægning. Den leverer `observed_only`-snapshots og rekonstruerer ikke historiske dagspriser.

## 6. Backup, gendannelse og drift

```bash
Rscript monitorering/scripts/monitor.R backup /var/backups/monitorering
# Gendan til NYE, ikke-eksisterende DB-/arkivplaceringer:
MONITOR_DB=/var/lib/monitorering-restore/monitorering.duckdb \
MONITOR_ARCHIVE=/var/lib/monitorering-restore/archive \
Rscript monitorering/scripts/monitor.R restore \
  /var/backups/monitorering/monitorering-20261030T120000Z
```

Backup indeholder database, råarkiv og checksums. Brug den faktiske returnerede backupsti i restore-kommandoen. Filkopiering af en aktiv database uden checkpoint er ikke en backup. Opgaver fejler ved samtidig skrivelås og kan køres igen, når den anden proces er afsluttet. Fjern aldrig lock-filen som løsning på en aktiv lås. Automatisér evt. backup i et separat vedligeholdelsesvindue; der slettes ikke automatisk backups.

Rå takster, databaser, inbox, logs, backups og credentials skal ligge i git-ignorerede runtime-mapper eller uden for repositoryet. Kun offentlige aggregerede Test 8-forbrugsfixtures er med i git. Produktionsscripts foretager ikke databaseændringer ved importfejl; afbrudt arkivering kan efterlade en ufarlig fil uden referencer.

En senere Shiny-app kan forespørge SQL-visninger via DBI/DuckDB. Brug en publiceret read-only kopi eller koordinér korte forbindelser med opdateringer: et separat Shiny-process med en permanent åben databaseforbindelse kan blokere den eksterne writer.

Se [datamodel og beregningskontrakt](docs/ARCHITECTURE.md), [adgangsforhold og kilder](docs/SOURCES.md) og [teknisk status](docs/STATUS.md).

## Arkiverede takstfiler uden kendt prisperiode

Tag først backup med den gamle kode. Efter opdatering kør `monitor.R init` for den transaktionelle opgradering til databaseskema 2. Eksisterende forbrugs- og taksthistorik bevares. Herefter:

```bash
Rscript monitorering/scripts/import_archived_tariff.R /sti/til/Taksten_2026-08-06.xlsx
```

Kommandoen læser originalfilen uden at ændre den og arkiverer priser og alle råfelter i databasen. Nominel og effektiv gyldighed forbliver NULL. Filens ændringstid bruges kun som tydeligt mærket proxy for indsamlingstid, ikke som verificeret prisgyldighed. Fuldstændighed er ukendt (`complete=false`), så disse arkiver indgår ikke i automatiske historiske eller aktuelle prisberegninger. Gentagen import af samme uændrede fil er idempotent. De kan senere gennemgås og suppleres med dokumenteret kildeinformation. Filnavnets dato bruges aldrig til at gætte en takstperiode.

## Samlet dagligt prisjob

Se [DAILY_TARIFF.md](docs/DAILY_TARIFF.md) for den nye strenge indsamler, manuel serviceprøve og efterfølgende aktivering. Den gamle opt-in-wrapper bevares til fejlsøgning; den er ikke det daglige job.
