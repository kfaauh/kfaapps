# Observationer, tilskudsestimat og næste indlæsninger

Denne ændring bygger på merged PR #11. Der er ingen ændring af databaseformat,
råhistorik, prisformel eller den strenge regels krav om målte produkttilskud.
Ingen produktionsinstallation udføres fra Work.

## Fortolkning i appen

- **Observeret tilskud:** sum af de kendte produktmålinger. NA udelades og
  tælles; en helt tom/all-NA måned forbliver NA. Helt uobserverede produkter kan
  ikke tælles i antallet af manglende produktmålinger.
- **Streng fremskrivning:** januar til seneste observerede afsluttede måned,
  mindst tre måneder uden scopehuller eller manglende produkttilskud. At denne
  tekniske regel er opfyldt dokumenterer ikke uafhængig populationsdækning.
- **Estimat fra observerede månedssummer:** samme sammenhængende månedsgrundlag,
  men tillader delvise produktsummer. Observeret sum plus gennemsnittet af de
  seneste tre måneder gange scenariemånederne. Antal manglende tilskudsmålinger
  vises for hele grundlaget og for de tre beregningsmåneder. Ingen sæsonkorrektion.
- **Prisgrundlag ved observeret DDD:** de observerede produktmængder værdisat
  ved senest observerede priser. Ikke faktisk tilskud eller årsprognose.
- **Produktbaseret prisscenarie:** seneste tre måneders produkt-DDD og seneste
  pris/DDD. Manglende produkter og priser udelades. Afledt DDD er et estimat.
  Oktoberpriser anvendt på august er kontrafaktisk, ikke dokumenteret historik.

Forespørgselsstatus, observationer og populationsattestation vises separat.
Senere tomme forespørgselsmåneder indgår ikke som datamåneder i nogen af de tre
fremskrivningsfunktioner. Interne månedshuller blokerer. Målte nuller bevares.
Scenarieperioden begynder efter sidste observerede afsluttede måned og slutter
31. december; den kan begynde før dags dato. Samme måneder året før vises som
observerede summer med manglende målinger; ændringer er ikke pris-/mængdeeffekter.
Søgning dækker indlæste ATC-koder og danske navne for et begrænset sæt almindelige
grupper. Der er endnu ikke et fuldt ATC-navnekatalog. Der er ingen offentlig upload.

## Kontrolleret udvidelse af forbrugsgrundlaget

Udtrækkene kræver stadig operatørens autoriserede rapportbrowser. Kolleger kan
analysere centralt publicerede data i den fælles app uden SSH eller DevTools.
Ingen token flyttes til Shiny. Understøttet serveradgang/bulkmetode og fuldt
ATC-navnekatalog er udestående; der indføres ikke automatisk browserlogin.

Følgende batches er foreslået; **ingen af dem er liveindsamlet af denne PR**:

| Trin | Afgrænsning | Måneder | Formål |
|---|---|---|---|
| 1 | N06 | oktober–december 2025, én måned først | Fuldt sammenligningsår; kontroller første måneds summer i rapporten |
| 2 | N06 | maj–juli 2026 | Første dokumenterede revision; behold gamle filpar og sammenlign før/efter |
| 3 | C09 | januar–marts 2025 og januar–marts 2026, ét år ad gangen | Afprøv ny gruppe med begrænset sammenligneligt grundlag |
| 4 | C09 | april–juli 2025 og 2026, højst tre måneder pr. kørsel | Udvid først efter godkendt måling af trin 3 |
| 5 | B01, A10 | én gruppe og én måned først, derefter som ovenfor | Fagligt relevante næste kandidater; afvent kapacitetsmålinger |

Brug eksisterende browserpanel med standardpause, sekventielle forespørgsler,
request-budget og checkpoint. Ingen parallel backfill. Et checkpoint er ikke
bevis for gemte filpar. Kontroller CSV+manifest før genoptagelse. Tomme måneder
må ikke præsenteres som observerede nuller. Juli 2026 er seneste manuelt bekræftede
kildemåned ved overdragelsen; en nyere kildemåned skal verificeres i rapporten.

Efter hvert batch: noter requests, varighed, bytes og valideret råsum. Tag mål
før og efter import med nedenstående script og `/usr/bin/time -v`. Noter også
oplevelsen ved skift af ATC/periode i Shiny (netværkssvartid og proces-RAM måles
separat; `object.size` er ikke procesmaksimum). Stop udvidelse ved fejl, uventede
summer eller mærkbar forringelse; undersøg før næste batch. Tallene er en baseline,
ikke en allerede fastlagt kapacitetsgrænse.

Revisionspolitik: efter en verificeret månedlig kildeudgivelse genhentes nyeste
måned samt de to foregående tilgængelige måneder for hver aktiv ATC-gruppe.
Kvartalsvis genhentes årets tilgængelige måneder i samme små batches.
Revisionshistorik bevares, og både nye, ændrede og bortfaldne observationer
kontrolleres. Dette er en operatørplan, ikke en ny timer. Population attesteres
kun med separat dokumenteret bevis.

Læsende baseline (kan køres før PR-installation; metrics-script kræver den nye kode):

```bash
set -euo pipefail
cd /home/au323402/monitorering-kfaapps
export R_LIBS_USER=/home/au323402/monitorering-r-library
export MONITOR_RUNTIME=/home/au323402/monitorering-data
export MONITOR_DB="$MONITOR_RUNTIME/monitorering.duckdb"
export MONITOR_ARCHIVE="$MONITOR_RUNTIME/archive"
export MONITOR_LOG="$MONITOR_RUNTIME/events.jsonl"
systemctl list-timers --all monitorering-priser.timer --no-pager
systemctl show monitorering-priser.service -p Result -p ExecMainStatus -p ExecMainStartTimestamp
sudo journalctl -u monitorering-priser.service -n 60 --no-pager
ls -lt "$MONITOR_RUNTIME/backups"
if test -f "$MONITOR_RUNTIME/last-tariff-success.json"; then
  /usr/bin/Rscript -e 'x<-jsonlite::read_json(file.path(Sys.getenv("MONITOR_RUNTIME"),"last-tariff-success.json")); print(x[c("status","time","snapshot_id","analysis_publication")])'
fi
/usr/bin/time -v /usr/bin/Rscript monitorering/scripts/publication_metrics.R \
  /var/lib/monitorering-public/analysis.rds N06
```

## Installation efter konkret aftale

Merge/installation er ikke autoriseret af denne PR. Følgende er til den senere
samlede opdatering. Det aktive takstjob bruger udviklingscheckoutet: opdater det
kun i en aftalt driftsperiode uden aktiv service/import. Bevar lokale ændringer;
stop ved ikke-rent checkout. Brug ikke reset/clean. Ingen geninstallation af pakker
eller erstatning af credentials er nødvendig.

Efter aftalt merge på serveren:

```bash
set -euo pipefail
cd /home/au323402/monitorering-kfaapps
export R_LIBS_USER=/home/au323402/monitorering-r-library
export MONITOR_RUNTIME=/home/au323402/monitorering-data
export MONITOR_DB="$MONITOR_RUNTIME/monitorering.duckdb"
export MONITOR_ARCHIVE="$MONITOR_RUNTIME/archive"
export MONITOR_LOG="$MONITOR_RUNTIME/events.jsonl"
test -z "$(git status --porcelain)"
# Stop hvis servicen kører; vent på afslutning før opdatering.
test "$(systemctl show monitorering-priser.service -p ActiveState --value)" != activating
git fetch origin
git switch main
git merge --ff-only origin/main
/usr/bin/Rscript monitorering/scripts/monitor.R backup "$MONITOR_RUNTIME/backups"
/usr/bin/Rscript monitorering/tests/observation_estimates.R
/usr/bin/Rscript monitorering/tests/publication.R
/usr/bin/Rscript monitorering/scripts/monitor.R publish-analysis /var/lib/monitorering-public/analysis.rds
sudo install -o root -g root -m 0644 monitorering-app/app.R monitorering-app/analysis.R /srv/shiny-server/monitorering/
sudo install -d -o root -g root -m 0755 /srv/shiny-server/monitorering/tmp
sudo touch /srv/shiny-server/monitorering/tmp/restart.txt
```

Automatisk genpublicering aktiveres med en separat drop-in; eksisterende root-only
credentialfil berøres ikke. Publiceringsmappen skal fortsat være operatørejet,
gruppe shiny og 2750. Den nye fil og `.previous` har 0640. Uden `MONITOR_PUBLICATION`
forbliver eksisterende takstjob uændret.

```bash
set -euo pipefail
sudo install -d -m 0755 /etc/systemd/system/monitorering-priser.service.d
sudo tee /etc/systemd/system/monitorering-priser.service.d/monitorering-publication.conf >/dev/null <<'UNIT'
[Service]
Environment=MONITOR_PUBLICATION=/var/lib/monitorering-public/analysis.rds
ReadWritePaths=/var/lib/monitorering-public
UNIT
sudo systemctl daemon-reload
systemctl show monitorering-priser.service -p ReadWritePaths
# Efter aftalt testkørsel (indsamler live takst):
sudo systemctl start monitorering-priser.service
systemctl show monitorering-priser.service -p Result -p ExecMainStatus
sudo journalctl -u monitorering-priser.service -n 60 --no-pager
```

Takstjobbet genpublicerer efter vellykket publicering til prisappen, under den
fælles writer-lås, med databasevalidering og atomisk udskiftning. Låskollision
eller fejl bevarer sidste gode analysefil og giver fejlet service uden ny
succeskvittering. Prisappen kan allerede være opdateret, selv om analysepublicering
fejler; systemet har ikke en atomisk transaktion på tværs af de to appfiler.
Genkør efter konkret fejlrettelse; slet aldrig en aktiv lås. Succes kræver både
servicekvittering og læsbar ny RDS med ny takstobservation i appen.

Efter installation: kontroller N06 januar–september 2026. August/september skal
stå uden observationer, sidste data er juli, scenariet august–december. Strengt
tilskud er fortsat blokeret af manglende målinger; det særskilte estimat skal vise
antal mangler. Sammenlign observerede summer med overdragelsen og undersøg evt.
revisioner. Åbn også eksisterende prisapp og hjemmeside. Bekræft ny automatisk
kørsel med kvittering næste morgen; første natkørsel er ikke verificeret af Work.
