# Afledt DDD i prisscenariet

DDD pr. pakning kan estimeres som ESP / kildens ESP pr. DDD. Scenarieprisen er derfor TSP × kilde-ESP/DDD / ESP. Positive, endelige ESP og kilde-ESP/DDD samt ikke-negativ TSP kræves. Nul TSP er tilladt. Ingen afrunding til heltals-DDD foretages; kildens afrunding kan påvirke resultatet, og der angives ingen udokumenteret præcisionsgrænse.

Nye importer bevarer den afledte denominator som `ddd_per_pack` og mærker den `unverified_ratio`. Den verificerede `tsp_per_ddd` forbliver NULL. Det daglige takstjob bruger samme normalisering. Eksisterende snapshots genbruges gennem scenarievisningen ud fra bevarede kildeprisfelter, uden ændring af råarkiv, observationstid eller importhistorik. Ingen ny kildeindsamling eller genimport er nødvendig.

Kun det aktuelle prisscenarie og dets produktfremskrivning bruger fallbacken. Samme varenummer og ATC kræves. Historiske prisjoins og faktisk tilskud er uændrede. Appen viser antal estimerede rækker, grundlaget i tabellen og kildeafrundingsforbehold. Produktfremskrivningens eksport indeholder også DDD-grundlag. Et snapshot observeret nu er ikke dokumentation for priser i 2025.

## Opdatering efter merge

I det rene servercheckout opdateres main med `git fetch origin` og `git merge --ff-only origin/main`. Brug eksisterende MONITOR_* og R_LIBS_USER som hidtil. Tag først en database-/arkivbackup med `Rscript monitorering/scripts/monitor.R backup BACKUPMAPPE`.

Kør derefter:

```bash
/usr/bin/Rscript monitorering/scripts/monitor.R init
/usr/bin/Rscript monitorering/scripts/monitor.R publish-analysis /var/lib/monitorering-public/analysis.rds
sudo install -o root -g root -m 0644 monitorering-app/app.R monitorering-app/analysis.R /srv/shiny-server/monitorering/
sudo touch /srv/shiny-server/monitorering/tmp/restart.txt
```

`init` genopbygger SQL-visningerne under writer-lås; det sletter ikke indlæste observationer. Brug appens genindlæsning og kontrollér matchantal, estimerede rækker og uændrede tilskudssummer. Historisk prisgrundlag skal fortsat være tomt uden dokumenterede gyldighedsintervaller og verificeret DDD.

Tests: `Rscript monitorering/tests/derived_ddd.R` dækker nyt og eksisterende snapshot, afledt pris, nul denominator, uændret streng historik, publicering og produktfremskrivning. Syntetiske data. Integration, publicering og takstjob testes separat. Server/liveverifikation udestår.
