# Monitorering · kort brugervejledning

Monitorering viser månedligt lægemiddelforbrug i DDD (definerede døgndoser) og
observeret tilskud i kroner. Du arbejder direkte i browseren med de publicerede
data.

## Eksempel · diabetesmidler i første kvartal 2026

1. Vælg **A10 · Midler mod diabetes** under **ATC-gruppe**.
   Sæt **Fra måned** til **2026-01**, **Til måned** til **2026-03** og
   **Bopælsregion** til **Hele Danmark**.
2. Læs periodens observerede summer under **Forbrug og tilskud**. Kurverne
   viser DDD og tilskud pr. måned. Åbn **Månedstal** for at se værdierne bag
   kurverne. En orange ring markerer delvis dækning eller manglende
   produktmålinger. Et hul i kurven betyder, at der mangler en observeret værdi.
3. Åbn **Sammenlign år**. Appen slår automatisk januar–marts 2025 op for samme
   ATC-gruppe og geografi. Tabellen viser procentvise forskelle pr. måned.
   En blank procent kan skyldes en manglende måling eller et nul året før.
   Kontrollér dækning og manglende målinger i begge år før fortolkning.
4. Skift **Bopælsregion** til **Region Midtjylland**. Aflæs de samme måneder
   og sammenlign med landstallene, du lige så. Vælg en anden region for at
   gentage sammenligningen. Appen viser absolutte summer, så forskellene
   afspejler også forskellig befolkningsstørrelse. **Hele Danmark** hentes fra
   sit eget nationale udtræk. Summen af regionerne kan afvige fra landstallet.
5. Åbn **Datastatus** og kontrollér både **Dækning** og **Observationer**.
   En indlæst forespørgsel kan dække det valgte ATC-område, selv om
   populationsdækningen er uverificeret, eller målinger mangler. Summerne
   omfatter kendte målinger. Tallet for manglende produktmålinger omfatter
   kun varenumre med en række i de indlæste data.
6. Klik **Hent CSV**. Filen indeholder produktrækker for de valgte filtre,
   med geografi og publiceringstidspunkt. Målinger, der mangler på en række,
   står tomme. Måneder uden produktrækker får ingen rækker i filen. Brug
   semikolon som separator, hvis du importerer filen i Excel. Årssammenligningens
   procenter beregnes i visningen.

Hvis du vil se ét bestemt præparat, kan du indtaste det sekscifrede varenummer
under **Flere filtre**. Åbn **ATC og varenumre** på forsiden for at se
produktrækker og vælge ATC-niveau for tabellen.

## Takster og fremskrivning

- **Takster og kobling** viser matchdage pr. produktmåned. Et historisk
  prisgrundlag kræver egnet pris pr. DDD og dokumenteret daglig gyldighed
  for hele måneden. En observeret takst kan ses under **Takstobservationer**.
- **Fremskrivning og scenarie → Tilskud** viser, om årets tilskud kan
  fremskrives. Appen kræver mindst tre sammenhængende, afsluttede måneder
  fra januar. De sidste tre måneders gennemsnit bruges frem til december
  uden sæsonkorrektion. Hvis enkelte produktmålinger mangler, kan appen vise
  et særskilt estimat fra de observerede månedssummer. Den stiplede kurve
  viser estimatet.
- **Pris ved observeret DDD** værdisætter observeret forbrug med senest
  observerede tilskudspris pr. DDD. Kontrollér prisdato, andel matchet DDD og
  eventuel afledt DDD under **Varenumre, prisdato og prisgrundlag**.
  **Produktbaseret prisscenarie** viser et beregnet prisgrundlag frem til
  december for varer med egnet pris og DDD i de tre beregningsmåneder.
