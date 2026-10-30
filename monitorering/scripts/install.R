#!/usr/bin/env Rscript
# Linux binary repository recommended by DuckDB; fall back to source CRAN.
repo <- if(Sys.info()[['sysname']]=='Linux') sprintf(
 'https://p3m.dev/cran/latest/bin/linux/manylinux_2_28-%s/%s',R.version[['arch']],substr(getRversion(),1,3)
) else 'https://cloud.r-project.org'
packages <- c('DBI','duckdb','jsonlite','digest','filelock','readxl')
missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) install.packages(missing,repos=c(repo,'https://cloud.r-project.org'),Ncpus=2)
stopifnot(all(vapply(packages,requireNamespace,logical(1),quietly=TRUE)))
print(vapply(packages,function(x) as.character(packageVersion(x)),character(1)))
