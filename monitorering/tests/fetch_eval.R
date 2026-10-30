# Verify the wrapper's actual evaluation statement with no network or credentials.
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
root <- normalizePath(file.path(dirname(script),'..'))
wrapper <- parse(file.path(root,'scripts','archive_existing_fetch.R'))
selected <- Filter(function(e) is.call(e) && identical(e[[1]],as.name('eval')) && identical(e[[2]],as.name('exprs')),as.list(wrapper))
stopifnot(length(selected)==1)
run <- function(code) {
 env <- new.env(parent=baseenv());exprs <- parse(text=code)
 eval(selected[[1]])
 env
}
e <- run('f<-tempfile();writeLines("downloaded",f);on.exit(unlink(f),add=TRUE);contents<-readLines(f);present_during_read<-file.exists(f)')
stopifnot(identical(e$contents,'downloaded'),isTRUE(e$present_during_read),!file.exists(e$f))
# An unrelated failure must still execute cleanup; preserve environment for inspection.
env <- new.env(parent=baseenv());exprs <- parse(text='f<-tempfile();writeLines("x",f);on.exit(unlink(f),add=TRUE);stop("synthetic failure")')
err <- tryCatch(eval(selected[[1]]),error=function(e)e)
stopifnot(inherits(err,'error'),!file.exists(env$f))
cat('PASS: downloaded file survives until read; cleanup runs on success and failure; no HTTP requests.\n')
