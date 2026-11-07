# Copy NCI's fitted LCDRAT models, unchanged, from lcmodels 4.1.3
# (R/sysdata.rda): the lung cancer death model (LCDRAT) and the model for death
# from other causes (cox.death), which holds the matching baseline hazard.
# usage: Rscript data-raw/build_sysdata.R <unpacked lcmodels source dir>
e <- new.env()
load(file.path(commandArgs(trailingOnly = TRUE)[1], "R", "sysdata.rda"), envir = e)
LCDRAT <- e$LCDRAT
cox.death <- e$cox.death
save(LCDRAT, cox.death, file = "R/sysdata.rda", compress = "xz", version = 2)
