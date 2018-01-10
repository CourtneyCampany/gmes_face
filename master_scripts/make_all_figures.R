to_pdf <- function(expr, filename, ..., verbose=TRUE) {
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

source("master_scripts/functions.R")

fn <- function(...)file.path("master_scripts",...)

to_pdf(source("master_scripts/phys_boxplots.R"),fn("Figure1.pdf"), width=12, height=6)
to_pdf(source("master_scripts/cicc_boxplots.R"),fn("Figure2.pdf"), width=12, height=6)
to_pdf(source("master_scripts/photo_gmgs.R"),fn("Figure3.pdf"), width=10, height=6)
to_pdf(source("master_scripts/cond_stom_anatomy.R"),fn("Figure4.pdf"), width=10, height=6)
to_pdf(source("master_scripts/gmes_anatomy.R"),fn("Figure5.pdf"), width=10, height=6)
to_pdf(source("master_scripts/lma_anatomy.R"),fn("Figure6.pdf"), width=10, height=6)